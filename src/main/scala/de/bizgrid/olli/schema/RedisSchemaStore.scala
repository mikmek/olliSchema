package de.bizgrid.olli.schema

import redis.clients.jedis.JedisPooled
import redis.clients.jedis.params.{ScanParams, SetParams}

import java.util.UUID
import scala.jdk.CollectionConverters.*

/**
 * Lesemodell in der eigenen Redis-Instanz von olliSchema (ADR-0068). Nur Standard-Datenstrukturen,
 * keine Module.
 *
 * Die Projektion schreibt ausschliesslich ueber [[commit]]: ein Lua-Skript prueft zuerst die
 * Entity-Version und alle Bindungen und wendet danach alle Operationen eines Events an. Redis fuehrt
 * Skripte atomar aus; weil alle Pruefungen vor dem ersten Schreibvorgang liegen, schreibt das Skript
 * entweder alles oder nichts (SCH-A08).
 */
final class RedisSchemaStore(redis: JedisPooled) extends SchemaStore {

  private val Unbind =
    """if redis.call('GET', KEYS[1]) == ARGV[1] then return redis.call('DEL', KEYS[1]) end
      |return 0""".stripMargin

  /**
   * ARGV[1]: JSON-Array der Operationen, ARGV[2]/ARGV[3]: Versionsschluessel und -wert (leer = ohne
   * Versionspruefung). Ergebnis: `applied`, `duplicate` oder `conflict:<key>`.
   */
  private[schema] val Commit =
    """local ops = cjson.decode(ARGV[1])
      |local vkey, vval = ARGV[2], ARGV[3]
      |if vkey ~= '' then
      |  local cur = redis.call('GET', vkey)
      |  if cur and tonumber(cur) >= tonumber(vval) then return 'duplicate' end
      |end
      |local shadow = {}
      |local function current(key)
      |  local v = shadow[key]
      |  if v == nil then return redis.call('GET', key) end
      |  return v
      |end
      |for _, op in ipairs(ops) do
      |  local t = op[1]
      |  if t == 'set' then shadow[op[2]] = op[3]
      |  elseif t == 'del' then shadow[op[2]] = false
      |  elseif t == 'unbind' then if current(op[2]) == op[3] then shadow[op[2]] = false end
      |  elseif t == 'bind' then
      |    local cur = current(op[2])
      |    if cur and cur ~= op[3] then return 'conflict:' .. op[2] end
      |    shadow[op[2]] = op[3]
      |  end
      |end
      |for _, op in ipairs(ops) do
      |  local t = op[1]
      |  if t == 'set' or t == 'bind' then redis.call('SET', op[2], op[3])
      |  elseif t == 'del' then redis.call('DEL', op[2])
      |  elseif t == 'sadd' or t == 'srem' then
      |    local cmd = string.upper(t)
      |    for i = 3, #op, 500 do redis.call(cmd, op[2], unpack(op, i, math.min(i + 499, #op))) end
      |  elseif t == 'zadd' then redis.call('ZADD', op[2], op[3], op[4])
      |  elseif t == 'zrem' then redis.call('ZREM', op[2], op[3])
      |  elseif t == 'unbind' then if redis.call('GET', op[2]) == op[3] then redis.call('DEL', op[2]) end
      |  elseif t == 'push' then
      |    redis.call('LPUSH', op[2], op[3])
      |    redis.call('LTRIM', op[2], 0, tonumber(op[4]) - 1)
      |  end
      |end
      |if vkey ~= '' then redis.call('SET', vkey, vval) end
      |return 'applied'""".stripMargin

  def get(key: String): Option[String] = Option(redis.get(key))
  def set(key: String, value: String): Unit = redis.set(key, value)
  def delete(keys: String*): Unit = if (keys.nonEmpty) redis.del(keys*)

  def bindOnce(key: String, value: String): String = {
    redis.set(key, value, SetParams.setParams().nx())
    Option(redis.get(key)).getOrElse(value)
  }

  def unbind(key: String, value: String): Unit = redis.eval(Unbind, List(key).asJava, List(value).asJava)
  def addToSet(key: String, members: String*): Unit = if (members.nonEmpty) redis.sadd(key, members*)
  def removeFromSet(key: String, members: String*): Unit = if (members.nonEmpty) redis.srem(key, members*)
  def members(key: String): Set[String] = redis.smembers(key).asScala.toSet
  def sample(key: String, count: Int): Seq[String] = Option(redis.srandmember(key, count)).fold(Seq.empty[String])(_.asScala.toSeq)
  def setSize(key: String): Long = redis.scard(key)
  def addRelease(classKey: String, version: Int, uuid: UUID): Unit = redis.zadd(Keys.releases(classKey), version.toDouble, uuid.toString)
  def removeRelease(classKey: String, uuid: UUID): Unit = redis.zrem(Keys.releases(classKey), uuid.toString)

  def releases(classKey: String): Seq[(Int, UUID)] =
    redis.zrangeWithScores(Keys.releases(classKey), 0, -1).asScala.toSeq
      .map(tuple => (tuple.getScore.toInt, UUID.fromString(tuple.getElement))).sortBy(_._1)

  def pushBounded(key: String, value: String, max: Int): Unit = {
    redis.lpush(key, value)
    redis.ltrim(key, 0, (max - 1).toLong)
  }

  def recent(key: String, limit: Int): Seq[String] = redis.lrange(key, 0, (limit - 1).toLong).asScala.toSeq

  def commit(ops: Seq[StoreOp], version: Option[(UUID, Long)]): CommitResult = {
    val (versionKey, versionValue) = version.fold(("", ""))((entity, value) => (Keys.version(entity), value.toString))
    redis.eval(Commit, List.empty[String].asJava, List(RedisSchemaStore.encode(ops), versionKey, versionValue).asJava) match {
      case "applied" => CommitResult.Applied
      case "duplicate" => CommitResult.Duplicate
      case other: String if other.startsWith("conflict:") => CommitResult.Conflict(other.stripPrefix("conflict:"))
      case other => throw new IllegalStateException(s"Unexpected commit result $other")
    }
  }

  def ping(): Boolean = scala.util.Try(redis.ping().equalsIgnoreCase("PONG")).getOrElse(false)

  def clear(): Long = Keys.Prefixes.map { prefix =>
    var cursor = ScanParams.SCAN_POINTER_START
    var deleted = 0L
    val params = new ScanParams().`match`(s"$prefix*").count(1000)
    while ({
      val page = redis.scan(cursor, params)
      val keys = page.getResult.asScala.toSeq
      if (keys.nonEmpty) deleted += redis.del(keys*)
      cursor = page.getCursor
      cursor != ScanParams.SCAN_POINTER_START
    }) ()
    deleted
  }.sum

  def close(): Unit = redis.close()
}

object RedisSchemaStore {
  /** Operationen als JSON-Array von String-Arrays fuer das Commit-Skript. */
  def encode(ops: Seq[StoreOp]): String = ujson.write(ujson.Arr.from(ops.map {
    case StoreOp.Set(key, value) => ujson.Arr("set", key, value)
    case StoreOp.Delete(key) => ujson.Arr("del", key)
    case StoreOp.SAdd(key, members) => ujson.Arr.from(Seq[ujson.Value]("sadd", key) ++ members.map(ujson.Str(_)))
    case StoreOp.SRem(key, members) => ujson.Arr.from(Seq[ujson.Value]("srem", key) ++ members.map(ujson.Str(_)))
    case StoreOp.ZAdd(key, score, member) => ujson.Arr("zadd", key, score.toString, member)
    case StoreOp.ZRem(key, member) => ujson.Arr("zrem", key, member)
    case StoreOp.Bind(key, value) => ujson.Arr("bind", key, value)
    case StoreOp.Unbind(key, value) => ujson.Arr("unbind", key, value)
    case StoreOp.Push(key, value, max) => ujson.Arr("push", key, value, max.toString)
  }))
}
