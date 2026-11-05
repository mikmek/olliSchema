package de.bizgrid.olli.schema

import redis.clients.jedis.JedisPooled
import redis.clients.jedis.params.{ScanParams, SetParams}

import java.util.UUID
import scala.jdk.CollectionConverters.*

/**
 * Lesemodell in der eigenen Redis-Instanz von olliSchema (ADR-0068). Nur Standard-Datenstrukturen,
 * keine Module; Mehrschluessel-Bedingungen laufen als Lua-Skript.
 */
final class RedisSchemaStore(redis: JedisPooled) extends SchemaStore {

  /** Version nur nach oben setzen: idempotente Projektion je Entity-Version. */
  private val RaiseVersion =
    """local current = redis.call('GET', KEYS[1])
      |if current and tonumber(current) >= tonumber(ARGV[1]) then return 0 end
      |redis.call('SET', KEYS[1], ARGV[1])
      |return 1""".stripMargin

  private val Unbind =
    """if redis.call('GET', KEYS[1]) == ARGV[1] then return redis.call('DEL', KEYS[1]) end
      |return 0""".stripMargin

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
  def popFromSet(key: String, count: Int): Seq[String] = Option(redis.spop(key, count.toLong)).fold(Seq.empty[String])(_.asScala.toSeq)
  def setSize(key: String): Long = redis.scard(key)
  def addRelease(classKey: String, version: Int, uuid: UUID): Unit = redis.zadd(Keys.releases(classKey), version.toDouble, uuid.toString)
  def removeRelease(classKey: String, uuid: UUID): Unit = redis.zrem(Keys.releases(classKey), uuid.toString)

  def releases(classKey: String): Seq[(Int, UUID)] =
    redis.zrangeWithScores(Keys.releases(classKey), 0, -1).asScala.toSeq
      .map(tuple => (tuple.getScore.toInt, UUID.fromString(tuple.getElement))).sortBy(_._1)

  def raiseVersion(entity: UUID, version: Long): Boolean =
    redis.eval(RaiseVersion, List(Keys.version(entity)).asJava, List(version.toString).asJava) match {
      case value: java.lang.Long => value == 1L
      case _ => false
    }

  def pushBounded(key: String, value: String, max: Int): Unit = {
    redis.lpush(key, value)
    redis.ltrim(key, 0, (max - 1).toLong)
  }

  def recent(key: String, limit: Int): Seq[String] = redis.lrange(key, 0, (limit - 1).toLong).asScala.toSeq

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
