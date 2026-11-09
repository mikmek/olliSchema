package de.bizgrid.olli.schema

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Schluessel des Lesemodells (ADR-0068). Erweiterungen gegenueber der Skizze sind markiert. */
object Keys {
  def url(url: String) = s"schema:url:$url"
  def doc(uuid: UUID) = s"schema:doc:$uuid"
  def validity(uuid: UUID) = s"schema:validity:$uuid"
  def index(application: String, domain: String) = s"schema:idx:$application:$domain"
  def refBy(uuid: UUID) = s"schema:refby:$uuid"
  def alias(originalUri: String) = s"schema:alias:$originalUri"
  def meta(targetUrl: String, metaType: MetaType) = s"schema:meta:$targetUrl:${metaType.wire}"
  def entityValidity(entity: UUID) = s"entityvalidity:$entity"

  /** Erweiterung: alle projizierten Schema-UUIDs (Listing ohne application/domain). */
  val All = "schema:all"
  /** Erweiterung: Fremdschemas, Listing unter `ext`. */
  val External = "schema:idx:ext"
  /** Erweiterung: Released-Versionen einer Klasse als Sorted Set Version -> UUID (Release-Pruefung). */
  def releases(classKey: String) = s"schema:releases:$classKey"
  /** Erweiterung: Schemas, die auf eine noch nicht aufloesbare URL warten. */
  def waiting(url: String) = s"schema:waiting:$url"
  /** Erweiterung: Entities, die ein Schema verwenden (fuer Neuvalidierung, nicht fuer CheckReferer). */
  def users(uuid: UUID) = s"schema:users:$uuid"
  /** Erweiterung: Entities, deren Schema-URL (noch) nicht bekannt ist. */
  def missingUsers(url: String) = s"schema:users-missing:$url"
  /** Erweiterung: letzter Entity-Stand und projizierte Version. */
  def entityState(entity: UUID) = s"entitystate:$entity"
  def version(entity: UUID) = s"entityversion:$entity"
  /** Erweiterung: Warteschlange der gedrosselten Neuvalidierung. */
  val Revalidate = "schema:revalidate"
  val Violations = "schema:violations"
  val DeadLetters = "schema:deadletters"

  /** Alle Praefixe des Lesemodells; die Reprojektion loescht genau diese. */
  val Prefixes: Seq[String] = Seq("schema:", "entityvalidity:", "entitystate:", "entityversion:")
}

/** Primitiver Schreibvorgang einer Transaktion (SCH-A08). */
enum StoreOp {
  case Set(key: String, value: String)
  case Delete(key: String)
  case SAdd(key: String, members: Seq[String])
  case SRem(key: String, members: Seq[String])
  case ZAdd(key: String, score: Int, member: String)
  case ZRem(key: String, member: String)
  /** Setzt `key` auf `value`; scheitert die ganze Transaktion, wenn `key` einen anderen Wert traegt. */
  case Bind(key: String, value: String)
  /** Loescht `key` nur, wenn er `value` traegt. */
  case Unbind(key: String, value: String)
  case Push(key: String, value: String, max: Int)
}

enum CommitResult {
  case Applied
  /** Die Entity-Version war bereits projiziert; nichts wurde geschrieben. */
  case Duplicate
  /** Eine Bindung gehoert inzwischen einem anderen Wert; nichts wurde geschrieben, neu berechnen. */
  case Conflict(key: String)
}

/**
 * Speicher des Lesemodells. Bewusst schluesselbasiert und ohne Fachlogik (ADR-0068); die Fachlogik liegt
 * in [[Projection]].
 *
 * Schreibvorgaenge der Projektion laufen ueber [[TransactionView]] und [[commit]]: alle Operationen eines
 * Events werden zusammen mit der Versionspruefung atomar angewendet oder gar nicht (SCH-A08). Die
 * Einzel-Schreibmethoden bleiben fuer Diagnose und Werkzeuge.
 */
trait SchemaStore {
  def get(key: String): Option[String]
  def set(key: String, value: String): Unit
  def delete(keys: String*): Unit
  /** Setzt nur, wenn der Schluessel fehlt oder bereits `value` traegt (SET NX). Liefert den gueltigen Wert. */
  def bindOnce(key: String, value: String): String
  /** Loescht den Schluessel nur, wenn er `value` traegt. */
  def unbind(key: String, value: String): Unit
  def addToSet(key: String, members: String*): Unit
  def removeFromSet(key: String, members: String*): Unit
  def members(key: String): Set[String]
  /** Bis zu `count` Elemente einer Menge, ohne sie zu entfernen. */
  def sample(key: String, count: Int): Seq[String]
  def setSize(key: String): Long
  def addRelease(classKey: String, version: Int, uuid: UUID): Unit
  def removeRelease(classKey: String, uuid: UUID): Unit
  /** Released-Versionen einer Klasse, aufsteigend. */
  def releases(classKey: String): Seq[(Int, UUID)]
  def pushBounded(key: String, value: String, max: Int): Unit
  def recent(key: String, limit: Int): Seq[String]
  /**
   * Wendet `ops` atomar an. Mit `version` nur, wenn die projizierte Version der Entity darunter liegt;
   * die Version wird im selben Schritt angehoben.
   */
  def commit(ops: Seq[StoreOp], version: Option[(UUID, Long)]): CommitResult
  def ping(): Boolean
  /** Loescht das gesamte Lesemodell; liefert die Anzahl geloeschter Schluessel. */
  def clear(): Long
  def close(): Unit

  final def version(entity: UUID): Option[Long] = get(Keys.version(entity)).flatMap(_.toLongOption)
}

/**
 * Transaktionssicht fuer ein Event (SCH-A08): Lesevorgaenge sehen den Basisspeicher zusammen mit den
 * eigenen, noch nicht geschriebenen Aenderungen; Schreibvorgaenge werden gesammelt und mit [[commit]]
 * in einem Schritt angewendet. Bricht die Verarbeitung vorher ab, bleibt der Speicher unveraendert.
 */
final class TransactionView(base: SchemaStore) extends SchemaStore {
  private final class SetDelta(var cleared: Boolean = false) {
    val added = mutable.LinkedHashSet.empty[String]
    val removed = mutable.Set.empty[String]
  }
  private final class ZDelta(var cleared: Boolean = false) {
    val added = mutable.LinkedHashMap.empty[UUID, Int]
    val removed = mutable.Set.empty[UUID]
  }

  private val values = mutable.Map.empty[String, Option[String]]
  private val sets = mutable.Map.empty[String, SetDelta]
  private val zsets = mutable.Map.empty[String, ZDelta]
  private val ops = mutable.ArrayBuffer.empty[StoreOp]

  def operations: Seq[StoreOp] = ops.toSeq

  def get(key: String): Option[String] = values.getOrElse(key, base.get(key))

  def set(key: String, value: String): Unit = {
    values(key) = Some(value)
    ops += StoreOp.Set(key, value)
  }

  def delete(keys: String*): Unit = keys.foreach { key =>
    values(key) = None
    sets(key) = new SetDelta(cleared = true)
    zsets(key) = new ZDelta(cleared = true)
    ops += StoreOp.Delete(key)
  }

  def bindOnce(key: String, value: String): String = get(key) match {
    case Some(current) => current
    case None =>
      values(key) = Some(value)
      ops += StoreOp.Bind(key, value)
      value
  }

  def unbind(key: String, value: String): Unit = if (get(key).contains(value)) {
    values(key) = None
    ops += StoreOp.Unbind(key, value)
  }

  def addToSet(key: String, members: String*): Unit = if (members.nonEmpty) {
    val delta = sets.getOrElseUpdate(key, new SetDelta())
    delta.added ++= members
    delta.removed --= members
    ops += StoreOp.SAdd(key, members)
  }

  def removeFromSet(key: String, members: String*): Unit = if (members.nonEmpty) {
    val delta = sets.getOrElseUpdate(key, new SetDelta())
    delta.removed ++= members
    delta.added --= members
    ops += StoreOp.SRem(key, members)
  }

  def members(key: String): Set[String] = sets.get(key) match {
    case None => base.members(key)
    case Some(delta) => ((if (delta.cleared) Set.empty[String] else base.members(key)) -- delta.removed) ++ delta.added
  }

  def sample(key: String, count: Int): Seq[String] = members(key).take(count).toSeq
  def setSize(key: String): Long = members(key).size.toLong

  def addRelease(classKey: String, version: Int, uuid: UUID): Unit = {
    val key = Keys.releases(classKey)
    val delta = zsets.getOrElseUpdate(key, new ZDelta())
    delta.added(uuid) = version
    delta.removed -= uuid
    ops += StoreOp.ZAdd(key, version, uuid.toString)
  }

  def removeRelease(classKey: String, uuid: UUID): Unit = {
    val key = Keys.releases(classKey)
    val delta = zsets.getOrElseUpdate(key, new ZDelta())
    delta.removed += uuid
    delta.added -= uuid
    ops += StoreOp.ZRem(key, uuid.toString)
  }

  def releases(classKey: String): Seq[(Int, UUID)] = zsets.get(Keys.releases(classKey)) match {
    case None => base.releases(classKey)
    case Some(delta) =>
      val kept = (if (delta.cleared) Nil else base.releases(classKey)).filterNot((_, uuid) => delta.removed(uuid) || delta.added.contains(uuid))
      (kept ++ delta.added.toSeq.map((uuid, version) => (version, uuid))).sortBy(_._1)
  }

  def pushBounded(key: String, value: String, max: Int): Unit = ops += StoreOp.Push(key, value, max)
  def recent(key: String, limit: Int): Seq[String] = base.recent(key, limit)

  /** Wendet die gesammelten Operationen atomar auf den Basisspeicher an. */
  def commit(version: Option[(UUID, Long)]): CommitResult = base.commit(ops.toSeq, version)

  def commit(more: Seq[StoreOp], version: Option[(UUID, Long)]): CommitResult = base.commit(ops.toSeq ++ more, version)
  def ping(): Boolean = base.ping()
  def clear(): Long = throw new UnsupportedOperationException("clear inside a transaction")
  def close(): Unit = ()
}

/** Prozesslokaler Speicher fuer Tests und Entwicklung ohne Redis. */
final class InMemorySchemaStore extends SchemaStore {
  private val values = new ConcurrentHashMap[String, String]()
  private val sets = new ConcurrentHashMap[String, java.util.Set[String]]()
  private val sorted = new ConcurrentHashMap[String, java.util.concurrent.ConcurrentSkipListMap[Int, UUID]]()
  private val lists = new ConcurrentHashMap[String, java.util.List[String]]()

  def get(key: String): Option[String] = Option(values.get(key))
  def set(key: String, value: String): Unit = values.put(key, value)
  def delete(keys: String*): Unit = keys.foreach { key => values.remove(key); sets.remove(key); sorted.remove(key); lists.remove(key) }
  def bindOnce(key: String, value: String): String = Option(values.putIfAbsent(key, value)).getOrElse(value)
  def unbind(key: String, value: String): Unit = values.remove(key, value)
  private def setFor(key: String) = sets.computeIfAbsent(key, _ => ConcurrentHashMap.newKeySet[String]())
  def addToSet(key: String, members: String*): Unit = if (members.nonEmpty) setFor(key).addAll(members.asJava)
  def removeFromSet(key: String, members: String*): Unit = Option(sets.get(key)).foreach(_.removeAll(members.asJava))
  def members(key: String): Set[String] = Option(sets.get(key)).fold(Set.empty[String])(_.asScala.toSet)
  def sample(key: String, count: Int): Seq[String] = members(key).take(count).toSeq
  def setSize(key: String): Long = Option(sets.get(key)).fold(0L)(_.size.toLong)
  def addRelease(classKey: String, version: Int, uuid: UUID): Unit = zadd(Keys.releases(classKey), version, uuid)
  private def zadd(key: String, version: Int, uuid: UUID): Unit = {
    val map = sorted.computeIfAbsent(key, _ => new java.util.concurrent.ConcurrentSkipListMap[Int, UUID]())
    map.entrySet().removeIf(_.getValue == uuid)
    map.put(version, uuid)
  }
  def removeRelease(classKey: String, uuid: UUID): Unit = zrem(Keys.releases(classKey), uuid)
  private def zrem(key: String, uuid: UUID): Unit = Option(sorted.get(key)).foreach(map => map.entrySet().removeIf(_.getValue == uuid))
  def releases(classKey: String): Seq[(Int, UUID)] =
    Option(sorted.get(Keys.releases(classKey))).fold(Seq.empty[(Int, UUID)])(_.asScala.toSeq.sortBy(_._1))
  def pushBounded(key: String, value: String, max: Int): Unit = synchronized {
    val list = lists.computeIfAbsent(key, _ => new java.util.ArrayList[String]())
    list.add(0, value)
    while (list.size > max) list.remove(list.size - 1)
  }
  def recent(key: String, limit: Int): Seq[String] = Option(lists.get(key)).fold(Seq.empty[String])(_.asScala.take(limit).toSeq)

  def commit(ops: Seq[StoreOp], version: Option[(UUID, Long)]): CommitResult = synchronized {
    if (version.exists((entity, value) => this.version(entity).exists(_ >= value))) CommitResult.Duplicate
    else {
      // Bindungen gegen den Stand pruefen, den die vorangehenden Operationen derselben Transaktion ergeben.
      val shadow = mutable.Map.empty[String, Option[String]]
      def current(key: String) = shadow.getOrElse(key, get(key))
      val conflict = ops.collectFirst(Function.unlift[StoreOp, String] {
        case StoreOp.Set(key, value) => shadow(key) = Some(value); None
        case StoreOp.Delete(key) => shadow(key) = None; None
        case StoreOp.Unbind(key, value) => if (current(key).contains(value)) shadow(key) = None; None
        case StoreOp.Bind(key, value) =>
          if (current(key).exists(_ != value)) Some(key) else { shadow(key) = Some(value); None }
        case _ => None
      })
      conflict match {
        case Some(key) => CommitResult.Conflict(key)
        case None =>
          ops.foreach {
            case StoreOp.Set(key, value) => set(key, value)
            case StoreOp.Delete(key) => delete(key)
            case StoreOp.SAdd(key, members) => addToSet(key, members*)
            case StoreOp.SRem(key, members) => removeFromSet(key, members*)
            case StoreOp.ZAdd(key, score, member) => zadd(key, score, UUID.fromString(member))
            case StoreOp.ZRem(key, member) => zrem(key, UUID.fromString(member))
            case StoreOp.Bind(key, value) => set(key, value)
            case StoreOp.Unbind(key, value) => unbind(key, value)
            case StoreOp.Push(key, value, max) => pushBounded(key, value, max)
          }
          version.foreach((entity, value) => set(Keys.version(entity), value.toString))
          CommitResult.Applied
      }
    }
  }

  def ping(): Boolean = true
  def clear(): Long = {
    val count = values.size + sets.size + sorted.size + lists.size
    values.clear(); sets.clear(); sorted.clear(); lists.clear()
    count.toLong
  }
  def close(): Unit = ()
}
