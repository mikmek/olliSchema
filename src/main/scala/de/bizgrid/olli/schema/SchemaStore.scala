package de.bizgrid.olli.schema

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
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

/**
 * Speicher des Lesemodells. Bewusst schluesselbasiert und ohne Fachlogik (ADR-0068); die
 * Fachlogik liegt in [[Projection]]. Die Redis-Implementierung und die In-Memory-Implementierung
 * fuer Tests teilen damit dieselbe Semantik.
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
  def popFromSet(key: String, count: Int): Seq[String]
  def setSize(key: String): Long
  def addRelease(classKey: String, version: Int, uuid: UUID): Unit
  def removeRelease(classKey: String, uuid: UUID): Unit
  /** Released-Versionen einer Klasse, aufsteigend. */
  def releases(classKey: String): Seq[(Int, UUID)]
  /** Setzt die projizierte Version nur nach oben (Lua-Compare-and-Set). Liefert true, wenn sie gestiegen ist. */
  def raiseVersion(entity: UUID, version: Long): Boolean
  def pushBounded(key: String, value: String, max: Int): Unit
  def recent(key: String, limit: Int): Seq[String]
  def ping(): Boolean
  /** Loescht das gesamte Lesemodell; liefert die Anzahl geloeschter Schluessel. */
  def clear(): Long
  def close(): Unit

  final def version(entity: UUID): Option[Long] = get(Keys.version(entity)).flatMap(_.toLongOption)
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
  def popFromSet(key: String, count: Int): Seq[String] = synchronized {
    val set = setFor(key)
    val taken = set.asScala.take(count).toSeq
    set.removeAll(taken.asJava)
    taken
  }
  def setSize(key: String): Long = Option(sets.get(key)).fold(0L)(_.size.toLong)
  def addRelease(classKey: String, version: Int, uuid: UUID): Unit =
    sorted.computeIfAbsent(classKey, _ => new java.util.concurrent.ConcurrentSkipListMap[Int, UUID]()).put(version, uuid)
  def removeRelease(classKey: String, uuid: UUID): Unit =
    Option(sorted.get(classKey)).foreach(map => map.entrySet().removeIf(_.getValue == uuid))
  def releases(classKey: String): Seq[(Int, UUID)] =
    Option(sorted.get(classKey)).fold(Seq.empty[(Int, UUID)])(_.asScala.toSeq.sortBy(_._1))
  def raiseVersion(entity: UUID, version: Long): Boolean = synchronized {
    val current = get(Keys.version(entity)).flatMap(_.toLongOption)
    if (current.forall(_ < version)) { set(Keys.version(entity), version.toString); true } else false
  }
  def pushBounded(key: String, value: String, max: Int): Unit = synchronized {
    val list = lists.computeIfAbsent(key, _ => new java.util.ArrayList[String]())
    list.add(0, value)
    while (list.size > max) list.remove(list.size - 1)
  }
  def recent(key: String, limit: Int): Seq[String] = Option(lists.get(key)).fold(Seq.empty[String])(_.asScala.take(limit).toSeq)
  def ping(): Boolean = true
  def clear(): Long = {
    val count = values.size + sets.size + sorted.size + lists.size
    values.clear(); sets.clear(); sorted.clear(); lists.clear()
    count.toLong
  }
  def close(): Unit = ()
}
