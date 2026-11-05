package de.bizgrid.olli.schema

import java.util.UUID
import scala.util.Try

/** Lesender Zugriff auf das Lesemodell; die einzige Stelle, die Schema-URLs aufloest (ADR-0062). */
final class Catalog(val store: SchemaStore, val urls: SchemaUrls) {

  def record(uuid: UUID): Option[SchemaRecord] =
    store.get(Keys.doc(uuid)).flatMap(json => Try(SchemaRecord.fromJson(ujson.read(json))).toOption)

  def validity(uuid: UUID): Option[SchemaValidity] =
    store.get(Keys.validity(uuid)).flatMap(json => Try(SchemaValidity.fromJson(ujson.read(json))).toOption)

  /**
   * UUID zu einer Dokument-URL: `by-id`, abgeleitete URL oder Original-URI eines Fremdschemas. Ein
   * Fragment wird ignoriert. Es gibt keinen Netzzugriff.
   */
  def uuidFor(url: String): Option[UUID] = {
    val document = References.stripFragment(url.trim)
    urls.byIdUuid(document).filter(uuid => store.get(Keys.doc(uuid)).nonEmpty)
      .orElse(store.get(Keys.url(document)).flatMap(value => Try(UUID.fromString(value)).toOption))
      .orElse(store.get(Keys.alias(document)).flatMap(value => Try(UUID.fromString(value)).toOption))
  }

  def resolve(url: String): Option[SchemaRecord] = uuidFor(url).flatMap(record)

  def document(url: String): Option[ujson.Value] = resolve(url).map(_.document)

  /**
   * Basis-URL eines Datensatzes fuer relative Referenzen: bei Fremdschemas die Original-URI (ihr
   * `$id` bleibt unveraendert), sonst die abgeleitete URL, die auch als `$id` im Dokument steht.
   */
  def baseUrl(record: SchemaRecord): String =
    record.info.flatMap(_.external).map(_.originalUri).orElse(record.url).getOrElse(urls.byId(record.uuid))

  /** Alle URLs, unter denen ein Datensatz aufgeloest werden kann. */
  def addresses(record: SchemaRecord): Seq[String] =
    (Seq(urls.byId(record.uuid)) ++ record.url.toSeq ++ record.info.flatMap(_.external).map(_.originalUri).toSeq).distinct

  def list(application: Option[String], domain: Option[String]): Seq[SchemaRecord] = {
    val key = (application, domain) match {
      case (Some("ext"), _) => Keys.External
      case (Some(app), Some(dom)) => Keys.index(app, dom)
      case _ => Keys.All
    }
    store.members(key).toSeq.flatMap(value => Try(UUID.fromString(value)).toOption).flatMap(record)
      .filter(r => application.forall(app => app == "ext" || r.info.flatMap(_.internal).exists(_.application == app)))
      .sortBy(r => r.url.getOrElse(urls.byId(r.uuid)))
  }

  def metaSchemas(targetUrl: String, metaType: Option[MetaType]): Seq[SchemaRecord] =
    metaType.fold(MetaType.values.toSeq)(Seq(_)).flatMap(kind => store.members(Keys.meta(targetUrl, kind)))
      .flatMap(value => Try(UUID.fromString(value)).toOption).distinct.flatMap(record)

  def referers(uuid: UUID): Seq[SchemaRecord] =
    store.members(Keys.refBy(uuid)).toSeq.flatMap(value => Try(UUID.fromString(value)).toOption).flatMap(record)

  def entityValidity(entity: UUID): Option[EntityValidity] =
    store.get(Keys.entityValidity(entity)).flatMap(json => Try(EntityValidity.fromJson(ujson.read(json))).toOption)

  def entityState(entity: UUID): Option[EntityState] =
    store.get(Keys.entityState(entity)).flatMap(json => Try(EntityState.fromJson(ujson.read(json))).toOption)

  def lastRelease(classKey: String): Option[(Int, UUID)] = store.releases(classKey).lastOption
}
