package de.bizgrid.olli.schema

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.UUID

/** Schemasprachen, die das `schema`-Property einer Schema-Entity benennt (REQ-SCH-001). */
object SchemaLanguage {
  val JsonSchema202012 = "https://json-schema.org/draft/2020-12/schema"

  /** Gleichwertige Schreibweisen derselben Sprache. */
  def normalize(value: String): String = value.trim.stripSuffix("#") match {
    case "http://json-schema.org/draft/2020-12/schema" => JsonSchema202012
    case other => other
  }
}

/**
 * Projizierte Schema-Entity (`schema:doc:<uuid>`, ADR-0068). `entityVersion` ist die Version der
 * Entity, aus der der Stand stammt (siehe [[EntityVersion]]).
 *
 * `url` fehlt, wenn die Metadaten keine URL ergeben (`infoProblems`) oder die abgeleitete URL bereits
 * einer anderen UUID gehoert. `refs` sind die absoluten Dokument-URLs aller `$ref`-Ziele.
 */
final case class SchemaRecord(
  uuid: UUID,
  url: Option[String],
  info: Option[SchemaInfo],
  infoProblems: Seq[Problem],
  language: String,
  document: ujson.Value,
  entityVersion: Long,
  tenant: UUID,
  project: UUID,
  registeredAt: Long,
  refs: Seq[String]
) {
  /** Starker ETag ueber Inhalt und Metadaten; aendert sich mit jedem Draft-Stand. */
  lazy val etag: String = {
    val digest = MessageDigest.getInstance("SHA-256")
      .digest(ujson.write(ujson.Obj("info" -> info.fold[ujson.Value](ujson.Null)(_.toJson), "document" -> document)).getBytes(StandardCharsets.UTF_8))
    digest.take(16).map(byte => f"$byte%02x").mkString
  }

  def isImmutable: Boolean = info.exists(_.status.isImmutable)

  /** Released, deprecated oder withdrawn: gilt fuer REQ-SCH-005 als Released-Ziel. Fremdschemas gelten als released. */
  def countsAsReleased: Boolean = isImmutable

  def toJson: ujson.Value = {
    val obj = ujson.Obj(
      "uuid" -> uuid.toString,
      "language" -> language,
      "document" -> document,
      "entityVersion" -> ujson.Num(entityVersion.toDouble),
      "tenant" -> tenant.toString,
      "project" -> project.toString,
      "registeredAt" -> ujson.Num(registeredAt.toDouble),
      "refs" -> ujson.Arr.from(refs.map(ujson.Str(_))),
      "infoProblems" -> ujson.Arr.from(infoProblems.map(_.toJson)),
      "etag" -> etag)
    url.foreach(value => obj("url") = value)
    info.foreach(value => obj("info") = value.toJson)
    obj
  }
}

object SchemaRecord {
  def fromJson(value: ujson.Value): SchemaRecord = SchemaRecord(
    uuid = UUID.fromString(value("uuid").str),
    url = value.obj.get("url").map(_.str),
    info = value.obj.get("info").map(infoFromJson),
    infoProblems = value.obj.get("infoProblems").fold(Seq.empty[Problem])(_.arr.toSeq.map(Problem.fromJson)),
    language = value("language").str,
    document = value("document"),
    entityVersion = value("entityVersion").num.toLong,
    tenant = UUID.fromString(value("tenant").str),
    project = UUID.fromString(value("project").str),
    registeredAt = value("registeredAt").num.toLong,
    refs = value("refs").arr.toSeq.map(_.str))

  private def infoFromJson(value: ujson.Value): SchemaInfo = {
    val obj = value.obj
    val identity = obj.get("external") match {
      case Some(external) => ExternalIdentity(external("publisher").str, external("path").str, external("originalUri").str)
      case None => InternalIdentity(obj("application").str, obj("domain").str, obj("class").str, obj("version").num.toInt,
        obj.get("branch").map(_.str), obj.get("tenant").map(entry => UUID.fromString(entry.str)))
    }
    val meta = for {
      kind <- obj.get("metaType").flatMap(entry => MetaType.fromWire(entry.str))
      target <- obj.get("target").map(_.str)
    } yield MetaBinding(kind, target)
    SchemaInfo(identity, SchemaStatus.fromWire(obj("status").str).getOrElse(SchemaStatus.Released), obj.get("basedOn").map(_.num.toInt), meta)
  }
}

/** Schema-Gueltigkeit nach ADR-0064. */
enum ValidityState(val wire: String) {
  case Valid extends ValidityState("valid")
  case Invalid extends ValidityState("invalid")
  case Unresolved extends ValidityState("unresolved")
}

final case class SchemaValidity(state: ValidityState, entityVersion: Long, etag: String, checkedAt: Long, problems: Seq[Problem]) {
  def errors: Seq[Problem] = problems.filterNot(_.warning)

  def toJson: ujson.Value = ujson.Obj(
    "state" -> state.wire,
    "entityVersion" -> ujson.Num(entityVersion.toDouble),
    "etag" -> etag,
    "checkedAt" -> Json.instant(checkedAt),
    "problems" -> ujson.Arr.from(problems.map(_.toJson)))
}

object SchemaValidity {
  def fromJson(value: ujson.Value): SchemaValidity = SchemaValidity(
    ValidityState.values.find(_.wire == value("state").str).getOrElse(ValidityState.Invalid),
    value("entityVersion").num.toLong,
    value("etag").str,
    java.time.Instant.parse(value("checkedAt").str).toEpochMilli,
    value("problems").arr.toSeq.map(Problem.fromJson))
}

/**
 * Letzter projizierter Stand einer Entity (`entitystate:<uuid>`). olliSchema braucht ihn, weil
 * `EntityPatched` nur einen Merge-Patch traegt und Neuvalidierungen ohne Aufruf beim EntityService
 * auskommen sollen.
 */
final case class EntityState(id: UUID, tenant: UUID, project: UUID, kind: UUID, schema: String, data: ujson.Value,
                             entityVersion: Long, registeredAt: Long) {
  def toJson: ujson.Value = ujson.Obj(
    "id" -> id.toString, "tenant" -> tenant.toString, "project" -> project.toString, "kind" -> kind.toString,
    "schema" -> schema, "data" -> data, "entityVersion" -> ujson.Num(entityVersion.toDouble),
    "registeredAt" -> ujson.Num(registeredAt.toDouble))
}

object EntityState {
  def fromJson(value: ujson.Value): EntityState = EntityState(
    UUID.fromString(value("id").str), UUID.fromString(value("tenant").str), UUID.fromString(value("project").str),
    UUID.fromString(value("kind").str), value("schema").str, value("data"), value("entityVersion").num.toLong,
    value("registeredAt").num.toLong)
}

/** Zustand der Entity-Validierung nach ADR-0064. */
enum EntityValidityState(val wire: String) {
  case Pending extends EntityValidityState("pending")
  case Valid extends EntityValidityState("valid")
  case Invalid extends EntityValidityState("invalid")
  case Stale extends EntityValidityState("stale")
  case SchemaMissing extends EntityValidityState("schema-missing")
}

final case class ValidationError(pointer: String, keyword: String, message: String) {
  def toJson: ujson.Value = ujson.Obj("pointer" -> pointer, "keyword" -> keyword, "message" -> message)
}

object ValidationError {
  def fromJson(value: ujson.Value): ValidationError = ValidationError(value("pointer").str, value("keyword").str, value("message").str)
}

/**
 * Kompaktes Entity-Validierungsergebnis (`entityvalidity:<uuid>`, ADR-0064): erste N Fehler, volle
 * Listen werden bei Bedarf neu berechnet.
 */
final case class EntityValidity(
  entityId: UUID,
  state: EntityValidityState,
  schemaUrl: String,
  schemaUuid: Option[UUID],
  schemaVersion: Option[Int],
  schemaEntityVersion: Option[Long],
  entityVersion: Long,
  errorCount: Int,
  errors: Seq[ValidationError],
  checkedAt: Long,
  detail: Option[String] = None,
  /** Die Fehlerzaehlung hat bei `validation.max-errors` abgebrochen; die wahre Zahl ist groesser. */
  errorCountTruncated: Boolean = false
) {
  def toJson: ujson.Value = {
    val obj = ujson.Obj(
      "entityId" -> entityId.toString,
      "state" -> state.wire,
      "schemaUrl" -> schemaUrl,
      "entityVersion" -> ujson.Num(entityVersion.toDouble),
      "errorCount" -> errorCount,
      "errors" -> ujson.Arr.from(errors.map(_.toJson)),
      "checkedAt" -> Json.instant(checkedAt))
    schemaUuid.foreach(value => obj("schemaUuid") = value.toString)
    schemaVersion.foreach(value => obj("schemaVersion") = value)
    schemaEntityVersion.foreach(value => obj("schemaEntityVersion") = ujson.Num(value.toDouble))
    detail.foreach(value => obj("detail") = value)
    if (errorCountTruncated) obj("errorCountTruncated") = true
    obj
  }
}

object EntityValidity {
  def fromJson(value: ujson.Value): EntityValidity = EntityValidity(
    UUID.fromString(value("entityId").str),
    EntityValidityState.values.find(_.wire == value("state").str).getOrElse(EntityValidityState.Pending),
    value("schemaUrl").str,
    value.obj.get("schemaUuid").map(entry => UUID.fromString(entry.str)),
    value.obj.get("schemaVersion").map(_.num.toInt),
    value.obj.get("schemaEntityVersion").map(_.num.toLong),
    value("entityVersion").num.toLong,
    value("errorCount").num.toInt,
    value("errors").arr.toSeq.map(ValidationError.fromJson),
    java.time.Instant.parse(value("checkedAt").str).toEpochMilli,
    value.obj.get("detail").map(_.str),
    value.obj.get("errorCountTruncated").exists(_.bool))
}

/**
 * Version einer Entity aus Sicht der Projektion.
 *
 * Die Entity-Events tragen bisher keine Sequenznummer. Der Kafka-Key ist die Entity-ID, alle Events
 * einer Entity liegen also in derselben Partition; ihr Offset waechst je Entity streng monoton und ist
 * bei einem Replay derselbe. Bis der EntityService eine Versionsnummer publiziert, ist der Offset die
 * Entity-Version (Abweichung, siehe doc/schema-service.md). Eine Aenderung der Partitionszahl von
 * `entity.events` verlangt deshalb eine Reprojektion.
 */
object EntityVersion
