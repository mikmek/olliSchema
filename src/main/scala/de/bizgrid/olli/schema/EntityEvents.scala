package de.bizgrid.olli.schema

import de.bizgrid.olli.event.{OlliCbor, OlliEventEnvelope}

import java.util.UUID
import scala.util.Try

/** Felder eines Update- oder Patch-Events; `None` bedeutet unveraendert. */
final case class EntityChange(tenant: Option[UUID], project: Option[UUID], kind: Option[UUID], schema: Option[String], data: Option[ujson.Value])

/** Fachliche Sicht von olliSchema auf `entity.events`, Payload-Version 2. */
enum EntityEvent {
  case Created(id: UUID, tenant: UUID, project: UUID, kind: UUID, schema: String, data: ujson.Value, registeredAt: Long)
  case Updated(id: UUID, change: EntityChange, registeredAt: Long)
  case Patched(id: UUID, change: EntityChange, registeredAt: Long)
  case Deleted(id: UUID, registeredAt: Long)

  def id: UUID
}

/**
 * Eigener Leser fuer den Vertrag von `entity.events` (Envelope nach ADR-0020, Payload-Version 2 aus
 * olliEntity). olliSchema teilt keinen Code mit olliEntity; liest nur die Felder, die es braucht.
 */
object EntityEvents {
  val AggregateType = "entity"
  val SchemaVersion = 2

  def decode(bytes: Array[Byte]): Either[String, (OlliEventEnvelope, EntityEvent)] = {
    import OlliEventEnvelope.given
    for {
      envelope <- OlliCbor.decode[OlliEventEnvelope](bytes).left.map(_.message)
      _ <- Either.cond(envelope.aggregateType == AggregateType, (), s"Unexpected aggregate type '${envelope.aggregateType}'")
      _ <- Either.cond(envelope.schemaVersion == SchemaVersion, (), s"Unsupported entity event version ${envelope.schemaVersion}")
      event <- fromPayload(envelope.eventType, envelope.payload)
    } yield (envelope, event)
  }

  def fromPayload(eventType: String, value: ujson.Value): Either[String, EntityEvent] = Try {
    eventType match {
      case "EntityCreated" => EntityEvent.Created(uuid(value, "id"), uuid(value, "tenant"), uuid(value, "project"), uuid(value, "kind"),
        value("schema").str, value("data"), value("registeredAt").num.toLong)
      case "EntityUpdated" => EntityEvent.Updated(uuid(value, "id"), change(value("update")), value("registeredAt").num.toLong)
      case "EntityPatched" => EntityEvent.Patched(uuid(value, "id"), change(value("patch")), value("registeredAt").num.toLong)
      case "EntityDeleted" => EntityEvent.Deleted(uuid(value, "id"), value("registeredAt").num.toLong)
      case other => throw new IllegalArgumentException(s"Unsupported entity event type '$other'")
    }
  }.toEither.left.map(error => Option(error.getMessage).getOrElse(error.getClass.getSimpleName))

  private def uuid(value: ujson.Value, field: String): UUID = UUID.fromString(value(field).str)

  private def optional(value: ujson.Value, field: String): Option[ujson.Value] = value.obj.get(field).filterNot(_ == ujson.Null)

  private def change(value: ujson.Value): EntityChange = EntityChange(
    optional(value, "tenant").map(entry => UUID.fromString(entry.str)),
    optional(value, "project").map(entry => UUID.fromString(entry.str)),
    optional(value, "kind").map(entry => UUID.fromString(entry.str)),
    optional(value, "schema").map(_.str),
    optional(value, "data"))
}
