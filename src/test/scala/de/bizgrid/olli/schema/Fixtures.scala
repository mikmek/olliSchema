package de.bizgrid.olli.schema

import java.util.UUID
import scala.concurrent.duration.*

/** Gemeinsamer Aufbau: In-Memory-Lesemodell, echter JSON-Schema-Validator, keine Netzwerkzugriffe. */
final class Fixtures {
  val SchemaKind: UUID = UUID.fromString("5c4e3a10-0000-4000-8000-000000000001")
  val DataKind: UUID = UUID.fromString("11111111-0000-4000-8000-000000000001")
  val Tenant: UUID = UUID.fromString("22222222-0000-4000-8000-000000000001")
  val Project: UUID = UUID.fromString("33333333-0000-4000-8000-000000000001")
  val Ns = "https://bizgrid.de/ollischema/"

  val limits: Limits = Limits(storedErrors = 3, maxErrors = 100, maxDocumentBytes = 1 << 20, maxReferenceDepth = 8,
    maxReferencedSchemas = 50, regexTimeout = 200.millis, compiledCacheSize = 50)
  val metrics = new SchemaMetrics
  val store = new InMemorySchemaStore
  val catalog = new Catalog(store, new SchemaUrls(Ns))
  val checker = new Checker(catalog, Map(SchemaLanguage.JsonSchema202012 -> new JsonSchemaSupport(limits.regexTimeout)), limits, metrics)
  val projection = new Projection(catalog, checker, Set(SchemaKind), metrics)

  private var offset = 0L
  def nextOffset(): Long = { offset += 1; offset }

  def info(application: String = "rave", domain: String = "building", className: String = "room", version: Int = 1,
           branch: Option[String] = None, extra: Seq[(String, ujson.Value)] = Nil): ujson.Obj = {
    val obj = ujson.Obj("application" -> application, "domain" -> domain, "class" -> className, "version" -> version)
    branch.foreach(value => obj("branch") = value)
    extra.foreach((key, value) => obj(key) = value)
    obj
  }

  def schemaData(info: ujson.Value, document: ujson.Value): ujson.Value = ujson.Obj("schemaInfo" -> info, "document" -> document)

  def createSchema(id: UUID, info: ujson.Value, document: ujson.Value): Long = {
    val version = nextOffset()
    projection.apply(EntityEvent.Created(id, Tenant, Project, SchemaKind, SchemaLanguage.JsonSchema202012, schemaData(info, document), version), version)
    version
  }

  def updateSchema(id: UUID, info: ujson.Value, document: ujson.Value): Long = {
    val version = nextOffset()
    projection.apply(EntityEvent.Updated(id, EntityChange(None, None, None, None, Some(schemaData(info, document))), version), version)
    version
  }

  def createEntity(id: UUID, schema: String, data: ujson.Value): Long = {
    val version = nextOffset()
    projection.apply(EntityEvent.Created(id, Tenant, Project, DataKind, schema, data, version), version)
    version
  }

  def patchEntity(id: UUID, patch: ujson.Value): Long = {
    val version = nextOffset()
    projection.apply(EntityEvent.Patched(id, EntityChange(None, None, None, None, Some(patch)), version), version)
    version
  }

  val roomDocument: ujson.Value = ujson.Obj(
    "$schema" -> SchemaLanguage.JsonSchema202012,
    "type" -> "object",
    "required" -> ujson.Arr("Name"),
    "properties" -> ujson.Obj(
      "Nr" -> ujson.Obj("type" -> "string"),
      "Name" -> ujson.Obj("type" -> "string", "maxLength" -> 20),
      "Cover" -> ujson.Obj("type" -> "string", "format" -> "uuid"),
      "Area" -> ujson.Obj("type" -> "number", "minimum" -> 0)))

  def validity(id: UUID): SchemaValidity = catalog.validity(id).getOrElse(sys.error(s"no validity for $id"))
}
