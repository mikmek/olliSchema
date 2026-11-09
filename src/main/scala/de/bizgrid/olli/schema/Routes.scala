package de.bizgrid.olli.schema

import de.bizgrid.olli.schema.Json.*
import org.apache.pekko.http.scaladsl.model.headers.{Accept, EntityTag, RawHeader}
import org.apache.pekko.http.scaladsl.model.{HttpEntity, HttpHeader, HttpResponse, MediaTypes, StatusCode, StatusCodes}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.{Directive1, Route}

import java.util.UUID
import java.util.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

/** Snapshot dessen, was /health/ready meldet; Funktionen, damit jede Abfrage den aktuellen Stand liest. */
final case class Readiness(store: () => Boolean, consumer: () => Boolean)

/**
 * HTTP-Schnittstelle von olliSchema. Vertrag: `doc/olli-schema-api.openapi.yaml`.
 *
 * Alle Aufrufe, die Redis lesen oder validieren, laufen auf dem `blocking`-Kontext. Er ist vom
 * Kafka-Consumer und der gedrosselten Neuvalidierung getrennt, damit interaktive Aufrufe nicht hinter
 * Batch-Arbeit warten (ADR-0063).
 */
final class Routes(catalog: Catalog, checker: Checker, projection: Projection, entityIndex: EntityIndexPort,
                   importer: Option[SchemaImporter], metrics: SchemaMetrics, readiness: Readiness, limits: Limits,
                   apiTimeout: FiniteDuration, timeoutAfter: FiniteDuration => Future[Unit])(using blocking: ExecutionContext) {
  val ServiceName = "olli-schema"
  private val Immutable = RawHeader("Cache-Control", "public, max-age=31536000, immutable")
  private val NoCache = RawHeader("Cache-Control", "no-cache")
  private val NilUuid = new UUID(0L, 0L)

  val route: Route = concat(
    path("health" / "live") { get { complete(ujson.Obj("status" -> "ok", "service" -> ServiceName)) } },
    path("health" / "ready") { get {
      val store = readiness.store()
      val consumer = readiness.consumer()
      val ready = store && consumer
      val body = ujson.Obj("status" -> (if (ready) "ready" else "unavailable"), "service" -> ServiceName,
        "redis" -> store, "consumer" -> consumer)
      if (ready) complete(body) else complete((StatusCodes.ServiceUnavailable, body))
    } },
    ServiceMetrics.route(ServiceName)(metrics.snapshot ++ Seq(
      "schema_revalidation_pending" -> Try(projection.pendingRevalidations).getOrElse(-1L),
      "schema_compiled_cache_size" -> checker.compiledCacheSize.toLong)),

    // Oeffentlicher Namensraum (ADR-0059): https://bizgrid.de/ollischema/<pfad> -> GET /ollischema/<pfad>.
    pathPrefix("ollischema") { extractUnmatchedPath { rest => get {
      val url = catalog.urls.fromPath(rest.toString)
      withRecord(catalog.resolve(url)) { record => serve(record) }
    } } },

    pathPrefix("schemas") { concat(
      pathEndOrSingleSlash { get { parameters("application".optional, "domain".optional, "status".optional, "branch".optional) {
        (application, domain, status, branch) =>
          respond {
            val items = catalog.list(application, domain)
              .filter(record => status.forall(value => record.info.exists(_.status.wire == value)))
              .filter(record => branch.forall(value => record.info.flatMap(_.branch).contains(value)))
            ok(ujson.Obj("items" -> ujson.Arr.from(items.map(summary))))
          }
      } } },
      pathPrefix(JavaUUID) { uuid => concat(
        pathEndOrSingleSlash { get { withRecord(catalog.record(uuid)) { record => serve(record) } } },
        path("validity") { get { respond {
          catalog.record(uuid).fold(notFound(uuid)) { record =>
            ok(catalog.validity(uuid).getOrElse(checker.checkSchema(record)).toJson)
          }
        } } },
        path("referers") { get { respond {
          catalog.record(uuid).fold(notFound(uuid)) { record =>
            metrics.refererQueries.incrementAndGet()
            val schemas = catalog.referers(uuid).map(summary)
            val body = ujson.Obj("uuid" -> uuid.toString, "schemas" -> ujson.Arr.from(schemas))
            entityIndex.usage(catalog.addresses(record)) match {
              case Right(usage) =>
                body("entities") = ujson.Obj("count" -> usage.count, "truncated" -> usage.truncated,
                  "items" -> ujson.Arr.from(usage.entities.map(id => ujson.Str(id.toString))))
              case Left(problem) =>
                body("entities") = ujson.Null
                body("entitiesError") = problem
            }
            ok(body)
          }
        } } },
        path("release-check") { get { parameters("entityVersion".as[Long]) { expected => respond {
          metrics.releaseChecks.incrementAndGet()
          catalog.record(uuid).fold(notFound(uuid)) { record =>
            if (record.entityVersion < expected)
              json(StatusCodes.Conflict, ujson.Obj("error" -> "projection-behind", "projectedVersion" -> ujson.Num(record.entityVersion.toDouble),
                "message" -> s"olliSchema has projected version ${record.entityVersion}, not yet $expected; retry"))
            else if (record.entityVersion > expected)
              json(StatusCodes.Conflict, ujson.Obj("error" -> "version-mismatch", "projectedVersion" -> ujson.Num(record.entityVersion.toDouble),
                "message" -> s"The entity has moved on to version ${record.entityVersion}"))
            else ok(checker.releaseCheck(record, catalog.validity(uuid)).toJson)
          }
        } } } },
        path("meta") { get { parameters("type".optional) { kind => respond {
          catalog.record(uuid).fold(notFound(uuid)) { record =>
            kind.map(value => MetaType.fromWire(value).toRight(value)) match {
              case Some(Left(value)) => json(StatusCodes.BadRequest, Json.error(s"Unknown meta-schema type '$value'"))
              case other =>
                val metaType = other.flatMap(_.toOption)
                val items = record.url.toSeq.flatMap(url => catalog.metaSchemas(url, metaType)) ++ catalog.metaSchemas(catalog.urls.byId(uuid), metaType)
                ok(ujson.Obj("items" -> ujson.Arr.from(items.distinctBy(_.uuid).map(summary))))
            }
          }
        } } } }
      ) }
    ) },

    // REQ-SCH-009: Validierung von noch nicht gespeichertem JSON gegen ein angegebenes Schema.
    path("validate") { post { entity(as[String]) { body => respondWithin {
      Try(ujson.read(body)).toOption.filter(_.objOpt.nonEmpty) match {
        case None => json(StatusCodes.BadRequest, Json.error("Body must be a JSON object with 'schema' and 'data'"))
        case Some(request) =>
          (request.obj.get("schema").flatMap(_.strOpt), request.obj.get("data")) match {
            case (Some(schema), Some(data)) =>
              metrics.apiValidations.incrementAndGet()
              val maxErrors = request.obj.get("maxErrors").flatMap(_.numOpt).map(_.toInt).filter(_ > 0).fold(limits.maxErrors)(math.min(_, limits.maxErrors))
              checker.validateData(schema, data, maxErrors) match {
                case Left(NotValidated(state, detail)) => json(StatusCodes.UnprocessableContent, ujson.Obj("state" -> state.wire, "schemaUrl" -> schema,
                  "error" -> detail))
                case Right((record, outcome)) => ok(outcomeJson(schema, record, outcome))
              }
            case _ => json(StatusCodes.BadRequest, Json.error("Body must contain 'schema' (URL) and 'data'"))
          }
      }
    } } } },

    pathPrefix("entities" / JavaUUID / "validation") { id => pathEndOrSingleSlash { concat(
      get { parameters("full".as[Boolean].withDefault(false)) { full => respondWithin {
        if (!full) catalog.entityValidity(id).fold(json(StatusCodes.NotFound, Json.error(s"No validation result for entity $id")))(v => ok(v.toJson))
        else catalog.entityState(id).filterNot(projection.isSchemaEntity) match {
          case None => json(StatusCodes.NotFound, Json.error(s"Entity $id is not tracked by olliSchema"))
          case Some(state) =>
            // Volle Fehlerliste wird neu berechnet, nicht gespeichert (ADR-0064).
            metrics.apiValidations.incrementAndGet()
            checker.validateData(state.schema, state.data, limits.maxErrors) match {
              case Left(NotValidated(missing, detail)) => ok(ujson.Obj("entityId" -> id.toString, "state" -> missing.wire, "schemaUrl" -> state.schema,
                "entityVersion" -> ujson.Num(state.entityVersion.toDouble), "detail" -> detail))
              case Right((record, outcome)) =>
                val body = outcomeJson(state.schema, record, outcome)
                body("entityId") = id.toString
                body("entityVersion") = ujson.Num(state.entityVersion.toDouble)
                ok(body)
            }
        }
      } } },
      post { respondWithin {
        projection.revalidate(id).fold(json(StatusCodes.NotFound, Json.error(s"Entity $id is not tracked by olliSchema")))(v => ok(v.toJson))
      } }
    ) } },

    path("imports") { post { optionalHeaderValueByName("X-Olli-Principal") { principal => entity(as[String]) { body => respondWithin {
      importer match {
        case None => json(StatusCodes.Forbidden, Json.error("Schema import is disabled (ollischema.import.allowed-principals is empty)"))
        case Some(service) =>
          val caller = principal.flatMap(value => Try(UUID.fromString(value.trim)).toOption)
          val request = Try(ujson.read(body)).toOption.flatMap(_.objOpt)
          (caller, request) match {
            case (None, _) => json(StatusCodes.Forbidden, Json.error("X-Olli-Principal is required for imports"))
            case (Some(user), _) if !service.allowed(user) => json(StatusCodes.Forbidden, Json.error("Principal has no import permission"))
            case (_, None) => json(StatusCodes.BadRequest, Json.error("Body must be a JSON object"))
            case (Some(user), Some(fields)) =>
              def uuidField(name: String) = fields.get(name).flatMap(_.strOpt).flatMap(value => Try(UUID.fromString(value)).toOption).getOrElse(NilUuid)
              (fields.get("uri").flatMap(_.strOpt), fields.get("publisher").flatMap(_.strOpt)) match {
                case (Some(uri), Some(publisher)) =>
                  service.importSchema(uri, publisher, uuidField("tenant"), uuidField("project"), user) match {
                    case Right(items) => json(StatusCodes.Accepted, ujson.Obj("items" -> ujson.Arr.from(items.map(_.toJson))))
                    case Left(problem) => json(StatusCodes.UnprocessableContent, Json.error(problem))
                  }
                case _ => json(StatusCodes.BadRequest, Json.error("Body must contain 'uri' and 'publisher'"))
              }
          }
      }
    } } } } },

    pathPrefix("diagnostics") { concat(
      path("violations") { get { parameters("limit".as[Int].withDefault(100)) { limit => respond {
        ok(ujson.Obj("items" -> ujson.Arr.from(catalog.store.recent(Keys.Violations, limit.max(1).min(1000)).map(ujson.read(_)))))
      } } } },
      path("dead-letters") { get { parameters("limit".as[Int].withDefault(100)) { limit => respond {
        ok(ujson.Obj("items" -> ujson.Arr.from(catalog.store.recent(Keys.DeadLetters, limit.max(1).min(1000)).map(ujson.read(_)))))
      } } } }
    ) }
  )

  // ---------------------------------------------------------------------------------------------

  private def ok(value: ujson.Value): HttpResponse = json(StatusCodes.OK, value)

  private def json(status: StatusCode, value: ujson.Value): HttpResponse =
    HttpResponse(status, entity = HttpEntity(org.apache.pekko.http.scaladsl.model.ContentTypes.`application/json`, ujson.write(value)))

  private def notFound(uuid: UUID): HttpResponse = json(StatusCodes.NotFound, Json.error(s"Schema $uuid is not known"))

  private def respond(response: => HttpResponse): Route = onComplete(Future(response)(using blocking)) {
    case Success(value) => complete(value)
    case Failure(error) => complete(json(StatusCodes.ServiceUnavailable, Json.error(s"Request failed: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}")))
  }

  /** Wie `respond`, aber mit Laufzeitgrenze je Validierung (ADR-0069). */
  private def respondWithin(response: => HttpResponse): Route = {
    val work = Future(response)(using blocking)
    val limit = Promise[HttpResponse]()
    timeoutAfter(apiTimeout).foreach(_ => limit.tryFailure(new TimeoutException(s"Validation exceeded $apiTimeout")))(using blocking)
    onComplete(Future.firstCompletedOf(Seq(work, limit.future))(using blocking)) {
      case Success(value) => complete(value)
      case Failure(error: TimeoutException) => complete(json(StatusCodes.ServiceUnavailable, Json.error(error.getMessage)))
      case Failure(error) => complete(json(StatusCodes.ServiceUnavailable, Json.error(s"Request failed: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}")))
    }
  }

  private def withRecord(lookup: => Option[SchemaRecord]): Directive1[SchemaRecord] =
    onComplete(Future(lookup)(using blocking)).flatMap {
      case Success(Some(record)) => provide(record)
      case Success(None) => complete(json(StatusCodes.NotFound, Json.error("Schema is not known")))
      case Failure(error) => complete(json(StatusCodes.ServiceUnavailable, Json.error(s"Read model unavailable: ${error.getMessage}")))
    }

  /**
   * Auslieferung eines Schemas (REQ-SCH-002): `application/schema+json` liefert das Dokument,
   * `application/json` die Huelle mit Metadaten und Gueltigkeit. Released ist `immutable`, Drafts sind
   * `no-cache` mit ETag (ADR-0059).
   */
  private def serve(record: SchemaRecord): Route = optionalHeaderValueByType(Accept) { accept =>
    // Nur ein ausdrueckliches application/json waehlt die Huelle; */* und schema+json liefern das Dokument.
    val envelope = accept.exists { header =>
      val explicit = header.mediaRanges.filterNot(_.isWildcard).map(_.value.takeWhile(_ != ';').trim.toLowerCase)
      explicit.contains(MediaTypes.`application/json`.value) && !explicit.contains(Json.SchemaJson.value)
    }
    val validity = Try(catalog.validity(record.uuid)).toOption.flatten
    val cache: HttpHeader = if (record.isImmutable) Immutable else NoCache
    val headers = Seq[HttpHeader](cache, RawHeader("Vary", "Accept"), RawHeader("X-Olli-Schema-Uuid", record.uuid.toString)) ++
      validity.map(value => RawHeader("X-Olli-Schema-Validity", value.state.wire)).toSeq ++
      record.info.map(info => RawHeader("X-Olli-Schema-Status", info.status.wire)).toSeq
    respondWithHeaders(headers) {
      conditional(EntityTag(if (envelope) s"${record.etag}-env" else record.etag)) {
        if (envelope) {
          val body = summary(record)
          body("document") = record.document
          body("refs") = ujson.Arr.from(record.refs.map(ujson.Str(_)))
          body("infoProblems") = ujson.Arr.from(record.infoProblems.map(_.toJson))
          validity.foreach(value => body("validity") = value.toJson)
          complete(ok(body))
        } else complete(HttpResponse(entity = HttpEntity(Json.SchemaJsonContentType, ujson.write(record.document))))
      }
    }
  }

  private def summary(record: SchemaRecord): ujson.Obj = {
    val obj = ujson.Obj(
      "uuid" -> record.uuid.toString,
      "byIdUrl" -> catalog.urls.byId(record.uuid),
      "language" -> record.language,
      "entityVersion" -> ujson.Num(record.entityVersion.toDouble),
      "etag" -> record.etag,
      "tenant" -> record.tenant.toString,
      "project" -> record.project.toString,
      "registeredAt" -> Json.instant(record.registeredAt))
    record.url.foreach(value => obj("url") = value)
    record.info.foreach(value => obj("info") = value.toJson)
    Try(catalog.validity(record.uuid)).toOption.flatten.foreach(value => obj("validityState") = value.state.wire)
    obj
  }

  private def outcomeJson(schemaUrl: String, record: SchemaRecord, outcome: ValidationOutcome): ujson.Obj = {
    val obj = ujson.Obj(
      "state" -> (if (outcome.valid) EntityValidityState.Valid.wire else EntityValidityState.Invalid.wire),
      "schemaUrl" -> schemaUrl,
      "schemaUuid" -> record.uuid.toString,
      "schemaEntityVersion" -> ujson.Num(record.entityVersion.toDouble),
      "errorCount" -> outcome.errorCount,
      "errors" -> ujson.Arr.from(outcome.errors.map(_.toJson)))
    record.info.flatMap(_.internal).foreach(identity => obj("schemaVersion") = identity.version)
    if (outcome.truncated) obj("errorCountTruncated") = true
    outcome.aborted.foreach(reason => obj("detail") = s"Not fully validated: $reason")
    obj
  }
}
