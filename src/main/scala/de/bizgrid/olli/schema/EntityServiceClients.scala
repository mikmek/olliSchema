package de.bizgrid.olli.schema

import java.net.URLEncoder
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.time.Duration
import java.util.UUID
import scala.util.Try

/** Verwender eines Schemas laut EntityIndex (REQ-SCH-006). */
final case class SchemaUsage(count: Int, entities: Seq[UUID], truncated: Boolean)

/** Fragt den EntityIndex nach Verwendern eines Schemas und nach dem Stand einer Entity. */
trait EntityIndexPort {
  def usage(schemaUrls: Seq[String]): Either[String, SchemaUsage]
  def entity(id: UUID): Option[EntityState]
}

/**
 * HTTP-Client gegen den EntityIndex (`GET /entities?tag=schema:<url>`, `GET /entities/{id}`). Die
 * Tag-Abfrage ist ein exakter Vergleich des `schema`-Propertys; Verwender werden deshalb je Adresse
 * des Schemas (abgeleitete URL, by-id, Original-URI) abgefragt und vereinigt.
 */
final class HttpEntityIndex(baseUrl: String, pageSize: Int, maxEntities: Int, timeout: Duration) extends EntityIndexPort {
  private val client = HttpClient.newBuilder().connectTimeout(timeout).followRedirects(HttpClient.Redirect.NEVER).build()

  private def get(path: String): Either[String, (Int, String)] = Try {
    val request = HttpRequest.newBuilder(java.net.URI.create(baseUrl.stripSuffix("/") + path)).timeout(timeout).GET().build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    (response.statusCode(), response.body())
  }.toEither.left.map(error => s"EntityIndex not reachable: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}")

  def usage(schemaUrls: Seq[String]): Either[String, SchemaUsage] = {
    val found = scala.collection.mutable.LinkedHashSet[UUID]()
    var truncated = false
    val failure = schemaUrls.distinct.iterator.map { url =>
      var offset = 0
      var done = false
      var error: Option[String] = None
      while (!done && error.isEmpty) {
        val tag = URLEncoder.encode(s"schema:$url", StandardCharsets.UTF_8)
        get(s"/entities?tag=$tag&offset=$offset&limit=$pageSize") match {
          case Left(problem) => error = Some(problem)
          case Right((200, body)) =>
            val page = Try(ujson.read(body).arr.toSeq.map(entry => UUID.fromString(entry("id").str))).getOrElse(Nil)
            found ++= page
            offset += page.size
            if (page.size < pageSize) done = true
            else if (found.size >= maxEntities) { truncated = true; done = true }
          case Right((status, _)) => error = Some(s"EntityIndex answered $status")
        }
      }
      error
    }.collectFirst { case Some(problem) => problem }
    failure.toLeft(SchemaUsage(found.size, found.toSeq.take(maxEntities), truncated))
  }

  def entity(id: UUID): Option[EntityState] =
    get(s"/entities/$id").toOption.collect { case (200, body) => body }.flatMap { body =>
      Try {
        val entity = ujson.read(body)("entity")
        EntityState(id, UUID.fromString(entity("tenant").str), UUID.fromString(entity("project").str), UUID.fromString(entity("kind").str),
          entity("schema").str, entity("data"), 0L, entity.obj.get("registeredAt").map(_.num.toLong).getOrElse(0L))
      }.toOption
    }
}

/** Schreibt Schema-Entities in die EntityRegistry (Fremdschema-Import, REQ-SCH-013). */
trait EntityRegistryPort {
  def create(body: ujson.Value, principal: UUID): Either[String, UUID]
}

final class HttpEntityRegistry(baseUrl: String, timeout: Duration) extends EntityRegistryPort {
  private val client = HttpClient.newBuilder().connectTimeout(timeout).followRedirects(HttpClient.Redirect.NEVER).build()

  def create(body: ujson.Value, principal: UUID): Either[String, UUID] = Try {
    val request = HttpRequest.newBuilder(java.net.URI.create(baseUrl.stripSuffix("/") + "/entities"))
      .timeout(timeout)
      .header("Content-Type", "application/json")
      .header("X-Olli-Principal", principal.toString)
      .POST(HttpRequest.BodyPublishers.ofString(ujson.write(body)))
      .build()
    client.send(request, HttpResponse.BodyHandlers.ofString())
  }.toEither.left.map(error => s"EntityRegistry not reachable: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}")
    .flatMap { response =>
      if (response.statusCode() == 201 || response.statusCode() == 200)
        Try(UUID.fromString(response.body().trim.stripPrefix("\"").stripSuffix("\""))).toEither.left.map(_ => s"Unexpected EntityRegistry answer '${response.body()}'")
      else Left(s"EntityRegistry answered ${response.statusCode()}: ${response.body().take(300)}")
    }
}
