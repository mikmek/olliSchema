package de.bizgrid.olli.schema

import java.io.InputStream
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.{Inet4Address, Inet6Address, InetAddress, URI}
import java.nio.charset.StandardCharsets
import java.time.Duration
import java.util.UUID
import scala.collection.mutable
import scala.util.Try

final case class ImportSettings(
  allowedPrincipals: Set[UUID],
  allowedHosts: Seq[String],
  allowHttp: Boolean,
  maxBytes: Int,
  connectTimeout: Duration,
  requestTimeout: Duration,
  maxDocuments: Int,
  origin: UUID,
  schemaKind: UUID
)

/** Laedt ein Fremdschema; ausschliesslich ueber den expliziten Import (REQ-SCH-016). */
trait SchemaFetcher {
  def fetch(uri: URI): Either[String, ujson.Value]
}

/**
 * Netzzugriff des Imports mit den Schutzmassnahmen aus ADR-0069: Allowlist der Hosts, nur https
 * (http nur nach Konfiguration), Standardports, keine Redirects, keine Ziele in internen
 * Adressbereichen (SSRF), Groessen- und Zeitlimit.
 *
 * Grenze: Der HTTP-Client loest den Namen fuer die Verbindung erneut auf. Gegen DNS-Rebinding zwischen
 * Pruefung und Verbindung schuetzt die Allowlist, nicht die Adresspruefung.
 */
final class GuardedHttpFetcher(settings: ImportSettings, resolveHost: String => Seq[InetAddress] = host => InetAddress.getAllByName(host).toSeq)
    extends SchemaFetcher {
  private val client = HttpClient.newBuilder()
    .connectTimeout(settings.connectTimeout)
    .followRedirects(HttpClient.Redirect.NEVER)
    .build()

  def check(uri: URI): Either[String, Unit] = {
    val scheme = Option(uri.getScheme).map(_.toLowerCase).getOrElse("")
    val host = Option(uri.getHost).map(_.toLowerCase).getOrElse("")
    if (!uri.isAbsolute || host.isEmpty) Left(s"'$uri' is not an absolute URI with a host")
    else if (scheme != "https" && !(scheme == "http" && settings.allowHttp)) Left(s"Scheme '$scheme' is not allowed")
    else if (Option(uri.getRawUserInfo).nonEmpty) Left("User info in import URIs is not allowed")
    else if (uri.getPort != -1 && uri.getPort != 443 && uri.getPort != 80) Left(s"Port ${uri.getPort} is not allowed")
    else if (!GuardedHttpFetcher.hostAllowed(host, settings.allowedHosts)) Left(s"Host '$host' is not on the import allowlist")
    else Try(resolveHost(host)).toEither.left.map(_ => s"Host '$host' cannot be resolved").flatMap { addresses =>
      addresses.find(GuardedHttpFetcher.isInternal) match {
        case Some(address) => Left(s"Host '$host' resolves to the internal address ${address.getHostAddress}")
        case None if addresses.isEmpty => Left(s"Host '$host' cannot be resolved")
        case None => Right(())
      }
    }
  }

  def fetch(uri: URI): Either[String, ujson.Value] = check(uri).flatMap { _ =>
    Try {
      val request = HttpRequest.newBuilder(uri).timeout(settings.requestTimeout)
        .header("Accept", "application/schema+json, application/json").GET().build()
      client.send(request, HttpResponse.BodyHandlers.ofInputStream())
    }.toEither.left.map(error => s"Fetching $uri failed: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}").flatMap { response =>
      val body = response.body()
      try {
        if (response.statusCode() >= 300 && response.statusCode() < 400) Left(s"$uri answered ${response.statusCode()}; redirects are not followed")
        else if (response.statusCode() != 200) Left(s"$uri answered ${response.statusCode()}")
        else readLimited(body, settings.maxBytes).flatMap(text => Try(ujson.read(text)).toEither.left.map(_ => s"$uri did not return JSON"))
      } finally body.close()
    }
  }

  private def readLimited(input: InputStream, max: Int): Either[String, String] = {
    val bytes = input.readNBytes(max + 1)
    if (bytes.length > max) Left(s"Document exceeds the import limit of $max bytes")
    else Right(new String(bytes, StandardCharsets.UTF_8))
  }
}

object GuardedHttpFetcher {
  def hostAllowed(host: String, allowed: Seq[String]): Boolean =
    allowed.map(_.trim.toLowerCase).filter(_.nonEmpty).exists(entry =>
      if (entry.startsWith(".")) host.endsWith(entry) || host == entry.drop(1) else host == entry)

  /** Loopback, Link-local, private Netze, CGNAT, Multicast, unspezifiziert und IPv6-ULA. */
  def isInternal(address: InetAddress): Boolean = {
    val bytes = address.getAddress
    address.isLoopbackAddress || address.isAnyLocalAddress || address.isLinkLocalAddress || address.isSiteLocalAddress ||
      address.isMulticastAddress || (address match {
        case _: Inet4Address => (bytes(0) & 0xff) == 100 && (bytes(1) & 0xc0) == 64 || (bytes(0) & 0xff) == 0
        case _: Inet6Address => (bytes(0) & 0xfe) == 0xfc
        case _ => false
      })
  }
}

final case class ImportedSchema(originalUri: String, url: String, entityId: UUID, outcome: String) {
  def toJson: ujson.Value = ujson.Obj("originalUri" -> originalUri, "url" -> url, "entityId" -> entityId.toString, "outcome" -> outcome)
}

/**
 * Import von Fremdschemas (REQ-SCH-013, ADR-0062): unveraendert abgelegt (Original-`$id` bleibt), als
 * Schema-Entity im EntityService registriert, transitive Abhaengigkeiten werden mitimportiert. Der
 * Alias Original-URI -> UUID entsteht in der Projektion, sobald das Event zurueckkommt.
 */
final class SchemaImporter(settings: ImportSettings, fetcher: SchemaFetcher, registry: EntityRegistryPort, catalog: Catalog, metrics: SchemaMetrics) {
  private val inFlight = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()

  def allowed(principal: UUID): Boolean = settings.allowedPrincipals(principal)

  def importSchema(uri: String, publisher: String, tenant: UUID, project: UUID, principal: UUID): Either[String, Seq[ImportedSchema]] = {
    if (!allowed(principal)) return Left("forbidden")
    SchemaInfo.nameProblems("publisher", publisher).headOption match {
      case Some(problem) => return Left(problem.message)
      case None => ()
    }
    val results = Seq.newBuilder[ImportedSchema]
    val queue = mutable.Queue(References.stripFragment(uri.trim))
    val seen = mutable.Set[String]()
    var failure: Option[String] = None
    while (queue.nonEmpty && failure.isEmpty) {
      val current = queue.dequeue()
      if (!seen(current)) {
        seen += current
        if (seen.size > settings.maxDocuments) failure = Some(s"Import exceeds ${settings.maxDocuments} documents")
        else catalog.uuidFor(current) match {
          case Some(existing) =>
            results += ImportedSchema(current, catalog.record(existing).flatMap(_.url).getOrElse(catalog.urls.byId(existing)), existing, "already-imported")
          case None if catalog.urls.isOwn(current) =>
            failure = Some(s"$current is in the olliSchema namespace but not in the registry")
          case None if !inFlight.add(current) =>
            results += ImportedSchema(current, "", UUID.nameUUIDFromBytes(current.getBytes(StandardCharsets.UTF_8)), "in-progress")
          case None =>
            try {
              importOne(current, publisher, tenant, project, principal) match {
                case Left(problem) => failure = Some(problem)
                case Right((imported, dependencies)) =>
                  results += imported
                  dependencies.filterNot(seen).foreach(queue.enqueue)
              }
            } finally inFlight.remove(current)
        }
      }
    }
    failure match {
      case Some(problem) =>
        metrics.importFailures.incrementAndGet()
        Left(problem)
      case None =>
        metrics.imports.incrementAndGet()
        Right(results.result())
    }
  }

  private def importOne(uri: String, publisher: String, tenant: UUID, project: UUID, principal: UUID): Either[String, (ImportedSchema, Seq[String])] =
    for {
      parsed <- Try(URI.create(uri)).toEither.left.map(_ => s"'$uri' is not a valid URI")
      document <- fetcher.fetch(parsed)
      _ <- Either.cond(document.objOpt.nonEmpty, (), s"$uri is not a JSON Schema object")
      path = SchemaImporter.externalPath(parsed)
      identity = ExternalIdentity(publisher, path, uri)
      language = document.obj.get("$schema").flatMap(_.strOpt).map(SchemaLanguage.normalize).getOrElse(SchemaLanguage.JsonSchema202012)
      entityId = UUID.nameUUIDFromBytes(s"ollischema:ext:$uri".getBytes(StandardCharsets.UTF_8))
      body = ujson.Obj(
        "id" -> entityId.toString, "tenant" -> tenant.toString, "project" -> project.toString,
        "kind" -> settings.schemaKind.toString, "schema" -> language, "context" -> "ollischema-import",
        "data" -> ujson.Obj("schemaInfo" -> ujson.Obj("external" -> ujson.Obj(
          "publisher" -> publisher, "originalUri" -> uri, "path" -> path)), "document" -> document),
        "origin" -> settings.origin.toString)
      created <- registry.create(body, principal)
      base = document.obj.get("$id").flatMap(_.strOpt).flatMap(id => References.resolve(uri, id)).getOrElse(uri)
      dependencies = References.externalTargets(document, base).getOrElse(Nil)
    } yield (ImportedSchema(uri, catalog.urls.url(identity), created, "registered"), dependencies)
}

object SchemaImporter {
  private val Unsafe = "[^a-z0-9_.~]".r

  /** Lokaler Pfad unter `ext/<publisher>/`: Host und Pfad der Original-URI, kleingeschrieben und bereinigt. */
  def externalPath(uri: URI): String = {
    val host = Option(uri.getHost).getOrElse("unknown").toLowerCase
    val rawPath = Option(uri.getPath).getOrElse("")
    val segments = (host +: rawPath.split('/').toSeq.filter(_.nonEmpty))
      .map(segment => Unsafe.replaceAllIn(segment.toLowerCase, "_"))
      .map(segment => if (segment == "." || segment == "..") "_" else segment)
    val path = segments.mkString("/")
    if (rawPath.isEmpty || rawPath.endsWith("/")) s"$path/index.json" else path
  }
}
