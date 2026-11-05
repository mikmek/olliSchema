package de.bizgrid.olli.schema

import org.apache.pekko.http.scaladsl.model.headers.{Accept, EntityTag, `If-None-Match`, RawHeader}
import org.apache.pekko.http.scaladsl.model.{ContentTypes, HttpEntity, MediaRange, MediaTypes, StatusCodes}
import org.apache.pekko.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.net.{InetAddress, URI}
import java.time.Duration
import java.util.UUID
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}

class RoutesSpec extends AnyFunSuite with Matchers with ScalatestRouteTest {
  private val principal = UUID.fromString("44444444-0000-4000-8000-000000000001")

  private final class FakeIndex extends EntityIndexPort {
    def usage(schemaUrls: Seq[String]): Either[String, SchemaUsage] = Right(SchemaUsage(2, Seq(UUID.randomUUID(), UUID.randomUUID()), truncated = false))
    def entity(id: UUID): Option[EntityState] = None
  }

  private final class Setup(importer: Fixtures => Option[SchemaImporter] = _ => None) {
    val f = new Fixtures
    given ExecutionContext = ExecutionContext.global
    val routes = new Routes(f.catalog, f.checker, f.projection, new FakeIndex, importer(f), f.metrics,
      Readiness(() => true, () => false), f.limits, 5.seconds, _ => Future.never)
    val route = routes.route
  }

  test("health and metrics follow the platform standard") {
    val s = new Setup()
    Get("/health/live") ~> s.route ~> check { status shouldBe StatusCodes.OK }
    Get("/health/ready") ~> s.route ~> check {
      status shouldBe StatusCodes.ServiceUnavailable
      ujson.read(responseAs[String])("consumer").bool shouldBe false
    }
    Get("/metrics") ~> s.route ~> check {
      val body = ujson.read(responseAs[String])
      body("service").str shouldBe "olli-schema"
      body("status").str shouldBe "ok"
      body("metrics").obj.keySet should contain("schema_events_processed_total")
    }
  }

  test("serves released schemas as immutable application/schema+json (REQ-SCH-002, ADR-0059)") {
    val s = new Setup()
    val id = UUID.randomUUID()
    s.f.createSchema(id, s.f.info(), s.f.roomDocument)
    Get("/ollischema/rave/building/room-v1.json") ~> s.route ~> check {
      status shouldBe StatusCodes.OK
      contentType.mediaType.value shouldBe "application/schema+json"
      header("Cache-Control").get.value should include("immutable")
      header("X-Olli-Schema-Validity").get.value shouldBe "valid"
      ujson.read(responseAs[String])("$id").str shouldBe "https://bizgrid.de/ollischema/rave/building/room-v1.json"
    }
    Get(s"/ollischema/by-id/$id") ~> s.route ~> check { status shouldBe StatusCodes.OK }
    Get(s"/schemas/$id") ~> Accept(MediaRange(MediaTypes.`application/json`)) ~> s.route ~> check {
      val body = ujson.read(responseAs[String])
      body("validity")("state").str shouldBe "valid"
      body("info")("class").str shouldBe "room"
    }
    Get("/ollischema/rave/building/nothing-v1.json") ~> s.route ~> check { status shouldBe StatusCodes.NotFound }
  }

  test("serves drafts with no-cache and answers If-None-Match with 304") {
    val s = new Setup()
    val id = UUID.randomUUID()
    s.f.createSchema(id, s.f.info(branch = Some("main")), s.f.roomDocument)
    val etag = s.f.catalog.record(id).get.etag
    Get("/ollischema/rave/building/room-v1-main.json") ~> s.route ~> check {
      header("Cache-Control").get.value shouldBe "no-cache"
      header("ETag").get.value shouldBe s"\"$etag\""
    }
    Get("/ollischema/rave/building/room-v1-main.json") ~> `If-None-Match`(EntityTag(etag)) ~> s.route ~> check {
      status shouldBe StatusCodes.NotModified
    }
  }

  test("validates unsaved JSON and returns full error lists (REQ-SCH-009)") {
    val s = new Setup()
    s.f.createSchema(UUID.randomUUID(), s.f.info(), s.f.roomDocument)
    val request = ujson.Obj("schema" -> "https://bizgrid.de/ollischema/rave/building/room-v1.json", "data" -> ujson.Obj("Area" -> -2))
    Post("/validate", HttpEntity(ContentTypes.`application/json`, ujson.write(request))) ~> s.route ~> check {
      status shouldBe StatusCodes.OK
      val body = ujson.read(responseAs[String])
      body("state").str shouldBe "invalid"
      body("errorCount").num shouldBe 2
    }
    val missing = ujson.Obj("schema" -> "https://bizgrid.de/ollischema/x/y/z-v1.json", "data" -> ujson.Obj())
    Post("/validate", HttpEntity(ContentTypes.`application/json`, ujson.write(missing))) ~> s.route ~> check {
      status shouldBe StatusCodes.UnprocessableContent
    }
  }

  test("entity validation, referers and release check") {
    val s = new Setup()
    val schema = UUID.randomUUID()
    val version = s.f.createSchema(schema, s.f.info(branch = Some("main")), s.f.roomDocument)
    val entity = UUID.randomUUID()
    s.f.createEntity(entity, "https://bizgrid.de/ollischema/rave/building/room-v1-main.json", ujson.Obj())
    Get(s"/entities/$entity/validation") ~> s.route ~> check {
      ujson.read(responseAs[String])("state").str shouldBe "invalid"
    }
    Get(s"/entities/$entity/validation?full=true") ~> s.route ~> check {
      ujson.read(responseAs[String])("errors").arr.head("keyword").str shouldBe "required"
    }
    Get(s"/schemas/$schema/referers") ~> s.route ~> check {
      ujson.read(responseAs[String])("entities")("count").num shouldBe 2
    }
    Get(s"/schemas/$schema/release-check?entityVersion=$version") ~> s.route ~> check {
      ujson.read(responseAs[String])("releasable").bool shouldBe true
    }
    Get(s"/schemas/$schema/release-check?entityVersion=${version + 1}") ~> s.route ~> check {
      status shouldBe StatusCodes.Conflict
      ujson.read(responseAs[String])("error").str shouldBe "projection-behind"
    }
  }

  test("imports require an allowed principal") {
    val settings = ImportSettings(Set(principal), Seq("standards.example.org"), allowHttp = false, 1000, Duration.ofSeconds(1),
      Duration.ofSeconds(1), 5, UUID.randomUUID(), UUID.fromString("5c4e3a10-0000-4000-8000-000000000001"))
    val fetcher = new SchemaFetcher { def fetch(uri: URI) = Right(ujson.Obj("$id" -> uri.toString, "type" -> "object")) }
    val registry = new EntityRegistryPort { def create(body: ujson.Value, principal: UUID) = Right(UUID.fromString(body("id").str)) }
    val s = new Setup(f => Some(new SchemaImporter(settings, fetcher, registry, f.catalog, f.metrics)))
    val body = HttpEntity(ContentTypes.`application/json`, """{"uri":"https://standards.example.org/a.json","publisher":"example"}""")
    Post("/imports", body) ~> s.route ~> check { status shouldBe StatusCodes.Forbidden }
    Post("/imports", body) ~> RawHeader("X-Olli-Principal", UUID.randomUUID().toString) ~> s.route ~> check { status shouldBe StatusCodes.Forbidden }
    Post("/imports", body) ~> RawHeader("X-Olli-Principal", principal.toString) ~> s.route ~> check {
      status shouldBe StatusCodes.Accepted
      ujson.read(responseAs[String])("items").arr.head("url").str shouldBe
        "https://bizgrid.de/ollischema/ext/example/standards.example.org/a.json"
    }
  }
}

class ImporterSpec extends AnyFunSuite with Matchers {
  private val settings = ImportSettings(Set.empty, Seq("standards.example.org", ".buildingsmart.org"), allowHttp = false, 100,
    Duration.ofSeconds(1), Duration.ofSeconds(1), 5, UUID.randomUUID(), UUID.randomUUID())

  private def guard(addresses: String*) = new GuardedHttpFetcher(settings, _ => addresses.map(InetAddress.getByName))

  test("SSRF guard: allowlist, scheme, port, user info and internal addresses (ADR-0069)") {
    guard("93.184.216.34").check(URI.create("https://standards.example.org/a.json")) shouldBe Right(())
    guard("93.184.216.34").check(URI.create("https://ifc.buildingsmart.org/a.json")) shouldBe Right(())
    guard("93.184.216.34").check(URI.create("https://evil.example.com/a.json")).isLeft shouldBe true
    guard("93.184.216.34").check(URI.create("http://standards.example.org/a.json")).isLeft shouldBe true
    guard("93.184.216.34").check(URI.create("https://standards.example.org:8443/a.json")).isLeft shouldBe true
    guard("93.184.216.34").check(URI.create("https://user@standards.example.org/a.json")).isLeft shouldBe true
    Seq("127.0.0.1", "10.1.2.3", "192.168.0.1", "172.16.5.5", "169.254.169.254", "100.64.0.1", "0.0.0.0", "::1", "fd00::1")
      .foreach(address => guard(address).check(URI.create("https://standards.example.org/a.json")).isLeft shouldBe true)
  }

  test("imports transitive dependencies and derives the local path") {
    val f = new Fixtures
    val documents = Map(
      "https://standards.example.org/ifc/Wall.json" -> ujson.Obj("$id" -> "https://standards.example.org/ifc/Wall.json",
        "properties" -> ujson.Obj("h" -> ujson.Obj("$ref" -> "Length.json"))),
      "https://standards.example.org/ifc/Length.json" -> ujson.Obj("type" -> "number"))
    val created = scala.collection.mutable.Buffer[ujson.Value]()
    val importer = new SchemaImporter(settings.copy(allowedPrincipals = Set(f.Tenant)),
      uri => documents.get(uri.toString).toRight("not found"),
      (body, _) => { created += body; Right(UUID.fromString(body("id").str)) }, f.catalog, f.metrics)
    val result = importer.importSchema("https://standards.example.org/ifc/Wall.json", "example", f.Tenant, f.Project, f.Tenant).toOption.get
    result.map(_.originalUri) shouldBe Seq("https://standards.example.org/ifc/Wall.json", "https://standards.example.org/ifc/Length.json")
    result.head.url shouldBe "https://bizgrid.de/ollischema/ext/example/standards.example.org/ifc/wall.json"
    created.head("data")("document") shouldBe documents("https://standards.example.org/ifc/Wall.json")
    importer.importSchema("https://standards.example.org/ifc/Wall.json", "example", f.Tenant, f.Project, UUID.randomUUID()) shouldBe Left("forbidden")
  }
}
