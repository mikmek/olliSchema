package de.bizgrid.olli.schema

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.UUID

class ModelSpec extends AnyFunSuite with Matchers {
  private val urls = new SchemaUrls("https://bizgrid.de/ollischema/")
  private val tenant = UUID.fromString("22222222-0000-4000-8000-000000000001")

  private def parse(info: ujson.Value) = SchemaInfo.parse(ujson.Obj("schemaInfo" -> info, "document" -> ujson.Obj()), tenant)

  test("derives released, draft, tenant, by-id and external URLs (ADR-0059)") {
    val released = InternalIdentity("rave", "building", "room", 1, None, None)
    urls.url(released) shouldBe "https://bizgrid.de/ollischema/rave/building/room-v1.json"
    urls.url(released.copy(version = 12, branch = Some("main"))) shouldBe "https://bizgrid.de/ollischema/rave/building/room-v12-main.json"
    urls.url(released.copy(tenant = Some(tenant))) shouldBe s"https://bizgrid.de/ollischema/rave/tenant/$tenant/building/room-v1.json"
    urls.releasedUrl(released.copy(version = 3, branch = Some("x"))) shouldBe "https://bizgrid.de/ollischema/rave/building/room-v3.json"
    val id = UUID.randomUUID()
    urls.byId(id) shouldBe s"https://bizgrid.de/ollischema/by-id/$id"
    urls.byIdUuid(urls.byId(id)) shouldBe Some(id)
    urls.url(ExternalIdentity("buildingsmart", "x.org/ifc/ifc.json", "https://x.org/ifc/ifc.json")) shouldBe
      "https://bizgrid.de/ollischema/ext/buildingsmart/x.org/ifc/ifc.json"
  }

  test("a branch means draft, no branch means released") {
    parse(FixturesInfo.room(branch = Some("main"))).map(_._1.status) shouldBe Right(SchemaStatus.Draft)
    parse(FixturesInfo.room()).map(_._1.status) shouldBe Right(SchemaStatus.Released)
    parse(FixturesInfo.room(extra = Seq("status" -> ujson.Str("deprecated")))).map(_._1.status) shouldBe Right(SchemaStatus.Deprecated)
    parse(FixturesInfo.room(extra = Seq("status" -> ujson.Str("draft")))).left.map(_.map(_.code)) shouldBe Left(Seq("missing-branch"))
  }

  test("enforces the naming rules: lowercase, no hyphen, reserved names") {
    parse(FixturesInfo.room(className = "Room")).left.map(_.map(_.code)) shouldBe Left(Seq("invalid-name"))
    parse(FixturesInfo.room(branch = Some("feature-x"))).left.map(_.map(_.code)) shouldBe Left(Seq("invalid-name"))
    parse(FixturesInfo.room(domain = "tenant")).left.map(_.map(_.code)) shouldBe Left(Seq("reserved-name"))
    parse(FixturesInfo.room(application = "ext")).left.map(_.map(_.code)) shouldBe Left(Seq("reserved-name"))
    parse(FixturesInfo.room(version = 0)).left.map(_.map(_.code)) shouldBe Left(Seq("invalid-version"))
  }

  test("tenant-scoped schemas take the tenant of the entity") {
    parse(FixturesInfo.room(extra = Seq("tenantScoped" -> ujson.True))).map(_._1.internal.flatMap(_.tenant)) shouldBe Right(Some(tenant))
  }

  test("extracts references relative to the base and ignores data keywords") {
    val document = ujson.Obj(
      "$id" -> "https://bizgrid.de/ollischema/rave/building/room-v1.json",
      "properties" -> ujson.Obj(
        "address" -> ujson.Obj("$ref" -> "../common/address-v2.json#/$defs/street"),
        "self" -> ujson.Obj("$ref" -> "#/$defs/local"),
        "abs" -> ujson.Obj("$ref" -> "https://example.org/x.json"),
        "data" -> ujson.Obj("const" -> ujson.Obj("$ref" -> "not-a-reference.json"))),
      "$defs" -> ujson.Obj("local" -> ujson.Obj("type" -> "string")))
    References.externalTargets(document, "https://bizgrid.de/ollischema/rave/building/room-v1.json") shouldBe
      Right(Seq("https://bizgrid.de/ollischema/rave/common/address-v2.json", "https://example.org/x.json"))
  }

  test("JSON merge patch follows RFC 7396") {
    Json.mergePatch(ujson.Obj("a" -> 1, "b" -> ujson.Obj("c" -> 2, "d" -> 3)), ujson.Obj("b" -> ujson.Obj("c" -> ujson.Null), "e" -> 5)) shouldBe
      ujson.Obj("a" -> 1, "b" -> ujson.Obj("d" -> 3), "e" -> 5)
  }

  test("records round-trip through their JSON form") {
    val record = SchemaRecord(UUID.randomUUID(), Some("https://bizgrid.de/ollischema/a/b/c-v1-main.json"),
      Some(SchemaInfo(InternalIdentity("a", "b", "c", 1, Some("main"), None), SchemaStatus.Draft, Some(0),
        Some(MetaBinding(MetaType.Layout, "https://bizgrid.de/ollischema/a/b/d-v1.json")))),
      Seq(Problem("x", "y", Some("/z"), warning = true)), SchemaLanguage.JsonSchema202012, ujson.Obj("type" -> "object"), 7, tenant, tenant, 9,
      Seq("https://example.org/r.json"))
    SchemaRecord.fromJson(ujson.read(ujson.write(record.toJson))) shouldBe record
  }
}

object FixturesInfo {
  def room(application: String = "rave", domain: String = "building", className: String = "room", version: Int = 1,
           branch: Option[String] = None, extra: Seq[(String, ujson.Value)] = Nil): ujson.Obj = {
    val obj = ujson.Obj("application" -> application, "domain" -> domain, "class" -> className, "version" -> version)
    branch.foreach(value => obj("branch") = value)
    extra.foreach((key, value) => obj(key) = value)
    obj
  }
}
