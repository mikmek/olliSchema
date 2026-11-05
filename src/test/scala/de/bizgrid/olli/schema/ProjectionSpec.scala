package de.bizgrid.olli.schema

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.UUID

class ProjectionSpec extends AnyFunSuite with Matchers {

  private def uuid() = UUID.randomUUID()

  test("projects a schema entity, derives URL and $id, and stores its validity (REQ-SCH-001, -002, -007)") {
    val f = new Fixtures
    val id = uuid()
    f.createSchema(id, f.info(), f.roomDocument)
    val record = f.catalog.record(id).get
    record.url shouldBe Some(s"${f.Ns}rave/building/room-v1.json")
    record.document("$id").str shouldBe s"${f.Ns}rave/building/room-v1.json"
    f.catalog.uuidFor(s"${f.Ns}rave/building/room-v1.json") shouldBe Some(id)
    f.catalog.uuidFor(s"${f.Ns}by-id/$id") shouldBe Some(id)
    f.validity(id).state shouldBe ValidityState.Valid
    f.catalog.list(Some("rave"), Some("building")).map(_.uuid) shouldBe Seq(id)
  }

  test("reports metaschema errors and missing metadata as invalid") {
    val f = new Fixtures
    val broken = uuid()
    f.createSchema(broken, f.info(), ujson.Obj("type" -> 17))
    f.validity(broken).state shouldBe ValidityState.Invalid
    f.validity(broken).problems.map(_.code) should contain("metaschema")

    val noInfo = uuid()
    val version = f.nextOffset()
    f.projection.apply(EntityEvent.Created(noInfo, f.Tenant, f.Project, f.SchemaKind, SchemaLanguage.JsonSchema202012, ujson.Obj("type" -> "object"), version), version)
    f.catalog.record(noInfo).flatMap(_.url) shouldBe None
    f.validity(noInfo).problems.map(_.code) should contain("missing-schema-info")
  }

  test("is idempotent per entity version") {
    val f = new Fixtures
    val id = uuid()
    val version = f.createSchema(id, f.info(), f.roomDocument)
    f.projection.apply(EntityEvent.Deleted(id, 0), version) shouldBe false
    f.catalog.record(id) should not be empty
    f.metrics.eventsDuplicate.get() shouldBe 1
  }

  test("an unresolved reference becomes valid once the target is projected (REQ-SCH-005, ADR-0062)") {
    val f = new Fixtures
    val room = uuid()
    f.createSchema(room, f.info(), ujson.Obj("properties" -> ujson.Obj("address" -> ujson.Obj("$ref" -> "../common/address-v1.json#/$defs/street"))))
    f.validity(room).state shouldBe ValidityState.Unresolved

    val address = uuid()
    f.createSchema(address, f.info(domain = "common", className = "address"),
      ujson.Obj("$defs" -> ujson.Obj("street" -> ujson.Obj("type" -> "string"))))
    f.validity(room).state shouldBe ValidityState.Valid
    f.catalog.referers(address).map(_.uuid) shouldBe Seq(room)
  }

  test("a released schema must not reference a draft, a draft may reference drafts of its branch") {
    val f = new Fixtures
    val draftTarget = uuid()
    f.createSchema(draftTarget, f.info(domain = "common", className = "address", branch = Some("main")), ujson.Obj("type" -> "object"))
    val released = uuid()
    f.createSchema(released, f.info(), ujson.Obj("properties" -> ujson.Obj("a" -> ujson.Obj("$ref" -> "../common/address-v1-main.json"))))
    f.validity(released).state shouldBe ValidityState.Invalid
    f.validity(released).problems.map(_.code) should contain("references-draft")

    val draft = uuid()
    f.createSchema(draft, f.info(className = "floor", branch = Some("main")), ujson.Obj("properties" -> ujson.Obj("a" -> ujson.Obj("$ref" -> "../common/address-v1-main.json"))))
    f.validity(draft).state shouldBe ValidityState.Valid

    val otherBranch = uuid()
    f.createSchema(otherBranch, f.info(className = "site", branch = Some("other")), ujson.Obj("properties" -> ujson.Obj("a" -> ujson.Obj("$ref" -> "../common/address-v1-main.json"))))
    f.validity(otherBranch).state shouldBe ValidityState.Valid
    f.validity(otherBranch).problems.map(_.code) shouldBe Seq("cross-branch-reference")
  }

  test("a released schema is immutable; only follow-up states are applied (REQ-SCH-003)") {
    val f = new Fixtures
    val id = uuid()
    f.createSchema(id, f.info(), f.roomDocument)
    f.updateSchema(id, f.info(), ujson.Obj("type" -> "string"))
    f.catalog.record(id).get.document("type").str shouldBe "object"
    f.metrics.immutabilityViolations.get() shouldBe 1
    f.store.recent(Keys.Violations, 10) should have size 1

    f.updateSchema(id, f.info(extra = Seq("status" -> ujson.Str("deprecated"))), f.roomDocument)
    f.catalog.record(id).get.info.get.status shouldBe SchemaStatus.Deprecated
    f.catalog.resolve(s"${f.Ns}rave/building/room-v1.json").map(_.uuid) shouldBe Some(id)

    f.projection.apply(EntityEvent.Deleted(id, 0), f.nextOffset())
    f.catalog.record(id) should not be empty
  }

  test("a URL belongs to exactly one UUID") {
    val f = new Fixtures
    val first = uuid()
    val second = uuid()
    f.createSchema(first, f.info(), f.roomDocument)
    f.createSchema(second, f.info(), f.roomDocument)
    f.catalog.uuidFor(s"${f.Ns}rave/building/room-v1.json") shouldBe Some(first)
    f.catalog.record(second).get.url shouldBe None
    f.validity(second).problems.map(_.code) should contain("url-conflict")
  }

  test("retargeting a draft moves its URL") {
    val f = new Fixtures
    val id = uuid()
    f.createSchema(id, f.info(version = 2, branch = Some("main")), f.roomDocument)
    f.updateSchema(id, f.info(version = 3, branch = Some("main")), f.roomDocument)
    f.catalog.uuidFor(s"${f.Ns}rave/building/room-v2-main.json") shouldBe None
    f.catalog.uuidFor(s"${f.Ns}rave/building/room-v3-main.json") shouldBe Some(id)
  }

  test("validates entities asynchronously and never rejects them (REQ-SCH-008)") {
    val f = new Fixtures
    val schema = uuid()
    f.createSchema(schema, f.info(), f.roomDocument)
    val ok = uuid()
    f.createEntity(ok, s"${f.Ns}rave/building/room-v1.json", ujson.Obj("Name" -> "Kitchen"))
    f.catalog.entityValidity(ok).get.state shouldBe EntityValidityState.Valid
    f.catalog.entityValidity(ok).get.schemaVersion shouldBe Some(1)

    val bad = uuid()
    f.createEntity(bad, s"${f.Ns}rave/building/room-v1.json", ujson.Obj("Area" -> -1, "Name" -> 3))
    val result = f.catalog.entityValidity(bad).get
    result.state shouldBe EntityValidityState.Invalid
    result.errorCount shouldBe 2
    result.errors.map(_.pointer).toSet shouldBe Set("/Area", "/Name")

    f.patchEntity(bad, ujson.Obj("Area" -> 4, "Name" -> "Bath"))
    f.catalog.entityValidity(bad).get.state shouldBe EntityValidityState.Valid
  }

  test("entities against a subschema, by-id URLs and unknown URLs") {
    val f = new Fixtures
    val schema = uuid()
    f.createSchema(schema, f.info(domain = "common", className = "types"),
      ujson.Obj("$defs" -> ujson.Obj("code" -> ujson.Obj("type" -> "string", "pattern" -> "^[A-Z]{2}$"))))
    val fragment = uuid()
    f.createEntity(fragment, s"${f.Ns}rave/common/types-v1.json#/$$defs/code", ujson.Str("de"))
    f.catalog.entityValidity(fragment).get.state shouldBe EntityValidityState.Invalid

    val byId = uuid()
    f.createEntity(byId, s"${f.Ns}by-id/$schema", ujson.Obj())
    f.catalog.entityValidity(byId).get.state shouldBe EntityValidityState.Valid

    val missing = uuid()
    f.createEntity(missing, s"${f.Ns}rave/building/unknown-v1.json", ujson.Obj())
    f.catalog.entityValidity(missing).get.state shouldBe EntityValidityState.SchemaMissing

    val urn = uuid()
    f.createEntity(urn, "urn:example:schema:test", ujson.Obj())
    f.catalog.entityValidity(urn) shouldBe None
    f.catalog.entityState(urn) shouldBe None
  }

  test("draft changes mark entities stale and the throttled queue revalidates them (ADR-0061)") {
    val f = new Fixtures
    val draft = uuid()
    f.createSchema(draft, f.info(branch = Some("main")), f.roomDocument)
    val entity = uuid()
    f.createEntity(entity, s"${f.Ns}rave/building/room-v1-main.json", ujson.Obj("Name" -> "Kitchen"))
    f.catalog.entityValidity(entity).get.state shouldBe EntityValidityState.Valid

    f.updateSchema(draft, f.info(branch = Some("main")), ujson.Obj("type" -> "object", "required" -> ujson.Arr("Nr")))
    f.catalog.entityValidity(entity).get.state shouldBe EntityValidityState.Stale
    f.projection.pendingRevalidations shouldBe 1
    f.projection.drainRevalidation(10) shouldBe 1
    f.catalog.entityValidity(entity).get.state shouldBe EntityValidityState.Invalid
  }

  test("a schema that appears later revalidates entities waiting for it") {
    val f = new Fixtures
    val entity = uuid()
    f.createEntity(entity, s"${f.Ns}rave/building/room-v1.json", ujson.Obj("Name" -> "Kitchen"))
    f.catalog.entityValidity(entity).get.state shouldBe EntityValidityState.SchemaMissing
    f.createSchema(uuid(), f.info(), f.roomDocument)
    f.projection.drainRevalidation(10)
    f.catalog.entityValidity(entity).get.state shouldBe EntityValidityState.Valid
  }

  test("release check: valid, next version, pins same-branch drafts whose release exists (REQ-SCH-010)") {
    val f = new Fixtures
    f.createSchema(uuid(), f.info(domain = "common", className = "address"), ujson.Obj("type" -> "object"))
    val addressDraft = uuid()
    f.createSchema(addressDraft, f.info(domain = "common", className = "address", branch = Some("main")), ujson.Obj("type" -> "object"))
    val room = uuid()
    val version = f.createSchema(room, f.info(branch = Some("main")),
      ujson.Obj("properties" -> ujson.Obj("a" -> ujson.Obj("$ref" -> "../common/address-v1-main.json"))))

    val check = f.checker.releaseCheck(f.catalog.record(room).get, f.catalog.validity(room))
    check.record.entityVersion shouldBe version
    check.problems shouldBe Nil
    check.nextVersion shouldBe Some(1)
    check.pins shouldBe Seq(s"${f.Ns}rave/common/address-v1-main.json" -> s"${f.Ns}rave/common/address-v1.json")
    check.releasedUrl shouldBe Some(s"${f.Ns}rave/building/room-v1.json")

    // Nach einem Release von v1 ist die Zielversion des Branches veraltet.
    f.createSchema(uuid(), f.info(), f.roomDocument)
    val outdated = f.checker.releaseCheck(f.catalog.record(room).get, f.catalog.validity(room))
    outdated.problems.map(_.code) shouldBe Seq("target-version-outdated")
    outdated.nextVersion shouldBe Some(2)
  }

  test("release check refuses dependencies that are not released yet") {
    val f = new Fixtures
    f.createSchema(uuid(), f.info(domain = "common", className = "address", branch = Some("main")), ujson.Obj("type" -> "object"))
    val room = uuid()
    f.createSchema(room, f.info(branch = Some("main")), ujson.Obj("properties" -> ujson.Obj("a" -> ujson.Obj("$ref" -> "../common/address-v1-main.json"))))
    f.checker.releaseCheck(f.catalog.record(room).get, f.catalog.validity(room)).problems.map(_.code) shouldBe Seq("dependency-not-released")
  }

  test("meta-schemas report selectors that do not exist in the target (REQ-SCH-011)") {
    val f = new Fixtures
    f.createSchema(uuid(), f.info(), f.roomDocument)
    val target = s"${f.Ns}rave/building/room-v1.json"
    val layout = uuid()
    f.createSchema(layout, f.info(className = "roomlayout", extra = Seq("metaType" -> ujson.Str("layout"), "target" -> ujson.Str(target))),
      ujson.Obj("tree" -> ujson.Obj("nodeText" -> "{$.Nr} – {$.Name}"), "card" -> ujson.Obj("title" -> "$.Name", "image" -> "$.Cover", "badge" -> "$.Floor")))
    val problems = f.validity(layout).problems
    problems.map(_.code) shouldBe Seq("unknown-selector")
    problems.head.pointer shouldBe Some("/card/badge")
    f.catalog.metaSchemas(target, Some(MetaType.Layout)).map(_.uuid) shouldBe Seq(layout)
  }

  test("a deleted draft leaves its referers unresolved") {
    val f = new Fixtures
    val address = uuid()
    f.createSchema(address, f.info(domain = "common", className = "address", branch = Some("main")), ujson.Obj("type" -> "object"))
    val room = uuid()
    f.createSchema(room, f.info(branch = Some("main")), ujson.Obj("properties" -> ujson.Obj("a" -> ujson.Obj("$ref" -> "../common/address-v1-main.json"))))
    f.validity(room).state shouldBe ValidityState.Valid
    f.projection.apply(EntityEvent.Deleted(address, 0), f.nextOffset())
    f.catalog.record(address) shouldBe None
    f.validity(room).state shouldBe ValidityState.Unresolved
  }

  test("external schemas keep their $id and resolve through the alias (REQ-SCH-013)") {
    val f = new Fixtures
    val external = uuid()
    val original = "https://standards.example.org/ifc/v4/wall.json"
    val document = ujson.Obj("$id" -> original, "type" -> "object", "properties" -> ujson.Obj("Height" -> ujson.Obj("type" -> "number")))
    val version = f.nextOffset()
    f.projection.apply(EntityEvent.Created(external, f.Tenant, f.Project, f.SchemaKind, SchemaLanguage.JsonSchema202012,
      ujson.Obj("schemaInfo" -> ujson.Obj("external" -> ujson.Obj("publisher" -> "example", "originalUri" -> original,
        "path" -> "standards.example.org/ifc/v4/wall.json")), "document" -> document), version), version)
    val record = f.catalog.record(external).get
    record.document("$id").str shouldBe original
    record.url shouldBe Some(s"${f.Ns}ext/example/standards.example.org/ifc/v4/wall.json")
    f.catalog.uuidFor(original) shouldBe Some(external)
    f.validity(external).state shouldBe ValidityState.Valid

    val user = uuid()
    f.createSchema(user, f.info(), ujson.Obj("properties" -> ujson.Obj("wall" -> ujson.Obj("$ref" -> original))))
    f.validity(user).state shouldBe ValidityState.Valid
    val entity = uuid()
    f.createEntity(entity, original, ujson.Obj("Height" -> "high"))
    f.catalog.entityValidity(entity).get.state shouldBe EntityValidityState.Invalid
  }
}
