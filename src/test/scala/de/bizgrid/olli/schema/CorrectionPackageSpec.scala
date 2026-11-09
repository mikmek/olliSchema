package de.bizgrid.olli.schema

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.UUID
import scala.concurrent.duration.*

/** Korrekturpaket SCH-A04 bis SCH-A08 aus dem Pruefbericht 2026-11-08. */
class CorrectionPackageSpec extends AnyFunSuite with Matchers {
  private def uuid() = UUID.randomUUID()
  private val Ns = "https://bizgrid.de/ollischema/"

  private def layout(f: Fixtures, id: UUID, target: String, document: ujson.Value, branch: Option[String] = None): Long =
    f.createSchema(id, f.info(className = "roomlayout", branch = branch,
      extra = Seq("metaType" -> ujson.Str("layout"), "target" -> ujson.Str(target))), document)

  // ---- SCH-A04 -------------------------------------------------------------------------------

  test("SCH-A04: a meta-schema projected before its target becomes valid when the target appears") {
    val f = new Fixtures
    val meta = uuid()
    layout(f, meta, s"${Ns}rave/building/room-v1.json", ujson.Obj("card" -> ujson.Obj("title" -> "$.Name")))
    f.validity(meta).state shouldBe ValidityState.Unresolved
    f.createSchema(uuid(), f.info(), f.roomDocument)
    f.validity(meta).state shouldBe ValidityState.Valid
  }

  test("SCH-A04: a target change re-checks the selectors of bound meta-schemas") {
    val f = new Fixtures
    val target = uuid()
    f.createSchema(target, f.info(branch = Some("main")), f.roomDocument)
    val meta = uuid()
    layout(f, meta, s"${Ns}rave/building/room-v1-main.json", ujson.Obj("card" -> ujson.Obj("image" -> "$.Cover")), branch = Some("main"))
    f.validity(meta).errors shouldBe empty
    f.updateSchema(target, f.info(branch = Some("main")), ujson.Obj("type" -> "object", "properties" -> ujson.Obj("Name" -> ujson.Obj("type" -> "string"))))
    f.validity(meta).problems.map(_.code) should contain("unknown-selector")
  }

  test("SCH-A04: a change in a schema referenced by the target propagates to the meta-schema") {
    val f = new Fixtures
    val address = uuid()
    f.createSchema(address, f.info(domain = "common", className = "address", branch = Some("main")),
      ujson.Obj("properties" -> ujson.Obj("Street" -> ujson.Obj("type" -> "string"))))
    f.createSchema(uuid(), f.info(branch = Some("main")),
      ujson.Obj("properties" -> ujson.Obj("Address" -> ujson.Obj("$ref" -> "../common/address-v1-main.json"))))
    val meta = uuid()
    layout(f, meta, s"${Ns}rave/building/room-v1-main.json", ujson.Obj("card" -> ujson.Obj("subtitle" -> "$.Address.Street")), branch = Some("main"))
    f.validity(meta).errors shouldBe empty
    f.updateSchema(address, f.info(domain = "common", className = "address", branch = Some("main")),
      ujson.Obj("properties" -> ujson.Obj("City" -> ujson.Obj("type" -> "string"))))
    f.validity(meta).problems.map(_.code) should contain("unknown-selector")
  }

  test("SCH-A04: deleting a draft target leaves bound meta-schemas unresolved") {
    val f = new Fixtures
    val target = uuid()
    f.createSchema(target, f.info(branch = Some("main")), f.roomDocument)
    val meta = uuid()
    layout(f, meta, s"${Ns}rave/building/room-v1-main.json", ujson.Obj("card" -> ujson.Obj("title" -> "$.Name")), branch = Some("main"))
    f.projection.apply(EntityEvent.Deleted(target, 0), f.nextOffset())
    f.validity(meta).state shouldBe ValidityState.Unresolved
  }

  // ---- SCH-A05 -------------------------------------------------------------------------------

  private val target = ujson.Obj(
    "type" -> "object",
    "properties" -> ujson.Obj(
      "Name" -> ujson.Obj("type" -> "string"),
      "Tags" -> ujson.Obj("type" -> "array", "items" -> ujson.Obj("type" -> "string")),
      "Rooms" -> ujson.Obj("type" -> "array", "items" -> ujson.Obj("properties" -> ujson.Obj(
        "Area" -> ujson.Obj("type" -> "number"), "Name" -> ujson.Obj("type" -> "string"))))))

  private def codes(expression: String): Seq[String] =
    Selectors.check(Seq(Selector(expression, "/x", Selector.Kind.JsonPath)), s"${Ns}a/b/c-v1.json", target, _ => None, 200.millis).map(_.code)

  test("SCH-A05: every member of a multi-selection must exist") {
    codes("$['Name','Rooms']") shouldBe Nil
    codes("$['Name','Missing']") shouldBe Seq("unknown-selector")
    codes("$..['Area','Nope']") shouldBe Seq("unknown-selector")
    codes("$..['Area','Name']") shouldBe Nil
  }

  test("SCH-A05: queries inside filters are checked against the filtered children and the root") {
    codes("$.Rooms[?@.Area > 10].Name") shouldBe Nil
    codes("$.Rooms[?@.Height > 10]") shouldBe Seq("unknown-selector")
    codes("$.Rooms[?@.Area > $.Missing]") shouldBe Seq("unknown-selector")
    codes("$.Rooms[?@.Area >= 1 && (@.Name == 'Kitchen' || !@.Name)]") shouldBe Nil
    codes("$.Rooms[?length(@.Name) > 3]") shouldBe Nil
    codes("$.Rooms[?match(@.Name, 'K.*')]") shouldBe Nil
    codes("$[?count(@.*) > 1]") shouldBe Nil
  }

  test("SCH-A05: malformed or unsupported constructs are rejected instead of widened") {
    codes("$.Rooms[?@.Area > ]") shouldBe Seq("invalid-selector")
    codes("$.Rooms[?unknown(@.Area)]") shouldBe Seq("invalid-selector")
    codes("$.Rooms[?length(@.Area, 1) > 0]") shouldBe Seq("invalid-selector")
    codes("$.Rooms[?@.Area]extra") shouldBe Seq("invalid-selector")
    codes("$.Rooms[?'literal']") shouldBe Seq("invalid-selector")
    codes("$.Rooms[?@.Area == @.*]") shouldBe Seq("invalid-selector")
    codes("$.Rooms[1:x]") shouldBe Seq("invalid-selector")
    codes("$.Rooms[-0]") shouldBe Seq("invalid-selector")
    codes("$['unterminated]") shouldBe Seq("invalid-selector")
  }

  test("SCH-A05: index and slice require an array, string escapes are decoded") {
    codes("$.Tags[0]") shouldBe Nil
    codes("$.Rooms[1:3].Area") shouldBe Nil
    codes("$.Name[0]") shouldBe Seq("unknown-selector")
    codes("$.Name[1:2]") shouldBe Seq("unknown-selector")
    codes("$['N\\u0061me']") shouldBe Nil
  }

  // ---- SCH-A06 -------------------------------------------------------------------------------

  test("SCH-A06: patternProperties in the selector check run with a time limit") {
    val evil = ujson.Obj("type" -> "object", "patternProperties" -> ujson.Obj("^(.*a){20}$" -> ujson.Obj("type" -> "string")))
    val started = System.nanoTime()
    val problems = Selectors.check(Seq(Selector("$['" + "a" * 36 + "!']", "/x", Selector.Kind.JsonPath)), s"${Ns}a/b/c-v1.json", evil, _ => None, 50.millis)
    (System.nanoTime() - started).nanos should be < 5.seconds
    problems.map(_.code) should contain("selector-regex-timeout")
  }

  // ---- SCH-A07 -------------------------------------------------------------------------------

  test("SCH-A07: instance size is limited for entity validation and the API") {
    val f = new Fixtures(_.copy(maxInstanceBytes = 100))
    f.createSchema(uuid(), f.info(), f.roomDocument)
    val entity = uuid()
    f.createEntity(entity, s"${Ns}rave/building/room-v1.json", ujson.Obj("Name" -> "x" * 200))
    val result = f.catalog.entityValidity(entity).get
    result.state shouldBe EntityValidityState.Invalid
    result.errors.head.keyword shouldBe ValidationOutcome.LimitKeyword
    result.detail.get should include("limit is 100")
  }

  test("SCH-A07: a validation exceeding its time limit is aborted, also in the consumer path") {
    val f = new Fixtures(_.copy(maxValidationTime = 1.millis, maxInstanceBytes = 50 << 20))
    f.createSchema(uuid(), f.info(className = "list"), ujson.Obj("type" -> "array",
      "items" -> ujson.Obj("type" -> "object", "properties" -> ujson.Obj("v" -> ujson.Obj("type" -> "string", "minLength" -> 1)))))
    val entity = uuid()
    val started = System.nanoTime()
    f.createEntity(entity, s"${Ns}rave/building/list-v1.json", ujson.Arr.from((1 to 200000).map(i => ujson.Obj("v" -> i.toString))))
    (System.nanoTime() - started).nanos should be < 30.seconds
    val result = f.catalog.entityValidity(entity).get
    result.detail.get should include("time limit")
    result.errors.head.keyword shouldBe ValidationOutcome.LimitKeyword
  }

  test("SCH-A07: error collection stops at the count limit and reports truncation") {
    val f = new Fixtures(_.copy(maxErrors = 5))
    f.createSchema(uuid(), f.info(className = "list"), ujson.Obj("type" -> "array", "items" -> ujson.Obj("type" -> "string")))
    val entity = uuid()
    f.createEntity(entity, s"${Ns}rave/building/list-v1.json", ujson.Arr.from((1 to 50).map(i => ujson.Num(i))))
    val result = f.catalog.entityValidity(entity).get
    result.errorCount shouldBe 5
    result.errorCountTruncated shouldBe true
    result.errors should have size 3
  }

  test("SCH-A07: reference limits also apply to entity validation") {
    val f = new Fixtures(_.copy(maxReferencedSchemas = 1))
    f.createSchema(uuid(), f.info(domain = "common", className = "c"), ujson.Obj("type" -> "string"))
    f.createSchema(uuid(), f.info(domain = "common", className = "b"), ujson.Obj("$ref" -> "c-v1.json"))
    f.createSchema(uuid(), f.info(domain = "common", className = "a"), ujson.Obj("$ref" -> "b-v1.json"))
    val entity = uuid()
    f.createEntity(entity, s"${Ns}rave/common/a-v1.json", ujson.Str("x"))
    val result = f.catalog.entityValidity(entity).get
    result.state shouldBe EntityValidityState.SchemaMissing
    result.detail.get should include("Reference limit")
  }

  // ---- SCH-A08 -------------------------------------------------------------------------------

  /** Validator, der fuer Dokumente mit `"boom": true` mitten in der Projektion scheitert. */
  private final class Exploding(delegate: SchemaLanguageSupport) extends SchemaLanguageSupport {
    @volatile var armed = true
    def language: String = delegate.language
    def checkDocument(document: ujson.Value, deadline: Long): Seq[Problem] =
      if (armed && document.objOpt.exists(_.contains("boom"))) throw new IllegalStateException("simulated crash")
      else delegate.checkDocument(document, deadline)
    def compile(url: String, document: ujson.Value, resolve: String => Option[ujson.Value], cacheable: Boolean) =
      delegate.compile(url, document, resolve, cacheable)
  }

  test("SCH-A08: a failure in the middle of an event leaves the read model untouched; redelivery succeeds") {
    val exploding = new Exploding(new JsonSchemaSupport(200.millis))
    val f = new Fixtures(languagesOverride = Some(Map(SchemaLanguage.JsonSchema202012 -> exploding)))
    val id = uuid()
    f.createSchema(id, f.info(version = 2, branch = Some("main")), f.roomDocument)
    val before = f.store.get(Keys.doc(id))

    // Retarget nach v3 mit Abbruch: weder die neue URL noch die Freigabe der alten darf sichtbar werden.
    val broken = ujson.Obj("type" -> "object", "boom" -> true)
    val version = f.nextOffset()
    val event = EntityEvent.Updated(id, EntityChange(None, None, None, None, Some(f.schemaData(f.info(version = 3, branch = Some("main")), broken))), version)
    an[IllegalStateException] should be thrownBy f.projection.apply(event, version)
    f.store.get(Keys.doc(id)) shouldBe before
    f.catalog.uuidFor(s"${Ns}rave/building/room-v2-main.json") shouldBe Some(id)
    f.catalog.uuidFor(s"${Ns}rave/building/room-v3-main.json") shouldBe None
    f.store.version(id) should not be Some(version)

    exploding.armed = false
    f.projection.apply(event, version) shouldBe true
    f.catalog.uuidFor(s"${Ns}rave/building/room-v2-main.json") shouldBe None
    f.catalog.uuidFor(s"${Ns}rave/building/room-v3-main.json") shouldBe Some(id)
    f.catalog.list(Some("rave"), Some("building")).map(_.uuid) shouldBe Seq(id)
  }

  test("SCH-A08: a failed delete keeps everything needed to repeat the cleanup") {
    val exploding = new Exploding(new JsonSchemaSupport(200.millis))
    val f = new Fixtures(languagesOverride = Some(Map(SchemaLanguage.JsonSchema202012 -> exploding)))
    exploding.armed = false
    val target = uuid()
    f.createSchema(target, f.info(domain = "common", className = "address", branch = Some("main")), ujson.Obj("type" -> "object"))
    val referer = uuid()
    // Der Referenzierer traegt "boom": seine Neupruefung beim Loeschen des Ziels scheitert.
    f.createSchema(referer, f.info(branch = Some("main")), ujson.Obj("boom" -> true, "properties" -> ujson.Obj("a" -> ujson.Obj("$ref" -> "../common/address-v1-main.json"))))
    exploding.armed = true
    val version = f.nextOffset()
    an[IllegalStateException] should be thrownBy f.projection.apply(EntityEvent.Deleted(target, 0), version)
    f.catalog.entityState(target) should not be empty
    f.catalog.record(target) should not be empty

    exploding.armed = false
    f.projection.apply(EntityEvent.Deleted(target, 0), version) shouldBe true
    f.catalog.record(target) shouldBe None
    f.catalog.entityState(target) shouldBe None
    f.store.members(Keys.All) should not contain target.toString
    f.validity(referer).state shouldBe ValidityState.Unresolved
  }

  test("SCH-A08: a binding taken concurrently aborts the whole commit") {
    val store = new InMemorySchemaStore
    val tx = new TransactionView(store)
    tx.set("schema:doc:x", "doc")
    tx.bindOnce("schema:url:u", "x")
    store.set("schema:url:u", "other")
    tx.commit(Some(UUID.fromString("00000000-0000-0000-0000-0000000000aa") -> 5L)) shouldBe CommitResult.Conflict("schema:url:u")
    store.get("schema:doc:x") shouldBe None
    store.version(UUID.fromString("00000000-0000-0000-0000-0000000000aa")) shouldBe None
  }

  test("SCH-A08: commits are idempotent per entity version") {
    val store = new InMemorySchemaStore
    val entity = UUID.randomUUID()
    store.commit(Seq(StoreOp.Set("k", "1")), Some(entity -> 3L)) shouldBe CommitResult.Applied
    store.commit(Seq(StoreOp.Set("k", "2")), Some(entity -> 3L)) shouldBe CommitResult.Duplicate
    store.get("k") shouldBe Some("1")
  }

  test("SCH-A08: revalidation queue entries leave the queue only together with the new result") {
    val f = new Fixtures
    val draft = uuid()
    f.createSchema(draft, f.info(branch = Some("main")), f.roomDocument)
    val entity = uuid()
    f.createEntity(entity, s"${Ns}rave/building/room-v1-main.json", ujson.Obj("Name" -> "Kitchen"))
    f.updateSchema(draft, f.info(branch = Some("main")), ujson.Obj("type" -> "object", "required" -> ujson.Arr("Nr")))
    f.store.sample(Keys.Revalidate, 10) shouldBe Seq(entity.toString)
    f.store.sample(Keys.Revalidate, 10) shouldBe Seq(entity.toString)
    f.projection.drainRevalidation(10) shouldBe 1
    f.projection.pendingRevalidations shouldBe 0
    f.catalog.entityValidity(entity).get.state shouldBe EntityValidityState.Invalid
  }
}
