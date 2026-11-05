package de.bizgrid.olli.schema

import de.bizgrid.olli.event.{OlliCbor, OlliEventEnvelope}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.UUID
import scala.concurrent.duration.*

class ValidatorSpec extends AnyFunSuite with Matchers {
  private val support = new JsonSchemaSupport(100.millis)

  test("never touches the network: an unknown absolute reference does not compile (REQ-SCH-016)") {
    val requested = scala.collection.mutable.Buffer[String]()
    val document = ujson.Obj("$ref" -> "https://example.org/schemas/remote.json")
    val result = support.compile("https://bizgrid.de/ollischema/a/b/c-v1.json", document, url => { requested += url; None }, cacheable = false)
    result.left.map(_.map(_.code)) shouldBe Left(Seq("unresolved-reference"))
    requested.toSeq.distinct shouldBe Seq("https://example.org/schemas/remote.json")
  }

  test("resolves references only through the local resolver") {
    val local = Map("https://bizgrid.de/ollischema/a/common/name-v1.json" -> ujson.Obj("type" -> "string", "minLength" -> 2))
    val compiled = support.compile("https://bizgrid.de/ollischema/a/b/c-v1.json",
      ujson.Obj("properties" -> ujson.Obj("name" -> ujson.Obj("$ref" -> "../common/name-v1.json"))), local.get, cacheable = false)
    val outcome = compiled.toOption.get.validate(ujson.Obj("name" -> "x"), 10)
    outcome.errorCount shouldBe 1
    outcome.errors.head.pointer shouldBe "/name"
    outcome.errors.head.keyword shouldBe "minLength"
  }

  test("a catastrophic pattern ends with an error instead of blocking (ReDoS, ADR-0069)") {
    val compiled = support.compile("https://bizgrid.de/ollischema/a/b/redos-v1.json",
      ujson.Obj("type" -> "string", "pattern" -> "^(a+)+$"), _ => None, cacheable = false).toOption.get
    val started = System.nanoTime()
    val outcome = compiled.validate(ujson.Str("a" * 40 + "!"), 10)
    (System.nanoTime() - started).nanos should be < 5.seconds
    outcome.valid shouldBe false
    outcome.errors.head.keyword shouldBe "pattern"
  }

  test("the metaschema check reports malformed keywords") {
    support.checkDocument(ujson.Obj("type" -> "object", "properties" -> ujson.Obj("a" -> ujson.Obj("minLength" -> "two")))) should not be empty
    support.checkDocument(ujson.Obj("type" -> "object")) shouldBe empty
  }

  test("decodes entity.events envelopes (ADR-0020, payload version 2)") {
    import OlliEventEnvelope.given
    val id = UUID.randomUUID()
    val payload = ujson.Obj("id" -> id.toString, "patch" -> ujson.Obj("id" -> id.toString, "tenant" -> ujson.Null, "project" -> ujson.Null,
      "kind" -> ujson.Null, "schema" -> "https://bizgrid.de/ollischema/a/b/c-v1.json", "context" -> ujson.Null,
      "data" -> ujson.Obj("x" -> 1), "origin" -> id.toString), "registeredAt" -> 5, "registeredBy" -> id.toString, "origin" -> id.toString)
    val envelope = OlliEventEnvelope(UUID.randomUUID(), "EntityPatched", 2, "entity-registry", "entity", id, 5, None, None, None, payload, Map.empty)
    EntityEvents.decode(OlliCbor.encode(envelope)).map(_._2) shouldBe Right(EntityEvent.Patched(id,
      EntityChange(None, None, None, Some("https://bizgrid.de/ollischema/a/b/c-v1.json"), Some(ujson.Obj("x" -> 1))), 5))
    EntityEvents.decode(OlliCbor.encode(envelope.copy(schemaVersion = 3))).isLeft shouldBe true
    EntityEvents.decode(Array[Byte](1, 2, 3)).isLeft shouldBe true
  }
}

class SelectorsSpec extends AnyFunSuite with Matchers {
  private val target = ujson.Obj(
    "type" -> "object",
    "properties" -> ujson.Obj(
      "Name" -> ujson.Obj("type" -> "string"),
      "Rooms" -> ujson.Obj("type" -> "array", "items" -> ujson.Obj("$ref" -> "#/$defs/room")),
      "Meta" -> ujson.Obj("allOf" -> ujson.Arr(ujson.Obj("properties" -> ujson.Obj("Owner" -> ujson.Obj("type" -> "string")))))),
    "$defs" -> ujson.Obj("room" -> ujson.Obj("properties" -> ujson.Obj("Area" -> ujson.Obj("type" -> "number")))))

  private def problems(expression: String, kind: Selector.Kind = Selector.Kind.JsonPath) =
    Selectors.check(Seq(Selector(expression, "/x", kind)), "https://bizgrid.de/ollischema/a/b/c-v1.json", target, _ => None).map(_.code)

  test("accepts declared properties through $ref, items, wildcards, filters and combinators") {
    problems("$.Name") shouldBe Nil
    problems("$.Rooms[*].Area") shouldBe Nil
    problems("$.Rooms[0].Area") shouldBe Nil
    problems("$.Rooms[?@.Area > 10].Area") shouldBe Nil
    problems("$['Meta'].Owner") shouldBe Nil
    problems("$..Area") shouldBe Nil
    problems("/Rooms/3/Area", Selector.Kind.JsonPointer) shouldBe Nil
  }

  test("reports undeclared properties and malformed expressions") {
    problems("$.Floor") shouldBe Seq("unknown-selector")
    problems("$.Rooms[*].Height") shouldBe Seq("unknown-selector")
    problems("$..Height") shouldBe Seq("unknown-selector")
    problems("$.Rooms[") shouldBe Seq("invalid-selector")
  }

  test("extracts whole-string paths, template placeholders and pointers") {
    Selectors.extract(ujson.Obj("a" -> "{$.Nr} – {$.Name}", "b" -> "$.Cover", "c" -> "plain text", "rule" -> ujson.Obj("pointer" -> "/Area")))
      .map(s => (s.expression, s.location)) shouldBe Seq(("$.Nr", "/a"), ("$.Name", "/a"), ("$.Cover", "/b"), ("/Area", "/rule/pointer"))
  }
}
