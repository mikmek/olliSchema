package de.bizgrid.olli.schema

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import redis.clients.jedis.JedisPooled
import redis.embedded.RedisServer

import java.net.ServerSocket
import java.util.UUID
import scala.util.Try

/**
 * Der Redis-Adapter gegen eine echte, eingebettete Redis, vor allem das atomare Commit-Skript (SCH-A08).
 * Startet die mitgelieferte Binaerdatei auf der Plattform nicht, werden die Tests uebersprungen.
 */
class RedisSchemaStoreSpec extends AnyFunSuite with Matchers with BeforeAndAfterAll {
  private val port = Try { val socket = new ServerSocket(0); try socket.getLocalPort finally socket.close() }.getOrElse(6399)
  private lazy val server: Option[RedisServer] = Try { val s = new RedisServer(port); s.start(); s }.toOption
  private lazy val store: Option[RedisSchemaStore] = server.map(_ => new RedisSchemaStore(new JedisPooled("localhost", port)))

  override def afterAll(): Unit = {
    store.foreach(_.close())
    server.foreach(s => Try(s.stop()))
  }

  private def withStore(body: RedisSchemaStore => Unit): Unit = store match {
    case Some(value) if value.ping() => body(value)
    case _ => cancel("Embedded Redis is not available on this platform")
  }

  test("commit applies all operations and raises the version atomically") {
    withStore { store =>
      val entity = UUID.randomUUID()
      val member = UUID.randomUUID()
      val ops = Seq(StoreOp.Set("schema:doc:a", "doc"), StoreOp.SAdd("schema:all", (1 to 1200).map(_.toString)),
        StoreOp.SRem("schema:all", Seq("7")), StoreOp.Bind("schema:url:a", "a"), StoreOp.ZAdd("schema:releases:c", 2, member.toString),
        StoreOp.Push("schema:violations", "v", 2))
      store.commit(ops, Some(entity -> 10L)) shouldBe CommitResult.Applied
      store.get("schema:doc:a") shouldBe Some("doc")
      store.setSize("schema:all") shouldBe 1199
      store.releases("c") shouldBe Seq(2 -> member)
      store.version(entity) shouldBe Some(10L)
      store.commit(Seq(StoreOp.Set("schema:doc:a", "changed")), Some(entity -> 10L)) shouldBe CommitResult.Duplicate
      store.get("schema:doc:a") shouldBe Some("doc")
    }
  }

  test("a conflicting binding writes nothing; unbind before bind in the same commit is honoured") {
    withStore { store =>
      store.set("schema:url:b", "other")
      val entity = UUID.randomUUID()
      store.commit(Seq(StoreOp.Set("schema:doc:b", "doc"), StoreOp.Bind("schema:url:b", "mine")), Some(entity -> 1L)) shouldBe
        CommitResult.Conflict("schema:url:b")
      store.get("schema:doc:b") shouldBe None
      store.version(entity) shouldBe None
      store.commit(Seq(StoreOp.Unbind("schema:url:b", "other"), StoreOp.Bind("schema:url:b", "mine")), None) shouldBe CommitResult.Applied
      store.get("schema:url:b") shouldBe Some("mine")
    }
  }

  test("the projection runs end to end against Redis") {
    withStore { store =>
      store.clear()
      val urls = new SchemaUrls("https://bizgrid.de/ollischema/")
      val catalog = new Catalog(store, urls)
      val f = new Fixtures
      val checker = new Checker(catalog, f.languages, f.limits, f.metrics)
      val projection = new Projection(catalog, checker, Set(f.SchemaKind), f.metrics)
      val id = UUID.randomUUID()
      projection.apply(EntityEvent.Created(id, f.Tenant, f.Project, f.SchemaKind, SchemaLanguage.JsonSchema202012,
        f.schemaData(f.info(branch = Some("main")), f.roomDocument), 1), 1) shouldBe true
      catalog.uuidFor("https://bizgrid.de/ollischema/rave/building/room-v1-main.json") shouldBe Some(id)
      catalog.validity(id).map(_.state) shouldBe Some(ValidityState.Valid)
      projection.apply(EntityEvent.Deleted(id, 2), 2) shouldBe true
      catalog.record(id) shouldBe None
    }
  }
}
