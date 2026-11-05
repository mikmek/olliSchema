package de.bizgrid.olli.schema

import com.typesafe.config.{Config, ConfigFactory}
import org.apache.pekko.actor.typed.{ActorSystem, DispatcherSelector}
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.http.scaladsl.Http
import redis.clients.jedis.JedisPooled

import java.util.UUID
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/** Aus der Konfiguration `ollischema` abgeleitete Bausteine, gemeinsam fuer Main und Tests. */
object Settings {
  def limits(config: Config): Limits = {
    val validation = config.getConfig("validation")
    Limits(
      storedErrors = validation.getInt("stored-errors"),
      maxErrors = validation.getInt("max-errors"),
      maxDocumentBytes = validation.getInt("max-document-bytes"),
      maxReferenceDepth = validation.getInt("max-reference-depth"),
      maxReferencedSchemas = validation.getInt("max-referenced-schemas"),
      regexTimeout = validation.getDuration("regex-timeout").toMillis.millis,
      compiledCacheSize = validation.getInt("compiled-cache-size"))
  }

  def schemaKinds(config: Config): Set[UUID] = config.getStringList("schema-kinds").asScala.map(UUID.fromString).toSet

  def importSettings(config: Config): ImportSettings = {
    val imports = config.getConfig("import")
    ImportSettings(
      allowedPrincipals = imports.getStringList("allowed-principals").asScala.map(UUID.fromString).toSet,
      allowedHosts = imports.getStringList("allowed-hosts").asScala.toSeq,
      allowHttp = imports.getBoolean("allow-http"),
      maxBytes = imports.getInt("max-bytes"),
      connectTimeout = imports.getDuration("connect-timeout"),
      requestTimeout = imports.getDuration("request-timeout"),
      maxDocuments = imports.getInt("max-documents"),
      origin = UUID.fromString(imports.getString("origin")),
      schemaKind = schemaKinds(config).toSeq.sorted.head)
  }
}

/**
 * olliSchema (ADR-0063): Autoritaet ueber die Schemas einer Plattform-Instanz ohne eigenes
 * Write-Modell. Projektion aus `entity.events`, Lesemodell in eigener Redis-Instanz, Aufloesung,
 * Kompilierung und Validierung nur gegen diese lokale Registry.
 */
object Main {
  def main(args: Array[String]): Unit = {
    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "olli-schema")
    implicit val ec: ExecutionContext = system.executionContext
    val config = ConfigFactory.load().getConfig("ollischema")
    val limits = Settings.limits(config)
    val metrics = new SchemaMetrics

    val redis = new JedisPooled(config.getString("redis.host"), config.getInt("redis.port"))
    val store = new RedisSchemaStore(redis)
    val catalog = new Catalog(store, new SchemaUrls(config.getString("namespace")))
    val languages = Seq[SchemaLanguageSupport](new JsonSchemaSupport(limits.regexTimeout)).map(s => s.language -> s).toMap
    val checker = new Checker(catalog, languages, limits, metrics)

    val indexConfig = config.getConfig("entity-index")
    val entityIndex = new HttpEntityIndex(indexConfig.getString("base-url"), indexConfig.getInt("page-size"),
      indexConfig.getInt("max-entities"), indexConfig.getDuration("timeout"))
    val projection = new Projection(catalog, checker, Settings.schemaKinds(config), metrics,
      lookup = entityIndex.entity, onViolation = message => system.log.warn(message))

    val importSettings = Settings.importSettings(config)
    val importer = Option.when(importSettings.allowedPrincipals.nonEmpty) {
      val registryConfig = config.getConfig("entity-registry")
      new SchemaImporter(importSettings, new GuardedHttpFetcher(importSettings),
        new HttpEntityRegistry(registryConfig.getString("base-url"), registryConfig.getDuration("timeout")), catalog, metrics)
    }
    if (importer.isEmpty) system.log.info("Schema import is disabled: ollischema.import.allowed-principals is empty")

    val consumer = new EntityEventConsumer(KafkaSettings.fromConfig(config.getConfig("kafka")), projection, store, metrics)
    consumer.start()

    // Gedrosselte Neuvalidierung (ADR-0061) auf einem eigenen Thread, getrennt von interaktiven Aufrufen.
    val revalidation = config.getConfig("revalidation")
    val interval = revalidation.getDuration("interval").toMillis.millis
    val batch = revalidation.getInt("batch")
    val revalidator = java.util.concurrent.Executors.newSingleThreadScheduledExecutor(runnable => {
      val thread = new Thread(runnable, "olli-schema-revalidation")
      thread.setDaemon(true)
      thread
    })
    revalidator.scheduleWithFixedDelay(() => Try(projection.drainRevalidation(batch)).failed.foreach(error =>
      system.log.warn("Revalidation batch failed: {}", error.getMessage)), interval.toMillis, interval.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS)

    system.whenTerminated.foreach { _ =>
      consumer.close()
      revalidator.shutdownNow()
      store.close()
    }

    val blocking = system.dispatchers.lookup(DispatcherSelector.blocking())
    val timeoutAfter: FiniteDuration => Future[Unit] = delay => {
      val promise = Promise[Unit]()
      system.scheduler.scheduleOnce(delay, () => promise.trySuccess(()))
      promise.future
    }
    val routes = new Routes(catalog, checker, projection, entityIndex, importer, metrics,
      Readiness(() => store.ping(), () => consumer.isRunning), limits, config.getDuration("validation.api-timeout").toMillis.millis,
      timeoutAfter)(using blocking)

    Http().newServerAt(config.getString("host"), config.getInt("port")).bind(routes.route).onComplete {
      case Success(binding) => system.log.info("olliSchema online at {}", binding.localAddress)
      case Failure(error) =>
        system.log.error("Failed to bind olliSchema HTTP endpoint, terminating", error)
        system.terminate()
    }
    Await.result(system.whenTerminated, Duration.Inf)
  }
}

/**
 * Einmalwerkzeug fuer die Reprojektion (REQ-SCH-018): loescht das Lesemodell und die Offsets der
 * Consumer-Gruppe. Danach baut olliSchema beim naechsten Start alles aus `entity.events` neu auf.
 * Nur bei gestopptem Service ausfuehren (`bin/reproject-main -Dconfig.file=...`).
 */
object ReprojectMain {
  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load().getConfig("ollischema")
    val kafka = KafkaSettings.fromConfig(config.getConfig("kafka"))
    val store = new RedisSchemaStore(new JedisPooled(config.getString("redis.host"), config.getInt("redis.port")))
    try {
      val deleted = store.clear()
      println(s"Deleted $deleted read-model keys from Redis ${config.getString("redis.host")}:${config.getInt("redis.port")}")
    } finally store.close()

    val properties = new java.util.Properties()
    properties.put("bootstrap.servers", kafka.bootstrapServers.mkString(","))
    val admin = org.apache.kafka.clients.admin.Admin.create(properties)
    try {
      Try(admin.deleteConsumerGroups(java.util.List.of(kafka.groupId)).all().get(30, java.util.concurrent.TimeUnit.SECONDS)) match {
        case Success(_) => println(s"Deleted consumer group ${kafka.groupId}; the next start replays ${kafka.topic} from earliest")
        case Failure(error) => println(s"Consumer group ${kafka.groupId} not deleted (${Option(error.getCause).getOrElse(error).getMessage}); " +
          "if it does not exist, nothing is to do")
      }
    } finally admin.close()
  }
}
