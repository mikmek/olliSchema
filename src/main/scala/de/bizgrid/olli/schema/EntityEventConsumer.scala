package de.bizgrid.olli.schema

import com.typesafe.config.Config
import org.apache.kafka.clients.consumer.{ConsumerConfig, KafkaConsumer}
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, StringDeserializer}
import org.slf4j.LoggerFactory

import java.time.Duration
import java.util.Properties
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

final case class KafkaSettings(bootstrapServers: Seq[String], topic: String, groupId: String, clientId: String, pollTimeout: Duration) {
  def consumerProperties: Properties = {
    val properties = new Properties()
    properties.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers.mkString(","))
    properties.put(ConsumerConfig.GROUP_ID_CONFIG, groupId)
    properties.put(ConsumerConfig.CLIENT_ID_CONFIG, clientId)
    properties.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest")
    properties.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "false")
    properties.put(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, "200")
    properties.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, classOf[StringDeserializer].getName)
    properties.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, classOf[ByteArrayDeserializer].getName)
    properties
  }
}

object KafkaSettings {
  def fromConfig(config: Config): KafkaSettings = KafkaSettings(
    config.getStringList("bootstrap-servers").asScala.toSeq, config.getString("topic"), config.getString("group-id"),
    config.getString("client-id"), config.getDuration("poll-timeout"))
}

/**
 * Konsument von `entity.events` (ADR-0063). At-least-once: Offsets werden nach der Verarbeitung eines
 * Polls committet, die Projektion ist idempotent je Entity-Version. Ein unlesbarer oder nicht
 * projizierbarer Record landet in `schema:deadletters` und blockiert die Partition nicht (GREQ-008).
 *
 * Wie im EntityIndex wird der KafkaConsumer in der Schleife gebaut; ein fehlender Broker ist ein
 * Retry mit gedeckeltem Backoff, kein Prozessabbruch. `isRunning` meldet die Zeit dazwischen als nicht bereit.
 */
final class EntityEventConsumer(settings: KafkaSettings, projection: Projection, store: SchemaStore, metrics: SchemaMetrics) extends AutoCloseable {
  private val log = LoggerFactory.getLogger(getClass)
  private val running = new AtomicBoolean(true)
  private val ready = new AtomicBoolean(false)
  private val active = new AtomicReference[KafkaConsumer[String, Array[Byte]]]()
  private val MinBackoff = Duration.ofSeconds(1)
  private val MaxBackoff = Duration.ofSeconds(30)

  private val thread = new Thread(() => run(), s"${settings.groupId}-consumer")
  thread.setDaemon(true)

  def start(): Unit = thread.start()

  def isRunning: Boolean = running.get() && ready.get() && thread.isAlive

  private def run(): Unit = {
    var backoff = MinBackoff
    while (running.get()) {
      try consume(() => backoff = MinBackoff)
      catch {
        case NonFatal(error) if running.get() =>
          metrics.consumerErrors.incrementAndGet()
          log.warn("olliSchema is not consuming {}: {}. Retrying in {}s.", settings.topic, error.getMessage, Long.box(backoff.toSeconds))
          val deadline = System.nanoTime() + backoff.toNanos
          while (running.get() && System.nanoTime() < deadline) Thread.sleep(200)
          backoff = if (backoff.multipliedBy(2).compareTo(MaxBackoff) > 0) MaxBackoff else backoff.multipliedBy(2)
        case NonFatal(_) => ()
      }
    }
  }

  private def consume(onPoll: () => Unit): Unit = {
    val consumer = new KafkaConsumer[String, Array[Byte]](settings.consumerProperties)
    active.set(consumer)
    try {
      consumer.subscribe(List(settings.topic).asJava)
      while (running.get()) {
        val records = consumer.poll(settings.pollTimeout)
        ready.set(true)
        onPoll()
        records.asScala.foreach(record => handle(record.topic(), record.partition(), record.offset(), record.value()))
        if (!records.isEmpty) consumer.commitSync()
      }
    } finally {
      ready.set(false)
      active.set(null)
      try consumer.close(Duration.ofSeconds(2)) catch { case NonFatal(_) => () }
    }
  }

  private[schema] def handle(topic: String, partition: Int, offset: Long, value: Array[Byte]): Unit = {
    val outcome =
      if (value == null) Left("Record has no value")
      else EntityEvents.decode(value).flatMap { (_, event) =>
        try { projection.apply(event, offset); Right(()) }
        catch {
          // Ist das Lesemodell nicht erreichbar, ist das kein Fehler des Records: ohne Commit neu aufbauen
          // und denselben Poll spaeter wiederholen, statt den Record zu verwerfen.
          case NonFatal(error) if !store.ping() => throw error
          case NonFatal(error) => Left(s"Projection failed: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}")
        }
      }
    outcome.left.foreach { reason =>
      metrics.deadLetters.incrementAndGet()
      log.error("Dead-lettered record {}-{}@{}: {}", topic, Int.box(partition), Long.box(offset), reason)
      // GREQ-010: kein Payload im Diagnosepfad, nur Fundstelle und Grund.
      try store.pushBounded(Keys.DeadLetters, ujson.write(ujson.Obj("topic" -> topic, "partition" -> partition,
        "offset" -> ujson.Num(offset.toDouble), "reason" -> reason, "at" -> Json.instant(System.currentTimeMillis()))), 1000)
      catch { case NonFatal(error) => log.error("Could not record dead letter", error) }
    }
  }

  override def close(): Unit = {
    running.set(false)
    Option(active.get()).foreach(_.wakeup())
    thread.join(5000)
  }
}
