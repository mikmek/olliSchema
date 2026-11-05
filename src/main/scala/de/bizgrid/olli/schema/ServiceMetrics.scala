package de.bizgrid.olli.schema

import de.bizgrid.olli.schema.Json.*
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

import java.lang.management.ManagementFactory

/**
 * Der einheitliche /metrics-Endpoint (platform-observability). Route, Envelope, Content-Type und
 * Statuscode sind in jedem olli-Service identisch; servicespezifisch ist allein der Inhalt von "metrics".
 *
 * Bewusst immer 200, solange der Prozess lebt: /metrics ist Telemetrie, Readiness bleibt allein
 * Sache von /health/ready.
 *
 * Gegenstuecke: de.bizgrid.olli.entity.shared.ServiceMetrics, de.bizgrid.olli.media.ServiceMetrics und
 * de.bizgrid.olli.trace.ServiceMetrics. Eine Kopie je Repository, weil die Repositories getrennt gebaut werden.
 */
object ServiceMetrics {

  /** `metrics` ist by-name und wird pro Abruf ausgewertet. */
  def route(service: String)(metrics: => Seq[(String, Long)]): Route =
    path("metrics") { get { complete(body(service, metrics)) } }

  def body(service: String, metrics: Seq[(String, Long)]): ujson.Value = ujson.Obj(
    "service" -> service,
    "status" -> "ok",
    "uptimeSeconds" -> ujson.Num(uptimeSeconds.toDouble),
    "metrics" -> ujson.Obj.from(metrics.map((name, value) => name -> ujson.Num(value.toDouble))))

  private def uptimeSeconds: Long = ManagementFactory.getRuntimeMXBean.getUptime / 1000L
}
