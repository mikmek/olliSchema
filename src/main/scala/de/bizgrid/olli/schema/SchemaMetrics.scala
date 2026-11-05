package de.bizgrid.olli.schema

import java.util.concurrent.atomic.AtomicLong

/** Prozesslokale Zaehler fuer /metrics (platform-observability). */
final class SchemaMetrics {
  val eventsProcessed = new AtomicLong()
  val eventsDuplicate = new AtomicLong()
  val eventsIgnored = new AtomicLong()
  val deadLetters = new AtomicLong()
  val consumerErrors = new AtomicLong()
  val schemasProjected = new AtomicLong()
  val schemasRemoved = new AtomicLong()
  val schemaChecks = new AtomicLong()
  val compilations = new AtomicLong()
  val entityValidations = new AtomicLong()
  val entityValidationsInvalid = new AtomicLong()
  val apiValidations = new AtomicLong()
  val revalidations = new AtomicLong()
  val immutabilityViolations = new AtomicLong()
  val urlConflicts = new AtomicLong()
  val entityLookups = new AtomicLong()
  val imports = new AtomicLong()
  val importFailures = new AtomicLong()
  val refererQueries = new AtomicLong()
  val releaseChecks = new AtomicLong()

  def snapshot: Seq[(String, Long)] = Seq(
    "schema_events_processed_total" -> eventsProcessed.get(),
    "schema_events_duplicate_total" -> eventsDuplicate.get(),
    "schema_events_ignored_total" -> eventsIgnored.get(),
    "schema_dead_letters_total" -> deadLetters.get(),
    "schema_consumer_errors_total" -> consumerErrors.get(),
    "schema_schemas_projected_total" -> schemasProjected.get(),
    "schema_schemas_removed_total" -> schemasRemoved.get(),
    "schema_schema_checks_total" -> schemaChecks.get(),
    "schema_compilations_total" -> compilations.get(),
    "schema_entity_validations_total" -> entityValidations.get(),
    "schema_entity_validations_invalid_total" -> entityValidationsInvalid.get(),
    "schema_api_validations_total" -> apiValidations.get(),
    "schema_revalidations_total" -> revalidations.get(),
    "schema_immutability_violations_total" -> immutabilityViolations.get(),
    "schema_url_conflicts_total" -> urlConflicts.get(),
    "schema_entity_lookups_total" -> entityLookups.get(),
    "schema_imports_total" -> imports.get(),
    "schema_import_failures_total" -> importFailures.get(),
    "schema_referer_queries_total" -> refererQueries.get(),
    "schema_release_checks_total" -> releaseChecks.get())
}
