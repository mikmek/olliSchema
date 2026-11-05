package de.bizgrid.olli.schema

import com.networknt.schema.path.PathType
import com.networknt.schema.regex.{RegularExpression, RegularExpressionFactory}
import com.networknt.schema.resource.{InputStreamSource, ResourceLoader, SchemaLoader}
import com.networknt.schema.{AbsoluteIri, Schema, SchemaLocation, SchemaRegistry, SchemaRegistryConfig, SpecificationVersion}
import tools.jackson.databind.JsonNode
import tools.jackson.databind.json.JsonMapper

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.util.regex.Pattern
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** Ergebnis einer Instanzvalidierung: Gesamtzahl der Fehler und die ersten davon. */
final case class ValidationOutcome(errorCount: Int, errors: Seq[ValidationError]) {
  def valid: Boolean = errorCount == 0
}

/** Kompiliertes Schema hinter dem schmalen Validator-Interface (ADR-0067). */
trait CompiledSchema {
  def validate(data: ujson.Value, maxErrors: Int): ValidationOutcome
}

/**
 * Sprachunterstuetzung hinter einem schmalen Interface (ADR-0067). Weitere Sprachen (XSD, DTD) und
 * spezialisierte Hot-Path-Bypaesse werden hier ergaenzt, ohne Projektion und API zu aendern.
 */
trait SchemaLanguageSupport {
  def language: String
  /** Pruefung gegen das Metaschema der Sprache. */
  def checkDocument(document: ujson.Value): Seq[Problem]
  /** Kompiliert ein Schema; Referenzen werden ausschliesslich ueber `resolve` aufgeloest. */
  def compile(url: String, document: ujson.Value, resolve: String => Option[ujson.Value], cacheable: Boolean): Either[Seq[Problem], CompiledSchema]
}

/** Ausgeloest, wenn ein `pattern` die Laufzeitgrenze ueberschreitet (ReDoS-Schutz, ADR-0069). */
final class RegexTimeoutException(pattern: String) extends RuntimeException(s"Regular expression '$pattern' exceeded its time limit")

/**
 * java.util.regex mit Zeitgrenze: die Eingabe wird als CharSequence gereicht, die nach Ablauf der
 * Frist abbricht. Katastrophisches Backtracking endet damit mit einem Fehler statt einer blockierten
 * Validierung.
 */
final class TimeLimitedRegexFactory(timeout: FiniteDuration) extends RegularExpressionFactory {
  def getRegularExpression(regex: String): RegularExpression = {
    val pattern = Pattern.compile(regex)
    (value: String) => {
      val deadline = System.nanoTime() + timeout.toNanos
      pattern.matcher(new DeadlineCharSequence(value, deadline, regex)).find()
    }
  }
}

private final class DeadlineCharSequence(underlying: CharSequence, deadline: Long, pattern: String) extends CharSequence {
  def length(): Int = underlying.length()
  def charAt(index: Int): Char = {
    if (System.nanoTime() > deadline) throw new RegexTimeoutException(pattern)
    underlying.charAt(index)
  }
  def subSequence(start: Int, end: Int): CharSequence = new DeadlineCharSequence(underlying.subSequence(start, end), deadline, pattern)
  override def toString: String = underlying.toString
}

/**
 * JSON Schema 2020-12 mit networknt json-schema-validator (ADR-0067).
 *
 * Der Schema-Loader kennt keinen Netz-Loader: Metaschemas kommen aus dem Klassenpfad der Bibliothek,
 * alles andere aus der lokalen Registry ueber `resolve` (REQ-SCH-016, ADR-0062).
 */
final class JsonSchemaSupport(regexTimeout: FiniteDuration) extends SchemaLanguageSupport {
  val language: String = SchemaLanguage.JsonSchema202012

  private val mapper: JsonMapper = JsonMapper.builder().build()

  private val config: SchemaRegistryConfig = new SchemaRegistryConfig.Builder()
    .pathType(PathType.JSON_POINTER)
    .regularExpressionFactory(new TimeLimitedRegexFactory(regexTimeout))
    .build()

  /** Registry ohne Ressourcen ausser den Metaschemas, fuer die Metaschema-Pruefung. */
  private lazy val metaRegistry = registry(_ => None, cache = true)
  private lazy val metaSchema: Schema = metaRegistry.getSchema(SchemaLocation.of(SpecificationVersion.DRAFT_2020_12.getDialectId))

  def toNode(value: ujson.Value): JsonNode = mapper.readTree(ujson.write(value))

  private def registry(resolve: String => Option[ujson.Value], cache: Boolean): SchemaRegistry = {
    val local: ResourceLoader = (iri: AbsoluteIri) =>
      resolve(iri.toString).map { document =>
        val bytes = ujson.write(document).getBytes(StandardCharsets.UTF_8)
        (() => new ByteArrayInputStream(bytes)): InputStreamSource
      }.orNull
    val loader = new SchemaLoader(java.util.List.of(), java.util.List.of(local))
    SchemaRegistry.withDefaultDialect(SpecificationVersion.DRAFT_2020_12, builder => {
      builder.schemaLoader(loader).schemaRegistryConfig(config).schemaCacheEnabled(cache)
      ()
    })
  }

  def checkDocument(document: ujson.Value): Seq[Problem] =
    try {
      document match {
        case _: ujson.Obj | ujson.True | ujson.False => ()
        case _ => return Seq(Problem("metaschema", "A JSON Schema must be an object or a boolean", Some("")))
      }
      metaSchema.validate(toNode(document)).asScala.toSeq.map { error =>
        Problem("metaschema", error.getMessage, Some(error.getInstanceLocation.toString))
      }
    } catch {
      case NonFatal(error) => Seq(Problem("metaschema", message(error), Some("")))
    }

  def compile(url: String, document: ujson.Value, resolve: String => Option[ujson.Value], cacheable: Boolean): Either[Seq[Problem], CompiledSchema] =
    try {
      val schema = registry(resolve, cacheable).getSchema(SchemaLocation.of(url), toNode(document))
      // Loest alle Referenzen sofort auf, damit Kompilierfehler bei der Speicherung sichtbar werden (REQ-SCH-007).
      schema.initializeValidators()
      Right(new NetworkntCompiledSchema(schema))
    } catch {
      case NonFatal(error) =>
        // Der lokale Loader liefert fuer unbekannte URLs nichts; networknt meldet das als
        // SchemaException <- FileNotFoundException bzw. als InvalidSchemaRefException.
        val chain = Iterator.iterate[Throwable](error)(_.getCause).takeWhile(_ != null).toSeq
        val unresolved = chain.exists(cause => cause.isInstanceOf[java.io.FileNotFoundException] ||
          cause.getClass.getSimpleName == "InvalidSchemaRefException")
        Left(Seq(Problem(if (unresolved) "unresolved-reference" else "compile", message(chain.last))))
    }

  private final class NetworkntCompiledSchema(schema: Schema) extends CompiledSchema {
    def validate(data: ujson.Value, maxErrors: Int): ValidationOutcome =
      try {
        val errors = schema.validate(toNode(data)).asScala.toSeq
        ValidationOutcome(errors.size, errors.take(maxErrors).map { error =>
          ValidationError(error.getInstanceLocation.toString, Option(error.getKeyword).getOrElse(""), error.getMessage)
        })
      } catch {
        case error: RegexTimeoutException => ValidationOutcome(1, Seq(ValidationError("", "pattern", error.getMessage)))
        case NonFatal(error) if Option(error.getCause).exists(_.isInstanceOf[RegexTimeoutException]) =>
          ValidationOutcome(1, Seq(ValidationError("", "pattern", error.getCause.getMessage)))
      }
  }

  private def message(error: Throwable): String = Option(error.getMessage).getOrElse(error.getClass.getSimpleName)
}
