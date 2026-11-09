package de.bizgrid.olli.schema

import com.networknt.schema.path.PathType
import com.networknt.schema.regex.{RegularExpression, RegularExpressionFactory}
import com.networknt.schema.resource.{InputStreamSource, ResourceLoader, SchemaLoader}
import com.networknt.schema.{AbsoluteIri, Error, ExecutionConfig, ExecutionContext, Schema, SchemaLocation, SchemaRegistry, SchemaRegistryConfig, SpecificationVersion}
import tools.jackson.databind.JsonNode
import tools.jackson.databind.json.JsonMapper

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.util.regex.Pattern
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
 * Ergebnis einer Instanzvalidierung.
 *
 * `errorCount` zaehlt hoechstens bis zur Zaehlgrenze des Budgets; `truncated` meldet, dass dort
 * abgebrochen wurde (die wahre Zahl ist groesser). `aborted` nennt den Grund, wenn die Validierung ein
 * Ressourcenlimit erreicht hat und kein vollstaendiges Ergebnis vorliegt (SCH-A07).
 */
final case class ValidationOutcome(errorCount: Int, errors: Seq[ValidationError], truncated: Boolean = false, aborted: Option[String] = None) {
  def valid: Boolean = errorCount == 0 && aborted.isEmpty
}

object ValidationOutcome {
  /** Keyword der Fehlerzeile, die ein Ressourcenlimit statt eines Schemaverstosses meldet. */
  val LimitKeyword = "limit"

  def limit(reason: String): ValidationOutcome =
    ValidationOutcome(1, Seq(ValidationError("", LimitKeyword, reason)), aborted = Some(reason))
}

/**
 * Ressourcenbudget einer Validierung (ADR-0069, SCH-A07): Anzahl gespeicherter Fehler, Zaehlgrenze
 * (danach bricht die Validierung ab, statt alle Fehler zu sammeln) und Frist als `System.nanoTime`.
 */
final case class ValidationBudget(keepErrors: Int, countLimit: Int, deadline: Long)

/** Kompiliertes Schema hinter dem schmalen Validator-Interface (ADR-0067). */
trait CompiledSchema {
  def validate(data: ujson.Value, budget: ValidationBudget): ValidationOutcome
}

/**
 * Sprachunterstuetzung hinter einem schmalen Interface (ADR-0067). Weitere Sprachen (XSD, DTD) und
 * spezialisierte Hot-Path-Bypaesse werden hier ergaenzt, ohne Projektion und API zu aendern.
 */
trait SchemaLanguageSupport {
  def language: String
  /** Pruefung gegen das Metaschema der Sprache, mit Frist als `System.nanoTime`. */
  def checkDocument(document: ujson.Value, deadline: Long): Seq[Problem]
  /** Kompiliert ein Schema; Referenzen werden ausschliesslich ueber `resolve` aufgeloest. */
  def compile(url: String, document: ujson.Value, resolve: String => Option[ujson.Value], cacheable: Boolean): Either[Seq[Problem], CompiledSchema]
}

/** Ausgeloest, wenn ein `pattern` die Laufzeitgrenze ueberschreitet (ReDoS-Schutz, ADR-0069). */
final class RegexTimeoutException(pattern: String) extends RuntimeException(s"Regular expression '$pattern' exceeded its time limit", null, false, false)

/** Ausgeloest, wenn eine Validierung ihre Frist ueberschreitet; bricht die Arbeit ab (SCH-A07). */
final class ValidationTimeoutException extends RuntimeException("Validation exceeded its time limit", null, false, false)

/** Ausgeloest, wenn die Zaehlgrenze der Fehler erreicht ist. */
private final class ErrorLimitReached extends RuntimeException(null, null, false, false)

/**
 * java.util.regex mit Zeitgrenze: die Eingabe wird als CharSequence gereicht, die nach Ablauf der
 * Frist abbricht. Katastrophisches Backtracking endet damit mit einem Fehler statt einer blockierten
 * Validierung.
 */
final class TimeLimitedRegexFactory(timeout: FiniteDuration) extends RegularExpressionFactory {
  def getRegularExpression(regex: String): RegularExpression = {
    val pattern = Pattern.compile(regex)
    (value: String) => SafeRegex.find(pattern, value, timeout)
  }
}

/** Zeitbegrenzte Regex-Suche fuer alle Stellen, die Muster aus fremden Schemas ausfuehren (SCH-A06). */
object SafeRegex {
  /** Wirft [[RegexTimeoutException]], wenn die Suche laenger als `timeout` dauert. */
  def find(pattern: Pattern, value: String, timeout: FiniteDuration): Boolean =
    pattern.matcher(new DeadlineCharSequence(value, System.nanoTime() + timeout.toNanos, pattern.pattern())).find()
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
 * Ausfuehrungskontext mit Frist. networknt ruft `evaluationPathAddLast` vor jedem Keyword auf demselben
 * Kontext auf, auch in Unterschemas und Kombinatoren; dort wird die Frist geprueft und die Arbeit
 * kooperativ abgebrochen.
 */
private final class DeadlineExecutionContext(config: ExecutionConfig, deadline: Long) extends ExecutionContext(config) {
  private def check(): Unit = if (System.nanoTime() > deadline) throw new ValidationTimeoutException
  override def evaluationPathAddLast(segment: String): Unit = { check(); super.evaluationPathAddLast(segment) }
  override def evaluationPathAddLast(index: Int): Unit = { check(); super.evaluationPathAddLast(index) }
}

/** Fehlerliste, die bei Erreichen der Zaehlgrenze die Validierung beendet, statt weiter zu sammeln. */
private final class BoundedErrorList(limit: Int) extends java.util.ArrayList[Error] {
  override def add(error: Error): Boolean = {
    if (size() >= limit) throw new ErrorLimitReached
    super.add(error)
  }
  override def add(index: Int, error: Error): Unit = {
    if (size() >= limit) throw new ErrorLimitReached
    super.add(index, error)
  }
  override def addAll(errors: java.util.Collection[? <: Error]): Boolean = {
    errors.forEach(error => add(error))
    !errors.isEmpty
  }
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

  private val executionConfig: ExecutionConfig = new ExecutionConfig.Builder()
    .locale(config.getLocale)
    .formatAssertionsEnabled(config.getFormatAssertionsEnabled)
    .failFast(false)
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

  /** Fuehrt `schema` mit Frist und Zaehlgrenze aus. */
  private def run(schema: Schema, node: JsonNode, countLimit: Int, deadline: Long): Either[String, (Seq[Error], Boolean)] = {
    val context = new DeadlineExecutionContext(executionConfig, deadline)
    val errors = new BoundedErrorList(countLimit)
    context.setErrors(errors)
    try {
      schema.validate(context, node)
      Right((errors.asScala.toSeq, false))
    } catch {
      case _: ErrorLimitReached => Right((errors.asScala.toSeq, true))
      case error: ValidationTimeoutException => Left(error.getMessage)
      case error: RegexTimeoutException => Left(error.getMessage)
      case NonFatal(error) if causes(error).exists(_.isInstanceOf[ErrorLimitReached]) => Right((errors.asScala.toSeq, true))
      case NonFatal(error) if causes(error).exists(cause => cause.isInstanceOf[ValidationTimeoutException] || cause.isInstanceOf[RegexTimeoutException]) =>
        Left(causes(error).find(cause => cause.isInstanceOf[ValidationTimeoutException] || cause.isInstanceOf[RegexTimeoutException]).get.getMessage)
    }
  }

  def checkDocument(document: ujson.Value, deadline: Long): Seq[Problem] =
    document match {
      case _: ujson.Obj | ujson.True | ujson.False =>
        try {
          run(metaSchema, toNode(document), 1000, deadline) match {
            case Right((errors, _)) => errors.map(error => Problem("metaschema", error.getMessage, Some(error.getInstanceLocation.toString)))
            case Left(reason) => Seq(Problem("validation-limit", s"Metaschema check aborted: $reason"))
          }
        } catch {
          case NonFatal(error) => Seq(Problem("metaschema", message(error), Some("")))
        }
      case _ => Seq(Problem("metaschema", "A JSON Schema must be an object or a boolean", Some("")))
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
        val chain = causes(error)
        val unresolved = chain.exists(cause => cause.isInstanceOf[java.io.FileNotFoundException] ||
          cause.getClass.getSimpleName == "InvalidSchemaRefException")
        Left(Seq(Problem(if (unresolved) "unresolved-reference" else "compile", message(chain.last))))
    }

  private final class NetworkntCompiledSchema(schema: Schema) extends CompiledSchema {
    def validate(data: ujson.Value, budget: ValidationBudget): ValidationOutcome =
      run(schema, toNode(data), budget.countLimit, budget.deadline) match {
        case Left(reason) => ValidationOutcome.limit(reason)
        case Right((errors, truncated)) =>
          ValidationOutcome(errors.size, errors.take(budget.keepErrors).map { error =>
            ValidationError(error.getInstanceLocation.toString, Option(error.getKeyword).getOrElse(""), error.getMessage)
          }, truncated)
      }
  }

  private def causes(error: Throwable): Seq[Throwable] = Iterator.iterate[Throwable](error)(_.getCause).takeWhile(_ != null).toSeq

  private def message(error: Throwable): String = Option(error.getMessage).getOrElse(error.getClass.getSimpleName)
}
