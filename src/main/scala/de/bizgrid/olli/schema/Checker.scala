package de.bizgrid.olli.schema

import java.nio.charset.StandardCharsets
import java.util.UUID
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

final case class Limits(
  storedErrors: Int,
  maxErrors: Int,
  maxDocumentBytes: Int,
  maxReferenceDepth: Int,
  maxReferencedSchemas: Int,
  regexTimeout: FiniteDuration,
  compiledCacheSize: Int
)

/** Transitive Referenzziele eines Schemas, aufgeloest gegen die lokale Registry. */
final case class Closure(records: Seq[SchemaRecord], missing: Seq[String], limitExceeded: Option[String])

/** Ergebnis der Release-Pruefung (REQ-SCH-010), Grundlage des FSM-Guards. */
final case class ReleaseCheck(
  record: SchemaRecord,
  validity: SchemaValidity,
  nextVersion: Option[Int],
  releasedUrl: Option[String],
  pins: Seq[(String, String)],
  problems: Seq[Problem]
) {
  def releasable: Boolean = problems.isEmpty

  def toJson: ujson.Value = {
    val obj = ujson.Obj(
      "uuid" -> record.uuid.toString,
      "releasable" -> releasable,
      "entityVersion" -> ujson.Num(record.entityVersion.toDouble),
      "validity" -> validity.toJson,
      "pins" -> ujson.Arr.from(pins.map((from, to) => ujson.Obj("from" -> from, "to" -> to))),
      "problems" -> ujson.Arr.from(problems.map(_.toJson)))
    record.url.foreach(value => obj("url") = value)
    record.info.flatMap(_.internal).foreach(identity => obj("targetVersion") = identity.version)
    nextVersion.foreach(value => obj("nextVersion") = value)
    releasedUrl.foreach(value => obj("releasedUrl") = value)
    obj
  }
}

/**
 * Schema-Gueltigkeit (ADR-0064), Release-Pruefung (REQ-SCH-010) und Entity-Validierung (REQ-SCH-008,
 * REQ-SCH-009). Zustandslos bis auf den Cache kompilierter Schemas: nur Schemas, deren gesamte
 * Referenzhuelle unveraenderlich ist, werden gecacht (ADR-0059).
 */
final class Checker(catalog: Catalog, languages: Map[String, SchemaLanguageSupport], limits: Limits, metrics: SchemaMetrics,
                    clock: () => Long = () => System.currentTimeMillis()) {

  private val UnresolvedCodes = Set("unresolved-reference", "unresolved-target")
  private val WrapperUrl = s"${catalog.urls.namespace}_validation/entity-ref.json"

  private val compiledCache = new java.util.LinkedHashMap[String, CompiledSchema](64, 0.75f, true) {
    override def removeEldestEntry(eldest: java.util.Map.Entry[String, CompiledSchema]): Boolean = size() > limits.compiledCacheSize
  }

  def compiledCacheSize: Int = compiledCache.synchronized(compiledCache.size())

  /** Transitive Huelle der Referenzziele, begrenzt in Tiefe und Anzahl (ADR-0069). */
  def closure(record: SchemaRecord): Closure = {
    val seen = mutable.LinkedHashMap[UUID, SchemaRecord]()
    val missing = mutable.LinkedHashSet[String]()
    var exceeded: Option[String] = None
    var frontier = Seq(record)
    var depth = 0
    while (frontier.nonEmpty && exceeded.isEmpty) {
      if (depth >= limits.maxReferenceDepth) exceeded = Some(s"Reference depth exceeds ${limits.maxReferenceDepth}")
      else {
        val next = Seq.newBuilder[SchemaRecord]
        frontier.foreach(current => current.refs.foreach { url =>
          catalog.resolve(url) match {
            case Some(target) if target.uuid == record.uuid || seen.contains(target.uuid) => ()
            case Some(target) =>
              seen(target.uuid) = target
              next += target
            case None => missing += url
          }
        })
        if (seen.size > limits.maxReferencedSchemas) exceeded = Some(s"More than ${limits.maxReferencedSchemas} referenced schemas")
        frontier = next.result()
        depth += 1
      }
    }
    Closure(seen.values.toSeq, missing.toSeq, exceeded)
  }

  /** Ermittelt die Schema-Gueltigkeit eines Datensatzes (REQ-SCH-007). */
  def checkSchema(record: SchemaRecord): SchemaValidity = {
    metrics.schemaChecks.incrementAndGet()
    val problems = Seq.newBuilder[Problem]
    problems ++= record.infoProblems
    val support = languages.get(record.language)
    if (support.isEmpty) problems += Problem("unsupported-language", s"Schema language '${record.language}' is not supported yet")
    val size = ujson.write(record.document).getBytes(StandardCharsets.UTF_8).length
    if (size > limits.maxDocumentBytes) problems += Problem("document-too-large", s"Document has $size bytes, limit is ${limits.maxDocumentBytes}")

    val structural = problems.result()
    val all: Seq[Problem] =
      if (structural.exists(!_.warning) || support.isEmpty) structural
      else structural ++ contentProblems(record, support.get)

    val errors = all.filterNot(_.warning)
    val state =
      if (errors.isEmpty) ValidityState.Valid
      else if (errors.forall(problem => UnresolvedCodes(problem.code))) ValidityState.Unresolved
      else ValidityState.Invalid
    SchemaValidity(state, record.entityVersion, record.etag, clock(), all)
  }

  private def contentProblems(record: SchemaRecord, support: SchemaLanguageSupport): Seq[Problem] = {
    val problems = Seq.newBuilder[Problem]
    problems ++= support.checkDocument(record.document)
    References.extract(record.document, catalog.baseUrl(record)).left.foreach(problems ++= _)

    val hull = closure(record)
    hull.limitExceeded.foreach(reason => problems += Problem("reference-limit", reason))
    hull.missing.foreach(url => problems += Problem("unresolved-reference", s"Reference target '$url' is not in the local registry"))

    // REQ-SCH-005: Released verweist transitiv nur auf Released.
    if (record.countsAsReleased)
      hull.records.filterNot(_.countsAsReleased).foreach { target =>
        problems += Problem("references-draft", s"Released schema references draft '${target.url.getOrElse(catalog.urls.byId(target.uuid))}'")
      }
    else {
      // ADR-0061: Drafts desselben Branches duerfen sich referenzieren, andere Branches sind nicht vorgesehen.
      val branch = record.info.flatMap(_.branch)
      record.refs.flatMap(catalog.resolve).filter(target => target.info.exists(_.isDraft) && target.info.flatMap(_.branch) != branch).foreach { target =>
        problems += Problem.warning("cross-branch-reference", s"Draft references draft '${target.url.getOrElse("")}' of another branch; expect breakage")
      }
    }

    val beforeCompile = problems.result()
    val compileProblems =
      if (beforeCompile.exists(problem => !problem.warning && problem.code != "references-draft")) Nil
      else compile(record, support, hull).left.toOption.getOrElse(Nil)

    beforeCompile ++ compileProblems ++ record.info.flatMap(_.meta).toSeq.flatMap(binding => metaProblems(record, binding))
  }

  /** Meta-Schema-Gueltigkeit analog ADR-0064: Ziel vorhanden, passender Status, alle Selektoren existieren. */
  private def metaProblems(record: SchemaRecord, binding: MetaBinding): Seq[Problem] =
    catalog.resolve(binding.target) match {
      case None => Seq(Problem("unresolved-target", s"Target schema '${binding.target}' is not in the local registry", Some("/schemaInfo/target")))
      case Some(target) =>
        val status =
          if (target.countsAsReleased) Nil
          else if (record.countsAsReleased) Seq(Problem("target-not-released", "A released meta-schema must bind to a released target schema", Some("/schemaInfo/target")))
          else Seq(Problem.warning("target-not-released", "Meta-schema binds to a draft; copy and re-check it after the target is released"))
        status ++ Selectors.check(Selectors.extract(record.document), catalog.baseUrl(target), target.document, catalog.document)
    }

  private def compile(record: SchemaRecord, support: SchemaLanguageSupport, hull: Closure): Either[Seq[Problem], CompiledSchema] = {
    val cacheable = record.isImmutable && hull.records.forall(_.isImmutable) && hull.missing.isEmpty
    val key = s"${record.uuid}:${record.etag}"
    if (cacheable) Option(compiledCache.synchronized(compiledCache.get(key))) match {
      case Some(compiled) => return Right(compiled)
      case None => ()
    }
    metrics.compilations.incrementAndGet()
    val result = support.compile(catalog.baseUrl(record), record.document, catalog.document, cacheable)
    if (cacheable) result.foreach(compiled => compiledCache.synchronized(compiledCache.put(key, compiled)))
    result
  }

  /** Kompiliertes Schema zu einer (auch fragmentierten) Schema-URL, etwa dem `schema`-Property einer Entity. */
  def compiledFor(schemaUrl: String): Either[EntityValidityState, (SchemaRecord, CompiledSchema)] =
    catalog.resolve(schemaUrl) match {
      case None => Left(EntityValidityState.SchemaMissing)
      case Some(record) => languages.get(record.language) match {
        case None => Left(EntityValidityState.SchemaMissing)
        case Some(support) =>
          val hull = closure(record)
          val fragment = schemaUrl.indexOf('#') match {
            case -1 => ""
            case index => schemaUrl.substring(index)
          }
          val result =
            if (fragment.isEmpty || fragment == "#") compile(record, support, hull)
            else {
              // Teilschema (#/$defs/... oder #anchor) ueber ein Hilfsdokument, das nur referenziert.
              val target = catalog.baseUrl(record) + fragment
              val cacheable = record.isImmutable && hull.records.forall(_.isImmutable) && hull.missing.isEmpty
              val key = s"${record.uuid}:${record.etag}:$fragment"
              Option(compiledCache.synchronized(compiledCache.get(key))).filter(_ => cacheable).map(Right(_)).getOrElse {
                metrics.compilations.incrementAndGet()
                val wrapper = ujson.Obj("$schema" -> record.language, "$ref" -> target)
                val compiled = support.compile(WrapperUrl, wrapper, catalog.document, cacheable)
                if (cacheable) compiled.foreach(value => compiledCache.synchronized(compiledCache.put(key, value)))
                compiled
              }
            }
          result.left.map(_ => EntityValidityState.SchemaMissing).map(compiled => (record, compiled))
      }
    }

  /** Validiert JSON gegen eine Schema-URL (REQ-SCH-009). */
  def validateData(schemaUrl: String, data: ujson.Value, maxErrors: Int): Either[EntityValidityState, (SchemaRecord, ValidationOutcome)] =
    compiledFor(schemaUrl).map((record, compiled) => (record, compiled.validate(data, maxErrors)))

  /** Kompaktes Entity-Validierungsergebnis (ADR-0064). */
  def validateEntity(state: EntityState): EntityValidity = {
    metrics.entityValidations.incrementAndGet()
    val result = validateData(state.schema, state.data, limits.storedErrors) match {
      case Left(missingState) =>
        val record = catalog.resolve(state.schema)
        EntityValidity(state.id, missingState, state.schema, record.map(_.uuid), record.flatMap(_.info).flatMap(_.internal).map(_.version),
          record.map(_.entityVersion), state.entityVersion, 0, Nil, clock(),
          Some(record.fold("Schema URL is not in the local registry")(_ => "Schema is not compilable; see its validity")))
      case Right((record, outcome)) =>
        EntityValidity(state.id, if (outcome.valid) EntityValidityState.Valid else EntityValidityState.Invalid, state.schema,
          Some(record.uuid), record.info.flatMap(_.internal).map(_.version), Some(record.entityVersion), state.entityVersion,
          outcome.errorCount, outcome.errors, clock())
    }
    if (result.state == EntityValidityState.Invalid) metrics.entityValidationsInvalid.incrementAndGet()
    result
  }

  /**
   * Release-Pruefung fuer den FSM-Guard (REQ-SCH-010, ADR-0064): Gueltigkeit fuer genau die gepinnte
   * Entity-Version, Referenzen nur auf Released-Schemas bzw. auf Drafts desselben Branches, deren
   * Released-Gegenstueck bereits existiert (diese werden gepinnt), Zielversion = letzte Release + 1.
   */
  def releaseCheck(record: SchemaRecord, cached: Option[SchemaValidity]): ReleaseCheck = {
    // Weicht der Cache vom Datensatz ab, wird neu gerechnet (TOCTOU, ADR-0064).
    val validity = cached.filter(value => value.entityVersion == record.entityVersion && value.etag == record.etag)
      .getOrElse(checkSchema(record))
    val problems = Seq.newBuilder[Problem]
    val info = record.info
    if (!info.exists(_.isDraft)) problems += Problem("not-a-draft", "Only drafts can be released")
    if (validity.state != ValidityState.Valid)
      problems +=Problem("schema-not-valid", s"Schema validity is '${validity.state.wire}'")

    val identity = info.flatMap(_.internal)
    val branch = info.flatMap(_.branch)
    val pins = Seq.newBuilder[(String, String)]
    record.refs.foreach { url =>
      catalog.resolve(url) match {
        case None => problems += Problem("unresolved-reference", s"Reference target '$url' is not in the local registry")
        case Some(target) if target.countsAsReleased => ()
        case Some(target) =>
          (target.info.flatMap(_.internal), target.info.flatMap(_.branch)) match {
            case (Some(targetIdentity), targetBranch) if targetBranch == branch =>
              val released = catalog.urls.releasedUrl(targetIdentity)
              if (catalog.resolve(released).exists(_.countsAsReleased)) pins += (url -> released)
              else problems += Problem("dependency-not-released", s"Release '$url' first; '$released' does not exist yet")
            case _ => problems += Problem("references-draft", s"Reference '$url' points to a draft of another branch")
          }
      }
    }

    val next = identity.map(id => catalog.lastRelease(id.classKey).fold(1)(_._1 + 1))
    for (id <- identity; expected <- next if id.version != expected)
      problems += Problem("target-version-outdated", s"Target version is ${id.version}, the next release is v$expected; retarget the branch")

    info.flatMap(_.meta).foreach { binding =>
      if (!catalog.resolve(binding.target).exists(_.countsAsReleased))
        problems += Problem("target-not-released", "A meta-schema can only be released against a released target")
    }

    ReleaseCheck(record, validity, next, identity.map(catalog.urls.releasedUrl), pins.result(), problems.result())
  }
}
