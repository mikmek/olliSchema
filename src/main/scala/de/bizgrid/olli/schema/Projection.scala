package de.bizgrid.olli.schema

import java.util.UUID
import scala.collection.mutable
import scala.util.Try

/**
 * Projektion von `entity.events` in das Lesemodell (ADR-0063, ADR-0068).
 *
 * Idempotent je Entity-Version: ein Event, dessen Version nicht ueber der projizierten liegt, wird
 * verworfen. Die Version wird als letzter Schritt angehoben; ein Abbruch dazwischen fuehrt bei der
 * erneuten Zustellung zu denselben, idempotenten Schreibvorgaengen.
 *
 * @param lookup Fallback fuer den Stand einer Entity, die olliSchema noch nicht verfolgt (etwa ein
 *               Patch, der erstmals eine Schema-Referenz setzt). Liefert None, wenn nicht ermittelbar.
 */
final class Projection(catalog: Catalog, checker: Checker, schemaKinds: Set[UUID], metrics: SchemaMetrics,
                       lookup: UUID => Option[EntityState] = _ => None, maxDependents: Int = 500,
                       onViolation: String => Unit = _ => ()) {
  private val store = catalog.store
  private val urls = catalog.urls

  /** Eine Entity verwendet ein Schema, wenn ihr `schema`-Property eine absolute http(s)-URL ist (REQ-SCH-008). */
  def isSchemaReference(schema: String): Boolean = SchemaInfo.isAbsoluteHttpUri(schema.trim)

  def isSchemaEntity(state: EntityState): Boolean = schemaKinds(state.kind)

  private def tracked(state: EntityState): Boolean = isSchemaEntity(state) || isSchemaReference(state.schema)

  /** Wendet ein Event an. Liefert false, wenn es als Duplikat verworfen wurde. */
  def apply(event: EntityEvent, version: Long): Boolean = synchronized {
    if (store.version(event.id).exists(_ >= version)) {
      metrics.eventsDuplicate.incrementAndGet()
      false
    } else {
      val prior = catalog.entityState(event.id)
      val next = nextState(prior, event, version)
      next match {
        case Some(state) if tracked(state) => store.set(Keys.entityState(state.id), ujson.write(state.toJson))
        case _ => if (prior.nonEmpty) store.delete(Keys.entityState(event.id))
      }

      val priorSchemaEntity = prior.filter(isSchemaEntity)
      next.filter(isSchemaEntity) match {
        case Some(state) => projectSchema(state)
        case None => priorSchemaEntity.foreach(state => removeSchema(state.id))
      }

      val priorUser = prior.filter(state => !isSchemaEntity(state) && isSchemaReference(state.schema))
      next.filter(state => !isSchemaEntity(state) && isSchemaReference(state.schema)) match {
        case Some(state) => validateEntity(state, priorUser)
        case None =>
          priorUser.foreach(untrackUsage)
          if (priorUser.nonEmpty) store.delete(Keys.entityValidity(event.id))
          if (next.isEmpty && prior.isEmpty) metrics.eventsIgnored.incrementAndGet()
      }

      store.raiseVersion(event.id, version)
      metrics.eventsProcessed.incrementAndGet()
      true
    }
  }

  private def nextState(prior: Option[EntityState], event: EntityEvent, version: Long): Option[EntityState] = event match {
    case EntityEvent.Created(id, tenant, project, kind, schema, data, registeredAt) =>
      Some(EntityState(id, tenant, project, kind, schema, data, version, registeredAt))
    case EntityEvent.Deleted(_, _) => None
    case EntityEvent.Updated(id, change, registeredAt) => merge(id, prior, change, version, registeredAt, patch = false)
    case EntityEvent.Patched(id, change, registeredAt) => merge(id, prior, change, version, registeredAt, patch = true)
  }

  private def merge(id: UUID, prior: Option[EntityState], change: EntityChange, version: Long, registeredAt: Long, patch: Boolean): Option[EntityState] = {
    def applyTo(base: EntityState) = base.copy(
      tenant = change.tenant.getOrElse(base.tenant), project = change.project.getOrElse(base.project),
      kind = change.kind.getOrElse(base.kind), schema = change.schema.getOrElse(base.schema),
      data = change.data.fold(base.data)(data => if (patch) Json.mergePatch(base.data, data) else data),
      entityVersion = version, registeredAt = registeredAt)
    prior match {
      case Some(base) => Some(applyTo(base))
      case None =>
        // Bisher nicht verfolgt. Nur wenn das Event die Entity erstmals relevant macht, wird ihr voller
        // Stand gebraucht; alle anderen Entities bleiben unbeachtet.
        val becomesRelevant = change.schema.exists(isSchemaReference) || change.kind.exists(schemaKinds)
        if (!becomesRelevant) None
        else {
          metrics.entityLookups.incrementAndGet()
          lookup(id).map(applyTo).orElse {
            for {
              tenant <- change.tenant; project <- change.project; kind <- change.kind
              schema <- change.schema; data <- change.data if !patch
            } yield EntityState(id, tenant, project, kind, schema, data, version, registeredAt)
          }.orElse {
            change.schema.filter(isSchemaReference).foreach { schema =>
              putEntityValidity(EntityValidity(id, EntityValidityState.Pending, schema, None, None, None, version, 0, Nil,
                System.currentTimeMillis(), Some("Entity state is unknown to olliSchema; revalidate after the next full update")))
            }
            None
          }
        }
    }
  }

  // ---------------------------------------------------------------------------------------------
  // Schemas

  private def projectSchema(state: EntityState): Unit = {
    metrics.schemasProjected.incrementAndGet()
    val prior = catalog.record(state.id)
    val parsed = SchemaInfo.parse(state.data, state.tenant)
    val language = SchemaLanguage.normalize(state.schema)

    val candidate: SchemaRecord = parsed match {
      case Right((info, document)) =>
        val url = urls.url(info.identity)
        val stored = if (info.external.nonEmpty) document else References.withId(document, url)
        SchemaRecord(state.id, Some(url), Some(info), Nil, language, stored, state.entityVersion, state.tenant, state.project, state.registeredAt, Nil)
      case Left(problems) =>
        val document = state.data.objOpt.flatMap(_.get("document")).getOrElse(ujson.Null)
        SchemaRecord(state.id, None, None, problems, language, document, state.entityVersion, state.tenant, state.project, state.registeredAt, Nil)
    }

    // REQ-SCH-003: ein Released-Stand ist unveraenderlich; nur Folgestatus duerfen sich aendern.
    val effective = prior.filter(_.isImmutable) match {
      case Some(released) =>
        val sameContent = candidate.document == released.document && candidate.url == released.url && candidate.language == released.language
        if (!sameContent) violation(state.id, s"Released schema ${released.url.getOrElse(state.id.toString)} was changed in entity version ${state.entityVersion}; the change is ignored")
        val status = candidate.info.map(_.status).filter(_.isImmutable).getOrElse(released.info.get.status)
        released.copy(info = released.info.map(_.copy(status = status)), entityVersion = state.entityVersion, registeredAt = state.registeredAt)
      case None => candidate
    }

    // Eine UUID gehoert genau einer URL und umgekehrt (REQ-SCH-002).
    val bound = effective.url match {
      case Some(url) =>
        val owner = store.bindOnce(Keys.url(url), state.id.toString)
        if (owner == state.id.toString) effective
        else {
          metrics.urlConflicts.incrementAndGet()
          effective.copy(url = None, infoProblems = effective.infoProblems :+ Problem("url-conflict", s"URL $url already belongs to schema $owner"))
        }
      case None => effective
    }
    prior.flatMap(_.url).filterNot(bound.url.contains).foreach(old => store.unbind(Keys.url(old), state.id.toString))

    val aliasProblems = bound.info.flatMap(_.external).toSeq.flatMap { external =>
      val owner = store.bindOnce(Keys.alias(external.originalUri), state.id.toString)
      if (owner == state.id.toString) Nil
      else Seq(Problem("alias-conflict", s"Original URI ${external.originalUri} is already imported as $owner"))
    }
    prior.flatMap(_.info).flatMap(_.external).map(_.originalUri)
      .filterNot(uri => bound.info.flatMap(_.external).exists(_.originalUri == uri))
      .foreach(uri => store.unbind(Keys.alias(uri), state.id.toString))

    val refs = References.externalTargets(bound.document, catalog.baseUrl(bound)).getOrElse(Nil)
    val record = bound.copy(infoProblems = bound.infoProblems ++ aliasProblems, refs = refs)

    store.set(Keys.doc(record.uuid), ujson.write(record.toJson))
    reindex(prior, record)
    store.set(Keys.validity(record.uuid), ujson.write(checker.checkSchema(record).toJson))

    val changed = prior.forall(_.etag != record.etag) || prior.map(_.url) != Some(record.url)
    if (changed) refreshDependents(record, prior)
  }

  private def reindex(prior: Option[SchemaRecord], record: SchemaRecord): Unit = {
    val id = record.uuid.toString
    store.addToSet(Keys.All, id)
    prior.foreach(old => indexKey(old).foreach(key => store.removeFromSet(key, id)))
    indexKey(record).foreach(key => store.addToSet(key, id))

    prior.flatMap(_.info).flatMap(_.internal).filter(_ => prior.exists(_.info.exists(_.status.isImmutable)))
      .foreach(identity => store.removeRelease(identity.classKey, record.uuid))
    for (info <- record.info; identity <- info.internal if info.status.isImmutable) store.addRelease(identity.classKey, identity.version, record.uuid)

    prior.flatMap(_.info).flatMap(_.meta).foreach(binding => store.removeFromSet(Keys.meta(binding.target, binding.metaType), id))
    record.info.flatMap(_.meta).foreach(binding => store.addToSet(Keys.meta(binding.target, binding.metaType), id))

    linkReferences(prior.map(_.refs).getOrElse(Nil), record)
  }

  private def indexKey(record: SchemaRecord): Option[String] = record.info.map(_.identity).map {
    case identity: InternalIdentity => Keys.index(identity.application, identity.domain)
    case _: ExternalIdentity => Keys.External
  }

  /** Pflegt Reverse-Index (`schema:refby`) und Warteliste fuer noch nicht aufloesbare Ziele. */
  private def linkReferences(oldRefs: Seq[String], record: SchemaRecord): Unit = {
    val id = record.uuid.toString
    oldRefs.foreach { url =>
      store.removeFromSet(Keys.waiting(url), id)
      catalog.uuidFor(url).foreach(target => store.removeFromSet(Keys.refBy(target), id))
    }
    record.refs.foreach { url =>
      catalog.uuidFor(url) match {
        case Some(target) => store.addToSet(Keys.refBy(target), id)
        case None => store.addToSet(Keys.waiting(url), id)
      }
    }
  }

  /**
   * Nach einer Aenderung: wartende und referenzierende Schemas neu pruefen (transitiv, begrenzt) und
   * die Entities der betroffenen Schemas gedrosselt neu validieren (ADR-0061).
   */
  private def refreshDependents(changed: SchemaRecord, prior: Option[SchemaRecord]): Unit = {
    val addresses = (catalog.addresses(changed) ++ prior.toSeq.flatMap(catalog.addresses)).distinct
    val waiting = addresses.flatMap(url => store.members(Keys.waiting(url)))
    val visited = mutable.LinkedHashSet[UUID](changed.uuid)
    var frontier = (waiting ++ store.members(Keys.refBy(changed.uuid))).flatMap(value => Try(UUID.fromString(value)).toOption).distinct
    while (frontier.nonEmpty && visited.size <= maxDependents) {
      val next = Seq.newBuilder[UUID]
      frontier.filterNot(visited).foreach { uuid =>
        visited += uuid
        catalog.record(uuid).foreach { dependent =>
          linkReferences(dependent.refs, dependent)
          store.set(Keys.validity(uuid), ujson.write(checker.checkSchema(dependent).toJson))
          next ++= store.members(Keys.refBy(uuid)).flatMap(value => Try(UUID.fromString(value)).toOption)
        }
      }
      frontier = next.result().distinct
    }
    // Entities: Verwender aller betroffenen Schemas und alle, die bisher auf eine dieser URLs warteten.
    val users = visited.toSeq.flatMap(uuid => store.members(Keys.users(uuid))) ++ addresses.flatMap(url => store.members(Keys.missingUsers(url)))
    markStale(users.distinct)
  }

  private def markStale(entities: Seq[String]): Unit = if (entities.nonEmpty) {
    entities.flatMap(value => Try(UUID.fromString(value)).toOption).foreach { entity =>
      catalog.entityValidity(entity).filter(_.state != EntityValidityState.SchemaMissing).foreach { validity =>
        store.set(Keys.entityValidity(entity), ujson.write(validity.copy(state = EntityValidityState.Stale).toJson))
      }
    }
    store.addToSet(Keys.Revalidate, entities*)
  }

  private def removeSchema(uuid: UUID): Unit = catalog.record(uuid).foreach { record =>
    if (record.isImmutable) violation(uuid, s"Released schema ${record.url.getOrElse(uuid.toString)} was deleted in the EntityService; it stays resolvable")
    else {
      metrics.schemasRemoved.incrementAndGet()
      val id = uuid.toString
      record.url.foreach(url => store.unbind(Keys.url(url), id))
      record.info.flatMap(_.external).foreach(external => store.unbind(Keys.alias(external.originalUri), id))
      store.removeFromSet(Keys.All, id)
      indexKey(record).foreach(key => store.removeFromSet(key, id))
      record.info.flatMap(_.meta).foreach(binding => store.removeFromSet(Keys.meta(binding.target, binding.metaType), id))
      linkReferences(record.refs, record.copy(refs = Nil))
      val referers = store.members(Keys.refBy(uuid))
      val users = store.members(Keys.users(uuid))
      store.delete(Keys.doc(uuid), Keys.validity(uuid), Keys.refBy(uuid), Keys.users(uuid))
      // Referenzierer warten nun wieder auf diese URLs; ihre Gueltigkeit wird unresolved.
      referers.flatMap(value => Try(UUID.fromString(value)).toOption).flatMap(catalog.record).foreach { dependent =>
        linkReferences(dependent.refs, dependent)
        store.set(Keys.validity(dependent.uuid), ujson.write(checker.checkSchema(dependent).toJson))
      }
      markStale(users.toSeq)
    }
  }

  private def violation(uuid: UUID, message: String): Unit = {
    metrics.immutabilityViolations.incrementAndGet()
    store.pushBounded(Keys.Violations, ujson.write(ujson.Obj("uuid" -> uuid.toString, "message" -> message,
      "at" -> Json.instant(System.currentTimeMillis()))), 1000)
    onViolation(message)
  }

  // ---------------------------------------------------------------------------------------------
  // Entities

  private def usageKey(state: EntityState): String =
    catalog.uuidFor(state.schema).fold(Keys.missingUsers(References.stripFragment(state.schema.trim)))(Keys.users)

  private def untrackUsage(state: EntityState): Unit = {
    store.removeFromSet(usageKey(state), state.id.toString)
    store.removeFromSet(Keys.missingUsers(References.stripFragment(state.schema.trim)), state.id.toString)
  }

  private def validateEntity(state: EntityState, prior: Option[EntityState]): Unit = {
    prior.foreach(untrackUsage)
    store.addToSet(usageKey(state), state.id.toString)
    putEntityValidity(checker.validateEntity(state))
  }

  /** Schreibt nur, wenn das Ergebnis nicht aelter als das gespeicherte ist. */
  private def putEntityValidity(result: EntityValidity): Unit =
    if (catalog.entityValidity(result.entityId).forall(_.entityVersion <= result.entityVersion))
      store.set(Keys.entityValidity(result.entityId), ujson.write(result.toJson))

  /** Neuvalidierung einer Entity aus ihrem projizierten Stand (gedrosselte Warteschlange, API). */
  def revalidate(entity: UUID): Option[EntityValidity] = synchronized {
    metrics.revalidations.incrementAndGet()
    catalog.entityState(entity).filterNot(isSchemaEntity) match {
      case Some(state) =>
        validateEntity(state, Some(state))
        catalog.entityValidity(entity)
      case None =>
        store.delete(Keys.entityValidity(entity))
        None
    }
  }

  /** Arbeitet bis zu `batch` Eintraege der Neuvalidierungs-Warteschlange ab. */
  def drainRevalidation(batch: Int): Int = {
    val ids = store.popFromSet(Keys.Revalidate, batch).flatMap(value => Try(UUID.fromString(value)).toOption)
    ids.foreach(revalidate)
    ids.size
  }

  def pendingRevalidations: Long = store.setSize(Keys.Revalidate)
}
