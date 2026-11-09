package de.bizgrid.olli.schema

import java.util.UUID
import scala.collection.mutable
import scala.util.Try

/**
 * Projektion von `entity.events` in das Lesemodell (ADR-0063, ADR-0068).
 *
 * Atomar je Event (SCH-A08): alle Aenderungen werden gegen eine [[TransactionView]] berechnet, die den
 * eigenen, noch nicht geschriebenen Stand sieht, und dann zusammen mit der Versionspruefung in einem
 * Schritt geschrieben. Bricht die Verarbeitung vorher ab, bleibt das Lesemodell unveraendert; eine
 * erneute Zustellung rechnet vom alten Stand aus neu. Ein bereits projiziertes Event wird verworfen.
 *
 * @param lookup Fallback fuer den Stand einer Entity, die olliSchema noch nicht verfolgt (etwa ein
 *               Patch, der erstmals eine Schema-Referenz setzt). Liefert None, wenn nicht ermittelbar.
 */
final class Projection(catalog: Catalog, checker: Checker, schemaKinds: Set[UUID], metrics: SchemaMetrics,
                       lookup: UUID => Option[EntityState] = _ => None, maxDependents: Int = 500,
                       onViolation: String => Unit = _ => (), maxAttempts: Int = 3) {
  private val base = catalog.store

  /** Eine Entity verwendet ein Schema, wenn ihr `schema`-Property eine absolute http(s)-URL ist (REQ-SCH-008). */
  def isSchemaReference(schema: String): Boolean = SchemaInfo.isAbsoluteHttpUri(schema.trim)

  def isSchemaEntity(state: EntityState): Boolean = schemaKinds(state.kind)

  /** Wendet ein Event an. Liefert false, wenn es als Duplikat verworfen wurde. */
  def apply(event: EntityEvent, version: Long): Boolean = synchronized {
    if (base.version(event.id).exists(_ >= version)) {
      metrics.eventsDuplicate.incrementAndGet()
      false
    } else transact(Some(event.id -> version))(_.applyEvent(event, version)) match {
      case CommitResult.Applied =>
        metrics.eventsProcessed.incrementAndGet()
        true
      case _ =>
        metrics.eventsDuplicate.incrementAndGet()
        false
    }
  }

  /**
   * Fuehrt `body` auf einer frischen Transaktion aus und schreibt das Ergebnis. Bei einem Bindungskonflikt
   * (ein anderer Schreiber war schneller) wird vom neuen Stand aus bis zu `maxAttempts`-mal neu gerechnet.
   */
  private def transact(version: Option[(UUID, Long)], extra: Seq[StoreOp] = Nil)(body: Work => Unit): CommitResult = {
    var attempt = 0
    var outcome: Option[CommitResult] = None
    while (outcome.isEmpty) {
      attempt += 1
      val work = new Work(new TransactionView(base))
      body(work)
      work.tx.commit(extra, version) match {
        case CommitResult.Conflict(key) if attempt < maxAttempts => ()
        case CommitResult.Conflict(key) => throw new IllegalStateException(s"Concurrent update of $key; giving up after $attempt attempts")
        case other =>
          if (other == CommitResult.Applied) work.publish()
          outcome = Some(other)
      }
    }
    outcome.get
  }

  /** Neuvalidierung einer Entity aus ihrem projizierten Stand (gedrosselte Warteschlange, API). */
  def revalidate(entity: UUID): Option[EntityValidity] = synchronized {
    metrics.revalidations.incrementAndGet()
    // Der Eintrag verlaesst die Warteschlange erst zusammen mit dem neuen Ergebnis (SCH-A08).
    transact(None, Seq(StoreOp.SRem(Keys.Revalidate, Seq(entity.toString))))(_.revalidate(entity))
    catalog.entityValidity(entity)
  }

  /** Arbeitet bis zu `batch` Eintraege der Neuvalidierungs-Warteschlange ab. */
  def drainRevalidation(batch: Int): Int = {
    val ids = base.sample(Keys.Revalidate, batch).flatMap(value => Try(UUID.fromString(value)).toOption)
    ids.foreach(revalidate)
    ids.size
  }

  def pendingRevalidations: Long = base.setSize(Keys.Revalidate)

  /** Die Verarbeitung eines Events bzw. einer Neuvalidierung auf einer Transaktionssicht. */
  private final class Work(val tx: TransactionView) {
    private val store: SchemaStore = tx
    private val view = catalog.on(tx)
    private val check = checker.on(view)
    private val urls = view.urls
    private val counters = mutable.ArrayBuffer.empty[java.util.concurrent.atomic.AtomicLong]
    private val messages = mutable.ArrayBuffer.empty[String]

    /** Metriken und Meldungen erst nach erfolgreichem Schreiben (keine Doppelzaehlung bei Wiederholung). */
    def publish(): Unit = {
      counters.foreach(_.incrementAndGet())
      messages.foreach(onViolation)
    }

    private def tracked(state: EntityState): Boolean = isSchemaEntity(state) || isSchemaReference(state.schema)

    def applyEvent(event: EntityEvent, version: Long): Unit = {
      val prior = view.entityState(event.id)
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
          if (next.isEmpty && prior.isEmpty) counters += metrics.eventsIgnored
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
      def applyTo(state: EntityState) = state.copy(
        tenant = change.tenant.getOrElse(state.tenant), project = change.project.getOrElse(state.project),
        kind = change.kind.getOrElse(state.kind), schema = change.schema.getOrElse(state.schema),
        data = change.data.fold(state.data)(data => if (patch) Json.mergePatch(state.data, data) else data),
        entityVersion = version, registeredAt = registeredAt)
      prior match {
        case Some(state) => Some(applyTo(state))
        case None =>
          // Bisher nicht verfolgt. Nur wenn das Event die Entity erstmals relevant macht, wird ihr voller
          // Stand gebraucht; alle anderen Entities bleiben unbeachtet.
          val becomesRelevant = change.schema.exists(isSchemaReference) || change.kind.exists(schemaKinds)
          if (!becomesRelevant) None
          else {
            counters += metrics.entityLookups
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

    // -------------------------------------------------------------------------------------------
    // Schemas

    private def projectSchema(state: EntityState): Unit = {
      counters += metrics.schemasProjected
      val prior = view.record(state.id)
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

      // Eine URL gehoert zu jedem Zeitpunkt hoechstens einer UUID (REQ-SCH-002).
      val bound = effective.url match {
        case Some(url) =>
          val owner = store.bindOnce(Keys.url(url), state.id.toString)
          if (owner == state.id.toString) effective
          else {
            counters += metrics.urlConflicts
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

      val refs = References.externalTargets(bound.document, view.baseUrl(bound)).getOrElse(Nil)
      val record = bound.copy(infoProblems = bound.infoProblems ++ aliasProblems, refs = refs)

      store.set(Keys.doc(record.uuid), ujson.write(record.toJson))
      reindex(prior, record)
      store.set(Keys.validity(record.uuid), ujson.write(check.checkSchema(record).toJson))

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
        view.uuidFor(url).foreach(target => store.removeFromSet(Keys.refBy(target), id))
      }
      record.refs.foreach { url =>
        view.uuidFor(url) match {
          case Some(target) => store.addToSet(Keys.refBy(target), id)
          case None => store.addToSet(Keys.waiting(url), id)
        }
      }
    }

    /** Schemas, die von `record` abhaengen: per `$ref` referenzierend und als Meta-Schema an es gebunden (SCH-A04). */
    private def dependentsOf(uuid: UUID, addresses: Seq[String]): Seq[UUID] =
      (store.members(Keys.refBy(uuid)).toSeq.flatMap(value => Try(UUID.fromString(value)).toOption) ++ view.metaSchemaIds(addresses)).distinct

    /**
     * Nach einer Aenderung: wartende, referenzierende und gebundene Meta-Schemas transitiv neu pruefen
     * (begrenzt) und die Entities der betroffenen Schemas gedrosselt neu validieren (ADR-0061).
     */
    private def refreshDependents(changed: SchemaRecord, prior: Option[SchemaRecord]): Unit = {
      val addresses = (view.addresses(changed) ++ prior.toSeq.flatMap(view.addresses)).distinct
      val waiting = addresses.flatMap(url => store.members(Keys.waiting(url))).flatMap(value => Try(UUID.fromString(value)).toOption)
      val visited = mutable.LinkedHashSet[UUID](changed.uuid)
      var frontier = (waiting ++ dependentsOf(changed.uuid, addresses)).distinct
      while (frontier.nonEmpty && visited.size <= maxDependents) {
        val next = Seq.newBuilder[UUID]
        frontier.filterNot(visited).foreach { uuid =>
          visited += uuid
          view.record(uuid).foreach { dependent =>
            linkReferences(dependent.refs, dependent)
            store.set(Keys.validity(uuid), ujson.write(check.checkSchema(dependent).toJson))
            next ++= dependentsOf(uuid, view.addresses(dependent))
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
        view.entityValidity(entity).filter(_.state != EntityValidityState.SchemaMissing).foreach { validity =>
          store.set(Keys.entityValidity(entity), ujson.write(validity.copy(state = EntityValidityState.Stale).toJson))
        }
      }
      store.addToSet(Keys.Revalidate, entities*)
    }

    private def removeSchema(uuid: UUID): Unit = view.record(uuid).foreach { record =>
      if (record.isImmutable) violation(uuid, s"Released schema ${record.url.getOrElse(uuid.toString)} was deleted in the EntityService; it stays resolvable")
      else {
        counters += metrics.schemasRemoved
        val id = uuid.toString
        val addresses = view.addresses(record)
        val dependents = dependentsOf(uuid, addresses)
        record.url.foreach(url => store.unbind(Keys.url(url), id))
        record.info.flatMap(_.external).foreach(external => store.unbind(Keys.alias(external.originalUri), id))
        store.removeFromSet(Keys.All, id)
        indexKey(record).foreach(key => store.removeFromSet(key, id))
        record.info.flatMap(_.meta).foreach(binding => store.removeFromSet(Keys.meta(binding.target, binding.metaType), id))
        linkReferences(record.refs, record.copy(refs = Nil))
        val users = store.members(Keys.users(uuid))
        store.delete(Keys.doc(uuid), Keys.validity(uuid), Keys.refBy(uuid), Keys.users(uuid))
        // Referenzierer warten nun wieder auf diese URLs, gebundene Meta-Schemas verlieren ihr Ziel.
        dependents.flatMap(view.record).foreach { dependent =>
          linkReferences(dependent.refs, dependent)
          store.set(Keys.validity(dependent.uuid), ujson.write(check.checkSchema(dependent).toJson))
        }
        markStale(users.toSeq)
      }
    }

    private def violation(uuid: UUID, message: String): Unit = {
      counters += metrics.immutabilityViolations
      messages += message
      store.pushBounded(Keys.Violations, ujson.write(ujson.Obj("uuid" -> uuid.toString, "message" -> message,
        "at" -> Json.instant(System.currentTimeMillis()))), 1000)
    }

    // -------------------------------------------------------------------------------------------
    // Entities

    private def usageKey(state: EntityState): String =
      view.uuidFor(state.schema).fold(Keys.missingUsers(References.stripFragment(state.schema.trim)))(Keys.users)

    private def untrackUsage(state: EntityState): Unit = {
      store.removeFromSet(usageKey(state), state.id.toString)
      store.removeFromSet(Keys.missingUsers(References.stripFragment(state.schema.trim)), state.id.toString)
    }

    private def validateEntity(state: EntityState, prior: Option[EntityState]): Unit = {
      prior.foreach(untrackUsage)
      store.addToSet(usageKey(state), state.id.toString)
      putEntityValidity(check.validateEntity(state))
    }

    /** Schreibt nur, wenn das Ergebnis nicht aelter als das gespeicherte ist. */
    private def putEntityValidity(result: EntityValidity): Unit =
      if (view.entityValidity(result.entityId).forall(_.entityVersion <= result.entityVersion))
        store.set(Keys.entityValidity(result.entityId), ujson.write(result.toJson))

    def revalidate(entity: UUID): Unit =
      view.entityState(entity).filterNot(isSchemaEntity) match {
        case Some(state) => validateEntity(state, Some(state))
        case None => store.delete(Keys.entityValidity(entity))
      }
  }
}
