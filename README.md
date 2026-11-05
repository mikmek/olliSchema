# olliSchema

`olliSchema` ist die Autorität über alle Schemas einer Plattform-Instanz (Namensraum
`https://bizgrid.de/ollischema/`). Schemas sind Entities mit Schema-`kind` im EntityService; olliSchema
hat kein eigenes Write-Modell, sondern projiziert `entity.events` in eine eigene Redis-Instanz und ist
für Auflösung, Kompilierung, Validierung, Fremdschema-Import und Meta-Schemas zuständig
([ADR-0063](../doc/adr/ADR-0063-ollischema-hybrider-service-schnitt.md)).

| Service | Port | Verantwortung |
|---|---:|---|
| olliSchema | 8110 | Schema-Auslieferung per URL/UUID, Schema- und Entity-Validierung, CheckReferer, Release-Prüfung, Import |
| olli-schema-redis | 6380 (Host) | Lesemodell, nur für olliSchema |

- Beschreibung, Schema-Entity-Vertrag, Redis-Schlüssel, Konfiguration, Metriken: [doc/schema-service.md](../doc/schema-service.md)
- Anforderungen: [doc/schema-requirements.md](../doc/schema-requirements.md) (REQ-SCH-001 bis REQ-SCH-018)
- Vertrag: [doc/olli-schema-api.openapi.yaml](doc/olli-schema-api.openapi.yaml)

## Bauen

Benötigt das publizierte Artefakt `de.bizgrid:olli-event-core_3` (`sbt publishAll` in
`../olliEventCore`, [ADR-0028](../doc/adr/ADR-0028-publizierte-bibliotheken.md)) und JDK 17 oder neuer
(Jackson 3 im JSON-Schema-Validator).

```powershell
sbt test Docker/publishLocal
cp conf\olli-schema.conf.example conf\olli-schema.conf
docker compose -f docker-compose.app.yml up -d
```

Lokal ohne Container: `sbt stage` und `-Dconfig.file=<absoluter Pfad>\conf\local.conf` (Vorlage
`conf/local.conf.example`). Zur Laufzeit braucht olliSchema Kafka (`entity.events`) und seine Redis;
EntityIndex und EntityRegistry nur für CheckReferer bzw. den Import.

Reprojektion bei gestopptem Service: `target/universal/stage/bin/reproject-main -Dconfig.file=...`
löscht das Lesemodell und die Offsets der Consumer-Gruppe.
