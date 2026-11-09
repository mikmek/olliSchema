# Lokaler Build und Deployment auf olli-server

Stand: 2026-11-07, ergänzt 2026-11-09 (Voraussetzungen, Werkzeugtests). Umsetzung für olliSchema als erster Service.
GitHub speichert Quellcode; Build/Tests laufen auf dem Notebook. Es werden keine GitHub
Actions oder Registry benötigt. Release `20261107T151434Z-91761c2624e7` wurde übertragen
und direkt in einer SSH-Sitzung aktiviert. Readiness vom Notebook und der Statusaufruf
über PowerShell einschließlich sudo sind bestätigt. Zweiter vollständiger Rollout und
ein realer Rollback-Test stehen noch aus.

## Voraussetzungen

- Windows PowerShell 5.1 oder PowerShell 7, Git, sbt, JDK 21 und laufendes Docker Desktop.
- Saubere, committete Repositories olliSchema und benachbartes olliEventCore.
- SSH-Alias `olli-server`; bei Bedarf WireGuard aktiv. Der private Schlüssel muss im
  Windows-SSH-Agenten geladen sein. sudo wird weiterhin interaktiv bestätigt.
- Server: Python 3.12+, Docker/Compose, Netz `infrastructure_default`, laufender Kafka-Broker
  und die Entity-APIs. Bestehendes Redis-Image `olli-migration/image-b6636bae9624:backup`.
- HDD unter `/srv/olli-data` tatsächlich gemountet; keine öffentliche Portfreigabe erforderlich.
- `/srv/olli/config/infrastructure.env` mit expliziter privater `OLLI_BIND_ADDRESS` (nicht `0.0.0.0`)
  und mindestens 3 GiB frei auf der System-SSD; beides prüft der Preflight.

Der Build publiziert olliEventCore aus dem geprüften Commit neu in die lokalen Paket-Caches.
Dadurch wird das bisherige SNAPSHOT nicht einfach aus einem unbekannten Cache übernommen.
Unveränderliche Bibliotheksversionen bleiben eine spätere Verbesserung.
Die Quellstandprüfung nimmt ausschließlich generierte sbt-Verzeichnisse `target`,
`project/target` und `project/project` aus: Im bisherigen EventCore-Repository sind
teilweise Buildartefakte versioniert, die sich bei jedem Build ändern.
Diese Artefakte werden nicht als neuer Quellstand committed oder übertragen.

## Einmalige Installation des Serverwerkzeugs

PowerShell auf dem Notebook, aus dem olliSchema-Verzeichnis:

```powershell
ssh olli-server "mkdir -p /srv/olli/deploy/schema-bootstrap"
scp .\deploy\server.py .\deploy\install.sh .\deploy\olli-schema.conf olli-server:/srv/olli/deploy/schema-bootstrap/
```

In der SSH-Sitzung:

```bash
sudo bash /srv/olli/deploy/schema-bootstrap/install.sh
sudo /usr/local/lib/olli-deploy/schema.py status
```

Das installiert ein root-eigenes Werkzeug unter `/usr/local/lib/olli-deploy/schema.py`
und die anfängliche Konfiguration. Es startet keine Container und ändert keine sudo-Regeln.
Bestehende Laufzeitkonfiguration wird bei erneuter Werkzeuginstallation beibehalten.
Die Vorlage schaltet Fremdschema-Import nicht frei und verändert keine Entity-Daten.

## Build und Rollout

Aus dem Repository, bei Bedarf mit `powershell -File` oder `pwsh -File` starten:

```powershell
.\scripts\Deploy-OlliSchema.ps1 -Action Build
```

Das prüft beide Git-Arbeitsbäume, testet/publiziert EventCore, testet/baut olliSchema,
kennzeichnet das Image mit UTC-Zeit und Commit und erzeugt unter
`target\deploy\<release>` ein komprimiertes Imagearchiv, Manifest und Buildlog.
JAVA_HOME/PATH werden nur für den Prozess umgestellt und anschließend wiederhergestellt.
Bei abweichendem Installationsort `-JavaHome 'C:\Pfad\zum\jdk-21'` verwenden.
Während eines Builds keinen zweiten Build/Publikationslauf derselben Repositories starten.

Ein fertiges Paket übertragen und aktivieren:

```powershell
.\scripts\Deploy-OlliSchema.ps1 -Action Deploy -ReleaseDirectory '.\target\deploy\<release>'
```

Ohne `-ReleaseDirectory` baut `-Action Deploy` zuerst neu. Ein erfolgreich gebautes Paket
kann ohne erneuten Build übertragen werden. Der Server prüft SHA256 und Image-Labels,
archiviert auf HDD, lädt das Image und prüft beim Start HTTP-Readiness statt nur den Port.
Eine Release-ID wird nicht überschrieben. Bei Abbruch Zustand/Logs prüfen und nicht dieselbe
teilweise angelegte Release-ID blind erneut verwenden.

Startzeit, Release und Exitcode werden lokal als `deployment.log` gespeichert.
Die SSH-Ausgabe bleibt direkt am Terminal, damit sudo-Passwortabfragen sichtbar bleiben.
Serverseitige Docker-Befehlsausgaben werden im Releaseordner protokolliert.
Anmeldedaten werden nicht im Skript hinterlegt. Vor jedem Remote-Vorgang wird die
Agent-Anmeldung mit BatchMode und zehn Sekunden Verbindungs-Timeout geprüft.
Ohne geladenen Schlüssel bricht der Ablauf früh ab, statt mehrfach die Passphrase abzufragen.
Keepalives erkennen eine abgebrochene Verbindung nach erfolgreicher Anmeldung;
sie ersetzen nicht die Agent-Anmeldung und lösen keinen Daten-Rollback aus.

Der Windows-Dienst `ssh-agent` wurde aktiviert. Schlüssel laden und Verbindung prüfen:

```powershell
ssh-add "$env:USERPROFILE\.ssh\olli_server_ed25519"
ssh -o BatchMode=yes -o ConnectTimeout=10 olli-server "echo SSH_OK"
```

Die ursprüngliche Abbruchursache war im SSH-Protokoll nicht eindeutig erkennbar.
Nach Laden des Schlüssels funktionierten SSH_OK und der interaktive sudo-Statusaufruf.

## Verhalten auf dem Server

| Pfad | Inhalt |
|---|---|
| `/srv/olli/incoming/schema/<release>` | hochgeladenes Archiv und Manifest; frank |
| `/srv/olli-data/releases/olli-schema/<release>` | geprüftes Imagearchiv, Manifest, Docker-Deploymentlog; root |
| `/srv/olli/deploy/schema/releases/<release>/compose.yaml` | feste Compose-Definition dieses Releases |
| `/srv/olli/deploy/schema/state.json` | zuletzt erfolgreicher Stand, Vorgänger, ggf. fehlgeschlagener Versuch |
| `/srv/olli/config/schema/olli-schema.conf` | aktuelle Vorlage für folgende Deployments |
| `/srv/olli/config/schema/releases/<release>/olli-schema.conf` | Konfigurationssnapshot für dieses Release |
| `olli-schema-redis-data` | eigene Redis-Persistenz auf SSD |

Aktive Images bleiben im Docker-Speicher auf SSD; Archive liegen auf HDD.
Es erfolgt noch keine automatische Löschung alter Releases, Images oder Uploads.
Aufbewahrung/Platzüberwachung müssen nach dem ersten Rollout ergänzt werden.

Compose-Projekt `olli-schema`, Dienste `schema` und `schema-redis`.
Nur API-Port 8110 wird an die konfigurierte private Server-IP gebunden.
Die eigene Redis erhält keine Host-Portfreigabe und ersetzt nicht den EntityIndex-Redis.
Die Consumer-Gruppe `olli-schema-z420-v1` liest beim ersten Start die noch vorhandenen
`entity.events`. Das ist keine Garantie eines vollständigen historischen Bestands:
Kafka-Retention und der tatsächliche Projektionsstand sind bei der Abnahme zu prüfen.
Keine bestehenden Consumer-Gruppen, Offsets oder anderen Redis-Daten werden gelöscht.

## Rollback

```powershell
.\scripts\Deploy-OlliSchema.ps1 -Action Status
.\scripts\Deploy-OlliSchema.ps1 -Action Rollback
```

Rollback stellt die vorherige Anwendung und deren Konfigurationssnapshot wieder her.
Nach einem gescheiterten Update ohne automatische Rückkehr ist das Ziel der zuletzt
erfolgreiche Stand. Beim allerersten Release gibt es noch keinen Vorgänger.
Redis-Daten und verarbeitete Ereignisse werden **nicht** zurückgesetzt.
Rollback nur bei kompatiblen Daten-/Protokollständen verwenden.

Automatischer Rollback ist standardmäßig ausgeschaltet. Für einen zuvor fachlich auf
Rückwärtskompatibilität geprüften Build kann `-RollbackSafe` gesetzt werden.
Diese Entscheidung wird im Manifest festgehalten. Ein fehlgeschlagener Erststart bzw.
ein nicht als kompatibel freigegebenes Update stoppt den Schema-Dienst und erhält Redis/Daten.
Infrastruktur- und Datenbankupgrades sind kein Bestandteil dieses Skripts.

## Abnahme und Prüfungen

Nach dem ersten Rollout `/health/ready` auf Port 8110, Logs, Kafka-Verarbeitung,
vorhandene Schemaauflösung und den lokalen Client prüfen. Readiness zeigt nur technische
Bereitschaft, keinen abgeschlossenen historischen Projektionsaufbau.
Der kanonische Namensraum `https://bizgrid.de/ollischema/` richtet keine öffentliche
Domain-/Reverse-Proxy-Verbindung ein.

Werkzeugtests ohne Serverzugriff:

```powershell
python -m unittest discover -s deploy/tests -v
powershell -File deploy/tests/Test-NativeArguments.ps1
```

Der PowerShell-Test prüft die Argumentweitergabe von `Invoke-Checked` an native Programme;
`unittest discover` erfasst ihn nicht.

Sie prüfen korrupte Archive, Dirty-Source-Abweisung, HTTP-Healthcheck/Redis-Abgrenzung
und den fehlgeschlagenen kompatiblen Rollout. Ein realer Server-Rollback ist zusätzlich
gezielt zu erproben. Skripte führen keinen Datenreset und keine Reprojektion aus.
