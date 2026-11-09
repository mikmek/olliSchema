ThisBuild / organization := "de.bizgrid"
ThisBuild / version := sys.props.getOrElse("olli.build.version", "0.1.0-SNAPSHOT")
ThisBuild / scalaVersion := "3.6.2"

lazy val pekkoVersion = "1.6.0"
lazy val pekkoHttpVersion = "1.4.0"
lazy val kafkaVersion = "3.9.0"
lazy val ujsonVersion = "4.4.3"

// Publiziertes Artefakt aus ../olliEventCore (`sbt publishAll`), nicht das Nachbarverzeichnis (ADR-0028).
lazy val eventCoreVersion = "0.1.0-SNAPSHOT"

/**
 * olliSchema (ADR-0063): ein Modul, ein Deploymentartefakt (ADR-0025, ADR-0026). Kein eigenes
 * Write-Modell: die Projektion entsteht aus `entity.events`, das Lesemodell liegt in einer eigenen
 * Redis-Instanz (ADR-0068).
 */
lazy val root = project.in(file("."))
  .enablePlugins(JavaAppPackaging, DockerPlugin)
  .settings(
    name := "olli-schema",
    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-feature", "-unchecked", "-Xfatal-warnings"),
    Compile / mainClass := Some("de.bizgrid.olli.schema.Main"),
    // Zweites Startskript bin/reproject-main: Einmalwerkzeug fuer die Reprojektion (REQ-SCH-018).
    Compile / discoveredMainClasses := Seq("de.bizgrid.olli.schema.ReprojectMain"),
    Test / fork := true,
    libraryDependencies ++= Seq(
      "de.bizgrid" %% "olli-event-core" % eventCoreVersion,
      "org.apache.pekko" %% "pekko-actor-typed" % pekkoVersion,
      "org.apache.pekko" %% "pekko-stream" % pekkoVersion,
      "org.apache.pekko" %% "pekko-http" % pekkoHttpVersion,
      "org.apache.pekko" %% "pekko-slf4j" % pekkoVersion,
      "org.apache.kafka" % "kafka-clients" % kafkaVersion,
      "redis.clients" % "jedis" % "5.2.0",
      // JSON Schema 2020-12 (ADR-0067). Bringt Jackson 3 (tools.jackson) mit; Jackson bleibt auf den
      // Validator-Adapter beschraenkt, alles andere arbeitet mit ujson.
      "com.networknt" % "json-schema-validator" % "3.0.0",
      "com.lihaoyi" %% "ujson" % ujsonVersion,
      "ch.qos.logback" % "logback-classic" % "1.5.32",
      "com.typesafe" % "config" % "1.4.7",
      "org.scalatest" %% "scalatest" % "3.2.20" % Test,
      // Echte Redis fuer den Test des atomaren Commit-Skripts (SCH-A08); der Test wird uebersprungen,
      // wenn die mitgelieferte Binaerdatei auf der Plattform nicht startet.
      "com.github.kstyrc" % "embedded-redis" % "0.6" % Test,
      "org.apache.pekko" %% "pekko-http-testkit" % pekkoHttpVersion % Test,
      // ScalatestRouteTest needs the classic testkit; pekko-http-testkit declares it as provided.
      "org.apache.pekko" %% "pekko-testkit" % pekkoVersion % Test,
      "org.apache.pekko" %% "pekko-stream-testkit" % pekkoVersion % Test
    ),
    maintainer := "bizGrid",
    dockerBaseImage := "eclipse-temurin:21-jre-jammy",
    dockerExposedPorts := Seq(8110),
    dockerUpdateLatest := !sys.props.contains("olli.build.version"),
    dockerLabels ++= Seq(
      "olli.source.commit" -> "org.opencontainers.image.revision",
      "olli.eventcore.commit" -> "de.bizgrid.eventcore.revision",
      "olli.build.version" -> "de.bizgrid.release"
    ).flatMap { case (property, label) => sys.props.get(property).map(label -> _) }.toMap,
    Universal / javaOptions ++= Seq("-J-Xms256m", "-J-Xmx1g", "-Dfile.encoding=UTF-8")
  )
