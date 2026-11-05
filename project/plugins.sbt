// Ein Artefakt pro Modul: JavaAppPackaging erzeugt lib/ + bin/, DockerPlugin daraus ein Image.
// Version 1.10.0 liegt bereits im lokalen Coursier-Cache und baut damit auch offline.
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.10.0")
