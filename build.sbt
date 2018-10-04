name := "delphi-webapi"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.12.4"

val akkaHttpVersion = "10.1.5"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.4"

libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.0.11"
libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % "10.0.11"
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.12"
libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.1"
libraryDependencies += "io.spray" %% "spray-json" % "1.3.3"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.11.1"

val elastic4sVersion = "6.3.0"
libraryDependencies ++= Seq(
  "com.sksamuel.elastic4s" %% "elastic4s-core" % elastic4sVersion,


  // for the http client
  "com.sksamuel.elastic4s" %% "elastic4s-http" % elastic4sVersion,

  // if you want to use reactive streams
  "com.sksamuel.elastic4s" %% "elastic4s-http-streams" % elastic4sVersion,

)


lazy val webapi = (project in file(".")).
  enablePlugins(JavaAppPackaging).
  enablePlugins(DockerPlugin).
  enablePlugins(ScalastylePlugin).
  settings(
    dockerBaseImage := "openjdk:jre-alpine"
  ).
  enablePlugins(AshScriptPlugin).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "de.upb.cs.swt.delphi.webapi"
  )

scalastyleConfig := baseDirectory.value / "project" / "scalastyle-config.xml"
