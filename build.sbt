name := "delphi-webapi"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.12.4"

val akkaVersion = "2.5.16"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
)

val akkaHttpVersion = "10.1.5"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion
)
//Including http client for elastic4s
libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.5.3"

val elastic4sVersion = "6.3.8"
libraryDependencies ++= Seq(
  "com.sksamuel.elastic4s" %% "elastic4s-core" % elastic4sVersion,
  //Excluding default 4.5.2 due to https://snyk.io/vuln/SNYK-JAVA-ORGAPACHEHTTPCOMPONENTS-31517
  "com.sksamuel.elastic4s" %% "elastic4s-http" % elastic4sVersion exclude("org.apache.httpcomponents", "httpclient"),
  "com.sksamuel.elastic4s" %% "elastic4s-http-streams" % elastic4sVersion,
)


libraryDependencies += "com.pauldijou" %% "jwt-core" % "1.0.0"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.4"
libraryDependencies += "io.spray" %% "spray-json" % "1.3.5"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "it,test"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3" % Runtime

lazy val webapi = (project in file(".")).
  //https://www.scala-sbt.org/1.x/docs/Testing.html
  configs(IntegrationTest).
  settings(
    Defaults.itSettings,
  ).
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

// Pinning secure versions of insecure transitive libraryDependencies
// Please update when updating dependencies above (including Play plugin)
libraryDependencies ++= Seq(
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.9.10.1"
)

trapExit := false
fork := true
connectInput := true