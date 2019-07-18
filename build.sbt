import com.typesafe.sbt.SbtGit.git
import _root_.sbtcrossproject.CrossPlugin.autoImport.CrossType
import microsites._

lazy val libs = org.typelevel.libraries
  .addJVM(name = "scala-influxdb-client", version = "0.6.1", org = "com.paulgoldbaum")
  .add(name = "scalacheck-shapeless_1.14", version = "1.2.3", org="com.github.alexarchambault")
  .add(name = "newtype", version = "0.4.2", org="io.estatico")

val apache2 = "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html")
val gh = GitHubSettings(org = "kailuowang", proj = "tempus", publishOrg = "com.kailuowang", license = apache2)

lazy val rootSettings = buildSettings ++ commonSettings ++ publishSettings ++ scoverageSettings

lazy val tempus = project.in(file("."))
  .aggregate(core, laws, tests, storage, bench)
  .settings(
    rootSettings,
    crossScalaVersions := Nil,
    noPublishSettings)


lazy val core = project
  .settings(
    name := "tempus-core",
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
    rootSettings,
    libs.dependencies("cats-core"),
    libs.dependencies("newtype"),
    simulacrumSettings(libs, false),
    doctestTestFramework := DoctestTestFramework.ScalaTest,
    libs.testDependencies( "scalatest","scalacheck")
  )


lazy val laws = project
  .dependsOn(core)
  .settings(
    name := "tempus-laws",
    rootSettings,
    disciplineDependencies,
    libs.dependencies("cats-laws"))

lazy val tests = project
  .dependsOn(laws)
  .dependsOn(core % "test -> test")
  .aggregate(core, laws) //to run doctests
  .settings(
    name := "tempus-tests",
    rootSettings,
    noPublishSettings,
    disciplineDependencies)
  .settings(libs.testDependencies( "scalatest", "scalacheck-shapeless_1.14", "cats-testkit"))

lazy val bench = project
  .dependsOn(core)
  .settings(
    name := "tempus-bench",
    noPublishSettings,
    rootSettings,
    coverageEnabled := false,
    libs.dependencies("kittens"))
  .enablePlugins(JmhPlugin)


lazy val storage = project
  .dependsOn(core % "compile -> compile; it -> test")
  .aggregate(core)
  .configs(IntegrationTest)
  .settings(
    name := "tempus-storage",
    rootSettings,
    Defaults.itSettings,
    libs.dependency("scalatest", Some(IntegrationTest.name)),
    libs.dependency("scalacheck", Some(IntegrationTest.name)),
    libs.dependencies(
      "cats-effect",
      "scala-influxdb-client"
    )
  )



lazy val devKai = Developer("Kailuo Wang", "@kailuowang", "kailuo.wang@gmail.com", new java.net.URL("http://kailuowang.com"))
lazy val commonSettings = sharedCommonSettings ++ Seq(
  parallelExecution in Test := false,
  scalaVersion := libs.vers("scalac_2.12"),
  crossScalaVersions := Seq(libs.vers("scalac_2.11"), scalaVersion.value),
  developers := List(devKai)) ++ scalacAllSettings ++ unidocCommonSettings ++ addCompilerPlugins(libs, "kind-projector") ++ Seq(
  scalacOptions ++= (if(priorTo2_13(scalaVersion.value)) Nil else
    Seq("-Ywarn-unused:-implicits")) ++ Seq("-Xlint:-package-object-classes")
)


lazy val buildSettings = sharedBuildSettings(gh, libs)
lazy val publishSettings = sharedPublishSettings(gh) ++ credentialSettings ++ sharedReleaseProcess

lazy val scoverageSettings = sharedScoverageSettings(60)

lazy val disciplineDependencies = libs.dependencies("discipline-core", "scalacheck")
