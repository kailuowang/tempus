import com.typesafe.sbt.SbtGit.git
import _root_.sbtcrossproject.CrossPlugin.autoImport.CrossType
import microsites._

lazy val libs = org.typelevel.libraries
  .addJVM(name = "scala-influxdb-client", version = "0.6.1", org = "com.paulgoldbaum")
  .add(name = "scalacheck-shapeless_1.14", version = "1.2.0", org="com.github.alexarchambault")
  .add(name = "newtype", version = "0.4.2", org="io.estatico")

val apache2 = "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html")
val gh = GitHubSettings(org = "kailuowang", proj = "tempus", publishOrg = "com.kailuowang", license = apache2)

lazy val rootSettings = buildSettings ++ commonSettings ++ publishSettings ++ scoverageSettings
lazy val module = mkModuleFactory(gh.proj, mkConfig(rootSettings, commonJvmSettings, commonJsSettings))
lazy val prj = mkPrjFactory(rootSettings)

lazy val tempus = project
  .configure(mkRootConfig(rootSettings, rootJVM))
  .aggregate(rootJVM, rootJS)
  .settings(noPublishSettings)


lazy val rootJVM = project
  .configure(mkRootJvmConfig(gh.proj, rootSettings, commonJvmSettings))
  .aggregate(timeSeriesJVM, timeSeriesLawsJVM, timeSeriesTestsJVM, timeSeriesStorage, timeSeriesBench)
  .settings(noPublishSettings)


lazy val rootJS = project
  .configure(mkRootJsConfig(gh.proj, rootSettings, commonJsSettings))
  .aggregate(timeSeriesJS, timeSeriesLawsJS, timeSeriesTestsJS)
  .settings(noPublishSettings)

lazy val timeSeries = prj(timeSeriesM)
lazy val timeSeriesJVM = timeSeriesM.jvm
lazy val timeSeriesJS = timeSeriesM.js
lazy val timeSeriesM = module("time-series-core", CrossType.Pure)
  .settings(
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
    libs.dependencies("cats-core"),
    libs.dependencies("newtype"),
    simulacrumSettings(libs, false),
    doctestTestFramework := DoctestTestFramework.ScalaTest,
    libs.testDependencies( "scalatest","scalacheck")
  ).jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-java-time" % "0.2.3"
  )


lazy val timeSeriesLaws = prj(timeSeriesLawsM)
lazy val timeSeriesLawsJVM = timeSeriesLawsM.jvm
lazy val timeSeriesLawsJS = timeSeriesLawsM.js
lazy val timeSeriesLawsM = module("time-series-laws", CrossType.Pure)
  .dependsOn(timeSeriesM)
  .settings(disciplineDependencies)
  .settings(libs.dependencies("cats-laws"))

lazy val timeSeriesTests = prj(timeSeriesTestsM)
lazy val timeSeriesTestsJVM = timeSeriesTestsM.jvm
lazy val timeSeriesTestsJS = timeSeriesTestsM.js
lazy val timeSeriesTestsM = module("time-series-tests", CrossType.Pure)
  .dependsOn(timeSeriesLawsM)
  .dependsOn(timeSeriesM % "test -> test")
  .aggregate(timeSeriesM, timeSeriesLawsM) //to run doctests
  .settings(disciplineDependencies)
  .settings(libs.testDependencies( "scalatest", "scalacheck-shapeless_1.14", "cats-testkit"))

lazy val timeSeriesBench = Project("time-series-bench", file("time-series-bench"))
  .dependsOn(timeSeriesJVM)
  .settings(
    moduleName := "tempus-timeSeries-bench",
    noPublishSettings,
    commonJvmSettings,
    coverageEnabled := false,
    libs.dependencies("kittens"))
  .enablePlugins(JmhPlugin)


lazy val timeSeriesStorage = Project("time-series-storage", file("time-series-storage"))
  .dependsOn(timeSeriesJVM % "compile -> compile; test -> test")
  .aggregate(timeSeriesJVM)
  .configs(IntegrationTest)
  .settings(
    moduleName := "tempus-timeSeries-storage",
  
    libs.testDependencies("scalatest", "scalacheck"),
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

lazy val commonJvmSettings = Seq()

lazy val publishSettings = sharedPublishSettings(gh) ++ credentialSettings ++ sharedReleaseProcess

lazy val scoverageSettings = sharedScoverageSettings(60)

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  // currently sbt-doctest doesn't work in JS builds
  // https://github.com/tkawachi/sbt-doctest/issues/52
  doctestGenTests := Seq.empty
)

lazy val disciplineDependencies = libs.dependencies("discipline-core", "scalacheck")