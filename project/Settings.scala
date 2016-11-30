import sbt._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

/**
 * Application settings. Configure the build for your application here.
 * You normally don't have to touch the actual build definition after this.
 */
object Settings {
  /** The name of your application */
  val name = "RAT"

  /** The version of your application */
  val version = "1.1.4"

  /** Options for the scala compiler */
  val scalacOptions = Seq(
    "-Xlint",
    "-unchecked",
    "-deprecation",
    "-feature"
  )

  /** Declare global dependency versions here to avoid mismatches in multi part dependencies */
  object versions {
    val scala = "2.11.8"
    val scalaDom = "0.9.1"
    val scalajsReact = "0.11.2"
    val scalaCSS = "0.4.1"
    val log4js = "1.4.10"
    val autowire = "0.2.5"
    val booPickle = "1.2.4"
    val diode = "1.0.0"
    val uTest = "0.4.3"

    val react = "15.1.0"
    val jQuery = "1.11.1"
    val bootstrap = "4.0.0-alpha.4"//"3.3.6"
    val chartjs = "2.1.3"

    val playScripts = "0.5.0"
    val quill = "0.10.0"
    val quill_jdbc = "3.8.11.2"

    val scalarx = "0.3.1"
    val pathjs = "0.3.2"
    val dagre = "0.7.4"

    val dgraph = "0.1.1-SNAPSHOT"
    val nlputils = "0.1.1-SNAPSHOT"

    val quicklens = "1.4.8"
    val circe = "0.6.1"
  }

  /**
   * These dependencies are shared between JS and JVM projects
   * the special %%% function selects the correct version for each project
   */
  val sharedDependencies = Def.setting(Seq(
    "com.lihaoyi" %%% "autowire" % versions.autowire,
    "me.chrons" %%% "boopickle" % versions.booPickle,
    "com.github.omidb" %%% "dgraph" % versions.dgraph,
    "com.github.omidb" %%% "nlputils" % versions.nlputils,
    "com.softwaremill.quicklens" %%% "quicklens" % versions.quicklens
  ))

  /** Dependencies only used by the JVM project */
  val jvmDependencies = Def.setting(Seq(
    "com.vmunier" %% "play-scalajs-scripts" % versions.playScripts,
    "org.webjars" % "font-awesome" % "4.3.0-1" % Provided,
//    "org.webjars.bower" % "bootstrap" % versions.bootstrap exclude("org.webjars", "jquery"), //% Provided,
    "com.lihaoyi" %% "utest" % versions.uTest % Test,
    "org.xerial" % "sqlite-jdbc" % versions.quill_jdbc,
    "io.getquill" %% "quill-jdbc" % versions.quill,
    "com.github.mrmechko" %% "strips2" % "0.0.1-SNAPSHOT",
    "io.circe" %% "circe-core" % versions.circe,
    "io.circe" %% "circe-generic" % versions.circe,
    "io.circe" %% "circe-parser" % versions.circe

  ))

  /** Dependencies only used by the JS project (note the use of %%% instead of %%) */
  val scalajsDependencies = Def.setting(Seq(
    "com.github.japgolly.scalajs-react" %%% "core" % versions.scalajsReact,
    "com.github.japgolly.scalajs-react" %%% "extra" % versions.scalajsReact,
    "com.github.japgolly.scalacss" %%% "ext-react" % versions.scalaCSS,
    "me.chrons" %%% "diode" % versions.diode,
    "me.chrons" %%% "diode-react" % versions.diode,
    "org.scala-js" %%% "scalajs-dom" % versions.scalaDom,
    "com.lihaoyi" %%% "utest" % versions.uTest % Test,
    "com.lihaoyi" %%% "scalarx" % versions.scalarx,
    "eu.unicredit" %%% "paths-scala-js" % versions.pathjs
  ))

  /** Dependencies for external JS libs that are bundled into a single .js file according to dependency order */
  val jsDependencies = Def.setting(Seq(
    "org.webjars.bower" % "react" % versions.react / "react-with-addons.js" minified "react-with-addons.min.js" commonJSName "React",
    "org.webjars.bower" % "react" % versions.react / "react-dom.js" minified "react-dom.min.js" dependsOn "react-with-addons.js" commonJSName "ReactDOM",
    //"org.webjars" % "jquery" % versions.jQuery / "jquery.js" minified "jquery.min.js",
//    "org.webjars.bower" % "bootstrap" % versions.bootstrap / "dist/bootstrap.js" minified "dist/bootstrap.min.js"  dependsOn "jquery.js",
    "org.webjars" % "chartjs" % versions.chartjs / "Chart.js" minified "Chart.min.js",
    "org.webjars" % "log4javascript" % versions.log4js / "js/log4javascript_uncompressed.js" minified "js/log4javascript.js"
//    "org.webjars.bower" % "dagre" % versions.dagre / "dagre.core.js" minified "dagre.core.min.js"
  ))
}
