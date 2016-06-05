name := "aiolia"

version := "0.1"

val scalaV = "2.11.8"

scalaVersion := scalaV

libraryDependencies ++=
  "org.specs2" % "specs2-core_2.11" % "3.8.2" % "test" ::
  "org.specs2" % "specs2-mock_2.11" % "3.8.2" % "test" ::
  "org.scala-lang" % "scala-reflect" % scalaV ::
  "org.scala-lang" % "scala-compiler" % scalaV ::
  "net.coobird" % "thumbnailator" % "0.4.8" ::
  "org.ode4j" % "core" % "0.3.0" ::
  Nil

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

scalacOptions in Test ++= Seq("-Yrangepos") // for specs2

// logBuffered := false // display results as soon as they’ve been executed

scalacOptions ++=
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-deprecation" ::
  "-explaintypes" ::
  "-feature" ::
  "-language:_" ::
  "-Xlint:_" ::
  "-Ywarn-unused" ::
  // "-Xdisable-assertions" ::
  // "-optimize" ::
  // "-Yopt:_" :: // enables all 2.12 optimizations
  // "-Yinline" :: "-Yinline-warnings" ::
  Nil

// scalaxy - rewrite collection code to while loops
scalacOptions += "-Xplugin-require:scalaxy-streams"

scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams"))

scalacOptions in Test += "-Xplugin-disable:scalaxy-streams"

autoCompilerPlugins := true

resolvers += Resolver.sonatypeRepo("snapshots")

addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4")
// addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.4-SNAPSHOT")

initialize ~= { _ =>
  System.setProperty("scalaxy.streams.quiet", "true")
}

mainClass in assembly := Some("aiolia.app.ImageCompressionMeta")

test in assembly := {}

// make console life easier
initialCommands in console := """
import aiolia._
import aiolia.graph._
import aiolia.graph._
import aiolia.graph.DSL._
import aiolia.util._

val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
import universe._
"""
