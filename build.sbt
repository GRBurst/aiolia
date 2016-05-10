name := "aiolia"

version := "0.1"

val scalaV = "2.11.8"

scalaVersion := scalaV

libraryDependencies ++=
  "org.specs2" % "specs2-core_2.11" % "3.7.2" % "test" ::
  "org.specs2" % "specs2-mock_2.11" % "3.7.2" % "test" ::
  "org.scala-lang" % "scala-reflect" % scalaV ::
  "org.scala-lang" % "scala-compiler" % scalaV ::
  "net.coobird" % "thumbnailator" % "0.4.8" ::
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
  //"-Yinline", "-Yinline-warnings" ::
  "-language:_" ::
  "-Xlint:_" ::
  "-Ywarn-unused" ::
  // "-Xdisable-assertions" ::
  // "-Yopt:_" :: // enables all optimizations
  Nil

initialCommands in console := """
import aiolia._
import aiolia.graph._
import aiolia.graph.types._
import aiolia.graph.dsl._
import aiolia.helpers._

val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
import universe._
"""
