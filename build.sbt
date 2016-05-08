name := "aiolia"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies ++=
  "org.specs2" %% "specs2-core" % "3.7.2" % "test" ::
  "org.specs2" %% "specs2-mock" % "3.7.2" % "test" ::
  Nil

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
  "-Xdisable-assertions" ::
  "-optimize" ::
  Nil

initialCommands in console := """
import aiolia._
import aiolia.graph._
import aiolia.graph.types._
import aiolia.graph.dsl._
import aiolia.helpers._
"""
