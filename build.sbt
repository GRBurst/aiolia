name := "aiolia"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.7.2" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")
