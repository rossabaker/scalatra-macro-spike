organization := "com.rossabaker.scalatra"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.0-M7"

libraryDependencies <+= 
  (scalaVersion)("org.scala-lang" % "scala-compiler" % _)
