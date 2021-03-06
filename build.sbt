course := "progfun1"
assignment := "recfun"

scalaVersion := "2.13.5"

scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")

libraryDependencies += "org.scalameta" %% "munit" % "0.7.27" % Test

testFrameworks += new TestFramework("munit.Framework")
