name := "scala_with_cats"

version := "1.3"

scalaVersion := "2.13.1"

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-unchecked",
  "-deprecation",
  "-feature"
)

libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.0"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.3" % Test

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.19" % Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

parallelExecution in Test := false
