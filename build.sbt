name := "scala_with_cats"

version := "1.2"

scalaVersion := "2.12.5"

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Ypartial-unification",
  "-unchecked",
  "-deprecation",
  "-feature"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.1.0"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.9" % "test"


parallelExecution in Test := false
