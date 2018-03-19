name := "scala_with_cats"

version := "1.0"

scalaVersion := "2.12.4"

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Ypartial-unification",
  "-unchecked",
  "-deprecation",
  "-feature"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2" % "test"


parallelExecution in Test := false
