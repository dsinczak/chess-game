val cats = "org.typelevel" %% "cats-core" % "1.0.0"
val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5" % "test"
val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

val root = (project in file("."))
  .settings(
    name := """chess-game""",
    version := "1.0",
    scalaVersion := "2.12.4"
  ).settings(
  libraryDependencies ++= Seq(
    cats,
    scalaTest,
    scalaCheck
  ))
  .settings(scalacOptions += "-Ypartial-unification") // for cats higher-kinded types

