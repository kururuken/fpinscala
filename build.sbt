val commonSettings = Seq(
  scalaVersion := "2.12.11",
  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.0.1" % "test")
)

// libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0"
// libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )
