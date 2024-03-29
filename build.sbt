lazy val root = (project in file(".")).settings(
  name := "Recipe-book",
  version := "1.0",
  scalaVersion := "3.1.3",
  Test / parallelExecution := false,
  scalacOptions := Seq("-unchecked", "-deprecation"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.12" % Test,
    "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
  )
)
