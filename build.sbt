lazy val root = (project in file(".")).settings(
  name := "Recipe-book",
  version := "1.0",
  scalaVersion := "2.13.5",
  Test / parallelExecution := true,
  scalacOptions := Seq("-unchecked", "-deprecation"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.7" % Test,
    "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
  )
)
