lazy val root = (project in file(".")).
  settings(
    name := "Recipe-book",
    version := "1.0",
    scalaVersion := "2.12.12",
    parallelExecution in Test := false,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.3",
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
  )