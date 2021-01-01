lazy val root = (project in file(".")).
  settings(
    name := "Recipe-book",
    version := "1.0",
    scalaVersion := "2.12.2",
    parallelExecution in Test := true,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3",
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
  )