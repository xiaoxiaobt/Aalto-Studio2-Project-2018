lazy val root = (project in file(".")).
  settings(
    name := "Recipe-book",
    version := "1.0",
    scalaVersion := "2.13.4",
    parallelExecution in Test := true,
    scalacOptions := Seq("-unchecked", "-deprecation"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.3",
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
  )