lazy val root = (project in file(".")).
  settings(
    name := "Recipe-book",
    version := "1.0",
    scalaVersion := "2.13.5",
    parallelExecution in Test := true,
    scalacOptions := Seq("-unchecked", "-deprecation"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.6",
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
  )
