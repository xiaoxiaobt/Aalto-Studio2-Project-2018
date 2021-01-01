lazy val root = (project in file(".")).
  settings(
    name := "Recipe-book",
    version := "1.0",
    scalaVersion := "2.12.2",
    parallelExecution in Test := false,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3",
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "junit" % "junit" % "4.13.1" % Test

  )