ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.9"

lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

// Add JavaFX dependencies
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")

lazy val root = (project in file("."))
  .settings(
    name := "EVM",
    libraryDependencies += "org.scala-lang.modules"    %% "scala-parser-combinators" % "2.1.1",
    libraryDependencies += "org.scalafx" %% "scalafx" % "18.0.1-R28",
//    libraryDependencies += "org.openjfx" % "javafx" % "12.0.2" pomOnly(),
    libraryDependencies ++= javaFXModules.map(m => "org.openjfx" % s"javafx-$m" % "11" classifier osName),
    libraryDependencies ++= Seq(
      "org.apache.poi" % "poi" % "3.9",
      "org.apache.poi" % "poi-ooxml" % "3.9"
    ),

  )
