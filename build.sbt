name := "BigData and Machine Learning"
 
version := "1.0"

scalaVersion := "2.12.12"

run / fork := true
run / connectInput := true

// Determine OS version of JavaFX binaries
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")

libraryDependencies += "org.scalafx" %% "scalafx" % "11-R16"

libraryDependencies ++= javaFXModules.map( m =>
  "org.openjfx" % s"javafx-$m" % "11" classifier osName
)

// libraryDependencies += "org.apache.spark" %% "spark-core" % "3.0.0"
// libraryDependencies += "org.apache.spark" %% "spark-sql" % "3.0.0"
// libraryDependencies += "org.apache.spark" %% "spark-mllib" % "3.0.0"
// libraryDependencies += "org.apache.spark" %% "spark-graphx" % "3.0.0"
libraryDependencies += "org.apache.spark" %% "spark-core" % "3.0.0" % "provided"
libraryDependencies += "org.apache.spark" %% "spark-sql" % "3.0.0" % "provided"
libraryDependencies += "org.apache.spark" %% "spark-mllib" % "3.0.0" % "provided"
libraryDependencies += "org.apache.spark" %% "spark-graphx" % "3.0.0" % "provided"
