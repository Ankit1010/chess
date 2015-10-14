seq(Revolver.settings: _*) //for sbt-revolver

resolvers += "pathikrit" at "https://dl.bintray.com/pathikrit/maven/" //for better-files

val autoImports: String = List("scalaz._","Scalaz._", "atto._", "Atto._",
  "scala.io._", "java.io._", "java.nio._","java.nio.charset._",
  "chess._", "chess.models._", "chess.Parsers._").mkString("import ",",",";\n")

lazy val chess = (project in file(".")).
  settings(
    organization := "org.ankits",
    version := "0.1.0",
    scalaVersion := "2.11.7",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.1.4",
      "com.github.nscala-time" %% "nscala-time" % "2.2.0",
      "org.tpolecat" %% "atto-core"  % "0.4.2",
      "com.github.pathikrit" %% "better-files" % "2.12.2",
      "org.scalatest" %% "scalatest" % "2.2.4"
    ),
    initialCommands in console := autoImports
  )
