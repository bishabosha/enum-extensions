val dottyVersion = "3.0.0-M1"

lazy val core = project
  .settings(
    name := "enum-extensions",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
