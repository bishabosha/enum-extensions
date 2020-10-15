val dottyVersion = "0.27.0-RC1"

lazy val core = project
  .settings(
    name := "enum-extensions",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
