ThisBuild / scalaVersion := "3.0.0"
ThisBuild / libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

lazy val core = project
  .settings(
    name := "enum-extensions",
    version := "0.1.0"
  )

lazy val examples = project
  .dependsOn(core)
  .settings(
    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.6.6"
  )
