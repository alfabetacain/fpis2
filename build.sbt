val scala3Version = "3.4.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "fpis",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-encoding", "utf-8", "-deprecation", "-feature", "-no-indent"),

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
