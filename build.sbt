lazy val root = project
  .in(file("."))
  .settings(
    scalaVersion                           := "3.1.2-RC1-bin-20211025-968dd1b-NIGHTLY",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
