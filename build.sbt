val thisScalaVersion = "2.12.8"

val root = Project("root", file("."))
  .settings(
    name := "reconstruct",
    organization := "com.example",
    version := "0.1.0",
    scalaVersion := thisScalaVersion,
    /* crossScalaVersions := Seq("2.11.11", thisScalaVersion), */
    scalacOptions ++= Seq("-language:higherKinds", "-Ypartial-unification"),

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.0.0-M4" % Optional,
      "io.circe" %% "circe-core" % "0.11.1" % Optional,
      "com.propensive" %% "magnolia" % "0.10.0",

      "org.typelevel" %% "cats-laws" % "2.0.0-M4" % "test",
      "io.circe" %% "circe-parser" % "0.11.1" % "test", //TODOO: delme
      "org.scalatest" %% "scalatest" % "3.0.7" % "test",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
    )
  )
