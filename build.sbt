scalaVersion := "2.11.2"

libraryDependencies ++=
  Seq(
    "org.scala-lang" %  "scala-reflect" % "2.11.2",
    "org.specs2"     %% "specs2-core"   % "2.4.4")

scalacOptions in GlobalScope ++= Seq("-Xcheckinit", "-Xlint", "-deprecation", "-unchecked", "-feature", "-language:_")
