name := "la-scala-coding-dan"

version := "1.0-SNAPSHOT"

organization := "lascala"

scalaVersion := "2.10.4"

//javacOptions ++= Seq("-source", "1.5")

libraryDependencies ++= Seq(
  //"com.oracle" % "ojdbc6" % "11.2.0.3",
  //"com.oracle" % "ojdbc5" % "11.2.0.2.0",
  //"com.typesafe.slick" %% "slick" % "1.0.1",
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.2",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "org.specs2" %% "specs2" % "2.3.10" % "test",
  "junit" % "junit" % "4.10" % "test"
)

//EclipseKeys.withSource := true

testOptions in Test += Tests.Argument("-oD")

// for fork
//javaOptions in run += "-Xmx64M"

connectInput in run := true

//fork in run := true

