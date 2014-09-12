name := "la-scala-coding-dan"

version := "1.0-SNAPSHOT"

organization := "lascala"

scalaVersion := "2.11.2"

//javacOptions ++= Seq("-source", "1.5")

libraryDependencies ++= {
  val scalaIoVersion = "0.4.3"
  Seq(
    //"com.oracle" % "ojdbc6" % "11.2.0.3",
    //"com.oracle" % "ojdbc5" % "11.2.0.2.0",
    //"com.typesafe.slick" %% "slick" % "1.0.1",
    "com.github.scala-incubator.io" %% "scala-io-core" % scalaIoVersion,
    "com.github.scala-incubator.io" %% "scala-io-file" % scalaIoVersion,
    "org.specs2" %% "specs2" % "2.4" % "test",
    "org.scalatest" %% "scalatest" % "2.2.1" % "test"
  )
}

//EclipseKeys.withSource := true

testOptions in Test += Tests.Argument("-oD")

// for fork
//javaOptions in run += "-Xmx64M"

connectInput in run := true

//fork in run := true
