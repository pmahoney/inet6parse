scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "3")
