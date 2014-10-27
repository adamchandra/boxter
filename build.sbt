organization := "org.adamchandra"

name := "boxter"

scalaVersion := "2.11.2"

libraryDependencies := Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.0"
)

releaseSettings

ReleaseKeys.releaseProcess := releaseConfig.releaseSteps

//Iesl.scalaSettings(Iesl.DebugVars)

publishTo := ieslConfig.publishToIesl(version.value)

ieslConfig.creds
