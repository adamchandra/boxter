name := "boxter-brown"

organization := "cc.acs"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.1"

libraryDependencies +=  "org.scalaz" %% "scalaz-core" % "6.0.3"

initialCommands in console := """
  import acs.boxes.Boxes._
  import scalaz._
  import Scalaz._
  println("Boxes imported...")
"""

seq(lsSettings :_*)


// set Ivy logging to be at the highest level
ivyLoggingLevel := UpdateLogging.Full

// set the prompt (for this build) to include the project id.
shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

// set the prompt (for the current project) to include the username
shellPrompt := { state => System.getProperty("user.name") + "> " }

// disable printing timing information, but still print [success]
showTiming := false


// disable using the Scala version in output paths and artifacts
// crossPaths := false

// fork a new JVM for 'run' and 'test:run'
// fork := true

// fork a new JVM for 'test:run', but not 'run'
// fork in Test := true

// add a JVM option to use when forking a JVM for 'run'
// javaOptions += "-Xmx2G"

// only use a single thread for building
parallelExecution := true

// Execute tests in the current project serially
//   Tests from other projects may still run concurrently.
// parallelExecution in Test := false

// only show warnings and errors on the screen for compilations.
//  this applies to both test:compile and compile and is Info by default
// logLevel in compile := Level.Warn

// only show warnings and errors on the screen for all tasks (the default is Info)
//  individual tasks can then be more verbose using the previous setting
logLevel := Level.Warn

// only store messages at info and above (the default is Debug)
//   this is the logging level for replaying logging with 'last'
persistLogLevel := Level.Debug

// only show 10 lines of stack traces
// traceLevel := 10

// only show stack traces up to the first sbt stack frame
traceLevel := 0


// publishTo <<= publishTo({repo => (Some("IESL Repo" at (repo map (r => r.name + (if (repo.endsWith("SNAPSHOT")) "snapshots" else "releases")))))})
publishTo := Some("snapshots" at "http://iesl.cs.umass.edu:8081/nexus/content/repositories/snapshots")

// Specify a file containing credentials for publishing. The format is:
credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

