name := "boxes"

organization := "cc.acs"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.0-M3"

initialCommands in console := """
  // import acs.boxes.Boxes._
  import scalaz._
  import Scalaz._
  // val welcome = (text("Boxes imported...") % text("  ---") %| text("Commence hacking..")) +| text("==>")
  // printBox(welcome)
"""

// set Ivy logging to be at the highest level
ivyLoggingLevel := UpdateLogging.Full

// set the prompt (for this build) to include the project id.
shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

// set the prompt (for the current project) to include the username
shellPrompt := { state => System.getProperty("user.name") + "> " }

// disable printing timing information, but still print [success]
showTiming := false

// only use a single thread for building
parallelExecution := true

// only show warnings and errors on the screen for all tasks (the default is Info)
//  individual tasks can then be more verbose using the previous setting
logLevel := Level.Info

// only store messages at info and above (the default is Debug)
//   this is the logging level for replaying logging with 'last'
persistLogLevel := Level.Debug

// only show 10 lines of stack traces
// traceLevel := 10
traceLevel := 0

