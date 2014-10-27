import sbt._
import Keys._
import edu.umass.cs.iesl.sbtbase.Dependencies
import edu.umass.cs.iesl.sbtbase.IeslProject._

object Builder extends Build {
  val vers = "1.0"
  val organization = "org.adamchandra"
  val scalaV = "2.10.2"

  implicit val allDeps: Dependencies = new Dependencies()
  import allDeps._

  lazy val boxter = {
    val deps: Seq[ModuleID] = Seq(
      scalazCore()
    )

    (Project("boxter", new java.io.File("."))
      .ieslSetup(vers, deps, Public, WithSnapshotDependencies, org = organization, conflict = ConflictStrict)
      .settings(scalacOptions ++= List("-feature", "-language:implicitConversions", "-language:postfixOps", "-language:reflectiveCalls", "-language:existentials"))
      .settings(scalaVersion := scalaV)
    )
  }
}

