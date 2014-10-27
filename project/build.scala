import sbt._
import Keys._

object ieslConfig {

  import edu.umass.cs.iesl.sbtbase.{IeslProject => Iesl, Config=>IeslConfig}

  def publishToIesl(vers: String): Option[Resolver] = publishToIesl(vers, Iesl.Public)

  def publishToIesl(vers: String, repotype: Iesl.RepoType): Option[Resolver] =  {
    import Iesl._
    import IeslConfig._
    def repo(name: String) = name at nexusHttpsUrl + "/content/repositories/" + name
    val isSnapshot = vers.endsWith("SNAPSHOT")
    val isPrivate = if (repotype == Private) "private-" else ""
    val repoName = isPrivate + (if (isSnapshot) "snapshots" else "releases")
    Some(repo(repoName))
  }

  var creds = Iesl.creds
}

object releaseConfig {


  import sbtrelease.ReleasePlugin._
  import sbtrelease._
  import ReleaseStateTransformations._

  val releaseSteps = Seq[ReleaseStep](
    checkSnapshotDependencies,              // : ReleaseStep
    inquireVersions,                        // : ReleaseStep
    runTest,                                // : ReleaseStep
    setReleaseVersion,                      // : ReleaseStep
    commitReleaseVersion,                   // : ReleaseStep, performs the initial git checks
    tagRelease,                             // : ReleaseStep
    // publishArtifacts,                       // : ReleaseStep, checks whether `publishTo` is properly set up
    setNextVersion,                         // : ReleaseStep
    commitNextVersion,                      // : ReleaseStep
    pushChanges                             // : ReleaseStep, also checks that an upstream branch is properly configured
  )

}
