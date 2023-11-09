import Dependencies._
import Settings._

val root = (project in file("."))
  .settings(commonSettings)
    .settings(
            name := "cache_deps",
            version := "0.1",
            scalaVersion := Versions.scala,
            //---------------------------------------------------------------
            organization := "io.github.alexgruperm",
            homepage := Some(url("https://github.com/AlexGruPerm/cache_deps")),
            scmInfo := Some(ScmInfo(url("https://github.com/AlexGruPerm/cache_deps"), "git@github.com:alexgruperm/cache_deps.git")),
            developers := List(Developer("AlexGruPerm", "Yakushev Aleksey", "ugr@bk.ru", url("https://github.com/AlexGruPerm"))),
            publishMavenStyle := true,
            crossPaths := true,
            versionScheme := Some("early-semver"),
            publishTo := {
              val nexus = "https://oss.sonatype.org/"
              if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
              else Some("releases" at nexus + "service/local/staging/deploy/maven2")
            },
            //---------------------------------------------------------------
            assembly / assemblyJarName := "cache-deps.jar",
            assembly / test := Def.sequential(Test / test).value,
            scalafmtOnCompile := false,
            libraryDependencies ++= (dependencies ++ testDependencies)
    )

connectInput / run := true

/*import ReleaseTransformations._
releaseCrossBuild := true
releaseProcess := Seq[ReleaseStep](
        checkSnapshotDependencies, // check that there are no SNAPSHOT dependencies
        inquireVersions, // ask user to enter the current and next verion
        runClean, // clean
        runTest, // run tests
        setReleaseVersion, // set release version in version.sbt
        commitReleaseVersion, // commit the release version
        tagRelease, // create git tag
        releaseStepCommandAndRemaining("+publishSigned"), // run +publishSigned command to sonatype stage release
        setNextVersion, // set next version in version.sbt
        commitNextVersion, // commint next version
        releaseStepCommand("sonatypeRelease"), // run sonatypeRelease and publish to maven central
        pushChanges // push changes to git
)
*/
