import sbt._
import Keys._

object Settings {

  val commonSettings: Seq[Def.Setting[_]] = Seq(
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-explaintypes",
      "-feature",
      "-unchecked",
      "-language:postfixOps",
      "-Xcheckinit",
      "-Xfatal-warnings",
      "-Ywarn-unused:params,-implicits"
    )
  )

}

object Dependencies {
  val cats = Seq(
    "org.typelevel" %% "cats-core"   % Versions.catsCore,
    "org.typelevel" %% "cats-effect" % Versions.catsEffect
  )

  val dependencies = cats

  val testDependencies = Seq(
    "org.scalacheck" %% "scalacheck"          % Versions.scalacheck,
    "org.typelevel"  %% "scalacheck-effect"   % Versions.scalacheckEffectVersion,
    "org.typelevel"  %% "munit-cats-effect" % Versions.munitCatsEffect
  ).map(_ % Test)
}

object Versions {
  val scala                   = "2.13.1"
  val catsCore                = "2.10.0"
  val catsEffect              = "3.4.8"
  val scalacheckEffectVersion = "1.0.4"
  val scalacheck              = "1.15.4"
  val munitCatsEffect         = "2.0.0-M3"
}
