import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) {
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.7" % "test" withSources()
  val scalacheck = "org.scala-tools.testing" %% "scalacheck" % "1.8" % "test" withSources()
  lazy val junit = "junit" % "junit" % "4.4" % "test" withSources() // for running specs with IDEA

  lazy val testPrepare = task { None } dependsOn(copyTestResources, testCompile)
}