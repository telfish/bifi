import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) {
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.7" % "test" withSources()

  lazy val testPrepare = task { None } dependsOn(copyTestResources, testCompile)
}