import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) {
  val snapshots = ScalaToolsSnapshots

  val specs = "org.scala-tools.testing" %% "specs" % "1.6.8-SNAPSHOT" % "test" withSources()
  val scalacheck = "org.scala-tools.testing" %% "scalacheck" % "1.8" % "test" withSources()
  lazy val junit = "junit" % "junit" % "4.4" % "test" withSources() // for running specs with IDEA

  override def compileOptions = CompileOption("-unchecked") :: super.compileOptions.toList
  override def testCompileOptions = CompileOption("-Ydependent-method-types") :: super.testCompileOptions.toList

  lazy val testPrepare = task { None } dependsOn(copyTestResources, testCompile)

  // Additional JUnit XML report generation support
  import eu.henkelmann.sbt.JUnitXmlTestsListener
  //create a listener that writes to the normal output directory
  def junitXmlListener: TestReportListener = new JUnitXmlTestsListener(outputPath.toString)
  //add the new listener to the already configured ones
  override def testListeners: Seq[TestReportListener] = super.testListeners ++ Seq(junitXmlListener)
}