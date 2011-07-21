import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) with TelfishProject {
  override def testCompileOptions = CompileOption("-Ydependent-method-types") :: super.testCompileOptions.toList
}
