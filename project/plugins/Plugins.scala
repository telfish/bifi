import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  object Repositories {
    val JUnitXMLReportRepo= MavenRepository("Christoph's Maven Repo", "http://maven.henkelmann.eu/")
  }

  import Repositories._
  lazy val junitXMLReportModuleConfig = ModuleConfiguration("eu.henkelmann", JUnitXMLReportRepo)

  lazy val jUnitXMLReport = "eu.henkelmann" % "junit_xml_listener" % "0.2"
}
