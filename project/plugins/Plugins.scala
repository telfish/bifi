import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  val TelfishNexus = "Telfish Nexus" at "http://silo/content/repositories/releases/"
  val telfish = "com.telfish" % "sbt-plugin" % "1.2.1"
}
