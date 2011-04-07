import sbt._
import reaktor.scct.ScctProject

class ScalaConeProject(info: ProjectInfo) extends DefaultProject(info) with ScctProject {

  // Additional repositories
  val snapshots = "snapshots" at "http://scala-tools.org/repo-snapshots"
  val releases  = "releases" at "http://scala-tools.org/repo-releases"

  // Compile Dependencies
  val slf4jApi = "org.slf4j" % "slf4j-api" % "1.6.1" % "compile"
  val logback = "ch.qos.logback" % "logback-classic" % "0.9.28" % "compile"
  val slf4s = "com.weiglewilczek.slf4s" %% "slf4s" % "1.0.4"

  // Test dependencies
  val specs2 = "org.specs2" %% "specs2" % "1.1" % "test"

  // Spec2 configuration
  def specs2Framework = new TestFramework("org.specs2.runner.SpecsFramework")
  override def testFrameworks = super.testFrameworks ++ Seq(specs2Framework)
}

 

 