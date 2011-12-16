
import sbt._
import Keys._


object Buildz extends Build {

  val core = "net.strong_links" %% "core" % "0.2"  
  val cglib = "cglib" % "cglib-nodep" % "2.2"
  val bcel = "org.apache.bcel" % "bcel" % "5.2"  
  val squeryl = "org.squeryl" %% "squeryl" % "0.9.5-SNAPSHOT"            
  val versionOfScala = "2.9.1"
  val jettyVersion = "7.5.4.v20111024"
				   
  def buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "net.strong_links",
    version := "0.2",
    scalaVersion := versionOfScala,
    logLevel in Global := Level.Warn,
    publishArtifact in packageDoc := false,
    resolvers += ScalaToolsSnapshots
  )

  lazy val root = Project(
    id = "root",
    base = file("."),    
    settings = buildSettings
  ) aggregate(scalaforms)

  lazy val build = Project(
    id = "project",
    base = file("project"),    
    settings = buildSettings ++ Seq(    
      sbtPlugin := true
    )
  )
   
  import net.strong_links.epoxy.Epoxy


  lazy val scalaforms = Project(
    id = "scalaforms",
    base = file("scalaforms"),    
    settings = buildSettings ++ Epoxy.init ++ Seq(
/*    I18nGen(
      new I18nCatalog("net.strong_links.scalaforms", "./scalaforms/src/main/scala", "fr"),
      new I18nCatalog("net.strong_links.scalaforms", "./scalaforms/src/main/scala", "fr", "CA"),
      new I18nCatalog("net.strong_links.scalaforms", "./scalaforms/src/main/scala", "en", "UK")
    ),
*/    
    Epoxy.epoxyTemplateRoots <<= sourceDirectory.map(src =>  Seq(src/ "main/templates")),
    Epoxy.epoxyResourceRoots <<= sourceDirectory.map(src =>  Nil: Seq[File]),
	  libraryDependencies  ++=  Seq(        
	      core,
	      bcel,
        "net.databinder" %% "unfiltered-filter" % "0.5.1",
        "net.databinder" %% "unfiltered-jetty" % "0.5.1",      
        "org.slf4j" % "slf4j-api" % "1.6.1",
        "ch.qos.logback" % "logback-classic" % "1.0.0",
        "org.eclipse.jetty" % "jetty-webapp" % jettyVersion,
        "com.h2database" % "h2" % "1.3.160",
        "org.cometd.java" % "cometd-java-server" % "2.3.1",
        "org.cometd.java" % "cometd-java-common" % "2.3.1",        
        //"org.scala-tools.time" %% "time" % "0.5",
        squeryl
      )	    
    )
  )
 
}
