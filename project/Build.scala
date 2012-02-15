import sbt._
import Keys._


object Buildz extends Build {

  val core = "com.strong-links" %% "core" % "0.2"  
  val cglib = "cglib" % "cglib-nodep" % "2.2"
  val bcel = "org.apache.bcel" % "bcel" % "5.2"  
  val squeryl = "org.squeryl" %% "squeryl" % "0.9.5-extended-types-poc2"
  val versionOfScala = "2.9.1"
  val jettyVersion = "7.5.4.v20111024"
				   
  def buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.strong-links",
    version := "0.2",
    scalaVersion := versionOfScala,
    resolvers += ScalaToolsSnapshots
  )
   
  import com.strong_links.epoxy.Epoxy
  import com.strong_links.core._
  import com.strong_links.i18ngen.I18nGen
  import com.strong_links.i18ngen.I18nGen._
  import java.util.Locale
  import I18nStock._
  
  lazy val scalaforms = Project(
    id = "scalaforms",
    base = file("scalaforms"),    
    settings = buildSettings ++ Epoxy.init ++ I18nGen.init ++ Seq(
     com.strong_links.i18ngen.I18nGen.i18nConfigs := Seq(
         new I18nConfig("com.strong_links.scalaforms", en_US, Seq(fr), Seq(fr_CA, fr_FR)),
         new I18nConfig("com.strong_links.scalaforms.templates", en_US, Seq(fr), Seq(fr_CA, fr_FR))
     ),
    Epoxy.epoxyTemplateRoots <<= sourceDirectory.map(src =>  Seq(src/ "main/templates")),
    Epoxy.epoxyResourceRoots <<= sourceDirectory.map(src =>  Nil: Seq[File]),
	  libraryDependencies  ++=  Seq(        
	      core,
	      bcel,
        "net.databinder" %% "unfiltered-filter" % "0.5.1",
        "net.databinder" %% "unfiltered-jetty" % "0.5.1",
        "net.databinder" %% "unfiltered-json" % "0.5.1",
        "org.slf4j" % "slf4j-api" % "1.6.1",
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
