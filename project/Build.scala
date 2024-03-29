import sbt._
import Keys._


object Buildz extends Build {

  val core = "com.strong-links" %% "core" % "0.2"  
  val cglib = "cglib" % "cglib-nodep" % "2.2"
  val bcel = "org.apache.bcel" % "bcel" % "5.2"  
  val squeryl = "org.squeryl" %% "squeryl" % "0.9.5-extended-types-poc2"
  val versionOfScala = "2.9.1"
  val unfilteredVersion = "0.6.0"
				   
  def buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.strong-links",
    version := "0.3",
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
    base = file("."),    
    settings = buildSettings ++ Epoxy.init ++ I18nGen.init ++ Seq(
     com.strong_links.i18ngen.I18nGen.i18nConfigs := Seq(
         new I18nConfig("com.strong_links.scalaforms", en_US, Seq(fr), Seq(fr_CA, fr_FR), Map(en -> en_US)),
         new I18nConfig("com.strong_links.scalaforms.templates", en_US, Seq(fr), Seq(fr_CA, fr_FR), Map(en -> en_US))
     ),
    Epoxy.epoxyTemplateRoots <<= sourceDirectory.map(src =>  Seq(src/ "main/templates")),
    Epoxy.epoxyResourceRoots <<= sourceDirectory.map(src =>  Nil: Seq[File]),
	  libraryDependencies  ++=  Seq(        
	      core,
	      bcel,
        "net.databinder" %% "unfiltered-filter" % unfilteredVersion,
        "net.databinder" %% "unfiltered-jetty" % unfilteredVersion,
        "net.databinder" %% "unfiltered-json" % unfilteredVersion,
        "org.slf4j" % "slf4j-api" % "1.6.1",
        "com.h2database" % "h2" % "1.3.160",
        "org.cometd.java" % "cometd-java-server" % "2.3.1",
        "org.cometd.java" % "cometd-java-common" % "2.3.1",        
        //"org.scala-tools.time" %% "time" % "0.5",
        squeryl
      )	    
    )
  )
 
}
