package com.strong_links.scalaforms

import com.strong_links.core._
import java.io.File

case class StaticResourceNode(contextName: String, urlPath: String) {

  if (contextName.isEmpty)
    Errors.fatal("No context name was provided.")

  if (contextName.exists(!_.isLower))
    Errors.fatal("Invalid context name _" << contextName, "Only lowercase letters are allowed here.")

  val url = {
    //val node = new File(OS.translatePath(urlPath))
    val node = new File(urlPath)
    IO.checkForExistingDirectory(node)
    node.toURI.toURL
  }

  override def toString = "StaticResourceNode(context _, url _)" << (contextName, url)
}

