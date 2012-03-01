package com.strong_links.scalaforms

import com.strong_links.core._
import java.io.File

object StaticResourceNode {

  val prefix = "/static."

  def apply(packageName: String, urlPath: String) = new StaticResourceNode(packageName, urlPath)
}

class StaticResourceNode(packageName: String, urlPath: String) {

  if (packageName.isEmpty)
    Errors.fatal("No package name was provided.")

  I18nConfig.checkPackageSegments(Util.split(packageName, '.'))

  val context = StaticResourceNode.prefix + packageName

  val url = IO.checkForExistingDirectory(new File(new File(urlPath).path)).toURI.toURL

  override def toString = "StaticResourceNode(package _, url _)" << (packageName, url)
}

