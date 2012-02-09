package com.strong_links.scalaforms

import com.strong_links.core._

import com.strong_links.scalaforms.schema._
import com.strong_links.scalaforms.squeryl.SquerylFacade._
import javax.servlet.http.HttpSession
import unfiltered.request._
import javax.servlet.http.HttpServletRequest
import com.sun.org.apache.xalan.internal.xsltc.compiler.Output
import java.io.PrintWriter
import javax.servlet.http.HttpSession

trait OutputContext {
  val i18nLocale: I18nLocale
  val out: ServerOutputStream
}

class InteractionContext(var iws: IdentityWithinServer, val identityManager: IdentityManager,
  val uriExtracter: UriExtracter, val httpRequest: HttpRequest[HttpServletRequest], val i18nLocale: I18nLocale,
  params: Map[String, Seq[String]], val out: ServerOutputStream) extends OutputContext {


  override def toString = iws.toString

  def authId = iws.authentication.rootAuthenticationUuid.value

  def currentIdendityUsername = iws.systemAccount.username.value

  def login(username: String) {
    iws = identityManager.login(username, this)
  }

  def logout {
    identityManager.logout(this)
  }
}

