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



class InteractionContext(var iws: IdentityWithinServer, val server: Server,
  val u: UriExtracter, val httpRequest: HttpRequest[HttpServletRequest], val i18nLocale: I18nLocale, 
  params: Map[String, Seq[String]], val out: ServerOutputStream) extends Logging {

  def <<(s: String) = out.write(s)

  val allowed =
    iws.roleSet.allows(u.interactions.asInstanceOf[InteractionsEnabler[_]]) ||
      iws.roleSet.allows(u.method)

  if (!allowed)
    Errors.fatal("Interaction _ not allowed for user _." << (u.uri, iws.systemAccount.username.value))

  override def toString =
    iws.toString

  def authId = iws.authentication.rootAuthenticationUuid.value

  def currentIdendityUsername = iws.systemAccount.username.value

  def login(username: String) {
    iws = SqueryInteractionRunner.login(username, this)
  }

  def logout {
    SqueryInteractionRunner.logout(this)
  }
}

