package com.strong_links.scalaforms
import unfiltered.request.HttpRequest
import unfiltered.response.Responder
import javax.servlet.http.HttpServletRequest
import org.eclipse.jetty.servlet.ServletContextHandler
import unfiltered.response.ResponseFunction

trait IdentityManager {

  def executeInteractionRequest(
    isPost: Boolean,
    httpRequest: HttpRequest[HttpServletRequest],
    extractedUri: UriExtracter,
    params: Map[String, Seq[String]],
    createInteractionContext: (IdentityWithinServer, ServerOutputStream) => InteractionContext,
    invokeInteraction: InteractionContext => Unit): ResponseFunction[Any]

  def login(username: String, ic: InteractionContext): IdentityWithinServer

  def logout(ic: InteractionContext): Unit

  def init(ctx: ServletContextHandler) {}
}