package com.strong_links.scalaforms

import com.strong_links.core._
import com.strong_links.scalaforms._

import com.strong_links.core._
import com.strong_links.scalaforms._
import unfiltered.request._
import unfiltered.response._
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpSession
import java.io._
import org.eclipse.jetty.server.handler.ErrorHandler
import org.eclipse.jetty.server.Request
import javax.servlet.http.HttpServletResponse
import org.eclipse.jetty.http.HttpStatus
import javax.servlet.Filter
import javax.servlet.FilterConfig
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.FilterChain
import java.sql.Driver
import org.slf4j.LoggerFactory

object Unfiltered {

  // Make a base Unfiltered server, able to serve static resources.
  def makeServer(port: Int, host: String, staticResourceNodes: Seq[StaticResourceNode]) = {

    def staticResourceFilter = new Filter {

      def init(filterConfig: FilterConfig) {}

      def doFilter(request: ServletRequest, response: ServletResponse, chain: FilterChain) {

        val jettyRequest = request.asInstanceOf[org.eclipse.jetty.server.Request]

        //jettyRequest.setServletPath(" mettre le path transforme ici ...")

        chain.doFilter(request, response)
      }

      def destroy {}
    }

    def staticResourceErrorHandler = new ErrorHandler {
      override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse) {
        try
          super.handle(target, baseRequest, request, response)
        catch
          Errors.fatalCatch("Error serving static resource.")
      }
    }

    val server = unfiltered.jetty.Http(port, host)

    def addStaticResourceContext(staticResourceNode: StaticResourceNode) {
      server.context(staticResourceNode.context) { ctx =>
        ctx.resources(staticResourceNode.url)
        ctx.filter(staticResourceFilter)
        ctx.current.setErrorHandler(staticResourceErrorHandler)
      }
    }

    staticResourceNodes.foreach(addStaticResourceContext)

    server
  }
}
