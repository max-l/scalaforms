package net.strong_links.scalaforms

import net.strong_links.core._
import net.strong_links.scalaforms._
import unfiltered.request._
import unfiltered.response._
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpSession
import java.io._
import org.eclipse.jetty.server.handler.ErrorHandler
import org.eclipse.jetty.server.Request
import javax.servlet.http.HttpServletResponse
import org.eclipse.jetty.server.HttpConnection
import org.eclipse.jetty.http.HttpStatus
import javax.servlet.Filter
import javax.servlet.FilterConfig
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.FilterChain

trait Server extends Logging {

  def activeRoles: Seq[Role]

  def adminRole: Role

  def anonymousRole: Role

  private val _adminRole = adminRole

  private val _anonymousRole = anonymousRole

  val allRoles = activeRoles ++ Seq(_adminRole) ++ Seq(_anonymousRole)

  private[scalaforms] def roleForFqn(fqn: String) =
    allRoles.find(_.fqn == fqn).getOrElse(Errors.fatal("Unknown role _" << fqn))

  private object InteractionHandler extends unfiltered.filter.Plan {
    def intent = {
      case httpRequest: HttpRequest[_] =>
        val (isPost, path0, pars) = httpRequest match {
          case GET(Path(p) & Params(params)) => (false, p, params)
          case POST(Path(p) & Params(params)) => (true, p, params)
        }

        val path = { // Jetty will encode the sessionId after ';' if it is not certain that the browser supports Cookies
          // characters at the right of ';' are for Jetty only 
          val i = path0.indexOf(';')
          if (i == -1)
            path0
          else
            path0.substring(0, i)
        }

        executeInteractionRequest(isPost, httpRequest, path, pars)
    }
  }

  private def lookupIdentity(session: HttpSession, receivedAuthIds: Option[Seq[String]]) = {

    val receivedAuthId = receivedAuthIds match {
      case None => None
      case Some(Seq(aId)) => Some(aId)
      case Some(seq) => Errors.fatal("More than one authId was received (_)." << seq.length)
      case someJunk => Errors.fatal("Invalid authId _." << someJunk)
    }

    val (needsRedirect, iws) =
      (session.isNew, receivedAuthId) match {
        case (true, None) =>
          (true, IdentityWithinServer.createAnonymous(session, this))
        case (true, Some(aId)) =>
          Errors.fatal("Invalid or expired authentication _." << aId)
        case (false, None) =>
          (true, IdentityWithinServer.createAnonymous(session, this))
        case (false, Some(aId)) =>
          (false, jettyAdapter.getIdentity(session, aId).getOrElse(Errors.fatal("Unknown authId _." << aId)))
      }

    (needsRedirect, iws)
  }

  def processUri(iws: IdentityWithinServer, session: HttpSession, u: UriExtracter,
    httpRequest: HttpRequest[HttpServletRequest], sos: ServerOutputStream) {
    try
      interactionContext.using(new InteractionContext(iws, this, u, httpRequest)) {
        userLocale.using(iws.systemAccount.preferredLocale) {
          fieldTransformer.using(identityFieldTransformer) {
            SqueryInteractionRunner.run(sos)
          }
        }
      }
    catch {
      case e: Exception => Errors.fatal(e)
    }
  }

  def executeInteractionRequest(isPost: Boolean, httpRequest: HttpRequest[HttpServletRequest], uri: String, params: Map[String, Seq[String]]) = {

    logInfo("Intercepting URI '_'." << uri)

    val u = new UriExtracter(uri)

    val session = httpRequest.underlying.getSession(true): HttpSession

    import net.strong_links.scalaforms.squeryl.SquerylFacade._
    val (needsRedirect, iws) = inTransaction {
      lookupIdentity(session, params.get("authId"))
    }

    logDebug("Identity is '_'." << iws)

    if (needsRedirect)
      Redirect("_?authId=_" << (uri, iws.authentication.rootAuthenticationUuid.value))
    else {
      new ResponseStreamer {
        def stream(os: OutputStream) {
          val sos = new ServerOutputStream(os)
          processUri(iws, session, u, httpRequest, sos)
          sos.flush
        }
      }
    }
  }

  private[scalaforms] val jettyAdapter = new JettyAdapter(this)

  def start(host: String, port: Int, staticContentDirectory: File): Unit = {

    logInfo("Starting server on port _." <<< port)
    val webRoot = staticContentDirectory.getCanonicalFile
    logInfo("Static content directory _" <<< webRoot.getAbsolutePath)

    SqueryInteractionRunner.init

    unfiltered.jetty.Http(8080, host).
      context(applicationWebroot) { c =>
        jettyAdapter.init(c.current, port, host)
        c.filter(InteractionHandler)
      }.
      context(cometWebroot)(ctx => ComedDServerImpl.createBayeuxHandlerInto(ctx.current)).
      context(anyWebroot)(ctx => {

        ctx.resources(webRoot.toURI.toURL)

        ctx.filter(new Filter {
          def init(filterConfig: FilterConfig) {}
          def doFilter(request: ServletRequest, response: ServletResponse, chain: FilterChain) {

            val jettyRequest = request.asInstanceOf[org.eclipse.jetty.server.Request]

            //jettyRequest.setServletPath(" mettre le path transforme ici ...")

            chain.doFilter(request, response)
          }
          def destroy {}
        })

        ctx.current.setErrorHandler(new ErrorHandler {
          override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse) {
            try {

              val connection = HttpConnection.getCurrentConnection
              val statusCode = connection.getResponse.getStatus
              val reason = HttpStatus.getMessage(statusCode)

              logError("ERROR serving static file : _, _ " << (statusCode, reason))

              super.handle(target, baseRequest, request, response)
            } catch {
              case e: Exception => {
                throw e
              }
            }
          }
        })
      }).
      run
  }
}
