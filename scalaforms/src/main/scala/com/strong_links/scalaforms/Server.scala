package com.strong_links.scalaforms

import com.strong_links.core._
import com.strong_links.scalaforms._

import unfiltered.request._
import unfiltered.response._
import javax.servlet.http._
import java.io._
import java.sql.Driver
import org.slf4j.LoggerFactory

trait Server extends Logging {
  outer =>

  def activeRoles: Seq[Role]

  /**
   * The admin role is no different than others, except that generally permission giving
   * actions should be exclusive to this role, it is exposed in this (Server) trait
   * mainly to force applicative code to designate a role as being 'administrative'.
   */
  def adminRole: Role

  def anonymousRole: Role

  private val _adminRole = adminRole

  private val _anonymousRole = anonymousRole

  val allRoles = activeRoles ++ Seq(_adminRole) ++ Seq(_anonymousRole)

  private[scalaforms] def roleForFqn(fqn: String) =
    allRoles.find(_.fqn == fqn).getOrElse(Errors.fatal("Unknown role _" << fqn))

  protected def createIdentityManager: IdentityManager

  private lazy val _identityManager = createIdentityManager

  def identityManager = _identityManager

  private object InteractionHandler extends unfiltered.filter.Plan {

    object TrimSemicolon {
      def unapply(s: String): Option[String] = Some(s.indexOf(';') match { case -1 => s; case x => s.substring(0, x) })
    }

    def intent = {
      case httpRequest: HttpRequest[_] =>
        val (isPost, originalPath, params) = httpRequest match {
          case GET(Path(p) & Params(params))  => (false, p, params)
          case POST(Path(p) & Params(params)) => (true, p, params)
          case _                              => Errors.fatal("Unsupported request.")
        }

        val TrimSemicolon(path) = originalPath

        logInfo("Intercepting URI _." << path)
        logDebug("Raw URI _." << originalPath)

        val ux = new UriExtracter(path)

        _identityManager.executeInteractionRequest(isPost, httpRequest, ux, params,
          createInteractionContext = { (iws, sos) =>
            Errors.trap("Creating interaction context for URI _" << originalPath) {
              logDebug("will create InteractionContext for _." << iws.authId)
              val i18nLocale = I18nStock.fr_CA
              new InteractionContext(iws, _identityManager, ux, httpRequest, i18nLocale, params, sos, ux.interactionDefinition)
            }
          },
          invokeInteraction = { ic =>
            Errors.trap("Invoking interaction context for URI _, authId=_" << (originalPath, ic.authId)) {
              val interaction = ic.uriExtracter.invokeInteraction(ic)
              fieldTransformer.using(identityFieldTransformer) {
                interaction.process(ic)
              }
            }
          })
    }
  }

  def start(port: Int, host: String, staticResourceNodes: Seq[StaticResourceNode]): Unit = {

    Logging.setLogger(LoggerFactory.getLogger)

    logInfo("Starting server on port: _" <<< port)
    logInfo("Static resource nodes: _" <<< staticResourceNodes)

    // Create basic server that can serve static resources
    val server = Unfiltered.makeServer(port, host, staticResourceNodes)

    // Add the ability to serve "Interaction" requests.
    server.context(intWebroot) { ctx =>
      _identityManager.init(ctx.current)
      ctx.filter(InteractionHandler)
    }

    // Add the ability to serve "Comet" requests.
    server.context(cometWebroot) { ctx =>
      ComedDServerImpl.createBayeuxHandlerInto(ctx.current)
    }

    server.run
  }
}
