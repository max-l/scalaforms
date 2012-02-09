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

  private object InteractionHandler extends  unfiltered.filter.Plan {

    object TrimSemicolon {
      def unapply(s: String): Option[String] = Some(s.indexOf(';') match { case -1 => s; case x => s.substring(0, x) })
    }

    def intent = {
      case httpRequest: HttpRequest[_] =>
        val (isPost, TrimSemicolon(path), params) = httpRequest match {
          case GET(Path(p) & Params(params)) => (false, p, params)
          case POST(Path(p) & Params(params)) => (true, p, params)
        }

        logInfo("Intercepting URI '_'." << path)
        
        val ux = new UriExtracter(path)
        
        _identityManager.executeInteractionRequest(isPost, httpRequest, ux, params, 
          createInteractionContext = { (iws, sos) =>
            
            try {
        
              var i18nLocale = iws.systemAccount.preferredI18nLocale
              i18nLocale = I18nStock.fr_CA
              println("Serving request with locale _." << i18nLocale)
              
              val allowed =
                iws.roleSet.allows(ux.interactions.asInstanceOf[InteractionsEnabler[_]]) ||
                  iws.roleSet.allows(ux.method)
            
              if (!allowed)
                Errors.fatal("Interaction _ not allowed for user _." << (path, iws.systemAccount.username.value))
              
              new InteractionContext(iws, _identityManager, ux, httpRequest, i18nLocale, params, sos)
            } 
            catch {
              Errors.fatalCatch("Processing URI _" << path)
            }
          }, 
          invokeInteraction = { ic =>
            val interaction = ic.u.invoke(ic)
            
            fieldTransformer.using(identityFieldTransformer) {
              interaction.process(ic) 
            }
          }
        )
    }
  }

  def start(port: Int, host: String, staticResourceNodes: Seq[StaticResourceNode]): Unit = {

    Logging.setLogger(LoggerFactory.getLogger)

    logInfo("Starting server on port: _" <<< port)
    logInfo("Static resource nodes: _" <<< staticResourceNodes)

    val server = Unfiltered.makeServer(port, host, staticResourceNodes)

    server.context(applicationWebroot) { ctx =>
      _identityManager.init(ctx.current)
      ctx.filter(InteractionHandler)
    }

    server.context(cometWebroot) { ctx =>
      ComedDServerImpl.createBayeuxHandlerInto(ctx.current)
    }

    server.run
  }
}
