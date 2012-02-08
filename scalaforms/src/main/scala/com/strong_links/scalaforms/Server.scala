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

  def activeRoles: Seq[Role]

  def adminRole: Role

  def anonymousRole: Role

  private val _adminRole = adminRole

  private val _anonymousRole = anonymousRole

  val allRoles = activeRoles ++ Seq(_adminRole) ++ Seq(_anonymousRole)

  private[scalaforms] def roleForFqn(fqn: String) =
    allRoles.find(_.fqn == fqn).getOrElse(Errors.fatal("Unknown role _" << fqn))

  private object InteractionHandler extends unfiltered.filter.Plan {

    object TrimSemicolon {
      def unapply(s: String): Option[String] = Some(s.indexOf(';') match { case -1 => s; case x => s.substring(0, x) })
    }

    def intent = {
      case httpRequest: HttpRequest[_] =>
        val (isPost, TrimSemicolon(path), params) = httpRequest match {
          case GET(Path(p) & Params(params)) => (false, p, params)
          case POST(Path(p) & Params(params)) => (true, p, params)
        }

        executeInteractionRequest(isPost, httpRequest, path, params)
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
    httpRequest: HttpRequest[HttpServletRequest], sos: ServerOutputStream, params: Map[String, Seq[String]]) {
    try {

      var i18nLocale = iws.systemAccount.preferredI18nLocale
      i18nLocale = I18nStock.fr_CA
      println("Serving request with locale _." << i18nLocale)
      val ctx = new InteractionContext(iws, this, u, httpRequest, i18nLocale, params, sos)

      fieldTransformer.using(identityFieldTransformer) {
        SqueryInteractionRunner.run(ctx)
      }
    } catch
      Errors.fatalCatch("Processing URI _" << u.uri)
  }

  def executeInteractionRequest(isPost: Boolean, httpRequest: HttpRequest[HttpServletRequest], uri: String, params: Map[String, Seq[String]]) = {

    logInfo("Intercepting URI '_'." << uri)

    val u = new UriExtracter(uri)

    val session = httpRequest.underlying.getSession(true): HttpSession

    import com.strong_links.scalaforms.squeryl.SquerylFacade._
    val (needsRedirect, iws) = inTransaction {
      lookupIdentity(session, params.get("authId"))
    }

    logDebug("Identity is '_'." << iws)

    if (needsRedirect)
      Redirect("_?authId=_" << (uri, iws.authentication.rootAuthenticationUuid.value))
    else {
      new ResponseStreamer {
        def stream(os: OutputStream) {
          val out = new ServerOutputStream(os)
          processUri(iws, session, u, httpRequest, out, params)
          out.flush
        }
      }
    }
  }

  private[scalaforms] val jettyAdapter = new JettyAdapter(this)

  def start(port: Int, host: String, staticResourceNodes: Seq[StaticResourceNode], jdbcDriver: Driver, jdbcUrlWithUsernameAndPassword: String): Unit = {

    Logging.setLogger(LoggerFactory.getLogger)

    logInfo("Starting server on port: _" <<< port)
    logInfo("Static resource nodes: _" <<< staticResourceNodes)

    val server = Unfiltered.makeServer(jettyAdapter, port, host, staticResourceNodes, jdbcDriver, jdbcUrlWithUsernameAndPassword)

    server.context(applicationWebroot) { ctx =>
      jettyAdapter.init(ctx.current, port, host, jdbcDriver, jdbcUrlWithUsernameAndPassword)
      ctx.filter(InteractionHandler)
    }

    server.context(cometWebroot) { ctx =>
      ComedDServerImpl.createBayeuxHandlerInto(ctx.current)
    }

    server.run
  }
}
