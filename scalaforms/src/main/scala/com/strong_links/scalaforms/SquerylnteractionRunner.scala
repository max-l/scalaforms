package com.strong_links.scalaforms

import com.strong_links.core._
import com.strong_links.scalaforms._
import com.strong_links.scalaforms.schema._
import com.strong_links.scalaforms.domains._
import com.strong_links.scalaforms.squeryl.SquerylFacade._
import org.squeryl.SessionFactory
import org.squeryl.Session
import org.squeryl.adapters.H2Adapter
import javax.servlet.http.HttpSession
import unfiltered.request.HttpRequest
import javax.servlet.http.HttpServletRequest
import java.io.OutputStream
import unfiltered.response._
import org.eclipse.jetty.server.session.SessionHandler
import org.eclipse.jetty.server.session.JDBCSessionIdManager
import org.eclipse.jetty.server.session.JDBCSessionManager
import scala.collection.mutable.HashMap
import org.eclipse.jetty.servlet.ServletContextHandler

class SqueryInteractionRunner(port: Int, host: String, jdbcDriver: java.sql.Driver, jdbcUrl: String, server: Server)
  extends IdentityManager with Logging {

  override def init(ctx: ServletContextHandler) {

    val jism = new JDBCSessionIdManager(ctx.getServer)

    jism.setWorkerName(host + "_" + port) // this is the cluster node name (must be unique within cluster) 

    jism.setDriverInfo(jdbcDriver, jdbcUrl)
    jism.setScavengeInterval(60)
    ctx.getServer.setSessionIdManager(jism)

    val jsm = new JDBCSessionManager {

      override def updateSession(data: org.eclipse.jetty.server.session.JDBCSessionManager#SessionData) = {

        val sid = data.getId
        val session = this.getSession(sid)
        val aMap = getSessionAttributeMap(session)
        // avoid saving the SessionAttributeMap as a blob in the DB
        try {
          logDebug("will update session _" << data)
          // set it to null before save
          session.setAttribute(AUTHENTICATION_ATTRIBUTE_KEY, null)
          super.updateSession(data)
        } finally {
          // restore it after
          session.setAttribute(AUTHENTICATION_ATTRIBUTE_KEY, aMap)
        }
      }

      override def getSession(idInCluster: String): JDBCSessionManager#Session = {

        val s = super.getSession(idInCluster)
        if (s == null)
          return null

        if (sessionAuthenticationsLoaded(s))
          return s

        try {
          import com.strong_links.scalaforms.squeryl.SquerylFacade._
          val iwss = transaction {
            load(s, server)
          }

          for (iws <- iwss) {
            logDebug("Identity _ loaded from database into http session." << iws)
            getSessionAttributeMap(s).put(iws.authentication.rootAuthenticationUuid.value, iws)
          }

          s
        } catch
          Errors.fatalCatch("Error getting IdentityWithinServer.")
      }
    }
    jsm.setSessionIdManager(jism)
    jsm.setMaxInactiveInterval(60 * 5)
    val sh = new SessionHandler(jsm)
    ctx.setSessionHandler(sh);
  }

  private def lookupIdentity(httpRequest: HttpRequest[HttpServletRequest], receivedAuthIds: Option[Seq[String]], server: Server) = {

    val session = httpRequest.underlying.getSession(true): HttpSession

    logDebug("Session id: _" << session.getId)

    val receivedAuthId = receivedAuthIds match {
      case None => None
      case Some(Seq(aId)) => Some(aId)
      case Some(seq) => Errors.fatal("More than one authId was received (_)." << seq.length)
      case someJunk => Errors.fatal("Invalid authId _." << someJunk)
    }

    val (needsRedirect, iws) =
      (session.isNew, receivedAuthId) match {
        case (true, None) =>
          (true, createAnonymous(session, server))
        case (true, Some(aId)) =>
          Errors.fatal("Invalid or expired authentication _ (a new session just started)." << aId)
        case (false, None) =>
          (true, createAnonymous(session, server))
        case (false, Some(aId)) =>
          (false, recoverExistingIdentityWithinServer(session, aId, server))
      }

    (needsRedirect, iws)
  }

  private def recoverExistingIdentityWithinServer(session: HttpSession, authenticationId: String, server: Server) =
    getIdentity(session, authenticationId).getOrElse(Errors.fatal("Unknown authId _." << authenticationId))

  def executeInteractionRequest(
    isPost: Boolean,
    httpRequest: HttpRequest[HttpServletRequest],
    extractedUri: UriExtracter,
    params: Map[String, Seq[String]],
    server: Server,
    createInteractionContext: (IdentityWithinServer, ServerOutputStream) => InteractionContext,
    invokeInteraction: InteractionContext => Unit): ResponseFunction[Any] = {

    val (needsRedirect, iws) = inTransaction {
      lookupIdentity(httpRequest, params.get("authId"), server)
    }

    logDebug("Identity is '_'." << iws)

    if (needsRedirect)
      Redirect("_?authId=_" << (extractedUri.uri, iws.authentication.rootAuthenticationUuid.value))
    else {

      val t = new ResponseStreamer {
        def stream(os: OutputStream) {
          val out = new ServerOutputStream(os)
          val ic = createInteractionContext(iws, out)
          run(ic, invokeInteraction)
          out.flush
          os.close
        }
      }

      if (extractedUri.interactionDefinition.isJson)
        JsonContent ~> t
      else
        t
    }
  }

  /**
   * This will behave as an 'act as' is already logged in
   */
  def login(username: String, ic: InteractionContext) =
    authenticateWithNewIdentity(ic.iws, username, ic.server)

  def logout(ic: InteractionContext) {

    val iws = ic.iws

    update(Schema.authentications)(a =>
      where(a.id === iws.authentication.id)
        set (a.endTime := Some(nowTimestamp)))

    if (iws.authentication.isAnonymous) {
      iws.session.invalidate()
    } else {

      val authId = iws.authentication.rootAuthenticationUuid.value

      val rootAuth = authId

      val previousSession = iws.session

      val previousIws =
        lastActiveIdentityFor(iws.session, rootAuth, ic.server).
          getOrElse(Errors.fatal("Found no previous authentication for rootAuthenticationUuid = _, while logging out of authentication.id=_.) "
            << (rootAuth, iws.authentication.id)))

      setIdentity(iws.session, previousIws)

      logDebug("Authentication _ ended, replaced by '_'." << (iws, previousIws))

      ic.iws = previousIws

      val newSession = iws.session

      if (previousSession != newSession)
        Errors.fatal("Session has changed from _ to _." << (previousSession, newSession))
    }
  }

  private def createAnonymous(session: HttpSession, server: Server): IdentityWithinServer =
    create(session, Schema.anonymousAccountId, Util.newGuid, server)

  private def authenticateWithNewIdentity(currentIdentity: IdentityWithinServer, username: String, server: Server): IdentityWithinServer = {

    val sa = Schema.systemAccounts.where(_.username === username).
      headOption.getOrElse(Errors.fatal("Account not found, username=_." << username))

    create(currentIdentity.session, sa, currentIdentity.authentication.rootAuthenticationUuid.value, server)
  }

  private def create(session: HttpSession, systemAccountId: Long, rootAuthenticationId: String, server: Server): IdentityWithinServer = {

    val sa = Schema.systemAccounts.where(_.id === systemAccountId).
      headOption.getOrElse(Errors.fatal("Account not found, id=_." << systemAccountId))

    create(session, sa, rootAuthenticationId, server)
  }

  private def loadRoles(sa: SystemAccount, server: Server) = {

    val roleFqnsOfAccount = sa.roleDefinitions.map(_.fqn.value).toSeq
    val serverRoles = server.allRoles
    val roles = serverRoles.filter(sr => roleFqnsOfAccount.exists(_ == sr.fqn))
    roles
  }

  private def create(session: HttpSession, sa: SystemAccount, rootAuthenticationId: String, server: Server): IdentityWithinServer = inTransaction {

    val a = new Authentication
    a.httpSessionId :- session.getId
    a.rootAuthenticationUuid :- rootAuthenticationId
    a.accountId :- sa.id
    a.startTime :- new java.sql.Timestamp(System.currentTimeMillis)

    val roles = loadRoles(sa, server)

    Schema.authentications.insert(a)

    val iws = new IdentityWithinServer(session, a, sa, Schema.users.get(sa.userId.value), new RoleSet(roles))
    setIdentity(session, iws)

    logDebug("New authentication : _." << iws)

    iws
  }

  private def lastActiveIdentityFor(session: HttpSession, rootAuthId: String, server: Server) = {
    val z =
      from(identitiesForSession(session.getId))(z =>
        where(z._2.rootAuthenticationUuid === rootAuthId and z._2.endTime.isNull)
          select (z)
          orderBy (z._2.startTime desc))

    val iws =
      for ((u, a, sa) <- z) yield {
        val roles = loadRoles(sa, server)
        new IdentityWithinServer(session, a, sa, u, new RoleSet(roles))
      }

    iws.headOption
  }

  private def identitiesForSession(sessionId: String) = {
    import Schema._

    from(users, authentications, systemAccounts)((u, a, sa) =>
      where(u.id === sa.userId and sa.id === a.accountId and a.httpSessionId === sessionId)
        select ((u, a, sa)))
  }

  private def load(session: HttpSession, server: Server) = {

    val authenticationsForSession = identitiesForSession(session.getId).toSeq

    val lastAuthenticationForEachRootAuthentications =
      authenticationsForSession.groupBy(_._2.rootAuthenticationUuid.value).mapValues(a => a.toSeq.maxBy(t => t._2.startTime.value)).values

    for ((u, a, sa) <- lastAuthenticationForEachRootAuthentications) yield {
      val roles = sa.roleDefinitions.map(rd => server.roleForFqn(rd.fqn.value))
      new IdentityWithinServer(session, a, sa, u, new RoleSet(roles.toSeq))
    }
  }

  private def run(ic: InteractionContext, invokeInteraction: InteractionContext => Unit) {

    val tx = transaction {
      val tx0 = new Transaction
      tx0.startTime :- nowTimestamp
      tx0.status :- CompletionStatusDomain.InProgress
      tx0.authenticationId :- ic.iws.authentication.id
      tx0.interactionFqn :- ic.uriExtracter.fqn
      tx0.interactionArgs :- {
        //TODO:  should we add 'truncate when length exceeded' behavior in the Field/Domain ?
        val maxLength = tx0.interactionArgs.domain.maxLength.get
        val args = ic.uriExtracter.rawStringArgs.mkString("\t")
        if (args.length > maxLength)
          args.substring(0, maxLength)
        else
          args
      }

      Schema.transactions.insert(tx0)
      tx0
    }

    try
      transaction {

        invokeInteraction(ic)

        update(Schema.transactions)(t =>
          where(t.id === tx.id)
            set (t.status := CompletionStatusDomain.Success,
              t.endTime := Some(nowTimestamp)))
      }
    catch {
      case e: Exception =>
        transaction {
          val stackDump = e.getStackTraceString
          update(Schema.transactions)(t =>
            where(t.id === tx.id)
              set (t.status := CompletionStatusDomain.Failure,
                t.endTime := Some(nowTimestamp),
                t.stackDump := Some(stackDump)))
        }
        throw e
    }
  }

  private val AUTHENTICATION_ATTRIBUTE_KEY = "ROOT_AUTHENTICATION_KEY"

  private def sessionAuthenticationsLoaded(s: HttpSession) =
    s.getAttribute(AUTHENTICATION_ATTRIBUTE_KEY) != null

  private def setIdentity(s: HttpSession, iws: IdentityWithinServer) {

    getSessionAttributeMap(s).put(iws.authentication.rootAuthenticationUuid.value, iws)
  }

  private def getIdentity(s: HttpSession, rootAuthenticationId: String) =
    getSessionAttributeMap(s).get(rootAuthenticationId)

  private def getSessionAttributeMap(s: HttpSession): HashMap[String, IdentityWithinServer] = {

    val o = s.getAttribute(AUTHENTICATION_ATTRIBUTE_KEY)
    if (o != null)
      o.asInstanceOf[HashMap[String, IdentityWithinServer]]
    else {
      val tmp = new HashMap[String, IdentityWithinServer]
      s.setAttribute(AUTHENTICATION_ATTRIBUTE_KEY, tmp)

      tmp
    }
  }

}

