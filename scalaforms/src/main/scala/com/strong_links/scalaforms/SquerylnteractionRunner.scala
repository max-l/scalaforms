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

object SqueryInteractionRunner extends Logging {

  def login(username: String, ic: InteractionContext) =
    authenticateWithNewIdentity(ic.iws, username, ic.server)

  def actAs(username: String, ic: InteractionContext) {
    ic.iws = authenticateWithNewIdentity(ic.iws, username, ic.server)
  }
    
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

      ic.server.jettyAdapter.setIdentity(iws.session, previousIws)

      logDebug("Authentication _ ended, replaced by '_'." << (iws, previousIws))

      ic.iws = previousIws

      val newSession = iws.session

      if (previousSession != newSession)
        Errors.fatal("Session has changed from _ to _." << (previousSession, newSession))
    }
  }  

  def createAnonymous(session: HttpSession, server: Server): IdentityWithinServer =
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
    server.jettyAdapter.setIdentity(session, iws)

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

  def load(session: HttpSession, server: Server) = {

    val authenticationsForSession = identitiesForSession(session.getId).toSeq

    val lastAuthenticationForEachRootAuthentications =
      authenticationsForSession.groupBy(_._2.rootAuthenticationUuid.value).mapValues(a => a.toSeq.maxBy(t => t._2.startTime.value)).values

    for ((u, a, sa) <- lastAuthenticationForEachRootAuthentications) yield {
      val roles = sa.roleDefinitions.map(rd => server.roleForFqn(rd.fqn.value))
      new IdentityWithinServer(session, a, sa, u, new RoleSet(roles.toSeq))
    }
  }
  
  private [scalaforms] def run(ic: InteractionContext) {


    val tx = transaction {
      val tx0 = new Transaction
      tx0.startTime :- nowTimestamp
      tx0.status :- CompletionStatusDomain.InProgress
      tx0.authenticationId :- ic.iws.authentication.id
      tx0.interactionFqn :- ic.u.fqn
      tx0.interactionArgs :- {
        //TODO:  should we add 'truncate when length exceeded' behavior in the Field/Domain ?
        val maxLength = tx0.interactionArgs.domain.maxLength.get
        val args = ic.u.rawStringArgs.mkString("\t")
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
        val interaction = ic.u.invoke(ic)
        val results = interaction.process(ic)
        update(Schema.transactions)(t =>
          where(t.id === tx.id)
          set(t.status  := CompletionStatusDomain.Success,                      
              t.endTime := Some(nowTimestamp)               
          )
        )
        results
      }
    catch {
      case e: Exception => transaction {
        val stackDump = e.getStackTraceString
        update(Schema.transactions)(t =>
          where(t.id === tx.id)
          set(t.status  := CompletionStatusDomain.Failure,                      
              t.endTime := Some(nowTimestamp),
              t.stackDump := Some(stackDump)
          )
        )
      }
      throw e
    }    
  } 
}

  
