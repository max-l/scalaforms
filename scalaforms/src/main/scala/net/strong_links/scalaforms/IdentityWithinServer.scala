package net.strong_links.scalaforms

import net.strong_links.core._
import net.strong_links.scalaforms.domains._
import net.strong_links.scalaforms.schema._

import net.strong_links.scalaforms.squeryl.SquerylDslSupport._
import javax.servlet.http.HttpSession

private[scalaforms] class IdentityWithinServer(val session: HttpSession, val authentication: Authentication, val systemAccount: SystemAccount, val user: User, val roleSet: RoleSet) {

  override def toString = {
    "Identity(username=_,rootAuthId=_,roles=_)" <<
      (systemAccount.username.value, authentication.rootAuthenticationUuid.value, roleSet.roles)
  }
}

object IdentityWithinServer extends Logging {

  def createAnonymous(session: HttpSession, server: Server): IdentityWithinServer =
    create(session, Schema.anonymousAccountId, Util.newGuid, server)

  def authenticateWithNewIdentity(currentIdentity: IdentityWithinServer, username: String, server: Server): IdentityWithinServer = {

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

  def lastActiveIdentityFor(session: HttpSession, rootAuthId: String, server: Server) = {
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
}
