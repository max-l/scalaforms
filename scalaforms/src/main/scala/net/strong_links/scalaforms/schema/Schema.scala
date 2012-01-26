package net.strong_links.scalaforms.schema

import net.strong_links.core._
import net.strong_links.scalaforms._
import net.strong_links.scalaforms.domains._
import net.strong_links.scalaforms.squeryl.SquerylFacade._
import org.squeryl.KeyedEntity
import org.squeryl.SessionFactory
import org.squeryl.Session
import org.squeryl.adapters.H2Adapter
import java.io.File
import org.squeryl.dsl.CompositeKey2
import org.squeryl.IndirectKeyedEntity

object Schema extends org.squeryl.Schema {

  val dbFilePrefix = "faw-db"

  def dbFiles = {
    val workDir = new File(".")
    workDir.listFiles.filter(_.getName.startsWith(dbFilePrefix)).toSeq
  }

  val users = table[User]

  val systemAccounts = table[SystemAccount]

  val transactions = table[Transaction]

  val authentications = table[Authentication]

  val roleDefinitions = table[RoleDefinition]

  val systemAccountToRoles =
    manyToManyRelation(systemAccounts, roleDefinitions).
      via[SystemAccountToRole]((s, r, s2r) => (s2r.roleDefinitionId === s.id, r.id === s2r.systemAccountId))

  on(transactions)(t => declare(
    t.stackDump is (dbType("text"))))

  on(roleDefinitions)(rd => declare(
    rd.id is (primaryKey)))

  def hashPassword(password: String) =
    new String(Util.md5(password.getBytes))

  private def createSyntheticAccount(username: String, accountId: Long) = {

    val u = new User
    u.firstName :- username
    u.lastName :- username
    u.creationTransactionId :- 0
    u.lastUpdateTransactionId :- 0

    users.insert(u)

    val a = new SystemAccount
    a.username :- username
    a.userId :- u.id
    a.password :- ""
    a.failedLoginAttempts :- 0
    a.creationTransactionId :- 0
    a.lastUpdateTransactionId :- 0
    a.preferredLanguageCode :- "en"

    systemAccounts.insert(a)

    // We want synthetic account to have constant ids : 
    update(systemAccounts)(sa =>
      where(sa.id === a.id)
        set (sa.id := accountId))

    a.idField :- accountId

    (u, a)
  }

  val anonymousAccountId = 0
  val serverAccountId = -1

  def populateSeedData(server: Server) {

    createSyntheticAccount("server", serverAccountId)

    val (anonymousUser, anonymousSystemAccount_) = createSyntheticAccount("anonymous", anonymousAccountId)

    initializeRoles(server.allRoles)

    val anonRoleFqn = server.anonymousRole.fqn

    val anonRoleDef = roleDefinitions.where(_.fqn === anonRoleFqn).head

    anonRoleDef.systemAccounts.associate(anonymousSystemAccount_)
  }

  def initializeRoles(rs: Traversable[Role]) {

    rs.foreach(r => {

      val hash = r.fqnStrongHash

      roleDefinitions.lookup(hash) match {
        case None =>
          val rd = new RoleDefinition
          rd.idField :- hash
          rd.fqn :- r.fqn
          rd.creationTransactionId :- 0
          rd.lastUpdateTransactionId :- 0
          roleDefinitions.insert(rd)
        case Some(rd) =>
          if (rd.fqn.value != r.fqn)
            Errors.fatal("FQN collision in RoleDefinion _." << rd.fqn)
      }
    })
  }
}

trait BaseObject extends IndirectKeyedEntity[Long, Field[KeyDomain, Long]] {

  val idField = DatabaseKeyDomain ~
  //TODO: pre initialize sample objects
  def id = idField.internalValue
  // Prevent the use of the * operator on fields, which indicate a collection of values.
  implicit def doNotUseStarOperator = new DoNotUseStarOperator
}

trait BaseFingerprintedObject extends BaseObject {
  val creationTransactionId = CreationTransactionIdDomain ~
  val lastUpdateTransactionId = LastUpdateTransactionIdDomain ~
}

class User extends BaseFingerprintedObject {
  val firstName = FirstNameDomain ~
  val lastName = LastNameDomain ~
}

class SystemAccount extends BaseFingerprintedObject {
  val userId = DatabaseKeyDomain ~
  val username = UsernameDomain ~
  val password = PasswordDomain ~
  val failedLoginAttempts = FailedLoginAttemptsDomain ~
  val preferredLanguageCode = PreferredLanguageCodeDomain ~
  lazy val preferredLocale = I18nLocale(preferredLanguageCode.value)
  lazy val roleDefinitions = Schema.systemAccountToRoles.left(this)
}

class RoleDefinition extends BaseFingerprintedObject {

  val fqn = FqnDomain ~

  lazy val systemAccounts = Schema.systemAccountToRoles.right(this)

  override def toString =
    "RoleDefinition(_,_)" << (id.toString, fqn.value)
}

class SystemAccountToRole extends KeyedEntity[CompositeKey2[Field[DatabaseKeyDomain, Long], Field[DatabaseKeyDomain, Long]]] {

  val systemAccountId = DatabaseKeyDomain ~
  val roleDefinitionId = DatabaseKeyDomain ~

  def id = compositeKey(systemAccountId, roleDefinitionId)
}

class Authentication extends BaseObject {

  val httpSessionId = HttpSessionIdDomain ~
  val rootAuthenticationUuid = UuidKeyDomain ~

  val accountId = DatabaseKeyDomain ~
  val startTime = StartTimeDomain ~
  val endTime = EndTimeDomain ~?

  def isAnonymous = id == 0

  def systemAccount = inTransaction {
    Schema.systemAccounts.where(_.id === accountId).
      headOption.getOrElse(Errors.fatal("Account not found, id = _." << accountId))
  }
}

class Transaction extends BaseObject {
  val authenticationId = DatabaseKeyDomain ~
  val interactionFqn = InteractionFqnDomain ~
  val interactionArgs = InteractionArgsDomain ~
  val startTime = StartTimeDomain ~
  val endTime = EndTimeDomain ~?
  val status = TransactionStatusDomain ~
  val stackDump = StackDumpDomain ~?
}

