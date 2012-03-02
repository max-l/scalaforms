
package com.strong_links.scalaforms.domains

import com.strong_links.core._
import com.strong_links.scalaforms
import com.strong_links.scalaforms._
import com.strong_links.scalaforms.i18nCatalog._

abstract class KeyDomain extends LongDomain

object KeyDomain extends KeyDomain {
  val label = i18n("id")
}

class NameDomain extends StringDomain {
  val label = i18n("Name")
}

object UsernameDomain extends NameDomain {
  override val label = i18n("Username")
}

object FirstNameDomain extends NameDomain {
  override val label = i18n("First name")
}

object LastNameDomain extends NameDomain {
  override val label = i18n("Last name")
}

object NickNameDomain extends NameDomain {
  override val label = i18n("Nick name")
}

object TitleDomain extends NameDomain {
  override val label = i18n("Title")
}

class UrlDomain extends StringDomain {
  override val label = i18n("url")
}

object FacebookIdDomain extends LongDomain {
  override val label = i18n("Facebook Id")
}

object EmailAddressDomain extends StringDomain {
  override val label = i18n("email")
}

object ClearTextPasswordDomain extends StringDomain {
  val label = i18n("Password")
}

object PasswordHashDomain extends StringDomain {
  val label = i18n("Password hash")
}

object PasswordSaltDomain extends StringDomain {
  val label = i18n("Salt")
}

object StackDumpDomain extends StringDomain {
  val label = i18n("Stack dump")
}

object LastUpdateTransactionIdDomain extends KeyDomain {
  val label = i18n("Last update transaction id")
}

object CreationTransactionIdDomain extends KeyDomain {
  val label = i18n("Creation transaction id")
}

object CompletionStatusDomain extends I18nEnumeration {
  val InProgress = Value(1, i18n("In Progress"))
  val Success = Value(2, i18n("Success"))
  val Failure = Value(3, i18n("Failure"))
  val Timeout = Value(4, i18n("Timeout"))
}

trait DatabaseKeyDomain extends KeyDomain {
  val label = i18n("Database key")
}

object DatabaseKeyDomain extends DatabaseKeyDomain

object StartTimeDomain extends TimestampDomain {
  val label = i18n("Start date/time")
}

object EndTimeDomain extends TimestampDomain {
  val label = i18n("End date/time")
}

object TransactionStatusDomain extends EnumerationDomain(CompletionStatusDomain) {
  val label = i18n("Status")
}

object FailedLoginAttemptsDomain extends IntDomain {
  val label = i18n("Failed login attempts")
}

object PreferredLanguageCodeDomain extends StringDomain {
  val label = i18n("Preferred language")
}

abstract class MarkdownTextDomain extends StringDomain

object HttpSessionIdDomain extends StringDomain {
  val label = i18n("Http session id")
}

object UuidKeyDomain extends StringDomain {
  val label = i18n("uuid")
}

object FqnDomain extends StringDomain {
  val label = i18n("Fully qualified name")
}

object InteractionFqnDomain extends StringDomain {
  val label = i18n("Interaction fully qualified name")
}

object InteractionArgsDomain extends StringDomain {
  val label = i18n("Interaction arguments")
  override def maxLength = Some(1024)
}
