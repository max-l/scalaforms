package com.strong_links.scalaforms

import com.strong_links.core._
import com.strong_links.scalaforms._

trait I18nEnumeration extends scala.Enumeration {

  class I18NValue(id: Int, val name: I18n) extends Val(id, name.key)

  protected def Value(i: Int, name: I18n): Value =
    new I18NValue(i, name)

  private[scalaforms] lazy val sampleValue =
    this.values.headOption.getOrElse(Errors.fatal("Enumeration _ has no values." << this.getClass.getName))

  protected final class DoNotUseThisSignature1

  private def unreacheable = Errors.fatal("Unreacheable code.")

  def Value(i: Int)(implicit blocker: DoNotUseThisSignature1): Value =
    unreacheable

  def Value(i: Int, s: String)(implicit blocker: DoNotUseThisSignature1): Value =
    unreacheable

  def Value(s: String)(implicit blocker: DoNotUseThisSignature1): Value =
    unreacheable

  def Value(implicit blocker: DoNotUseThisSignature1): Value =
    unreacheable
}

