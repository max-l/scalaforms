package com.strong_links.scalaforms

import com.strong_links.core._
import java.sql.{ Date => JavaDate }
import java.io.PrintWriter
import java.sql.Timestamp

trait Domain[A] {

  /**
   * Non Option[] Fields are not initialized when defined (created with ~),
   * Scala forces us to initialize them, since Field has @specialized classes
   * for Java primitives, they must be initialized with the proper value,
   * that is done in DomainInt, DomainLong, etc. All non primitive types are
   * initialized with a proper default value.
   *
   * Applicative code should not override this method, so it sealed for external packages.
   */
  protected[scalaforms] def defaultValue: A

  // Business-type attributes

  def label: I18n

  def help: Option[I18n] = None

  def bounds: (Option[A], Option[A]) = (None, None)

  def choices: Choices[A] = None

  def mandatory = false

  // belongs in Form DSL ?
  def requiresConfirmation = false

  def editable = true

  // Operators  
  def ~ = new Field[this.type, A](this, defaultValue, false)

  def ~? = new OptionalField[this.type, A](this, None)

  def ~(a: A) = new Field[this.type, A](this, a, true)

  def ~(a: Option[A]) = new OptionalField[this.type, A](this, a)

  def * : ChoiceField[this.type, A] = *(Nil)

  def *(a: Seq[A]): ChoiceField[this.type, A] = new ChoiceField[this.type, A](this, Nil)

  def decode(s: String): A

  def format(a: A) = {
    a.toString
  }

  def render(a: A): String = a.toString

  def noneValue = {
    ""
  }

  // Generate a compile-time error if the * operator is used in a context where "DoNotUseStarOperator"
  // has been put in the scope (currently in Squeryl).
  def *(implicit doNotUseStarOperator: DoNotUseStarOperator) = 0

  // System helpers
  override def toString = {
    "Domain _" <<< label
  }

  protected def error(params: LoggingParameter*) = {
    Errors.fatal(Seq(toString: LoggingParameter) ++ params.toSeq: _*)
  }
}

class DoNotUseStarOperator {
  Errors.fatal("Unexpected run-time use of the DoNotUseStarOperator class.")
}

object Casing extends Enumeration {
  type Casing = Val
  val Lower, Upper, Mixed = Value
}

trait StringDomain extends Domain[String] {

  val defaultValue = ""

  def maxLength: Option[Int] = Some(128)

  def casing = Casing.Mixed

  def permittedCharacters: Option[String] = None

  def nonPermittedCharacters: Option[String] = None

  def decode(s: String) = s
}

trait IntDomain extends Domain[Int] {

  val defaultValue = 0

  def decode(s: String) =
    Integer.parseInt(s)
}

trait BooleanDomain extends Domain[Boolean] {

  val defaultValue = false

  def decode(s: String) = s match {
    case "true" => true
    case "false" => false
    case _ => Errors.fatal("Invalid boolean _." <<< s)
  }
 
}

trait LongDomain extends Domain[Long] {

  val defaultValue = 0L

  def decode(s: String) =
    java.lang.Long.parseLong(s)
}

trait DateDomain extends Domain[JavaDate] {

  val defaultValue = new JavaDate(0)

  def decode(s: String) =
    Errors.fatal("Not implemented.")

  private val sdf = new java.text.SimpleDateFormat("yyyy.MM.dd HH:mm:ss")

  override def render(a: JavaDate): String =
    sdf.format(a)
}

trait TimestampDomain extends Domain[Timestamp] {

  val defaultValue = new Timestamp(0)

  def decode(s: String) =
    Errors.fatal("Not implemented.")

  private val sdf = new java.text.SimpleDateFormat("yyyy.MM.dd HH:mm:ss")

  override def render(a: Timestamp): String =
    sdf.format(a)
}

trait BigDecimalDomain extends Domain[BigDecimal] {

  val defaultValue = BigDecimal(0)

  def decode(s: String) =
    BigDecimal(s)
}

abstract class EnumerationDomain[E <: I18nEnumeration](val e: E) extends Domain[Enumeration#Value] {

  lazy val defaultValue: E#Value =
    e.sampleValue

  def decode(s: String) =
    e.apply(Integer.parseInt(s))
}

