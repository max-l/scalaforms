package com.strong_links.scalaforms

import com.strong_links.core._

abstract class BaseField[A](var internalValue: A, val domain: Domain[_]) extends FieldRendering {

  var isUDI = false

  if (domain == null)
    Errors.fatal("No domain defined.")

  override def toString = "BaseField _" << domain.label

  def isOptional: Boolean

  // When we are dealing with basic fields, the only available rendering is the one displaying the value
  // itself.
  def render(os: OutStream) {
    os.write(Convert.toHtml(internalValue.toString))
  }

  def renderLabel(os: OutStream) {
  }

  def renderControl(os: OutStream) {
  }

  def renderHelp(os: OutStream) {
  }

  def renderError(os: OutStream) {
  }
}

final class Field[+D <: Domain[A], @specialized(Int, Long, Boolean, Float, Double, Char, Byte) A] private[scalaforms] (override val domain: D, _internalValue: A, private[scalaforms] var initialized: Boolean) extends BaseField[A](_internalValue, domain) {

  def isOptional = false

  def value: A = {
    if (!initialized)
      Errors.fatal("Attempted to read an uninitialized field.")
    internalValue
  }

  def :-(a: A) {
    initialized = true
    internalValue = a
  }

  override def clone = {
    new Field(domain, internalValue, initialized)
  }

  override def toString = "Field _" << domain.label
}

final class OptionalField[+D <: Domain[A], A] private[scalaforms] (override val domain: D, _internalValue: Option[A]) extends BaseField[Option[A]](_internalValue, domain) {

  def value: Option[A] =
    internalValue

  def isOptional = true

  def :-(a1: Option[A]) {
    internalValue = a1
  }

  override def toString = "OptionalField _" << domain.label
}

final class ChoiceField[+D <: Domain[A], A] private[scalaforms] (override val domain: D, _internalValue: Seq[A]) extends BaseField[Seq[A]](_internalValue, domain) {

  def value: Seq[A] =
    internalValue

  def isOptional = true

  def :-(a1: Seq[A]) {
    internalValue = a1
  }

  override def toString = "ChoiceField with _" << domain
}

