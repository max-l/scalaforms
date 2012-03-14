package com.strong_links.scalaforms.ui

import com.strong_links.core._

import com.strong_links.scalaforms._
import com.strong_links.scalaforms.i18nCatalog._
import com.strong_links.scalaforms.templates.standard.forms

class FormField[T](val baseField: BaseField[T]) extends DisplayAttributes[FormField[T]]
  with Used[FormField[T]]
  with Hidden[FormField[T]]
  with Mandatory[FormField[T]]
  with ReadOnly[FormField[T]]
  with FieldRendering {

  private[ui] def self = this

  private[ui] def ignoredForRulesComputation = !isUsed || isHidden

  // Get the default attributes from the associated domain and their getters.
  private[ui] var _choices = baseField.domain.choices
  _label = Some(baseField.domain.label)

  // Other form field attributes.
  private[ui] var _tabOrder = -1

  // Values used while handling the form itself.
  private[ui] var errorMessage: Option[String] = None
  private[ui] var originalInput = ""

  // Helpers.
  private[ui] def inputName = "f" + _tabOrder
  private[ui] def inputId = inputName

  protected def error(params: LoggingParameter*) = {
    Errors.fatal(Seq(toString: LoggingParameter) ++ params.toSeq: _*)
  }

  override def toString = (_label, if (_tabOrder == -1) "" else (_tabOrder + " ")) match {
    case (None, bts) =>
      bts
    case (Some(label), bts) if bts.length > 0 =>
      bts + " " + label
    case (Some(label), bts) =>
      bts + label
  }

  protected def defaultRenderer(oc: OutputContext) {
    //    forms.field1(field)(oc)
    //    forms.field2(oc)
    //    forms.field3(field)(oc)
  }

  def renderLabel(oc: OutputContext) {
    _label match {
      case None =>
      //case Some(label) => forms.fieldLabelFor(label, inputId)(oc)
    }
  }

  def renderControl(oc: OutputContext) {
    oc.out.write(Convert.toHtml("Control for form field _" << this))
  }

  def renderHelp(oc: OutputContext) {
    oc.out.write(Convert.toHtml("Help for form field _" << this))
  }

  def renderError(oc: OutputContext) {
    oc.out.write(Convert.toHtml("Error for form field _" << this))
  }
}

