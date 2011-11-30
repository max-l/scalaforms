package net.strong_links.scalaforms.ui

import net.strong_links.core._
import net.strong_links.scalaforms._
import net.strong_links.scalaforms.templates._

class FormFieldSet(val formFields: FormField*) extends DisplayAttributes[FormFieldSet] {

  override def toString = {
    "Field set _" << _label
  }
  
  def defaultRenderer(os: OutStream) {
    _label match {
      case None =>
        standard.forms.fieldSetStart(os)
      case Some(label) =>
        standard.forms.fieldSetStartWithLabel(label)(os)
    }
    standard.forms.beforeFields(os)
    formFields.foreach(_.render(os))
    standard.forms.afterFields(os)
    standard.forms.fieldSetEnd(os)
  }
}

object FormFieldSet {
  def apply(formFields: FormField*): FormFieldSet = {
    new FormFieldSet(formFields: _*)
  }
}

  
