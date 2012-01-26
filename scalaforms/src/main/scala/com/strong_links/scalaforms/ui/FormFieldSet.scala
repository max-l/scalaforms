package com.strong_links.scalaforms.ui

import com.strong_links.core._
import com.strong_links.scalaforms._
import com.strong_links.scalaforms.templates.standard.forms

class FormFieldSet(val formFields: FormField*) extends DisplayAttributes[FormFieldSet] {

  override def toString = {
    "Field set _" << _label
  }

  def defaultRenderer(os: OutStream) {
    _label match {
      case None =>
        forms.fieldSetStart(os)
      case Some(label) =>
        forms.fieldSetStartWithLabel(label)(os)
    }
    forms.beforeFields(os)
    formFields.foreach(_.render(os))
    forms.afterFields(os)
    forms.fieldSetEnd(os)
  }
}

object FormFieldSet {
  def apply(formFields: FormField*): FormFieldSet = {
    new FormFieldSet(formFields: _*)
  }
}

