package com.strong_links.scalaforms.ui

import com.strong_links.core._
import com.strong_links.scalaforms._
import com.strong_links.scalaforms.templates.standard.forms

class FormFieldSet(val formFields: FormField*) extends DisplayAttributes[FormFieldSet] {

  override def toString = {
    "Field set _" << _label
  }

  def defaultRenderer(oc: OutputContext) {
    _label match {
      case None =>
        forms.fieldSetStart(oc)
      case Some(label) =>
        forms.fieldSetStartWithLabel(label)(oc)
    }
    forms.beforeFields(oc)
    formFields.foreach(_.render(oc))
    forms.afterFields(oc)
    forms.fieldSetEnd(oc)
  }
}

object FormFieldSet {
  def apply(formFields: FormField*): FormFieldSet = {
    new FormFieldSet(formFields: _*)
  }
}

