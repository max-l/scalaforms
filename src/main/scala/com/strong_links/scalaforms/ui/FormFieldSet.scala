package com.strong_links.scalaforms.ui

import com.strong_links.core._
import com.strong_links.scalaforms._
import com.strong_links.scalaforms.templates.standard.forms

class FormFieldSet(val formFields: FormField[_]*) extends DisplayAttributes[FormFieldSet] {

  override def toString = {
    "Field set _" << _label
  }

  def defaultRenderer(oc: OutputContext) {
    _label match {
      case None =>
        forms.fieldSetStart.emit(oc)
      case Some(label) =>
        forms.fieldSetStartWithLabel(label).emit(oc)
    }
    forms.beforeFields.emit(oc)
    formFields.foreach(_.render(oc))
    forms.afterFields.emit(oc)
    forms.fieldSetEnd.emit(oc)
  }
}

object FormFieldSet {
  def apply(formFields: FormField[_]*): FormFieldSet = {
    new FormFieldSet(formFields: _*)
  }
}

