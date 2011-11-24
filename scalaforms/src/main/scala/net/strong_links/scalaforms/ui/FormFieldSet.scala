package net.strong_links.scalaforms.ui

import net.strong_links.core._
import net.strong_links.scalaforms._

class FormFieldSet(val formFields: FormField*) extends DisplayAttributes[FormFieldSet] {

  override def toString = {
    "Field set _" << _label
  }
  
  def defaultRenderer(os: OutStream) {
    _label match {
      case None =>
        standard.Forms.fieldSetStart(os)
      case Some(label) =>
        standard.Forms.fieldSetStartWithLabel(label)(os)
    }
    standard.Forms.beforeFields(os)
    formFields.foreach(_.render(os))
    standard.Forms.afterFields(os)
    standard.Forms.fieldSetEnd(os)
  }
}

object FormFieldSet {
  def apply(formFields: FormField*): FormFieldSet = {
    new FormFieldSet(formFields: _*)
  }
}

  
