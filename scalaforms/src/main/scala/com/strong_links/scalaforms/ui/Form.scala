package com.strong_links.scalaforms.ui

import com.strong_links.core._
import com.strong_links.scalaforms._
import com.strong_links.scalaforms.templates.standard.forms

class Form(val formFieldSets: FormFieldSet*) extends DisplayAttributes[Form] with FieldTransformer {

  Util.findDuplicatesOption(formFieldSets).collect { case x => Errors.fatal("Duplicate form field sets _." << x) }

  val formFields = formFieldSets.flatten(_.formFields).toList

  Util.findDuplicatesOption(formFields).collect { case x => Errors.fatal("Duplicate form fields _." << x) }

  (0 /: formFields)((seq, ff) => { ff._tabOrder = seq; seq + 1 })

  val runMap = formFields.map((ff) => (ff.field, ff)).toMap[BaseField[_], FormField]

  println("Form fields: _" << formFields)

  def transform(baseField: BaseField[_]): FieldRendering = {
    if (runMap.contains(baseField))
      runMap(baseField)
    else
      baseField
  }

  def defaultRenderer(os: OutStream) {
    _label match {
      case None =>
        forms.formStart(os)
      case Some(label) =>
        forms.formStartWithLabel(label)(os)
    }
    formFieldSets.foreach(_.render(os))
    forms.formEnd(os)
  }

  override def render(os: OutStream) {
    fieldTransformer.using(this) {
      super.render(os)
    }
  }
}

object Form {

  def apply(formFieldSets: FormFieldSet*): Form = {
    new Form(formFieldSets: _*)
  }

  def apply(formFields: FormField*)(implicit d: DummyImplicit): Form = {
    new Form(FormFieldSet(formFields: _*))
  }

  def make(code: FormBuilder => Form) = {
    val f = new FormBuilder {
      def build = code(this)
    }
    f.build
  }
}