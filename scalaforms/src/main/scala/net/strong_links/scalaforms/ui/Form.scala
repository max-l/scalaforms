package net.strong_links.scalaforms.ui

import net.strong_links.core._
import net.strong_links.scalaforms._
import net.strong_links.scalaforms.templates._

class Form(val formFieldSets: FormFieldSet*) extends DisplayAttributes[Form] with FieldTransformer {

  Util.checkDuplicates(formFieldSets) { ffs => Errors.fatal("_ appears twice on the form." << ffs) }
  val formFields = formFieldSets.flatten(_.formFields).toList
  Util.checkDuplicates(formFields) { ff => Errors.fatal("_ appears twice on the form." << ff) }
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
        standard.forms.formStart(os)
      case Some(label) =>
        standard.forms.formStartWithLabel(label)(os)
    }
    formFieldSets.foreach(_.render(os))
    standard.forms.formEnd(os)
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
