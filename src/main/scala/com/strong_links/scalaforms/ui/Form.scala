package com.strong_links.scalaforms.ui

import com.strong_links.core._
import com.strong_links.scalaforms._
import com.strong_links.scalaforms.i18nCatalog._
import com.strong_links.scalaforms.templates.standard.forms

trait FormElement

trait MultiFormElement extends FormElement {
  def elements: Seq[FormElement]
  private[ui] lazy val lazyElements = elements

  private[ui] def extractFormFields(formFields: scala.collection.mutable.ListBuffer[FormField[_]]) {
    for (element <- lazyElements) element match {
      case ffe: FormFieldElement[_] => 
        formFields += ffe.formField
      case mfe: MultiFormElement => 
        mfe.extractFormFields(formFields)
      case _ => 
    }
  }
}

class FormFieldElement[T](val formField: FormField[T]) extends FormElement {
  override def toString = "FormFieldElement(_)" << (formField)
}

class GroupElement(groupLabel: Option[I18n])(val formElements: FormElement*)
  extends MultiFormElement
  with Used[GroupElement]
  with Hidden[GroupElement] {
  def self = this
  def elements = formElements.toSeq
  override def toString = "GroupElement(_, _)" << (groupLabel, formElements)
}

abstract class Form[T <: AnyRef](t: T) extends Subform(t)(new FieldIdentityMap) {

  override def toString = "Form(_)" << (super.toString)

  // Ensure that each form field appears once only.
  // Assign tab order according to the form field declaration order.
  def scanFormFields = {
    val formFields = scala.collection.mutable.ListBuffer[FormField[_]]()
    extractFormFields(formFields)
    val identityMap = new java.util.IdentityHashMap[FormField[_], FormField[_]]
    val dups = scala.collection.mutable.ListBuffer[FormField[_]]()
    formFields.foreach(ff => if (identityMap.containsKey(ff)) dups += ff else identityMap.put(ff, ff))
    if (dups != Nil)
      Errors.fatal("Duplicate form fields on form: _" << dups)
    (0 /: formFields)((seq, ff) => { ff._tabOrder = seq; seq + 1 })
  }
}

abstract class Subform[T <: AnyRef](t: T)(implicit val identityMap: FieldIdentityMap)
  extends MultiFormElement {

  override def toString = "Subform(_)" << (elements.map(_.toString))

  private def transformField[T](baseField: BaseField[T]) = {
    if (!identityMap.containsKey(baseField)) {
      val x = new FormField[T](baseField)
      identityMap.put(baseField, x)
      x.spy("FormField created")
    } else {
      identityMap.get(baseField).asInstanceOf[FormField[T]].spy("FormField reused")
    }
  }

  implicit def baseFieldToFormField[T](baseField: BaseField[T]) = transformField(baseField)

  implicit def baseFieldToFormFieldElement[T](baseField: BaseField[T]) = new FormFieldElement[T](transformField[T](baseField))

  implicit def formFieldToFormFieldElement[T](formField: FormField[T]) = new FormFieldElement[T](formField)

  def group(formElements: FormElement*) = new GroupElement(None)(formElements: _*)

  def group(groupLabel: I18n)(formElements: FormElement*) = new GroupElement(Some(groupLabel))(formElements: _*)

  def elements: Seq[FormElement]
}

class xxForm(val formFieldSets: FormFieldSet*) extends DisplayAttributes[xxForm]  {

  Util.findDuplicatesOption(formFieldSets).collect { case x => Errors.fatal("Duplicate form field sets _." << x) }

  val formFields = formFieldSets.flatten(_.formFields).toList

  Util.findDuplicatesOption(formFields).collect { case x => Errors.fatal("Duplicate form fields _." << x) }

  (0 /: formFields)((seq, ff) => { ff._tabOrder = seq; seq + 1 })

  val runMap = formFields.map((ff) => (ff.baseField, ff)).toMap[BaseField[_], FormField[_]]

  println("Form fields: _" << formFields)

  def transform(baseField: BaseField[_]): FieldRendering = {
    if (runMap.contains(baseField))
      runMap(baseField)
    else
      baseField
  }

  def defaultRenderer(oc: OutputContext) {
    _label match {
      case None =>
        forms.formStart.emit(oc)
      case Some(label) =>
        forms.formStartWithLabel(label).emit(oc)
    }
    formFieldSets.foreach(_.render(oc))
    forms.formEnd.emit(oc)
  }

  override def render(oc: OutputContext) {
//    fieldTransformer.using(this) {
//      super.render(oc)
//    }
  }
}

object xxForm {

  def apply(formFieldSets: FormFieldSet*): xxForm = {
    new xxForm(formFieldSets: _*)
  }

  def apply(formFields: FormField[_]*)(implicit d: DummyImplicit): xxForm = {
    new xxForm(FormFieldSet(formFields: _*))
  }
}
