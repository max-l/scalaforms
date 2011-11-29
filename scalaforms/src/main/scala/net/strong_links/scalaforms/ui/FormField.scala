package net.strong_links.scalaforms.ui

import net.strong_links.core._
import net.strong_links.scalaforms._

object FormField {
  val fieldSpacer = capture(standard.Forms.fieldSpacer(_))  
}

class FormField(val field: BaseField[_]) extends DisplayAttributes[FormField] with FieldRendering {

  import FormField._
  
  // Get the default attributes from the associated domain and their getters.
  private [ui] var _mandatory = field.domain.mandatory   
  private [ui] var _editable = field.domain.editable
  private [ui] var _choices = field.domain.choices
  _label = Some(field.domain.label)
  
  // Other form field attributes.
  private [ui] var _solidaryFormField: Option[FormField] = None
  private [ui] var _tabOrder = -1

  // Values used while handling the form itself.
  private [ui] var errorMessage: Option[String] = None
  private [ui] var originalInput = ""
    
  // Helpers.
  private [ui] def inputName = "f" + _tabOrder
  private [ui] def inputId = inputName

  protected def error(params: LoggingParameter*) = {
    Errors.fatal(Seq(toString : LoggingParameter) ++ params.toSeq: _*)
  }

  override def toString = {
    _label match {
      case None => 
        basicToString
      case Some(label) =>
        (basicToString + " _") << label.msgid
    }
  }
  
  private def basicToString = {
    "Form field" + (if (_tabOrder == -1) "" else " " + _tabOrder.toString)
  }
  
  // Business-type functions.
  
  def editable = { 
    if (!field.domain.editable)
      error("Field is always not editable.")
    _editable = true
    this 
  }

  def notEditable = { 
    _editable = false
    this 
  }

  def editableIf(b: Boolean) = {
    if (b) editable else notEditable
  }

  def mandatory = { 
    _mandatory = false
    this 
  }

  def notMandatory = { 
    if (field.domain.mandatory)
      error("Field is always mandatory.")
    _editable = true
    this 
  }

  def mandatoryIf(b: Boolean) = {
    if (b) mandatory else notMandatory
  }
  
  def solidaryWith(formField: FormField) {
    this._solidaryFormField match {
      case Some(sf) => error("Already solidary with _." << sf)
      case _ => 
    }
    _solidaryFormField = Some(formField)
  }
  
  def notSolidary {
    this._solidaryFormField match {
      case None => error("Not solidary with a form field.")
      case _ => 
    }
    _solidaryFormField = None
  }

  protected def defaultRenderer(os: OutStream) {
    standard.Forms.field(field, fieldSpacer)(os)
  }
  
  def renderLabel(os: OutStream) {
    _label match {
      case None =>
      case Some(label) => 
        standard.Forms.fieldLabelFor(label, inputId)(os)
    }
  }
  
  def renderControl(os: OutStream) {
    os.write(Convert.toHtml("Control for form field _" << this))
  }
  
  def renderHelp(os: OutStream) {
    os.write(Convert.toHtml("Help for form field _" << this))
  }
    
  def renderError(os: OutStream) {
    os.write(Convert.toHtml("Error for form field _" << this))
  }
}

