/*package net.strong_links.scalaforms.ui

import net.strong_links.core._
import net.strong_links.scalaforms._
import net.strong_links.scalaforms.schema._

class Form(val formSections: Seq[FormSection]) {

  def this(formFields: Seq[FormField[_,_]])(implicit dp: DummyParam) = this(Seq(new FormSection(formFields)))
}

class FormSection(val formFields: Seq[FormField[_,_]]) {
}

class FormField[D <: Domain, A](field: Field[D, A]) {
  
  private def choicesMap = field.domain.staticChoicesMap  
  private var availableChoices: Option[Seq[A]] = None 

  def restrictChoicesTo(c: Seq[A]) = { 
    availableChoices = Some(c) 
    this
  }
      
//  (implicit ev: D <:< DomainEnumeration[E]) = {}
  
  
  //def restrictEnumChoice[E](c: Seq[E])(implicit ev: D <:< DomainEnumeration[E]) = {}
  
  val inputName: String = ""
  private var isReadOnly: Boolean = false
  private var isCompulsory : Boolean = false
  private var errorMessage: Option[I18n] = None
  private var receivedValueFromUser: String = ""
    
  def readOnly = { isReadOnly = true; this }
  def readWrite = { isReadOnly = false; this }
  def compulsory = { isCompulsory = true; this }
  def notCompulsory = { isCompulsory = false; this }
  
  def readOnlyWhen(b: Boolean) = { 
    if (b) {isReadOnly = true} 
    this 
  }
}

class Related[D <: Domain, A](field: Field[D,A], map: Map[A,String])
 extends FormField[D,A](field) {
  //choices = Some((a: A) => map(a)) 
}
  
class GridDef1[A1](f: A1 => Seq[FormField[_,_]], c1: Class[_]) {
  def render(i: Iterable[A1]) = 
    i.map(f(_))
}

class GridDef2[A1,A2](f: (A1,A2) => Seq[FormField[_,_]], c1: Class[_], c2: Class[_]) {
  def render(i: Iterable[(A1,A2)]) = 
    i.map(z => f(z._1, z._2))
}

class MapWithPrecursor[D <: Domain, A, T](f: Field[D,A]) {
  def mapWith(m: Map[A,String]) = new Related[D,A](f, m)
}

*/