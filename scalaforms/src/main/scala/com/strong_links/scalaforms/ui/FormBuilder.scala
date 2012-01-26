package com.strong_links.scalaforms.ui

import com.strong_links.core._
import com.strong_links.scalaforms._

trait FormBuilder {
  
  val buildMap = scala.collection.mutable.Map[BaseField[_], FormField]()
  
  implicit def upgrade(baseField: BaseField[_]) = { 
    if (buildMap.contains(baseField)) 
      buildMap(baseField)
    else {
      val ff = new FormField(baseField)
      buildMap(baseField) = ff
      ff
    }
  }

  def build: Form
}
