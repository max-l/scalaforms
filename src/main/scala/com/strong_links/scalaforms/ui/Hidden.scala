package com.strong_links.scalaforms.ui

trait Hidden[ParentType] {

  private [ui] def self: ParentType
  
  private val rules = new BooleanRulesSet(false)

  def isHidden = rules.value
  
  def hidden = rules.add(true, self)
  
  def notHidden = rules.add(false, self)

  def hiddenWhen[T](formField: FormField[T], value: T) = rules.add(true, formField, value, self)
  
  def notHiddenWhen[T](formField: FormField[T], value: T) = rules.add(false, formField, value, self)
}

