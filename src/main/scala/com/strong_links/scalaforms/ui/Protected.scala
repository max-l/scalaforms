package com.strong_links.scalaforms.ui

import com.strong_links.scalaforms._
import java.lang.reflect.Method

trait ReadOnly[ParentType] {

  private [ui] def self: ParentType
  
  private val rules = new BooleanRulesSet(true)

  def isReadOnly = rules.value
  
  def readOnly = rules.add(true, self)
  
  def notReadOnly = rules.add(false, self)
  
  def readOnlyWhen[T](formField: FormField[T], value: T) = rules.add(true, formField, value, self)
  
  def notReadOnlyWhen[T](formField: FormField[T], value: T) = rules.add(false, formField, value, self)

  def readOnlyWhen(condition: => Boolean) = rules.add(true, condition, self)
  
  def notReadOnlyWhen(condition: => Boolean) = rules.add(false, condition, self)

  def readOnlyWhen(roleSet: RoleSet, module: Module[_]) = rules.add(true, roleSet, module, self)

  def readOnlyWhen(roleSet: RoleSet, method: Method) = rules.add(true, roleSet, method, self)
  
  def readOnlyWhen(roleSet: RoleSet, gp: GeneralPermission) = rules.add(true, roleSet, gp, self)

  def notReadOnlyWhen(roleSet: RoleSet, module: Module[_]) = rules.add(false, roleSet, module, self)

  def notReadOnlyWhen(roleSet: RoleSet, method: Method) = rules.add(false, roleSet, method, self)
  
  def notReadOnlyWhen(roleSet: RoleSet, gp: GeneralPermission) = rules.add(false, roleSet, gp, self)
}

