package com.strong_links.scalaforms.ui

import com.strong_links.scalaforms._
import java.lang.reflect.Method

trait Used[ParentType] {

  private [ui] def self: ParentType
  
  private val rules = new BooleanRulesSet(true)

  def isUsed = rules.value
  
  def used = rules.add(true, self)
  
  def notUsed = rules.add(false, self)
  
  def usedWhen[T](formField: FormField[T], value: T) = rules.add(true, formField, value, self)
  
  def notUsedWhen[T](formField: FormField[T], value: T) = rules.add(false, formField, value, self)

  def usedWhen(condition: => Boolean) = rules.add(true, condition, self)
  
  def notUsedWhen(condition: => Boolean) = rules.add(false, condition, self)

  def usedWhen(roleSet: RoleSet, module: Module[_]) = rules.add(true, roleSet, module, self)

  def usedWhen(roleSet: RoleSet, method: Method) = rules.add(true, roleSet, method, self)
  
  def usedWhen(roleSet: RoleSet, gp: GeneralPermission) = rules.add(true, roleSet, gp, self)

  def notUsedWhen(roleSet: RoleSet, module: Module[_]) = rules.add(false, roleSet, module, self)

  def notUsedWhen(roleSet: RoleSet, method: Method) = rules.add(false, roleSet, method, self)
  
  def notUsedWhen(roleSet: RoleSet, gp: GeneralPermission) = rules.add(false, roleSet, gp, self)
}

