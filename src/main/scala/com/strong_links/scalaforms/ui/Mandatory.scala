package com.strong_links.scalaforms.ui

import com.strong_links.scalaforms.Module
import com.strong_links.scalaforms.RoleSet
import java.lang.reflect.Method
import com.strong_links.scalaforms.GeneralPermission

trait Mandatory[ParentType] {

  private [ui] def self: ParentType
  
  private val rules = new BooleanRulesSet(false)

  def isMandatory = rules.value
  
  def mandatory = rules.add(true, self)
    
  def notMandatory = rules.add(false, self)

  def mandatoryWhen[T](formField: FormField[T], value: T) = rules.add(true, formField, value, self)
  
  def notMandatoryWhen[T](formField: FormField[T], value: T) = rules.add(false, formField, value, self)

  def mandatoryWhen(condition: => Boolean) = rules.add(true, condition, self)
  
  def notMandatoryWhen(condition: => Boolean) = rules.add(false, condition, self)

  def mandatoryWhen(roleSet: RoleSet, module: Module[_]) = rules.add(true, roleSet, module, self)

  def mandatoryWhen(roleSet: RoleSet, method: Method) = rules.add(true, roleSet, method, self)
  
  def mandatoryWhen(roleSet: RoleSet, gp: GeneralPermission) = rules.add(true, roleSet, gp, self)

  def notMandatoryWhen(roleSet: RoleSet, module: Module[_]) = rules.add(false, roleSet, module, self)

  def notMandatoryWhen(roleSet: RoleSet, method: Method) = rules.add(false, roleSet, method, self)
  
  def notMandatoryWhen(roleSet: RoleSet, gp: GeneralPermission) = rules.add(false, roleSet, gp, self)
}

