package com.strong_links.scalaforms.ui

import com.strong_links.scalaforms._
import java.lang.reflect.Method

trait BooleanRule {
  def evaluate(default: Boolean): Boolean 
}

class ImmediateBooleanRule(b: Boolean) extends BooleanRule {
  def evaluate(default: Boolean) = b
}

class ModulePermissionBooleanRule(b: Boolean, roleSet: RoleSet, module: Module[_]) extends BooleanRule {
  def evaluate(default: Boolean) = if (roleSet.allows(module)) b else !b
}

class GeneralPermissionBooleanRule(b: Boolean, roleSet: RoleSet, gp: GeneralPermission) extends BooleanRule {
  def evaluate(default: Boolean) = if (roleSet.allows(gp)) b else !b
}

class MethodPermissionBooleanRule(b: Boolean, roleSet: RoleSet, method: Method) extends BooleanRule {
  def evaluate(default: Boolean) = if (roleSet.allows(method)) b else !b
}

class FormFieldBooleanRule[T](b: Boolean, formField: FormField[T], value: T) extends BooleanRule {

  def evaluate(default: Boolean) = 
    if (formField.ignoredForRulesComputation) 
      // Hidden, ignore.
      default 
    else 
      // Not hidden, use for computation.
      if (formField.baseField.internalValue == value) 
        b 
      else 
        !b
}

class GeneralBooleanRule(b: Boolean, condition: => Boolean) extends BooleanRule {

  def evaluate(default: Boolean) = if (condition) b else !b
}


