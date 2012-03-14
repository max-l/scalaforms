package com.strong_links.scalaforms.ui
import java.lang.reflect.Method
import com.strong_links.scalaforms.RoleSet
import com.strong_links.scalaforms.Module
import com.strong_links.scalaforms.GeneralPermission

class BooleanRulesSet(initialValue: Boolean) {

  private [ui] var value = initialValue
    
  private val rules = scala.collection.mutable.ListBuffer[BooleanRule]()
  
  private [ui] def add[P](b: Boolean, parent: P): P = {
      val rule = new ImmediateBooleanRule(b)
      rules += rule
      parent
  }

  private [ui] def add[T, P](b: Boolean, formField: FormField[T], value: T, parent: P): P = {
      val rule = new FormFieldBooleanRule(b, formField, value)
      rules += rule
      parent
  }

  private [ui] def add[T, P](b: Boolean, condition: => Boolean, parent: P): P = {
      val rule = new GeneralBooleanRule(b, condition)
      rules += rule
      parent
  }

  private [ui] def add[P](b: Boolean, roleSet: RoleSet, module: Module[_], parent: P): P = {
      val rule = new ModulePermissionBooleanRule(b, roleSet, module)
      rules += rule
      parent
  }

  private [ui] def add[P](b: Boolean, roleSet: RoleSet, method: Method, parent: P): P = {
      val rule = new MethodPermissionBooleanRule(b, roleSet, method)
      rules += rule
      parent
  }

  private [ui] def add[P](b: Boolean, roleSet: RoleSet, gp: GeneralPermission, parent: P): P = {
      val rule = new GeneralPermissionBooleanRule(b, roleSet, gp)
      rules += rule
      parent
  }
  

  private [ui] def compute = rules.foreach(r => value = r.evaluate(value))
}

