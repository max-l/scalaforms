package com.strong_links.scalaforms

import com.strong_links.core._
import com.strong_links.scalaforms.i18nCatalog._

import java.lang.reflect._

// All traits / classes related to security have an internal key as well as 
// a user-readable name and a user-readable description.
private[scalaforms] trait PermIdent {

  def key: String
  def name: I18n
  def description: I18n

  override def toString = "_ (_, _)" << (key, name.toString, description.toString)
}

private[scalaforms] trait Allows {

  def permissions: List[Permission]

  // Get the permissions of one type T, and eliminate duplicates within this type.
  private def extract[T <: Permission](implicit m: Manifest[T]) =
    permissions.filter(m.erasure.isInstance).map(_.asInstanceOf[T]).groupBy(_.key).map(_._2.head).toList.sortWith(_.key < _.key)

  val modulePermissions = extract[ModulePermission[_]]
  val methodPermissions = extract[MethodPermission]
  val generalPermissions = extract[GeneralPermission]

  def allows(module: Module[_]) = modulePermissions.exists(_.module eq module)

  def allows(method: Method) = methodPermissions.exists(_.sameAs(method))

  def allows(gp: GeneralPermission) = generalPermissions.exists(_.key == gp.key)

  // Get permissions, sorted according to user locale for the general permissions.
  def getPermissions(i18nLocale: I18nLocale): Seq[Permission] =
    modulePermissions ::: methodPermissions :::
      generalPermissions.map(gp => (gp, gp.name.f(i18nLocale))).sortWith(_._2 < _._2).map(_._1)
}

trait Permission extends PermIdent

case class ModulePermission[L <: IdentityTrustLevel](module: Module[L]) extends Permission {

  def key = module.getClass.getCanonicalName
  def name = i18n("Module _") << key
  def description = i18n("Execute all interaction methods in module _") <<< key

  lazy val includedMethods = Tweaks.getPublicMethodsWithReturnType(module.getClass, classOf[Interaction])
}

case class MethodPermission(method: Method) extends Permission {

  def key = Tweaks.getFullMethodName(method)
  def name = i18n("Method _") << key
  def description = i18n("Execute method _") <<< key

  private[scalaforms] def sameAs(otherMethod: Method): Boolean = {
    val p1 = method.getParameterTypes
    val p2 = otherMethod.getParameterTypes
    if (p1.length != p2.length)
      return false
    val argMismatch = (p1 zip p2).exists(t => t._1 != t._2)
    if (argMismatch)
      return false
    Tweaks.getFullMethodName(method) == Tweaks.getFullMethodName(otherMethod)
  }
}

object MethodPermission {
  def from(f: AnyRef): Permission = MethodPermission(Tweaks.getInvokedMethod(f.getClass))
}

case class GeneralPermission(name: I18n, description: I18n) extends Permission {

  def key = name.key
}

case class Role(name: I18n, description: I18n, _permissions: Permission*) extends PermIdent with Allows {

  val key = name.key

  def permissions = _permissions.toList
}

case class RoleSet(roles: Role*) extends Allows {

  def permissions = roles.toList.flatten(_.permissions)

  def includes(role: Role) = roles.exists(_.key == role.key)
}

