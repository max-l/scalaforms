//TODO: Test pour vérifier qu'un nom de permission générale ne changera pas avec une
//      nouvelle version du compilateur.
//TODO: Permission sur une méthode, quand la méthode a plus d'une signature.

package net.strong_links.scalaforms

import net.strong_links.core._

import java.lang.reflect._

trait dumpable {

  def dump(cs: LeveledCharStream)
}

trait Permission {
  
  def description: String
  
  override def toString = description
}

private [scalaforms] class ClassPermission(val interactionsEnabler: InteractionsEnabler[_]) extends Permission {

  val name = interactionsEnabler.clasz.getCanonicalName
  lazy val includedMethods = Tweaks.getPublicMethodsWithReturnType(interactionsEnabler.clasz, classOf[Interaction])

  def description = "Execute all interaction methods in _" <<< name 
}

object ClassPermissions extends dumpable {
  
  private [scalaforms] val m = new IdentityMap[InteractionsEnabler[_], ClassPermission] 
  
  def dump(cs: LeveledCharStream) {
    cs.levelPrint("Class permissions loaded")
    cs.increaseLevel
    m.getValues.sortWith(_.name < _.name).foreach(c => {
      cs.levelPrint(c.name)
      cs.increaseLevel
      c.includedMethods.sortWith(_.getName < _.getName).foreach(m => cs.levelPrint(m.getName))
      cs.decreaseLevel
    })
    cs.decreaseLevel
  }
}

private [scalaforms] class MethodPermission(val method: Method) extends Permission {
  
  val name = Tweaks.getFullMethodName(method)

  def description = "Execute method _" <<< name
}

object MethodPermissions extends dumpable {
  
  private [scalaforms] val m = new IdentityMap[Method, MethodPermission] 
  
  def dump(cs: LeveledCharStream) {
    cs.levelPrint("Method permissions loaded")
    cs.increaseLevel
    m.getValues.sortWith(_.name < _.name).foreach(mp => cs.levelPrint(mp.name))
    cs.decreaseLevel
  }
}

trait FullyQualifiedName {
  val fqn = {
    def keep(s: String) = !s.isEmpty && (try {s.toInt; false} catch {case _ => true})
    getClass.getName.replace('$', '.').split('.').filter(keep).mkString(".") 
  }
}

trait GeneralPermission extends Permission with FullyQualifiedName {

  val name = fqn
  val description: String = "General permission _" <<< name

  GeneralPermissions.list.append(this)
}

object GeneralPermissions extends dumpable {
    
  private [scalaforms] val list = scala.collection.mutable.ListBuffer[GeneralPermission]() 

  def dump(cs: LeveledCharStream) {
    cs.levelPrint("General permissions loaded")
    cs.increaseLevel
    list.sortWith(_.description < _.description).foreach(c => cs.levelPrint(c.description))
    cs.decreaseLevel
  }
}

object Permission {
  
  private [scalaforms] def makeClassPermission(c: InteractionsEnabler[_]): Permission = {
    ClassPermissions.m.put(c) { c => new ClassPermission(c) } 
  }

  private [scalaforms] def makeMethodPermission(f: AnyRef): Permission = {
    MethodPermissions.m.put(Tweaks.getInvokedMethod(f.getClass)) { new MethodPermission(_) }
  }
}

trait Role extends dumpable with FullyQualifiedName {
  def name: I18n
  def permissions: Seq[Permission]
  lazy val (classPermissions, methodPermissions, generalPermissions) = {
    val m = new UniqueIdentityMap[Permission]
    permissions.foreach(m.put)
    val uniquePermissions = m.getKeys
    (Util.filterOn[ClassPermission](uniquePermissions),
     Util.filterOn[MethodPermission](uniquePermissions),
     Util.filterOn[GeneralPermission](uniquePermissions))
  }
  
  def allows(ie: InteractionsEnabler[_]) =
    classPermissions.exists(cp => {
      cp.interactionsEnabler eq ie
    })  
  
  def allows(method: Method) = 
    methodPermissions.exists(mp => methodEq(mp.method, method))
  

  private def methodEq(m1: Method, m2: Method): Boolean = {
    
    val p1 = m1.getParameterTypes
    val p2 = m2.getParameterTypes
    
    if (p1.length != p2.length)
      return false
      
    // verify that arg types match :
    val z = (p1 zip p2).exists(t => t._1 != t._2)
      
    if (z)
      return false
    
    Tweaks.getFullMethodName(m1) == Tweaks.getFullMethodName(m2)
  }
  
  def allows(gp: GeneralPermission) = {
    generalPermissions.contains(gp)
  }

  def dump(cs: LeveledCharStream) {
    cs.levelPrint("Permissions for role _" <<< name.msgid)
    def dumpList(list: List[Permission]) {
      list.sortWith(_.description < _.description).foreach(p => cs.levelPrint(p.description))      
    }
    cs.increaseLevel
    dumpList(classPermissions)
    dumpList(methodPermissions)
    dumpList(generalPermissions)
    cs.decreaseLevel
  }
  
  override def toString = fqn
  override def hashCode = fqn.hashCode
  override def equals(o: Any) = 
    o match {
      case r:Role => r.fqn == fqn
      case _ => false
    }  
}

class RoleSet(val roles: Seq[Role])  extends dumpable {

  private [scalaforms] def allows(ie: InteractionsEnabler[_]) = roles.exists(_.allows(ie))
  
  private [scalaforms] def allows(method: Method) = roles.exists(_.allows(method))
  
  def allows(gp: GeneralPermission) = roles.exists(_.allows(gp))  
  
  def dump(cs: LeveledCharStream) {
    cs.levelPrint("Role set")
    cs.increaseLevel
    roles.sortWith(_.name.msgid < _.name.msgid).foreach(_.dump(cs))
    cs.decreaseLevel
  }
  
  override def toString = {
    roles.sortWith(_.name.msgid < _.name.msgid).mkString(", ")
  }
}

object RoleSet {
  
  def apply(roles: Role*) = new RoleSet(roles.toList)
}
