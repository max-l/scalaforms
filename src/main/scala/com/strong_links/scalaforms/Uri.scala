package com.strong_links.scalaforms

import com.strong_links.core._

import java.lang.reflect.Method

case class Uri(val uri: String) {
  def format(implicit oc: OutputContext) = UriUtil.format(uri)
  override def toString = uri
}


class StaticUri(packageName: String, path: String) extends Uri(StaticResourceNode.prefix + packageName) {
  override def format(implicit oc: OutputContext) = UriUtil.format(uri)
}


object UriUtil {


  def format(uri: String) = uri


  //def apply(u: String) = new Uri(u)
  
  def apply(method: Method, args: Array[Object]): Uri = {

    val intWebroot = "/int"
    
    val className = {
      val cn = method.getDeclaringClass.getCanonicalName;
      if (cn.endsWith("$")) cn.substring(0, cn.length - 1) else cn
    }
    if (!intWebroot.startsWith("/"))
      Errors.fatal("Constant intWebroot _ does not start with a /." << intWebroot)
    val b = new StringBuilder
    b.append(intWebroot)
    b += '/'
    b.append(className)
    b += '/'
    b.append(method.getName)
    args.foreach(a => { b += '/'; b.append(a) })
    new Uri(b.toString)
  }
}

object UriExtracter {
  def apply(uri: String) = new UriExtracter(uri)
}

class UriExtracter(val uri: String) {

  def splitUri(path: String): (String, String, String, Array[String]) = {
    val segments = Util.split(path, "/").filter(!_.isEmpty)
    if (segments.length < 3)
      Errors.fatal("Invalid uri _." << uri)
    val webRootPath = segments(0)
    val className = segments(1)
    val methodName = segments(2)
    val args = segments.drop(3).toArray
    (webRootPath, className, methodName, args)
  }

  def createTypedArgFromString(c: Class[_], s: String, uri: String): AnyRef = {
    if (c.isAssignableFrom(classOf[String]))
      s
    else if (c.isAssignableFrom(classOf[Int]) || c.isAssignableFrom(classOf[java.lang.Integer]))
      Integer.decode(s).asInstanceOf[AnyRef]
    else if (c.isAssignableFrom(classOf[Float]) || c.isAssignableFrom(classOf[java.lang.Float]))
      java.lang.Float.valueOf(s).asInstanceOf[AnyRef]
    else {
      Errors.fatal("Unsupported type _ in URI _." << (c.getName, uri))
    }
  }

  def extractInformation = {
    val (webRootPath, className, methodName, args) = splitUri(uri)
    val fullClassName = className + "$"
    val c = try {
      Class.forName(fullClassName)
    } catch {
      case e: ClassNotFoundException =>
        Errors.fatal("Module _ not callable from http.\n_"
          << (className, e.getMessage))
      case t => Errors.fatal(t, "Unexpected exception loading class _." << fullClassName)
    }
    val module = c.getField("MODULE$").get(null).asInstanceOf[Module[IdentityTrustLevel]]
    val matchingMethodList = c.getMethods.filter(_.getName == methodName)
    matchingMethodList.length match {
      case 0 => Errors.fatal("Method _ not found in _." << (methodName, className))
      case 1 =>
      case _ => Errors.fatal("Method _._ is ambiguous." << (className, methodName))
    }
    val method = matchingMethodList.head
    val paramTypes = method.getParameterTypes
    val argCount = paramTypes.length
    val argTypes = method.getParameterTypes
    if (args.length != argTypes.length)
      Errors.fatal("Wrong number of arguments in uri _; got _, expected _. " << (uri, args.length, argTypes.length))
    val argTypesWithValues: Seq[(Class[_], String)] = argTypes.zip(args).toSeq
    val argsArrayForInvokation =
      argTypesWithValues.map((t: Tuple2[Class[_], String]) => createTypedArgFromString(t._1, t._2, uri))
    (webRootPath, method, module, argsArrayForInvokation.toArray.asInstanceOf[Array[AnyRef]], args)
  }

  val (webRootPath, method, module, args, rawStringArgs) = extractInformation

  val interactionDefinition =
    method.invoke(module, args: _*).asInstanceOf[InteractionDefinition[IdentityTrustLevel,Interaction]]

  def fqn = method.getDeclaringClass.getCanonicalName + "." + method.getName
  
  def toUri = UriUtil(method, args)
}


//TODO : verify that a module class exists with it's companion object that extends UriReferable...
trait UriReferable[M] {
  
  private val uriOnTL = new ThreadLocal[Uri]

  private object Cache {val map = new IdentityMap[Class[_], Object]}

  def uriFor(f: M => InteractionDefinition[_,_]): Uri = {
    val objClass = this.getClass
    
    val objParentClass = objClass.getSuperclass
    
    assert((objParentClass.getName +"$") == objClass.getName)
    
    val p = 
      Cache.map.put(objParentClass) {
        Tweaks.makeInterceptor(objParentClass, (_, m, args) => {
          uriOnTL.set(UriUtil(m, args)); null
        })
      }
    f(p.asInstanceOf[M])
    uriOnTL.get
  }

  def uriFor(f: (M => InteractionDefinition[_,_]), ic: InteractionContext[_]): Uri = {
    new Uri(uriFor(f).uri)
  }
}

