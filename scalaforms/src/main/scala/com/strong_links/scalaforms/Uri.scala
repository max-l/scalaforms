package com.strong_links.scalaforms

import com.strong_links.core._

import java.lang.reflect.Method

class Uri(val uri: String) {
  def format(implicit oc: OutputContext) = Uri.format(uri, oc.authId)
  override def toString = uri
}

class AuthenticatedUri(uri: String, authId: String) extends Uri(uri) {
  override def format(implicit oc: OutputContext) = Uri.format(uri, authId)
}

class StaticUri(packageName: String, path: String) extends Uri(StaticResourceNode.prefix + packageName) {
  override def format(implicit oc: OutputContext) = Uri.format(uri, oc.authId)
}

object Uri {

  def format(uri: String, authId: String) = uri + "?authId=" + authId

  def apply(method: Method, args: Array[Object]): Uri = {

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

  def apply(method: Method, args: Array[Object], ic: InteractionContext): AuthenticatedUri =
    new AuthenticatedUri(apply(method, args).uri, ic.authId)
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
        Errors.fatal("Interactions _ not callable from http, make sure it is extended by an InteractionsEnabler[__] object.\n_"
          << (className, e.getMessage))
      case t => Errors.fatal(t, "Unexpected exception loading class _." << fullClassName)
    }
    val interactions = c.getField("MODULE$").get(null).asInstanceOf[Interactions]
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
    (webRootPath, method, interactions, argsArrayForInvokation.toArray.asInstanceOf[Array[AnyRef]], args)
  }

  val (webRootPath, method, interactions, args, rawStringArgs) = extractInformation

  val interactionDefinition = method.invoke(interactions, args: _*).asInstanceOf[InteractionDefinition]

  def invokeInteraction(ic: InteractionContext) = interactionDefinition.f(ic)

  def toUri(ic: InteractionContext) = Uri(method, args, ic)

  def fqn = method.getDeclaringClass.getCanonicalName + "." + method.getName
}

