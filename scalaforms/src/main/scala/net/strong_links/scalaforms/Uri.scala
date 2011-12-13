package net.strong_links.scalaforms

import net.strong_links.core._

import java.lang.reflect.Method

object Uri {
  def apply(method: Method, args: Array[Object]): String = {
    val hasIc = interactionContext.isDefined
    val className = {
      val cn = method.getDeclaringClass.getCanonicalName;
      if (cn.endsWith("$")) cn.substring(0, cn.length - 1) else cn
    }
    if (!applicationWebroot.startsWith("/"))
      Errors.fatal("Constant applicationWebroot does not start with a /.")
    val x = List(applicationWebroot, className, method.getName).mkString("/")
    (if (args.length == 0) x else x + "/" + args.toList.mkString("/")) +
      (if (hasIc) "?authId=" + interactionContext.get.authId else "")
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
    val c = try {
      Class.forName(className + "$")
    } catch {
      case e: ClassNotFoundException =>
        Errors.fatal("Interactions _ not callable from http, make sure it is extended by an InteractionsEnabler[__] object.\n_"
          << (className, e.getMessage))
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

  def invoke[T] = {
    method.invoke(interactions, args: _*).asInstanceOf[T]
  }

  def toUri = Uri(method, args)

  def fqn =
    method.getDeclaringClass.getCanonicalName + "." +
      method.getName
}

