package net.strong_links.scalaforms

import net.strong_links.core._

import java.lang.reflect.Method
import net.sf.cglib.proxy._
import org.apache.bcel.Repository

object Tweaks {
  
  def makeInterceptor(c: Class[_], f: (Object, Method,  Array[Object]) => Object) = {
    val e = new Enhancer
    e.setSuperclass(c) 
    e.setCallback(new MethodInterceptor {
      def intercept(o: Object, m: Method, args: Array[Object], proxy: MethodProxy): Object = f(o, m, args)
    })   
    e.create()
  }

  def getInvokedMethod(c: Class[_]) = {
    val rep = Repository.lookupClass(c)
    val list = rep.getMethods.
        filter(_.getName == "apply").
        filter(_.getReturnType.toString != "java.lang.Object")
    if (list.length != 1)
      Errors.fatal("Unexpected number of methods _, 1 expected." << list.length)
    val targetMethod = list.head
    val lines = targetMethod.getCode.toString.split("\n")
    val flist = 
      for (line <- lines;
           segments = line.replace('\t', ' ').split(' ').filter(!_.isEmpty);
           if (segments.length >= 3);
           if (segments(1) == "invokeinterface");
           f = segments(2))
        yield f
    if (flist.length != 1)
      Errors.fatal("Unexpected number of invokeinterface lines _, 1 expected." << 
                   flist.length)
    val fullMethodName = flist.head.trim
    if (fullMethodName.isEmpty)
      Errors.fatal("Unexpected empty called method.")
    val segments = fullMethodName.split('.').toList
    if (segments.length < 2)
      Errors.fatal("Invalid full method name _." << fullMethodName)
    val className = segments.dropRight(1).mkString(".")
    val methodName = segments.takeRight(1).head
    val targetClass = Class.forName(className)
    val methods = targetClass.getMethods.toList.filter(_.getName == methodName)
    if (methods.length != 1)
      Errors.fatal("Invalid number of methods matching _ for class _; found _ while expecting 1." << 
                    (methodName, className, methods.length))
    methods.head                    
  }
  
  def getPublicMethodsWithReturnType(c: Class[_], returnType: Class[_]) = {
    import java.lang.reflect._
    def rmatch(x: Class[_]) = {
      var done = false
      var ok = false
      var sc = x
      while (!done) {
        ok = (sc == returnType)
        done = ok || (sc == classOf[Object] || sc.isPrimitive)
        if (!done)
          sc = sc.getSuperclass
      }
      ok
    }
    def selected(m: Method) = Modifier.isPublic(m.getModifiers) && rmatch(m.getReturnType)
    c.getMethods.filter(selected).toList
  }
  
  def getFullMethodName(method: Method) = {
    
    val dc0 = method.getDeclaringClass     
    val cn0 = dc0.getCanonicalName
        
    if(cn0.endsWith("$"))
      cn0.substring(0, cn0.length -1) + "." + method.getName
    else
      cn0 + "." + method.getName         
  }  
}

