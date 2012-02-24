package com.strong_links.scalaforms

import com.strong_links.core._
import com.strong_links.scalaforms.schema._
import com.strong_links.scalaforms.squeryl.SquerylFacade._
import javax.servlet.http.HttpSession
import unfiltered.request._
import unfiltered.{Cookie => UCookie}
import javax.servlet.http.HttpServletRequest
import com.sun.org.apache.xalan.internal.xsltc.compiler.Output
import java.io.PrintWriter
import javax.servlet.http.HttpSession
import scala.collection.mutable.ArrayBuffer

trait OutputContext {
  val i18nLocale: I18nLocale
  val out: ServerOutputStream
  def authId: String
}

class InteractionContext[+L <: IdentityTrustLevel](
    val identityTrustLevel: L,
    _insecureSessionId: Option[String],
    _correlatedUserId: Option[String],
    _userId: Option[String],
    inputCookies: Seq[UCookie]) {

  def correlatedUserId(implicit ev: L <:< CorrelatedWithLowTrust): Option[String] = _correlatedUserId

  def userId(implicit ev: L <:< WeaklyAuthenticated): String = _userId.get

  private val outgoingCookies = new ArrayBuffer[(String,CookieFormater[_])]

  def sendCookie[C](c: C)(implicit formater: CookieFormater[C]) {
    val encodedValue = formater.encode(c)
    outgoingCookies.append((encodedValue, formater))
  }

  def readCookie[C](implicit formater: CookieFormater[C]) =
    inputCookies.find(_.name == formater.cookieName).flatMap(uc => formater.decode(uc.value))
}

trait CookieFormater[T] {
  def cookieName: String
  def maxAgeInSeconds: Int
  def encode(t: T): String
  def decode(s: String): Option[T]
}

trait SimpleCookieFormater[T] extends CookieFormater[T] {
  
  def separator = ":"
    
  def encode(t: T): String =
    toList(t).mkString(separator)

  def decode(s: String) = 
    fromList(s.split(separator).toList)

  def toList(t: T): List[String]
  
  def fromList(l: List[String]): Option[T]  
}

case class ReceiveContext[A](seedObject: A, postArgs: Map[String,Seq[String]])

case class FormPostResult[+A](isOk: Boolean, message: Option[I18n], nextUri: Option[Uri], errorStructure: A)

class FormRenderContext[A,+B](val model: A, val postResult: Option[FormPostResult[B]], val out: ServerOutputStream) {
  def mapReceiveError[R](f: (I18n,B) => R) = postResult.foreach { pr =>
    f(pr.message.get, pr.errorStructure)
  }
}

class LoginFormRenderContext[A,+B](_model: A, _postResult: Option[FormPostResult[B]], _out: ServerOutputStream, val validPersistentLoginAvailable: Boolean)
 extends FormRenderContext(_model, _postResult, _out)

case class RenderContext[A](model: A, out: ServerOutputStream)

sealed trait LoginResult[+L <: IdentityTrustLevel]

sealed case class LoginSuccess[+L <: IdentityTrustLevel](userId: String, level: L, nextUri: Uri) extends LoginResult[L]

sealed case class LoginFailure(errorMessage: I18n, nextUri: Option[Uri] = None) extends LoginResult[Nothing]

case class IdentityTrustLevelEvidence[L](l: L)

class InteractionDefinition[L <: IdentityTrustLevel, I <: Interaction](val requiredTrustLevel: IdentityTrustLevel, f: InteractionContext[L] => I) {

  def createInteraction(ic: InteractionContext[L]) = f(ic)
} 


object GetMethod {
  def receive[A](f: Map[String,Seq[String]] => A) = new GetMethodFunc[A](f)

  class GetMethodFunc[A](f: Map[String,Seq[String]] => A) {

  }

  def receiveExternalLogin[L <: IdentityTrustLevel](f: Map[String,Seq[String]] => LoginResult[L])(implicit idl: IdentityTrustLevelEvidence[L]) =
    new LoginGetInteraction[L](idl.l) {
      def processGet(out: ServerOutputStream) {}
      def processLoginGet(args: Map[String,Seq[String]]): (LoginResult[L], (ServerOutputStream) => Unit) = {
        (f(args), sos => {})
      }
    }
}

object LoginForm {

  def prepare[A](prepareFunc: => A) =
    new LoginFormPrepare(prepareFunc _)

  class LoginFormPrepare[A](prepareFunc: () => A) {

    def receive[L <: IdentityTrustLevel](receiveFunc: ReceiveContext[A] => LoginResult[L])(implicit idl: IdentityTrustLevelEvidence[L]) =
      new LoginFormReceiver(prepareFunc, receiveFunc, idl.l)
  }

  class LoginFormReceiver[A,L <: IdentityTrustLevel](prepareFunc: () => A, receiveResultFunc: ReceiveContext[A] => LoginResult[L], resultingIdentityLevel: L) {

    def render(f: LoginFormRenderContext[A,FormPostResult[LoginResult[L]]] => Unit): LoginInteraction[L] =
      new AuthFormFuncs[A,L](resultingIdentityLevel, prepareFunc, receiveResultFunc, f)
  }

  class AuthFormFuncs[A,L <: IdentityTrustLevel]
   (maximalTrustLevel: L, prepareFunc: () => A, receiveResultFunc: ReceiveContext[A] => LoginResult[L], renderFunc: LoginFormRenderContext[A,FormPostResult[LoginResult[L]]] => Unit) 
     extends LoginInteraction[L](maximalTrustLevel) {

    def processGet(out: ServerOutputStream) {}
    
    def processGet(out: ServerOutputStream, validPersisentLoginPresent: Boolean) {
      val a = prepareFunc()
      renderFunc(new LoginFormRenderContext(a, None, out, validPersisentLoginPresent))
    }

    /**
     * returns  (resultOfReceive,closureToExecuteWhenReceiveFailed)
     */
    def processPost(postArgs: Map[String,Seq[String]]): (LoginResult[L], (ServerOutputStream) => Unit) = {
      val a = prepareFunc()
      // transform a with post results...
      val postRes = receiveResultFunc(ReceiveContext(a, postArgs))

      val convertedRes: FormPostResult[LoginResult[L]] = 
        postRes match {
          case LoginSuccess(userId,level, nextUri) => FormPostResult(true, None, Some(nextUri), LoginSuccess(userId,level, nextUri))
          case LoginFailure(msg,_)                 => FormPostResult(true, Some(msg), None, LoginFailure(msg)) 
        }

      (postRes, (out: ServerOutputStream) => new FormRenderContext(a, Option(convertedRes), out))
    }
  }
}

object Form {

  def prepare[A](prepareFunc: => A) =
    new FormPrepare(prepareFunc _)

  class FormPrepare[A](prepareFunc: () => A) {
    def receive[B](receiveFunc: ReceiveContext[A] => FormPostResult[B]) =
      new FormReceiver(prepareFunc, receiveFunc)
  }

  class FormReceiver[A,B](prepareFunc: () => A, receiveResultFunc: ReceiveContext[A] => FormPostResult[B]) {

    def render(f: FormRenderContext[A,B] => Unit) =
      new FormFuncs(prepareFunc, receiveResultFunc, f)
  }

  class FormFuncs[A,+B](prepareFunc: () => A, receiveResultFunc: ReceiveContext[A] => FormPostResult[B], renderFunc: FormRenderContext[A,B] => Unit) extends FormInteraction {

    def processGet(out: ServerOutputStream) {
      val a = prepareFunc()
      renderFunc(new FormRenderContext(a, None, out))
    }

    /**
     * returns  (resultOfReceive,closureToExecuteWhenReceiveFailed)
     */
    def processPost(postArgs: Map[String,Seq[String]]): (FormPostResult[B], (ServerOutputStream) => Unit) = {
      val a = prepareFunc()
      // transform a with post results...
      val postRes = receiveResultFunc(ReceiveContext(a, postArgs))

      (postRes, (out: ServerOutputStream) => renderFunc(new FormRenderContext(a, Some(postRes), out)))
    }
  }
}

trait BasicInteraction[I] {

  def prepare[R](prepareFunc: => R) =
    new BasicInteractionPrepare(prepareFunc _)
  
  class BasicInteractionPrepare[R](prepareFunc: () => R) {

    def render(renderFunc: RenderContext[R] => Unit) =
      createInteraction(prepareFunc, renderFunc)
  }

  def createInteraction[R](_prepareFunc: () => R, _renderFunc: RenderContext[R] => Unit): I

  class BasicInteractionRenderer[R](prepareFunc: () => R, renderFunc: RenderContext[R] => Unit) {

    //def render(f: RenderContext[R] => Unit) = createInteraction[R](prepareFunc, renderFunc, 

    def processGet(out: ServerOutputStream) = {
      val r = prepareFunc()
      renderFunc(RenderContext(r, out))
    }
  }
}


object Logout extends BasicInteraction[LogoutInteraction] {

  def createInteraction[R](_prepareFunc: () => R, _renderFunc: RenderContext[R] => Unit): LogoutInteraction =
    new BasicInteractionRenderer(_prepareFunc, _renderFunc) with LogoutInteraction
}

object Page extends BasicInteraction[Interaction] {

  def createInteraction[R](_prepareFunc: () => R, _renderFunc: RenderContext[R] => Unit): Interaction =
    new BasicInteractionRenderer(_prepareFunc, _renderFunc) with Interaction
}
