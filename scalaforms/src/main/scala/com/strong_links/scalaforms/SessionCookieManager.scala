package com.strong_links.scalaforms

import com.strong_links.core._
import com.strong_links.core.crypto._
import unfiltered.{Cookie => UCookie}
import scala.collection.mutable.HashMap
import org.apache.commons.codec.binary.StringUtils
import org.apache.commons.codec.binary.Base64


/**
 * the set of output cookies is in real life (in the browser) a global mutable variable,
 * so we model it here as such (mutable HashMap)...
 */

object SessionCookieManager {
  
  private val userCorrelationIdCookieName = "ucid"
  private val secureSessionTokenCookieName = "sst"
  private val anonymousNonSecureSessionIdCookieName = "anssid"
  private val nextUriAfterAuthCookieName = "nuaa"
    
  def resetAllCookies(path: String) = {
    Seq(userCorrelationIdCookieName, 
        secureSessionTokenCookieName, 
        anonymousNonSecureSessionIdCookieName, 
        nextUriAfterAuthCookieName) map { cookieName =>
      UCookie(cookieName,"").maxAge(0).path(path)
    }
  }
}

class SessionCookieManager(
    applicationIdForCookiePath: String,
    inputCookies: Seq[UCookie],
    serverSecretKey: Array[Byte],
    requestedIdentityTrustLevel: IdentityTrustLevel,
    sslSessionId: Option[String]) extends Logging {

  import SessionCookieManager._
  
  def stronlyAuthenticatedSessionMaxIdleTime = 5 * 60

  def anonymousNonSecureSessionMaxIdleTime = 5 * 60 * 60
  
  def userCorrelationCookieMaxAgeInDays = 200 

  def newCookie(name: String, value: String, maxAge: Int) =
    UCookie(name, value, None, Some(applicationIdForCookiePath)).maxAge(maxAge)
  
  private def mapCookiesForLogging(s: Iterable[UCookie]) = 
    s.map(c => c.name +"->" + c.value.substring(0,5) + "...")

  logDebug(
    "Input Cookies: _" << mapCookiesForLogging(inputCookies)
  )

  
  /**
   * The user correlation cookie allows "remembering" a user after terminating (logging out of) an authentication,
   * it contains a userId, that should not be sensitive, it is not encrypted nor signed. 
   */

  private def cookieValue(name: String) = 
    inputCookies.find(_.name == name).map(_.value)

  val secureSessionTokenRawValue = 
    cookieValue(secureSessionTokenCookieName)

  val (secureSessionTokenStatusIfExists, secureSessionToken) =
    if(sslSessionId.isEmpty) 
      (None, None)
    else secureSessionTokenRawValue match { 
      case Some(tokenCookieValue) => 
        val t =
          SecureSessionToken.validateAndExtend(
            tokenCookieValue,
            serverSecretKey,
            sslSessionId.get,
            stronlyAuthenticatedSessionMaxIdleTime)
            (Some(t._1), t._2)
      case None => (None, None)
    }

  secureSessionTokenStatusIfExists.filter(_ == ToughCookieStatus.Invalid).foreach { t =>
    logWarn("invalid secureSessionToken _." << secureSessionToken)
  }

  val needsToAuthenticate =
    (secureSessionTokenStatusIfExists, secureSessionToken) match {
      case (None, None) => 
        requestedIdentityTrustLevel >= StronglyAuthenticated
      case (Some(ToughCookieStatus.Expired), _) => 
        requestedIdentityTrustLevel >= StronglyAuthenticated
      case (Some(ToughCookieStatus.Valid), Some(token)) => 
        requestedIdentityTrustLevel > token.verifiedIdentityTrustLevel
    }

  def nextUriAfterAuth =
    cookieValue(nextUriAfterAuthCookieName).flatMap(v => if(v == "") None else Some(v))

  def cookieForNextUriAfterAuth(nextUri: String) = 
    newCookie(nextUriAfterAuthCookieName, nextUri, 60 * 5)

  val anonymousNonSecureSessionId =
    if(requestedIdentityTrustLevel.numericValue >= AnonymouslyIdentified.numericValue)
      cookieValue(anonymousNonSecureSessionIdCookieName)
    else 
      None

  val outgoingAnonymousNonSecureSessionIdCookieIfRequired =
    if(requestedIdentityTrustLevel.numericValue >= AnonymouslyIdentified.numericValue) Seq(
      newCookie(anonymousNonSecureSessionIdCookieName,
                anonymousNonSecureSessionId.getOrElse(Util.newGuid),
                anonymousNonSecureSessionMaxIdleTime
      )
    )
    else 
      Nil

  val userCorrelationId = 
    cookieValue(userCorrelationIdCookieName)

  def createStrongAuthenticator(userId: String, newTrustLevel: IdentityTrustLevel, forLogout: Boolean = false) = {
    val b = new ToughCookieBakery(
        serverSecretKey, 
        sslSessionId.getOrElse(Errors.fatal("Cannot create a strong authenticator on a non SSL connection.")))
    val duration = if(forLogout) 0 else stronlyAuthenticatedSessionMaxIdleTime
    val confidentialSessionToken = SecureSessionToken.newConfidentialSessionToken(newTrustLevel)
    val c = b.bake(userId, duration, confidentialSessionToken)
    val authenticator = newCookie(secureSessionTokenCookieName, c, stronlyAuthenticatedSessionMaxIdleTime)
    authenticator
  }

  def seqOfStrongAuthTerminationCookiesIfAuthenticated =
    secureSessionToken match {
      case None => Nil
      case Some(t) => Seq(
        newCookie(userCorrelationIdCookieName, t.userId, userCorrelationCookieMaxAgeInDays * 24 * 60 * 60),
        createStrongAuthenticator(t.userId, Unidentified).maxAge(60 * 5)
      )
    }
}



  /**
   * 
   *  userName | expirationTime | encrypt(data,k) | signature
   * 
   *  where :  
   *   k is an encryption key computed by : k = HMAC(userName | expirationTime, serverSecret)
   *   signature is : HMAC( userName| expirationTime | data | sessionKey, k)   
   *
   * Note : 
   * 
   * 1. The sessionKey protects against replay attacks (it should be the SSL session Id) 
   * 2. For additional security, the cookie should have the following attributes : 
   *    HttpOnly
   *    Expires : null (session cookie)
   *    Secure  
   * 3. the 'data' field is not encrypted, in could be, by overriding methods encryptData and decryptData. 
   */


object ToughCookieStatus extends Enumeration {
  type ToughCookieStatus = Value
  val Valid, Invalid, Expired = Value
}

class ToughCookieBakery(_serverSecret: Array[Byte], _sessionKey: String) extends CryptoUtil {

  def this(_serverSecretz: String, sk: String) = this(_serverSecretz.getBytes("UTF-8"), sk)

  def dataIsConfidential = false

  val serverSecret = _serverSecret : CryptoField
  val sessionKey = _sessionKey : CryptoField


  def validate(cookie: String) =
    if(cookie == null) // lets be defensive ...
      (ToughCookieStatus.Invalid, None)
    else 
      cookie.split(':').toList.map(s => s : CryptoField) match {
      case List(userIdInCookie, expirationTimeInCookie, dataInCookie, signatureInCookie) => {
        if(expirationTimeInCookie.asLong == None)
          (ToughCookieStatus.Invalid, None)
        else if(expirationTimeInCookie.asLong.get < System.currentTimeMillis)
          (ToughCookieStatus.Expired, None)
        else {

          val k = 
            hmacSha1(userIdInCookie, expirationTimeInCookie)(serverSecret)

          //println("k :" + k.value)

          val computedSinature = 
            hmacSha1(userIdInCookie, expirationTimeInCookie, decryptData(dataInCookie)(k), sessionKey)(k)

          //println("verif : \n_ \n_" << (signatureInCookie.value,computedSinature.value))

          if(signatureInCookie matches computedSinature) 
            (ToughCookieStatus.Valid, Some(expirationTimeInCookie.asLong.get, dataInCookie.value, userIdInCookie.value))
          else 
            (ToughCookieStatus.Invalid, None)
        }
      }
      case _ => (ToughCookieStatus.Invalid, None)
    }

  def bake(_userId: String, durationInSeconds: Int, _data: String) = {

    val userId = _userId : CryptoField
    val expiryTime =  System.currentTimeMillis + (durationInSeconds * 1000) : CryptoField
    val data = _data : CryptoField

    val k = 
      hmacSha1(userId, expiryTime)(serverSecret)

    //println("k :" + k.value)

    val signature = 
      hmacSha1(userId, expiryTime, encryptData(data)(k), sessionKey)(k)

    //println("sig : _" + signature.value)

    Seq(userId, expiryTime, data, signature).map(_.value).mkString(":")
  }

  protected def decryptData(encryptedData: CryptoField)(key: CryptoField) =
    if(dataIsConfidential) Errors.fatal("Confidential node non implemented.")
    else encryptedData

  protected def encryptData(data: CryptoField)(key: CryptoField) =
    if(dataIsConfidential) Errors.fatal("Confidential node non implemented.")
    else data

}


object SecureSessionToken extends Logging {

  def newConfidentialSessionToken(l: IdentityTrustLevel) = 
    Util.newGuid + "&" + l.numericValue
  
  // Some(secureSessionId, verifiedIdentityTrustLevel)
  private def parseConfidentialSessionToken(data: String) =
    try {
      data.split('&').toList match {
        case List(secureSessionId, verifiedIdentityTrustLevel) => 
          Some(secureSessionId, IdentityTrustLevel.decode(verifiedIdentityTrustLevel))
        case _ => None
      }
    }
    catch {
      case e: Exception => {
        logWarn("invalid confidentialSessionToken _." << data)
        None
      }
    }

  

  def validateAndExtend(cookieValue: String, serverSecretKey: Array[Byte], sslSessionId: String, sessionMaxIdleTime: Int) = 
    Errors.trap("Invalid secureSessionToken _." << cookieValue) {

      val b = new ToughCookieBakery(serverSecretKey, sslSessionId)
      b.validate(cookieValue) match {
        case (s @ ToughCookieStatus.Invalid, _) => (s, None)
        case (s @ ToughCookieStatus.Expired, _) => (s, None)
        case (s @ ToughCookieStatus.Valid, Some((expiryTime, data, userId))) => 
          parseConfidentialSessionToken(data) match {
            case None => (ToughCookieStatus.Invalid, None)
            case Some((secureSessionId, verifiedIdentityTrustLevel)) => 
              (s, Some(new SecureSessionToken(userId, data, b.bake(userId, sessionMaxIdleTime, data), secureSessionId, verifiedIdentityTrustLevel)))
        }
      }
    }
}

class SecureSessionToken(
    val userId: String, 
    val confidentialSessionToken: String, 
    val extendedToken: String, 
    val secureSessionId: String, 
    val verifiedIdentityTrustLevel: IdentityTrustLevel)


object ToughCookieBakery {
  
  def smokeTest = {

    val b = new ToughCookieBakery("454554gfdgfg835j392jf45jf34583f", "fwefe5945f3944455")
    val c = b.bake("zaza", 4*3243, "data")
    println(c)
    println("valid : " + b.validate(c)._1)
  }
  
  def microBenchmark = {
    
    smokeTest

    val iterations = 10000
    val t = Util.timerInSeconds(iterations) {

      val b = new ToughCookieBakery(
          "454554gfdgfg835j392jf45jf34583f4-5945f39444554gfdgfg835j392jf45jf34583f4-5945", 
          "454554gfdgfg835j392jf45jf34583f4-5945f39444554gfdgfg835j392jf45jf34583f4")
      val c = b.bake("maxou", 2*60, "travel")
      
      b.validate(c) match {
        case ToughCookieStatus.Valid => {}
        case _ => sys.error("!!!!")
      }
    }

    val validationTime = (t * 1000) / iterations
    println("avg time of a create + validate : " + validationTime.toFloat + " ms")
  }  
  
  def main(args: Array[String]): Unit = {
    smokeTest
  }
}
