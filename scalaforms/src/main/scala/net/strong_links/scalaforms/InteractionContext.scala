package net.strong_links.scalaforms

import net.strong_links.core._
import net.strong_links.scalaforms.schema._

import net.strong_links.scalaforms.squeryl.SquerylDslSupport._

import javax.servlet.http.HttpSession
import unfiltered.request._
import javax.servlet.http.HttpServletRequest

class InteractionContext(var iws: IdentityWithinServer, val server: Server, 
                         val u: UriExtracter, val httpRequest: HttpRequest[HttpServletRequest])
  extends Logger { 
  
  val allowed = 
    iws.roleSet.allows(u.interactions.asInstanceOf[InteractionsEnabler[_]]) || 
    iws.roleSet.allows(u.method)
    
  if (! allowed)
    Errors.fatal("Interaction _ not allowed for user _." << (u.uri, iws.systemAccount.username.value))
    
  override def toString = 
    iws.toString
  
  def authId = iws.authentication.rootAuthenticationUuid.value
  
  def currentIdendityUsername = iws.systemAccount.username.value
  
  def actAs(username: String)  {
    
    iws = IdentityWithinServer.authenticateWithNewIdentity(iws, username, server)    
  }
  
  def login(username: String) {        
    iws = IdentityWithinServer.authenticateWithNewIdentity(iws, username, server)    
  }
  
  def logout {
             
    update(Schema.authentications)(a => 
      where(a.id === iws.authentication.id)
      set(a.endTime := Some(nowTimestamp))
    )    
    
    if (iws.authentication.isAnonymous) {
      iws.session.invalidate()
    }
    else {
      
      val rootAuth = authId
      
      val previousSession = iws.session
      
      val previousIws = 
        IdentityWithinServer.lastActiveIdentityFor(iws.session, rootAuth, server).
          getOrElse(Errors.fatal("Found no previous authentication for rootAuthenticationUuid = _, while logging out of authentication.id=_.) " 
             << (rootAuth, iws.authentication.id)))
           
      server.jettyAdapter.setIdentity(iws.session, previousIws)
      
      logger.debug("Authentication _ ended, replaced by '_'." << (iws, previousIws))
          
      iws = previousIws

      val newSession = iws.session
      
      if (previousSession != newSession)
        Errors.fatal("Session has changed from _ to _." << (previousSession, newSession))
    }
  }
}

