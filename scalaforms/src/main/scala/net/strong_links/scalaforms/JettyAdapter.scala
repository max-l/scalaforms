package net.strong_links.scalaforms

import net.strong_links.core._
import net.strong_links.scalaforms.schema._

import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.server.session._
import javax.servlet.http.HttpSession
import scala.collection.mutable.HashMap

class JettyAdapter(server: Server) extends Logger {

  private val AUTHENTICATION_ATTRIBUTE_KEY = "ROOT_AUTHENTICATION_KEY" 

  private def sessionAuthenticationsLoaded(s: HttpSession) =
    s.getAttribute(AUTHENTICATION_ATTRIBUTE_KEY) != null
    
  def setIdentity(s: HttpSession, iws: IdentityWithinServer) {
    
    getSessionAttributeMap(s).put(iws.authentication.rootAuthenticationUuid.value, iws)
  }
    
  def getIdentity(s: HttpSession, rootAuthenticationId: String) = 
    getSessionAttributeMap(s).get(rootAuthenticationId)
  
  private def getSessionAttributeMap(s: HttpSession): HashMap[String, IdentityWithinServer] = {
    
    val o = s.getAttribute(AUTHENTICATION_ATTRIBUTE_KEY)    
    if( o != null)
      o.asInstanceOf[HashMap[String,IdentityWithinServer]]
    else {
      val tmp = new HashMap[String,IdentityWithinServer]
      s.setAttribute(AUTHENTICATION_ATTRIBUTE_KEY, tmp)
      
      tmp
    }    
  }
  
  def init(ctx: ServletContextHandler, port: Int, host: String) {
    
    
        val jism = new JDBCSessionIdManager(ctx.getServer)
        
        jism.setWorkerName(host + "_" + port) // this is the cluster node name (must be unique within cluster) 
        
        jism.setDriverInfo(Schema.dbConnectionInfo._1, Schema.dbConnectionInfo._2)
        jism.setScavengeInterval(60)
        ctx.getServer.setSessionIdManager(jism)
        
        val jsm = new JDBCSessionManager {
          
          override def updateSession(data: org.eclipse.jetty.server.session.JDBCSessionManager#SessionData) = {
            
            val sid = data.getId
            val session = this.getSession(sid)
            val aMap = getSessionAttributeMap(session)
            // avoid saving the SessionAttributeMap as a blob in the DB
            try {
              logger.debug("will update session _" << data)
              // set it to null before save
              session.setAttribute(AUTHENTICATION_ATTRIBUTE_KEY, null)
              super.updateSession(data)
            }
            finally {
              // restore it after
              session.setAttribute(AUTHENTICATION_ATTRIBUTE_KEY, aMap)
            }
          }
                    
          override def getSession(idInCluster: String): JDBCSessionManager#Session = {
            
            val s = super.getSession(idInCluster)
            if (s == null)
              return null
            
            if (sessionAuthenticationsLoaded(s))
              return s
              
            try {
              import net.strong_links.scalaforms.squeryl.SquerylDslSupport._                                
              val iwss = transaction {                
                IdentityWithinServer.load(s, server)                
              }
              
              for(iws <- iwss) {
                logger.debug("Identity _ loaded from database into http session." << iws)
                getSessionAttributeMap(s).put(iws.authentication.rootAuthenticationUuid.value, iws)
              }
              
              s
            }
            catch {
              case e:Exception => {
                println(e.printStackTrace)
                throw e
              }
            }
          }          
        }
        jsm.setSessionIdManager(jism)
        jsm.setMaxInactiveInterval(60 * 5)
        val sh = new SessionHandler(jsm)        
        ctx.setSessionHandler(sh);    
  }
  
}