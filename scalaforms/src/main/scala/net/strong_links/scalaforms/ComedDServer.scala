package net.strong_links.scalaforms

import net.strong_links.core._
 
import org.cometd.bayeux.Message;
import org.cometd.bayeux.server._
import org.cometd.server.AbstractService;

import org.eclipse.jetty.server._;
import org.eclipse.jetty.servlet._
import javax.servlet._


trait ComedDServer {
  thisServer=>
    
  class NonBroadcastableMessage(val services: AbstractService, val remoteSession: ServerSession, val message: Message)

  private var _bayeuxServer: Option[BayeuxServer] = None
  
  def isReady = _bayeuxServer != None

  def bayeuxServer = 
    _bayeuxServer.getOrElse(Errors.fatal("BayeuxServer not yet created !"));   
    
  def jettyInitParamsForCommetDServlet: Map[String,String] = Map.empty         
  
  def sessionAdded(s: ServerSession): Unit
  
  def sessionRemoved(s: ServerSession, timedOut: Boolean): Unit

  def messageReceived(m: NonBroadcastableMessage): Unit

  def initBayeuxServer: Unit
  
  private def _initBayeuxServer(bs: BayeuxServer): Unit = {

    new AbstractService(bs, "nbr") {
      thisService =>
      addService("/service/nbr", "rcv")
      def rcv(remote: ServerSession, m: Message) =
        messageReceived(new NonBroadcastableMessage(thisService, remote, m))
    }

    bs.addListener(new BayeuxServer.SessionListener {
      def sessionAdded(s: ServerSession) =
        thisServer.sessionAdded(s)
      def sessionRemoved(s: ServerSession, timedout: Boolean) =
        thisServer.sessionRemoved(s, timedout)
    })
    
    initBayeuxServer
  }
  
  def createBayeuxHandlerInto(ctx: ServletContextHandler) = {
    
    val sh = ctx.addServlet(classOf[org.cometd.server.CometdServlet], "/");
    
    for((k,v) <- jettyInitParamsForCommetDServlet)
      sh.setInitParameter(k, v);
    
    ctx.addEventListener(new ServletContextAttributeListener() {
      def attributeAdded(event: ServletContextAttributeEvent) {
        println("!1");
        if (BayeuxServer.ATTRIBUTE == event.getName) {
          _bayeuxServer = Some(event.getValue().asInstanceOf[BayeuxServer])
          _initBayeuxServer(_bayeuxServer.get)
        }
      }

      def attributeRemoved(e: ServletContextAttributeEvent) {}
      def attributeReplaced(e: ServletContextAttributeEvent) {}
    });    
  }
  
  def createBayeuxServerContext(contextPath: String): Handler = {

    val bayCtx = new ServletContextHandler(ServletContextHandler.SESSIONS);
    createBayeuxHandlerInto(bayCtx)
    bayCtx.setContextPath(contextPath);
    bayCtx
  }
  
}

object ComedDServerImpl extends ComedDServer {

  import org.cometd.bayeux.server._

  def sessionAdded(s: ServerSession) {
    println("new session :" + s)
  }

  def sessionRemoved(s: ServerSession, timedOut: Boolean) {
    println("session removed :" + s)
  }

  def messageReceived(m: NonBroadcastableMessage) {
    println("message received :" + m.message)
  }

  def initBayeuxServer = {
    bayeuxServer.setSecurityPolicy(new SecurityPolicy() {
      def canCreate(bs: BayeuxServer, ss: ServerSession, channelId: String, m: ServerMessage) = true
      def canHandshake(bs: BayeuxServer, ss: ServerSession, m: ServerMessage) = true
      def canPublish(bs: BayeuxServer, ss: ServerSession, c: ServerChannel, m: ServerMessage) = true
      def canSubscribe(bs: BayeuxServer, ss: ServerSession, sc: ServerChannel, m: ServerMessage) = true
    })
  }
  
  def _publishAtLarge {

    new Thread {
      override def run {

        import scala.collection.JavaConversions._

        for (i <- 1 to 100000000) {

          if (!ComedDServerImpl.isReady)
            Thread.sleep(1000 * 3)
          else {
            for (s <- ComedDServerImpl.bayeuxServer.getSessions) {

              Thread.sleep(1000 * 3)
              val output = new java.util.HashMap[String, Object]
              output.put("greeting", "--->, " + i);
              //val output = Map("greeting" -> i)
              s.deliver(s, "/nbr", output, null);
            }
          }
        }
      }
    }.start();
  }  
}
