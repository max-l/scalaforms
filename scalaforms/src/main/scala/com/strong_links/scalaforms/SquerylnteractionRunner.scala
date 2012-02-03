package com.strong_links.scalaforms

import com.strong_links.core._
import com.strong_links.scalaforms._
import com.strong_links.scalaforms.schema._
import com.strong_links.scalaforms.domains._
import com.strong_links.scalaforms.squeryl.SquerylFacade._
import org.squeryl.SessionFactory
import org.squeryl.Session
import org.squeryl.adapters.H2Adapter

object SqueryInteractionRunner {

  private [scalaforms] def init {

    Class.forName(Schema.dbConnectionInfo._1)
    
    def newJdbcConnection = {
      java.sql.DriverManager.getConnection(Schema.dbConnectionInfo._2)
    }
  
    SessionFactory.concreteFactory = Some(() => Session.create(newJdbcConnection, new H2Adapter))
  }

  private [scalaforms] def run(ic: InteractionContext) {

    val tx = transaction {
      val tx0 = new Transaction
      tx0.startTime :- nowTimestamp
      tx0.status :- CompletionStatusDomain.InProgress
      tx0.authenticationId :- ic.iws.authentication.id
      tx0.interactionFqn :- ic.u.fqn
      tx0.interactionArgs :- {
        //TODO:  should we add 'truncate when length exceeded' behavior in the Field/Domain ?
        val maxLength = tx0.interactionArgs.domain.maxLength.get
        val args = ic.u.rawStringArgs.mkString("\t")
        if (args.length > maxLength)
          args.substring(0, maxLength)
        else 
          args
      }
      
      Schema.transactions.insert(tx0)
      tx0
    }        
    
    try 
      transaction {
        val interaction = ic.u.invoke[Interaction]
        val results = interaction.process(ic)
        update(Schema.transactions)(t =>
          where(t.id === tx.id)
          set(t.status  := CompletionStatusDomain.Success,                      
              t.endTime := Some(nowTimestamp)               
          )
        )
        results
      }
    catch {
      case e: Exception => transaction {
        val stackDump = e.getStackTraceString
        update(Schema.transactions)(t =>
          where(t.id === tx.id)
          set(t.status  := CompletionStatusDomain.Failure,                      
              t.endTime := Some(nowTimestamp),
              t.stackDump := Some(stackDump)
          )
        )
      }
      throw e
    }    
  } 
}

  
