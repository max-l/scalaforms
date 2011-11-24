package net.strong_links.scalaforms

import net.strong_links.core._

trait SystemInteractions extends Interactions {

  def reportJavaScriptError(msg: String) = { 
    new RawInteraction {
      def action {
        Errors.fatal("Javascript error reported: _" << msg)
      }
    }
  }  
}

object SystemInteractions extends InteractionsEnabler[SystemInteractions] with SystemInteractions 

