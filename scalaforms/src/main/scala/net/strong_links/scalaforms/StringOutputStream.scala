package net.strong_links.scalaforms

import net.strong_links.core._

class StringOutputStream extends OutStream {
  
  val sb = new StringBuilder
  
  def write(s: String) {
    sb.append(s)  
  }

  def get = {
    sb.toString
  }
}
  
