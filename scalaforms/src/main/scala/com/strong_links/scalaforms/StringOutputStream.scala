package com.strong_links.scalaforms

import com.strong_links.core._

class StringOutputStream extends OutStream {
  
  val sb = new StringBuilder
  
  def write(s: String) {
    sb.append(s)  
  }

  def get = {
    sb.toString
  }
}
  
