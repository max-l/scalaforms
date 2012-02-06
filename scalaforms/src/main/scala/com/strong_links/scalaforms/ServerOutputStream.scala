package com.strong_links.scalaforms

import com.strong_links.core._
import java.io._

object ServerOutputStream {
  val flushSteps = List(500, 1000, 2000, 5000, 10000, 25000, 50000, 100000)
}

class ServerOutputStream(os: OutputStream) extends OutStream {

  import ServerOutputStream._

  var bytesWritten = 0L
  var currentFlushStep = flushSteps

  def write(s: String) {
    val b = s.getBytes("UTF8")
    try {
      os.write(b)
      bytesWritten += b.length
      if (bytesWritten >= currentFlushStep.head)
        flush
    } catch {
      case e: Exception => Errors.fatal(e, "Output stream write failed on buffer size _" << b.length)
    }
  }

  def flush = {
    os.flush
    bytesWritten = 0
    if (currentFlushStep.tail != Nil)
      currentFlushStep = currentFlushStep.tail
  }
}
