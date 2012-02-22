package com.strong_links.scalaforms

import com.strong_links.core._
import java.io.OutputStream

object ServerOutputStream {
  val flushSteps = List(500, 1000, 2000, 5000, 10000, 25000, 50000, 100000)
}

class ServerOutputStream(os: OutputStream) {

  import ServerOutputStream._

  private var bytesWritten = 0L
  private var currentFlushStep = flushSteps

  def <<(s: String) = write(s)
  
  def write(s: String) {
    val b = s.getBytes("UTF8")
    try {
      os.write(b)
      bytesWritten += b.length
      if (bytesWritten >= currentFlushStep.head)
        flush
    } catch
      Errors.fatalCatch("Output stream write failed on buffer size _" << b.length)
  }

  def flush = {
    os.flush
    bytesWritten = 0
    if (currentFlushStep.tail != Nil)
      currentFlushStep = currentFlushStep.tail
  }
}

