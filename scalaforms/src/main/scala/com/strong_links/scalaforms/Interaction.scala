package com.strong_links.scalaforms

import com.strong_links.core._
import com.strong_links.scalaforms.ui._
import com.strong_links.scalaforms.templates.standard.page

trait BaseInteraction {
  private var _os: OutStream = null
  def os = {
    if (_os == null)
      Errors.fatal("Output stream has not been initialized.")
    _os
  }
  def init(outStream: OutStream) {
    _os = outStream
  }
  def run: Unit
}

trait Interaction extends BaseInteraction {
  def preAction {}
  def action: Unit
  def postAction {}
  def run = {
    preAction
    action
    postAction
  }
}

object RawInteraction {
  def apply(code: RenderingFunction) = {
    new RawInteraction {
      def action = code(os)
    }
  }
}

trait RawInteraction extends Interaction {

  override def preAction {
    val parameterToReplace = "$parameterToReplace"
    val uri = SystemInteractions.uriFor(_.reportJavaScriptError(parameterToReplace))
    page.startPage(uri, parameterToReplace)(os)
    os.write("<h1>Start</h1>")
  }

  override def postAction {
    os.write("<h1>End</h1>")
    page.endPage(os)
  }
}

class StandardFormInteraction[R](f: => R)(getAction: Function1[R, Unit], postAction: Function1[R, Unit])
  extends Interaction {
  def action = getAction(f)
  def render {}
}

class StandardGetInteraction[R](f: => R)(getAction: Function1[R, Unit])
  extends Interaction {
  def action = getAction(f)
  def render {}
}