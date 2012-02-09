package com.strong_links.scalaforms

import com.strong_links.core._
import com.strong_links.scalaforms.ui._
import com.strong_links.scalaforms.templates.standard.page

trait Interaction {

  def process(ic: InteractionContext)
}

object Form {

  def prepare[R](prepareFunc: => R) =
    new FormPrepare(prepareFunc _)

  class FormPrepare[R](prepareFunc: () => R) {

    def renderWith(renderFunc: R => Unit) =
      new FormRenderer(prepareFunc, renderFunc)
  }

  class FormRenderer[R](prepareFunc: () => R, renderFunc: R => Unit) {

    def save(saveFunc: R => Unit) =
      new FormInteraction(prepareFunc, renderFunc, saveFunc)
  }

  class FormInteraction[R](val prepareFunc: () => R, val renderFunc: R => Unit, val saveFunc: R => Unit)
    extends Interaction {

    def process(ic: InteractionContext) {
      // incomplete ...
      val r = prepareFunc()
      renderFunc(r)
    }
  }

}

object GetAction {

  def apply(action: => Unit) = new GetInteraction(action _)

  class GetInteraction(action: () => Unit) extends Interaction {

    def process(ic: InteractionContext) {
      action()
    }
  }
}

object JsonAction {

  def apply(action: => Unit) = new JsonInteraction(action _)

  class JsonInteraction(action: () => Unit) extends Interaction {

    def process(ic: InteractionContext) {
      action()
    }
  }
}

object GetPage {

  def prepare[R](prepareFunc: => R) =
    new GetPrepare(prepareFunc _)

  class GetPrepare[R](prepareFunc: () => R) {

    def renderWith(renderFunc: R => Unit) =
      new PageInteraction(prepareFunc, renderFunc)
  }

  class PageInteraction[R](prepareFunc: () => R, renderFunc: R => Unit)
    extends Interaction {

    def process(ic: InteractionContext) {
      val r = prepareFunc()
      renderFunc(r)
    }
  }
}

object Interaction {

  def apply[T <: Interaction](f: InteractionContext => T)(implicit m: Manifest[T]) =
    new InteractionDefinition(f, m.erasure, m.erasure == classOf[JsonAction.JsonInteraction])
}

