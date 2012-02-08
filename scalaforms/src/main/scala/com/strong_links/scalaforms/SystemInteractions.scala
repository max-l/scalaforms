package com.strong_links.scalaforms

import com.strong_links.core._
import com.strong_links.scalaforms.schema.User

trait SystemInteractions extends Interactions

object SystemInteractions extends InteractionsEnabler[SystemInteractions] with SystemInteractions

trait Poutine extends Interactions {

  def toto = I18nPlural("""Hello world!""", 23).toString
  def totozz = I18nPlural("""Hello world!""", 23).toString

  def plus(i: Int, j: Int) = Interaction { implicit ctx =>
    GetPage prepare {
      val u = new User
      u.firstName :- "Maxime"
      u.lastName :- "Lévesque"
      val data = new {
        val xml = <b>Bold</b>
        val string = "<b>Super & 'Cool' !!!</b>"
        val user = u
      }

      data
    } renderWith { data =>
      com.strong_links.scalaforms.templates.misc.poutine.plus(
        Util.nowAsString, 1.toString, 2.toString, 3.toString, I18n("Hello world"), data.user, data)
    }
  }

  def div(i: Int, j: Int) = Interaction { ctx =>
    GetAction {
      ctx.out.write("_ + _ = _" << (i, j, i + j))
    }
  }
}

object Poutine extends InteractionsEnabler[Poutine] with Poutine
