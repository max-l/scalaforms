package com.strong_links.scalaforms

import com.strong_links.core._

class StringOutputStream extends WriteableOutputStream {
  private val sb = new StringBuilder

  def write(s: String) { sb.append(s) }

  def getString = sb.toString
}

object StringOutputStream {
  def capture(tf: TemplateFunction, _i18nLocale: I18nLocale) = {
    val oc = new OutputContext { val i18nLocale = _i18nLocale; val out = new StringOutputStream }
    tf.emit(oc)
    oc.out.getString
  }
}