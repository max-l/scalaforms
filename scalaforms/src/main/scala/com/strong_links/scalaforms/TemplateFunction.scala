package com.strong_links.scalaforms

import com.strong_links.core._

trait TemplateFunction { self =>

  def emit(implicit oc: OutputContext): Unit

  def asString(i18nLocale: I18nLocale) = StringOutputStream.capture(self, i18nLocale)
}