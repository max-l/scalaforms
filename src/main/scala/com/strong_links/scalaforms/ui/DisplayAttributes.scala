package com.strong_links.scalaforms.ui

import com.strong_links.core._
import com.strong_links.scalaforms._

trait DisplayAttributes[T] { self: T =>

  protected var _label: Option[I18n] = None
  protected var _hide = false
  protected var _renderer: RenderingFunction = defaultRenderer _

  def label(i18nLabel: Option[I18n]): T = {
    _label = i18nLabel
    this
  }

  def label(i18nLabel: I18n): T = {
    label(Some(i18nLabel))
  }

  def hide = {
    _hide = true
    this
  }

  def unhide = {
    _hide = false
    this
  }

  protected def defaultRenderer(oc: OutputContext): Unit

  def renderWithDefault = {
    _renderer = defaultRenderer _
    this
  }

  def renderWith(renderingFunction: RenderingFunction) = {
    _renderer = renderingFunction
    this
  }

  def render(oc: OutputContext) {
    if (!_hide)
      _renderer(oc)
  }
}

