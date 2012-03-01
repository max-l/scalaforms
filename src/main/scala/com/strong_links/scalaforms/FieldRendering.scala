package com.strong_links.scalaforms

trait FieldRendering {
  def render(oc: OutputContext): Unit
  def renderLabel(oc: OutputContext): Unit
  def renderControl(oc: OutputContext): Unit
  def renderHelp(oc: OutputContext): Unit
  def renderError(oc: OutputContext): Unit
}

