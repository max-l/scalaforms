package net.strong_links.scalaforms

trait FieldRendering {
  def render(os: OutStream): Unit
  def renderLabel(os: OutStream): Unit
  def renderControl(os: OutStream): Unit
  def renderHelp(os: OutStream): Unit
  def renderError(os: OutStream): Unit
}

