package com.strong_links.scalaforms

trait FieldTransformer {
  def transform(field: BaseField[_]): FieldRendering
}

