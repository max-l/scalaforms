package com.strong_links.scalaforms.ui

import com.strong_links.core._
import com.strong_links.scalaforms._

class GridDef1[A1](f: A1 => Seq[FormField[_]], c1: Class[_]) {
  def render(i: Iterable[A1]) = 
    i.map(f(_))
}

class GridDef2[A1,A2](f: (A1,A2) => Seq[FormField[_]], c1: Class[_], c2: Class[_]) {
  def render(i: Iterable[(A1,A2)]) = 
    i.map(z => f(z._1, z._2))
}

