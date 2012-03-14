package com.strong_links.scalaforms

import com.strong_links.core._
import java.util.IdentityHashMap

package object ui {

  type FieldIdentityMap = java.util.IdentityHashMap[BaseField[_], FormField[_]]
  
  def gridDef[A1](f: A1 => Seq[FormField[_]])(implicit m1: Manifest[A1]) =
    new GridDef1[A1](f, m1.erasure)
  
  def gridDef[A1, A2](f: (A1, A2) => Seq[FormField[_]])(implicit m1: Manifest[A1], m2: Manifest[A2]) =
    new GridDef2[A1, A2](f, m1.erasure, m2.erasure)
    
  def render[A1](i: Iterable[A1])(f: A1 => Seq[FormField[_]])(implicit m1: Manifest[A1]) =
    new GridDef1[A1](f,m1.erasure).render(i)
  
  def render[A1,A2](i: Iterable[(A1,A2)])(f: (A1,A2) => Seq[FormField[_]])(implicit m1: Manifest[A1], m2: Manifest[A2]) =
    new GridDef2[A1,A2](f,m1.erasure, m2.erasure).render(i)
}
