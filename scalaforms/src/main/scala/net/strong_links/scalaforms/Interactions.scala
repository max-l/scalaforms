package net.strong_links.scalaforms

import net.strong_links.core._

trait Interactions

class InteractionsEnabler[A <: Interactions](implicit manifest: Manifest[A]) {
  self: A =>
    
  private [scalaforms] def clasz = manifest.erasure
    
  private val uriOnTL = new ThreadLocal[String]
  
  private object Cache {    
    val map = new IdentityMap[Class[_], Object]
  }

  def uriFor(f: A => Interaction) = {
    val c = manifest.erasure
    val p = Cache.map.put(c) { c => Tweaks.makeInterceptor(c, (_, m, args) => {
      uriOnTL.set(Uri(m, args)); null
    })}
    f(p.asInstanceOf[A])
    uriOnTL.get
  }
}