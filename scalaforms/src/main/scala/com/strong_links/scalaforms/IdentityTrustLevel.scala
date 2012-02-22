package com.strong_links.scalaforms

import com.strong_links.core._

sealed trait IdentityTrustLevel {
  override def toString = this.getClass.getCanonicalName
  def numericValue: Int

  def >(other: IdentityTrustLevel) = 
    this.numericValue > other.numericValue    

  def >=(other: IdentityTrustLevel) = 
    this.numericValue >= other.numericValue    
}

object IdentityTrustLevel {
  def decode(l: String) = l match {
    case "1" => Unidentified
    case "2" => AnonymouslyIdentified
    case "3" => CorrelatedWithLowTrust
    case "4" => WeaklyAuthenticated
    case "5" => StronglyAuthenticated
    case "6" => SuperStronglyAuthenticated
    case "7" => DualStronglyAuthenticated
    case _ => Errors.fatal("Invalid IdentityTrustLevel _ " << l)
  }
}

sealed trait Unidentified extends IdentityTrustLevel
object Unidentified extends Unidentified {
  def numericValue = 1
}

sealed trait AnonymouslyIdentified extends Unidentified
object AnonymouslyIdentified extends AnonymouslyIdentified {
  def numericValue = 2
}

sealed trait CorrelatedWithLowTrust extends AnonymouslyIdentified
object CorrelatedWithLowTrust extends CorrelatedWithLowTrust {
  def numericValue = 3
}

sealed trait WeaklyAuthenticated extends CorrelatedWithLowTrust
object WeaklyAuthenticated extends WeaklyAuthenticated {
  def numericValue = 4
} 

sealed trait StronglyAuthenticated extends WeaklyAuthenticated
object StronglyAuthenticated extends StronglyAuthenticated {
  def numericValue = 5
}

sealed trait SuperStronglyAuthenticated extends StronglyAuthenticated
object SuperStronglyAuthenticated extends SuperStronglyAuthenticated {
  def numericValue = 6
}

sealed trait DualStronglyAuthenticated extends StronglyAuthenticated
object DualStronglyAuthenticated extends DualStronglyAuthenticated {
  def numericValue = 7
}
