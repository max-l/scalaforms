
package com.strong_links.scalaforms

import com.strong_links.core._


class Module[L <: IdentityTrustLevel](val defaultIdentityTrustLevel: L) {


  implicit val identityTrustLevelEvidence0 = IdentityTrustLevelEvidence[Unidentified](Unidentified)
  implicit val identityTrustLevelEvidence1 = IdentityTrustLevelEvidence[AnonymouslyIdentified](AnonymouslyIdentified)
  implicit val identityTrustLevelEvidence2 = IdentityTrustLevelEvidence[WeaklyAuthenticated](WeaklyAuthenticated)
  implicit val identityTrustLevelEvidence3 = IdentityTrustLevelEvidence[StronglyAuthenticated](StronglyAuthenticated)
  implicit val identityTrustLevelEvidence4 = IdentityTrustLevelEvidence[SuperStronglyAuthenticated](SuperStronglyAuthenticated)
  implicit val identityTrustLevelEvidence5 = IdentityTrustLevelEvidence[DualStronglyAuthenticated](DualStronglyAuthenticated)

  // Nécéssaire ???
  //def ok = new FormPostResult(true, None, None, Unit)

  def ok(nextUri: Uri) = 
    new FormPostResult(true, None, Some(nextUri), None)

//  def ok[A](nextUri: Uri) = 
//    new FormPostResult(true, None, None, None)

  def failed(errorMessage: I18n) =
    new FormPostResult(false, Some(errorMessage), None, None)

//  def failed(errorMessage: String) =
//    new FormPostResult(false, None, None, None)

  def failed[A](errorMessage: I18n, a: A) =
    new FormPostResult(false, Some(errorMessage), None, Some(a))

  //def failed(errorMessage: I18n, nextUri: Uri) = 0

  def interaction[I <: Interaction](f: InteractionContext[L] => I) =
    new InteractionDefinition(defaultIdentityTrustLevel, f)

  def interaction[I <: Interaction, O <: IdentityTrustLevel](identityTrustLevelOverride: O)(f: InteractionContext[O] => I) =
    new InteractionDefinition(identityTrustLevelOverride, f)
}
