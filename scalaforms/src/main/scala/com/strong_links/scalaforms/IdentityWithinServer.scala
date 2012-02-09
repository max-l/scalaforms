package com.strong_links.scalaforms

import com.strong_links.core._
import com.strong_links.scalaforms.domains._
import com.strong_links.scalaforms.schema._

import com.strong_links.scalaforms.squeryl.SquerylFacade._
import javax.servlet.http.HttpSession

private[scalaforms] class IdentityWithinServer(val session: HttpSession, val authentication: Authentication, val systemAccount: SystemAccount, val user: User, val roleSet: RoleSet) {

  override def toString = {
    "Identity(username=_,rootAuthId=_,roles=_)" <<
      (systemAccount.username.value, authentication.rootAuthenticationUuid.value, roleSet.roles)
  }
  
  def authId = 
    authentication.rootAuthenticationUuid.value
}
