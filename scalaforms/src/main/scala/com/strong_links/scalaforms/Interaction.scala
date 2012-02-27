package com.strong_links.scalaforms

import com.strong_links.core._

abstract trait Interaction {
  def processGet(out: ServerOutputStream): Unit
}


abstract trait LogoutInteraction extends Interaction
abstract trait  FormInteraction extends Interaction {
  def processPost(postArgs: Map[String,Seq[String]]): (FormPostResult[_], (ServerOutputStream) => Unit)
}
abstract case class LoginInteraction[L <: IdentityTrustLevel](maximalTrustLevel: L) extends Interaction {
  def processPost(postArgs: Map[String,Seq[String]]): (LoginResult[L], (ServerOutputStream) => Unit)
}

abstract case class LoginGetInteraction[L <: IdentityTrustLevel](maximalTrustLevel: L) extends Interaction {
  def processLoginGet(args: Map[String,Seq[String]]): (LoginResult[L], (ServerOutputStream) => Unit)
}
