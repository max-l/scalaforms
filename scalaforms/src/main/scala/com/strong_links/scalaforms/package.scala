package com.strong_links

import com.strong_links.core._

import java.sql.Timestamp
import java.io._

package object scalaforms {

  type RenderingFunction = OutStream => Unit

  def nowDate = new java.util.Date

  def nowTimestamp = new Timestamp(System.currentTimeMillis)

  implicit object TimestampOrdering extends Ordering[Timestamp] {
    def compare(x: Timestamp, y: Timestamp) = x.getTime compare y.getTime
  }

  def applicationWebroot = "/int"
  def cometWebroot = "/comet"
  def anyWebroot = "/"

  type Choices[A] = Option[Map[A, Option[String]]]

  implicit def interactions2Permission(c: InteractionsEnabler[_]) = Permission.makeClassPermission(c)
  implicit def interactionFunction2Permission(f: Function0[Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function1[_, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function2[_, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function3[_, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function4[_, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function5[_, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function6[_, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function7[_, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function8[_, _, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function9[_, _, _, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function10[_, _, _, _, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function11[_, _, _, _, _, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function12[_, _, _, _, _, _, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function13[_, _, _, _, _, _, _, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)
  implicit def interactionFunction2Permission(f: Function22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Interaction]) = Permission.makeMethodPermission(f)

  def capture(code: => RenderingFunction) = {
    val sos = new StringOutputStream
    code(sos)
    sos.get
  }

  object interactionContext extends ThreadLocalStack[InteractionContext]

  object fieldTransformer extends ThreadLocalStack[FieldTransformer]

  object identityFieldTransformer extends FieldTransformer {
    def transform(field: BaseField[_]): FieldRendering = field
  }

  implicit val catalog = PackageI18nConfig.catalog
}