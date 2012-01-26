package com.strong_links.scalaforms.squeryl


import com.strong_links.core._
import com.strong_links.scalaforms._
import org.squeryl._
import org.squeryl.dsl._
import java.sql.Timestamp
import java.sql.ResultSet
import java.lang.reflect.{Field => JavaField,_}
import org.squeryl.internals._
import java.util.Date


object SquerylFacade extends SquerylFacade

trait SquerylFacade extends PrimitiveTypeMode {
  
  
  implicit def stringFieldToTEF(f: Field[_,String])
    (implicit ev: String => TypedExpression[String,TString]) =
    f.internalValue : TypedExpression[String,TString]
  
  implicit def optionStringFieldToTEF(f: OptionalField[_,String])
    (implicit ev: Option[String] => TypedExpression[Option[String],TOptionString]) =
    f.internalValue : TypedExpression[Option[String],TOptionString]

  implicit def dateFieldToTEF(f: Field[_,Date])
    (implicit ev: Date => TypedExpression[Date,TDate]) =
    f.internalValue : TypedExpression[Date,TDate]
  
  implicit def optionDateFieldToTEF(f: OptionalField[_,Date])
    (implicit ev: Option[Date] => TypedExpression[Option[Date],TOptionDate]) =
    f.internalValue : TypedExpression[Option[Date],TOptionDate]
  
  implicit def timestampFieldToTEF(f: Field[_,Timestamp])
    (implicit ev: Timestamp => TypedExpression[Timestamp,TTimestamp]) =
    f.internalValue : TypedExpression[Timestamp,TTimestamp]
  
  implicit def optionTimestampFieldToTEF(f: OptionalField[_,Timestamp])
    (implicit ev: Option[Timestamp] => TypedExpression[Option[Timestamp],TOptionTimestamp]) =
    f.internalValue : TypedExpression[Option[Timestamp],TOptionTimestamp]
  
  implicit def booleanFieldToTEF(f: Field[_,Boolean])
    (implicit ev: Boolean => TypedExpression[Boolean,TBoolean]) =
    f.internalValue : TypedExpression[Boolean,TBoolean]
  
  implicit def optionBooleanFieldToTEF(f: OptionalField[_,Boolean])
    (implicit ev: Option[Boolean] => TypedExpression[Option[Boolean],TOptionBoolean]) =
    f.internalValue : TypedExpression[Option[Boolean],TOptionBoolean]
  
  implicit def enumFieldToTEF[A <: Enumeration#Value](f: Field[_,A])
    (implicit ev: A => TypedExpression[A,TEnumValue[A]]) =
    f.internalValue : TypedExpression[A,TEnumValue[A]]
  
  implicit def optionEnumFieldToTEF[A <: Enumeration#Value](f: OptionalField[_,A])
    (implicit ev: Option[A] => TypedExpression[A,TOptionEnumValue[A]]) =
    f.internalValue : TypedExpression[A,TOptionEnumValue[A]]

  implicit def byteFieldToTEF(f: Field[_,Byte])
    (implicit ev: Byte => TypedExpression[Byte,TByte]) =
    f.internalValue : TypedExpression[Byte,TByte]
  
  implicit def optionByteFieldToTEF(f: OptionalField[_,Byte])
    (implicit ev: Option[Byte] => TypedExpression[Option[Byte],TOptionByte]) =
    f.internalValue : TypedExpression[Option[Byte],TOptionByte]
  
  implicit def intFieldToTEF(f: Field[_,Int])
    (implicit ev: Int => TypedExpression[Int,TInt]) =
    f.internalValue : TypedExpression[Int,TInt]
  
  implicit def optionIntFieldToTEF(f: OptionalField[_,Int])
    (implicit ev: Option[Int] => TypedExpression[Option[Int],TOptionInt]) =
    f.internalValue : TypedExpression[Option[Int],TOptionInt]

  implicit def longFieldToTEF(f: Field[_,Long])
    (implicit ev: Long => TypedExpression[Long,TLong]) =
    f.internalValue : TypedExpression[Long,TLong]
  
  implicit def optionLongFieldToTEF(f: OptionalField[_,Long])
    (implicit ev: Option[Long] => TypedExpression[Option[Long],TOptionLong]) =
    f.internalValue : TypedExpression[Option[Long],TOptionLong]
  
  implicit def floatFieldToTEF(f: Field[_,Float])
    (implicit ev: Float => TypedExpression[Float,TFloat]) =
    f.internalValue : TypedExpression[Float,TFloat]
  
  implicit def optionFloatFieldToTEF(f: OptionalField[_,Float])
    (implicit ev: Option[Float] => TypedExpression[Option[Float],TOptionFloat]) =
    f.internalValue : TypedExpression[Option[Float],TOptionFloat]

  implicit def doubleFieldToTEF(f: Field[_,Double])
    (implicit ev: Double => TypedExpression[Double,TDouble]) =
    f.internalValue : TypedExpression[Double,TDouble]
  
  implicit def optionDoubleFieldToTEF(f: OptionalField[_,Double])
    (implicit ev: Option[Double] => TypedExpression[Option[Double],TOptionDouble]) =
    f.internalValue : TypedExpression[Option[Double],TOptionDouble]
  
  implicit def bigDecimalFieldToTEF(f: Field[_,BigDecimal])
    (implicit ev: BigDecimal => TypedExpression[BigDecimal,TBigDecimal]) =
    f.internalValue : TypedExpression[BigDecimal,TBigDecimal]
  
  implicit def optionBigDecimalFieldToTEF(f: OptionalField[_,BigDecimal])
    (implicit ev: Option[BigDecimal] => TypedExpression[Option[BigDecimal],TOptionBigDecimal]) =
    f.internalValue : TypedExpression[Option[BigDecimal],TOptionBigDecimal]
  
  val classOfField = classOf[BaseField[Any]]
  val classOfEnumVal = classOf[Enumeration#Value]
  val optionClass = classOf[Option[_]]
  
  override def isSupported(c: Class[_]): Boolean =
    classOfField.isAssignableFrom(c) || super.isSupported(c)

  boot

  
  object DummyE extends com.strong_links.scalaforms.I18nEnumeration {
    type DummyE = Value
    val z = Value(-1, I18n(""))
  }
  
  override def sampleValueFor(c: Class[_]) = 
    if(classOf[com.strong_links.scalaforms.I18nEnumeration#I18NValue].isAssignableFrom(c))
      new java.lang.Integer(0)
    else
      super.sampleValueFor(c)
  
  private def boot = {

    val primitiveTypeFactory = FieldMetaData.factory

    FieldMetaData.factory = new FieldMetaDataFactory {
      
      def createPosoFactory(posoMetaData: PosoMetaData[_]): () => AnyRef =
        () => {
          val c = posoMetaData.constructor
          val r = c._1.newInstance(c._2: _*).asInstanceOf[AnyRef];
          r
        }

      def build(parentMetaData: PosoMetaData[_], name: String,
        property: (Option[JavaField], Option[Method], Option[Method], Set[java.lang.annotation.Annotation]),
        sampleInstance4OptionTypeDeduction: AnyRef, isOptimisticCounter: Boolean) = {

        val (field, getter, setter, annotations) = property

        val f0 = getter.get.invoke(sampleInstance4OptionTypeDeduction)

        if (!f0.isInstanceOf[BaseField[_]]) {
          primitiveTypeFactory.build(parentMetaData, name, property, sampleInstance4OptionTypeDeduction, isOptimisticCounter)
        } 
        else {

          val f = f0.asInstanceOf[BaseField[Any]]

          val domain = f.domain

          val typeOfFieldOrTypeOfOption = domain.defaultValue.getClass

          val isEnumeration = classOfEnumVal.isAssignableFrom(typeOfFieldOrTypeOfOption)

          val isOption = f.isOptional

          new FieldMetaData(
            parentMetaData,
            name,
            typeOfFieldOrTypeOfOption,
            typeOfFieldOrTypeOfOption,
            None,
            isOption,
            getter,
            setter,
            field,
            None,
            isOptimisticCounter,
            f.domain.defaultValue.asInstanceOf[AnyRef]) {

            private def getField(o: AnyRef) =
              getter.get.invoke(o).asInstanceOf[BaseField[Object]]

            override def getNativeJdbcValue(o:AnyRef): AnyRef = {
              val f = getField(o)
              f.internalValue
            }

            override def nativeJdbcType =
              typeOfFieldOrTypeOfOption
            
            protected override def createResultSetHandler =
              (rs:ResultSet,i:Int) => {
                 val z = rs.getObject(i)
                 if(rs.wasNull) null
                 else z.asInstanceOf[AnyRef]
              }

            override def length =
              domain match {
                case s:StringDomain => s.maxLength.getOrElse(super.length)
                case _ => super.length
              }            
            
            override def setFromResultSet(target: AnyRef, rs: java.sql.ResultSet, index: Int) {

              val nativeJdbcValue = rs.getObject(index)

              if (rs.wasNull())
                return

              val f = getField(target)

              val fieldValue = 
                f.domain match {
                  case d:EnumerationDomain[_] => d.e.apply(rs.getInt(index))
                  case _ => nativeJdbcValue
                }

              if (! isOption) {
                f.asInstanceOf[Field[_,_]].initialized = true
                f.internalValue = fieldValue
              }
              else
                f.internalValue = Some(fieldValue)                
            }

            override def get(o: AnyRef) =               
              if (isOption)
                getField(o).internalValue              
              else {
                val f = getField(o)
                val f0 = f.asInstanceOf[Field[Domain[Object],Object]]
                
                if (! f0.initialized)
                  Errors.fatal("Attempted to save an unititialized value of field _ on _." <<(name, parentMetaData.clasz.getCanonicalName))
                  
                f0.value
              }
            
            
            override def set(target: AnyRef, v: AnyRef) =               
              if(isOption)
                getField(target).internalValue = Some(v)
              else {
                val f = getField(target)
                f.internalValue = v
                f.asInstanceOf[Field[_,_]].initialized = true
              }
          }
        }
      }
    }
  }
}
