
package net.strong_links.scalaforms.squeryl

import net.strong_links.core._
import net.strong_links.scalaforms._
import org.squeryl.internals.FieldReferenceLinker
import org.squeryl.PrimitiveTypeMode
import org.squeryl.dsl._
import org.squeryl.dsl.ast._
import org.squeryl.dsl.ast.ConstantExpressionNode
import org.squeryl.internals._
import org.squeryl.Query
import java.lang.reflect.{ Field => JavaField, Method }
import java.sql.ResultSet
import java.sql.Timestamp

object SquerylDslSupport extends SquerylDslSupport

trait SquerylDslSupport extends SquerylDslSupportBase {

  //consumed by FawSchema's constructor
  implicit def zis = this
  
  implicit def singleColumnQuery2RightHandSideOfInZ[A, R <: BaseField[A]](q: Query[R]) = new RightHandSideOfIn[A](q.ast, Some(true))

  def boot = {

    val primitiveTypeFactory = FieldMetaData.factory

    FieldMetaData.factory = new FieldMetaDataFactory {

      val classOfField = classOf[BaseField[Any]]
      val classOfEnumVal = classOf[Enumeration#Value]
      val optionClass = classOf[Option[_]]

      override def isSupportedFieldType(c: Class[_]): Boolean = {
        val b = classOfField.isAssignableFrom(c) ||
          super.isSupportedFieldType(c)          
        b
      }
      
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
        } else {
          val f = f0.asInstanceOf[BaseField[Any]]

          val domain = f.domain
          
          val typeOfFieldOrTypeOfOption = domain.defaultValue.getClass

          val isEnumeration = classOfEnumVal.isAssignableFrom(typeOfFieldOrTypeOfOption)

          val isOption = f.isOptional 
              //optionClass.isAssignableFrom(f.internalValue.getClass)

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

            
            override def length =
              domain match {
                case s:StringDomain => s.maxLength.getOrElse(super.length)
                case _ => super.length
              }            
            
            override def setFromResultSet(target: AnyRef, rs: java.sql.ResultSet, index: Int) = {
              val f = getField(target)
              val v = rs.getObject(index)
              if (!rs.wasNull()) {

                val ve = f.domain match {
                    case d:EnumerationDomain[_] => d.e.apply(rs.getInt(index))
                    case _ => v
                  }                  

                if (! isOption) {
                  f.asInstanceOf[Field[_,_]].initialized = true
                  f.internalValue = ve
                }
                else
                  f.internalValue = Some(ve)                
              }
            }

            override def get(o: AnyRef) = {
              val f = getField(o)
              if (isOption) 
                f.internalValue              
              else {
                val f0 = f.asInstanceOf[Field[Domain[Object],Object]]
                
                if (! f0.initialized)
                  Errors.fatal("Attempted to save an unititialized value of field _ on _." <<(name, parentMetaData.clasz.getCanonicalName))
                  
                f0.value
              }
            }
            
            override def set(target: AnyRef, v: AnyRef) = {
              val f = getField(target)
              if(isOption)
                f.internalValue = Some(v)
              else { 
                f.internalValue = v
                f.asInstanceOf[Field[_,_]].initialized = true
              }
            }
          }
        }
      }
    }
  }   
}

trait SquerylDslSupportBase extends PrimitiveTypeMode {

  private def convertNumerical[T](f: Field[_,T], outMapper: OutMapper[T]) = fieldReference match {
    case Some(e) => new SelectElementReference[T](e)(outMapper) with NumericalExpression[T]
    case None => new ConstantExpressionNode[T](f.internalValue)(outMapper) with NumericalExpression[T]
  }
  
  private def convertNumericalOptional[T](f: OptionalField[_,T], outMapper: OutMapper[Option[T]]) = fieldReference match {
    case Some(e: SelectElement) => new SelectElementReference[Option[T]](e)(outMapper) with NumericalExpression[Option[T]]
    case None => new ConstantExpressionNode[Option[T]](f.value)(outMapper) with NumericalExpression[Option[T]]    
  }
  
  implicit def long2ScalarLong(f: Field[_,Long]) = convertNumerical(f, createOutMapperLongType)

  implicit def int2ScalarInt(f: Field[_,Int]) = convertNumerical(f, createOutMapperIntType)
  
  implicit def double2ScalarDouble(f: Field[_,Double]) = convertNumerical(f, createOutMapperDoubleType)

  
  implicit def decimal2ScalarDecimal(f: Field[_,BigDecimal]) = convertNumerical(f, createOutMapperBigDecimalType)

  implicit def optionInt2ScalarInt(f: OptionalField[_,Int]) = convertNumericalOptional(f, createOutMapperIntTypeOption)
  
  implicit def optionLong2ScalarLong(f: OptionalField[_,Long]) = convertNumericalOptional(f, createOutMapperLongTypeOption)
  
  implicit def optionLongField2OptionLong(f: OptionalField[_,Long]) = convertNumericalOptional(f, createOutMapperLongTypeOption)

  implicit def optionDouble2ScalarDouble(f: OptionalField[_,Double]) = convertNumericalOptional(f, createOutMapperDoubleTypeOption)

  implicit def optionDecimal2ScalarBoolean(f: OptionalField[_,BigDecimal]) = convertNumericalOptional(f, createOutMapperBigDecimalTypeOption)

  
  implicit def string2ScalarString(f: Field[_,String]) = fieldReference match {
    case Some(e) => new SelectElementReference[String](e)(createOutMapperStringType) with StringExpression[String] with NonNumericalExpression[String]
    case None => new ConstantExpressionNode[String](f.internalValue)(createOutMapperStringType) with StringExpression[String] with NonNumericalExpression[String]
  }


  /** Conversion needed for outer joins */
  //implicit def optionIntField2OptionInt(f: Option[Field[_,Int]]) = convertNumericalOption(f, createOutMapperIntTypeOption)
  //implicit def optionDoubleField2OptionDouble(f: Option[TypedField[Double]]) = convertNumericalOption(f, createOutMapperDoubleTypeOption)
  //implicit def optionDecimalField2OptionDecimal(f: Option[TypedField[BigDecimal]]) = convertNumericalOption(f, createOutMapperBigDecimalTypeOption)
  /*
  implicit def optionBooleanField2Boolean(f: Option[TypedField[Boolean]]) = fieldReference match {
    case Some(e) => new SelectElementReference[Boolean](e)(createOutMapperBooleanType) with BooleanExpression[Boolean] with NonNumericalExpression[Boolean]
    case None => new ConstantExpressionNode[Boolean](getValue(f).getOrElse(false)) with BooleanExpression[Boolean] with NonNumericalExpression[Boolean]
  }
  implicit def optionStringField2OptionString(f: Option[TypedField[String]]) = fieldReference match {
    case Some(e) => new SelectElementReference[String](e)(createOutMapperStringType) with StringExpression[String] with NonNumericalExpression[String]
    case None => new ConstantExpressionNode[String](getValueOrNull(f)) with StringExpression[String] with NonNumericalExpression[String]
    
  private def convertNumericalOption[T](f: Option[TypedField[T]], outMapper: OutMapper[Option[T]]) = fieldReference match {
    case Some(e) => new SelectElementReference[Option[T]](e)(outMapper) with NumericalExpression[Option[T]] with NumericalExpression[Option[T]]
    case None => new ConstantExpressionNode[Option[T]](getValue(f)) with NumericalExpression[Option[T]] with NumericalExpression[Option[T]]
  }    
  }
  */
  
  implicit def optionString2ScalarString(f: OptionalField[_,String]) = fieldReference match {
    case Some(e) => new SelectElementReference[Option[String]](e)(createOutMapperStringTypeOption) with StringExpression[Option[String]] with NonNumericalExpression[Option[String]]
    case None => new ConstantExpressionNode[Option[String]](f.value)(createOutMapperStringTypeOption) with StringExpression[Option[String]] with NonNumericalExpression[Option[String]]
  }
  
  
  implicit def bool2ScalarBoolean(f: Field[_,Boolean]) = fieldReference match {
    case Some(e) => new SelectElementReference[Boolean](e)(createOutMapperBooleanType) with BooleanExpression[Boolean] with NonNumericalExpression[Boolean]
    case None => new ConstantExpressionNode[Boolean](f.internalValue)(createOutMapperBooleanType) with BooleanExpression[Boolean] with NonNumericalExpression[Boolean]
  }
  
  implicit def optionBoolean2ScalarBoolean(f: OptionalField[_,Boolean]) = fieldReference match {
    case Some(e) => new SelectElementReference[Option[Boolean]](e)(createOutMapperBooleanTypeOption) with BooleanExpression[Option[Boolean]] with NonNumericalExpression[Option[Boolean]]
    case None => new ConstantExpressionNode[Option[Boolean]](f.value)(createOutMapperBooleanTypeOption) with BooleanExpression[Option[Boolean]] with NonNumericalExpression[Option[Boolean]]
  }

  implicit def enum2EnumExpr[EnumType <: Enumeration](f: Field[_,EnumType#Value]) = fieldReference match {
    case Some(e) => new SelectElementReference[Enumeration#Value](e)(e.createEnumerationMapper) with EnumExpression[Enumeration#Value] with NonNumericalExpression[Enumeration#Value]
    case None => new ConstantExpressionNode[Enumeration#Value](f.internalValue)(outMapperFromEnumValue(f.value)) with EnumExpression[Enumeration#Value] with NonNumericalExpression[Enumeration#Value]
  }
  
  implicit def date2ScalarDate(f: Field[_,java.util.Date]) = fieldReference match {
    case Some(e) => new SelectElementReference[java.util.Date](e)(createOutMapperDateType) with DateExpression[java.util.Date]
    case None => new ConstantExpressionNode[java.util.Date](f.internalValue)(createOutMapperDateType) with DateExpression[java.util.Date]
  }
  
  implicit def optionDate2ScalarDate(f: OptionalField[_,java.util.Date]) = fieldReference match {
    case Some(e) => new SelectElementReference[Option[java.util.Date]](e)(createOutMapperDateTypeOption) with DateExpression[Option[java.util.Date]]
    case None => new ConstantExpressionNode[Option[java.util.Date]](f.value)(createOutMapperDateTypeOption) with DateExpression[Option[java.util.Date]]
  }
    
  implicit def timestamp2ScalarTimestamp(f: Field[_,Timestamp]) = fieldReference match {
    case Some(e) => new SelectElementReference[Timestamp](e)(createOutMapperTimestampType) with DateExpression[Timestamp]
    case None => new ConstantExpressionNode[Timestamp](f.internalValue)(createOutMapperTimestampType) with DateExpression[Timestamp]
  }
  
  implicit def optionTimestamp2ScalarTimestamp(f: OptionalField[_,Timestamp]) = fieldReference match {
    case Some(e) => new SelectElementReference[Option[Timestamp]](e)(createOutMapperTimestampTypeOption) with DateExpression[Option[Timestamp]]
    case None => new ConstantExpressionNode[Option[Timestamp]](f.value)(createOutMapperTimestampTypeOption) with DateExpression[Option[Timestamp]]
  }
  
  implicit def optionEnum2ScalaEnum[EnumType <: Enumeration](f: Field[_,Option[Enumeration#Value]]) = fieldReference match {
    case Some(e) => 
      new SelectElementReference[Option[Enumeration#Value]](e)(e.createEnumerationOptionMapper) with EnumExpression[Option[Enumeration#Value]]
    case None => {
      val v = f.value
      val m = outMapperOptionFromOptionEnumValue(f.value).get : OutMapper[Option[Enumeration#Value]]      
      new ConstantExpressionNode[Option[Enumeration#Value]](v)(m) with EnumExpression[Option[Enumeration#Value]]    
    }
  }
  

 //implicit def singleColumnQuery2RightHandSideOfIn1[A](q: Query[Field[_,A]]) = new RightHandSideOfIn[A](q.ast)  


/*  
  implicit def optionEnumField2OptionEnum[EnumType <: Enumeration](f: Option[TypedField[EnumType#Value]]) = fieldReference match {
    case Some(e) => new SelectElementReference[Enumeration#Value](e)(e.createEnumerationMapper) with EnumExpression[Enumeration#Value] with NonNumericalExpression[Enumeration#Value]
    case None => new ConstantExpressionNode[Enumeration#Value](getValue(f).orNull) with EnumExpression[Enumeration#Value] with NonNumericalExpression[Enumeration#Value]
  }
*/
  
   // Returns the field that was last referenced by Squeryl. Can also be None.
  private def fieldReference = FieldReferenceLinker.takeLastAccessedFieldReference

}
