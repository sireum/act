// #Sireum

package org.sireum.hamr.act.util

import org.sireum._
import org.sireum.hamr.codegen.common.properties.{OsateProperties, PropertyUtil}
import org.sireum.hamr.ir

object TypeUtil {

  def isMissingType(c: ir.Component) : B = {
    return isMissingTypeClassifier(c.classifier.get)
  }

  def isMissingTypeClassifier(c: ir.Classifier) : B = {
    return c.name == Util.MISSING_AADL_TYPE
  }

  def isBaseType(c: ir.Component): B = {
    return isBaseTypeString(c.classifier.get.name)
  }

  def isBaseTypeString(c: String): B = {
    return translateBaseType(c).nonEmpty
  }

  def isRecordType(c: ir.Component): B = {
    return c.category == ir.ComponentCategory.Data && c.subComponents.size > 0
  }

  def isEnumDef(c: ir.Component): B = {
    val ret: B = PropertyUtil.getDiscreetPropertyValue(c.properties, OsateProperties.DATA_MODEL__DATA_REPRESENTATION) match {
      case Some(ir.ValueProp("Enum")) => T
      case _ => F
    }
    return ret
  }

  def isArrayDef(c: ir.Component): B = {
    val ret: B = PropertyUtil.getDiscreetPropertyValue(c.properties, OsateProperties.DATA_MODEL__DATA_REPRESENTATION) match {
      case Some(ir.ValueProp("Array")) => T
      case _ => F
    }
    return ret
  }

  def getArrayBaseType(c: ir.Component): Option[String] = {
    PropertyUtil.getDiscreetPropertyValue(c.properties, OsateProperties.DATA_MODEL__BASE_TYPE) match {
      case Some(i: ir.ClassifierProp) =>
        if(isBaseTypeString(i.name)) {
          return translateBaseType(i.name)
        } else {
          return Some(Util.getClassifierFullyQualified(ir.Classifier(i.name)))
        }
      case _ => return None[String]()
    }
  }

  def getArrayDimension(c: ir.Component): Option[Z] = {
    return PropertyUtil.getUnitPropZ(c.properties, OsateProperties.DATA_MODEL__DIMENSION)
  }

  def translateBaseType(c: String): Option[String] = {
    c match {
      case "Base_Types::Boolean" => Some("bool")

      case "Base_Types::Integer_8"  => Some(s"int8_t")
      case "Base_Types::Integer_16" => Some(s"int16_t")
      case "Base_Types::Integer_32" => Some(s"int32_t")
      case "Base_Types::Integer_64" => Some(s"int64_t")

      case "Base_Types::Unsigned_8"  => Some(s"uint8_t")
      case "Base_Types::Unsigned_16" => Some(s"uint16_t")
      case "Base_Types::Unsigned_32" => Some(s"uint32_t")
      case "Base_Types::Unsigned_64" => Some(s"uint64_t")

      case "Base_Types::Float"    => Some("float")
      case "Base_Types::Float_32" => Some("double")
      case "Base_Types::Float_64" => Some("double")

      case "Base_Types::Character" => Some("char")
      case "Base_Types::String" => Some("char*")

      case "Base_Types::Integer" =>
        Util.reporter.error(None(), Util.toolName, "Unbounded Base_Types::Integer is not supported")
        None[String]()

      case _ => None[String]()
    }
  }
}
