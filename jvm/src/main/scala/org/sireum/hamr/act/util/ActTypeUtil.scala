// #Sireum

package org.sireum.hamr.act.util

import org.sireum._
import org.sireum.hamr.act.util.Util.reporter
import org.sireum.hamr.codegen.common.properties.{OsateProperties, PropertyUtil}
import org.sireum.hamr.ir

object ActTypeUtil {

  def isBaseType(c: ir.Component): B = {
    return isBaseTypeString(c.classifier.get.name)
  }

  def isBaseTypeString(c: String): B = {
    return translateBaseType(c).nonEmpty
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
        if (isBaseTypeString(i.name)) {
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
      case "Base_Types::Boolean" => return Some("bool")

      case "Base_Types::Integer_8" => return Some(s"int8_t")
      case "Base_Types::Integer_16" => return Some(s"int16_t")
      case "Base_Types::Integer_32" => return Some(s"int32_t")
      case "Base_Types::Integer_64" => return Some(s"int64_t")

      case "Base_Types::Unsigned_8" => return Some(s"uint8_t")
      case "Base_Types::Unsigned_16" => return Some(s"uint16_t")
      case "Base_Types::Unsigned_32" => return Some(s"uint32_t")
      case "Base_Types::Unsigned_64" => return Some(s"uint64_t")

      case "Base_Types::Float" => return Some("float")
      case "Base_Types::Float_32" => return Some("double")
      case "Base_Types::Float_64" => return Some("double")

      case "Base_Types::Character" => return Some("char")
      case "Base_Types::String" => return Some("char*")

      case "Base_Types::Integer" =>
        reporter.error(None(), Util.toolName, "Unbounded Base_Types::Integer is not supported")
        return None[String]()

      case _ => return None[String]()
    }
  }
}
