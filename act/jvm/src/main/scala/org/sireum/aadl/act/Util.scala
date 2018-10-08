// #Sireum

package org.sireum.aadl.act

import org.sireum._
import org.sireum.ops._
import org.sireum.aadl.ir

object Util {

  val GEN_ARTIFACT_PREFIX: String = "tb"

  val MONITOR_COMP_SUFFIX: String  = "Monitor"

  val MONITOR_EVENT_NOTIFICATION_TYPE: String =  "QueuedData"
  val MONITOR_DATA_NOTIFICATION_TYPE: String  = "DataportWrite"

  val NOTIFICATION_TYPE: String = "Notification"

  val INTERFACE_PREFIX: String  = s"${GEN_ARTIFACT_PREFIX}_Monitor"

  val PROP_DATA_MODEL__DATA_REPRESENTATION: String = "Data_Model::Data_Representation"
  val PROP_DATA_MODEL__DIMENSION: String = "Data_Model::Dimension"
  val PROP_DATA_MODEL__BASE_TYPE: String = "Data_Model::Base_Type"

  val PROP_ACTUAL_PROCESSOR_BINDING: String = "Deployment_Properties::Actual_Processor_Binding"
  val PROP_QUEUE_SIZE: String = "Communication_Properties::Queue_Size"

  val DEFAULT_QUEUE_SIZE: Z = 1

  val CONNECTOR_SEL4_NOTIFICATION: String = "sel4Notification"
  val CONNECTOR_RPC: String = "seL4RPCCall"

  val DIR_SRC: String = "src"
  val DIR_INCLUDES: String = "includes"
  val DIR_COMPONENTS: String = "components"
  val DIR_INTERFACES: String = "interfaces"
  val DIR_MONITORS: String = "tb_Monitors"

  def getClassifierFullyQualified(c : ir.Classifier) : String = {
    val t: String = TypeUtil.translateBaseType(c.name) match {
      case Some(v) => v
      case _ => c.name
    }
    return replaceAll(replaceAll(t, "::", "__"), ".", "_")
  }

  def getClassifier(c : ir.Classifier) : String = {
    var s = StringOps(c.name)
    var index = s.lastIndexOf(':') + 1
    s = StringOps(s.substring(index, c.name.size))
    return replaceAll(s.s, ".", "_")
  }

  @pure def getDiscreetPropertyValue(properties: ISZ[ir.Property], propertyName: String): Option[ir.PropertyValue] = {
    for (p <- properties if getLastName(p.name) == propertyName) {
      return Some(ISZOps(p.propertyValues).first)
    }
    return None[ir.PropertyValue]()
  }


  def getLastName(n : ir.Name) : String = {
    return n.name(n.name.size - 1)
  }

  def getName(n : ir.Name) : String = {
    return st"""${(n.name, "_")}""".render
  }


  def isMonitor(s: String) : B = {
    val ss = StringOps(s)
    return ss.startsWith(GEN_ARTIFACT_PREFIX) && ss.endsWith(MONITOR_COMP_SUFFIX)
  }

  def getMonitorName(comp: ir.Component, feature: ir.FeatureEnd) : String = {
    val cname = Util.getLastName(comp.identifier)
    val fname = Util.getLastName(feature.identifier)
    return s"${GEN_ARTIFACT_PREFIX}_${cname}_${fname}_${MONITOR_COMP_SUFFIX}"
  }

  def getMonitorNotificationType(t: ir.FeatureCategory.Type) : String = {
    val ret: String = t match {
      case ir.FeatureCategory.DataPort => Util.MONITOR_DATA_NOTIFICATION_TYPE
      case ir.FeatureCategory.EventDataPort => Util.MONITOR_EVENT_NOTIFICATION_TYPE
      case _ =>
        halt(s"Not expecting: ${t}")
    }
    return ret
  }

  def genMonitorFeatureName(f: ir.FeatureEnd, num: Option[Z]): String = {
    return s"${GEN_ARTIFACT_PREFIX}_${Util.getLastName(f.identifier)}${if(num.nonEmpty) num.get else ""}"
  }

  def genMonitorNotificationFeatureName(f: ir.FeatureEnd): String = {
    return s"${GEN_ARTIFACT_PREFIX}_${Util.getLastName(f.identifier)}_notification"
  }

  def getMonitorWriterName(f: ir.FeatureEnd): String = {
    return s"${getClassifierFullyQualified(f.classifier.get)}_writer"
  }

  def getMonitorWriterParamName(c: ir.Component): String = {
    val name = Util.getClassifierFullyQualified(c.classifier.get)
    if(TypeUtil.isArrayDef(c)) {
      return getContainerName(name)
    } else {
      return name
    }
  }

  def getInterfaceFilename(interfaceName: String): String = {
    return s"${interfaceName}.idl4"
  }

  def getInterfaceName(feature: ir.FeatureEnd): String = {
    val typeName = getClassifierFullyQualified(feature.classifier.get)

    if(feature.category == ir.FeatureCategory.DataPort) {
      return s"${INTERFACE_PREFIX}_${typeName}"
    } else {
      return s"${INTERFACE_PREFIX}_${typeName}_${getQueueSize(feature)}"
    }
  }

  def getTypeHeaderFileName(c: ir.Component) : Option[String] = {
    assert(c.category == ir.ComponentCategory.Process)
    val processor: Option[ir.PropertyValue] = getDiscreetPropertyValue(c.properties, PROP_ACTUAL_PROCESSOR_BINDING)
    val procName: String = processor match {
      case Some(v : ir.ReferenceProp) => getLastName(v.value)
      case _ => return None[String]()
    }
    return Some(s"${GEN_ARTIFACT_PREFIX}_${procName}_types")
  }

  def getContainerName(s: String) : String = {
    return s"${GEN_ARTIFACT_PREFIX}_${s}_container"
  }


  def getUnitPropZ(props: ISZ[ir.Property], propName: String): Option[Z] = {
    val ret: Option[Z] = getDiscreetPropertyValue(props, propName) match {
      case Some(v : ir.UnitProp) =>
        R(v.value) match {
          case Some(vv) => Some(conversions.R.toZ(vv))
          case _ => None[Z]()
        }
      case _ => None[Z]()
    }
    return ret
  }

  def getQueueSize(f: ir.Feature): Z = {
    val ret: Z = getUnitPropZ(f.properties, PROP_QUEUE_SIZE) match {
      case Some(z) => z
      case _ => DEFAULT_QUEUE_SIZE
    }
    return ret
  }


  def replaceAll(s: String, f: String, replacement: String): String = {
    var split: ISZ[String] = ISZ()

    val sops = StringOps(s)
    val cms = conversions.String.toCms(s)
    val fcms = conversions.String.toCms(f)

    var start: Z = 0
    var index: Z = 0
    var i: Z = 0
    for(c <- cms) {
      if(c == fcms(index)){
        index = index + 1
      }
      if(index == fcms.size) {
        split = split :+ sops.substring(start, i - fcms.size + 1)
        start = i + 1
        index = 0
      }
      i = i + 1
    }
    if(start < cms.size) {
      split = split :+ sops.substring(start, cms.size)
    }

    return  st"""${(split, replacement)}""".render
  }

  def toLowerCase(s: String):String = {
    val cms = conversions.String.toCms(s)
    return conversions.String.fromCms(cms.map((c: C) => COps(c).toLower))
  }
}

object TypeUtil {
  def isRecordType(c: ir.Component): B = {
    return c.category == ir.ComponentCategory.Data && c.subComponents.size > 0
  }

  def isArrayDef(c: ir.Component): B = {
    val ret: B = Util.getDiscreetPropertyValue(c.properties, Util.PROP_DATA_MODEL__DATA_REPRESENTATION) match {
      case Some(ir.ValueProp("Array")) => T
      case _ => F
    }
    return ret
  }

  def getArrayBaseType(c: ir.Component): Option[String] = {
    Util.getDiscreetPropertyValue(c.properties, Util.PROP_DATA_MODEL__BASE_TYPE) match {
      case Some(i: ir.ClassifierProp) =>
        if(isBaseType(i.name)) {
          Some(translateBaseType(i.name).get)
        } else {
          Some(Util.getClassifierFullyQualified(ir.Classifier(i.name)))
        }
      case _ => None[String]()
    }
  }

  def getArrayDimension(c: ir.Component): Option[Z] = {
    return Util.getUnitPropZ(c.properties, Util.PROP_DATA_MODEL__DIMENSION)
  }

  def isBaseType(c: String): B = {
    return translateBaseType(c).nonEmpty
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

      case "Base_Types::Character" =>
        cprintln(T, "Character type not currently supported")
        None[String]()
      case "Base_Types::String" =>
        cprintln(T, "String type not currently supported")
        None[String]()

      case _ => None[String]()
    }
  }
}

object StringTemplate{
  def tbInterface(macroName: String): ST = {
    val r : ST = st"""#ifdef ${macroName}
                     |#define ${macroName}
                     |
                     |#endif // ${macroName}
                     |"""
    return r
  }

  def tbImpl(typeName: String, dim: Z, typeHeaderFilename: String, monitorTypeHeaderFilename: String): ST = {
    val r: ST =
    st"""#include "../../../../include/${typeHeaderFilename}.h"
        |#include "../include/${monitorTypeHeaderFilename}.h"
        |
        |int mon_get_sender_id(void);
        |int monsig_emit(void);
        |
        |${typeName} contents[${dim}];
        |static uint32_t front = 0;
        |static uint32_t length = 0;
        |
        |static bool is_full(void) {
        |  return length == 1;
        |}
        |
        |static bool is_empty(void) {
        |  return length == 0;
        |}
        |
        |bool mon_dequeue(${typeName} * m) {
        |  if (mon_get_sender_id() != TB_MONITOR_READ_ACCESS) {
        |    return false;
        |  } else if (is_empty()) {
        |    return false;
        |  } else {
        |    *m = contents[front];
        |    front = (front + 1) % 1;
        |    length--;
        |    return true;
        |  }
        |}
        |
        |bool mon_enqueue(const ${typeName} * m) {
        |  if (mon_get_sender_id() != TB_MONITOR_WRITE_ACCESS) {
        |    return false;
        |  } else if (is_full()) {
        |    return false;
        |  } else {
        |    contents[(front + length) % 1] = *m;
        |    length++;
        |    monsig_emit();
        |    return true;
        |  }
        |}
        |"""
    return r
  }

}

@datatype class Monitor (i: ast.Instance,           // camkes monitor
                         interface: ast.Procedure,  // camkes interface
                         writer: ast.Procedure,     // writer interface
                         index: Z,                  // fan-out index
                         ci: ir.ConnectionInstance) // aadl connection

@datatype class ActContainer(models: ISZ[ast.ASTObject],
                            auxFiles: ISZ[(String, ST)])