// #Sireum

package org.sireum.aadl.act

import org.sireum._
import org.sireum.ops._
import org.sireum.aadl.ir
import org.sireum.aadl.ir.{Aadl, Component, FeatureEnd, Transformer}

object Util {

  val DEVELOPER_MODE: B = if(org.sireum.Os.env("ACT_DEVELOPER_MODE").nonEmpty) {
    addWarning("ACT developer mode enabled")
    T
  } else {
    F
  }

  val GEN_ARTIFACT_PREFIX: String = "sb"
  val GEN_ARTIFACT_CAP_PREFIX: String = "SB"

  val MONITOR_COMP_SUFFIX: String  = "Monitor"

  val MONITOR_EVENT_NOTIFICATION_TYPE: String =  "QueuedData"
  val MONITOR_DATA_NOTIFICATION_TYPE: String  = "DataportWrite"

  val NOTIFICATION_TYPE: String = "Notification"

  val INTERFACE_PREFIX: String  = brand("Monitor")


  val PROP_DATA_MODEL__DATA_REPRESENTATION: String = "Data_Model::Data_Representation"
  val PROP_DATA_MODEL__DIMENSION: String = "Data_Model::Dimension"
  val PROP_DATA_MODEL__BASE_TYPE: String = "Data_Model::Base_Type"
  val PROP_DATA_MODEL__ENUMERATORS: String = "Data_Model::Enumerators"

  val PROP_THREAD_PROPERTIES__DISPATCH_PROTOCOL: String = "Thread_Properties::Dispatch_Protocol"
  val PROP_THREAD_PROPERTIES__PRIORITY: String =  "Thread_Properties::Priority"
  val PROP_THREAD_PROPERTIES__PERIOD: String = "Timing_Properties::Period"

  val PROP_DEPLOYMENT_PROPERTIES__ACTUAL_PROCESSOR_BINDING: String = "Deployment_Properties::Actual_Processor_Binding"
  val PROP_COMMUNICATION_PROPERTIES__QUEUE_SIZE: String = "Communication_Properties::Queue_Size"

  val PROP_MEMORY_PROPERTIES__STACK_SIZE: String = "Memory_Properties::Stack_Size"

  val PROP_PROGRAMMING_PROPERTIES__INITIALIZE_ENTRYPOINT_SOURCE_TEXT: String = "Programming_Properties::Initialize_Entrypoint_Source_Text"
  val PROP_PROGRAMMING_PROPERTIES__SOURCE_TEXT: String = "Programming_Properties::Source_Text"

  val PROP_TB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT: String = "TB_SYS::Compute_Entrypoint_Source_Text"
  val PROP_SB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT: String = "SB_SYS::Compute_Entrypoint_Source_Text"

  val PROP_SB_SYS__CAmkES_Owner_Thread: String = "SB_SYS::CAmkES_Owner_Thread"
  val PROP_TB_SYS__CAmkES_Owner_Thread: String = "TB_SYS::CAmkES_Owner_Thread"

  val DEFAULT_QUEUE_SIZE: Z = z"1"
  val DEFAULT_PRIORITY: Z = z"201"
  val DEFAULT_STACK_SIZE: Z = z"1024"
  val DEFAULT_PERIOD: Z = z"1"

  val DIR_SRC: String = "src"
  val DIR_INCLUDES: String = "includes"
  val DIR_COMPONENTS: String = "components"
  val DIR_INTERFACES: String = "interfaces"
  val DIR_MONITORS: String = brand("Monitors")

  val CMAKE_VERSION: String = "3.8.2"

  val cKeywords: ISZ[String] = ISZ("auto", "break", "case", "char", "const", "continue", "default", "do", "double",
    "else", "enum", "extern", "float", "for", "goto", "if", "int", "long", "register", "return", "short",
    "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while")

  val MISSING_AADL_TYPE: String = "MISSING_AADL_TYPE"

  def brand(s: String): String = {
    return s"${Util.GEN_ARTIFACT_PREFIX}_${s}"
  }

  def cbrand(s: String): String = {
    return s"${Util.GEN_ARTIFACT_CAP_PREFIX}_$s"
  }

  def getClassifierFullyQualified(c : ir.Classifier) : String = {
    val t: String = TypeUtil.translateBaseType(c.name) match {
      case Some(v) => v
      case _ => c.name
    }
    return StringUtil.replaceAll(StringUtil.replaceAll(t, "::", "__"), ".", "_")
  }

  def getClassifier(c : ir.Classifier) : String = {
    var s = StringOps(c.name)
    val index = s.lastIndexOf(':') + 1
    s = StringOps(s.substring(index, c.name.size))
    return StringUtil.replaceAll(s.s, ".", "_")
  }

  @pure def getDiscreetPropertyValue(properties: ISZ[ir.Property], propertyName: String): Option[ir.PropertyValue] = {
    val ret: Option[ir.PropertyValue] = getPropertyValues(properties, propertyName) match {
      case ISZ(a) => Some(a)
      case _ => None[ir.PropertyValue]()
    }
    return ret
  }

  @pure def getPropertyValues(properties: ISZ[ir.Property], propertyName: String): ISZ[ir.PropertyValue] = {
    return properties.filter(p => getLastName(p.name) == propertyName).flatMap(p => p.propertyValues)
  }

  def getLastName(n : ir.Name) : String = {
    return n.name(n.name.size - 1)
  }

  def getName(n : ir.Name) : String = {
    return st"${(n.name, "_")}".render
  }


  def isMonitor(s: String) : B = {
    val ss = StringOps(s)
    return ss.startsWith(GEN_ARTIFACT_PREFIX) && ss.endsWith(MONITOR_COMP_SUFFIX)
  }

  def getMonitorName(comp: ir.Component, feature: ir.Feature) : String = {
    val cname = Util.getLastName(comp.identifier)
    val fname = Util.getLastName(feature.identifier)
    return brand(s"${cname}_${fname}_${MONITOR_COMP_SUFFIX}")
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

  def genMonitorFeatureName(f: ir.Feature, num: Option[Z]): String = {
    return brand(s"${Util.getLastName(f.identifier)}${if(num.nonEmpty) num.get else ""}")
  }

  def genMonitorNotificationFeatureName(f: ir.Feature, isDataPort: B): String = {
    val name = s"${Util.getLastName(f.identifier)}${if(isDataPort) "_notification" else "" }"
    return brand(name)
  }

  def getMonitorWriterName(f: ir.FeatureEnd): String = {
    return s"${getClassifierFullyQualified(f.classifier.get)}_writer"
  }

  def getWriterName(c: ir.Classifier): String = {
    return s"${getClassifierFullyQualified(c)}_writer"
  }

  def getSharedDataInterfaceName(c: ir.Classifier): String = {
    return brand(s"${Util.getClassifierFullyQualified(c)}_shared_var")
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
    return st""""../../${DIR_INTERFACES}/${interfaceName}.idl4"""".render
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
    val processor: Option[ir.PropertyValue] = getDiscreetPropertyValue(c.properties, PROP_DEPLOYMENT_PROPERTIES__ACTUAL_PROCESSOR_BINDING)
    val procName: String = processor match {
      case Some(v : ir.ReferenceProp) => getLastName(v.value)
      case _ => return None[String]()
    }
    return Some(brand(s"${procName}_types"))
  }

  def getContainerName(s: String) : String = {
    return brand(s"${s}_container")
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
    val ret: Z = getUnitPropZ(f.properties, PROP_COMMUNICATION_PROPERTIES__QUEUE_SIZE) match {
      case Some(z) => z
      case _ => DEFAULT_QUEUE_SIZE
    }
    return ret
  }

  def isPeriodic(c: ir.Component): B = {
    return getDispatchProtocol(c) == Some(Dispatch_Protocol.Periodic)
  }

  def isSporadic(c: ir.Component): B = {
    return getDispatchProtocol(c) == Some(Dispatch_Protocol.Sporadic)
  }

  def getDispatchProtocol(c: ir.Component): Option[Dispatch_Protocol.Type] = {
    val ret: Option[Dispatch_Protocol.Type] = getDiscreetPropertyValue(c.properties, PROP_THREAD_PROPERTIES__DISPATCH_PROTOCOL) match {
      case Some(ir.ValueProp("Periodic")) => Some(Dispatch_Protocol.Periodic)
      case Some(ir.ValueProp("Sporadic")) => Some(Dispatch_Protocol.Sporadic)
      case _ => None[Dispatch_Protocol.Type]()
    }
    return ret
  }

  def getPriority(c: ir.Component): Option[Z] = {
    val ret: Option[Z] = getDiscreetPropertyValue(c.properties, PROP_THREAD_PROPERTIES__PRIORITY) match {
      case Some(ir.UnitProp(z, _)) =>
        R(z) match {
          case Some(v) => Some(conversions.R.toZ(v))
          case _ => None[Z]()
        }
      case _ => None[Z]()
    }
    return ret
  }

  def getStackSize(c: ir.Component): Option[Z] = {
    val ret: Option[Z] = getDiscreetPropertyValue(c.properties, PROP_MEMORY_PROPERTIES__STACK_SIZE) match {
      case Some(ir.UnitProp(z, u)) =>
        R(z) match {
          case Some(v) =>
            val _v = conversions.R.toZ(v)
            val _ret: Option[Z] = u match {
              case Some("bits")  => Some(_v / z"8")
              case Some("Bytes") => Some(_v)
              case Some("KByte") => Some(_v * z"1000")
              case Some("MByte") => Some(_v * z"1000" * z"1000")
              case Some("GByte") => Some(_v * z"1000" * z"1000" * z"1000")
              case Some("TByte") => Some(_v * z"1000" * z"1000" * z"1000" * z"1000")
              case _ => None[Z]()
            }
            _ret
          case _ => None[Z]()
        }
      case _ => None[Z]()
    }
    return ret

  }

  def getPeriod(c: ir.Component): Option[Z] = {
    val ret: Option[Z] = getDiscreetPropertyValue(c.properties, PROP_THREAD_PROPERTIES__PERIOD) match {
      case Some(ir.UnitProp(z, u)) =>
        R(z) match {
          case Some(v) =>
            val _v = conversions.R.toZ(v)
            val _ret: Option[Z] = u match {
              case Some("ps")  => Some(_v / (z"1000" * z"1000" * z"1000"))
              case Some("ns")  => Some(_v / (z"1000" * z"1000"))
              case Some("us")  => Some(_v / (z"1000"))
              case Some("ms")  => Some(_v)
              case Some("sec") => Some(_v * z"1000")
              case Some("min") => Some(_v * z"1000" * z"60")
              case Some("hr")  => Some(_v * z"1000" * z"60" * z"60")
              case _ => None[Z]()
            }
            _ret
          case _ => None[Z]()
        }
      case _ => None[Z]()
    }
    return ret
  }

  def getInitializeEntryPoint(properties: ISZ[ir.Property]): Option[String] = {
    val ret: Option[String] = getDiscreetPropertyValue(properties, PROP_PROGRAMMING_PROPERTIES__INITIALIZE_ENTRYPOINT_SOURCE_TEXT) match {
      case Some(ir.ValueProp(v)) => Some(v)
      case _ => None[String]()
    }
    return ret
  }

  def getComputeEntrypointSourceText(properties: ISZ[ir.Property]): Option[String] = {
    var ret: Option[String] = getDiscreetPropertyValue(properties, PROP_SB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT) match {
      case Some(ir.ValueProp(v)) => Some(v)
      case _ => None[String]()
    }
    if(ret.isEmpty) {
      ret = getDiscreetPropertyValue(properties, PROP_TB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT) match {
        case Some(ir.ValueProp(v)) =>
          addWarning(s"Property ${PROP_TB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT} is deprecated, use ${PROP_SB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT} instead.")

          Some(v)
        case _ => None[String]()
      }
    }
    return ret
  }

  def getCamkesOwnerThread(p: ISZ[ir.Property]): Option[String] = {
    var ret: Option[String] = getDiscreetPropertyValue(p, PROP_SB_SYS__CAmkES_Owner_Thread) match {
      case Some(ir.ValueProp(v)) => Some(v)
      case _ => None[String]()
    }
    if(ret.isEmpty) {
      ret = getDiscreetPropertyValue(p, PROP_TB_SYS__CAmkES_Owner_Thread) match {
        case Some(ir.ValueProp(v)) =>
          addWarning(s"Property ${PROP_TB_SYS__CAmkES_Owner_Thread} is deprecated, use ${PROP_SB_SYS__CAmkES_Owner_Thread} instead.")

          Some(v)
        case _ => None[String]()
      }
    }
    return ret
  }

  def getSourceText(properties: ISZ[ir.Property]): ISZ[String] = {
    return getPropertyValues(properties, PROP_PROGRAMMING_PROPERTIES__SOURCE_TEXT).map(p => p.asInstanceOf[ir.ValueProp].value)
  }

  def isDataport(f: ir.FeatureEnd): B = {
    return f.category == ir.FeatureCategory.DataPort || f.category == ir.FeatureCategory.EventDataPort
  }

  def addMessage(msg: String): Unit = { cprintln(F, msg) }

  def addWarning(msg: String): Unit = { cprintln(F, s"WARNING: $msg") }

  def addError(msg:String): Unit = { cprintln(T, s"ERROR: $msg") }
}

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
    val ret: B = Util.getDiscreetPropertyValue(c.properties, Util.PROP_DATA_MODEL__DATA_REPRESENTATION) match {
      case Some(ir.ValueProp("Enum")) => T
      case _ => F
    }
    return ret
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
        if(isBaseTypeString(i.name)) {
          return translateBaseType(i.name)
        } else {
          return Some(Util.getClassifierFullyQualified(ir.Classifier(i.name)))
        }
      case _ => return None[String]()
    }
  }

  def getArrayDimension(c: ir.Component): Option[Z] = {
    return Util.getUnitPropZ(c.properties, Util.PROP_DATA_MODEL__DIMENSION)
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
        Util.addError("Unbounded Base_Types::Integer is not supported")
        None[String]()

      case _ => None[String]()
    }
  }
}

object StringUtil {
  def getDirectory(path: String): String = {
    val so = StringOps(path)
    val index = so.lastIndexOf('/')
    if(index >= 0) {
      return so.substring(0, index + 1)
    } else {
      return path
    }
  }

  def replaceAll(s: String, from: String, to: String): String = {
    return StringOps(s).replaceAllLiterally(from, to)
  }

  def toLowerCase(s: String):String = {
    val cms = conversions.String.toCms(s)
    return conversions.String.fromCms(cms.map((c: C) => COps(c).toLower))
  }
}

object StringTemplate{
  val SB_VERIFY: String = Util.cbrand("VERIFY")

  val MON_READ_ACCESS:String = Util.cbrand("MONITOR_READ_ACCESS")
  val MON_WRITE_ACCESS:String = Util.cbrand("MONITOR_WRITE_ACCESS")

  def tbInterface(macroName: String): ST = {
    val r : ST = st"""#ifdef ${macroName}
                     |#define ${macroName}
                     |
                     |#endif // ${macroName}
                     |"""
    return r
  }

  def tbTypeHeaderFile(macroName: String, typeHeaderFileName: String, defs: ISZ[ST], preventBadging: B): ST = {
    val badges: ST = if(preventBadging) {st""} else {st"""
                                                         |#define $MON_READ_ACCESS 111
                                                         |#define $MON_WRITE_ACCESS 222"""}
    val macroname = s"__${Util.cbrand("AADL")}_${typeHeaderFileName}__H"

    val body = st"""#ifndef ${macroname}
                   |#define ${macroname}
                   |
                   |#include <stdbool.h>
                   |#include <stdint.h>
                   |
                   |#ifndef ${SB_VERIFY}
                   |#include <stddef.h>
                   |#endif // ${SB_VERIFY}
                   |
                   |#define __${Util.cbrand("OS")}_CAMKES__${badges}
                   |
                   |#ifndef ${SB_VERIFY}
                   |#define MUTEXOP(OP)\
                   |if((OP) != 0) {\
                   |  fprintf(stderr,"Operation " #OP " failed in %s at %d.\n",__FILE__,__LINE__);\
                   |  *((int*)0)=0xdeadbeef;\
                   |}
                   |#else
                   |#define MUTEXOP(OP) OP
                   |#endif // ${SB_VERIFY}
                   |#ifndef ${SB_VERIFY}
                   |#define CALLBACKOP(OP)\
                   |if((OP) != 0) {\
                   |  fprintf(stderr,"Operation " #OP " failed in %s at %d.\n",__FILE__,__LINE__);\
                   |  *((int*)0)=0xdeadbeef;\
                   |}
                   |#else
                   |#define CALLBACKOP(OP) OP
                   |#endif // ${SB_VERIFY}
                   |
                   |${(defs, "\n\n")}
                   |
                   |#endif // ${macroname}
                   |"""
    return body
  }

  def tbMissingType() : ST = {
    return st"""// placeholder for unspecified types in the AADL model
               |typedef bool ${Util.MISSING_AADL_TYPE};"""
  }

  def tbMonReadWrite(typeName: String, dim: Z, monitorTypeHeaderFilename: String, typeHeaderFilename: String,
                     preventBadging: B): ST = {
    val read: ST = st"""*m = contents;
                       |return true;"""
    val mon_read: ST = if(preventBadging) { read } else {
      st"""if (mon_get_sender_id() != $MON_READ_ACCESS) {
          |  return false;
          |} else {
          |  ${read}}
          |}"""
    }

    val write: ST = st"""contents = *m;
                        |monsig_emit();
                        |return true;"""
    val mon_write: ST = if(preventBadging) { write } else {
      st"""bool mon_write(const $typeName * m) {
          |  if (mon_get_sender_id() != $MON_WRITE_ACCESS)  {
          |    return false;
          |  } else {
          |    ${write}
          |  }
          |}"""
    }

    val senderSig: ST = if(preventBadging) { st"" } else { st"""
                                                               |int mon_get_sender_id(void);""" }
    val r : ST =
      st"""#include "../../../../${Util.DIR_INCLUDES}/${typeHeaderFilename}.h"
          |#include "../${Util.DIR_INCLUDES}/${monitorTypeHeaderFilename}.h"
          |
          |${senderSig}int monsig_emit(void);
          |
          |static $typeName contents;
          |
          |bool mon_read($typeName * m) {
          |  ${mon_read}
          |}
          |
          |bool mon_write(const $typeName * m) {
          |  ${mon_write}
          |}"""
    return r
  }

  def tbEnqueueDequeue(typeName: String, dim: Z, monitorTypeHeaderFilename: String, typeHeaderFilename: String,
                       preventBadging: B): ST = {

    val mon_dequeue: ST = if(preventBadging) { st"" } else {
      st"""if (mon_get_sender_id() != $MON_READ_ACCESS) {
          |  return false;
          |} else """
    }

    val mon_enqueue: ST = if(preventBadging) { st"" } else {
      st"""if (mon_get_sender_id() != $MON_WRITE_ACCESS) {
          |    return false;
          |} else """
    }

    val r: ST =
    st"""#ifndef $SB_VERIFY
        |#include <stdio.h>
        |#endif // $SB_VERIFY
        |
        |#include "../../../../${Util.DIR_INCLUDES}/${typeHeaderFilename}.h"
        |#include "../${Util.DIR_INCLUDES}/${monitorTypeHeaderFilename}.h"
        |
        |int mon_get_sender_id(void);
        |int monsig_emit(void);
        |
        |${typeName} contents[${dim}];
        |static uint32_t front = 0;
        |static uint32_t length = 0;
        |
        |static bool is_full(void) {
        |  return length == ${dim};
        |}
        |
        |static bool is_empty(void) {
        |  return length == 0;
        |}
        |
        |bool mon_dequeue(${typeName} * m) {
        |  ${mon_dequeue}if (is_empty()) {
        |    return false;
        |  } else {
        |    *m = contents[front];
        |    front = (front + 1) % ${dim};
        |    length--;
        |    return true;
        |  }
        |}
        |
        |bool mon_enqueue(const ${typeName} * m) {
        |  ${mon_enqueue}if (is_full()) {
        |    return false;
        |  } else {
        |    contents[(front + length) % ${dim}] = *m;
        |    length++;
        |    monsig_emit();
        |    return true;
        |  }
        |}
        |"""
    return r
  }

  val AUX_C_SOURCES: String = "cSources"
  val AUX_C_INCLUDES: String = "cIncludes"

  def cmakeList(projectName: String, rootServer: String, components: ISZ[ST], cmakeVersion: String,
                auxCSources: ISZ[ST], hasConnectorDefs: B): ST = {
    val aux:ST = if(auxCSources.nonEmpty) {
      st"""set(${AUX_C_SOURCES} "")
          |set(${AUX_C_INCLUDES} "aux/includes")
          |
          |${(auxCSources, "\n")}
          |"""
    } else { st"""""" }

    val connectors: ST = if(hasConnectorDefs) { st"""# add path to connector templates
                                                    |CAmkESAddTemplatesPath(../../../../components/templates/)
                                                    |"""}
    else { st"" }

    val r: ST =
      st"""cmake_minimum_required(VERSION ${cmakeVersion})
          |
          |project (${rootServer} C)
          |
          |${connectors}
          |${aux}
          |${(components, "\n\n")}
          |
          |DeclareCAmkESRootserver(${rootServer}.camkes)
          |"""
    return r
  }

  def auxTemplate(i: String): ST = {
    return st"list(APPEND ${AUX_C_SOURCES} ${i})"
  }

  def cmakeComponent(componentName: String, sources: ISZ[String], includes: ISZ[String], hasAux: B): ST = {
    val s: ST = if(sources.nonEmpty){
      st"""SOURCES ${ if(hasAux) s"$${${AUX_C_SOURCES}} "  else "" }${(sources, " ")}"""
    } else{
      st""
    }
    val i: ST = if(includes.nonEmpty){
      st"""INCLUDES ${ if(hasAux)  s"$${${AUX_C_INCLUDES}} "  else "" }${(includes, " ")}"""
    } else{
      st""
    }

    val r: ST =
      st"""DeclareCAmkESComponent(${componentName}
          |  ${s}
          |  ${i}
          |)"""
    return r
  }

  def configurationPriority(name: String, priority: Z): ST = {
    return st"${name}.priority = ${priority};"
  }

  def configurationStackSize(name: String, size: Z): ST = {
    return st"${name}._control_stack_size = ${size};"
  }

  val SEM_WAIT: String = Util.brand("dispatch_sem_wait")
  val SEM_POST: String = Util.brand("dispatch_sem_post")

  def componentTypeImpl(filename: String, auxCSources: ISZ[ST], stmts: ISZ[ST],
                        preInitComments: ISZ[ST], runPreEntries: ISZ[ST], cDrainQueues: ISZ[ST],
                        isSporadic: B): ST = {
    val initialLock: ST = if(isSporadic) { st"" } else { st"""// Initial lock to await dispatch input.
                                                             |MUTEXOP(${SEM_WAIT}())"""}
    val ret:ST = st"""#include "../${Util.DIR_INCLUDES}/${filename}.h"
        |${(auxCSources, "\n")}
        |#include <string.h>
        |#include <camkes.h>
        |
        |${(stmts, "\n\n")}
        |
        |void pre_init(void) {
        |  ${(preInitComments, "\n")}
        |}
        |
        |/************************************************************************
        | * int run(void)
        | * Main active thread function.
        | ************************************************************************/
        |int run(void) {
        |  ${(runPreEntries, "\n")}
        |  ${initialLock}
        |  for(;;) {
        |    MUTEXOP(${SEM_WAIT}())
        |    // Drain the queues
        |    ${(cDrainQueues, "\n")}
        |  }
        |  return 0;
        |}
        |"""
    return ret
  }

  def componentInitializeEntryPoint(componentName: String, methodName: String): (ST, ST) = {
    val init: String = Util.brand(s"entrypoint_${componentName}_initializer")
    val ret: ST =
      st"""/************************************************************************
          | *  ${init}:
          | *
          | * This is the function invoked by an active thread dispatcher to
          | * call to a user-defined entrypoint function.  It sets up the dispatch
          | * context for the user-defined entrypoint, then calls it.
          | *
          | ************************************************************************/
          |void ${init}(const int64_t * in_arg) {
          |  ${methodName}((int64_t *) in_arg);
          |}"""
    val dummy = Util.brand("dummy")
    val runEntry: ST = st"""{
                           |  int64_t ${dummy};
                           |  ${init}(&${dummy});
                           |}"""
    return (ret, runEntry)
  }

  def cEventNotificiationHandler(handlerName: String, regCallback: String): ST = {
    val ret: ST =
      st"""static void ${handlerName}(void * unused) {
          |  MUTEXOP(${SEM_POST}())
          |  CALLBACKOP(${regCallback}(${handlerName}, NULL));
          |}"""
    return ret
  }

  def cRegCallback(handlerName: String, regCallback: String): ST = {
    val ret: ST = st"CALLBACKOP(${regCallback}(${handlerName}, NULL));"
    return ret
  }

  val VAR_PERIODIC_OCCURRED : String = Util.brand("occurred_periodic_dispatcher")
  val VAR_PERIODIC_TIME : String = Util.brand("time_periodic_dispatcher")
  val METHOD_PERIODIC_CALLBACK : String = Util.brand("timer_complete_callback")

  def periodicDispatchElems() : ST = {
    val ret = st"""static bool ${VAR_PERIODIC_OCCURRED};
                  |static int64_t ${VAR_PERIODIC_TIME};
                  |
                  |/************************************************************************
                  | * periodic_dispatcher_write_int64_t
                  | * Invoked from remote periodic dispatch thread.
                  | *
                  | * This function records the current time and triggers the active thread
                  | * dispatch from a periodic event.  Note that the periodic dispatch
                  | * thread is the *only* thread that triggers a dispatch, so we do not
                  | * mutex lock the function.
                  | *
                  | ************************************************************************/
                  |
                  |bool periodic_dispatcher_write_int64_t(const int64_t * arg) {
                  |    ${VAR_PERIODIC_OCCURRED} = true;
                  |    ${VAR_PERIODIC_TIME} = *arg;
                  |    MUTEXOP(${SEM_POST});
                  |    return true;
                  |}
                  |
                  |void ${METHOD_PERIODIC_CALLBACK}(void *_ UNUSED) {
                  |   // we want time in microseconds, not nanoseconds, so we divide by 1000.
                  |   int64_t ${VAR_PERIODIC_TIME} = ${Util.brand("timer_time()")} / 1000LL;
                  |   (void)periodic_dispatcher_write_int64_t(&${VAR_PERIODIC_TIME});
                  |   ${registerPeriodicCallback()}
                  |}
                  |"""
    return ret
  }

  def registerPeriodicCallback(): ST = {
    return st"CALLBACKOP(${Util.brand("timer_complete_reg_callback")}(${METHOD_PERIODIC_CALLBACK}, NULL));"
  }

  def drainPeriodicQueue(componentName: String, userEntrypoint: String): (ST, ST) = {
    val methodName = Util.brand(s"entrypoint_${componentName}_periodic_dispatcher")

    val impl = st"""void ${methodName}(const int64_t * in_arg) {
                   |  ${userEntrypoint}((int64_t *) in_arg);
                   |}"""

    val drain = st"""if(${VAR_PERIODIC_OCCURRED}){
                    |  ${VAR_PERIODIC_OCCURRED} = false;
                    |  ${methodName}(&${VAR_PERIODIC_TIME});
                    |}"""
    return (impl, drain)
  }
}

object TimerUtil {

  val SEM_DISPATCH: String = Util.brand("dispatch_sem")

  val TIMER_ID: String = Util.brand("timer")
  val TIMER_ID_DISPATCHER: String = "timer"

  val TIMER_NOTIFICATION_ID: String = Util.brand("timer_complete")
  val TIMER_NOTIFICATION_DISPATCHER_ID: String = "timer_complete"

  val TIMER_TYPE: String = "Timer"
  val TIMER_INSTANCE: String = "time_server"

  val TIMER_SERVER_CLASSIFIER: String = "TimeServer"
  val TIMER_SERVER_TIMER_ID: String = "the_timer"
  val TIMER_SERVER_NOTIFICATION_ID: String = "timer_notification"

  val DISPATCH_CLASSIFIER: String = "dispatch_periodic"
  val DISPATCH_PERIODIC_INSTANCE: String = "dispatch_periodic_inst"
  val DISPATCH_TIMER_ID: String = "timer"

  def dispatchComponent(notifications: ISZ[ast.Emits]): ast.Instance = {
    val i = ast.Instance(
      address_space = "",
      name = DISPATCH_PERIODIC_INSTANCE,
      component = ast.Component(
        control = F,
        hardware = F,
        name = DISPATCH_CLASSIFIER,
        mutexes = ISZ(),
        binarySemaphores = ISZ(),
        semaphores = ISZ(),
        dataports = ISZ(),
        emits = notifications,
        uses = ISZ(ast.Uses(
          name = DISPATCH_TIMER_ID,
          typ = TIMER_TYPE,
          optional = F)),
        consumes = ISZ(ast.Consumes(
          name = TIMER_NOTIFICATION_DISPATCHER_ID,
          typ = Util.NOTIFICATION_TYPE,
          optional = F)),
        provides = ISZ(),
        includes = ISZ(),
        attributes = ISZ(),
        imports = ISZ(st""""../../${Util.DIR_INTERFACES}/${TIMER_TYPE}.idl4"""".render)
      ))
    return i
  }

  def calendar(componentName: String, period: Z): ST = {
    val st = st"""if ((aadl_calendar_counter % (${period} / aadl_tick_interval)) == 0) {
                 |  ${componentName}_periodic_dispatcher_emit();
                 |}"""
    return st
  }

  def dispatchComponentCSource(modelTypesHeader: String, calendars: ISZ[ST]): Resource = {
    val THREAD_CALENDAR = Util.brand("thread_calendar")
    val st: ST = st"""#include <string.h>
                     |#include <camkes.h>
                     |#include ${modelTypesHeader}
                     |
                     |// prototypes for clock functions
                     |void clock_init();
                     |void clock_set_interval_in_ms(uint32_t interval);
                     |void clock_start_timer(void);
                     |void clock_irq_callback(void);
                     |uint64_t clock_get_time();
                     |
                     |// Declarations for managing periodic thread dispatch
                     |const uint32_t aadl_tick_interval = 1;
                     |const uint32_t aadl_hyperperiod_subdivisions = 5;
                     |uint32_t aadl_calendar_counter = 0;
                     |uint32_t aadl_calendar_ticks = 0;
                     |
                     |void ${THREAD_CALENDAR}() {
                     |  ${(calendars, "\n")}
                     |
                     |  aadl_calendar_counter = (aadl_calendar_counter + 1) % aadl_hyperperiod_subdivisions;
                     |  aadl_calendar_ticks++;
                     |}
                     |
                     |void ${TIMER_NOTIFICATION_DISPATCHER_ID}_callback() {
                     |  ${THREAD_CALENDAR}();
                     |}
                     |
                     |// no op under the new time server scheme.
                     |void clock_init() { }
                     |
                     |// Set interrupt interval, in milliseconds.
                     |void clock_set_interval_in_ms(uint32_t interval) {
                     |  timer_periodic(0, ((uint64_t)interval) * NS_IN_MS);
                     |}
                     |
                     |// no op under the new time server scheme
                     |void clock_start_timer(void) { }
                     |
                     |// defer to time server
                     |uint64_t clock_get_time() {
                     |  return (timer_time() / NS_IN_MS);
                     |}
                     |
                     |int run(void) {
                     |  clock_init();
                     |  clock_set_interval_in_ms(1);
                     |  clock_start_timer();
                     |  return 0;
                     |}
                     |"""

    val compTypeFileName:String = Util.brand(DISPATCH_CLASSIFIER)
    return Resource(s"${Util.DIR_COMPONENTS}/${DISPATCH_CLASSIFIER}/${Util.DIR_SRC}/${compTypeFileName}.c", st)
  }

  def timerComponent(): ast.Instance = {
    val i = ast.Instance(
      address_space = "",
      name = TIMER_INSTANCE,
      component = ast.Component(
        control = F,
        hardware = F,
        name = TIMER_SERVER_CLASSIFIER,
        mutexes = ISZ(),
        binarySemaphores = ISZ(),
        semaphores = ISZ(),
        dataports = ISZ(),
        emits = ISZ(ast.Emits(
          name = TIMER_SERVER_NOTIFICATION_ID,
          typ = Util.NOTIFICATION_TYPE)),
        uses = ISZ(),
        consumes = ISZ(),
        provides = ISZ(ast.Provides(
          name = TIMER_SERVER_TIMER_ID,
          typ = TIMER_TYPE)),
        includes = ISZ(),
        attributes = ISZ(),
        imports = ISZ()
      )
    )
    return i
  }

  def timerCSource(): Resource = {
    val st: ST = st"""#include <stdio.h>
                     |#include <camkes.h>
                     |
                     |int the_timer_oneshot_relative(int id, uint64_t ns) {
                     |    return -1;
                     |}
                     |
                     |int the_timer_oneshot_absolute(int id, uint64_t ns) {
                     |    return -1;
                     |}
                     |
                     |int the_timer_periodic(int id, uint64_t ns) {
                     |    return -1;
                     |}
                     |
                     |int the_timer_stop(int id) {
                     |    return -1;
                     |}
                     |
                     |unsigned int the_timer_completed() {
                     |    return 1;
                     |}
                     |
                     |uint64_t the_timer_time() {
                     |    return 1;
                     |}
                     |
                     |uint64_t the_timer_tsc_frequency() {
                     |    return 1;
                     |}
                     |"""

    val compTypeFileName:String = Util.brand(TIMER_INSTANCE)
    return Resource(s"${Util.DIR_COMPONENTS}/${TIMER_SERVER_CLASSIFIER}/${Util.DIR_SRC}/${compTypeFileName}.c", st)
  }

  def timerInterface(): Resource = {
    val _st: ST = st"""procedure Timer {
                      |  unsigned int completed();
                      |  int periodic(in int tid, in uint64_t ns);
                      |  int oneshot_absolute(in int tid, in uint64_t ns);
                      |  int oneshot_relative(in int tid, in uint64_t ns);
                      |  int stop(in int tid);
                      |  uint64_t time();
                      |  uint64_t tsc_frequency();
                      |};
                      |"""
    return Resource(s"${Util.DIR_INTERFACES}/${TIMER_TYPE}.idl4", _st)
  }

  def componentNotificationName(name: String): String = {
    return s"${name}_periodic_dispatcher"
  }

  def configurationTimerAttribute(instanceName: String, i: Z, isDispatcher: B): ST = {
    val id: String = if(isDispatcher) {TIMER_ID_DISPATCHER} else {TIMER_ID}
    return st"""${instanceName}.${id}_attributes = ${i};"""
  }

  def configurationTimerGlobalEndpoint(instanceName: String, classifier: String, id: String): ST = {
    return st"""${instanceName}.${id}_global_endpoint = "${classifier}_${id}";"""
  }
}

@datatype class ActContainer(rootServer: String,
                             connectors: ISZ[ast.Connector],
                             models: ISZ[ast.ASTObject],
                             monitors: ISZ[Monitor],
                             cContainers: ISZ[C_Container],
                             auxFiles: ISZ[Resource])


@datatype class Monitor (i: ast.Instance,           // camkes monitor
                         interface: ast.Procedure,  // camkes interface
                         writer: ast.Procedure,     // writer interface
                         cimplementation: Resource,
                         cinclude: Resource,
                         index: Z,                  // fan-out index
                         ci: ir.ConnectionInstance) // aadl connection

@datatype class C_Container(component: String,
                            cSources: ISZ[Resource],
                            cIncludes: ISZ[Resource],
                            sourceText: ISZ[String])

@datatype class C_SimpleContainer(cImpl: Option[ST],
                                  cIncl: Option[ST],
                                  preInits: Option[ST],
                                  drainQueues: Option[(ST, ST)])

@datatype class Resource(path: String,
                         contents: ST)

@enum object Dispatch_Protocol {
  'Periodic
  'Sporadic
}

@datatype class SharedData(owner: ir.Component,
                           ownerFeature: Option[ir.FeatureAccess],
                           typ: ir.Classifier,
                           subcomponentId: String)

@enum object Sel4ConnectorTypes {
  'seL4GlobalAsynchCallback
  'seL4Notification
  'seL4RPCCall
  'seL4SharedData
  'seL4TimeServer
}

object Transformers {

  @datatype class UnboundedIntegerRewriter extends ir.Transformer.PrePost[B] {
    val unboundInt: String = "Base_Types::Integer"
    val int32: String = "Base_Types::Integer_32"

    override def postClassifier(ctx: B, o: ir.Classifier): ir.Transformer.Result[B, ir.Classifier] = {
      if(o.name == unboundInt) {
        Util.addWarning(s"Replacing classifier ${unboundInt} with ${int32}")
        return ir.Transformer.Result(T, Some(ir.Classifier(int32)))
      } else {
        return ir.Transformer.Result(ctx, None())
      }
    }

    override def postClassifierProp(ctx: B, o: ir.ClassifierProp): ir.Transformer.Result[B, ir.PropertyValue] = {
      if(o.name == unboundInt) {
        Util.addWarning(s"Replacing classifier ${unboundInt} with ${int32}")
        return ir.Transformer.Result(T, Some(ir.ClassifierProp(int32)))
      } else {
        return ir.Transformer.Result(ctx, None())
      }
    }
  }



  @datatype class CTX(requiresMissingType: B,
                      hasErrors: B)

  @datatype class MissingTypeRewriter extends ir.Transformer.PrePost[CTX] {

    val missingType: ir.Component = ir.Component(
      ir.Name(ISZ(), None()), // identifier
      ir.ComponentCategory.Data, // category
      Some(ir.Classifier(Util.MISSING_AADL_TYPE)), // classifier
      ISZ(), // features
      ISZ(), // subComponents
      ISZ(), // connections
      ISZ(), // connectionInstances
      ISZ(), // properties
      ISZ(), // flows
      ISZ(), // modes
      ISZ() // annexes
    )

    val missingArrayBaseType: ir.Property = ir.Property(
      name = ir.Name(ISZ(Util.PROP_DATA_MODEL__BASE_TYPE), None()),
      propertyValues = ISZ(ir.ClassifierProp(Util.MISSING_AADL_TYPE)))

    val sporadicProp: ir.Property = ir.Property(
      name = ir.Name(ISZ(Util.PROP_THREAD_PROPERTIES__DISPATCH_PROTOCOL), None()),
      propertyValues = ISZ(ir.ValueProp("Sporadic")))


    override def postAadl(ctx: CTX, o: Aadl): Transformer.Result[CTX, Aadl] = {
      if(ctx.requiresMissingType) {
        ir.Transformer.Result(ctx, Some(o(dataComponents = o.dataComponents :+ missingType)))
      } else {
        ir.Transformer.Result(ctx, None[ir.Aadl]())
      }
    }

    override def postComponent(ctx: CTX, o: Component): Transformer.Result[CTX, Component] = {

      o.category match {
        case ir.ComponentCategory.Data =>
          if(o.classifier.isEmpty) {
            Util.addWarning(s"Classifier not specified for ${Util.getName(o.identifier)}.  Substituting ${Util.MISSING_AADL_TYPE}")

            ir.Transformer.Result(ctx(requiresMissingType = T), Some(o(classifier = Some(ir.Classifier(Util.MISSING_AADL_TYPE)))))
          } else if (TypeUtil.isArrayDef(o) && TypeUtil.getArrayBaseType(o).isEmpty) {
            Util.addWarning(s"Base type not specified for ${o.classifier.get.name}.  Substituting ${Util.MISSING_AADL_TYPE}")

            ir.Transformer.Result(ctx(requiresMissingType = T), Some(o(properties = o.properties :+ missingArrayBaseType)))
          } else {
            ir.Transformer.Result(ctx, None[ir.Component]())
          }

        case ir.ComponentCategory.Thread =>
          Util.getDiscreetPropertyValue(o.properties, Util.PROP_THREAD_PROPERTIES__DISPATCH_PROTOCOL) match {
            case Some(ir.ValueProp(x)) =>
              if(x != "Periodic" && x != "Sporadic") {
                Util.addError(s"${o.classifier.get.name} has unsupported dispatch protocol ${x}.")

                ir.Transformer.Result(ctx(hasErrors = T), None[ir.Component]())
              } else {
                ir.Transformer.Result(ctx, None[ir.Component]())
              }
            case _ =>
              Util.addWarning(s"${Util.PROP_THREAD_PROPERTIES__DISPATCH_PROTOCOL} not specified for thread ${o.classifier.get.name}.  Treating it as sporadic.")

              ir.Transformer.Result(ctx, Some(o(properties =  o.properties :+ sporadicProp)))
          }
        case _ => ir.Transformer.Result(ctx, None[ir.Component]())
      }
    }

    override def postFeatureEnd(ctx: CTX, o: FeatureEnd): Transformer.Result[CTX, FeatureEnd] = {
      if (Util.isDataport(o) && o.classifier.isEmpty) {
        Util.addWarning(s"No datatype specified for data port ${Util.getName(o.identifier)}.  Substituting ${Util.MISSING_AADL_TYPE} ")

        ir.Transformer.Result(ctx(requiresMissingType = T), Some(o(classifier = Some(ir.Classifier(Util.MISSING_AADL_TYPE)))))
      } else {
        ir.Transformer.Result(ctx, None[ir.FeatureEnd]())
      }
    }
  }
}
