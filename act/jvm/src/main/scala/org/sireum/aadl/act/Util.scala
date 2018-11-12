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

  val PROP_THREAD_PROPERTIES__DISPATCH_PROTOCOL: String = "Thread_Properties::Dispatch_Protocol"
  val PROP_THREAD_PROPERTIES__PRIORITY: String =  "Thread_Properties::Priority"
  val PROP_THREAD_PROPERTIES__PERIOD: String = "Timing_Properties::Period"

  val PROP_DEPLOYMENT_PROPERTIES__ACTUAL_PROCESSOR_BINDING: String = "Deployment_Properties::Actual_Processor_Binding"
  val PROP_COMMUNICATION_PROPERTIES__QUEUE_SIZE: String = "Communication_Properties::Queue_Size"

  val PROP_MEMORY_PROPERTIES__STACK_SIZE: String = "Memory_Properties::Stack_Size"

  val DEFAULT_QUEUE_SIZE: Z = z"1"
  val DEFAULT_PRIORITY: Z = z"201"
  val DEFAULT_STACK_SIZE: Z = z"1024"
  val DEFAULT_PERIOD: Z = z"1"

  val CONNECTOR_SEL4_NOTIFICATION: String = "seL4Notification"
  val CONNECTOR_RPC: String = "seL4RPCCall"
  val CONNECTOR_SEL4_TIMESERVER: String = "seL4TimeServer"
  val CONNECTOR_SEL4_GLOBAL_ASYNCH_CALLBACK: String = "seL4GlobalAsynchCallback"

  val DIR_SRC: String = "src"
  val DIR_INCLUDES: String = "includes"
  val DIR_COMPONENTS: String = "components"
  val DIR_INTERFACES: String = "interfaces"
  val DIR_MONITORS: String = "tb_Monitors"

  val CMAKE_VERSION: String = "3.8.2"

  def getClassifierFullyQualified(c : ir.Classifier) : String = {
    val t: String = TypeUtil.translateBaseType(c.name) match {
      case Some(v) => v
      case _ => c.name
    }
    return StringUtil.replaceAll(StringUtil.replaceAll(t, "::", "__"), ".", "_")
  }

  def getClassifier(c : ir.Classifier) : String = {
    var s = StringOps(c.name)
    var index = s.lastIndexOf(':') + 1
    s = StringOps(s.substring(index, c.name.size))
    return StringUtil.replaceAll(s.s, ".", "_")
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
    return st""""../../interfaces/${interfaceName}.idl4"""".render
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
    val ret: Z = getUnitPropZ(f.properties, PROP_COMMUNICATION_PROPERTIES__QUEUE_SIZE) match {
      case Some(z) => z
      case _ => DEFAULT_QUEUE_SIZE
    }
    return ret
  }

  def getDispatchProtocol(c: ir.Component): Option[Dispatch_Protocol.Type] = {
    val ret: Option[Dispatch_Protocol.Type] = getDiscreetPropertyValue(c.properties, PROP_THREAD_PROPERTIES__DISPATCH_PROTOCOL) match {
      case Some(ir.ValueProp("Periodic")) => Some(Dispatch_Protocol.Periodic)
      case Some(ir.ValueProp("Spordic")) => Some(Dispatch_Protocol.Sporadic)
      case _ => None[Dispatch_Protocol.Type]()
    }
    return ret
  }

  def getPriority(c: ir.Component): Option[Z] = {
    val ret: Option[Z] = getDiscreetPropertyValue(c.properties, PROP_THREAD_PROPERTIES__PRIORITY) match {
      case Some(ir.UnitProp(z, u)) =>
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

object StringTemplate{
  def tbInterface(macroName: String): ST = {
    val r : ST = st"""#ifdef ${macroName}
                     |#define ${macroName}
                     |
                     |#endif // ${macroName}
                     |"""
    return r
  }

  def tbMonReadWrite(typeName: String, dim: Z, monitorTypeHeaderFilename: String, typeHeaderFilename: String): ST = {
    var r : ST =
      st"""#include "../../../../${Util.DIR_INCLUDES}/${typeHeaderFilename}.h"
          |#include "../${Util.DIR_INCLUDES}/${monitorTypeHeaderFilename}.h"
          |
          |int mon_get_sender_id(void);
          |int monsig_emit(void);
          |
          |static $typeName contents;
          |
          |bool mon_read($typeName * m) {
          |  if (mon_get_sender_id() != TB_MONITOR_READ_ACCESS) {
          |    return false;
          |  } else {
          |    *m = contents;
          |    return true;
          |  }
          |}
          |
          |bool mon_write(const $typeName * m) {
          |  if (mon_get_sender_id() != TB_MONITOR_WRITE_ACCESS) {
          |    return false;
          |  } else {
          |    contents = *m;
          |    monsig_emit();
          |    return true;
          |  }
          |}
          |"""
    return r;
  }

  def tbEnqueueDequeue(typeName: String, dim: Z, monitorTypeHeaderFilename: String, typeHeaderFilename: String): ST = {
    val r: ST =
    st"""#include "../../../../${Util.DIR_INCLUDES}/${typeHeaderFilename}.h"
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

  val AUX_C_SOURCES: String = "cSources"
  val AUX_C_INCLUDES: String = "cIncludes"

  def cmakeList(projectName: String, rootServer: String, components: ISZ[ST], cmakeVersion: String, auxCSources: ISZ[ST]): ST = {
    val r: ST =
      st"""cmake_minimum_required(VERSION ${cmakeVersion})
          |
          |project (${rootServer} C)
          |
          |# add path to connector templates
          |CAmkESAddTemplatesPath(../../../../components/templates/)
          |
          |set(${AUX_C_SOURCES} "")
          |set(${AUX_C_INCLUDES} "aux/includes")
          |
          |${(auxCSources, "\n")}
          |
          |${(components, "\n\n")}
          |
          |DeclareCAmkESRootserver(${rootServer}.camkes)
          |"""
    return r
  }

  def auxTemplate(i: String): ST = {
    return st"""list(APPEND ${AUX_C_SOURCES} ${i})"""
  }

  def cmakeComponent(componentName: String, sources: ISZ[String], includes: ISZ[String]): ST = {
    val s: ST = if(sources.nonEmpty){
      st"""SOURCES $${${AUX_C_SOURCES}} ${(sources, " ")}"""
    } else{
      st""""""
    }
    val i: ST = if(includes.nonEmpty){
      st"""INCLUDES $${${AUX_C_INCLUDES}} ${(includes, " ")}"""
    } else{
      st""""""
    }

    val r: ST =
      st"""DeclareCAmkESComponent(${componentName}
          |  ${s}
          |  ${i}
          |)"""
    return r
  }

  def configurationPriority(name: String, priority: Z): ST = {
    return st"""${name}.priority = ${priority};"""
  }

  def configurationStackSize(name: String, size: Z): ST = {
    return st"""${name}._control_stack_size = ${size};"""
  }
}

object TimerUtil {

  val SEM_DISPATCH: String = s"${Util.GEN_ARTIFACT_PREFIX}_dispatch_sem"

  val TIMER_ID: String = s"${Util.GEN_ARTIFACT_PREFIX}_timer"
  val TIMER_ID_DISPATCHER: String = "timer"

  val TIMER_NOTIFICATION_ID: String = s"${Util.GEN_ARTIFACT_PREFIX}_timer_complete"
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
                     |void tb_thread_calendar() {
                     |  ${(calendars, "\n")}
                     |
                     |  aadl_calendar_counter = (aadl_calendar_counter + 1) % aadl_hyperperiod_subdivisions;
                     |  aadl_calendar_ticks++;
                     |}
                     |
                     |void ${TIMER_NOTIFICATION_DISPATCHER_ID}_callback() {
                     |  tb_thread_calendar();
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

    val compTypeFileName:String = s"${Util.GEN_ARTIFACT_PREFIX}_${DISPATCH_CLASSIFIER}"
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

    val compTypeFileName:String = s"${Util.GEN_ARTIFACT_PREFIX}_${TIMER_INSTANCE}"
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
                            cIncludes: ISZ[Resource])

@datatype class Resource(path: String,
                         contents: ST)

@enum object Dispatch_Protocol {
  'Periodic
  'Sporadic
}