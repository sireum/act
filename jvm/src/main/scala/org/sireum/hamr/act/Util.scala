// #Sireum

package org.sireum.hamr.act

import org.sireum._
import org.sireum.ops._
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Aadl, Component, FeatureEnd, Transformer}
import org.sireum.message.{Position, Reporter}

object Util {

  var reporter: Reporter = Reporter.create
  var verbose: B = T

  val toolName: String = "HAMR Codegen - ACT"

  val DEVELOPER_MODE: B = if(org.sireum.Os.env("ACT_DEVELOPER_MODE").nonEmpty) {
    reporter.warn(None(), Util.toolName, "ACT developer mode enabled")
    T
  } else {
    F
  }

  val GEN_ARTIFACT_PREFIX: String = "sb"
  val GEN_ARTIFACT_CAP_PREFIX: String = "SB"

  val MONITOR_COMP_SUFFIX: String  = "Monitor"

  val EVENT_NOTIFICATION_TYPE: String = "ReceiveEvent"
  
  val MONITOR_EVENT_DATA_NOTIFICATION_TYPE: String =  "ReceiveEvent"
  val MONITOR_EVENT_NOTIFICATION_TYPE: String =  "QueuedData"
  val MONITOR_DATA_NOTIFICATION_TYPE: String  = "DataportWrite"

  val MONITOR_INTERFACE_NAME_EVENT: String = "AADLEvent"
  val MONITOR_INTERFACE_NAME_RECEIVER: String = s"${MONITOR_INTERFACE_NAME_EVENT}_Receiver"
  val MONITOR_INTERFACE_NAME_SENDER: String = s"${MONITOR_INTERFACE_NAME_EVENT}_Sender"

  val NOTIFICATION_TYPE: String = "Notification"

  val INTERFACE_PREFIX: String  = brand("Monitor")


  val PROP_DATA_MODEL__DATA_REPRESENTATION: String = "Data_Model::Data_Representation"
  val PROP_DATA_MODEL__DIMENSION: String = "Data_Model::Dimension"
  val PROP_DATA_MODEL__BASE_TYPE: String = "Data_Model::Base_Type"
  val PROP_DATA_MODEL__ENUMERATORS: String = "Data_Model::Enumerators"

  val PROP_THREAD_PROPERTIES__DISPATCH_PROTOCOL: String = "Thread_Properties::Dispatch_Protocol"
  val PROP_THREAD_PROPERTIES__PRIORITY: String =  "Thread_Properties::Priority"

  val PROP_DEPLOYMENT_PROPERTIES__ACTUAL_PROCESSOR_BINDING: String = "Deployment_Properties::Actual_Processor_Binding"
  val PROP_COMMUNICATION_PROPERTIES__QUEUE_SIZE: String = "Communication_Properties::Queue_Size"

  val PROP_MEMORY_PROPERTIES__STACK_SIZE: String = "Memory_Properties::Stack_Size"

  val PROP_PROGRAMMING_PROPERTIES__INITIALIZE_ENTRYPOINT_SOURCE_TEXT: String = "Programming_Properties::Initialize_Entrypoint_Source_Text"
  val PROP_PROGRAMMING_PROPERTIES__SOURCE_TEXT: String = "Programming_Properties::Source_Text"
  val PROP_PROGRAMMING_PROPERTIES__COMPUTE_ENTRYPOINT_SOURCE_TEXT: String = "Programming_Properties::Compute_Entrypoint_Source_Text"
  
  val PROP_TB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT: String = "TB_SYS::Compute_Entrypoint_Source_Text"
  val PROP_SB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT: String = "SB_SYS::Compute_Entrypoint_Source_Text"
    
  val PROP_SB_SYS__CAmkES_Owner_Thread: String = "SB_SYS::CAmkES_Owner_Thread"
  val PROP_TB_SYS__CAmkES_Owner_Thread: String = "TB_SYS::CAmkES_Owner_Thread"

  val PROP_Timing_Properties__Compute_Execution_Time: String = "Timing_Properties::Compute_Execution_Time"
  val PROP_Timing_Properties__Clock_Period: String = "Timing_Properties::Clock_Period"
  val PROP_Timing_Properties__Frame_Period: String = "Timing_Properties::Frame_Period"
  val PROP_Timing_Properties__Period: String = "Timing_Properties::Period"
  
  val PROP_CASE_Scheduling__Domain: String = "CASE_Scheduling::Domain"
  val PROP_CASE_Scheduling__Schedule_Source_Text: String = "CASE_Scheduling::Schedule_Source_Text"
  
  val DEFAULT_QUEUE_SIZE: Z = z"1"
  val DEFAULT_PRIORITY: Z = z"201"
  val DEFAULT_STACK_SIZE: Z = z"1024"
  val DEFAULT_PERIOD: Z = z"1"

  val DIR_SRC: String = "src"
  val DIR_INCLUDES: String = "includes"
  val DIR_COMPONENTS: String = "components"
  val DIR_INTERFACES: String = "interfaces"
  val DIR_MONITORS: String = brand("Monitors")
  val DIR_SAMPLING_PORTS: String = "sampling_ports"

  val SlangTypeLibrary: String = "SlangTypeLibrary"
  
  val CMAKE_VERSION: String = "3.8.2"

  val cKeywords: ISZ[String] = ISZ("auto", "break", "case", "char", "const", "continue", "default", "do", "double",
    "else", "enum", "extern", "float", "for", "goto", "if", "int", "long", "register", "return", "short",
    "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while")

  val MISSING_AADL_TYPE: String = "MISSING_AADL_TYPE"

  val HAMR_INCLUDES_NAME: String = "HAMR_INCLUDES"

  val HAMR_LIB_NAME: String = "HAMR_LIB"

  val AUX_CODE_DIRECTORY_NAME: String = "aux_code"

  val camkesStdConnectors: String = "<std_connector.camkes>"
  val camkesGlobalConnectors: String = "<global-connectors.camkes>"
  
  def brand(s: String): String = {
    return s"${Util.GEN_ARTIFACT_PREFIX}_${s}"
  }

  def cbrand(s: String): String = {
    return s"${Util.GEN_ARTIFACT_CAP_PREFIX}_$s"
  }

  def nameToString(n: ir.Name): String = {
    return org.sireum.ops.ISZOps(n.name).foldLeft((r: String, s: String) => if(r.size == 0) s else s"${r}_${s}", "")
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
  
  @pure def getProperty(properties: ISZ[ir.Property], propertyName: String): Option[ir.Property] = {
    val op = properties.filter(container => getLastName(container.name) == propertyName)
    val ret: Option[ir.Property] = if(op.nonEmpty) {
      assert(op.size == 1) // sanity check, OSATE doesn't allow properties to be assigned to more than once
      Some(op(0))
    } else {
      None()
    }
    return ret
  }
  
  @pure def getDiscreetPropertyValue(properties: ISZ[ir.Property], propertyName: String): Option[ir.PropertyValue] = {
    val ret: Option[ir.PropertyValue] = getPropertyValues(properties, propertyName) match {
      case ISZ(a) => Some(a)
      case _ => None[ir.PropertyValue]()
    }
    return ret
  }
  
  @pure def getPropertyValues(properties: ISZ[ir.Property], propertyName: String): ISZ[ir.PropertyValue] = {
    return properties.filter(container => getLastName(container.name) == propertyName).flatMap(p => p.propertyValues)
  }

  def getLastName(n : ir.Name) : String = {
    return n.name(n.name.size - 1)
  }

  def getName(n : ir.Name) : String = {
    return st"${(n.name, "_")}".render
  }

  def getEventPortSendReceiveMethodName(feature: FeatureEnd): String = {
    val featureName = Util.getLastName(feature.identifier)
    val direction: String = feature.direction match {
      case ir.Direction.In => "dequeue"
      case ir.Direction.Out => "enqueue"
      case x => halt(s"Unexpected direction ${x}")
    } 
    return Util.brand(s"${featureName}_${direction}")
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

  def genSeL4NotificationName(f: ir.Feature, isDataPort: B): String = {
    val name = s"${Util.getLastName(f.identifier)}${if(isDataPort) "_notification" else "" }"
    return brand(name)
  }

  def genSeL4NotificationQueueName(f: ir.Feature, queueSize: Z): String = {
    val name = s"${Util.getLastName(f.identifier)}_${queueSize}_notification"
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

  def getEventSBNotificationName(featureId: String): String = {
    return brand(s"${featureId}")
  }

  def getSel4TypeName(aadlType: ir.Component, hamrIntegration: B): String = {
    if(hamrIntegration) {
      return "union_art_DataContent"
    } else {
      val name = Util.getClassifierFullyQualified(aadlType.classifier.get)
      if (TypeUtil.isArrayDef(aadlType)) {
        return getContainerName(name)
      } else {
        return name
      }
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

  def getInterfaceNameIhor(feature: ir.FeatureEnd, isSender: B): String = {
    val ret: String = if(feature.category == ir.FeatureCategory.EventPort) {
      if(isSender) {
        Util.MONITOR_INTERFACE_NAME_SENDER
      } else {
        Util.MONITOR_INTERFACE_NAME_RECEIVER
      }
    } else {
      val base = getInterfaceName(feature)
      if(isSender){
        s"${base}_Sender"
      } else {
        s"${base}_Receiver"
      }
    }
    return ret
  }

  def getTypeHeaderFileName(c: ir.Component) : String = {
    /*
    assert(c.category == ir.ComponentCategory.Process)
    val processor: Option[ir.PropertyValue] = 
      getDiscreetPropertyValue(c.properties, PROP_DEPLOYMENT_PROPERTIES__ACTUAL_PROCESSOR_BINDING)
    
    val procName: String = processor match {
      case Some(v : ir.ReferenceProp) => getLastName(v.value)
      case _ => return None[String]()
    }
   */
    val name = Util.getLastName(c.identifier)
    return brand(s"${name}_types")
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

  /* unit conversions consistent with AADL/ISO */
  def getStackSizeInBytes(c: ir.Component): Option[Z] = {
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

  def convertToMS(value: String, unit: Option[String]): Option[Z] = {
    val ret: Option[Z] = R(value) match {
      case Some(v) =>
        val _v = conversions.R.toZ(v)
        val ret: Option[Z] = unit match {
          case Some("ps")  => Some(_v / (z"1000" * z"1000" * z"1000"))
          case Some("ns")  => Some(_v / (z"1000" * z"1000"))
          case Some("us")  => Some(_v / (z"1000"))
          case Some("ms")  => Some(_v)
          case Some("sec") => Some(_v * z"1000")
          case Some("min") => Some(_v * z"1000" * z"60")
          case Some("hr")  => Some(_v * z"1000" * z"60" * z"60")
          case _ => None[Z]()
        }
        ret
      case _ => None()
    }
    return ret
  }
  
  def getPeriod(c: ir.Component): Option[Z] = {
    val ret: Option[Z] = getDiscreetPropertyValue(c.properties, PROP_Timing_Properties__Period) match {
      case Some(ir.UnitProp(z, u)) => convertToMS(z, u)
      case _ => None[Z]()
    }
    return ret
  }

  def getActualProcessorBinding(c: Component): Option[String] = {

    val ret: Option[String] =
      getDiscreetPropertyValue(c.properties, PROP_DEPLOYMENT_PROPERTIES__ACTUAL_PROCESSOR_BINDING) match {
        case Some(v : ir.ReferenceProp) => Some(getName(v.value))
        case _ => return None[String]()
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
    val PROP_sb = PROP_SB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT
    val PROP_tb = PROP_TB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT
    val PROP_pp = PROP_PROGRAMMING_PROPERTIES__COMPUTE_ENTRYPOINT_SOURCE_TEXT
    
    val sbcest = getProperty(properties, PROP_sb)
    val tbcest = getProperty(properties, PROP_tb)
    val ppcest = getProperty(properties, PROP_pp)
    
    if((sbcest.nonEmpty && tbcest.nonEmpty) || (sbcest.nonEmpty && ppcest.nonEmpty) || (tbcest.nonEmpty && ppcest.nonEmpty)){
      val props = st"${PROP_sb}, ${PROP_tb}, ${PROP_pp}"
      reporter.warn(sbcest.get.name.pos, Util.toolName, s"Only one of the following properties should be set for a component: ${props}")
    }

    val ret: Option[String] = if(sbcest.nonEmpty) {
      val values = sbcest.get.propertyValues.map((m: ir.PropertyValue) => m.asInstanceOf[ir.ValueProp])
      assert(values.size > 0)
      if(values.size > 1) {
        reporter.warn(None(), Util.toolName, s"${Util.toolName} only supports a single compute entry point for property ${PROP_sb}")
      }
      Some(values(0).value)
    } else if (tbcest.nonEmpty) {
      val values = tbcest.get.propertyValues.map((m: ir.PropertyValue) => m.asInstanceOf[ir.ValueProp])
      assert(values.size > 0)
      reporter.warn(None(), Util.toolName, s"Property ${PROP_tb} is deprecated, use ${PROP_sb} or ${PROP_pp} instead.")
      if(values.size > 1) {
        reporter.warn(None(), Util.toolName, s"${Util.toolName} only supports a single compute entry point for property ${PROP_tb}")
      }
      Some(values(0).value)
    } else if(ppcest.nonEmpty) {
      val values = ppcest.get.propertyValues.map((m: ir.PropertyValue) => m.asInstanceOf[ir.ValueProp])
      assert(values.size == 1)
      Some(values(0).value)
    } else {
      None()
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
          reporter.warn(None(), Util.toolName, s"Property ${PROP_TB_SYS__CAmkES_Owner_Thread} is deprecated, use ${PROP_SB_SYS__CAmkES_Owner_Thread} instead.")

          Some(v)
        case _ => None[String]()
      }
    }
    return ret
  }

  def getSourceText(properties: ISZ[ir.Property]): ISZ[String] = {
    return getPropertyValues(properties, PROP_PROGRAMMING_PROPERTIES__SOURCE_TEXT).map(p => p.asInstanceOf[ir.ValueProp].value)
  }


  def isThread(f: ir.Component): B = { return f.category == ir.ComponentCategory.Thread}

  def isData(f: ir.Component): B = { return f.category == ir.ComponentCategory.Data }

  def isPeriodic(c: ir.Component): B = { return getDispatchProtocol(c) == Some(Dispatch_Protocol.Periodic) }

  def isSporadic(c: ir.Component): B = { return getDispatchProtocol(c) == Some(Dispatch_Protocol.Sporadic) }
  
  
  def isEventPort(f: ir.Feature): B = {
    return f.category == ir.FeatureCategory.EventPort || f.category == ir.FeatureCategory.EventDataPort
  }

  def isDataPort(f: ir.Feature): B = {
    return f.category == ir.FeatureCategory.DataPort || f.category == ir.FeatureCategory.EventDataPort
  }

  def isDataAccesPort(f: ir.Feature): B = { return f.category == ir.FeatureCategory.DataAccess }
  
  def isSubprogramAccess(f: ir.Feature): B = { return f.category == ir.FeatureCategory.SubprogramAccess }
  
  def isSubprogramAccessGroup(f: ir.Feature): B = { return f.category == ir.FeatureCategory.SubprogramAccessGroup }
  
  def isInPort(f: ir.Feature): B = {
    return (isEventPort(f) || isDataPort(f)) && f.asInstanceOf[FeatureEnd].direction == ir.Direction.In
  }

  def isOutPort(f: ir.Feature): B = {
    return (isEventPort(f) || isDataPort(f)) && f.asInstanceOf[FeatureEnd].direction == ir.Direction.Out
  }

  
  def getInPorts(c: ir.Component): ISZ[ir.FeatureEnd] = {
    return c.features.filter(f => f.isInstanceOf[ir.FeatureEnd] && Util.isInPort(f)).map(f => f.asInstanceOf[FeatureEnd])
  }

  def getOutPorts(c: ir.Component): ISZ[ir.FeatureEnd] = {
    return c.features.filter(f => f.isInstanceOf[ir.FeatureEnd] && Util.isOutPort(f)).map(f => f.asInstanceOf[FeatureEnd])
  }

  def relativizePaths(anchorDir: String, toRel: String, pathSep: C, anchorResource: String) : String = {
    val ais = conversions.String.toCis(anchorDir)
    val tis = conversions.String.toCis(toRel)

    var commonPrefix = 0
    var stop = F
    while(commonPrefix < ais.size && commonPrefix < tis.size && !stop) {
      if(ais(commonPrefix) == tis(commonPrefix)){
        commonPrefix = commonPrefix + 1;
      } else {
        stop = T
      }
    }

    if(commonPrefix > 0) {
      var seps = s""
      val offset: Z = if(commonPrefix == ais.size) { 0 } else { -1 }

      for(i <- commonPrefix - offset until ais.size) {
        if(ais(i) == pathSep) {
          seps = s"${pathSep}..${seps}"
        }
      }
      val r = StringOps(toRel)
      val ret = s"${anchorResource}${seps}${r.substring(commonPrefix - offset, r.size)}"

      /*
      println(st"""anchorDir = ${anchorDir}
                  |toRel =     ${toRel}
                  |ret =       ${ret}""".render)
      */
      return ret
    } else {
      return toRel
    }
  }

  def getComponents(m: ir.Aadl): ISZ[ir.Component] = {
    assert(m.components.size == 1)
    var r: ISZ[ir.Component] = ISZ()
    def s(c : ir.Component): Unit = {
      r = r :+ c
      for (sc <- c.subComponents) {
        s(sc)
      }
    }
    s(m.components(0))
    return r
  }

  def getComponent(m: ir.Aadl, compName: ir.Name): Option[ir.Component] = {
    val f = getComponents(m).filter(p => p.identifier == compName)
    return if(f.nonEmpty) Some(f(0)) else None()
  }

  def getPort(c: ir.Component, portName: ir.Name): Option[ir.FeatureEnd] = {
    for(f <- c.features if(f.isInstanceOf[ir.FeatureEnd] && f.identifier == portName)) {
      return Some(f.asInstanceOf[ir.FeatureEnd])
    }
    return None()
  }

  def hamrIntegration(platform: ActPlatform.Type): B = {
    platform match {
      case ActPlatform.SeL4 => return T
      case ActPlatform.SeL4_Only => return F
      case ActPlatform.SeL4_TB => return F
    }
  }

  def createResource(path: String, contents: ST, overwrite: B): Resource = {
    return Resource(path, contents, overwrite, F)
  }

  def createExeResource(path: String, contents: ST, overwrite: B): Resource = {
    return Resource(path, contents, overwrite, T)
  }


  def getUserEventEntrypointMethodName(component: ir.Component, feature: ir.FeatureEnd): String = {
    val fid = Util.getLastName(feature.identifier)
    val cid = Util.getClassifier(component.classifier.get)
    return Util.brand(s"entrypoint_${cid}_${fid}")
  }
  
  def getEventDataSBQueueName(typeName: String, queueSize: Z): String = {
    return brand(s"queue_${typeName}_${queueSize}")
  }

  def getEventData_SB_QueueHeaderFileName(typeName: String, queueSize: Z): String = {
    return s"${getEventDataSBQueueName(typeName, queueSize)}.h"
  }

  def getEventData_SB_QueueImplFileName(typeName: String, queueSize: Z): String = {
    return s"${getEventDataSBQueueName(typeName, queueSize)}.c"
  }

  def getEventDataSBQueueTypeName(typeName: String, queueSize: Z): String = {
    return s"${getEventDataSBQueueName(typeName, queueSize)}_t"
  }

  def getEventData_SB_RecvQueueName(typeName: String, queueSize: Z): String = {
    return s"${getEventDataSBQueueName(typeName, queueSize)}_Recv"
  }
  def getEventData_SB_RecvQueueTypeName(typeName: String, queueSize: Z): String = {
    return s"${getEventData_SB_RecvQueueName(typeName, queueSize)}_t"
  }
  def getEventData_SB_RecvQueueFeatureName(featureId: String): String = {
    return brand(s"${featureId}_recv_queue")
  }

  def getEventDataSBQueueSrcFeatureName(featureId: String, queueSize: Z): String = {
    return brand(s"${featureId}_queue_${queueSize}")
  }

  def getEventDataSBQueueDestFeatureName(featureId: String): String = {
    return brand(s"${featureId}_queue")
  }
  
  
  def getEventSBCounterName(featureId: String): String = {
    return brand(s"${featureId}_counter")
  }


  val SB_EVENT_COUNTER_TYPE: String = Util.brand("event_counter_t")
  val SB_COUNTER_FILENAME: String = Util.brand("event_counter.h")

  def sbCounterTypeDeclResource(): Resource = {
    val counter: ST =st"""#pragma once
                         |
                         |#include <stdint.h>
                         |
                         |typedef _Atomic uintmax_t ${SB_EVENT_COUNTER_TYPE}; 
                         |"""
    Util.createResource(s"${Util.DIR_INCLUDES}/${Util.SB_COUNTER_FILENAME}", counter, T)
  }
  
  def sbCounterInclude(): ST = {
    return st"#include <${Util.SB_COUNTER_FILENAME}>"
  }

  def getConnectionName(index: Z): String = { return s"conn${index}"}
  
  def createConnection(connectionName: String, 
                       connectionType: Sel4ConnectorTypes.Type,
                       srcComponent: String, srcFeature: String,
                       dstComponent: String, dstFeature: String): ast.Connection = {
    val from_ends: ISZ[ast.ConnectionEnd] = ISZ(ast.ConnectionEnd(
      isFrom = T,
      component = srcComponent,
      end = srcFeature))

    val to_ends: ISZ[ast.ConnectionEnd] = ISZ(ast.ConnectionEnd(
      isFrom = F,
      component = dstComponent,
      end = dstFeature))

    val con = ast.Connection(
      name = connectionName,
      connectionType = s"$connectionType",
      from_ends = from_ends,
      to_ends = to_ends
    )

    return con
  }

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
        Util.reporter.error(None(), Util.toolName, "Unbounded Base_Types::Integer is not supported")
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

  def toUpperCase(s: String):String = {
    val cms = conversions.String.toCms(s)
    return conversions.String.fromCms(cms.map((c: C) => COps(c).toUpper))
  }
  
  def sanitizeName(s: String): String = {
    return replaceAll(replaceAll(s, "-", "_"), ".", "_")
  }
}

@datatype class ActContainer(rootServer: String,
                             connectors: ISZ[ast.Connector],
                             models: ISZ[ast.ASTObject],
                             monitors: ISZ[Monitor],
                             samplingPorts: ISZ[SamplingPortInterface],
                             cContainers: ISZ[C_Container],
                             auxFiles: ISZ[Resource],
                             globalImports: ISZ[String],
                             requiresTimeServer: B
                            )

@sig trait Monitor {
  def i: ast.Instance
  def cimplementation: Resource
  def cinclude: Resource
  def index: Z
  def ci: ir.ConnectionInstance
}

@datatype class TB_Monitor (i: ast.Instance,           // camkes monitor
                            interface: ast.Procedure,  // camkes interface
                            providesVarName: String,
                            cimplementation: Resource,
                            cinclude: Resource,
                            index: Z,                  // fan-out index
                            ci: ir.ConnectionInstance  // aadl connection
                           ) extends Monitor

@datatype class Ihor_Monitor (i: ast.Instance,           // camkes monitor
                              interfaceReceiver: ast.Procedure,  // camkes interface
                              interfaceSender: ast.Procedure,  // camkes interface
                              providesReceiverVarName: String,
                              providesSenderVarName: String,
                              cimplementation: Resource,
                              cinclude: Resource,
                              index: Z,                  // fan-out index
                              ci: ir.ConnectionInstance // aadl connection 
                             ) extends Monitor

@datatype class C_Container(instanceName: String,
                            componentId: String,
                            cSources: ISZ[Resource],
                            cIncludes: ISZ[Resource],
                            sourceText: ISZ[String],
                            externalCSources: ISZ[String],
                            externalCIncludeDirs: ISZ[String])

@datatype class C_SimpleContainer(cIncludes: ISZ[ST],
                                  cInterface: Option[ST],
                                  cImplementation: Option[ST],
                                  preInits: Option[ST],
                                  postInits: Option[ST],
                                  drainQueues: Option[(ST, ST)])

@datatype class CamkesAssemblyContribution(imports: ISZ[String],
                                           instances: ISZ[ast.Instance],
                                           connections: ISZ[ast.Connection],
                                           configurations: ISZ[ST],
                                           cContainers: ISZ[C_Container],
                                           auxResourceFiles: ISZ[Resource])

@datatype class CamkesComponentContributions(shell: ast.Component)

@datatype class CamkesGlueCodeContributions(header: CamkesGlueCodeHeaderContributions,
                                            impl: CamkesGlueCodeImplContributions)

@datatype class CamkesGlueCodeHeaderContributions(includes: ISZ[String],
                                                  methods: ISZ[ST])

@datatype class CamkesGlueCodeImplContributions(includes: ISZ[String],
                                                globals: ISZ[ST],
                                                         
                                                methods: ISZ[ST],
                                                        
                                                preInitStatements: ISZ[ST],
                                                postInitStatements: ISZ[ST],
                                                         
                                                mainPreLoopStatements: ISZ[ST],
                                                mainLoopStatements: ISZ[ST])

@enum object Dispatch_Protocol {
  'Periodic
  'Sporadic
}

@datatype class SamplingPortInterface(name: String,
                                      structName: String,
                                      sel4TypeName: String,
                                      headerPath: String,
                                      implPath: String) 

@datatype class SharedData(owner: ir.Component,
                           ownerFeature: Option[ir.FeatureAccess],
                           typ: ir.Classifier,
                           subcomponentId: String)

@datatype class QueueObject(queueName: String,
                            queueSize: Z)

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

    override def postClassifier(ctx: B, o: ir.Classifier): ir.Transformer.TPostResult[B, ir.Classifier] = {
      if(o.name == unboundInt) {
        Util.reporter.warn(None(), Util.toolName, s"Replacing classifier ${unboundInt} with ${int32}")
        return ir.Transformer.TPostResult(T, Some(ir.Classifier(int32)))
      } else {
        return ir.Transformer.TPostResult(ctx, None())
      }
    }

    override def postClassifierProp(ctx: B, o: ir.ClassifierProp): ir.Transformer.TPostResult[B, ir.PropertyValue] = {
      if(o.name == unboundInt) {
        Util.reporter.warn(None(), Util.toolName, s"Replacing classifier ${unboundInt} with ${int32}")
        return ir.Transformer.TPostResult(T, Some(ir.ClassifierProp(int32)))
      } else {
        return ir.Transformer.TPostResult(ctx, None())
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
      propertyValues = ISZ(ir.ClassifierProp(Util.MISSING_AADL_TYPE)),
      appliesTo = ISZ())

    val sporadicProp: ir.Property = ir.Property(
      name = ir.Name(ISZ(Util.PROP_THREAD_PROPERTIES__DISPATCH_PROTOCOL), None()),
      propertyValues = ISZ(ir.ValueProp("Sporadic")),
      appliesTo = ISZ())


    override def postAadl(ctx: CTX, o: Aadl): Transformer.TPostResult[CTX, Aadl] = {
      if(ctx.requiresMissingType) {
        ir.Transformer.TPostResult(ctx, Some(o(dataComponents = o.dataComponents :+ missingType)))
      } else {
        ir.Transformer.TPostResult(ctx, None[ir.Aadl]())
      }
    }

    override def postComponent(ctx: CTX, o: Component): Transformer.TPostResult[CTX, Component] = {

      o.category match {
        case ir.ComponentCategory.Data =>
          if(o.classifier.isEmpty) {
            Util.reporter.warn(None(), Util.toolName, s"Classifier not specified for ${Util.getName(o.identifier)}.  Substituting ${Util.MISSING_AADL_TYPE}")

            ir.Transformer.TPostResult(ctx(requiresMissingType = T), Some(o(classifier = Some(ir.Classifier(Util.MISSING_AADL_TYPE)))))
          } else if (TypeUtil.isArrayDef(o) && TypeUtil.getArrayBaseType(o).isEmpty) {
            Util.reporter.warn(None(), Util.toolName, s"Base type not specified for ${o.classifier.get.name}.  Substituting ${Util.MISSING_AADL_TYPE}")

            ir.Transformer.TPostResult(ctx(requiresMissingType = T), Some(o(properties = o.properties :+ missingArrayBaseType)))
          } else {
            ir.Transformer.TPostResult(ctx, None[ir.Component]())
          }

        case ir.ComponentCategory.Thread =>
          Util.getDiscreetPropertyValue(o.properties, Util.PROP_THREAD_PROPERTIES__DISPATCH_PROTOCOL) match {
            case Some(ir.ValueProp(x)) =>
              if(x != "Periodic" && x != "Sporadic") {
                Util.reporter.error(None(), Util.toolName, s"${o.classifier.get.name} has unsupported dispatch protocol ${x}.")

                ir.Transformer.TPostResult(ctx(hasErrors = T), None[ir.Component]())
              } else {
                ir.Transformer.TPostResult(ctx, None[ir.Component]())
              }
            case _ =>
              Util.reporter.warn(None(), Util.toolName, s"${Util.PROP_THREAD_PROPERTIES__DISPATCH_PROTOCOL} not specified for thread ${o.classifier.get.name}.  Treating it as sporadic.")

              ir.Transformer.TPostResult(ctx, Some(o(properties =  o.properties :+ sporadicProp)))
          }
        case _ => ir.Transformer.TPostResult(ctx, None[ir.Component]())
      }
    }

    override def postFeatureEnd(ctx: CTX, o: FeatureEnd): Transformer.TPostResult[CTX, FeatureEnd] = {
      if (Util.isDataPort(o) && o.classifier.isEmpty) {
        Util.reporter.warn(None(), Util.toolName, s"No datatype specified for data port ${Util.getName(o.identifier)}.  Substituting ${Util.MISSING_AADL_TYPE} ")

        ir.Transformer.TPostResult(ctx(requiresMissingType = T), Some(o(classifier = Some(ir.Classifier(Util.MISSING_AADL_TYPE)))))
      } else {
        ir.Transformer.TPostResult(ctx, None[ir.FeatureEnd]())
      }
    }
  }
}

@datatype class Resource(path: String,
                         content: ST,
                         overwrite: B,
                         makeExecutable: B)

@datatype class ActResult(val resources: ISZ[Resource])

@enum object ReportKind{
  'Info
  'Warning
  'Error
}

@enum object ActPlatform {
  'SeL4
  'SeL4_Only
  'SeL4_TB
}

@datatype class HamrLib(instanceName: String,
                        includeDirs: ISZ[String],
                        staticLib: String)

@datatype class ActOptions(outputDir: String,
                           auxFiles: Map[String, String],
                           aadlRootDirectory: Option[String],
                           platform: ActPlatform.Type,
                           hamrLibs: Map[String, HamrLib],
                           hamrBasePackageName: Option[String])

@datatype class Names(c: ir.Component,
                      basePackage: String) {
  val split: ISZ[String] = {
    val san = StringUtil.replaceAll(c.classifier.get.name, "::", ":")
    ops.StringOps(san).split(char => char == ':')
  }
    
  def component: String = { return StringUtil.sanitizeName(split(1)) }
  
  def componentImpl: String = { return s"${component}_Impl"  }

  def bridge: String = { return s"${component}_Bridge" }
  
  def aadlPackage: String = { return split(0) }

  def packageName: String = { return s"${basePackage}.${aadlPackage}" }

  def packagePath: String = { return s"${basePackage}/${aadlPackage}" }

  def path: ISZ[String] = { return ISZ(basePackage, aadlPackage) }
  
  def identifier: String = { return Util.getLastName(c.identifier) }
  
  def instanceName: String = { return Util.getName(c.identifier) }  
  
  
  def cPackageName: String = { return st"${(path, "_")}".render }
  
  def cEntryPointAdapterName: String = { return s"${component}_adapter" }
  
  def cEntryPointAdapterQualifiedName: String = { return s"${cPackageName}_${cEntryPointAdapterName}" }


  def sel4SlangExtensionName: String = { return s"${component}_seL4Nix" }
  
  def sel4SlangExtensionQualifiedNameC: String = { return s"${cPackageName}_${sel4SlangExtensionName}" }
}

@record class Counter() {
  var count: Z = 0 // start at 0 so first is 1 which prevents capability conflict issues

  def increment(): Z = {
    count = count + 1
    return count
  }
}