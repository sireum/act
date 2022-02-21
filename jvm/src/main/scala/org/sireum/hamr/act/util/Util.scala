// #Sireum

package org.sireum.hamr.act.util

import org.sireum._
import org.sireum.hamr.act.ast
import org.sireum.hamr.act.ast.{AstBasicComment, AstComment, BinarySemaphore, CommentLocation, Consumes, Dataport, Emits, Mutex, Provides, Semaphore, TODO, Uses}
import org.sireum.hamr.act.proof.ProofContainer.{CAmkESComponentCategory, CAmkESConnectionType}
import org.sireum.hamr.act.proof.ProofUtil
import org.sireum.hamr.act.vm.MetaPort
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.properties.{OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlPort, AadlProcess, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.codegen.common.{CommonUtil, StringUtil}
import org.sireum.hamr.ir
import org.sireum.message.Reporter

object Util {

  val reporter: Reporter = org.sireum.message.Reporter.create

  var verbose: B = T

  val toolName: String = "HAMR Codegen - ACT"

  // special message 'kind' so instruction messages can be filtered
  val ACT_INSTRUCTIONS_MESSAGE_KIND: String = "ACT - Instructions"

  val GEN_ARTIFACT_PREFIX: String = "sb"
  val GEN_ARTIFACT_CAP_PREFIX: String = "SB"

  val MONITOR_COMP_SUFFIX: String = "Monitor"

  val EVENT_NOTIFICATION_TYPE: String = "ReceiveEvent"

  val MONITOR_EVENT_DATA_NOTIFICATION_TYPE: String = "ReceiveEvent"
  val MONITOR_EVENT_NOTIFICATION_TYPE: String = "QueuedData"
  val MONITOR_DATA_NOTIFICATION_TYPE: String = "DataportWrite"

  val MONITOR_INTERFACE_NAME_EVENT: String = "AADLEvent"
  val MONITOR_INTERFACE_NAME_RECEIVER: String = s"${MONITOR_INTERFACE_NAME_EVENT}_Receiver"
  val MONITOR_INTERFACE_NAME_SENDER: String = s"${MONITOR_INTERFACE_NAME_EVENT}_Sender"

  val NOTIFICATION_TYPE: String = "Notification"

  val INTERFACE_PREFIX: String = brand("Monitor")

  val PROP_TB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT: String = "TB_SYS::Compute_Entrypoint_Source_Text"
  val PROP_SB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT: String = "SB_SYS::Compute_Entrypoint_Source_Text"

  val PROP_SB_SYS__CAmkES_Owner_Thread: String = "SB_SYS::CAmkES_Owner_Thread"
  val PROP_TB_SYS__CAmkES_Owner_Thread: String = "TB_SYS::CAmkES_Owner_Thread"

  val DEFAULT_QUEUE_SIZE: Z = z"1"
  val DEFAULT_PRIORITY: Z = z"201"
  val DEFAULT_STACK_SIZE: Z = z"1024"
  val DEFAULT_PERIOD: Z = z"1"

  val DIR_SRC: String = "src"
  //val DIR_INCLUDES: String = "includes"
  val DIR_TYPES: String = "types"
  val DIR_COMPONENTS: String = "components"
  val DIR_INTERFACES: String = "interfaces"
  val DIR_MONITORS: String = brand("Monitors")

  val SBTypeLibrary: String = "SB_Type_Library"
  val SlangTypeLibrary: String = "SlangTypeLibrary"

  val cKeywords: ISZ[String] = ISZ("auto", "break", "case", "char", "const", "continue", "default", "do", "double",
    "else", "enum", "extern", "float", "for", "goto", "if", "int", "long", "register", "return", "short",
    "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while")

  val MISSING_AADL_TYPE: String = "MISSING_AADL_TYPE"

  val HAMR_INCLUDES_NAME: String = "HAMR_INCLUDES"

  val HAMR_LIB_NAME: String = "HAMR_LIB"

  val AUX_CODE_DIRECTORY_NAME: String = "aux_code"

  val camkesStdConnectors: String = "<std_connector.camkes>"
  val camkesGlobalConnectors: String = "<global-connectors.camkes>"

  @pure def brand(s: String): String = {
    return s"${Util.GEN_ARTIFACT_PREFIX}_${s}"
  }

  @pure def cbrand(s: String): String = {
    return s"${Util.GEN_ARTIFACT_CAP_PREFIX}_$s"
  }


  @pure def getSbTypeHeaderFilename(): String = {
    return brand("types")
  }

  @pure def getSbTypeHeaderFilenameWithExtension(): String = {
    return Util.genCHeaderFilename(getSbTypeHeaderFilename())
  }

  @pure def getSbTypeHeaderFilenameForIncludes(): String = {
    return s"<${getSbTypeHeaderFilenameWithExtension()}>"
  }


  @pure def getTypeRootPath(): String = {
    return DIR_TYPES
  }

  @pure def getTypeIncludesPath(): String = {
    return s"${getTypeRootPath()}/includes"
  }

  @pure def getTypeSrcPath(): String = {
    return s"${getTypeRootPath()}/src"
  }


  def getClassifierFullyQualified(c: ir.Classifier): String = {
    val t: String = ActTypeUtil.translateBaseType(c.name) match {
      case Some(v) => v
      case _ => c.name
    }
    return StringUtil.replaceAll(StringUtil.replaceAll(t, "::", "__"), ".", "_")
  }

  def getClassifier(c: ir.Classifier): String = {
    var s = ops.StringOps(c.name)
    val index = s.lastIndexOf(':') + 1
    s = ops.StringOps(s.substring(index, c.name.size))
    return StringUtil.replaceAll(s.s, ".", "_")
  }

  def getShortPath(aadlComponent: AadlComponent): String = {
    val componentName = aadlComponent.component.identifier.name
    assert(ops.StringOps(componentName(0)).endsWith("Instance"))
    val p = ops.ISZOps(componentName).tail
    return st"${(p, "_")}".render
  }

  def getCamkesComponentName(aadlComponent: AadlComponent, symbolTable: SymbolTable): String = {
    val shortPath = getShortPath(aadlComponent)
    val name = s"${Util.getClassifier(aadlComponent.component.classifier.get)}_${shortPath}"

    val ret: String = aadlComponent match {
      case p: AadlProcess =>
        assert(p.toVirtualMachine(symbolTable))
        return s"VM_${name}"
      case t: AadlThread => name
      case _ => halt(s"Unexpected: ${aadlComponent}")
    }
    return ret
  }

  def getCamkesComponentIdentifier(aadlComponent: AadlComponent, symbolTable: SymbolTable): String = {
    val ret: String = aadlComponent match {
      case p: AadlProcess =>
        assert(p.toVirtualMachine(symbolTable))
        s"vm${p.identifier}"
      case t: AadlThread => getShortPath(t)
      case _ => halt(s"Unexpected: ${aadlComponent}")
    }
    return ret
  }

  def getEventPortSendReceiveMethodName(aadlPort: AadlPort): String = {
    val featureName = aadlPort.identifier
    val direction: String = aadlPort.direction match {
      case ir.Direction.In => "dequeue"
      case ir.Direction.Out => "enqueue"
      case x => halt(s"Unexpected direction ${x}")
    }
    return Util.brand(s"${featureName}_${direction}")
  }

  def isMonitor(s: String): B = {
    val ss = ops.StringOps(s)
    return ss.startsWith(GEN_ARTIFACT_PREFIX) && ss.endsWith(MONITOR_COMP_SUFFIX)
  }

  def getMonitorName(comp: ir.Component, feature: ir.Feature): String = {
    val cname = CommonUtil.getLastName(comp.identifier)
    val fname = CommonUtil.getLastName(feature.identifier)
    return brand(s"${cname}_${fname}_${MONITOR_COMP_SUFFIX}")
  }

  def getMonitorNotificationType(t: ir.FeatureCategory.Type): String = {
    val ret: String = t match {
      case ir.FeatureCategory.DataPort => Util.MONITOR_DATA_NOTIFICATION_TYPE
      case ir.FeatureCategory.EventDataPort => Util.MONITOR_EVENT_NOTIFICATION_TYPE
      case _ =>
        halt(s"Not expecting: ${t}")
    }
    return ret
  }

  def genMonitorFeatureName(featureSimpleName: String, num: Option[Z]): String = {
    return brand(s"${featureSimpleName}${if (num.nonEmpty) num.get else ""}")
  }

  def genSeL4NotificationName(featureSimpleName: String, isDataPort: B): String = {
    val name = s"${featureSimpleName}${if (isDataPort) "_notification" else ""}"
    return brand(name)
  }

  def genSeL4NotificationQueueName(featureSimpleName: String, queueSize: Z): String = {
    val name = s"${featureSimpleName}_${queueSize}_notification"
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
    if (hamrIntegration) {
      return "union_art_DataContent"
    } else {
      val name = Util.getClassifierFullyQualified(aadlType.classifier.get)
      if (ActTypeUtil.isArrayDef(aadlType)) {
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

    if (feature.category == ir.FeatureCategory.DataPort) {
      return s"${INTERFACE_PREFIX}_${typeName}"
    } else {
      return s"${INTERFACE_PREFIX}_${typeName}_${PropertyUtil.getQueueSize(feature, Util.DEFAULT_QUEUE_SIZE)}"
    }
  }

  def getInterfaceNameIhor(feature: ir.FeatureEnd, isSender: B): String = {
    val ret: String = if (feature.category == ir.FeatureCategory.EventPort) {
      if (isSender) {
        Util.MONITOR_INTERFACE_NAME_SENDER
      } else {
        Util.MONITOR_INTERFACE_NAME_RECEIVER
      }
    } else {
      val base = getInterfaceName(feature)
      if (isSender) {
        s"${base}_Sender"
      } else {
        s"${base}_Receiver"
      }
    }
    return ret
  }

  def getContainerName(s: String): String = {
    return brand(s"${s}_container")
  }

  def getComputeEntrypointSourceText(properties: ISZ[ir.Property]): Option[String] = {
    val PROP_sb = PROP_SB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT
    val PROP_tb = PROP_TB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT
    val PROP_pp = OsateProperties.PROGRAMMING_PROPERTIES__COMPUTE_ENTRYPOINT_SOURCE_TEXT

    val sbcest = PropertyUtil.getProperty(properties, PROP_sb)
    val tbcest = PropertyUtil.getProperty(properties, PROP_tb)
    val ppcest = PropertyUtil.getProperty(properties, PROP_pp)

    if ((sbcest.nonEmpty && tbcest.nonEmpty) || (sbcest.nonEmpty && ppcest.nonEmpty) || (tbcest.nonEmpty && ppcest.nonEmpty)) {
      val props = st"${PROP_sb}, ${PROP_tb}, ${PROP_pp}"
      reporter.warn(sbcest.get.name.pos, Util.toolName, s"Only one of the following properties should be set for a component: ${props}")
    }

    val ret: Option[String] = if (sbcest.nonEmpty) {
      val values = sbcest.get.propertyValues.map((m: ir.PropertyValue) => m.asInstanceOf[ir.ValueProp])
      assert(values.size > 0)
      if (values.size > 1) {
        reporter.warn(None(), Util.toolName, s"${Util.toolName} only supports a single compute entry point for property ${PROP_sb}")
      }
      Some(values(0).value)
    } else if (tbcest.nonEmpty) {
      val values = tbcest.get.propertyValues.map((m: ir.PropertyValue) => m.asInstanceOf[ir.ValueProp])
      assert(values.size > 0)
      reporter.warn(None(), Util.toolName, s"Property ${PROP_tb} is deprecated, use ${PROP_sb} or ${PROP_pp} instead.")
      if (values.size > 1) {
        reporter.warn(None(), Util.toolName, s"${Util.toolName} only supports a single compute entry point for property ${PROP_tb}")
      }
      Some(values(0).value)
    } else if (ppcest.nonEmpty) {
      val values = ppcest.get.propertyValues.map((m: ir.PropertyValue) => m.asInstanceOf[ir.ValueProp])
      assert(values.size == 1)
      Some(values(0).value)
    } else {
      None()
    }

    return ret
  }

  def getCamkesOwnerThread(p: ISZ[ir.Property]): Option[String] = {
    var ret: Option[String] = PropertyUtil.getDiscreetPropertyValue(p, PROP_SB_SYS__CAmkES_Owner_Thread) match {
      case Some(ir.ValueProp(v)) => Some(v)
      case _ => None[String]()
    }
    if (ret.isEmpty) {
      ret = PropertyUtil.getDiscreetPropertyValue(p, PROP_TB_SYS__CAmkES_Owner_Thread) match {
        case Some(ir.ValueProp(v)) =>
          reporter.warn(None(), Util.toolName, s"Property ${PROP_TB_SYS__CAmkES_Owner_Thread} is deprecated, use ${PROP_SB_SYS__CAmkES_Owner_Thread} instead.")

          Some(v)
        case _ => None[String]()
      }
    }
    return ret
  }

  def relativizePaths(anchorDir: String, toRel: String, pathSep: C, anchorResource: String): String = {
    val ais = conversions.String.toCis(anchorDir)
    val tis = conversions.String.toCis(toRel)

    var commonPrefix = 0
    var stop = F
    while (commonPrefix < ais.size && commonPrefix < tis.size && !stop) {
      if (ais(commonPrefix) == tis(commonPrefix)) {
        commonPrefix = commonPrefix + 1;
      } else {
        stop = T
      }
    }

    if (commonPrefix > 0) {
      var seps = s""
      val offset: Z = if (commonPrefix == ais.size) {
        0
      } else {
        -1
      }

      for (i <- commonPrefix - offset until ais.size) {
        if (ais(i) == pathSep) {
          seps = s"${pathSep}..${seps}"
        }
      }
      val r = ops.StringOps(toRel)
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

    def s(c: ir.Component): Unit = {
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
    return if (f.nonEmpty) Some(f(0)) else None()
  }

  def hamrIntegration(platform: ActPlatform.Type): B = {
    platform match {
      case ActPlatform.SeL4 => return T
      case ActPlatform.SeL4_Only => return F
      case ActPlatform.SeL4_TB => return F
    }
  }

  def genCHeaderFilename(s: String): String = {
    return s"${s}.h"
  }

  def genCImplFilename(s: String): String = {
    return s"${s}.c"
  }

  def getUserEventEntrypointMethodName(component: ir.Component, aadlPort: AadlPort): String = {
    val fid = aadlPort.identifier
    val cid = Util.getClassifier(component.classifier.get)
    return Util.brand(s"entrypoint_${cid}_${fid}")
  }

  def getEventDataSBQueueName(typeName: String, queueSize: Z): String = {
    return brand(s"queue_${typeName}_${queueSize}")
  }

  def getEventData_SB_QueueHeaderFileName(typeName: String, queueSize: Z): String = {
    return s"${Util.genCHeaderFilename(getEventDataSBQueueName(typeName, queueSize))}"
  }

  def getEventData_SB_QueueImplFileName(typeName: String, queueSize: Z): String = {
    return s"${Util.genCImplFilename(getEventDataSBQueueName(typeName, queueSize))}"
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
  val SB_COUNTER_HEADER_FILENAME: String = Util.brand(genCHeaderFilename("event_counter"))

  def sbCounterTypeDeclResource(): Resource = {
    val counter: ST =
      st"""#pragma once
          |
          |#include <stdint.h>
          |
          |typedef _Atomic uintmax_t ${SB_EVENT_COUNTER_TYPE};
          |"""
    ResourceUtil.createResource(s"${Util.getTypeIncludesPath()}/${Util.SB_COUNTER_HEADER_FILENAME}", counter, T)
  }

  def getSbCounterFilenameForIncludes(): String = {
    return s"<${Util.SB_COUNTER_HEADER_FILENAME}>"
  }

  def getConnectionName(index: Z): String = {
    return s"conn${index}"
  }

  def createConnectionEnd(isFrom: B, componentName: String, featureName: String): ast.ConnectionEnd = {
    return ast.ConnectionEnd(isFrom, componentName, featureName, ISZ())
  }

  def createConnections(connectionCategory: CAmkESConnectionType.Type,
                        connectionName: String,
                        connectionType: Sel4ConnectorTypes.Type,
                        fromEnds: ISZ[ast.ConnectionEnd],
                        toEnds: ISZ[ast.ConnectionEnd]): ast.Connection = {
    val ret = ast.Connection(
      name = connectionName,
      connectionType = connectionType.string,
      from_ends = fromEnds,
      to_ends = toEnds,
      comments = ISZ())

    ProofUtil.addCAmkESConnection(connectionCategory, ret)

    return ret
  }

  def createConnection(connectionCategory: CAmkESConnectionType.Type,
                       connectionName: String,
                       connectionType: Sel4ConnectorTypes.Type,
                       srcComponent: String,
                       srcFeature: String,
                       dstComponent: String,
                       dstFeature: String): ast.Connection = {
    val from_ends: ISZ[ast.ConnectionEnd] = ISZ(createConnectionEnd(
      isFrom = T,
      componentName = srcComponent,
      featureName = srcFeature))

    val to_ends: ISZ[ast.ConnectionEnd] = ISZ(createConnectionEnd(
      isFrom = F,
      componentName = dstComponent,
      featureName = dstFeature))

    val con = createConnections(
      connectionCategory = connectionCategory,
      connectionName = connectionName,
      connectionType = connectionType,
      fromEnds = from_ends,
      toEnds = to_ends
    )

    return con
  }

  def createConnectionC(connectionCategory: CAmkESConnectionType.Type,
                        connectionCounter: Counter,
                        connectionType: Sel4ConnectorTypes.Type,
                        srcComponent: String, srcFeature: String,
                        dstComponent: String, dstFeature: String): ast.Connection = {
    val connectionName = Util.getConnectionName(connectionCounter.increment())
    return Util.createConnection(connectionCategory, connectionName, connectionType, srcComponent, srcFeature, dstComponent, dstFeature)
  }

  def getDirectory(path: String): String = {
    val so = ops.StringOps(path)
    val index = so.lastIndexOf('/')
    if (index >= 0) {
      return so.substring(0, index + 1)
    } else {
      return path
    }
  }

  def createCAmkESLibraryComponent(componentCategory: CAmkESComponentCategory.Type,
                                   name: String,
                                   ports: ISZ[String]): ast.LibraryComponent = {
    val ret = ast.LibraryComponent(
      name = name,
      ports = ports,
      comments = ISZ())

    ProofUtil.addCamkesComponent(ret, componentCategory)

    return ret
  }

  def createCAmkESComponent(aadlThread: Option[AadlThread],
                            componentCategory: CAmkESComponentCategory.Type,

                            control: B,
                            hardware: B,
                            name: String,
                            mutexes: ISZ[Mutex],
                            binarySemaphores: ISZ[BinarySemaphore],
                            semaphores: ISZ[Semaphore],
                            dataports: ISZ[Dataport],
                            emits: ISZ[Emits],
                            uses: ISZ[Uses],
                            consumes: ISZ[Consumes],
                            provides: ISZ[Provides],
                            includes: ISZ[String],
                            attributes: ISZ[TODO],
                            imports: ISZ[String],
                            preprocessorIncludes: ISZ[String],
                            externalEntities: ISZ[String]): ast.Component = {
    val ret = ast.Component(
      control = control,
      hardware = hardware,
      name = name,
      mutexes = mutexes,
      binarySemaphores = binarySemaphores,
      semaphores = semaphores,
      dataports = dataports,
      emits = emits,
      uses = uses,
      consumes = consumes,
      provides = provides,
      includes = includes,
      attributes = attributes,
      preprocessorIncludes = preprocessorIncludes,
      imports = imports,
      externalEntities = externalEntities,
      comments = ISZ())

    ProofUtil.addCamkesComponent(ret, componentCategory)

    return ret
  }

  def createCAmkESInstance(originAadl: Option[AadlComponent],

                           address_space: String,
                           name: String,
                           component: ast.CamkesComponent): ast.Instance = {
    val ret = ast.Instance(
      address_space = address_space,
      name = name,
      component = component,
      comments = ISZ())

    ProofUtil.addCAmkESInstance(originAadl, ret)

    return ret
  }

  def createDataport_VMRefinement(aadlComponent: AadlProcess,
                        metaPort: MetaPort,
                        symbolTable: SymbolTable,

                        name: String,
                        optional: B,
                        typ: String): ast.Dataport = {

    val ret: ast.Dataport = ast.Dataport(
      name = name,
      optional = optional,
      typ = typ,
      comments = ISZ())

    ProofUtil.addVMPortRefinement(ret, aadlComponent, metaPort, symbolTable)

    return ret
  }

  def createDataport_Refinement(aadlComponent: AadlComponent,
                                aadlPort: AadlPort,
                                symbolTable: SymbolTable,

                                name: String,
                                optional: B,
                                typ: String): ast.Dataport = {

    val ret: ast.Dataport = ast.Dataport(
      name = name,
      optional = optional,
      typ = typ,
      comments = ISZ())

    ProofUtil.addPortRefinement(ret, aadlComponent, aadlPort, symbolTable)

    return ret
  }

  def createUses_Refinement(aadlComponent: AadlComponent,
                            aadlPort: AadlPort,
                            symbolTable: SymbolTable,

                            name: String,
                            typ: String,
                            optional: B): ast.Uses = {
    val ret: ast.Uses = ast.Uses(
      name = name,
      typ = typ,
      optional = optional,
      comments = ISZ())

    ProofUtil.addPortRefinement(ret, aadlComponent, aadlPort, symbolTable)

    return ret
  }

  def createUses_PeriodicDispatcher(aadlComponent: AadlComponent,
                                    name: String,
                                    typ: String,
                                    optional: B): ast.Uses = {
    val ret = ast.Uses(name = name, typ = typ, optional = optional, comments = ISZ())
    ProofUtil.addPortPeriodicDispatcher(ret)
    return ret
  }

  def createUses_VMAux(aadlProcess: AadlProcess,
                       symbolTable: SymbolTable,
                       name: String,
                       typ: String,
                       optional: B): ast.Uses = {
    val ret = ast.Uses(name = name, typ = typ, optional = optional, comments = ISZ())
    ProofUtil.addVMPortAux(ret, aadlProcess,symbolTable)
    return ret
  }

  def createConsumes_Refinement(aadlComponent: AadlComponent,
                                aadlPort: AadlPort,
                                symbolTable: SymbolTable,

                                name: String,
                                typ: String,
                                optional: B): ast.Consumes = {
    val ret: ast.Consumes = ast.Consumes(
      name = name,
      typ = typ,
      optional = optional,
      comments = ISZ())

    ProofUtil.addPortRefinement(ret, aadlComponent, aadlPort, symbolTable)

    return ret
  }

  def createConsumes_VMRefinement(aadlComponent: AadlProcess,
                                  metaPort: MetaPort,
                                  symbolTable: SymbolTable,

                                  name: String,
                                  typ: String,
                                  optional: B): ast.Consumes = {
    val ret: ast.Consumes = ast.Consumes(
      name = name,
      typ = typ,
      optional = optional,
      comments = ISZ())

    ProofUtil.addVMPortRefinement(ret, aadlComponent, metaPort, symbolTable)

    return ret
  }

  def createConsumes_VMAux(aadlProcess: AadlProcess,
                           symbolTable: SymbolTable,

                           name: String,
                           typ: String,
                           optional: B): ast.Consumes = {
    val ret: ast.Consumes = ast.Consumes(
      name = name,
      typ = typ,
      optional = optional,
      comments = ISZ())

    ProofUtil.addVMPortAux(ret, aadlProcess, symbolTable)

    return ret
  }

  def createConsumes_PeriodicDispatcher(aadlComponent: AadlComponent,

                                        name: String,
                                        typ: String,
                                        optional: B): ast.Consumes = {
    val ret: ast.Consumes = ast.Consumes(
      name = name,
      typ = typ,
      optional = optional,
      comments = ISZ())

    ProofUtil.addPortPeriodicDispatcher(ret)

    return ret
  }

  def createConsumes_SelfPacing(aadlComponent: AadlComponent,
                                symbolTable: SymbolTable,

                                name: String,
                                typ: String,
                                optional: B): ast.Consumes = {
    val ret: ast.Consumes = ast.Consumes(
      name = name,
      typ = typ,
      optional = optional,
      comments = ISZ())

    ProofUtil.addPortSelfPacing(aadlComponent, ret, symbolTable)

    return ret
  }

  def createProvides_Monitor(monitorName: String,
                             name: String,
                             typ: String): ast.Provides = {
    val ret = ast.Provides(
      name = name,
      typ = typ,
      comments = ISZ())

    ProofUtil.addPortMonitor(monitorName, ret)

    return ret
  }

  def createProvides_VMAux(aadlProcess: AadlProcess,
                           symbolTable: SymbolTable,
                           name: String,
                           typ: String): ast.Provides = {
    val ret = ast.Provides(
      name = name,
      typ = typ,
      comments = ISZ())

    ProofUtil.addVMPortAux(ret, aadlProcess, symbolTable)

    return ret
  }

  def createEmits_Refinement(aadlComponent: AadlComponent,
                             aadlPort: AadlPort,
                             symbolTable: SymbolTable,

                             name: String,
                             typ: String): ast.Emits = {
    val ret: ast.Emits = ast.Emits(
      name = name,
      typ = typ,
      comments = ISZ())

    ProofUtil.addPortRefinement(ret, aadlComponent, aadlPort, symbolTable)

    return ret
  }

  def createEmits_VMRefinement(aadlComponent: AadlProcess,
                               metaPort: MetaPort,
                               symbolTable: SymbolTable,

                               name: String,
                               typ: String): ast.Emits = {
    val ret: ast.Emits = ast.Emits(
      name = name,
      typ = typ,
      comments = ISZ())

    ProofUtil.addVMPortRefinement(ret, aadlComponent, metaPort, symbolTable)

    return ret
  }

  def createEmits_VMAux(aadlProcess: AadlProcess,
                        symbolTable: SymbolTable,

                        name: String,
                        typ: String): ast.Emits = {
    val ret: ast.Emits = ast.Emits(
      name = name,
      typ = typ,
      comments = ISZ())

    ProofUtil.addVMPortAux(ret, aadlProcess, symbolTable)

    return ret
  }
  def createEmits_Monitor(monitorName: String,
                          name: String,
                          typ: String): ast.Emits = {
    val ret: ast.Emits = ast.Emits(
      name = name,
      typ = typ,
      comments = ISZ())

    ProofUtil.addPortMonitor(monitorName, ret)

    return ret
  }

  def createEmits_SelfPacing(aadlComponent: AadlComponent,
                             symbolTable: SymbolTable,
                             name: String,
                             typ: String): ast.Emits = {
    val ret = ast.Emits(name = name, typ = typ, comments = ISZ())

    ProofUtil.addPortSelfPacing(aadlComponent, ret, symbolTable)

    return ret
  }



  def createComment(comment: String): AstComment = {
    return AstBasicComment(CommentLocation.PRE, comment)
  }
}