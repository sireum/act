// #Sireum

package org.sireum.hamr.act

import org.sireum._
import org.sireum.ops.ISZOps
import org.sireum.hamr.act.ast._
import org.sireum.hamr.act.ast.{ASTObject, BinarySemaphore, Component, Composition, Connection, ConnectionEnd, ConnectorType, Consumes, Dataport, Direction, Emits, Instance, Method, Parameter, Procedure, Provides, Semaphore, Uses}
import org.sireum.hamr.act.cakeml.CakeML
import org.sireum.hamr.act.periodic.{Dispatcher, PeriodicDispatcher, PeriodicUtil}
import org.sireum.hamr.act.templates.{CMakeTemplate, EventDataQueueTemplate}
import org.sireum.hamr.act.utils.PathUtil
import org.sireum.hamr.act.vm.{VMGen, VM_Template}
import org.sireum.hamr.codegen.common.{CommonUtil, Names, StringUtil}
import org.sireum.hamr.codegen.common.properties.{OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Aadl, FeatureEnd}
import org.sireum.message.Reporter
import org.sireum.hamr.codegen.common.types.{TypeUtil => CommonTypeUtil}

@record class Gen(model: Aadl,
                  symbolTable: SymbolTable,
                  aadlTypes: AadlTypes,
                  actOptions: ActOptions,
                  reporter: Reporter) {

  val platform: ActPlatform.Type = actOptions.platform
  val hamrBasePackageName: Option[String] = actOptions.hamrBasePackageName
  
  var typeMap: HashSMap[String, ir.Component] = HashSMap.empty
  
  var sharedData: HashMap[String, SharedData] = HashMap.empty
  var samplingPorts: HashMap[String, SamplingPortInterface] = HashMap.empty
  var srcQueues: Map[String, Map[String, QueueObject]] = Map.empty // {dstFeatureId -> {srcFeatureId -> queueobj}}
  
  var connectors: ISZ[ast.Connector] = ISZ()
  var astObjects: ISZ[ASTObject] = ISZ()
  var monitors: HashSMap[String, Monitor] = HashSMap.empty // conn instname -> monitor
  var containers: ISZ[C_Container] = ISZ()
  var auxResourceFiles: ISZ[Resource] = ISZ()

  var camkesComponents: ISZ[ast.Component] = ISZ()
  var camkesConnections: ISZ[ast.Connection] = ISZ()

  var camkesConfiguration: ISZ[ST] = ISZ()
  var camkesConfigurationMacros: Set[String] = Set.empty // same idea, just can't end with a semi-colon

  var auxCImplIncludes: ISZ[ST] = ISZ()

  var globalPreprocessorIncludes: Set[String] = Set.empty[String]
  var globalImports: Set[String] = Set.empty[String] + Util.camkesStdConnectors
  var settingsCmakeEntries: ISZ[ST] = ISZ()
  
  val preventBadging: B = T
  val performHamrIntegration: B = Util.hamrIntegration(platform)
  val hookupPeriodicComponentsToTimeServer: B = F
  
  val connectionCounter: Counter = Counter()
  val timerAttributeCounter: Counter = Counter()

  def hasErrors(): B = {
    return reporter.hasError
  }

  def process(cSources: ISZ[String]) : (Option[ActContainer], Reporter) = {

    auxCImplIncludes = cSources.map(c => st"""#include "../../../${c}"""")

    for(d <- model.dataComponents){ typeMap = typeMap + (Util.getClassifierFullyQualified(d.classifier.get) ~> d) }
    val sortedData = sortData(typeMap.values)

    buildTransportMechanisms(symbolTable.rootSystem.component)

    if(!hasErrors()) {
      auxResourceFiles = auxResourceFiles :+
        Util.createResource(
          path = s"${Util.getTypeIncludesPath()}/${Util.getSbTypeHeaderFilenameWithExtension()}",
          contents = processDataTypes(sortedData),
          overwrite = T)
    }

    if(!hasErrors()) {
      gen(symbolTable.rootSystem.component)
    }

    if(!hasErrors()) {
      // merge assemblies
      val assemblies: ISZ[Assembly] = astObjects.filter(f => f.isInstanceOf[Assembly]).map(m => m.asInstanceOf[Assembly])
      val otherAstObjects: ISZ[ASTObject] =  astObjects.filter(f => !f.isInstanceOf[Assembly])

      val threadsToVMs = symbolTable.getThreads().filter((p: AadlThread) => p.toVirtualMachine(symbolTable))
      if(threadsToVMs.nonEmpty) {
        auxResourceFiles = auxResourceFiles ++ VMGen.getAuxResources(threadsToVMs, platform, symbolTable)
      }

      val mergedAssemblies: ISZ[Assembly] = VMGen.mergeVMs(assemblies)

      var instances: Set[Instance] = Set.empty
      var connections: Set[Connection] = Set.empty
      var compositionMacros: Set[String] = Set.empty

      for(assembly <- mergedAssemblies) {
        instances = instances ++ assembly.composition.instances
        connections = connections ++ assembly.composition.connections
        compositionMacros = compositionMacros ++ assembly.composition.externalEntities
      }

      connections = connections ++ camkesConnections

      {
        val periodicAssemblyContributions: CamkesAssemblyContribution =
          Dispatcher.handlePeriodicComponents(
            symbolTable, actOptions, connectionCounter, timerAttributeCounter,
            Util.getSbTypeHeaderFilenameForIncludes(), reporter)

        globalImports = globalImports ++ periodicAssemblyContributions.imports
        instances = instances ++ periodicAssemblyContributions.instances
        connections = connections ++ periodicAssemblyContributions.connections
        camkesConfiguration = camkesConfiguration ++ periodicAssemblyContributions.configurations
        containers = containers ++ periodicAssemblyContributions.cContainers
        settingsCmakeEntries = settingsCmakeEntries ++ periodicAssemblyContributions.settingCmakeEntries
        auxResourceFiles = auxResourceFiles ++ periodicAssemblyContributions.auxResourceFiles
      }

      auxResourceFiles = auxResourceFiles :+
        Util.createResource("settings.cmake", CMakeTemplate.genSettingsCmake(settingsCmakeEntries), F)

      val composition = Composition(
        groups = ISZ(),
        exports = ISZ(),
        instances = instances.elements,
        connections = connections.elements,
        externalEntities = compositionMacros.elements
      )
      
      astObjects = ISZ(Assembly(
        configuration = (Set.empty[String] ++ camkesConfiguration.map((m : ST) => m.render)).elements,
        configurationMacros = camkesConfigurationMacros.elements,
        composition = composition))
      
      astObjects = astObjects ++ otherAstObjects

      { // generate sb type library cmake
        val prefix: String = s"${Util.getTypeRootPath()}/"

        val typeSrcs: ISZ[Resource] = auxResourceFiles.filter(p => ops.StringOps(p.path).startsWith(Util.getTypeSrcPath()))
        var filenames: ISZ[String] = typeSrcs.map(m => {
          assert(ops.StringOps(m.path).startsWith(prefix), s"Unexpected type path ${m.path}")
          ops.StringOps(m.path).substring(prefix.size, m.path.size)
        })

        val spis: ISZ[String] = samplingPorts.values.map((m: SamplingPortInterface) => {
          assert(ops.StringOps(m.implPath).startsWith(prefix), s"Unexpected type path ${m.implPath}")
          ops.StringOps(m.implPath).substring(prefix.size, m.implPath.size)
        })

        filenames = filenames ++ spis

        val contents: ST = CMakeTemplate.cmake_generateTypeCmakeLists(filenames)

        auxResourceFiles = auxResourceFiles :+
          Util.createResource(s"${Util.getTypeRootPath()}/CMakeLists.txt", contents, T)

      } // end sb type library

      return (Some(ActContainer(
        rootServer = CommonUtil.getLastName(symbolTable.rootSystem.component.identifier),
        connectors = connectors,
        models = astObjects,
        monitors = monitors.values,
        samplingPorts = samplingPorts.values,
        cContainers = containers,
        auxFiles = auxResourceFiles,
        globalImports = globalImports.elements,
        globalPreprocessorIncludes = globalPreprocessorIncludes.elements,
        requiresTimeServer = PeriodicUtil.requiresTimeServer(symbolTable, actOptions.platform))), reporter)

    } else {
      return ((None[ActContainer](), reporter))
    }
  }

  def gen(c: ir.Component) : Unit = {
    c.category match {
      case ir.ComponentCategory.System =>
        for (sc <- c.subComponents) {
          gen(sc)
        }
        camkesConnections = camkesConnections ++ processConnections(c)

      case ir.ComponentCategory.Process =>
        val aadlProcess = symbolTable.getProcess(CommonUtil.getName(c.identifier))

        if(aadlProcess.toVirtualMachine()) {
          val threads = aadlProcess.subComponents.filter(c => CommonUtil.isThread(c.component))

          // sanity check: currently expecting exactly one thread pre process when virtualizing
          // TODO: move this to common sym resolver?
          assert(threads.size == 1)
        }

        val g: Composition = genProcess(aadlProcess)

        astObjects = astObjects :+ Assembly(
          configuration = ISZ(),
          configurationMacros = ISZ(),
          composition = g)

      case ir.ComponentCategory.Processor =>        // ignore for now?
      case ir.ComponentCategory.Bus =>              // ignore for now?
      case ir.ComponentCategory.Device =>           // ignore for now?
      case ir.ComponentCategory.Memory =>           // ignore for now?
      case ir.ComponentCategory.VirtualProcessor => // ignore for now?

      case x => halt(s"Not currently processing ${x}")
    }
  }

  def createConnection(connectionType: Sel4ConnectorTypes.Type,
                       srcComponent: String, srcFeature: String,
                       dstComponent: String, dstFeature: String): ast.Connection = {
    val name = Util.getConnectionName(connectionCounter.increment())
    return Util.createConnection(name, connectionType, srcComponent, srcFeature, dstComponent, dstFeature)
  }

  def processConnections(c: ir.Component): ISZ[ast.Connection] = {

    def createSeL4GlobalAsynchConnection(srcComponent: String, srcFeature: String,
                                         dstComponent: String, dstFeature: String): ast.Connection = {
      return createConnection(
        Sel4ConnectorTypes.seL4GlobalAsynch,
        srcComponent, srcFeature,
        dstComponent, dstFeature)
    }

    def createSeL4NotificationConnection(srcComponent: String, srcFeature: String,
                                         dstComponent: String, dstFeature: String): ast.Connection = {
      return createConnection(
        Sel4ConnectorTypes.seL4Notification,
        srcComponent, srcFeature,
        dstComponent, dstFeature)
    }

    def createRPCConnection(srcComponent: String, srcFeature: String,
                            dstComponent: String, dstFeature: String) : ast.Connection = {
      return createConnection(
        Sel4ConnectorTypes.seL4RPCCall,
        srcComponent, srcFeature,
        dstComponent, dstFeature)
    }

    def createSharedDataCounterConnection(conn: ir.ConnectionInstance) : ast.Connection = {
      val srcComponent = CommonUtil.getLastName(conn.src.component)
      val srcFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.src.feature.get)).get

      val dstComponent = CommonUtil.getLastName(conn.dst.component)
      val dstFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.dst.feature.get)).get

      val srcFeatureName = Util.getEventSBCounterName(CommonUtil.getLastName(srcFeature.identifier))
      val dstFeatureName = Util.getEventSBCounterName(CommonUtil.getLastName(dstFeature.identifier))

      camkesConfiguration = camkesConfiguration :+ st"""${srcComponent}.${srcFeatureName}_access = "W";"""
      camkesConfiguration = camkesConfiguration :+ st"""${dstComponent}.${dstFeatureName}_access = "R";"""

      return createConnection(Sel4ConnectorTypes.seL4SharedData,
        srcComponent, srcFeatureName,
        dstComponent, dstFeatureName
      )
    }

    def createSharedDataConnection(conn: ir.ConnectionInstance) : ast.Connection = {
      val srcComponent = CommonUtil.getLastName(conn.src.component)
      val srcFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.src.feature.get)).get

      val dstComponent = CommonUtil.getLastName(conn.dst.component)
      val dstFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.dst.feature.get)).get

      val srcFeatureName = Util.brand(CommonUtil.getLastName(srcFeature.identifier))
      val dstFeatureName = Util.brand(CommonUtil.getLastName(dstFeature.identifier))

      return createConnection(Sel4ConnectorTypes.seL4SharedData,
        srcComponent, srcFeatureName,
        dstComponent, dstFeatureName
      )
    }

    def createDataConnection(conn: ir.ConnectionInstance) : ISZ[ast.Connection] = {
      val monitor = getMonitorForConnectionInstance(conn).get.asInstanceOf[TB_Monitor]

      val srcComponent = CommonUtil.getLastName(conn.src.component)
      val dstComponent = CommonUtil.getLastName(conn.dst.component)

      val srcFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.src.feature.get)).get
      val dstFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.dst.feature.get)).get

      val srcFeatureName = Util.genMonitorFeatureName(srcFeature, Some(monitor.index))
      val dstFeatureName = Util.genMonitorFeatureName(dstFeature, None[Z]())

      var ret: ISZ[ast.Connection] = ISZ()

      // rpc src to mon
      ret = ret :+ createRPCConnection(srcComponent, srcFeatureName, monitor.i.name, monitor.providesVarName)

      // rpc mon to dst
      ret = ret :+ createRPCConnection(dstComponent, dstFeatureName, monitor.i.name, monitor.providesVarName)

      // notification monsig to dst
      return ret :+ createSeL4NotificationConnection(
        monitor.i.name, "monsig",
        dstComponent, Util.genSeL4NotificationName(dstFeature, T)
      )
    }

    def createDataConnection_Ihor(conn: ir.ConnectionInstance) :ISZ[ast.Connection] = {
      val monitor = getMonitorForConnectionInstance(conn).get.asInstanceOf[Ihor_Monitor]

      val srcComponent = CommonUtil.getLastName(conn.src.component)
      val dstComponent = CommonUtil.getLastName(conn.dst.component)

      val srcFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.src.feature.get)).get
      val dstFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.dst.feature.get)).get

      val srcFeatureName = Util.genMonitorFeatureName(srcFeature, Some(monitor.index))
      val dstFeatureName = Util.genMonitorFeatureName(dstFeature, None[Z]())

      var ret: ISZ[ast.Connection] = ISZ()

      // rpc src to mon
      ret = ret :+ createRPCConnection(srcComponent, srcFeatureName, monitor.i.name, monitor.providesSenderVarName)

      // rpc mon to dst
      ret = ret :+ createRPCConnection(dstComponent, dstFeatureName, monitor.i.name, monitor.providesReceiverVarName)

      // notification monsig to dst
      return ret :+ createSeL4NotificationConnection(
        monitor.i.name, "monsig",
        dstComponent, Util.genSeL4NotificationName(dstFeature, T)
      )
    }

    var connections: ISZ[ast.Connection] = ISZ()

    val handledConns = c.connectionInstances.filter(conn => isHandledConnection(conn))
    val unhandledConns = c.connectionInstances.filter(conn => !isHandledConnection(conn))

    resolveSharedDataFeatures(handledConns)
    val missingFeatures = sharedData.values.filter((f: SharedData) => f.ownerFeature.isEmpty)
    if(missingFeatures.nonEmpty) {
      reporter.error(None(), Util.toolName, s"Could not find the owner for the following data subcomponents: ${(missingFeatures.map((f: SharedData) => f.subcomponentId), ", ")}")
    }

    for(conn <- handledConns) {
      val dstPath: String = CommonUtil.getName(conn.dst.feature.get)
      val fdst: ir.Feature = symbolTable.airFeatureMap.get(dstPath).get

      val srcComponentName: String = CommonUtil.getLastName(conn.src.component)

      val dstComponentName: String = CommonUtil.getLastName(conn.dst.component)
      val dstFeatureName: String = CommonUtil.getLastName(conn.dst.feature.get)

      conn.kind match {
        case ir.ConnectionKind.Port =>
          fdst.category match {

            case ir.FeatureCategory.DataPort => {

              def handleDataPort_TB_Connection(): Unit = {
                connections = connections ++ createDataConnection(conn)
              }

              def handleDataPort_SB_Connection(): Unit = {
                val srcAadlThread: AadlThread = symbolTable.getThreadByName(conn.src.component)
                val dstAadlThread: AadlThread = symbolTable.getThreadByName(conn.dst.component)

                val srcToVM = srcAadlThread.toVirtualMachine(symbolTable)
                val dstToVM = dstAadlThread.toVirtualMachine(symbolTable)

                val srcComponentId: String = Util.getThreadIdentifier(srcAadlThread, symbolTable)
                val dstComponentId: String = Util.getThreadIdentifier(dstAadlThread, symbolTable)

                val srcFeature: ir.Feature = symbolTable.getFeatureFromName(conn.src.feature.get)
                val dstFeature: ir.Feature = symbolTable.getFeatureFromName(conn.dst.feature.get)

                { // dataport connection
                  val queueConnectorType: Sel4ConnectorTypes.Type =
                    if(srcToVM || dstToVM) Sel4ConnectorTypes.seL4SharedDataWithCaps
                    else Sel4ConnectorTypes.seL4SharedData

                  val srcFeatureQueueName: String = Util.brand(CommonUtil.getLastName(srcFeature.identifier))

                  val dstFeatureQueueName: String = Util.brand(CommonUtil.getLastName(dstFeature.identifier))

                  connections = connections :+ createConnection(
                    queueConnectorType,
                    srcComponentId, srcFeatureQueueName,
                    dstComponentId, dstFeatureQueueName
                  )

                  if (aadlTypes.rawConnections) {
                    val size: Z = CommonTypeUtil.getMaxBitsSize(aadlTypes) match {
                      case Some(z) =>
                        if (z % z"4096" == z"0") z
                        else (z / z"4096" + 1) * z"4096"
                      case _ => z"4096" // TODO or throw error?
                    }

                    val connectionName = Util.getConnectionName(connectionCounter.count)
                    camkesConfiguration = camkesConfiguration :+
                      st"""${connectionName}.size = ${size};"""
                  }
                }
              }

              platform match {
                case ActPlatform.SeL4_TB => handleDataPort_TB_Connection()
                case ActPlatform.SeL4_Only => handleDataPort_SB_Connection()
                case ActPlatform.SeL4 => handleDataPort_SB_Connection()
              }
            }

            case ir.FeatureCategory.EventDataPort => {

              def eventDataPort_SB_Connection_Profile(): Unit = {
                // sel4 notification plus shared queue

                val srcAadlThread: AadlThread = symbolTable.getThreadByName(conn.src.component)
                val dstAadlThread: AadlThread = symbolTable.getThreadByName(conn.dst.component)

                val srcToVM = srcAadlThread.toVirtualMachine(symbolTable)
                val dstToVM = dstAadlThread.toVirtualMachine(symbolTable)

                val srcComponentId: String = Util.getThreadIdentifier(srcAadlThread, symbolTable)
                val dstComponentId: String = Util.getThreadIdentifier(dstAadlThread, symbolTable)

                val srcPath: String = CommonUtil.getName(conn.src.feature.get)

                val srcFeature: ir.Feature = symbolTable.getFeatureFromName(conn.src.feature.get)
                val dstFeature: ir.Feature = symbolTable.getFeatureFromName(conn.dst.feature.get)

                val queueSize: Z = srcQueues.get(srcPath).get.get(dstPath).get.queueSize

                { // Notification connection
                  val _srcFeature = Util.genSeL4NotificationQueueName(srcFeature, queueSize)
                  val _dstFeature = Util.genSeL4NotificationName(fdst, T)

                  val notificationConnectorType: Sel4ConnectorTypes.Type =
                    if (dstToVM) Sel4ConnectorTypes.seL4GlobalAsynch
                    else Sel4ConnectorTypes.seL4Notification

                  connections = connections :+ createConnection(
                    notificationConnectorType,
                    srcComponentId, _srcFeature,
                    dstComponentId, _dstFeature)
                }

                { // Queue dataport connection
                  val queueConnectorType: Sel4ConnectorTypes.Type =
                    if (srcToVM || dstToVM) Sel4ConnectorTypes.seL4SharedDataWithCaps
                    else Sel4ConnectorTypes.seL4SharedData

                  val srcFeatureQueueName: String = Util.getEventDataSBQueueSrcFeatureName(CommonUtil.getLastName(srcFeature.identifier), queueSize)

                  val dstFeatureQueueName: String = Util.getEventDataSBQueueDestFeatureName(CommonUtil.getLastName(dstFeature.identifier))

                  connections = connections :+ createConnection(
                    queueConnectorType,
                    srcComponentId, srcFeatureQueueName,
                    dstComponentId, dstFeatureQueueName
                  )

                  if (aadlTypes.rawConnections) {
                    val size: Z = CommonTypeUtil.getMaxBitsSize(aadlTypes) match {
                      case Some(z) =>
                        if (z % z"4096" == z"0") z
                        else (z / z"4096" + 1) * z"4096"
                      case _ => z"4096" // TODO or throw error?
                    }

                    val connectionName = Util.getConnectionName(connectionCounter.count)
                    camkesConfiguration = camkesConfiguration :+
                      st"""${connectionName}.size = ${size};"""
                  }

                  camkesConfiguration = camkesConfiguration :+ st"""${srcComponentId}.${srcFeatureQueueName}_access = "W";"""
                  camkesConfiguration = camkesConfiguration :+ st"""${dstComponentId}.${dstFeatureQueueName}_access = "R";"""
                }
              }

              platform match {
                case ActPlatform.SeL4_TB => connections = connections ++ createDataConnection(conn)
                case ActPlatform.SeL4 => eventDataPort_SB_Connection_Profile()
                case ActPlatform.SeL4_Only => eventDataPort_SB_Connection_Profile()
              }
            }

            case ir.FeatureCategory.EventPort => {

              def eventPort_TB_Connection_Profile(): Unit = {
                // monitor
                connections = connections ++ createDataConnection_Ihor(conn)
              }

              def eventPort_SB_Connection_Profile(): Unit = {
                // sel4 notification plus shared counter
                val srcFeature = CommonUtil.getLastName(conn.src.feature.get)


                connections = connections :+ createSeL4NotificationConnection(
                  srcComponentName, Util.brand(srcFeature),
                  dstComponentName, Util.brand(dstFeatureName))

                connections = connections :+ createSharedDataCounterConnection(conn)
              }

              platform match {
                case ActPlatform.SeL4_TB => eventPort_TB_Connection_Profile()

                //case ActPlatform.SeL4 => eventPort_TB_Connection_Profile()
                case ActPlatform.SeL4 => eventPort_SB_Connection_Profile()
                case ActPlatform.SeL4_Only => eventPort_SB_Connection_Profile()
              }
            }

            case _ => halt (s"not expecting ${fdst.category}")
          }

        case ir.ConnectionKind.Access => {

          fdst.category match {
            case ir.FeatureCategory.SubprogramAccess =>
              val srcFeature = CommonUtil.getLastName(conn.src.feature.get)
              connections = connections :+ createRPCConnection(srcComponentName, srcFeature, dstComponentName, dstFeatureName)
            case ir.FeatureCategory.SubprogramAccessGroup =>
              val srcFeature = CommonUtil.getLastName(conn.src.feature.get)
              connections = connections :+ createRPCConnection(srcComponentName, srcFeature, dstComponentName, dstFeatureName)

            case ir.FeatureCategory.DataAccess =>
              val sd = sharedData.get(CommonUtil.getName(conn.src.component)).get
              val dstComp = symbolTable.airComponentMap.get(CommonUtil.getName(conn.dst.component)).get
              val ownerId = CommonUtil.getName(sd.owner.identifier)
              val dstId = CommonUtil.getName(dstComp.identifier)

              if (ownerId != dstId) {
                connections = connections :+ createConnection(Sel4ConnectorTypes.seL4SharedData,
                  dstComponentName, dstFeatureName,
                  CommonUtil.getLastName(sd.owner.identifier), CommonUtil.getLastName(sd.ownerFeature.get.identifier)
                )
              } else {
                // Ignore connection to the owner component
              }
            case _ => halt(s"not expecting ${fdst.category}")
          }
        }

        case _ => halt(s"not expecting ${conn.kind}")
      }
    }

    return connections
  }

  def genProcess(aadlProcess : AadlProcess) : Composition = {

    val c = aadlProcess.component

    var compositionMacros: ISZ[String] = ISZ()
    var connections: ISZ[Connection] = ISZ()
    var instances: ISZ[Instance] = ISZ()

    for(sc <- c.subComponents) {
      sc.category match {
        case ir.ComponentCategory.Thread =>
          val aadlThread = symbolTable.getThread(sc)

          if(aadlThread.toVirtualMachine(symbolTable)) {
            val processId = Util.getThreadIdentifier(aadlThread, symbolTable)
            val (component, auxResources) =
              VMGen(symbolTable, typeMap, samplingPorts, srcQueues, actOptions, reporter).genThread(aadlThread)

            instances = instances :+
              Instance(address_space = "",
                name = processId,
                component = component
              )

            auxResourceFiles = auxResourceFiles ++ auxResources

            connections = connections :+ createConnection(
              Sel4ConnectorTypes.seL4VMDTBPassthrough,
              processId, "dtb_self",
              processId, "dtb"
            )

            globalPreprocessorIncludes = globalPreprocessorIncludes ++
              VM_Template.vm_assembly_preprocessor_includes()

            globalImports = globalImports ++ VM_Template.vm_assembly_imports()

            compositionMacros = compositionMacros ++
              VM_Template.vm_composition_macros(aadlProcess.identifier)

            camkesConfiguration = camkesConfiguration ++
              VM_Template.vm_assembly_configuration_entries(processId)

            camkesConfigurationMacros = camkesConfigurationMacros ++
              VM_Template.vm_assembly_configuration_macros(aadlProcess.identifier)

            settingsCmakeEntries = settingsCmakeEntries ++ VM_Template.settings_cmake_entries()

            auxResourceFiles = auxResourceFiles :+ Util.sbCounterTypeDeclResource()

          } else {
            instances = instances :+
              Instance(address_space = "",
                name = aadlThread.identifier,
                component = genThread(aadlThread)
              )
          }

          PropertyUtil.getStackSizeInBytes(sc) match {
            case Some(bytes) =>
              camkesConfiguration = camkesConfiguration :+ StringTemplate.configurationStackSize(aadlThread.identifier, bytes)
            case _ =>
          }
          
        case ir.ComponentCategory.Subprogram =>
          var params: ISZ[Parameter] = ISZ()
          for(f <- sc.features) {
            val fend = f.asInstanceOf[ir.FeatureEnd]
            params = params :+ Parameter(array = F,
              direction = if(fend.direction == ir.Direction.In) Direction.In else Direction.Out,
              name = CommonUtil.getLastName(f.identifier),
              typ = Util.getClassifier(fend.classifier.get))
          }

          val method = Method(
            name = CommonUtil.getLastName(sc.identifier),
            parameters = params,
            returnType = None[String]()
          )

          val procName = Util.getClassifier(sc.classifier.get)
          astObjects = astObjects :+ Procedure(name = procName, methods = ISZ(method), includes = ISZ())

        case ir.ComponentCategory.SubprogramGroup =>
          var methods: ISZ[Method] = ISZ()

          for(m <- sc.features) {
            m match {
              case spa: ir.FeatureAccess =>
                if(spa.classifier.nonEmpty) {
                  val spComp = symbolTable.airClassifierMap.get(spa.classifier.get.name).get
                  assert(spComp.category == ir.ComponentCategory.Subprogram)

                  val methodName = CommonUtil.getLastName(spa.identifier)
                  var params: ISZ[Parameter] = ISZ()
                  for(param <- spComp.features) {
                    param match {
                      case p: ir.FeatureEnd =>
                        assert(param.category == ir.FeatureCategory.Parameter)

                        val paramName = CommonUtil.getLastName(param.identifier)
                        val dir: Direction.Type = p.direction match {
                          case ir.Direction.In => Direction.In
                          case ir.Direction.Out => Direction.Out
                          case x => halt(s"Unexpected direction ${x}")
                        }
                        val typName = Util.getClassifierFullyQualified(p.classifier.get)

                        params = params :+ Parameter(array = F, direction = dir, name = paramName, typ = typName)

                    }
                  }

                  methods = methods :+ Method(name = methodName, parameters = params, returnType = None[String]())
                } else{
                  reporter.error(None(), Util.toolName, s"Could not resolve feature ${CommonUtil.getName(spa.identifier)} from ${CommonUtil.getName(sc.identifier)}")
                }
              case _ =>
            }
          }

          if(sc.subComponents.nonEmpty) { halt(s"Subprogram group subcomponents not currently handled: ${sc}")}

          val procName = Util.getClassifier(sc.classifier.get)
          astObjects = astObjects :+ Procedure(name = procName, methods = methods, includes = ISZ())

        case ir.ComponentCategory.Data =>
          val typeName = Util.getClassifierFullyQualified(sc.classifier.get)

          val readMethod = Method(
            name = s"read_${typeName}",
            parameters = ISZ(Parameter(array = F, direction = Direction.Out, name = "arg", typ = typeName)),
            returnType = None[String]()
          )

          val writeMethod = Method(
            name = s"write_${typeName}",
            parameters = ISZ(Parameter(array = F, direction = Direction.Refin, name = "arg", typ = typeName)),
            returnType = None[String]()
          )

          val procName = Util.getSharedDataInterfaceName(sc.classifier.get)
          astObjects = astObjects :+ Procedure(
            name = procName, 
            methods = ISZ(readMethod, writeMethod), 
            includes = ISZ(Util.getSbTypeHeaderFilenameForIncludes())
          )

          Util.getCamkesOwnerThread(sc.properties) match {
            case Some(owner) =>
              val threads = symbolTable.airComponentMap.values.filter(f => f.category == ir.ComponentCategory.Thread)
              val thread = threads.filter(f => f.classifier.get.name == owner || Util.getClassifier(f.classifier.get) == owner)

              if(thread.nonEmpty) {
                val theOwner = thread(0)
                if(thread.size > 1) {
                  reporter.warn(None(), Util.toolName, s"Found multiple matches for ${Util.PROP_TB_SYS__CAmkES_Owner_Thread} property: ${owner}")
                }
                if(Util.getClassifier(theOwner.classifier.get) == owner){
                  reporter.warn(None(), Util.toolName, s"Fully qualified name '${theOwner.classifier.get.name}' should be used for ${Util.PROP_TB_SYS__CAmkES_Owner_Thread} property")
                }

                val subcomponentId = CommonUtil.getLastName(sc.identifier)
                sharedData = sharedData +
                  (CommonUtil.getName(sc.identifier) ~> SharedData(theOwner, None[ir.FeatureAccess](), sc.classifier.get, subcomponentId))

              } else {
                reporter.error(None(), Util.toolName, st"""${Util.PROP_TB_SYS__CAmkES_Owner_Thread}:  Could not locate component '${owner}'.  Please use one of the following:
                             |  ${(threads.map((m: ir.Component) => m.classifier.get.name), "\n")}""".render)
              }
            case _ =>
              reporter.error(None(), Util.toolName, st"""${Util.PROP_TB_SYS__CAmkES_Owner_Thread} property missing""".render)
          }

        case _ =>
          halt(s"Not handling: subcomponent of type '${sc.category}'.  ${CommonUtil.getName(sc.identifier)}")
      }
    }

    val monInstances = monitors.values.map((m: Monitor) => m.i)
    return Composition(
      groups = ISZ(),
      exports = ISZ(),
      instances = instances ++ monInstances,
      connections = connections ++ processConnections(c),
      externalEntities = compositionMacros
    )
  }

  def getSamplingPort(fend: FeatureEnd): SamplingPortInterface = {
    val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(fend.classifier.get)).get
    val sel4PortType: String = Util.getSel4TypeName(aadlPortType, performHamrIntegration)
    return samplingPorts.get(sel4PortType).get
  }

  def genThread(aadlThread : AadlThread) : Component = {

    val c = aadlThread.component

    assert(c.subComponents.isEmpty)
    assert(c.connectionInstances.isEmpty)

    val cid = Util.getCamkesComponentName(aadlThread, symbolTable)

    var provides: ISZ[Provides] = ISZ()
    var uses: ISZ[Uses] = ISZ()
    var emits: ISZ[Emits] = ISZ()
    var consumes: ISZ[Consumes] = ISZ()
    var dataports: ISZ[Dataport] = ISZ()

    var camkesIncludes: Set[String] = Set.empty
    camkesIncludes = camkesIncludes + Util.getSbTypeHeaderFilenameForIncludes()

    var cmakeSOURCES: ISZ[String] = ISZ()
    var cmakeINCLUDES: ISZ[String] = ISZ()
    
    var imports : Set[String] = Set.empty

    var gcInterfaceStatements: ISZ[ST] = ISZ()
    var gcImplIncludes: ISZ[ST] = ISZ()
    var gcImplMethods: ISZ[ST] = ISZ()
    var gcImplPreInits: ISZ[ST] = ISZ()
    var gcImplPostInits: ISZ[ST] = ISZ()
    var gcImplDrainQueues: ISZ[(ST, ST)] = ISZ()

    for(f <- aadlThread.getFeatureAccesses()) {
      
      def handleSubprogramAccess(): Unit = {
        val fid = CommonUtil.getLastName(f.identifier)

        val proc = Util.getClassifier(f.classifier.get)

        f.accessType match {
          case ir.AccessType.Requires =>
            imports = imports + Util.getInterfaceFilename(proc)
            uses = uses :+ Uses(
              name = fid,
              optional = F,
              typ = proc)
          case ir.AccessType.Provides =>
            imports = imports + Util.getInterfaceFilename(proc)
            provides = provides :+ Provides(
              name = fid,
              typ = proc)
        }
      }

      def handleDataAccess(): Unit = {
        val fid = CommonUtil.getLastName(f.identifier)

        val typeName = Util.getClassifierFullyQualified(f.classifier.get)
        val interfaceName = Util.getSharedDataInterfaceName(f.classifier.get)

        f.accessType match {
          case ir.AccessType.Requires =>
            imports = imports + Util.getInterfaceFilename(interfaceName)
            dataports = dataports :+ Dataport(
              name = fid,
              optional = F,
              typ = typeName)
          case ir.AccessType.Provides =>
            imports = imports + Util.getInterfaceFilename(interfaceName)
            dataports = dataports :+ Dataport(
              name = fid,
              optional = F,
              typ = typeName)
        }
      }

      f.category match {
        case ir.FeatureCategory.SubprogramAccessGroup =>
          handleSubprogramAccess()
        case ir.FeatureCategory.SubprogramAccess =>
          handleSubprogramAccess()

        case ir.FeatureCategory.DataAccess =>
          handleDataAccess()

        case _ =>
          reporter.error(None(), Util.toolName, s"Not expecting AccessType: ${CommonUtil.getName(f.identifier)}")
      }
    }

    for(f <- aadlThread.getFeatureEnds() if(symbolTable.isConnected(f))) {
      val fpath = CommonUtil.getName(f.identifier)
      val fid = CommonUtil.getLastName(f.identifier)

      def handleDataPort_SB_Profile(): Unit = {
        assert(f.direction == ir.Direction.In || f.direction == ir.Direction.Out)

        val samplingPort: SamplingPortInterface = getSamplingPort(f)

        camkesIncludes = camkesIncludes + s"<${Os.path(samplingPort.headerPath).name}>"

        cmakeSOURCES = cmakeSOURCES :+ samplingPort.implPath
        cmakeINCLUDES = cmakeINCLUDES :+ Os.path(samplingPort.headerPath).up.value

        gcImplMethods = StringTemplate.sbSamplingPortGlobalVarDecl(samplingPort, f) +: gcImplMethods

        camkesConfiguration = camkesConfiguration :+
          StringTemplate.sbSamplingPortConfigurationEntry(
            CommonUtil.getLastName(c.identifier), samplingPort, f)

        dataports = dataports :+ Dataport(
          name = Util.brand(fid),
          optional = F,
          typ = samplingPort.structName
        )
      }

      def handleDataPort_TB_Profile(): Unit = {
        f.direction match {
          case ir.Direction.In =>
            // uses monitor
            // consumes notification
            if (symbolTable.inConnections.get(fpath).nonEmpty) {
              val monitor = getMonitorForInPort(f).get.asInstanceOf[TB_Monitor]
              imports = imports + Util.getInterfaceFilename(monitor.interface.name)

              uses = uses :+ Uses(
                name = Util.genMonitorFeatureName(f, None[Z]()),
                typ = monitor.interface.name,
                optional = F)

              consumes = consumes :+ Consumes(
                name = Util.genSeL4NotificationName(f, T),
                typ = Util.getMonitorNotificationType(f.category),
                optional = F)
            }
          case ir.Direction.Out =>
            // uses monitor
            symbolTable.outConnections.get(fpath) match {
              case Some(outs) =>
                var i: Z = 0
                for (o <- outs) {
                  val monitor = monitors.get(CommonUtil.getName(o.name)).get.asInstanceOf[TB_Monitor]
                  imports = imports + Util.getInterfaceFilename(monitor.interface.name)

                  uses = uses :+ Uses(
                    name = Util.genMonitorFeatureName(f, Some(i)),
                    typ = monitor.interface.name,
                    optional = F
                  )
                  i = i + 1
                }
              case _ =>
            }
          case _ =>
            halt(s"Not expecting direction ${f.direction}")
        }
      }

      def handleEventPort(isEventData: B): Unit = {
        f.direction match {
          case ir.Direction.In =>

            val isConnected = symbolTable.inConnections.contains(CommonUtil.getName(f.identifier))
            val incorporateEntrypointSource = Util.getComputeEntrypointSourceText(f.properties).nonEmpty && aadlThread.isSporadic()

            if (isConnected && (incorporateEntrypointSource || performHamrIntegration)) {
              val name = Util.genSeL4NotificationName(f, isEventData)
              val handlerName = s"${name}_handler"
              val regCallback = s"${name}_reg_callback"

              if (isEventData) {
                val featureName = CommonUtil.getLastName(f.identifier)
                gcImplMethods = gcImplMethods :+
                  StringTemplate.cEventNotificationHandler(handlerName, regCallback, featureName)
              }
              gcImplPostInits = gcImplPostInits :+ StringTemplate.cRegCallback(handlerName, regCallback, f)
            } else {
              reporter.warn(None(), Util.toolName, s"port: ${fid} in thread: ${cid} does not have a compute entrypoint and will not be dispatched.")
            }

          case _ =>
        }
      }

      def eventDataPort_SB_Component_Profile(): Unit = {
        // notification plus a shared counter

        // correct for fan out connections?

        val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(f.classifier.get)).get
        val sel4TypeName = Util.getSel4TypeName(aadlPortType, performHamrIntegration)

        auxResourceFiles = auxResourceFiles :+ Util.sbCounterTypeDeclResource()

        f.direction match {
          case ir.Direction.In =>
            val queueSize = PropertyUtil.getQueueSize(f, Util.DEFAULT_QUEUE_SIZE)
            val queueType = Util.getEventDataSBQueueTypeName(sel4TypeName, queueSize)

            val queueImplFilename = Util.getEventData_SB_QueueImplFileName(sel4TypeName, queueSize)
            val queueHeaderFilename = Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, queueSize)

            cmakeSOURCES = cmakeSOURCES :+ s"${Util.getTypeSrcPath()}/${queueImplFilename}"
            camkesIncludes = camkesIncludes + s"<${queueHeaderFilename}>"

            val isOptional = symbolTable.getInConnections(fpath).isEmpty

            consumes = consumes :+ Consumes(
              name = Util.genSeL4NotificationName(f, T),
              typ = Util.EVENT_NOTIFICATION_TYPE,
              optional = isOptional)

            dataports = dataports :+ Dataport(
              name = Util.getEventDataSBQueueDestFeatureName(fid),
              typ = queueType,
              optional = isOptional)

          case ir.Direction.Out =>

            val dsts: Map[String, QueueObject] = srcQueues.get(fpath).get // dstId -> queueObject

            for (qo <- dsts.valueSet.elements) {
              val queueSize = qo.queueSize
              val queueType = Util.getEventDataSBQueueTypeName(sel4TypeName, queueSize)
              val queueImplName = Util.getEventData_SB_QueueImplFileName(sel4TypeName, queueSize)
              val queueHeaderFilename = Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, queueSize)

              cmakeSOURCES = cmakeSOURCES :+ s"${Util.getTypeSrcPath()}/${queueImplName}"
              camkesIncludes = camkesIncludes + s"<${queueHeaderFilename}>"

              emits = emits :+ Emits(
                name = Util.genSeL4NotificationQueueName(f, queueSize),
                typ = Util.EVENT_NOTIFICATION_TYPE)

              dataports = dataports :+ Dataport(
                name = Util.getEventDataSBQueueSrcFeatureName(fid, queueSize),
                typ = queueType,
                optional = F)
            }
          case _ => halt(s"Unexpected port direction: ${f.direction}")
        }
      }

        f.category match {
          case ir.FeatureCategory.DataPort =>
            platform match {
              case ActPlatform.SeL4_TB => handleDataPort_TB_Profile()
              case ActPlatform.SeL4 => handleDataPort_SB_Profile()
              case ActPlatform.SeL4_Only => handleDataPort_SB_Profile()
            }

          case ir.FeatureCategory.EventDataPort =>
            platform match {
              case ActPlatform.SeL4_TB =>
                handleDataPort_TB_Profile()
                handleEventPort(T)
              case ActPlatform.SeL4 => eventDataPort_SB_Component_Profile()
              case ActPlatform.SeL4_Only => eventDataPort_SB_Component_Profile()
            }

          case ir.FeatureCategory.EventPort =>

            def handleEventPort_TB_Component_Profile(): Unit = {
              f.direction match {
                case ir.Direction.In =>
                  // import receiver interface
                  imports = imports + s""""../../interfaces/${Util.MONITOR_INTERFACE_NAME_RECEIVER}.idl4""""

                  // uses
                  uses = uses :+ Uses(
                    name = Util.genMonitorFeatureName(f, None[Z]()),
                    typ = Util.MONITOR_INTERFACE_NAME_RECEIVER,
                    optional = F,
                  )

                  // consumes notification
                  consumes = consumes :+ Consumes(
                    name = Util.genSeL4NotificationName(f, T),
                    typ = Util.MONITOR_EVENT_DATA_NOTIFICATION_TYPE,
                    optional = F)

                case ir.Direction.Out =>
                  // import sender interface
                  imports = imports + s""""../../interfaces/${Util.MONITOR_INTERFACE_NAME_SENDER}.idl4""""

                  symbolTable.outConnections.get(fpath) match {
                    case Some(outs) =>
                      var i: Z = 0
                      for (o <- outs) {

                        uses = uses :+ Uses(
                          name = Util.genMonitorFeatureName(f, Some(i)),
                          typ = Util.MONITOR_INTERFACE_NAME_SENDER,
                          optional = F
                        )

                        i = i + 1
                      }
                  }

                case _ => halt(s"${fpath}: not expecting direction ${f.direction}")
              }
            }

            def handleEventPort_SB_Component_Profile(): Unit = {
              // notification plus a shared counter

              // correct for fan out connections?

              val counterType = Util.SB_EVENT_COUNTER_TYPE

              // add sb counter type decl resource
              auxResourceFiles = auxResourceFiles :+ Util.sbCounterTypeDeclResource()
              camkesIncludes = camkesIncludes + s"<${Util.SB_COUNTER_HEADER_FILENAME}>"

              f.direction match {
                case ir.Direction.In =>
                  consumes = consumes :+ Consumes(
                    name = Util.getEventSBNotificationName(fid),
                    typ = Util.EVENT_NOTIFICATION_TYPE,
                    optional = F)

                  dataports = dataports :+ Dataport(
                    name = Util.getEventSBCounterName(fid),
                    typ = counterType,
                    optional = F)

                case ir.Direction.Out =>
                  emits = emits :+ Emits(
                    name = Util.getEventSBNotificationName(fid),
                    typ = Util.EVENT_NOTIFICATION_TYPE)

                  dataports = dataports :+ Dataport(
                    name = Util.getEventSBCounterName(fid),
                    typ = counterType,
                    optional = F)

                case _ => halt(s"Unexpected port direction: ${f.direction}")
              }
            }

            platform match {
              case ActPlatform.SeL4_TB => handleEventPort_TB_Component_Profile()
              case ActPlatform.SeL4 => handleEventPort_SB_Component_Profile()
              case ActPlatform.SeL4_Only => handleEventPort_SB_Component_Profile()
            }

          case _ =>
            reporter.warn(None(), Util.toolName, s"Skipping ${f.category} for ${fid}.${fid}")
        }

        generateGlueCodePortInterfaceMethod(aadlThread, f) match {
          case Some(C_SimpleContainer(cIncludes, interface, impl, preInit, postInits, drainQueues)) =>
            if (cIncludes.nonEmpty) {
              gcImplIncludes = gcImplIncludes ++ cIncludes
            }
            if (interface.nonEmpty) {
              gcInterfaceStatements = gcInterfaceStatements :+ interface.get
            }
            if (impl.nonEmpty) {
              gcImplMethods = gcImplMethods :+ impl.get
            }
            if (preInit.nonEmpty) {
              gcImplPreInits = gcImplPreInits :+ preInit.get
            }
            if (postInits.nonEmpty) {
              gcImplPostInits = gcImplPostInits :+ postInits.get
            }
            if (drainQueues.nonEmpty) {
              gcImplDrainQueues = gcImplDrainQueues :+ drainQueues.get
            }
          case _ =>
        }

    }// end processing thread's features
    
    // Build camkes component and glue code files
    
    var binarySemaphores: ISZ[BinarySemaphore] = ISZ()
    var semaphores: ISZ[Semaphore] = ISZ()

    PropertyUtil.getDiscreetPropertyValue(c.properties, "camkes::Binary_Semaphore") match {
      case Some(v: ir.ValueProp) =>
        binarySemaphores = binarySemaphores :+ BinarySemaphore(v.value)
      case _ =>
    }
    
    
    var gcRunInitStmts: ISZ[ST] = gcImplDrainQueues.map(x => x._1)
    var gcRunPreLoopStmts: ISZ[ST] = ISZ()
    var gcRunLoopStartStmts: ISZ[ST] = ISZ()
    var gcRunLoopMidStmts: ISZ[ST] = gcImplDrainQueues.map(x => x._2)
    var gcRunLoopEndStmts: ISZ[ST] = ISZ()
    
    if(!PeriodicUtil.requiresPacerArtifacts(c, symbolTable, actOptions.platform)) {
      semaphores = semaphores :+ Semaphore(StringTemplate.SEM_DISPATCH)
            
      val semWait: ST = st"MUTEXOP(${StringTemplate.SEM_WAIT}())"
      
      gcRunPreLoopStmts = gcRunPreLoopStmts :+ semWait
      
      gcRunLoopStartStmts = gcRunLoopStartStmts :+ semWait
    }

    PropertyUtil.getDispatchProtocol(c) match {
      case Some(Dispatch_Protocol.Periodic) =>

        val (componentContributions, glueCodeContributions) = 
          Dispatcher.handlePeriodicComponent(symbolTable, actOptions, aadlThread, reporter)
        
        binarySemaphores = binarySemaphores ++ componentContributions.shell.binarySemaphores
        semaphores = semaphores ++ componentContributions.shell.semaphores
        imports = imports ++ componentContributions.shell.imports
        emits = emits ++ componentContributions.shell.emits
        consumes = consumes ++ componentContributions.shell.consumes
        uses = uses ++ componentContributions.shell.uses

        gcImplMethods = glueCodeContributions.impl.methods ++ gcImplMethods

        gcRunInitStmts = gcRunInitStmts ++ glueCodeContributions.impl.preInitStatements
        gcRunPreLoopStmts = gcRunPreLoopStmts ++ glueCodeContributions.impl.mainPreLoopStatements
        gcRunLoopStartStmts = gcRunLoopStartStmts ++ glueCodeContributions.impl.mainLoopStartStatements
        gcRunLoopMidStmts = gcRunLoopMidStmts ++ glueCodeContributions.impl.mainLoopStatements
        gcRunLoopEndStmts = gcRunLoopEndStmts ++ glueCodeContributions.impl.mainLoopEndStatements
        
        gcInterfaceStatements = gcInterfaceStatements ++ glueCodeContributions.header.methods
       
      case Some(Dispatch_Protocol.Sporadic) =>

      case x =>
        if(x.nonEmpty) {
          Util.reporter.error(None(), Util.toolName, s"Dispatch protocol $x for ${CommonUtil.getLastName(c.identifier)} is not supported")
        } else {
          reporter.warn(None(), Util.toolName, s"Dispatch Protocol not specified for ${CommonUtil.getLastName(c.identifier)}, assuming Sporadic")
        }
    }

    val names: Names = Names(c, hamrBasePackageName.get)
    
    if(!performHamrIntegration) {
      PropertyUtil.getInitializeEntryPoint(c.properties) match {
        case Some(methodName) =>
          gcInterfaceStatements = gcInterfaceStatements :+ st"""void ${methodName}(const int64_t *arg);"""
          val (cimpl, runEntry) = StringTemplate.componentInitializeEntryPoint(cid, methodName)
          gcImplMethods = gcImplMethods :+ cimpl
          gcRunInitStmts = gcRunInitStmts :+ runEntry
        case _ =>
      }
    }
        
    if(performHamrIntegration) {
      
      gcImplIncludes = gcImplIncludes :+ st"#include <${Util.genCHeaderFilename(names.cEntryPointAdapterName)}>"

      gcImplPreInits = st"""printf("Entering pre-init of ${cid}\n");""" +: gcImplPreInits

      gcImplPreInits = gcImplPreInits :+ StringTemplate.hamrIntialiseArchitecture(names.cEntryPointAdapterQualifiedName)
      
      gcImplPreInits = gcImplPreInits :+ StringTemplate.hamrInitialiseEntrypoint(names.cEntryPointAdapterQualifiedName)

      gcImplPreInits = gcImplPreInits :+ st"""printf("Leaving pre-init of ${cid}\n");"""

      val outgoingPorts: ISZ[ir.FeatureEnd] = CommonUtil.getOutPorts(c).filter(f =>
        symbolTable.outConnections.contains(CommonUtil.getName(f.identifier)))

      gcImplMethods = gcImplMethods ++ outgoingPorts.map((f: ir.FeatureEnd) => hamrSendOutgoingPort(c, names, f, typeMap))

      val unconnectedOutgoingPorts: ISZ[ir.FeatureEnd] = CommonUtil.getOutPorts(c).filter(f =>
        !symbolTable.outConnections.contains(CommonUtil.getName(f.identifier)))

      gcImplMethods = gcImplMethods ++ unconnectedOutgoingPorts.map((f: ir.FeatureEnd) => hamrSendUnconnectedOutgoingPort(c, names, f, typeMap))
      
      val inPorts: ISZ[ir.FeatureEnd] = CommonUtil.getInPorts(c).filter(f =>
        symbolTable.inConnections.contains(CommonUtil.getName(f.identifier)))

      gcImplMethods = gcImplMethods ++ inPorts.map((f: ir.FeatureEnd) => hamrReceiveIncomingPort(c, names, f, typeMap))

      val unconnectedInPorts: ISZ[ir.FeatureEnd] = CommonUtil.getInPorts(c).filter(f =>
        !symbolTable.inConnections.contains(CommonUtil.getName(f.identifier)))

      val freezeInEventPorts = 
        inPorts.filter((p : ir.FeatureEnd) => p.category == ir.FeatureCategory.EventPort)
          .map((eventPort : ir.FeatureEnd) =>
          st"${StringTemplate.samplingPortFreezeMethodName(eventPort)}();")
        
      // remove any mid loop statements and replace with sel4 specific ones
      gcRunLoopMidStmts = freezeInEventPorts ++ StringTemplate.hamrRunLoopEntries(names.cEntryPointAdapterQualifiedName)

      gcImplMethods = gcImplMethods ++ unconnectedInPorts.map((f: ir.FeatureEnd) => hamrReceiveUnconnectedIncomingPort(c, names, f, typeMap))
    }

    var cSources: ISZ[Resource] = ISZ()

    if(CakeML.requiresFFIs(aadlThread)) {
      val ffis: Resource = CakeML.processThread(aadlThread, hamrBasePackageName.get)
      cSources = cSources :+ ffis
    }

    val gcImpl = genComponentTypeImplementationFile(
      aadlThread = aadlThread,
      localCIncludes = auxCImplIncludes ++ gcImplIncludes,
      blocks = gcImplMethods,
      preInits = gcImplPreInits,
      postInits = gcImplPostInits,
      gcRunInitStmts = gcRunInitStmts,
      gcRunPreLoopStmts = gcRunPreLoopStmts,
      gcRunLoopStartStmts = gcRunLoopStartStmts,
      gcRunLoopMidStmts = gcRunLoopMidStmts,
      gcRunLoopEndStmts = gcRunLoopEndStmts,
      containsFFIs = CakeML.requiresFFIs(aadlThread))

    cSources = cSources :+ gcImpl

    containers = containers :+ C_Container(
      instanceName = names.instanceName,
      componentId = cid,

      cSources = cSources,
      cIncludes = ISZ(genComponentTypeInterfaceFile(c, gcInterfaceStatements)),

      sourceText = if(performHamrIntegration) ISZ() else PropertyUtil.getSourceText(c.properties),

      cmakeSOURCES = cmakeSOURCES,
      cmakeINCLUDES = cmakeINCLUDES :+ Util.getTypeIncludesPath(),
      cmakeLIBS = ISZ()
    )

    return Component(
      control = T,
      hardware = F,
      name = cid,

      mutexes = ISZ(),
      binarySemaphores = binarySemaphores,
      semaphores = semaphores,

      dataports = dataports,
      emits = emits,
      uses = uses,
      consumes = consumes,
      provides = provides,
      includes = camkesIncludes.elements,
      attributes = ISZ(),

      preprocessorIncludes = ISZ(),
      imports = imports.elements,

      externalEntities = ISZ()
    )
  }

  def hamrSendUnconnectedOutgoingPort(c: ir.Component, names: Names, srcPort: ir.FeatureEnd, typeMap: HashSMap[String, ir.Component]): ST = {
    val portName = CommonUtil.getLastName(srcPort.identifier)
    val sel4ExtName = names.sel4SlangExtensionQualifiedNameC

    val methodName = s"${sel4ExtName}_${portName}_Send"
    
    return StringTemplate.hamrSendUnconnectedOutgoingDataPort(methodName)
  }
  
  def hamrSendOutgoingPort(c: ir.Component, names: Names, srcPort: ir.FeatureEnd, typeMap: HashSMap[String, ir.Component]): ST = {
    val portName = CommonUtil.getLastName(srcPort.identifier)
    val sel4ExtName = names.sel4SlangExtensionQualifiedNameC

    val methodName = s"${sel4ExtName}_${portName}_Send"
    
    def handleOutDataPort(): ST = {
      val suffix: String = if(srcPort.category == ir.FeatureCategory.DataPort) "write" else "enqueue"
      val srcEnqueue = Util.brand(s"${CommonUtil.getLastName(srcPort.identifier)}_${suffix}")
      val camkesType = Util.getClassifierFullyQualified(srcPort.classifier.get)
      val sel4Type = Util.getSel4TypeName(typeMap.get(camkesType).get, performHamrIntegration)
      val slangPayloadType = StringTemplate.hamrSlangPayloadType(srcPort.classifier.get, hamrBasePackageName.get)

      val comment = st"// send ${portName}: ${srcPort.direction.name} ${srcPort.category.name} ${camkesType}"
      return StringTemplate.hamrSendOutgoingDataPort(comment, methodName, sel4Type, slangPayloadType, srcEnqueue)
    }
    
    def handleOutEventPort(): ST = {
      val srcEnqueue = Util.getEventPortSendReceiveMethodName(srcPort)
      val comment = st"// send ${portName}: ${srcPort.direction.name} ${srcPort.category.name}"
      return StringTemplate.hamrSendOutgoingEventPort(comment, methodName, srcEnqueue)
    }
    
    val ret: ST = srcPort.category match {
      case ir.FeatureCategory.DataPort => handleOutDataPort()
      case ir.FeatureCategory.EventDataPort => handleOutDataPort()
      case ir.FeatureCategory.EventPort => handleOutEventPort()
      case x => halt(s"Not handling ${x}")
    }
    
    return ret
  }

  def hamrReceiveUnconnectedIncomingPort(c: ir.Component, names: Names, srcPort: ir.FeatureEnd, typeMap: HashSMap[String, ir.Component]): ST = {
    val portName = CommonUtil.getLastName(srcPort.identifier)
    val sel4ExtName = names.sel4SlangExtensionQualifiedNameC

    val methodName = s"${sel4ExtName}_${portName}_Receive"

    val receiveMethod = StringTemplate.hamrReceiveUnconnectedIncomingEventPort(methodName)
    
    val isEmptyMethodName = s"${sel4ExtName}_${portName}_IsEmpty"
    val sel4IsEmptyMethodName = Util.brand(s"${portName}_is_empty")
    val isEmptyMethod = StringTemplate.hamrIsEmptyUnconnected(isEmptyMethodName, sel4IsEmptyMethodName)
    
    return st"""${isEmptyMethod}
               |
               |${receiveMethod}"""
  }
  
  def hamrReceiveIncomingPort(c: ir.Component, names: Names, srcPort: ir.FeatureEnd, typeMap: HashSMap[String, ir.Component]): ST = {
    val portName = CommonUtil.getLastName(srcPort.identifier)
    val sel4ExtName = names.sel4SlangExtensionQualifiedNameC

    val methodName = s"${sel4ExtName}_${portName}_Receive"
    val suffix: String = if(srcPort.category == ir.FeatureCategory.DataPort) "read" else "dequeue"
    val sel4ReadMethod = Util.brand(s"${portName}_${suffix}")

    val isEmptyMethodName = s"${sel4ExtName}_${portName}_IsEmpty"
    val sel4IsEmptyMethodName = Util.brand(s"${portName}_is_empty")

    
    def handleInDataPort(): ST = {
      val camkesType = Util.getClassifierFullyQualified(srcPort.classifier.get)
      val sel4Type = Util.getSel4TypeName(typeMap.get(camkesType).get, performHamrIntegration)
      val slangPayloadType = StringTemplate.hamrSlangPayloadType(srcPort.classifier.get, hamrBasePackageName.get)
      val isEmptyMethod = StringTemplate.hamrIsEmpty(isEmptyMethodName, sel4IsEmptyMethodName, srcPort)
      
      val comment = st"// receive ${portName}: ${srcPort.direction.name} ${srcPort.category.name} ${sel4Type}"
      val receiveMethod = StringTemplate.hamrReceiveIncomingDataPort(
        comment, methodName, sel4Type, slangPayloadType, sel4ReadMethod)

      return st"""${isEmptyMethod}
                 |
                 |${receiveMethod}"""
    }
    
    def handleInEventPort(): ST = {
      val comment = st"// receive ${portName}: ${srcPort.direction.name} ${srcPort.category.name}"
      val receiveMethod = StringTemplate.hamrReceiveIncomingEventPort(comment, methodName, sel4ReadMethod)

      val isEmptyMethod = StringTemplate.hamrIsEmpty(isEmptyMethodName, sel4IsEmptyMethodName, srcPort)
      
      return st"""${isEmptyMethod}
                 |
                 |${receiveMethod}"""
    }
    
    val ret: ST = srcPort.category match {
      case ir.FeatureCategory.DataPort => handleInDataPort()
      case ir.FeatureCategory.EventDataPort => handleInDataPort()
      case ir.FeatureCategory.EventPort => handleInEventPort()
      case x => halt(s"Not handling ${x}")
    }
    
    return ret
  }

  def buildSamplingPortInterfaces(): Unit = {
    platform match {
      case ActPlatform.SeL4_TB => return
      case ActPlatform.SeL4 => // use new mappings below
      case ActPlatform.SeL4_Only => // use new mappings below
    }

    for (portPath <- symbolTable.outConnections.keys.filter(p => symbolTable.outConnections.get(p).get.size > 0)) {

      for (connInst <- symbolTable.outConnections.get(portPath).get()) {
        val dst: ir.Component = symbolTable.airComponentMap.get(CommonUtil.getName(connInst.dst.component)).get
        val dstFeature: ir.Feature = symbolTable.airFeatureMap.get(CommonUtil.getName(connInst.dst.feature.get)).get

        connInst.kind match {
          case ir.ConnectionKind.Port =>
            dstFeature.category match {
              case ir.FeatureCategory.DataPort =>

                val dstFend = dstFeature.asInstanceOf[ir.FeatureEnd]
                val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(dstFend.classifier.get)).get
                val sel4PortType: String = Util.getSel4TypeName(aadlPortType, performHamrIntegration)

                val name = s"sp_${sel4PortType}"
                val structName = s"${name}_t"
                
                val spi = SamplingPortInterface(
                  name = name,
                  structName = structName,
                  sel4TypeName = sel4PortType,
                  headerFilename = Util.genCHeaderFilename(name),
                  implFilename = Util.genCImplFilename(name)
                )

                samplingPorts = samplingPorts + (sel4PortType ~> spi)
              case _ =>
            }
          case ir.ConnectionKind.Access => // no monitor needed
          case _ => reporter.warn(None(), Util.toolName, s"processInConnections: Not handling ${connInst}")
        }
      }
    }
  }
  
  def buildSBQueues(): Unit = {
    platform match {
      case ActPlatform.SeL4_TB => return

      //case ActPlatform.SeL4 => return
      case ActPlatform.SeL4 => // use new mappings below
      case ActPlatform.SeL4_Only => // use new mappings below
    }

    var seen: Set[String] = Set.empty[String]

    val entries = symbolTable.inConnections.entries.filter(p => p._2.size > 0)

    for (in <- entries) {
      for (connInst <- in._2) {
        val src: ir.Component = symbolTable.airComponentMap.get(CommonUtil.getName(connInst.src.component)).get
        val srcFeature: ir.Feature = symbolTable.airFeatureMap.get(CommonUtil.getName(connInst.src.feature.get)).get
        val srcId: String = CommonUtil.getName(srcFeature.identifier)
          
        val dst: ir.Component = symbolTable.airComponentMap.get(CommonUtil.getName(connInst.dst.component)).get
        val dstFeature: ir.Feature = symbolTable.airFeatureMap.get(CommonUtil.getName(connInst.dst.feature.get)).get
        val dstId: String = CommonUtil.getName(dstFeature.identifier)
        
        if (dstFeature.category == ir.FeatureCategory.EventDataPort) {
          val dstFend = dstFeature.asInstanceOf[ir.FeatureEnd]

          val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(dstFend.classifier.get)).get
          val sel4PortType: String = Util.getSel4TypeName(aadlPortType, performHamrIntegration)

          val queueSize = PropertyUtil.getQueueSize(dstFend, Util.DEFAULT_QUEUE_SIZE)

          val queueName = Util.getEventDataSBQueueName(sel4PortType, queueSize)
          if (!seen.contains(queueName)) {
            auxResourceFiles = auxResourceFiles ++ EventDataQueueTemplate.genSbQueueTypeFiles(sel4PortType, queueSize)

            seen = seen + queueName
          }

          var dstMap = srcQueues.getOrElse(srcId, Map.empty)
          assert(!dstMap.contains(dstId))
          dstMap = dstMap + (dstId ~> QueueObject(queueName, queueSize))
          srcQueues = srcQueues + (srcId ~> dstMap)
        }
      }
    }
  }
  
  def buildMonitors(): Unit = {
    platform match {
      case ActPlatform.SeL4_TB => // 

      //case ActPlatform.SeL4 => // 
      case ActPlatform.SeL4 => return 
      case ActPlatform.SeL4_Only => return  
    }
    
    for (portPath <- symbolTable.outConnections.keys.filter(p => symbolTable.outConnections.get(p).get.size > 0)) {
      var i: Z = 0
      for (connInst <- symbolTable.outConnections.get(portPath).get()) {

        val dst: ir.Component = symbolTable.airComponentMap.get(CommonUtil.getName(connInst.dst.component)).get
        val dstFeature: ir.Feature = symbolTable.airFeatureMap.get(CommonUtil.getName(connInst.dst.feature.get)).get
        
        def handleDataPort(f: ir.FeatureEnd): Unit = {
          val monitorName = Util.getMonitorName(dst, f)
          val interfaceName = Util.getInterfaceName(f)

          val typeName = Util.getClassifierFullyQualified(f.classifier.get)

          val providesVarName = "mon"
          
          val monitor = Component(
            control = F,
            hardware = F,
            name = monitorName,

            mutexes = ISZ(),
            binarySemaphores = ISZ(),
            semaphores = ISZ(),
            dataports = ISZ(),
            emits = ISZ(Emits(name = "monsig", typ = Util.getMonitorNotificationType(f.category))),
            uses = ISZ(),
            consumes = ISZ(),
            provides = ISZ(Provides(name = providesVarName, typ = interfaceName)),
            includes = ISZ(),
            attributes = ISZ(),
            preprocessorIncludes = ISZ(),
            imports = ISZ(st""""../../../${Util.DIR_INTERFACES}/${interfaceName}.idl4"""".render),

            externalEntities = ISZ()
          )

          val inst: Instance = Instance(
            address_space = "", 
            name = StringUtil.toLowerCase(monitorName), 
            component = monitor)

          val paramType: ir.Component = typeMap.get(typeName).get
          val paramTypeName: String = Util.getSel4TypeName(paramType, performHamrIntegration)

          val methods: ISZ[Method] = f.category match {
            case ir.FeatureCategory.EventDataPort => createQueueMethods(paramTypeName)
            case ir.FeatureCategory.DataPort => createReadWriteMethods(paramTypeName)
            case _ =>
              halt(s"not expecting ${f.category}")
          }

          val interface: Procedure = Procedure(
            name = interfaceName,
            methods = methods,
            includes = ISZ(Util.getSbTypeHeaderFilenameForIncludes())
          )

          val connInstName = CommonUtil.getName(connInst.name)

          val implName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/src/${Util.genCImplFilename(monitorName)}"
          val cimplementation: Resource =
            if(f.category == ir.FeatureCategory.DataPort) {
              Util.createResource(implName, StringTemplate.tbMonReadWrite(
                paramTypeName, PropertyUtil.getQueueSize(f, Util.DEFAULT_QUEUE_SIZE), monitorName, Util.getSbTypeHeaderFilenameForIncludes(), preventBadging), T)
            } else {
              Util.createResource(implName, StringTemplate.tbEnqueueDequeue(
                paramTypeName, PropertyUtil.getQueueSize(f, Util.DEFAULT_QUEUE_SIZE), monitorName, Util.getSbTypeHeaderFilenameForIncludes(), preventBadging), T)
            }

          val headerFilename = Util.genCHeaderFilename(monitorName)
          val headerFilePath = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/includes/${headerFilename}"
          val contents = StringTemplate.cHeaderFile(headerFilename, ISZ(), ISZ())
          val cincludes = Util.createResource(headerFilePath, contents, T)

          monitors = monitors + (connInstName ~> 
            TB_Monitor(inst, interface, 
              providesVarName,
              cimplementation, cincludes, i, connInst))
        }

        def buildIhorMonitor(f: ir.FeatureEnd): Unit = {
          val monitorName = Util.getMonitorName(dst, f)
          val interfaceNameReceiver = Util.getInterfaceNameIhor(f, F)
          val interfaceNameSender = Util.getInterfaceNameIhor(f, T)

          val typeName = Util.getClassifierFullyQualified(f.classifier.get)

          val providesReceiverVarName = "mon_receive"
          val providesSenderVarName = "mon_send"

          val paramType = typeMap.get(typeName).get
          val paramTypeName = Util.getSel4TypeName(paramType, performHamrIntegration)

          val monitor = Component(
            control = F,
            hardware = F,
            name = monitorName,

            mutexes = ISZ(Mutex("m")),
            binarySemaphores = ISZ(),
            semaphores = ISZ(),
            dataports = ISZ(),
            emits = ISZ(
              Emits(name = "monsig", typ = Util.MONITOR_EVENT_DATA_NOTIFICATION_TYPE)),
            uses = ISZ(),
            consumes = ISZ(),
            provides = ISZ(
              Provides(name = providesReceiverVarName, typ = interfaceNameReceiver),
              Provides(name = providesSenderVarName, typ = interfaceNameSender)),
            includes = ISZ(),
            attributes = ISZ(),
            preprocessorIncludes = ISZ(),
            imports = ISZ(
              st""""../../../${Util.DIR_INTERFACES}/${interfaceNameReceiver}.idl4"""".render,
              st""""../../../${Util.DIR_INTERFACES}/${interfaceNameSender}.idl4"""".render),

            externalEntities = ISZ()
          )

          val inst: Instance = Instance(address_space = "", name = StringUtil.toLowerCase(monitorName), component = monitor)

          val receiveMethod = Method(
            name = "dequeue",
            parameters = ISZ(
              Parameter(F, Direction.Out, "m", paramTypeName)),
            returnType = Some("bool"))

          val interfaceReceiver: Procedure = Procedure(
            name = interfaceNameReceiver,
            methods = ISZ(receiveMethod),
            includes = ISZ(Util.getSbTypeHeaderFilenameForIncludes())
          )

          val sendMethod = Method(
            name = "enqueue",
            parameters = ISZ(
              Parameter(F, Direction.Refin, "m", paramTypeName)),
            returnType = Some("bool"))

          val interfaceSender: Procedure = Procedure(
            name = interfaceNameSender,
            methods = ISZ(sendMethod),
            includes = ISZ(Util.getSbTypeHeaderFilenameForIncludes())
          )

          val connInstName = CommonUtil.getName(connInst.name)

          val implName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/src/${Util.genCImplFilename(monitorName)}"
          val cimplementation: Resource =
            Util.createResource(implName,
              StringTemplate.tbEnqueueDequeueIhor(paramTypeName,
                PropertyUtil.getQueueSize(f, Util.DEFAULT_QUEUE_SIZE),
                monitorName,
                Util.getSbTypeHeaderFilenameForIncludes(),
                preventBadging),
              T)

          val headerFilename = Util.genCHeaderFilename(monitorName)
          val headerFilePath = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/includes/${headerFilename}"
          val contents = StringTemplate.cHeaderFile(headerFilename, ISZ(), ISZ())
          val cincludes = Util.createResource(headerFilePath, contents, T)

          monitors = monitors + (connInstName ~>
            Ihor_Monitor(inst, interfaceReceiver, interfaceSender,
              providesReceiverVarName, providesSenderVarName,
              cimplementation, cincludes, i, connInst))
        }

        def buildIhorEventMonitor(f: ir.FeatureEnd): Unit = {
          assert(f.category == ir.FeatureCategory.EventPort)
          
          val monitorName = Util.getMonitorName(dst, f)
          val interfaceNameReceiver = Util.getInterfaceNameIhor(f, F)
          val interfaceNameSender = Util.getInterfaceNameIhor(f, T)

          val providesReceiverVarName = "mon_receive"
          val providesSenderVarName = "mon_send"

          val monitor = Component(
            control = F,
            hardware = F,
            name = monitorName,

            mutexes = ISZ(Mutex("m")),
            binarySemaphores = ISZ(),
            semaphores = ISZ(),
            dataports = ISZ(),
            emits = ISZ(
              Emits(name = "monsig", typ = Util.MONITOR_EVENT_DATA_NOTIFICATION_TYPE)),
            uses = ISZ(),
            consumes = ISZ(),
            provides = ISZ(
              Provides(name = providesReceiverVarName, typ = interfaceNameReceiver),
              Provides(name = providesSenderVarName, typ = interfaceNameSender)),
            includes = ISZ(),
            attributes = ISZ(),
            preprocessorIncludes = ISZ(),
            imports = ISZ(
              st""""../../../${Util.DIR_INTERFACES}/${interfaceNameReceiver}.idl4"""".render,
              st""""../../../${Util.DIR_INTERFACES}/${interfaceNameSender}.idl4"""".render),

            externalEntities = ISZ()
          )

          val inst: Instance = Instance(address_space = "", name = StringUtil.toLowerCase(monitorName), component = monitor)

          val isEmptyMethod = Method(
            name = "is_empty",
            parameters = ISZ(),
            returnType = Some("bool"))
          
          val receiveMethod = Method(
            name = "dequeue",
            parameters = ISZ(),
            returnType = Some("bool"))

          val interfaceReceiver: Procedure = Procedure(
            name = interfaceNameReceiver,
            methods = ISZ(isEmptyMethod, receiveMethod),
            includes = ISZ(Util.getSbTypeHeaderFilenameForIncludes())
          )

          val sendMethod = Method(
            name = "enqueue",
            parameters = ISZ(),
            returnType = Some("bool"))

          val interfaceSender: Procedure = Procedure(
            name = interfaceNameSender,
            methods = ISZ(sendMethod),
            includes = ISZ(Util.getSbTypeHeaderFilenameForIncludes())
          )

          val connInstName = CommonUtil.getName(connInst.name)

          val implName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/src/${Util.genCImplFilename(monitorName)}"
          val cimplementation: Resource =
            Util.createResource(
              implName,
              StringTemplate.tbRaiseGetEvents(PropertyUtil.getQueueSize(f, Util.DEFAULT_QUEUE_SIZE), monitorName, preventBadging),
              T)

          val headerFilename = Util.genCHeaderFilename(monitorName)
          val headerFilePath = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/includes/${headerFilename}"
          val headerContents = StringTemplate.cHeaderFile(headerFilename, ISZ(), ISZ())
          val cincludes = Util.createResource(headerFilePath, headerContents, T)

          monitors = monitors + (connInstName ~>
            Ihor_Monitor(inst, interfaceReceiver, interfaceSender,
              providesReceiverVarName, providesSenderVarName,
              cimplementation, cincludes, i, connInst))
        }
        
        connInst.kind match {
          case ir.ConnectionKind.Port =>
            dstFeature.category match {
              case ir.FeatureCategory.DataPort =>
                platform match {
                  case ActPlatform.SeL4_TB => handleDataPort(dstFeature.asInstanceOf[ir.FeatureEnd])

                  //case ActPlatform.SeL4 => handleDataPort(dstFeature.asInstanceOf[ir.FeatureEnd])
                  case ActPlatform.SeL4 => // use new mappings
                  case ActPlatform.SeL4_Only => // use new mappings
                }
                
              case ir.FeatureCategory.EventDataPort =>
                platform match {
                  case ActPlatform.SeL4_TB => handleDataPort(dstFeature.asInstanceOf[ir.FeatureEnd])
                  
                  //case ActPlatform.SeL4 => handleDataPort(dstFeature.asInstanceOf[ir.FeatureEnd])
                  case ActPlatform.SeL4 => buildIhorMonitor(dstFeature.asInstanceOf[ir.FeatureEnd])
                  case ActPlatform.SeL4_Only => buildIhorMonitor(dstFeature.asInstanceOf[ir.FeatureEnd])
                }
                
              case ir.FeatureCategory.EventPort =>
                def evenPort_Sel4_TB_Profile(): Unit = {
                  buildIhorEventMonitor(dstFeature.asInstanceOf[ir.FeatureEnd])
                }
                platform match {
                  case ActPlatform.SeL4_TB => evenPort_Sel4_TB_Profile()

                  //case ActPlatform.SeL4 => evenPort_Sel4_TB_Profile()
                  case ActPlatform.SeL4 => // not using monitors   
                  case ActPlatform.SeL4_Only => // not using monitors   
                }

              case _ =>
                halt(s"not expecting ${dst}")
            }
          case ir.ConnectionKind.Access => // no monitor needed
          case _ =>
            reporter.warn(None(), Util.toolName, s"processInConnections: Not handling ${connInst}")
        }

        i = i + 1
      }
    }
  }

  def createReadWriteMethods(typeName: String): ISZ[Method] = {
    return ISZ(
      Method(name = "is_empty", parameters = ISZ(), returnType = Some("bool")),      
      Method(name = "read", parameters = ISZ(Parameter(F, Direction.Out, "m", typeName)), returnType = Some("bool")),
      Method(name = "write", parameters = ISZ(Parameter(F, Direction.Refin, "m", typeName)), returnType = Some("bool")))
  }

  def createQueueMethods(typeName: String): ISZ[Method] = {
    return ISZ(
      Method(name = "is_empty", parameters = ISZ(), returnType = Some("bool")),
      Method(name = "enqueue", parameters = ISZ(Parameter(F, Direction.Refin, "m", typeName)), returnType = Some("bool")),
      Method(name = "dequeue", parameters = ISZ(Parameter(F, Direction.Out, "m", typeName)), returnType = Some("bool")))
  }

  def getMonitorForConnectionInstance(instance: ir.ConnectionInstance): Option[Monitor] = {
    for(m <- monitors.values if m.ci == instance){
      return Some(m)
    }
    return None[Monitor]()
  }

  def getMonitorForInPort(end: ir.FeatureEnd): Option[Monitor] = {
    val n = CommonUtil.getName(end.identifier)
    for(m <- monitors.values if CommonUtil.getName(m.ci.dst.feature.get) == n) {
      return Some(m)
    }
    return None[Monitor]()
  }

  def processDataTypes(values: ISZ[ir.Component]): ST = {
    if (performHamrIntegration) {
      val defs = ISZ(st"typedef union art_DataContent union_art_DataContent;")
      return StringTemplate.tbTypeHeaderFile(
        filename = Util.getSbTypeHeaderFilenameWithExtension(),
        includes = Some(st"#include <all.h>"),
        defs = defs,
        preventBadging = preventBadging)
    } else {
      val defs = values.filter((v : ir.Component) => TypeUtil.translateBaseType(v.classifier.get.name).isEmpty).
        map((v : ir.Component) => processDataType(v, F))

      return StringTemplate.tbTypeHeaderFile(
        filename = Util.getSbTypeHeaderFilenameWithExtension(),
        includes = None(),
        defs = defs,
        preventBadging = preventBadging)
    }
  }

  def processDataType(c: ir.Component, isField: B): ST = {
    val s: ST =
      if (TypeUtil.isRecordType(c)) {
        val name = Util.getClassifierFullyQualified(c.classifier.get)
        if(isField) {
          st"""$name"""
        } else {
          val fields: ISZ[ST] = c.subComponents.map(sub => {
            val fname: String = CommonUtil.getLastName(sub.identifier)
            if(ISZOps(Util.cKeywords).contains(fname)) {
              reporter.error(None(), Util.toolName, s"The attribute '${fname}' from data type ${name} is a reserved keyword")
            }
            st"${processDataType(sub, T)} ${fname};"
          })

          st"""typedef
              |  struct ${name} {
              |    ${(fields, "\n")}
              |  } ${name};"""
        }
      } else if (TypeUtil.isBaseType(c)) {
        st"${TypeUtil.translateBaseType(c.classifier.get.name)}"
      } else if (isField) {
        st"${Util.getClassifierFullyQualified(c.classifier.get)}"
      } else if (TypeUtil.isMissingType(c)) {
        StringTemplate.tbMissingType()
      } else if (TypeUtil.isEnumDef(c)) {
        val enums = PropertyUtil.getPropertyValues(c.properties, OsateProperties.DATA_MODEL__ENUMERATORS)
        val values: ISZ[String] = enums.map((e: ir.PropertyValue) => {
          e match {
            case ir.ValueProp(value) => value
            case _ =>
              reporter.error(None(), Util.toolName, s"Unexpected enum value: ${e}")
              ""
          }
        })
        val ename = Util.getClassifierFullyQualified(c.classifier.get)
        st"""typedef
            |  enum {${(values, ", ")}} ${ename};"""
      } else if (TypeUtil.isArrayDef(c)) {
        // TODO multidim arrays
        val name = Util.getClassifierFullyQualified(c.classifier.get)
        val dim: Z = TypeUtil.getArrayDimension(c) match {
          case Some(d) => d
          case _ =>
            reporter.error(None(), Util.toolName, s"Array dimension not specified for ${c.classifier.get.name}")
            z"-1"
        }
        val container = Util.getContainerName(name)
        st"""typedef ${TypeUtil.getArrayBaseType(c)} ${name} [${TypeUtil.getArrayDimension(c)}];
            |
            |typedef
            |  struct ${container} {
            |    ${name} f;
            |  } ${container};"""
      } else {
        reporter.error(None(), Util.toolName, s"Unexpected datatype: ${c}")
        st" "
      }


    return s
  }

  def generateGlueCodePortInterfaceMethod(aadlThread: AadlThread, feature: ir.FeatureEnd): Option[C_SimpleContainer] = {

    val component = aadlThread.component
    
    def handleDataPort(): Option[C_SimpleContainer] = {

      def dataport_TB_Profile() : Option[C_SimpleContainer] = {
        val (suffix, mod) : (String, String) = feature.direction match {
          case ir.Direction.In => ("read", "")
          case ir.Direction.Out => ("write", "const ")
          case x => halt(s"Unexpected direction: ${x}")
        }
        val name = Util.genMonitorFeatureName(feature, None[Z]())
        val paramType = Util.getSel4TypeName(
          typeMap.get(Util.getClassifierFullyQualified(feature.classifier.get)).get, performHamrIntegration)

        val inter = Some(st"""bool ${name}_${suffix}(${mod}${paramType} * ${name});""")
        val impl: Option[ST] =
          if(suffix == "write") {
            symbolTable.outConnections.get(CommonUtil.getName(feature.identifier)) match {
              case Some(conns) =>
                var accum: ISZ[ST] = ISZ()
                var i = 0
                val result = Util.brand("result")

                while (i < conns.size) {
                  accum = accum :+ st"""${result} &= ${name}${i}_${suffix}((${paramType} *) ${name});"""
                  i = i + 1
                }

                val methodName = s"${name}_${suffix}"
                Some(st"""/************************************************************************
                         | * ${methodName}:
                         | * Invoked from user code in the local thread.
                         | *
                         | * This is the function invoked by the local thread to make a
                         | * call to write to a remote data port.
                         | *
                         | * XXX: When simulating fan out, the caller of this function will only
                         | * receive a positive response when all enqueues are successful. When a
                         | * negative response is received it only indicates that at least one
                         | * enqueue attempt failed.
                         | *
                         | ************************************************************************/
                         |bool ${methodName}(${mod}${paramType} * ${name}){
                         |  bool ${result} = true;
                         |  ${(accum, "\n")}
                         |  return ${result};
                         |}""")
              case _ => None[ST]()
            }
          } else {
            None[ST]()
          }

        return Some(C_SimpleContainer(
          cIncludes = ISZ(),
          cInterface = inter,
          cImplementation = impl,
          preInits = None(), 
          postInits = None(), 
          drainQueues = None()))
      } 
      
      def dataport_SB_Profile() : Option[C_SimpleContainer] = {
        val samplingPort: SamplingPortInterface = getSamplingPort(feature)

        var interfaces: ISZ[ST] = ISZ()
        var implementations: ISZ[ST] = ISZ()
        var preInits: ISZ[ST] = ISZ()
        var postInits: ISZ[ST] = ISZ()
        
        preInits = preInits :+
          StringTemplate.sbSamplingPortInitialise(samplingPort, feature)
        
        feature.direction match {
          case ir.Direction.In =>
            interfaces = interfaces :+
              StringTemplate.sbSamplingPortGetterInterface(samplingPort, feature)

            implementations = implementations :+
              StringTemplate.sbSamplingPortGetterImplementation(samplingPort, feature)
            
          case ir.Direction.Out =>
            interfaces = interfaces :+
              StringTemplate.sbSamplingPortSetterInterface(samplingPort, feature)

            
            implementations = implementations :+
              StringTemplate.sbSamplingPortSetterImplementation(samplingPort, feature)
            
          case _ => halt(s"Unexpected direction ${feature}")
        }
        
        val interface: Option[ST] = if(interfaces.isEmpty) None() else Some(st"${(interfaces, "\n")}")
        val implementation: Option[ST] = if(implementations.isEmpty) None() else Some(st"${(implementations, "\n")}")
        val preInit: Option[ST] = if(preInits.isEmpty) None() else Some(st"${(preInits, "\n")}")
        val postInit: Option[ST] = if(postInits.isEmpty) None() else Some(st"${(postInits, "\n")}")
          
        return Some(C_SimpleContainer(
          cIncludes = ISZ(), 
          cInterface = interface, 
          cImplementation = implementation, 
          preInits = preInit,
          postInits = postInit,
          drainQueues= None()))
      }
      
      val ret: Option[C_SimpleContainer] = platform match {
        case ActPlatform.SeL4_TB => dataport_TB_Profile()
        case ActPlatform.SeL4 => dataport_SB_Profile()
        case ActPlatform.SeL4_Only => dataport_SB_Profile()
      }
      
      return ret
    }

    def handleEventDataPort(): Option[C_SimpleContainer] = {
      def handleEventData_SB_GlueCode_Profile(): Option[C_SimpleContainer] = {
        val featurePath = CommonUtil.getName(feature.identifier)
        val fid = CommonUtil.getLastName(feature.identifier)
        val featureName = Util.brand(fid)
  
        val aadlPortType = typeMap.get(Util.getClassifierFullyQualified(feature.classifier.get)).get
        val sel4TypeName = Util.getSel4TypeName(aadlPortType, performHamrIntegration)
          
        val ret: Option[C_SimpleContainer] = feature.direction match {
          case ir.Direction.In =>
            
            if(symbolTable.getInConnections(featurePath).isEmpty){
              return None()
            }
  
            val queueSize = PropertyUtil.getQueueSize(feature, Util.DEFAULT_QUEUE_SIZE)
            val queueHeaderName = Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, queueSize)
            val queueName = Util.getEventDataSBQueueName(sel4TypeName, queueSize)
  
            val featureQueueName = Util.getEventDataSBQueueDestFeatureName(fid)
            val featureNotificationName = Util.genSeL4NotificationName(feature, T)

            var preInits: ISZ[ST] = ISZ()
            var postInits: ISZ[ST] = ISZ()
            var drainQueue: Option[(ST,ST)] = None()
            var cInterfaces: ISZ[ST] = ISZ()
            var cImplementations: ISZ[ST] = ISZ()

            val counterVar = "numDropped"
            val counterType = Util.SB_EVENT_COUNTER_TYPE
            
            val recvQueueFeatureName = Util.getEventData_SB_RecvQueueFeatureName(fid)
            val recvQueueName = Util.getEventData_SB_RecvQueueName(sel4TypeName, queueSize)
            val recvQueueType = Util.getEventData_SB_RecvQueueTypeName(sel4TypeName, queueSize)

            val dequeuePollMethodName = s"${featureName}_dequeue_poll"
            val dequeueMethodName = s"${featureName}_dequeue"
            val dequeueInfrastructureMethodName = s"${queueName}_dequeue"
            
            val isEmptyMethodName = s"${featureName}_is_empty"
            val isEmptyInfrastructureMethodName = s"${queueName}_is_empty"
            
            cInterfaces = cInterfaces :+ st"bool ${dequeueMethodName}(${sel4TypeName} *);"
            
            val baseCimpl = st"""${recvQueueType} ${recvQueueFeatureName};
                                 |
                                 |/************************************************************************
                                 | * ${dequeuePollMethodName}:
                                 | ************************************************************************/
                                 |bool ${dequeuePollMethodName}(${counterType} *${counterVar}, ${sel4TypeName} *data) {
                                 |  return ${dequeueInfrastructureMethodName}(&${recvQueueFeatureName}, ${counterVar}, data);
                                 |}
                                 |                     
                                 |/************************************************************************
                                 | * ${dequeueMethodName}:
                                 | ************************************************************************/
                                 |bool ${dequeueMethodName}(${sel4TypeName} *data) {
                                 |  ${counterType} ${counterVar};
                                 |  return ${dequeuePollMethodName}(&${counterVar}, data);
                                 |}
                                 |
                                 |/************************************************************************
                                 | * ${isEmptyMethodName}:
                                 | *
                                 | * Helper method to determine if infrastructure port has received new
                                 | * events
                                 | ************************************************************************/
                                 |bool ${isEmptyMethodName}(){
                                 |  return ${isEmptyInfrastructureMethodName}(&${recvQueueFeatureName});
                                 |}"""
            
            cImplementations = cImplementations :+ baseCimpl
            
            if(aadlThread.isSporadic()) {
              val handlerName = s"${featureNotificationName}_handler"
              val regCallback = s"${featureNotificationName}_reg_callback"
              
              cImplementations = cImplementations :+
                StringTemplate.cEventNotificationHandler(handlerName, regCallback, featureName)
              
              postInits = postInits :+ StringTemplate.cRegCallback(handlerName, regCallback, feature)
              
              if(!performHamrIntegration) {
                Util.getComputeEntrypointSourceText(feature.properties) match {
                  case Some(userEntrypointMethodName) =>
    
                    val varName = featureName
    
                    val userEntrypointName = Util.getUserEventEntrypointMethodName(component, feature)
                    
                    drainQueue = Some((
                      st"",
                      st"""{
                          |  ${sel4TypeName} ${varName};
                          |  while (${dequeueMethodName}((${sel4TypeName} *) &${varName})) {
                          |    ${userEntrypointName}(&${varName});
                          |  }
                          |}"""))
    
                    cInterfaces = cInterfaces :+ st"void ${userEntrypointMethodName}(const ${sel4TypeName} *);"
      
                    val invokeHandler = s"${userEntrypointMethodName}((${sel4TypeName} *) in_arg);"
                    val entrypoint = st"""/************************************************************************
                                      | * ${userEntrypointName}:
                                      | *
                                      | * This is the function invoked by an active thread dispatcher to
                                      | * call to a user-defined entrypoint function.  It sets up the dispatch
                                      | * context for the user-defined entrypoint, then calls it.
                                      | *
                                      | ************************************************************************/
                                      |void ${userEntrypointName}(const ${sel4TypeName} * in_arg) {
                                      |  ${invokeHandler}
                                      |}
                                      |"""
    
                    cImplementations = cImplementations :+ entrypoint
                    
                  case _ =>
                }
              }
            }
            
            val cIncludes: ISZ[ST] = ISZ(st"""#include <${queueHeaderName}>
                                             |#include <${Util.SB_COUNTER_HEADER_FILENAME}>""")
            
            preInits = preInits :+ st"""// initialise data structure for incoming event data port ${fid}
                                       |${recvQueueName}_init(&${recvQueueFeatureName}, ${featureQueueName});"""
            
            val cInterface: Option[ST] = if(cInterfaces.isEmpty) None() else Some(st"${(cInterfaces, "\n\n")}")
            val cImplementation: Option[ST] = if(cImplementations.isEmpty) None() else Some(st"${(cImplementations, "\n\n")}")
            val preInit: Option[ST] = if(preInits.isEmpty) None() else Some(st"${(preInits, "\n\n")}")
            val postInit: Option[ST] = if(postInits.isEmpty) None() else Some(st"${(postInits, "\n\n")}")
            
            Some(C_SimpleContainer(
              cIncludes = cIncludes, 
              cInterface = cInterface, 
              cImplementation = cImplementation, 
              preInits = preInit,
              postInits = postInit, 
              drainQueues = drainQueue))
            
          case ir.Direction.Out =>
            
            val dsts: Map[String, QueueObject] = srcQueues.get(featurePath).get
  
            var cIncludes: ISZ[ST] = ISZ()
  
            var preInits: ISZ[ST] = ISZ()
            var postInits: ISZ[ST] = ISZ()
            
            val enqueueEntries: ISZ[ST] = dsts.valueSet.elements.map(qo => {
  
              val queueHeaderName = Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, qo.queueSize)
              val queueName = Util.getEventDataSBQueueName(sel4TypeName, qo.queueSize)
  
              val featureQueueName = Util.getEventDataSBQueueSrcFeatureName(fid, qo.queueSize)
              val featureNotificationName = Util.genSeL4NotificationQueueName(feature, qo.queueSize)
              
              cIncludes = cIncludes :+ st"#include <${queueHeaderName}>"
              preInits = preInits :+ st"""// initialise data structure for outgoing event data port ${fid}
                                         |${queueName}_init(${featureQueueName});"""
              
              st"""${queueName}_enqueue(${featureQueueName}, (${sel4TypeName}*) data);
                  |${featureNotificationName}_emit();
                  |"""
            })
            
            val methodName = s"${featureName}_enqueue"
  
            val interface = st"""bool ${methodName}(const ${sel4TypeName} *);"""
  
            val st = st"""bool ${methodName}(const ${sel4TypeName} *data) {
                         |  ${(enqueueEntries, "\n")}
                         |  return true;
                         |}"""
            
            val cInterface: Option[ST] = Some(interface)
            val cImplementation: Option[ST] = Some(st)
            val preInit: Option[ST] = if(preInits.isEmpty) None() else Some(st"${(preInits, "\n\n")}")
            val postInit: Option[ST]= if(postInits.isEmpty) None() else Some(st"${(postInits, "\n\n")}")
            val drainQueues: Option[(ST,ST)] = None()
  
            Some(C_SimpleContainer(
              cIncludes = cIncludes,
              cInterface = cInterface,
              cImplementation = cImplementation,
              preInits = preInit, 
              postInits = postInit,
              drainQueues = drainQueues))
              
          case x => halt(s"Unexpected direction: ${x}")
        }
        return ret
      }
      
      def handleEventData_TB_GlueCode_Profile(): Option[C_SimpleContainer] = {
       
        val interfaceId = Util.genMonitorFeatureName(feature, None[Z]())
        val paramType = Util.getSel4TypeName(typeMap.get(Util.getClassifierFullyQualified(feature.classifier.get)).get, performHamrIntegration)
        
        var interfaces: ISZ[ST] = ISZ()
        var drainQueue: Option[(ST, ST)] = None[(ST, ST)]()
          
        val impl: Option[ST] = feature.direction match {
          case ir.Direction.Out =>
            symbolTable.outConnections.get(CommonUtil.getName(feature.identifier)) match {
              case Some(conns) =>
                
                // create an enqueue method for user that handles fan outs
                
                val genMethodName = s"${interfaceId}_enqueue"
  
                val methodSig = st"bool ${genMethodName}(const ${paramType} * ${interfaceId})"
  
                interfaces = interfaces :+ st"${methodSig};"
  
                var fanOuts: ISZ[ST] = ISZ()
                var i = 0
                val result = Util.brand("result")
                while(i < conns.size) {
                  fanOuts = fanOuts :+ st"""${result} &= ${interfaceId}${i}_enqueue((${paramType} *) ${interfaceId});"""
                  i = i + 1
                }
  
                Some(st"""/************************************************************************
                         | * ${genMethodName}:
                         | * Invoked from user code in the local thread.
                         | *
                         | * This is the function invoked by the local thread to make a
                         | * call to write to a remote event data port.
                         | *
                         | * XXX: When simulating fan out, the caller of this function will only
                         | * receive a positive response when all enqueues are successful. When a
                         | * negative response is received it only indicates that at least one
                         | * enqueue attempt failed.
                         | *
                         | ************************************************************************/
                         |${methodSig}{
                         |  bool ${result} = true;
                         |  ${(fanOuts, "\n")}
                         |  return ${result};
                         |}""")
                
              case _ => None[ST]()
            }
          case ir.Direction.In =>
            val monitorDequeueMethodName = s"${interfaceId}_dequeue"
  
            val methodSig = st"bool ${monitorDequeueMethodName}(${paramType} * ${interfaceId})"
  
            interfaces = interfaces :+ st"${methodSig};"
  
            if(aadlThread.isSporadic() && !performHamrIntegration) {
              val eventDataEntryPointCode: Option[ST] = Util.getComputeEntrypointSourceText(feature.properties) match {
                case Some(userEventHandlerMethodName) =>
                  val simpleName = CommonUtil.getLastName(feature.identifier)
                  val varName = Util.brand(simpleName)
                  val userEntrypointName = Util.getUserEventEntrypointMethodName(component, feature)
  
                  drainQueue = Some((st"""${paramType} ${varName};""",
                    st"""while (${monitorDequeueMethodName}((${paramType} *) &${varName})) {
                        |  ${userEntrypointName}(&${varName});
                        |}"""))
  
                  interfaces = interfaces :+ st"void ${userEventHandlerMethodName}(const ${paramType} * in_arg);"
  
                  Some(st"""/************************************************************************
                           | * ${userEntrypointName}:
                           | *
                           | * This is the function invoked by an active thread dispatcher to
                           | * call to a user-defined entrypoint function.  It sets up the dispatch
                           | * context for the user-defined entrypoint, then calls it.
                           | *
                           | ************************************************************************/
                           |void ${userEntrypointName}(const ${paramType} * in_arg) {
                           |  ${userEventHandlerMethodName}((${paramType} *) in_arg);
                           |}""")
  
                case _ => None()
              }
  
              eventDataEntryPointCode
  
            } else {
              None()
            }
          case x => halt(s"Unexpected direction: ${x}")  
        }
  
        val inter: Option[ST] = if(interfaces.isEmpty) None() else Some(st"${(interfaces, "\n\n")}")
        
        return Some(C_SimpleContainer(cIncludes = ISZ(), cInterface = inter, cImplementation = impl,
          preInits = None(), postInits = None(), drainQueues = drainQueue))
      }

      val ret: Option[C_SimpleContainer] = platform match {
        case ActPlatform.SeL4_TB => handleEventData_TB_GlueCode_Profile()

        //case ActPlatform.SeL4 => handleEventData_TB_GlueCode_Profile()
        case ActPlatform.SeL4 => handleEventData_SB_GlueCode_Profile()
        case ActPlatform.SeL4_Only => handleEventData_SB_GlueCode_Profile()
        case _ => None()
      }
      return ret
    }
    
    def handleEventPort() : Option[C_SimpleContainer] = {
      
      def handleEventPort_TB_Profile(): Option[C_SimpleContainer] = {
        
        val featureName = CommonUtil.getLastName(feature.identifier)
        val monitorEnqueueDequeueMethodName = Util.getEventPortSendReceiveMethodName(feature)

        var interfaces: ISZ[ST] = ISZ()
        var implementations: ISZ[ST] = ISZ()
        var preInit: Option[ST] = None[ST]()
        var drainQueue: Option[(ST, ST)] = None[(ST, ST)]()
        
        feature.direction match {
          case ir.Direction.In =>
            
            interfaces = interfaces :+ st"bool ${monitorEnqueueDequeueMethodName}(void);"
            
            if(aadlThread.isSporadic()) {
              
              val callback = s"${Util.genSeL4NotificationName(feature, F)}_handler"
              val callback_reg = Util.brand(s"${featureName}_notification_reg_callback")
              val regCallback = st"CALLBACKOP(${callback_reg}(${callback}, NULL));"

              preInit = Some(regCallback)

              implementations = implementations :+
                st"""/************************************************************************
                    | * ${callback}:
                    | * Invoked by: seL4 notification callback
                    | *
                    | * This is the function invoked by an seL4 notification callback that  
                    | * dispatches the active-thread due to the arrival of an event on 
                    | * its ${featureName} event port
                    | *
                    | ************************************************************************/
                    |static void ${callback}(void *_ UNUSED){
                    |  MUTEXOP(${StringTemplate.SEM_POST}());
                    |  ${regCallback}
                    |}"""
              
              if(!performHamrIntegration) {
                Util.getComputeEntrypointSourceText(feature.properties) match {
  
                  case Some(computeEntryPointSourceText) =>
                    val simpleName = CommonUtil.getLastName(feature.identifier)
                    val entrypointMethodName = s"${Util.brand("entrypoint")}_${Util.getClassifier(component.classifier.get)}_${simpleName}"
  
                    drainQueue = Some(
                      (st"",
                        st"""while(${monitorEnqueueDequeueMethodName}()){
                            |  ${entrypointMethodName}();
                            |}""")
                    )
  
                    interfaces = interfaces :+ st"void ${computeEntryPointSourceText}(void);"
  
                    val invokeHandler: String = s"${computeEntryPointSourceText}();"
  
                    implementations = implementations :+
                      st"""/************************************************************************
                          | *  ${entrypointMethodName}
                          | *
                          | * This is the function invoked by an active thread dispatcher to
                          | * call to a user-defined entrypoint function.  It sets up the dispatch
                          | * context for the user-defined entrypoint, then calls it.
                          | *
                          | ************************************************************************/
                          |void ${entrypointMethodName}(void){
                          |  ${invokeHandler}
                          |}"""
                
                  case _ => 
                }
              }
            }
            
          case ir.Direction.Out =>

            var fanOuts = ISZ[ST]()

            interfaces = interfaces :+ st"bool ${monitorEnqueueDequeueMethodName}(void);"
            
            symbolTable.outConnections.get(CommonUtil.getName(feature.identifier)) match {
              case Some(conns) =>
                var i = 0
                while (i < conns.size) {
                  val interfaceName = Util.brand(s"${featureName}${i}")
                  fanOuts = fanOuts :+ st"""${interfaceName}_enqueue();"""
                  i = i + 1
                }
              case _ =>
            }

            implementations = implementations :+ 
              st"""/************************************************************************
                  | * ${monitorEnqueueDequeueMethodName}
                  | * Invoked from user code in the local thread.
                  | *
                  | * This is the function invoked by the local thread to make a
                  | * call to send to a remote event port.
                  | *
                  | ************************************************************************/
                  |bool ${monitorEnqueueDequeueMethodName}(void) {
                  |  ${(fanOuts, "\n")}
                  |  return true;
                  |}
                  |"""
              
          case x => halt(s"Not handling direction ${x}")    
        }
        
        val interface: Option[ST] = if(interfaces.isEmpty) None() else Some(st"${(interfaces, "\n\n")}")
        val implementation: Option[ST] = if(implementations.isEmpty) None() else Some(st"${(implementations, "\n\n")}")
        
        return Some(C_SimpleContainer(cIncludes = ISZ(), cInterface = interface, cImplementation = implementation, 
          preInits = preInit, postInits = None(), drainQueues = drainQueue))
      }
      
      def handleEventPort_SB_Profile(): Option[C_SimpleContainer] = {
        val featureName = CommonUtil.getLastName(feature.identifier)
        val compName = Util.getClassifier(component.classifier.get)
        val counterName = Util.getEventSBCounterName(featureName)
                
        var preInits: ISZ[ST] = ISZ()
        var postInits: ISZ[ST] = ISZ()
        var interfaces: ISZ[ST] = ISZ()
        var implementations: ISZ[ST] = ISZ()
        var drainQueues: ISZ[ST] = ISZ()
        
        feature.direction match {
          case ir.Direction.In =>
            
            val receivedEventsVarName = Util.brand(s"${featureName}_received_events")
            val lastCounterVarName = Util.brand(s"${featureName}_last_counter")

            val userPortInterfaceMethodName = Util.getEventPortSendReceiveMethodName(feature)

            interfaces = interfaces :+ st"""bool ${userPortInterfaceMethodName}(void);"""
            
            val isEmptyMethodName: String = Util.brand(s"${featureName}_is_empty")
            
            implementations = implementations :+ 
              st"""/************************************************************************
                  | *
                  | * Static variables and queue management functions for event port:
                  | *     ${featureName}
                  | *
                  | ************************************************************************/
                  |static ${Util.SB_EVENT_COUNTER_TYPE} ${receivedEventsVarName} = 0;
                  |static ${Util.SB_EVENT_COUNTER_TYPE} ${lastCounterVarName} = 0;
                  |
                  |/************************************************************************
                  | * ${userPortInterfaceMethodName}:
                  | * Invoked from local active thread.
                  | *
                  | * This is the function invoked by the active thread to decrement the
                  | * input event index.
                  | *
                  | ************************************************************************/
                  |bool ${userPortInterfaceMethodName}() {
                  |  if(${receivedEventsVarName} > 0) {
                  |    ${receivedEventsVarName}--;
                  |    return true;
                  |  } else {
                  |    return false;
                  |  }
                  |}
                  |
                  |/************************************************************************
                  | * ${isEmptyMethodName};
                  | * 
                  | * Helper method to determine if infrastructure port has received 
                  | * new events
                  | *
                  | ************************************************************************/
                  |bool ${isEmptyMethodName}() {
                  |  return *${counterName} == ${lastCounterVarName};
                  |}"""

            val freezeEventsMethodName: String = StringTemplate.samplingPortFreezeMethodName(feature)
            drainQueues = drainQueues :+ st"${freezeEventsMethodName}();"
            
            val currentCounter = s"current_${counterName}"
            val queueSize = PropertyUtil.getQueueSize(feature, Util.DEFAULT_QUEUE_SIZE)

            val freezeEvents: ST =
              st"""void ${freezeEventsMethodName}() {
                  |  ${Util.SB_EVENT_COUNTER_TYPE} ${currentCounter};
                  |
                  |  ${receivedEventsVarName} = 0; // drop any events not handled during last dispatch
                  |
                  |  // get current shared counter value
                  |  ${currentCounter} = *${counterName};
                  |
                  |  // Acquire memory fence - ensure preceding read occurs before any subsequent read or write
                  |  ${counterName}_acquire();
                  |
                  |  // NOTE: Counters can wrap, so we must use != below instead of >
                  |  while(${currentCounter} != ${lastCounterVarName}){
                  |    ${lastCounterVarName}++;
                  |    ${receivedEventsVarName}++;
                  |  }
                  |
                  |  if(${receivedEventsVarName} > 0) {
                  |  
                  |    // ${featureName}'s queue size is ${queueSize}
                  |    if(${receivedEventsVarName} > ${queueSize}) {
                  |      //printf("${compName}: dropping %i event(s) from incoming event port ${featureName}\n", (${receivedEventsVarName} - ${queueSize}));
                  |    
                  |      // drop events
                  |      ${receivedEventsVarName} = ${queueSize};
                  |    }
                  |  }
                  |}"""
            
            implementations = implementations :+ freezeEvents
            
            if(aadlThread.isSporadic()) {
              
              val entrypointMethodName = s"${Util.brand("entrypoint")}_${Util.getClassifier(component.classifier.get)}_${featureName}"
              
              val callback = s"${Util.genSeL4NotificationName(feature, F)}_handler"
              val callback_reg = Util.brand(s"${featureName}_reg_callback")
  
              implementations = implementations :+
                StringTemplate.cEventNotificationHandler(callback, callback_reg, featureName)
  
              postInits = postInits :+ StringTemplate.cRegCallback(callback, callback_reg, feature)
              
              if(!performHamrIntegration){
                Util.getComputeEntrypointSourceText(feature.properties) match {
                  case Some(computeEntrypointSourceText) =>
  
                    drainQueues = drainQueues :+ 
                        st"""{ 
                            |  if(${receivedEventsVarName} > 0) {
                            |    // dequeue one event and call the event handler
                            |    ${userPortInterfaceMethodName}();
                            |    ${entrypointMethodName}();
                            |  }
                            |}"""
    
                    interfaces = interfaces :+ st"void ${computeEntrypointSourceText}(void);"
    
                    implementations = implementations :+ 
                      st"""
                          |/************************************************************************
                          | *  ${entrypointMethodName}
                          | *
                          | * This is the function invoked by an active thread dispatcher to
                          | * call to a user-defined entrypoint function.  It sets up the dispatch
                          | * context for the user-defined entrypoint, then calls it.
                          | *
                          | ************************************************************************/
                          |void ${entrypointMethodName}(void){
                          |  ${computeEntrypointSourceText}();
                          |}"""
                    
                  case _ => 
                }
              }
            }
          case ir.Direction.Out =>

            val sendMethodName = Util.getEventPortSendReceiveMethodName(feature)
            val emit = Util.brand(s"${featureName}_emit()")

            interfaces = interfaces :+ st"bool ${sendMethodName}(void);"
            
            preInits = preInits :+ st"""// initialise shared counter for event port ${featureName}
                                       |*${counterName} = 0;""" 
            
            implementations = implementations :+ 
              st"""/************************************************************************
                  | * ${sendMethodName}
                  | * Invoked from user code in the local thread.
                  | *
                  | * This is the function invoked by the local thread to make a
                  | * call to send to a remote event port.
                  | *
                  | ************************************************************************/
                  |bool ${sendMethodName}(void) {
                  |  // ${counterName} is a dataport (shared memory) that is written by the sender 
                  |  // and read by the receiver(s). This counter is monotonicly increasing, 
                  |  // but can wrap.
                  |  (*${counterName})++;
                  |  
                  |  // Release memory fence - ensure subsequent write occurs after any preceeding read or write
                  |  ${counterName}_release();
                  |  
                  |  ${emit};
                  |  
                  |  return true;
                  |}
                  |"""
            
          case _ => halt(s"Unexpected direction ${feature.direction}")
        }

        val interface: Option[ST] = if(interfaces.isEmpty) None() else Some(st"${(interfaces, "\n\n")}")
        val implementation: Option[ST] = if(implementations.isEmpty) None() else Some(st"${(implementations, "\n\n")}")
        val cIncludes: ISZ[ST] = ISZ(st"#include ${Util.getSbCounterFilenameForIncludes()}")
        val preInit: Option[ST] = if(preInits.isEmpty) None() else Some(st"${(preInits, "\n\n")}")
        val postInit: Option[ST] = if(postInits.isEmpty) None() else Some(st"${(postInits, "\n\n")}")
        val drainQueue: Option[(ST, ST)] = Some((st"", st"${(drainQueues, "\n\n")}"))

        return Some(C_SimpleContainer(
          cIncludes = cIncludes,
          cInterface = interface,
          cImplementation = implementation,
          preInits = preInit,
          postInits = postInit,
          drainQueues = drainQueue))
      }
      
      val ret: Option[C_SimpleContainer] = platform match {
        case ActPlatform.SeL4_TB => handleEventPort_TB_Profile()

        //case ActPlatform.SeL4 => handleEventPort_TB_Profile()
        case ActPlatform.SeL4 => handleEventPort_SB_Profile()
        case ActPlatform.SeL4_Only => handleEventPort_SB_Profile()
      }
      return ret
    }
    
    val ret: Option[C_SimpleContainer] = feature.category match {
      case ir.FeatureCategory.DataPort => handleDataPort()
      case ir.FeatureCategory.EventPort => handleEventPort()
      case ir.FeatureCategory.EventDataPort => handleEventDataPort()
      case _ => None[C_SimpleContainer]()
    }
    return ret
  }

  def genComponentTypeInterfaceFile(component: ir.Component, sts: ISZ[ST]): Resource = {
    val name = Util.getClassifier(component.classifier.get)
    val compTypeHeaderFilename = Util.genCHeaderFilename(Util.brand(name))

    val contents = StringTemplate.cHeaderFile(
      filename = compTypeHeaderFilename,
      includes = ISZ(Util.getSbTypeHeaderFilenameForIncludes()),
      entries = sts
    )

    return Util.createResource(
      path = s"${Util.DIR_COMPONENTS}/${name}/includes/${compTypeHeaderFilename}",
      contents = contents,
      overwrite = T)
  }

  def genComponentTypeImplementationFile(aadlThread: AadlThread,
                                        
                                         localCIncludes: ISZ[ST], 
                                         blocks: ISZ[ST], 
                                         preInits: ISZ[ST], 
                                         postInits: ISZ[ST],

                                         gcRunInitStmts: ISZ[ST],
                                         gcRunPreLoopStmts: ISZ[ST],
                                         gcRunLoopStartStmts: ISZ[ST],
                                         gcRunLoopMidStmts: ISZ[ST],
                                         gcRunLoopEndStmts: ISZ[ST],

                                         containsFFIs: B): Resource = {

    val path = PathUtil.getComponentSourcePath(aadlThread)
    val name = Util.getClassifier(aadlThread.component.classifier.get)
    val componentHeaderFilename = Util.genCHeaderFilename(Util.brand(name))
    val componentImplFilename = Util.genCImplFilename(Util.brand(name))

    val runMethod: ST = StringTemplate.runMethod(
          locals = ISZ(),
          initStmts = gcRunInitStmts,
          preLoopStmts = gcRunPreLoopStmts,
          loopStartStmts = gcRunLoopStartStmts,
          loopBodyStmts = gcRunLoopMidStmts,
          loopEndStmts = gcRunLoopEndStmts,
          postLoopStmts = ISZ(),
          containsFFIs = containsFFIs)

    val glueCodeImpl: ST =  StringTemplate.componentTypeImpl(
      componentHeaderFilename = componentHeaderFilename,
      includes = localCIncludes,
      blocks = blocks,
      preInits = preInits,
      postInits = postInits,
      runMethod = runMethod)

    return Util.createResource(
      path = s"${path}/${componentImplFilename}",
      contents = glueCodeImpl,
      overwrite = T)
  }

  def buildTransportMechanisms(sys : ir.Component): B = {
   
    if(!hasErrors()) {
      buildMonitors()
    }
    
    if(!hasErrors()){
      buildSBQueues()  
    }
    
    if(!hasErrors()) {
      buildSamplingPortInterfaces()
    }
    
    return !hasErrors()
  }


  def resolveSharedDataFeatures(conns: ISZ[ir.ConnectionInstance]): Unit = {
    val dataConnections = conns.filter(f => f.kind == ir.ConnectionKind.Access).filter(
      f => symbolTable.airFeatureMap.get(CommonUtil.getName(f.dst.feature.get)).get.category == ir.FeatureCategory.DataAccess)

    for(conn <- dataConnections) {
      val srcComp = symbolTable.airComponentMap.get(CommonUtil.getName(conn.src.component)).get

      if(srcComp.category != ir.ComponentCategory.Data) {
        reporter.error(None(), Util.toolName, s"${CommonUtil.getLastName(conn.src.component)} is not a data component")
      }

      val (dataKey, dstFeatureName) = getPortConnectionNames(conn)
      sharedData.get(dataKey) match {
        case Some(sd) =>
          val dstComp = symbolTable.airComponentMap.get(CommonUtil.getName(conn.dst.component)).get
          val ownerId = CommonUtil.getName(sd.owner.identifier)
          val dstId = CommonUtil.getName(dstComp.identifier)

          if(ownerId == dstId) {
            val _f = dstComp.features.filter(f => CommonUtil.getName(f.identifier) == CommonUtil.getName(conn.dst.feature.get))
            if(_f.size != 1) {
              reporter.error(None(), Util.toolName, s"There are ${_f.size} matching features for ${CommonUtil.getName(conn.dst.feature.get)}, expecting only 1.")
            } else if(!_f(0).isInstanceOf[ir.FeatureAccess]) {
              reporter.error(None(), Util.toolName, s"${CommonUtil.getName(conn.dst.feature.get)} is not a FeatureAccess.")
            } else {
              // add the owner's feature
              sharedData = sharedData + (dataKey ~> SharedData(sd.owner, Some(_f(0).asInstanceOf[ir.FeatureAccess]), sd.typ, sd.subcomponentId))
            }
          }
        case _ =>
          reporter.error(None(), Util.toolName, s"Could not find data subcomponent: ${dataKey}")
      }
    }
  }
 
  def sortData(data: ISZ[ir.Component]): ISZ[ir.Component] = {
    def u(_c:ir.Component): String = { return Util.getClassifierFullyQualified(_c.classifier.get) }

    // build dependence graph so that required data types are processed first
    var graph: Graph[ir.Component, String] = Graph.empty
    for(d <- data){
      graph = graph * d
      if(TypeUtil.isRecordType(d)) {
        for (s <- d.subComponents) {
          val pair = (d, typeMap.get(u(s)).get)
          graph = graph + pair
        }
      } else if(TypeUtil.isArrayDef(d)) {
        val pair = (d, typeMap.get(TypeUtil.getArrayBaseType(d).get).get)
        graph = graph + pair
      } else {
        if(!(TypeUtil.isBaseType(d) || TypeUtil.isEnumDef(d) || TypeUtil.isMissingType(d))) {
          reporter.error(None(), Util.toolName, s"Unexpected data type ${d}")
        }
      }
    }

    /*
    def rx(r: String): String = { return StringUtil.replaceAll(r, "*", "star") }
    val nodes: ISZ[String] = graph.nodes.keys.map(c => s"${rx(u(c))}")
    val edges = graph.allEdges.map(e => st"${rx(u(e.source))} -> ${rx(u(e.dest))}")
    val g =st"""digraph {
               |  ${(nodes, ";\n")}
               |
               |  ${(edges, ";\n")}
               |}"""
    println(g.render)
    */

    var sorted: ISZ[ir.Component] = ISZ()
    def sortByClassifier(s: ISZ[ir.Component]): ISZ[ir.Component] = { return ISZOps(s).sortWith((a,b) => u(a) < u(b)) }
    def sortDependents(c : ir.Component): Unit = {
      if(ISZOps(sorted).contains(c)){ return }
      for (dependent <- sortByClassifier(graph.outgoing(c).map(m => m.dest))) {
        sortDependents(dependent)
      }
      sorted = sorted :+ c
    }
    for (r <- sortByClassifier(graph.nodes.keys.filter(k => graph.incoming(k).size == z"0"))) {
      sortDependents(r)
    }
    return sorted
  }
  
  def getPortConnectionNames(c: ir.ConnectionInstance): (String, String) = {
    val src = symbolTable.airComponentMap.get(CommonUtil.getName(c.src.component)).get
    val dst = symbolTable.airComponentMap.get(CommonUtil.getName(c.dst.component)).get

    val ret: (String, String) = (src.category, dst.category) match {
      case (ir.ComponentCategory.Thread, ir.ComponentCategory.Thread) =>
        (CommonUtil.getName(c.src.feature.get), CommonUtil.getName(c.dst.feature.get))
      case (ir.ComponentCategory.Data, ir.ComponentCategory.Thread) =>
        (CommonUtil.getName(c.src.component), CommonUtil.getName(c.dst.feature.get))

      case _ => halt(s"Unexpected connection: ${c}")
    }
    return ret
  }
  
  def isHandledConnection(c: ir.ConnectionInstance): B = {

    def validFeature(f: ir.Feature): B = {
      var ret: B = f match {
        case fend: ir.FeatureEnd =>
          fend.category match {
            case ir.FeatureCategory.DataAccess => T
            case ir.FeatureCategory.DataPort => T
            case ir.FeatureCategory.EventPort => T
            case ir.FeatureCategory.EventDataPort => T
            case ir.FeatureCategory.SubprogramAccessGroup => T

            case ir.FeatureCategory.AbstractFeature => F
            case ir.FeatureCategory.BusAccess => F
            case ir.FeatureCategory.FeatureGroup => F
            case ir.FeatureCategory.Parameter => F
            case ir.FeatureCategory.SubprogramAccess => F
          }
        case faccess: ir.FeatureAccess =>
          faccess.accessCategory match {
            case ir.AccessCategory.Data => T
            case ir.AccessCategory.SubprogramGroup => T
            case _ => F
          }
        case _ => F
      }
      return ret
    }
    val src = symbolTable.airComponentMap.get(CommonUtil.getName(c.src.component)).get
    val dst = symbolTable.airComponentMap.get(CommonUtil.getName(c.dst.component)).get

    val ret : B = (src.category, dst.category) match {
      case (ir.ComponentCategory.Thread, ir.ComponentCategory.Thread) =>

        val srcFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(c.src.feature.get)).get
        val dstFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(c.dst.feature.get)).get

        validFeature(srcFeature) && validFeature(dstFeature)

      case (ir.ComponentCategory.Data, ir.ComponentCategory.Thread) =>
        val dstFeature = symbolTable.airFeatureMap.get(CommonUtil.getName(c.dst.feature.get)).get
        validFeature(dstFeature)

      case _ =>
        F
    }

    return ret
  }
}

