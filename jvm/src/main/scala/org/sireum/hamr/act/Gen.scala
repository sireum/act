// #Sireum

package org.sireum.hamr.act

import org.sireum._
import org.sireum.hamr.act.ast._
import org.sireum.hamr.act.util._
import org.sireum.hamr.act.cakeml.CakeML
import org.sireum.hamr.act.connections._
import org.sireum.hamr.act.periodic.{Dispatcher, PacerTemplate, PeriodicUtil}
import org.sireum.hamr.act.proof.ProofContainer.{CAmkESComponentCategory, CAmkESConnectionType}
import org.sireum.hamr.act.proof.{ProofUtil, SMT2ProofGen}
import org.sireum.hamr.act.templates._
import org.sireum.hamr.act.util.PathUtil
import org.sireum.hamr.act.util.Util.reporter
import org.sireum.hamr.act.vm._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.properties.{OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.templates.StackFrameTemplate
import org.sireum.hamr.codegen.common.types.{AadlTypes, TypeUtil}
import org.sireum.hamr.codegen.common.util.{ExperimentalOptions, ResourceUtil, PathUtil => CommonPathUtil}
import org.sireum.hamr.codegen.common.{CommonUtil, Names, StringUtil}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Aadl, FeatureEnd}
import org.sireum.ops.ISZOps

@record class Gen(model: Aadl,
                  symbolTable: SymbolTable,
                  aadlTypes: AadlTypes,
                  actOptions: ActOptions) {

  val platform: ActPlatform.Type = actOptions.platform
  val hamrBasePackageName: Option[String] = actOptions.hamrBasePackageName

  var typeMap: HashSMap[String, ir.Component] = HashSMap.empty

  var sharedData: HashMap[String, SharedData] = HashMap.empty
  var samplingPorts: HashMap[String, SamplingPortInterface] = HashMap.empty
  var srcQueues: Map[String, Map[String, QueueObject]] = Map.empty // {dstFeatureId -> {srcFeatureId -> queueobj}}

  var astObjects: ISZ[ASTObject] = ISZ()
  var monitors: HashSMap[String, Monitor] = HashSMap.empty // conn instname -> monitor
  var containers: ISZ[C_Container] = ISZ()
  var auxResourceFiles: ISZ[Resource] = ISZ()

  var camkesComponents: ISZ[ast.Component] = ISZ()
  var camkesConnections: ISZ[ast.Connection] = ISZ()
  var camkesConnectorContainers: ISZ[ConnectorContainer] = ISZ()

  var camkesConfiguration: ISZ[ast.Configuration] = ISZ()

  var auxCImplIncludes: ISZ[ST] = ISZ()

  var globalPreprocessorIncludes: Set[String] = Set.empty[String]
  var globalImports: Set[String] = Set.empty[String] + Util.camkesStdConnectors
  var settingsCmakeEntries: ISZ[ST] = ISZ()

  val preventBadging: B = T
  val performHamrIntegration: B = Util.hamrIntegration(platform)
  val hookupPeriodicComponentsToTimeServer: B = F

  val connectionCounter: Counter = Counter()
  val timerAttributeCounter: Counter = Counter()

  val useCaseConnectors: B = Connections.useCaseEventDataPortConnector(actOptions.experimentalOptions)
  val useDomainScheduling: B = PeriodicUtil.useDomainScheduling(symbolTable, actOptions.platform)

  var sbConnectionContainer: Map[String, SBConnectionContainer] = Map.empty // connection_instance_path ~> refinements

  var vmArtifactsIntroduced: B = F

  def hasErrors: B = {
    return reporter.hasError
  }

  def process(cSources: ISZ[String]): Option[ActContainer] = {

    // Resolver ensures that the wire protocol is in place if there are cakeml components,
    // regardless of what the target platform is (e.g. could reject for sel4_tb even
    // though we don't do cakeml integration at that level)

    if (!hasErrors) {
      auxCImplIncludes = cSources.map(c => st"""#include "../../../${c}"""")

      for (d <- model.dataComponents) {
        typeMap = typeMap + (Util.getClassifierFullyQualified(d.classifier.get) ~> d)
      }
      val sortedData: ISZ[ir.Component] =
        if (performHamrIntegration) typeMap.values // will only be art.DataContent so no need to sort
        else sortData(typeMap.values)

      if (platform != ActPlatform.SeL4_TB) {
        sbConnectionContainer = SBConnections.preprocessConnectionInstances(symbolTable)
      }

      buildTransportMechanisms()

      if (!hasErrors) {
        auxResourceFiles = auxResourceFiles :+
          ResourceUtil.createResource(
            path = s"${Util.getTypeIncludesPath()}/${Util.getSbTypeHeaderFilenameWithExtension()}",
            content = processDataTypes(sortedData),
            overwrite = T)
      }
    }

    if (!hasErrors) {
      // build CAmkES components
      gen(symbolTable.rootSystem)
    }

    if (!hasErrors && platform != ActPlatform.SeL4_TB) {
      // build CAmkES connections
      val connContainer: ConnectionContainer = processSBConnections()
      camkesConnections = camkesConnections ++ connContainer.connections
      camkesConfiguration = camkesConfiguration ++ connContainer.configurationEntries
      camkesConnectorContainers = camkesConnectorContainers ++ connContainer.optConnectorHolder
    }

    if (!hasErrors) {
      // filter??
      camkesConnectorContainers = (Set.empty[ConnectorContainer] ++ camkesConnectorContainers).elements

      // merge assemblies
      val assemblies: ISZ[Assembly] = astObjects.filter(f => f.isInstanceOf[Assembly]).map(m => m.asInstanceOf[Assembly])
      val otherAstObjects: ISZ[ASTObject] = astObjects.filter(f => !f.isInstanceOf[Assembly])

      val processesToVMs = symbolTable.getProcesses().filter((p: AadlProcess) => p.toVirtualMachine(symbolTable))
      if (processesToVMs.nonEmpty) {
        auxResourceFiles = auxResourceFiles ++ VMGen.getAuxResources(processesToVMs, platform, symbolTable)
      }

      val mergedAssemblies: ISZ[Assembly] = VMGen.mergeVMs(assemblies)

      var instances: Set[Instance] = Set.empty
      var connections: Set[Connection] = Set.empty
      var compositionMacros: Set[String] = Set.empty

      for (assembly <- mergedAssemblies) {
        instances = instances ++ assembly.composition.instances
        connections = connections ++ assembly.composition.connections
        compositionMacros = compositionMacros ++ assembly.composition.externalEntities
      }

      connections = connections ++ camkesConnections

      {
        val periodicAssemblyContributions: CamkesAssemblyContribution =
          Dispatcher.handlePeriodicComponents(
            useDomainScheduling, symbolTable, actOptions, connectionCounter, timerAttributeCounter,
            Util.getSbTypeHeaderFilenameForIncludes())

        globalImports = globalImports ++ periodicAssemblyContributions.imports
        instances = instances ++ periodicAssemblyContributions.instances
        connections = connections ++ periodicAssemblyContributions.connections
        camkesConfiguration = camkesConfiguration ++ periodicAssemblyContributions.configurations
        containers = containers ++ periodicAssemblyContributions.cContainers
        settingsCmakeEntries = settingsCmakeEntries ++ periodicAssemblyContributions.settingCmakeEntries
        auxResourceFiles = auxResourceFiles ++ periodicAssemblyContributions.auxResourceFiles
      }

      auxResourceFiles = auxResourceFiles :+
        ResourceUtil.createResource("settings.cmake", CMakeTemplate.genSettingsCmake(settingsCmakeEntries), F)

      val composition = Composition(
        groups = ISZ(),
        exports = ISZ(),
        instances = instances.elements,
        connections = connections.elements,
        externalEntities = compositionMacros.elements,
        comments = ISZ()
      )

      astObjects = ISZ(Assembly(
        configuration = (Set.empty[ast.Configuration] ++ camkesConfiguration).elements,
        configurationMacros = ISZ(),
        composition = composition,
        comments = ISZ()))

      astObjects = astObjects ++ otherAstObjects

      { // generate sb type library cmake
        val prefix: String = s"${Util.getTypeRootPath()}/"

        val typeSrcs: ISZ[Resource] = auxResourceFiles.filter(p => ops.StringOps(p.dstPath).startsWith(Util.getTypeSrcPath()))
        var filenames: ISZ[String] = typeSrcs.map(m => {
          assert(ops.StringOps(m.dstPath).startsWith(prefix), s"Unexpected type path ${m.dstPath}")
          ops.StringOps(m.dstPath).substring(prefix.size, m.dstPath.size)
        })

        val spis: ISZ[String] = samplingPorts.values.map((m: SamplingPortInterface) => {
          assert(ops.StringOps(m.implPath).startsWith(prefix), s"Unexpected type path ${m.implPath}")
          ops.StringOps(m.implPath).substring(prefix.size, m.implPath.size)
        })

        filenames = filenames ++ spis

        val contents: ST = CMakeTemplate.cmake_generateTypeCmakeLists(filenames)

        auxResourceFiles = auxResourceFiles :+
          ResourceUtil.createResource(s"${Util.getTypeRootPath()}/CMakeLists.txt", contents, T)

      } // end sb type library


      if (ExperimentalOptions.generateRefinementProof(actOptions.experimentalOptions) && actOptions.platform != ActPlatform.SeL4_TB) {
        ProofUtil.proofContainer.modelSchedulingType = PeriodicUtil.getSchedulingType(symbolTable, actOptions.platform)
        auxResourceFiles = auxResourceFiles ++ SMT2ProofGen.genSmt2Proof(ProofUtil.proofContainer, astObjects, sbConnectionContainer, symbolTable, actOptions.outputDir, actOptions.platform)
      }

      return Some(ActContainer(
        rootServer = CommonUtil.getLastName(symbolTable.rootSystem.component.identifier),
        connectors = camkesConnectorContainers,
        models = astObjects,
        monitors = monitors.values,
        samplingPorts = samplingPorts.values,
        cContainers = containers,
        auxFiles = auxResourceFiles,
        globalImports = globalImports.elements,
        globalPreprocessorIncludes = globalPreprocessorIncludes.elements,
        requiresTimeServer = PeriodicUtil.requiresTimeServer(symbolTable, useDomainScheduling)))

    } else {
      return None[ActContainer]()
    }
  }

  def gen(c: AadlComponent): Unit = {
    c match {
      case aadlSystem: AadlSystem =>

        for (process <- aadlSystem.subComponents()) {
          gen(process)
        }

        if (platform == ActPlatform.SeL4_TB) {
          val (connections, configs) = processTBConnections(c.component)
          camkesConnections = camkesConnections ++ connections
          camkesConfiguration = camkesConfiguration ++ configs
        }
      case aadlProcess: AadlProcess =>

        val g: Composition = genProcess(aadlProcess)

        astObjects = astObjects :+ Assembly(
          configuration = ISZ(),
          configurationMacros = ISZ(),
          composition = g,
          comments = ISZ())

      case _ =>
    }
  }

  def genProcess(aadlProcess: AadlProcess): Composition = {

    val compositionMacros: ISZ[String] = ISZ()
    var connections: ISZ[Connection] = ISZ()
    var instances: ISZ[Instance] = ISZ()

    if (aadlProcess.toVirtualMachine(symbolTable)) {
      val camkesComponentId = Util.getCamkesComponentIdentifier(aadlProcess, symbolTable)
      val processId = Util.getCamkesComponentIdentifier(aadlProcess, symbolTable)

      val vmGen = VMGen(useDomainScheduling, typeMap, samplingPorts, srcQueues, actOptions)
      val (component, auxResources): (Component, ISZ[Resource])  = vmGen.genProcess(aadlProcess, symbolTable, sbConnectionContainer.values)

      instances = instances :+ Util.createCAmkESInstance(
        originAadl = Some(aadlProcess),
        address_space = "",
        name = processId,
        component = component
      )

      auxResourceFiles = auxResourceFiles ++ auxResources

      connections = connections ++ VM_COMPONENT_CONNECTIONS_DEF.connections(processId, connectionCounter)

      // add passthrough
      connections = connections :+ Util.createConnectionC(
        connectionCategory = CAmkESConnectionType.VM,
        connectionCounter = connectionCounter,
        connectionType = Sel4ConnectorTypes.seL4VMDTBPassthrough,
        srcComponent = processId, srcFeature = "dtb_self",
        dstComponent = processId, dstFeature ="dtb"
      )

      if(!vmArtifactsIntroduced) {
        vmArtifactsIntroduced = T

        instances = instances ++ VM_GENERAL_COMPOSITION_DEF.instances()

        // introduce time and serial servers
        instances = instances ++ VM_VIRTUAL_SERIAL_COMPONENTS_DEF.instances()

        // add connection from serial server to time server
        connections = connections ++ VM_VIRTUAL_SERIAL_COMPONENTS_DEF.connections(connectionCounter)

        camkesConfiguration = camkesConfiguration ++ VM_VIRTUAL_SERIAL_GENERAL_CONFIGURATION_DEF.configurations()
      }

      connections = connections ++ PER_VM_VIRTUAL_SERIAL_CONNECTIONS_DEF.connections(processId, connectionCounter)

      globalImports = globalImports ++ VM_Template.vm_assembly_imports()

      camkesConfiguration = camkesConfiguration ++
        VM_Template.vm_assembly_configuration_entries(processId) ++
        VM_GENERAL_CONFIGURATION_DEF.configurations() ++
        VM_CONFIGURATION_DEF.configurations(processId) ++
        PER_VM_VIRTUAL_SERIAL_CONFIGURATION_DEF.configurations(processId)

      settingsCmakeEntries = settingsCmakeEntries ++ VM_Template.settings_cmake_entries()

      auxResourceFiles = auxResourceFiles :+ Util.sbCounterTypeDeclResource()

      val virtProcessor = aadlProcess.getBoundProcessor(symbolTable).get.asInstanceOf[AadlVirtualProcessor]
      PropertyUtil.getStackSizeInBytes(virtProcessor.component) match {
        case Some(bytes) =>
          camkesConfiguration = camkesConfiguration :+ StringTemplate.configurationStackSize(camkesComponentId, bytes)
        case _ =>
      }

      if (useDomainScheduling && (platform == ActPlatform.SeL4_Only || platform == ActPlatform.SeL4)) {
        aadlProcess.getDomain() match {
          case Some(domain) =>
            camkesConfiguration = camkesConfiguration :+ PacerTemplate.domainConfiguration(camkesComponentId, domain)
          case _ =>
        }
      }
    } else {
      for (sc <- aadlProcess.subComponents) {
        sc match {
          case aadlThread: AadlThread => {

            val camkesComponentId = Util.getCamkesComponentIdentifier(aadlThread, symbolTable)
            instances = instances :+ Util.createCAmkESInstance(
              originAadl = Some(aadlThread),

              address_space = "",
              name = camkesComponentId,
              component = genThread(aadlThread)
            )

            PropertyUtil.getStackSizeInBytes(sc.component) match {
              case Some(bytes) =>
                camkesConfiguration = camkesConfiguration :+ StringTemplate.configurationStackSize(camkesComponentId, bytes)
              case _ =>
            }

            if (useDomainScheduling && (platform == ActPlatform.SeL4_Only || platform == ActPlatform.SeL4)) {
              aadlThread.getDomain(symbolTable) match {
                case Some(domain) =>
                  camkesConfiguration = camkesConfiguration :+ PacerTemplate.domainConfiguration(camkesComponentId, domain)
                case _ =>
              }
            }
          }
          case aadlSubprogram: AadlSubprogram => {
            var params: ISZ[Parameter] = ISZ()
            for (f <- aadlSubprogram.parameters) {
              params = params :+ Parameter(
                array = F,
                direction = if (f.direction == ir.Direction.In) Direction.In else Direction.Out,
                name = f.getName(),
                typ = Util.getClassifier(f.feature.classifier.get),
                comments = ISZ())
            }

            val method = Method(
              name = aadlSubprogram.identifier,
              parameters = params,
              returnType = None[String](),
              comments = ISZ()
            )

            val procName = aadlSubprogram.getClassifier()
            astObjects = astObjects :+ Procedure(name = procName, methods = ISZ(method), includes = ISZ(), comments = ISZ())
          }
          case spg: AadlSubprogramGroup => {
            var methods: ISZ[Method] = ISZ()

            for (m <- spg.component.features) {
              m match {
                case spa: ir.FeatureAccess =>
                  if (spa.classifier.nonEmpty) {
                    val spComp = symbolTable.airClassifierMap.get(spa.classifier.get.name).get
                    assert(spComp.category == ir.ComponentCategory.Subprogram)

                    val methodName = CommonUtil.getLastName(spa.identifier)
                    var params: ISZ[Parameter] = ISZ()
                    for (param <- spComp.features) {
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

                          params = params :+ Parameter(
                            array = F,
                            direction = dir,
                            name = paramName,
                            typ = typName,
                            comments = ISZ())
                      }
                    }

                    methods = methods :+ Method(name = methodName, parameters = params, returnType = None[String](), comments = ISZ())
                  } else {
                    reporter.error(None(), Util.toolName, s"Could not resolve feature ${CommonUtil.getName(spa.identifier)} from ${sc.path}")
                  }
                case _ =>
              }
            }

            if (sc.subComponents.nonEmpty) {
              halt(s"Subprogram group subcomponents not currently handled: ${sc}")
            }

            val procName = Util.getClassifier(spg.component.classifier.get)
            astObjects = astObjects :+ Procedure(name = procName, methods = methods, includes = ISZ(), comments = ISZ())
          }
          case aadlData: AadlData => {
            val classifier = aadlData.component.classifier.get
            val typeName = Util.getClassifierFullyQualified(classifier)

            val readMethod = Method(
              name = s"read_${typeName}",
              parameters = ISZ(Parameter(array = F, direction = Direction.Out, name = "arg", typ = typeName, comments = ISZ())),
              returnType = None[String](),
              comments = ISZ()
            )

            val writeMethod = Method(
              name = s"write_${typeName}",
              parameters = ISZ(Parameter(array = F, direction = Direction.Refin, name = "arg", typ = typeName, comments = ISZ())),
              returnType = None[String](),
              comments = ISZ()
            )

            val procName = Util.getSharedDataInterfaceName(classifier)
            astObjects = astObjects :+ Procedure(
              name = procName,
              methods = ISZ(readMethod, writeMethod),
              includes = ISZ(Util.getSbTypeHeaderFilenameForIncludes()),
              comments = ISZ()
            )
          }
          case _ =>
            halt(s"Not handling: subcomponent of type '${sc.component.category}'.  ${sc.path}")
        }
      }
    }
    val monInstances = monitors.values.map((m: Monitor) => m.i)

    if (platform == ActPlatform.SeL4_TB) {
      val (_connections, configs) = processTBConnections(aadlProcess.component)
      connections = connections ++ _connections
      camkesConfiguration = camkesConfiguration ++ configs
    }

    return Composition(
      groups = ISZ(),
      exports = ISZ(),
      instances = instances ++ monInstances,
      connections = connections,
      externalEntities = compositionMacros,
      comments = ISZ()
    )
  }

  def getSamplingPort(fend: FeatureEnd): SamplingPortInterface = {
    val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(fend.classifier.get)).get
    val sel4PortType: String = Util.getSel4TypeName(aadlPortType, performHamrIntegration)
    return samplingPorts.get(sel4PortType).get
  }

  def genThread(aadlThread: AadlThread): Component = {

    val c = aadlThread.component

    assert(c.subComponents.isEmpty)
    assert(c.connectionInstances.isEmpty)

    val camkesComponentName = Util.getCamkesComponentName(aadlThread, symbolTable)

    var provides: ISZ[Provides] = ISZ()
    var uses: ISZ[Uses] = ISZ()
    var emits: ISZ[Emits] = ISZ()
    var consumes: ISZ[Consumes] = ISZ()
    var dataports: ISZ[Dataport] = ISZ()

    var camkesIncludes: Set[String] = Set.empty
    camkesIncludes = camkesIncludes + Util.getSbTypeHeaderFilenameForIncludes()

    var cmakeSOURCES: ISZ[String] = ISZ()
    var cmakeINCLUDES: ISZ[String] = ISZ()

    var imports: Set[String] = Set.empty

    var gcInterfaceStatements: ISZ[ST] = ISZ()
    var gcImplIncludes: ISZ[ST] = ISZ()
    var gcImplMethods: ISZ[ST] = ISZ()
    var gcImplPreInits: ISZ[ST] = ISZ()
    var gcImplPostInits: ISZ[ST] = ISZ()
    var gcImplDrainQueues: ISZ[(ST, ST)] = ISZ()

    for (f <- aadlThread.getFeatureAccesses()) {

      def handleSubprogramAccess(): Unit = {
        val proc = Util.getClassifier(f.feature.classifier.get)

        f.kind match {
          case ir.AccessType.Requires =>
            imports = imports + Util.getInterfaceFilename(proc)
            uses = uses :+ Uses(
              name = f.identifier,
              optional = F,
              typ = proc,
              comments = ISZ())
          case ir.AccessType.Provides =>
            imports = imports + Util.getInterfaceFilename(proc)
            provides = provides :+ Provides(
              name = f.identifier,
              typ = proc,
              comments = ISZ())
        }
      }

      def handleDataAccess(): Unit = {
        val typeName = Util.getClassifierFullyQualified(f.feature.classifier.get)
        val interfaceName = Util.getSharedDataInterfaceName(f.feature.classifier.get)

        f.kind match {
          case ir.AccessType.Requires =>
            imports = imports + Util.getInterfaceFilename(interfaceName)
            dataports = dataports :+ Dataport(
              name = f.identifier,
              optional = F,
              typ = typeName,
              comments = ISZ())
          case ir.AccessType.Provides =>
            imports = imports + Util.getInterfaceFilename(interfaceName)
            dataports = dataports :+ Dataport(
              name = f.identifier,
              optional = F,
              typ = typeName,
              comments = ISZ())
        }
      }

      f match {
        case a: AadlSubprogramGroupAccess =>
          handleSubprogramAccess()
        case a: AadlSubprogramAccess =>
          handleSubprogramAccess()

        case a: AadlDataAccess =>
          handleDataAccess()

        case _ =>
          reporter.error(None(), Util.toolName, s"Not expecting AccessType: ${f.path}")
      }
    }

    val connectedPorts: ISZ[AadlPort] = aadlThread.getPorts().filter(p => symbolTable.isConnected(p.feature))
    for (f <- connectedPorts) {

      f match {
        case a: AadlDataPort =>
          def dataPort_SB_Profile(): Unit = {
            assert(f.direction == ir.Direction.In || f.direction == ir.Direction.Out)

            val samplingPort: SamplingPortInterface = getSamplingPort(f.feature)

            camkesIncludes = camkesIncludes + s"<${Os.path(samplingPort.headerPath).name}>"

            cmakeSOURCES = cmakeSOURCES :+ samplingPort.implPath
            cmakeINCLUDES = cmakeINCLUDES :+ CommonPathUtil.value(Os.path(samplingPort.headerPath).up.value)

            gcImplMethods = StringTemplate.sbSamplingPortGlobalVarDecl(samplingPort, a) +: gcImplMethods

            val camkesDataPortId = Util.brand(f.identifier)

            dataports = dataports :+ Util.createDataport_Refinement(
              aadlComponent = aadlThread,
              aadlPort = a,
              symbolTable = symbolTable,

              name = camkesDataPortId,
              optional = F,
              typ = samplingPort.structName
            )
          }

          def dataPort_TB_Profile(): Unit = {
            f.direction match {
              case ir.Direction.In =>
                // uses monitor
                // consumes notification
                if (symbolTable.inConnections.get(f.path).nonEmpty) {
                  val monitor = Monitors.getMonitorForInPort(f.path, monitors).get.asInstanceOf[TB_Monitor]
                  imports = imports + Util.getInterfaceFilename(monitor.interface.name)

                  uses = uses :+ Util.createUses_Refinement(
                    aadlComponent = aadlThread,
                    aadlPort = a,
                    symbolTable = symbolTable,

                    name = Util.genMonitorFeatureName(f.identifier, None[Z]()),
                    typ = monitor.interface.name,
                    optional = F)

                  consumes = consumes :+ Util.createConsumes_Refinement(
                    aadlComponent = aadlThread,
                    aadlPort = a,
                    symbolTable = symbolTable,

                    name = Util.genSeL4NotificationName(a.identifier, T),
                    typ = Util.getMonitorNotificationType(f.feature.category),
                    optional = F)
                }
              case ir.Direction.Out =>
                // uses monitor
                symbolTable.outConnections.get(f.path) match {
                  case Some(outs) =>
                    var i: Z = 0
                    for (o <- outs) {
                      val monitor = monitors.get(CommonUtil.getName(o.name)).get.asInstanceOf[TB_Monitor]
                      imports = imports + Util.getInterfaceFilename(monitor.interface.name)

                      uses = uses :+ Util.createUses_Refinement(
                        aadlComponent = aadlThread,
                        aadlPort = a,
                        symbolTable = symbolTable,

                        name = Util.genMonitorFeatureName(f.identifier, Some(i)),
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

          platform match {
            case ActPlatform.SeL4_TB => dataPort_TB_Profile()
            case ActPlatform.SeL4 => dataPort_SB_Profile()
            case ActPlatform.SeL4_Only => dataPort_SB_Profile()
          }

        case a: AadlEventDataPort =>

          def eventDataPort_TB_Profile(): Unit = {
            // handle data part
            f.direction match {
              case ir.Direction.In =>
                // uses monitor
                // consumes notification
                if (symbolTable.inConnections.get(f.path).nonEmpty) {
                  val monitor = Monitors.getMonitorForInPort(f.path, monitors).get.asInstanceOf[TB_Monitor]
                  imports = imports + Util.getInterfaceFilename(monitor.interface.name)

                  uses = uses :+ Util.createUses_Refinement(
                    aadlComponent = aadlThread,
                    aadlPort = a,
                    symbolTable = symbolTable,

                    name = Util.genMonitorFeatureName(f.identifier, None[Z]()),
                    typ = monitor.interface.name,
                    optional = F)

                  consumes = consumes :+ Util.createConsumes_Refinement(
                    aadlComponent = aadlThread,
                    aadlPort = a,
                    symbolTable = symbolTable,

                    name = Util.genSeL4NotificationName(a.identifier, T),
                    typ = Util.getMonitorNotificationType(f.feature.category),
                    optional = F)
                }
              case ir.Direction.Out =>
                // uses monitor
                symbolTable.outConnections.get(f.path) match {
                  case Some(outs) =>
                    var i: Z = 0
                    for (o <- outs) {
                      val monitor = monitors.get(CommonUtil.getName(o.name)).get.asInstanceOf[TB_Monitor]
                      imports = imports + Util.getInterfaceFilename(monitor.interface.name)

                      uses = uses :+ Util.createUses_Refinement(
                        aadlComponent = aadlThread,
                        aadlPort = a,
                        symbolTable = symbolTable,

                        name = Util.genMonitorFeatureName(f.identifier, Some(i)),
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

            // handle the event part
            f.direction match {
              case ir.Direction.In =>

                val incorporateEntrypointSource = Util.getComputeEntrypointSourceText(f.feature.properties).nonEmpty && aadlThread.isSporadic()

                if (incorporateEntrypointSource || performHamrIntegration) {
                  val name = Util.genSeL4NotificationName(a.identifier, T)
                  val handlerName = s"${name}_handler"
                  val regCallback = s"${name}_reg_callback"

                  val featureName = f.identifier
                  gcImplMethods = gcImplMethods :+
                    StringTemplate.cEventNotificationHandler(handlerName, regCallback, featureName)

                  gcImplPostInits = gcImplPostInits :+ StringTemplate.cRegCallback(handlerName, regCallback, f)
                } else {
                  reporter.warn(None(), Util.toolName, s"port: ${f.identifier} in thread: ${camkesComponentName} does not have a compute entrypoint and will not be dispatched.")
                }

              case _ =>
            }
          }

          def eventDataPort_SB_Profile(): Unit = {
            // notification plus a shared counter

            // correct for fan out connections?

            val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(f.feature.classifier.get)).get
            val sel4TypeName = Util.getSel4TypeName(aadlPortType, performHamrIntegration)

            auxResourceFiles = auxResourceFiles :+ Util.sbCounterTypeDeclResource()

            f.direction match {
              case ir.Direction.In =>
                val queueSize = PropertyUtil.getQueueSize(f.feature, Util.DEFAULT_QUEUE_SIZE)
                val queueType = Util.getEventDataSBQueueTypeName(sel4TypeName, queueSize)

                val queueImplFilename = Util.getEventData_SB_QueueImplFileName(sel4TypeName, queueSize)
                val queueHeaderFilename = Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, queueSize)

                cmakeSOURCES = cmakeSOURCES :+ s"${Util.getTypeSrcPath()}/${queueImplFilename}"
                camkesIncludes = camkesIncludes + s"<${queueHeaderFilename}>"

                val isOptional = symbolTable.getInConnections(f.path).isEmpty

                if (!useCaseConnectors) {
                  val camkesEventPortId = Util.genSeL4NotificationName(f.identifier, T)
                  consumes = consumes :+ Util.createConsumes_Refinement(
                    aadlComponent = aadlThread,
                    aadlPort = a,
                    symbolTable = symbolTable,

                    name = camkesEventPortId,
                    typ = Util.EVENT_NOTIFICATION_TYPE,
                    optional = isOptional)
                }

                val camkesDataPortId = Util.getEventDataSBQueueDestFeatureName(f.identifier)
                dataports = dataports :+ Util.createDataport_Refinement(
                  aadlComponent = aadlThread,
                  aadlPort = a,
                  symbolTable = symbolTable,

                  name = camkesDataPortId,
                  typ = queueType,
                  optional = isOptional)

              case ir.Direction.Out =>

                val dsts: Map[String, QueueObject] = srcQueues.get(f.path).get // dstId -> queueObject

                for (qo <- dsts.valueSet.elements) {
                  val queueSize = qo.queueSize
                  val queueType = Util.getEventDataSBQueueTypeName(sel4TypeName, queueSize)
                  val queueImplName = Util.getEventData_SB_QueueImplFileName(sel4TypeName, queueSize)
                  val queueHeaderFilename = Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, queueSize)

                  cmakeSOURCES = cmakeSOURCES :+ s"${Util.getTypeSrcPath()}/${queueImplName}"
                  camkesIncludes = camkesIncludes + s"<${queueHeaderFilename}>"

                  if (!useCaseConnectors) {
                    val camkesEventPortId = Util.genSeL4NotificationQueueName(f.identifier, queueSize)
                    emits = emits :+ Util.createEmits_Refinement(
                      aadlComponent = aadlThread,
                      aadlPort = a,
                      symbolTable = symbolTable,

                      name = camkesEventPortId,
                      typ = Util.EVENT_NOTIFICATION_TYPE)
                  }

                  val camkesDataPortId = Util.getEventDataSBQueueSrcFeatureName(f.identifier, queueSize)
                  dataports = dataports :+ Util.createDataport_Refinement(
                    aadlComponent = aadlThread,
                    aadlPort = a,
                    symbolTable = symbolTable,

                    name = camkesDataPortId,
                    typ = queueType,
                    optional = F)
                }
              case _ => halt(s"Unexpected port direction: ${f.direction}")
            }
          }

          platform match {
            case ActPlatform.SeL4_TB => eventDataPort_TB_Profile()
            case ActPlatform.SeL4 => eventDataPort_SB_Profile()
            case ActPlatform.SeL4_Only => eventDataPort_SB_Profile()
          }

        case a: AadlEventPort =>

          def eventPort_TB_Profile(): Unit = {
            f.direction match {
              case ir.Direction.In =>
                // import receiver interface
                imports = imports + s""""../../interfaces/${Util.MONITOR_INTERFACE_NAME_RECEIVER}.idl4""""

                // uses
                uses = uses :+ Util.createUses_Refinement(
                  aadlComponent = aadlThread,
                  aadlPort = a,
                  symbolTable = symbolTable,

                  name = Util.genMonitorFeatureName(f.identifier, None[Z]()),
                  typ = Util.MONITOR_INTERFACE_NAME_RECEIVER,
                  optional = F,
                )

                // consumes notification
                consumes = consumes :+ Util.createConsumes_Refinement(
                  aadlComponent = aadlThread,
                  aadlPort = a,
                  symbolTable = symbolTable,

                  name = Util.genSeL4NotificationName(f.identifier, T),
                  typ = Util.MONITOR_EVENT_DATA_NOTIFICATION_TYPE,
                  optional = F)

              case ir.Direction.Out =>
                // import sender interface
                imports = imports + s""""../../interfaces/${Util.MONITOR_INTERFACE_NAME_SENDER}.idl4""""

                symbolTable.outConnections.get(f.path) match {
                  case Some(outs) =>
                    var i: Z = 0
                    for (o <- outs) {

                      uses = uses :+ Util.createUses_Refinement(
                        aadlComponent = aadlThread,
                        aadlPort = a,
                        symbolTable = symbolTable,

                        name = Util.genMonitorFeatureName(f.identifier, Some(i)),
                        typ = Util.MONITOR_INTERFACE_NAME_SENDER,
                        optional = F
                      )

                      i = i + 1
                    }
                  case _ => halt(s"${f.path}: expecting out connection")
                }

              case _ => halt(s"${f.path}: not expecting direction ${f.direction}")
            }
          }

          def eventPort_SB_Profile(): Unit = {
            // notification plus a shared counter

            // correct for fan out connections?

            val counterType = Util.SB_EVENT_COUNTER_TYPE

            // add sb counter type decl resource
            auxResourceFiles = auxResourceFiles :+ Util.sbCounterTypeDeclResource()
            camkesIncludes = camkesIncludes + s"<${Util.SB_COUNTER_HEADER_FILENAME}>"

            f.direction match {
              case ir.Direction.In =>
                val camkesEventPortId = Util.getEventSBNotificationName(f.identifier)
                consumes = consumes :+ Util.createConsumes_Refinement(
                  aadlComponent = aadlThread,
                  aadlPort = a,
                  symbolTable = symbolTable,

                  name = camkesEventPortId,
                  typ = Util.EVENT_NOTIFICATION_TYPE,
                  optional = F)

                val camkesDataPortId = Util.getEventSBCounterName(f.identifier)
                dataports = dataports :+ Util.createDataport_Refinement(
                  aadlComponent = aadlThread,
                  aadlPort = a,
                  symbolTable = symbolTable,

                  name = camkesDataPortId,
                  typ = counterType,
                  optional = F)

              case ir.Direction.Out =>
                val camkesEventPortId = Util.getEventSBNotificationName(f.identifier)
                emits = emits :+ Util.createEmits_Refinement(
                  aadlComponent = aadlThread,
                  aadlPort = a,
                  symbolTable = symbolTable,

                  name = camkesEventPortId,
                  typ = Util.EVENT_NOTIFICATION_TYPE)

                val camkesDataPortId = Util.getEventSBCounterName(f.identifier)
                dataports = dataports :+ Util.createDataport_Refinement(
                  aadlComponent = aadlThread,
                  aadlPort = a,
                  symbolTable = symbolTable,

                  name = camkesDataPortId,
                  typ = counterType,
                  optional = F)

              case _ => halt(s"Unexpected port direction: ${f.direction}")
            }
          }

          platform match {
            case ActPlatform.SeL4_TB => eventPort_TB_Profile()
            case ActPlatform.SeL4 => eventPort_SB_Profile()
            case ActPlatform.SeL4_Only => eventPort_SB_Profile()
          }

        case _ =>
          reporter.warn(None(), Util.toolName, s"Skipping ${f.feature.category} for ${f.identifier}")
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

    } // end processing thread's features

    // Build camkes component and glue code files

    var binarySemaphores: ISZ[BinarySemaphore] = ISZ()
    var semaphores: ISZ[Semaphore] = ISZ()

    PropertyUtil.getDiscreetPropertyValue(c.properties, "camkes::Binary_Semaphore") match {
      case Some(v: ir.ValueProp) =>
        binarySemaphores = binarySemaphores :+ BinarySemaphore(name = v.value, comments = ISZ())
      case _ =>
    }


    var gcRunInitStmts: ISZ[ST] = gcImplDrainQueues.map(x => x._1)
    var gcRunPreLoopStmts: ISZ[ST] = ISZ()
    var gcRunLoopStartStmts: ISZ[ST] = ISZ()
    var gcRunLoopMidStmts: ISZ[ST] = gcImplDrainQueues.map(x => x._2)
    var gcRunLoopEndStmts: ISZ[ST] = ISZ()

    if (!PeriodicUtil.requiresPacerArtifacts(aadlThread, symbolTable, useDomainScheduling)) {
      semaphores = semaphores :+ Semaphore(name = StringTemplate.SEM_DISPATCH, comments = ISZ())

      val semWait: ST = st"MUTEXOP(${StringTemplate.SEM_WAIT}())"

      gcRunPreLoopStmts = gcRunPreLoopStmts :+ semWait

      gcRunLoopStartStmts = gcRunLoopStartStmts :+ semWait
    }

    aadlThread.dispatchProtocol match {
      case Dispatch_Protocol.Periodic =>

        val (componentContributions, glueCodeContributions) =
          Dispatcher.handlePeriodicComponent(useDomainScheduling, symbolTable, actOptions, aadlThread)

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

      case Dispatch_Protocol.Sporadic =>

    }

    val names: Names = Names(c, hamrBasePackageName.get)

    if (!performHamrIntegration) {
      PropertyUtil.getInitializeEntryPoint(c.properties) match {
        case Some(methodName) =>
          gcInterfaceStatements = gcInterfaceStatements :+ st"""void ${methodName}(const int64_t *arg);"""
          val (cimpl, runEntry) = StringTemplate.componentInitializeEntryPoint(camkesComponentName, methodName)
          gcImplMethods = gcImplMethods :+ cimpl
          gcRunInitStmts = gcRunInitStmts :+ runEntry
        case _ =>
      }
    }

    val uri = PathUtil.getComponentGlueCodeImplementationFilename(aadlThread)

    if (performHamrIntegration) {

      gcImplIncludes = gcImplIncludes :+ st"#include <${Util.genCHeaderFilename(names.cEntryPointAdapterName)}>"

      gcImplPreInits = st"""printf("Entering pre-init of ${camkesComponentName}\n");""" +: gcImplPreInits

      gcImplPreInits = gcImplPreInits :+ SlangEmbeddedTemplate.hamrIntialiseArchitecture(names.cEntryPointAdapterQualifiedName)

      gcImplPreInits = gcImplPreInits :+ SlangEmbeddedTemplate.hamrInitialiseEntrypoint(names.cEntryPointAdapterQualifiedName)

      gcImplPreInits = gcImplPreInits :+ st"""printf("Leaving pre-init of ${camkesComponentName}\n");"""

      val outgoingPorts: ISZ[AadlPort] = aadlThread.getPorts().filter((f: AadlPort) =>
        symbolTable.outConnections.contains(f.path))

      gcImplMethods = gcImplMethods ++ outgoingPorts.map((f: AadlPort) => hamrSendOutgoingPort(names, f, typeMap, uri))

      val unconnectedOutgoingPorts: ISZ[ir.FeatureEnd] = CommonUtil.getOutPorts(c).filter(f =>
        !symbolTable.outConnections.contains(CommonUtil.getName(f.identifier)))

      gcImplMethods = gcImplMethods ++ unconnectedOutgoingPorts.map((f: ir.FeatureEnd) => hamrSendUnconnectedOutgoingPort(names, f, uri))

      val inPorts: ISZ[ir.FeatureEnd] = CommonUtil.getInPorts(c).filter(f =>
        symbolTable.inConnections.contains(CommonUtil.getName(f.identifier)))

      gcImplMethods = gcImplMethods ++ inPorts.map((f: ir.FeatureEnd) => hamrReceiveIncomingPort(names, f, typeMap, uri))

      val unconnectedInPorts: ISZ[ir.FeatureEnd] = CommonUtil.getInPorts(c).filter(f =>
        !symbolTable.inConnections.contains(CommonUtil.getName(f.identifier)))

      val freezeInEventPorts: ISZ[ST] = {
        aadlThread.getPorts().filter((p: AadlPort) => p.direction == ir.Direction.In && p.isInstanceOf[AadlEventPort])
          .map((eventPort: AadlPort) =>
            st"${StringTemplate.samplingPortFreezeMethodName(eventPort)}();")
      }

      // remove any mid loop statements and replace with sel4 specific ones
      gcRunLoopMidStmts = freezeInEventPorts ++ SlangEmbeddedTemplate.hamrRunLoopEntries(names.cEntryPointAdapterQualifiedName)

      gcImplMethods = gcImplMethods ++ unconnectedInPorts.map((f: ir.FeatureEnd) => hamrReceiveUnconnectedIncomingPort(names, f, uri))
    }

    var cSources: ISZ[Resource] = ISZ()

    if (aadlThread.isCakeMLComponent()) {
      val ffis: ISZ[Resource] = CakeML.processThread( //
        aadlThread, //
        hamrBasePackageName.get, //
        symbolTable, //
        actOptions.aadlRootDirectory, //
        reporter)
      cSources = cSources ++ ffis
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
      containsFFIs = aadlThread.isCakeMLComponent())

    cSources = cSources :+ gcImpl

    containers = containers :+ C_Container(
      instanceName = "UNNEEDED_VAR", //names.instanceName,
      componentId = camkesComponentName,

      cSources = cSources,
      cIncludes = ISZ(genComponentTypeInterfaceFile(aadlThread, gcInterfaceStatements)),

      sourceText = if (performHamrIntegration) ISZ() else PropertyUtil.getSourceText(c.properties),

      cmakeSOURCES = cmakeSOURCES,
      cmakeINCLUDES = cmakeINCLUDES :+ Util.getTypeIncludesPath(),
      cmakeLIBS = ISZ()
    )

    val comp = Util.createCAmkESComponent(
      aadlThread = Some(aadlThread),
      componentCategory = CAmkESComponentCategory.Refinement,

      control = T,
      hardware = F,
      name = camkesComponentName,

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

    return comp
  }

  def hamrSendUnconnectedOutgoingPort(names: Names,
                                      srcPort: ir.FeatureEnd,
                                      uri: String): ST = {
    val portName = CommonUtil.getLastName(srcPort.identifier)
    val sel4ExtName = names.sel4SlangExtensionQualifiedNameC

    val methodName = s"${sel4ExtName}_${portName}_Send"

    val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(T, uri, "", methodName, 0)

    return SlangEmbeddedTemplate.hamrSendUnconnectedOutgoingDataPort(methodName, declNewStackFrame)
  }

  def hamrSendOutgoingPort(names: Names,
                           srcPort: AadlPort,
                           typeMap: HashSMap[String, ir.Component],
                           uri: String): ST = {
    val sel4ExtName = names.sel4SlangExtensionQualifiedNameC

    val methodName = s"${sel4ExtName}_${srcPort.identifier}_Send"

    val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(T, uri, "", methodName, 0)

    def handleOutDataPort(): ST = {
      val suffix: String = if (srcPort.isInstanceOf[AadlDataPort]) "write" else "enqueue"
      val srcEnqueue = Util.brand(s"${srcPort.identifier}_${suffix}")
      val camkesType = Util.getClassifierFullyQualified(srcPort.feature.classifier.get)
      val sel4Type = Util.getSel4TypeName(typeMap.get(camkesType).get, performHamrIntegration)
      val slangPayloadType = SlangEmbeddedTemplate.hamrSlangPayloadType(srcPort.feature.classifier.get, hamrBasePackageName.get)

      val comment = st"// send ${srcPort.identifier}: ${srcPort.direction.name} ${srcPort.feature.category.name} ${camkesType}"
      return SlangEmbeddedTemplate.hamrSendOutgoingDataPort(comment, methodName, sel4Type, slangPayloadType, srcEnqueue, declNewStackFrame)
    }

    def handleOutEventPort(): ST = {
      val srcEnqueue = Util.getEventPortSendReceiveMethodName(srcPort)
      val comment = st"// send ${srcPort.identifier}: ${srcPort.direction.name} ${srcPort.feature.category.name}"
      return SlangEmbeddedTemplate.hamrSendOutgoingEventPort(comment, methodName, srcEnqueue, declNewStackFrame)
    }

    val ret: ST = srcPort match {
      case a: AadlDataPort => handleOutDataPort()
      case a: AadlEventDataPort => handleOutDataPort()
      case a: AadlEventPort => handleOutEventPort()
      case x => halt(s"Not handling ${x}")
    }

    return ret
  }

  def hamrReceiveUnconnectedIncomingPort(names: Names,
                                         srcPort: ir.FeatureEnd,
                                         uri: String): ST = {
    val portName = CommonUtil.getLastName(srcPort.identifier)
    val sel4ExtName = names.sel4SlangExtensionQualifiedNameC

    val methodName = s"${sel4ExtName}_${portName}_Receive"

    val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(T, uri, "", methodName, 0)

    val receiveMethod = SlangEmbeddedTemplate.hamrReceiveUnconnectedIncomingEventPort(methodName, declNewStackFrame)

    val isEmptyMethodName = s"${sel4ExtName}_${portName}_IsEmpty"
    val sel4IsEmptyMethodName = Util.brand(s"${portName}_is_empty")
    val isEmptyMethod = SlangEmbeddedTemplate.hamrIsEmptyUnconnected(isEmptyMethodName, sel4IsEmptyMethodName)

    val ret: ST =
      st"""${isEmptyMethod}
          |
          |${receiveMethod}"""
    return ret
  }

  def hamrReceiveIncomingPort(names: Names,
                              srcPort: ir.FeatureEnd,
                              typeMap: HashSMap[String, ir.Component],
                              uri: String): ST = {
    val portName = CommonUtil.getLastName(srcPort.identifier)
    val sel4ExtName = names.sel4SlangExtensionQualifiedNameC

    val methodName = s"${sel4ExtName}_${portName}_Receive"
    val suffix: String = if (srcPort.category == ir.FeatureCategory.DataPort) "read" else "dequeue"
    val sel4ReadMethod = Util.brand(s"${portName}_${suffix}")

    val isEmptyMethodName = s"${sel4ExtName}_${portName}_IsEmpty"
    val sel4IsEmptyMethodName = Util.brand(s"${portName}_is_empty")

    val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(T, uri, "", methodName, 0)

    def handleInDataPort(): ST = {
      val camkesType = Util.getClassifierFullyQualified(srcPort.classifier.get)
      val sel4Type = Util.getSel4TypeName(typeMap.get(camkesType).get, performHamrIntegration)
      val slangPayloadType = SlangEmbeddedTemplate.hamrSlangPayloadType(srcPort.classifier.get, hamrBasePackageName.get)
      val isEmptyMethod = SlangEmbeddedTemplate.hamrIsEmpty(isEmptyMethodName, sel4IsEmptyMethodName, srcPort)

      val comment = st"// receive ${portName}: ${srcPort.direction.name} ${srcPort.category.name} ${sel4Type}"
      val receiveMethod = SlangEmbeddedTemplate.hamrReceiveIncomingDataPort(
        comment, methodName, sel4Type, slangPayloadType, sel4ReadMethod, declNewStackFrame)

      val ret: ST =
        st"""${isEmptyMethod}
            |
            |${receiveMethod}"""
      return ret
    }

    def handleInEventPort(): ST = {
      val comment = st"// receive ${portName}: ${srcPort.direction.name} ${srcPort.category.name}"
      val receiveMethod = SlangEmbeddedTemplate.hamrReceiveIncomingEventPort(comment, methodName, sel4ReadMethod, declNewStackFrame)

      val isEmptyMethod = SlangEmbeddedTemplate.hamrIsEmpty(isEmptyMethodName, sel4IsEmptyMethodName, srcPort)

      val ret: ST =
        st"""${isEmptyMethod}
            |
            |${receiveMethod}"""
      return ret
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
      case ActPlatform.SeL4 => // use new mappings below
      case ActPlatform.SeL4_Only => // use new mappings below
    }

    var seen: Set[String] = Set.empty[String]

    val entries = symbolTable.inConnections.entries.filter(p => p._2.size > 0)

    for (in <- entries) {
      for (connInst <- in._2 if sbConnectionContainer.contains(CommonUtil.getName(connInst.name))) {

        val sbcc = sbConnectionContainer.get(CommonUtil.getName(connInst.name)).get

        if (sbcc.dstPort.isInstanceOf[AadlEventDataPort]) {
          val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(sbcc.dstPort.feature.classifier.get)).get
          val sel4PortType: String = Util.getSel4TypeName(aadlPortType, performHamrIntegration)

          val queueSize = PropertyUtil.getQueueSize(sbcc.dstPort.feature, Util.DEFAULT_QUEUE_SIZE)

          val queueName = Util.getEventDataSBQueueName(sel4PortType, queueSize)
          if (!seen.contains(queueName)) {
            auxResourceFiles = auxResourceFiles ++ EventDataQueueTemplate.genSbQueueTypeFiles(sel4PortType, queueSize)

            seen = seen + queueName
          }

          var dstMap = srcQueues.getOrElse(sbcc.srcPort.path, Map.empty)
          assert(!dstMap.contains(sbcc.dstPort.path))
          dstMap = dstMap + (sbcc.dstPort.path ~> QueueObject(queueName, queueSize))
          srcQueues = srcQueues + (sbcc.srcPort.path ~> dstMap)
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
          val instanceName = StringUtil.toLowerCase(monitorName)
          val interfaceName = Util.getInterfaceName(f)

          val typeName = Util.getClassifierFullyQualified(f.classifier.get)

          val providesVarName = "mon"

          val monitor = Util.createCAmkESComponent(
            aadlThread = None(),
            componentCategory = CAmkESComponentCategory.Monitor,

            control = F,
            hardware = F,
            name = monitorName,

            mutexes = ISZ(),
            binarySemaphores = ISZ(),
            semaphores = ISZ(),
            dataports = ISZ(),
            emits = ISZ(
              Util.createEmits_Monitor(
                monitorName = instanceName,
                name = "monsig",
                typ = Util.getMonitorNotificationType(f.category))),
            uses = ISZ(),
            consumes = ISZ(),
            provides = ISZ(Util.createProvides_Monitor(
              monitorName = instanceName,
              name = providesVarName,
              typ = interfaceName)),
            includes = ISZ(),
            attributes = ISZ(),
            preprocessorIncludes = ISZ(),
            imports = ISZ(st""""../../../${Util.DIR_INTERFACES}/${interfaceName}.idl4"""".render),

            externalEntities = ISZ()
          )

          val inst: Instance = Instance(
            address_space = "",
            name = instanceName,
            component = monitor, comments = ISZ())

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
            includes = ISZ(Util.getSbTypeHeaderFilenameForIncludes()),
            comments = ISZ())

          val connInstName = CommonUtil.getName(connInst.name)

          val implName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/src/${Util.genCImplFilename(monitorName)}"
          val cimplementation: Resource =
            if (f.category == ir.FeatureCategory.DataPort) {
              ResourceUtil.createResource(implName, StringTemplate.tbMonReadWrite(
                paramTypeName, PropertyUtil.getQueueSize(f, Util.DEFAULT_QUEUE_SIZE), monitorName, Util.getSbTypeHeaderFilenameForIncludes(), preventBadging), T)
            } else {
              ResourceUtil.createResource(implName, StringTemplate.tbEnqueueDequeue(
                paramTypeName, PropertyUtil.getQueueSize(f, Util.DEFAULT_QUEUE_SIZE), monitorName, Util.getSbTypeHeaderFilenameForIncludes(), preventBadging), T)
            }

          val headerFilename = Util.genCHeaderFilename(monitorName)
          val headerFilePath = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/includes/${headerFilename}"
          val contents = StringTemplate.cHeaderFile(headerFilename, ISZ(), ISZ())
          val cincludes = ResourceUtil.createResource(headerFilePath, contents, T)

          monitors = monitors + (connInstName ~>
            TB_Monitor(inst, interface,
              providesVarName,
              cimplementation, cincludes, i, connInst))
        }

        def buildIhorMonitor(f: ir.FeatureEnd): Unit = {
          val monitorName = Util.getMonitorName(dst, f)
          val instanceName = StringUtil.toLowerCase(monitorName)
          val interfaceNameReceiver = Util.getInterfaceNameIhor(f, F)
          val interfaceNameSender = Util.getInterfaceNameIhor(f, T)

          val typeName = Util.getClassifierFullyQualified(f.classifier.get)

          val providesReceiverVarName = "mon_receive"
          val providesSenderVarName = "mon_send"

          val paramType = typeMap.get(typeName).get
          val paramTypeName = Util.getSel4TypeName(paramType, performHamrIntegration)

          val monitor = Util.createCAmkESComponent(
            aadlThread = None(),
            componentCategory = CAmkESComponentCategory.Monitor,

            control = F,
            hardware = F,
            name = monitorName,

            mutexes = ISZ(Mutex(name ="m", comments = ISZ())),
            binarySemaphores = ISZ(),
            semaphores = ISZ(),
            dataports = ISZ(),
            emits = ISZ(
              Util.createEmits_Monitor(
                monitorName = instanceName,
                name = "monsig",
                typ = Util.MONITOR_EVENT_DATA_NOTIFICATION_TYPE)),
            uses = ISZ(),
            consumes = ISZ(),
            provides = ISZ(
              Util.createProvides_Monitor(monitorName = instanceName, name = providesReceiverVarName, typ = interfaceNameReceiver),
              Util.createProvides_Monitor(monitorName = instanceName, name = providesSenderVarName, typ = interfaceNameSender)),
            includes = ISZ(),
            attributes = ISZ(),
            preprocessorIncludes = ISZ(),
            imports = ISZ(
              st""""../../../${Util.DIR_INTERFACES}/${interfaceNameReceiver}.idl4"""".render,
              st""""../../../${Util.DIR_INTERFACES}/${interfaceNameSender}.idl4"""".render),

            externalEntities = ISZ()
          )

          val inst: Instance = Instance(
            address_space = "",
            name = instanceName,
            component = monitor,
            comments = ISZ())

          val receiveMethod = Method(
            name = "dequeue",
            parameters = ISZ(
              Parameter(array = F, direction = Direction.Out, name ="m", typ = paramTypeName, comments = ISZ())),
            returnType = Some("bool"),
            comments = ISZ())

          val interfaceReceiver: Procedure = Procedure(
            name = interfaceNameReceiver,
            methods = ISZ(receiveMethod),
            includes = ISZ(Util.getSbTypeHeaderFilenameForIncludes()),
            comments = ISZ()
          )

          val sendMethod = Method(
            name = "enqueue",
            parameters = ISZ(
              Parameter(array = F, direction = Direction.Refin, name = "m", typ = paramTypeName, comments = ISZ())),
            returnType = Some("bool"),
            comments = ISZ())

          val interfaceSender: Procedure = Procedure(
            name = interfaceNameSender,
            methods = ISZ(sendMethod),
            includes = ISZ(Util.getSbTypeHeaderFilenameForIncludes()),
            comments = ISZ()
          )

          val connInstName = CommonUtil.getName(connInst.name)

          val implName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/src/${Util.genCImplFilename(monitorName)}"
          val cimplementation: Resource =
            ResourceUtil.createResource(implName,
              StringTemplate.tbEnqueueDequeueIhor(paramTypeName,
                PropertyUtil.getQueueSize(f, Util.DEFAULT_QUEUE_SIZE),
                monitorName,
                Util.getSbTypeHeaderFilenameForIncludes(),
                preventBadging),
              T)

          val headerFilename = Util.genCHeaderFilename(monitorName)
          val headerFilePath = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/includes/${headerFilename}"
          val contents = StringTemplate.cHeaderFile(headerFilename, ISZ(), ISZ())
          val cincludes = ResourceUtil.createResource(headerFilePath, contents, T)

          monitors = monitors + (connInstName ~>
            Ihor_Monitor(inst, interfaceReceiver, interfaceSender,
              providesReceiverVarName, providesSenderVarName,
              cimplementation, cincludes, i, connInst))
        }

        def buildIhorEventMonitor(f: ir.FeatureEnd): Unit = {
          assert(f.category == ir.FeatureCategory.EventPort)

          val monitorName = Util.getMonitorName(dst, f)
          val instanceName = StringUtil.toLowerCase(monitorName)
          val interfaceNameReceiver = Util.getInterfaceNameIhor(f, F)
          val interfaceNameSender = Util.getInterfaceNameIhor(f, T)

          val providesReceiverVarName = "mon_receive"
          val providesSenderVarName = "mon_send"

          val monitor = Util.createCAmkESComponent(
            aadlThread = None(),
            componentCategory = CAmkESComponentCategory.Monitor,

            control = F,
            hardware = F,
            name = monitorName,

            mutexes = ISZ(Mutex(name = "m", comments = ISZ())),
            binarySemaphores = ISZ(),
            semaphores = ISZ(),
            dataports = ISZ(),
            emits = ISZ(
              Util.createEmits_Monitor(monitorName = instanceName, name = "monsig", typ = Util.MONITOR_EVENT_DATA_NOTIFICATION_TYPE)),
            uses = ISZ(),
            consumes = ISZ(),
            provides = ISZ(
              Util.createProvides_Monitor(monitorName = instanceName, name = providesReceiverVarName, typ = interfaceNameReceiver),
              Util.createProvides_Monitor(monitorName = instanceName, name = providesSenderVarName, typ = interfaceNameSender)),
            includes = ISZ(),
            attributes = ISZ(),
            preprocessorIncludes = ISZ(),
            imports = ISZ(
              st""""../../../${Util.DIR_INTERFACES}/${interfaceNameReceiver}.idl4"""".render,
              st""""../../../${Util.DIR_INTERFACES}/${interfaceNameSender}.idl4"""".render),

            externalEntities = ISZ()
          )

          val inst: Instance = Instance(address_space = "", name = instanceName, component = monitor, comments = ISZ())

          val isEmptyMethod = Method(
            name = "is_empty",
            parameters = ISZ(),
            returnType = Some("bool"),
            comments = ISZ())

          val receiveMethod = Method(
            name = "dequeue",
            parameters = ISZ(),
            returnType = Some("bool"),
            comments = ISZ())

          val interfaceReceiver: Procedure = Procedure(
            name = interfaceNameReceiver,
            methods = ISZ(isEmptyMethod, receiveMethod),
            includes = ISZ(Util.getSbTypeHeaderFilenameForIncludes()),
            comments = ISZ()
          )

          val sendMethod = Method(
            name = "enqueue",
            parameters = ISZ(),
            returnType = Some("bool"),
            comments = ISZ())

          val interfaceSender: Procedure = Procedure(
            name = interfaceNameSender,
            methods = ISZ(sendMethod),
            includes = ISZ(Util.getSbTypeHeaderFilenameForIncludes()),
            comments = ISZ())

          val connInstName = CommonUtil.getName(connInst.name)

          val implName = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/src/${Util.genCImplFilename(monitorName)}"
          val cimplementation: Resource =
            ResourceUtil.createResource(
              implName,
              StringTemplate.tbRaiseGetEvents(PropertyUtil.getQueueSize(f, Util.DEFAULT_QUEUE_SIZE), monitorName, preventBadging),
              T)

          val headerFilename = Util.genCHeaderFilename(monitorName)
          val headerFilePath = s"${Util.DIR_COMPONENTS}/${Util.DIR_MONITORS}/${monitorName}/includes/${headerFilename}"
          val headerContents = StringTemplate.cHeaderFile(headerFilename, ISZ(), ISZ())
          val cincludes = ResourceUtil.createResource(headerFilePath, headerContents, T)

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
      Method(name = "is_empty", parameters = ISZ(), returnType = Some("bool"), comments = ISZ()),
      Method(name = "read", parameters = ISZ(Parameter(array = F, direction = Direction.Out, name = "m", typ = typeName, comments = ISZ())), returnType = Some("bool"), comments = ISZ()),
      Method(name = "write", parameters = ISZ(Parameter(array =F, direction = Direction.Refin, name = "m", typ = typeName, comments = ISZ())), returnType = Some("bool"), comments = ISZ()))
  }

  def createQueueMethods(typeName: String): ISZ[Method] = {
    return ISZ(
      Method(name = "is_empty", parameters = ISZ(), returnType = Some("bool"), comments = ISZ()),
      Method(name = "enqueue", parameters = ISZ(Parameter(array = F, direction = Direction.Refin, name = "m", typ = typeName, comments = ISZ())), returnType = Some("bool"), comments = ISZ()),
      Method(name = "dequeue", parameters = ISZ(Parameter(array = F, direction = Direction.Out, name = "m", typ = typeName, comments = ISZ())), returnType = Some("bool"), comments = ISZ()))
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
      val defs = values.filter((v: ir.Component) => ActTypeUtil.translateBaseType(v.classifier.get.name).isEmpty).
        map((v: ir.Component) => processDataType(v, F))

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
        if (isField) {
          st"""$name"""
        } else {
          val fields: ISZ[ST] = c.subComponents.map(sub => {
            val fname: String = CommonUtil.getLastName(sub.identifier)
            if (ISZOps(Util.cKeywords).contains(fname)) {
              reporter.error(None(), Util.toolName, s"The attribute '${fname}' from data type ${name} is a reserved keyword")
            }
            st"${processDataType(sub, T)} ${fname};"
          })

          st"""typedef
              |  struct ${name} {
              |    ${(fields, "\n")}
              |  } ${name};"""
        }
      } else if (ActTypeUtil.isBaseType(c)) {
        st"${ActTypeUtil.translateBaseType(c.classifier.get.name)}"
      } else if (isField) {
        st"${Util.getClassifierFullyQualified(c.classifier.get)}"
      } else if (TypeUtil.isMissingType(c)) {
        StringTemplate.tbMissingType()
      } else if (TypeUtil.isEnumType(c)) {
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
      } else if (ActTypeUtil.isArrayDef(c)) {
        // TODO multidim arrays
        val name = Util.getClassifierFullyQualified(c.classifier.get)

        val container = Util.getContainerName(name)
        st"""typedef ${ActTypeUtil.getArrayBaseType(c)} ${name} [${ActTypeUtil.getArrayDimension(c)}];
            |
            |typedef
            |  struct ${container} {
            |    ${name} f;
            |  } ${container};"""
      } else if (TypeUtil.isMissingType(c)) {
        reporter.error(None(), Util.toolName, s"${TypeUtil.MISSING_AADL_TYPE} found")
        st""
      }
      else {
        reporter.error(None(), Util.toolName, s"Unexpected datatype: ${c}")
        st" "
      }


    return s
  }

  def generateGlueCodePortInterfaceMethod(aadlThread: AadlThread, p: AadlPort): Option[C_SimpleContainer] = {

    val component = aadlThread.component

    def handleDataPort(): Option[C_SimpleContainer] = {

      def dataport_TB_Profile(): Option[C_SimpleContainer] = {
        val (suffix, mod): (String, String) = p.direction match {
          case ir.Direction.In => ("read", "")
          case ir.Direction.Out => ("write", "const ")
          case x => halt(s"Unexpected direction: ${x}")
        }
        val name = Util.genMonitorFeatureName(p.identifier, None[Z]())
        val paramType = Util.getSel4TypeName(
          typeMap.get(Util.getClassifierFullyQualified(p.feature.classifier.get)).get, performHamrIntegration)

        val inter = Some(st"""bool ${name}_${suffix}(${mod}${paramType} * ${name});""")
        val impl: Option[ST] =
          if (suffix == "write") {
            symbolTable.outConnections.get(p.path) match {
              case Some(conns) =>
                var accum: ISZ[ST] = ISZ()
                var i = 0
                val result = Util.brand("result")

                while (i < conns.size) {
                  accum = accum :+ st"""${result} &= ${name}${i}_${suffix}((${paramType} *) ${name});"""
                  i = i + 1
                }

                val methodName = s"${name}_${suffix}"
                Some(
                  st"""/************************************************************************
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

      def dataport_SB_Profile(): Option[C_SimpleContainer] = {
        val samplingPort: SamplingPortInterface = getSamplingPort(p.feature)

        var interfaces: ISZ[ST] = ISZ()
        var implementations: ISZ[ST] = ISZ()
        var preInits: ISZ[ST] = ISZ()
        val postInits: ISZ[ST] = ISZ()

        val aadlDataPort: AadlDataPort = p.asInstanceOf[AadlDataPort]

        p.direction match {
          case ir.Direction.In =>
            // don't init data structure since consumers don't have the correct perms

            interfaces = interfaces :+
              StringTemplate.sbSamplingPortGetterInterface(samplingPort, aadlDataPort)

            implementations = implementations :+
              StringTemplate.sbSamplingPortGetterImplementation(samplingPort, aadlDataPort)

          case ir.Direction.Out =>

            preInits = preInits :+
              StringTemplate.sbSamplingPortInitialise(samplingPort, aadlDataPort)

            interfaces = interfaces :+
              StringTemplate.sbSamplingPortSetterInterface(samplingPort, aadlDataPort)

            implementations = implementations :+
              StringTemplate.sbSamplingPortSetterImplementation(samplingPort, aadlDataPort)

          case _ => halt(s"Unexpected direction ${aadlDataPort.path}")
        }

        val interface: Option[ST] = if (interfaces.isEmpty) None() else Some(st"${(interfaces, "\n")}")
        val implementation: Option[ST] = if (implementations.isEmpty) None() else Some(st"${(implementations, "\n")}")
        val preInit: Option[ST] = if (preInits.isEmpty) None() else Some(st"${(preInits, "\n")}")
        val postInit: Option[ST] = if (postInits.isEmpty) None() else Some(st"${(postInits, "\n")}")

        return Some(C_SimpleContainer(
          cIncludes = ISZ(),
          cInterface = interface,
          cImplementation = implementation,
          preInits = preInit,
          postInits = postInit,
          drainQueues = None()))
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
        assert(!useCaseConnectors)

        val featureName = Util.brand(p.identifier)

        val aadlPortType = typeMap.get(Util.getClassifierFullyQualified(p.feature.classifier.get)).get
        val sel4TypeName = Util.getSel4TypeName(aadlPortType, performHamrIntegration)

        val ret: Option[C_SimpleContainer] = p.direction match {
          case ir.Direction.In => {

            if (symbolTable.getInConnections(p.path).isEmpty) {
              return None()
            }

            val queueSize = PropertyUtil.getQueueSize(p.feature, Util.DEFAULT_QUEUE_SIZE)
            val queueHeaderName = Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, queueSize)
            val queueName = Util.getEventDataSBQueueName(sel4TypeName, queueSize)

            val featureQueueName = Util.getEventDataSBQueueDestFeatureName(p.identifier)
            val featureNotificationName = Util.genSeL4NotificationName(p.identifier, T)

            var preInits: ISZ[ST] = ISZ()
            var postInits: ISZ[ST] = ISZ()
            var drainQueue: Option[(ST, ST)] = None()
            var cInterfaces: ISZ[ST] = ISZ()
            var cImplementations: ISZ[ST] = ISZ()

            val counterVar = "numDropped"
            val counterType = Util.SB_EVENT_COUNTER_TYPE

            val recvQueueFeatureName = Util.getEventData_SB_RecvQueueFeatureName(p.identifier)
            val recvQueueName = Util.getEventData_SB_RecvQueueName(sel4TypeName, queueSize)
            val recvQueueType = Util.getEventData_SB_RecvQueueTypeName(sel4TypeName, queueSize)

            val dequeuePollMethodName = s"${featureName}_dequeue_poll"
            val dequeueMethodName = s"${featureName}_dequeue"
            val dequeueInfrastructureMethodName = s"${queueName}_dequeue"

            val isEmptyMethodName = s"${featureName}_is_empty"
            val isEmptyInfrastructureMethodName = s"${queueName}_is_empty"

            cInterfaces = cInterfaces :+ st"bool ${dequeueMethodName}(${sel4TypeName} *);"

            val baseCimpl =
              st"""${recvQueueType} ${recvQueueFeatureName};
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

            if (aadlThread.isSporadic()) {
              val handlerName = s"${featureNotificationName}_handler"
              val regCallback = s"${featureNotificationName}_reg_callback"

              cImplementations = cImplementations :+
                StringTemplate.cEventNotificationHandler(handlerName, regCallback, featureName)

              postInits = postInits :+ StringTemplate.cRegCallback(handlerName, regCallback, p)

              if (!performHamrIntegration) {
                Util.getComputeEntrypointSourceText(p.feature.properties) match {
                  case Some(userEntrypointMethodName) =>

                    val varName = featureName

                    val userEntrypointName = Util.getUserEventEntrypointMethodName(component, p)

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
                    val entrypoint =
                      st"""/************************************************************************
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

            val cIncludes: ISZ[ST] = ISZ(
              st"""#include <${queueHeaderName}>
                  |#include <${Util.SB_COUNTER_HEADER_FILENAME}>""")

            // init the consumer's receive queue, declared locally so access permitted
            preInits = preInits :+
              st"""// initialise data structure for incoming event data port ${p.identifier}
                  |${recvQueueName}_init(&${recvQueueFeatureName}, ${featureQueueName});"""

            val cInterface: Option[ST] = if (cInterfaces.isEmpty) None() else Some(st"${(cInterfaces, "\n\n")}")
            val cImplementation: Option[ST] = if (cImplementations.isEmpty) None() else Some(st"${(cImplementations, "\n\n")}")
            val preInit: Option[ST] = if (preInits.isEmpty) None() else Some(st"${(preInits, "\n\n")}")
            val postInit: Option[ST] = if (postInits.isEmpty) None() else Some(st"${(postInits, "\n\n")}")

            Some(C_SimpleContainer(
              cIncludes = cIncludes,
              cInterface = cInterface,
              cImplementation = cImplementation,
              preInits = preInit,
              postInits = postInit,
              drainQueues = drainQueue))
          }
          case ir.Direction.Out => {

            val dsts: Map[String, QueueObject] = srcQueues.get(p.path).get

            var cIncludes: ISZ[ST] = ISZ()

            var preInits: ISZ[ST] = ISZ()
            val postInits: ISZ[ST] = ISZ()

            val enqueueEntries: ISZ[ST] = dsts.valueSet.elements.map(qo => {

              val queueHeaderName = Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, qo.queueSize)
              val queueName = Util.getEventDataSBQueueName(sel4TypeName, qo.queueSize)

              val featureQueueName = Util.getEventDataSBQueueSrcFeatureName(p.identifier, qo.queueSize)
              val featureNotificationName = Util.genSeL4NotificationQueueName(p.identifier, qo.queueSize)

              cIncludes = cIncludes :+ st"#include <${queueHeaderName}>"
              preInits = preInits :+
                st"""// initialise data structure for outgoing event data port ${p.identifier}
                    |${queueName}_init(${featureQueueName});"""

              val notification: Option[ST] =
                if (!useCaseConnectors) Some(st"${featureNotificationName}_emit();")
                else None()

              st"""${queueName}_enqueue(${featureQueueName}, (${sel4TypeName}*) data);
                  |${notification}
                  |"""
            })

            val methodName = s"${featureName}_enqueue"

            val interface = st"""bool ${methodName}(const ${sel4TypeName} *);"""

            val st =
              st"""bool ${methodName}(const ${sel4TypeName} *data) {
                  |  ${(enqueueEntries, "\n")}
                  |  return true;
                  |}"""

            val cInterface: Option[ST] = Some(interface)
            val cImplementation: Option[ST] = Some(st)
            val preInit: Option[ST] = if (preInits.isEmpty) None() else Some(st"${(preInits, "\n\n")}")
            val postInit: Option[ST] = if (postInits.isEmpty) None() else Some(st"${(postInits, "\n\n")}")
            val drainQueues: Option[(ST, ST)] = None()

            Some(C_SimpleContainer(
              cIncludes = cIncludes,
              cInterface = cInterface,
              cImplementation = cImplementation,
              preInits = preInit,
              postInits = postInit,
              drainQueues = drainQueues))
          }
          case x => halt(s"Unexpected direction: ${x}")
        }
        return ret
      }

      def handleEventData_CASE_Conenctors_SB_GlueCode_Profile(): Option[C_SimpleContainer] = {
        assert(useCaseConnectors)

        val featureName = Util.brand(p.identifier)

        val aadlPortType = typeMap.get(Util.getClassifierFullyQualified(p.feature.classifier.get)).get
        val sel4TypeName = Util.getSel4TypeName(aadlPortType, performHamrIntegration)

        val ret: Option[C_SimpleContainer] = p.direction match {
          case ir.Direction.In => {

            if (symbolTable.getInConnections(p.path).isEmpty) {
              return None()
            }

            val queueSize = PropertyUtil.getQueueSize(p.feature, Util.DEFAULT_QUEUE_SIZE)
            val queueHeaderName = Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, queueSize)
            val queueName = Util.getEventDataSBQueueName(sel4TypeName, queueSize)

            val featureQueueName = Util.getEventDataSBQueueDestFeatureName(p.identifier)
            val featureNotificationName = featureQueueName

            var preInits: ISZ[ST] = ISZ()
            var postInits: ISZ[ST] = ISZ()
            var drainQueue: Option[(ST, ST)] = None()
            var cInterfaces: ISZ[ST] = ISZ()
            var cImplementations: ISZ[ST] = ISZ()

            val counterVar = "numDropped"
            val counterType = Util.SB_EVENT_COUNTER_TYPE

            val recvQueueFeatureName = Util.getEventData_SB_RecvQueueFeatureName(p.identifier)
            val recvQueueName = Util.getEventData_SB_RecvQueueName(sel4TypeName, queueSize)
            val recvQueueType = Util.getEventData_SB_RecvQueueTypeName(sel4TypeName, queueSize)

            val dequeuePollMethodName = s"${featureName}_dequeue_poll"
            val dequeueMethodName = s"${featureName}_dequeue"
            val dequeueInfrastructureMethodName = s"${queueName}_dequeue"

            val isEmptyMethodName = s"${featureName}_is_empty"
            val isEmptyInfrastructureMethodName = s"${queueName}_is_empty"

            cInterfaces = cInterfaces :+ st"bool ${dequeueMethodName}(${sel4TypeName} *);"

            val baseCimpl =
              st"""${recvQueueType} ${recvQueueFeatureName};
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

            if (aadlThread.isSporadic()) {
              val handlerName = s"${featureNotificationName}_handler"
              val regCallback = s"${featureNotificationName}_reg_callback"

              cImplementations = cImplementations :+ st"extern int ${regCallback}(void (*cb)(void*), void *arg);"

              cImplementations = cImplementations :+
                StringTemplate.cEventNotificationHandler(handlerName, regCallback, featureName)

              postInits = postInits :+ StringTemplate.cRegCallback(handlerName, regCallback, p)

              if (!performHamrIntegration) {
                Util.getComputeEntrypointSourceText(p.feature.properties) match {
                  case Some(userEntrypointMethodName) =>

                    val varName = featureName

                    val userEntrypointName = Util.getUserEventEntrypointMethodName(component, p)

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
                    val entrypoint =
                      st"""/************************************************************************
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

            val cIncludes: ISZ[ST] = ISZ(
              st"""#include <${queueHeaderName}>
                  |#include <${Util.SB_COUNTER_HEADER_FILENAME}>""")

            preInits = preInits :+
              st"""// initialise data structure for incoming event data port ${p.identifier}
                  |${recvQueueName}_init(&${recvQueueFeatureName}, ${featureQueueName});"""

            val cInterface: Option[ST] = if (cInterfaces.isEmpty) None() else Some(st"${(cInterfaces, "\n\n")}")
            val cImplementation: Option[ST] = if (cImplementations.isEmpty) None() else Some(st"${(cImplementations, "\n\n")}")
            val preInit: Option[ST] = if (preInits.isEmpty) None() else Some(st"${(preInits, "\n\n")}")
            val postInit: Option[ST] = if (postInits.isEmpty) None() else Some(st"${(postInits, "\n\n")}")

            Some(C_SimpleContainer(
              cIncludes = cIncludes,
              cInterface = cInterface,
              cImplementation = cImplementation,
              preInits = preInit,
              postInits = postInit,
              drainQueues = drainQueue))
          }
          case ir.Direction.Out => {

            val dsts: Map[String, QueueObject] = srcQueues.get(p.path).get

            var cIncludes: ISZ[ST] = ISZ()

            var preInits: ISZ[ST] = ISZ()
            val postInits: ISZ[ST] = ISZ()

            var externMethods: ISZ[ST] = ISZ()
            val enqueueEntries: ISZ[ST] = dsts.valueSet.elements.map(qo => {

              val queueHeaderName = Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, qo.queueSize)
              val queueName = Util.getEventDataSBQueueName(sel4TypeName, qo.queueSize)

              val featureQueueName = Util.getEventDataSBQueueSrcFeatureName(p.identifier, qo.queueSize)
              val featureNotificationName = s"${featureQueueName}_emit_underlying"

              cIncludes = cIncludes :+ st"#include <${queueHeaderName}>"
              preInits = preInits :+
                st"""// initialise data structure for outgoing event data port ${p.identifier}
                    |${queueName}_init(${featureQueueName});"""

              externMethods = externMethods :+ st"extern void ${featureNotificationName}(void);"

              st"""${queueName}_enqueue(${featureQueueName}, (${sel4TypeName}*) data);
                  |${featureNotificationName}();"""
            })

            val methodName = s"${featureName}_enqueue"

            val interface = st"""bool ${methodName}(const ${sel4TypeName} *);"""

            val st =
              st"""${(externMethods, "\n")}
                  |
                  |bool ${methodName}(const ${sel4TypeName} *data) {
                  |  ${(enqueueEntries, "\n")}
                  |  return true;
                  |}"""

            val cInterface: Option[ST] = Some(interface)
            val cImplementation: Option[ST] = Some(st)
            val preInit: Option[ST] = if (preInits.isEmpty) None() else Some(st"${(preInits, "\n\n")}")
            val postInit: Option[ST] = if (postInits.isEmpty) None() else Some(st"${(postInits, "\n\n")}")
            val drainQueues: Option[(ST, ST)] = None()

            Some(C_SimpleContainer(
              cIncludes = cIncludes,
              cInterface = cInterface,
              cImplementation = cImplementation,
              preInits = preInit,
              postInits = postInit,
              drainQueues = drainQueues))
          }
          case x => halt(s"Unexpected direction: ${x}")
        }
        return ret
      }

      def handleEventData_TB_GlueCode_Profile(): Option[C_SimpleContainer] = {

        val interfaceId = Util.genMonitorFeatureName(p.identifier, None[Z]())
        val paramType = Util.getSel4TypeName(typeMap.get(Util.getClassifierFullyQualified(p.feature.classifier.get)).get, performHamrIntegration)

        var interfaces: ISZ[ST] = ISZ()
        var drainQueue: Option[(ST, ST)] = None[(ST, ST)]()

        val impl: Option[ST] = p.direction match {
          case ir.Direction.Out =>
            symbolTable.outConnections.get(p.path) match {
              case Some(conns) =>

                // create an enqueue method for user that handles fan outs

                val genMethodName = s"${interfaceId}_enqueue"

                val methodSig = st"bool ${genMethodName}(const ${paramType} * ${interfaceId})"

                interfaces = interfaces :+ st"${methodSig};"

                var fanOuts: ISZ[ST] = ISZ()
                var i = 0
                val result = Util.brand("result")
                while (i < conns.size) {
                  fanOuts = fanOuts :+ st"""${result} &= ${interfaceId}${i}_enqueue((${paramType} *) ${interfaceId});"""
                  i = i + 1
                }

                Some(
                  st"""/************************************************************************
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

            if (aadlThread.isSporadic() && !performHamrIntegration) {
              val eventDataEntryPointCode: Option[ST] = Util.getComputeEntrypointSourceText(p.feature.properties) match {
                case Some(userEventHandlerMethodName) =>
                  val varName = Util.brand(p.identifier)
                  val userEntrypointName = Util.getUserEventEntrypointMethodName(component, p)

                  drainQueue = Some((st"""${paramType} ${varName};""",
                    st"""while (${monitorDequeueMethodName}((${paramType} *) &${varName})) {
                        |  ${userEntrypointName}(&${varName});
                        |}"""))

                  interfaces = interfaces :+ st"void ${userEventHandlerMethodName}(const ${paramType} * in_arg);"

                  Some(
                    st"""/************************************************************************
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

        val inter: Option[ST] = if (interfaces.isEmpty) None() else Some(st"${(interfaces, "\n\n")}")

        return Some(C_SimpleContainer(cIncludes = ISZ(), cInterface = inter, cImplementation = impl,
          preInits = None(), postInits = None(), drainQueues = drainQueue))
      }

      val ret: Option[C_SimpleContainer] = platform match {
        case ActPlatform.SeL4_TB => handleEventData_TB_GlueCode_Profile()

        case ActPlatform.SeL4 if !useCaseConnectors => handleEventData_SB_GlueCode_Profile()
        case ActPlatform.SeL4 if useCaseConnectors => handleEventData_CASE_Conenctors_SB_GlueCode_Profile()

        case ActPlatform.SeL4_Only if !useCaseConnectors => handleEventData_SB_GlueCode_Profile()
        case ActPlatform.SeL4_Only if useCaseConnectors => handleEventData_CASE_Conenctors_SB_GlueCode_Profile()

        case _ => None()
      }
      return ret
    }

    def handleEventPort(): Option[C_SimpleContainer] = {

      def handleEventPort_TB_Profile(): Option[C_SimpleContainer] = {

        val monitorEnqueueDequeueMethodName = Util.getEventPortSendReceiveMethodName(p)

        var interfaces: ISZ[ST] = ISZ()
        var implementations: ISZ[ST] = ISZ()
        var preInit: Option[ST] = None[ST]()
        var drainQueue: Option[(ST, ST)] = None[(ST, ST)]()

        p.direction match {
          case ir.Direction.In =>

            interfaces = interfaces :+ st"bool ${monitorEnqueueDequeueMethodName}(void);"

            if (aadlThread.isSporadic()) {

              val callback = s"${Util.genSeL4NotificationName(p.identifier, F)}_handler"
              val callback_reg = Util.brand(s"${p.identifier}_notification_reg_callback")
              val regCallback = st"CALLBACKOP(${callback_reg}(${callback}, NULL));"

              preInit = Some(regCallback)

              implementations = implementations :+
                st"""/************************************************************************
                    | * ${callback}:
                    | * Invoked by: seL4 notification callback
                    | *
                    | * This is the function invoked by an seL4 notification callback that
                    | * dispatches the active-thread due to the arrival of an event on
                    | * its ${p.identifier} event port
                    | *
                    | ************************************************************************/
                    |static void ${callback}(void *_ UNUSED){
                    |  MUTEXOP(${StringTemplate.SEM_POST}());
                    |  ${regCallback}
                    |}"""

              if (!performHamrIntegration) {
                Util.getComputeEntrypointSourceText(p.feature.properties) match {

                  case Some(computeEntryPointSourceText) =>
                    val entrypointMethodName = s"${Util.brand("entrypoint")}_${Util.getClassifier(component.classifier.get)}_${p.identifier}"

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

            symbolTable.outConnections.get(p.path) match {
              case Some(conns) =>
                var i = 0
                while (i < conns.size) {
                  val interfaceName = Util.brand(s"${p.identifier}${i}")
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

        val interface: Option[ST] = if (interfaces.isEmpty) None() else Some(st"${(interfaces, "\n\n")}")
        val implementation: Option[ST] = if (implementations.isEmpty) None() else Some(st"${(implementations, "\n\n")}")

        return Some(C_SimpleContainer(cIncludes = ISZ(), cInterface = interface, cImplementation = implementation,
          preInits = preInit, postInits = None(), drainQueues = drainQueue))
      }

      def handleEventPort_SB_Profile(): Option[C_SimpleContainer] = {
        val compName = Util.getClassifier(component.classifier.get)
        val counterName = Util.getEventSBCounterName(p.identifier)

        var preInits: ISZ[ST] = ISZ()
        var postInits: ISZ[ST] = ISZ()
        var interfaces: ISZ[ST] = ISZ()
        var implementations: ISZ[ST] = ISZ()
        var drainQueues: ISZ[ST] = ISZ()

        p.direction match {
          case ir.Direction.In =>

            val receivedEventsVarName = Util.brand(s"${p.identifier}_received_events")
            val lastCounterVarName = Util.brand(s"${p.identifier}_last_counter")

            val userPortInterfaceMethodName = Util.getEventPortSendReceiveMethodName(p)

            interfaces = interfaces :+ st"""bool ${userPortInterfaceMethodName}(void);"""

            val isEmptyMethodName: String = Util.brand(s"${p.identifier}_is_empty")

            implementations = implementations :+
              st"""/************************************************************************
                  | *
                  | * Static variables and queue management functions for event port:
                  | *     ${p.identifier}
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
                  | * Helper method to determine if infrastructure port has not received
                  | * any new events since the last dispatch
                  | *
                  | ************************************************************************/
                  |bool ${isEmptyMethodName}() {
                  |  return ${receivedEventsVarName} == 0;
                  |}"""

            val freezeEventsMethodName: String = StringTemplate.samplingPortFreezeMethodName(p)
            drainQueues = drainQueues :+ st"${freezeEventsMethodName}();"

            val currentCounter = s"current_${counterName}"
            val queueSize = PropertyUtil.getQueueSize(p.feature, Util.DEFAULT_QUEUE_SIZE)

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
                  |    // ${p.identifier}'s queue size is ${queueSize}
                  |    if(${receivedEventsVarName} > ${queueSize}) {
                  |      //printf("${compName}: dropping %i event(s) from incoming event port ${p.identifier}\n", (${receivedEventsVarName} - ${queueSize}));
                  |
                  |      // drop events
                  |      ${receivedEventsVarName} = ${queueSize};
                  |    }
                  |  }
                  |}"""

            implementations = implementations :+ freezeEvents

            if (aadlThread.isSporadic()) {

              val entrypointMethodName = s"${Util.brand("entrypoint")}_${Util.getClassifier(component.classifier.get)}_${p.identifier}"

              val callback = s"${Util.genSeL4NotificationName(p.identifier, F)}_handler"
              val callback_reg = Util.brand(s"${p.identifier}_reg_callback")

              implementations = implementations :+
                StringTemplate.cEventNotificationHandler(callback, callback_reg, p.identifier)

              postInits = postInits :+ StringTemplate.cRegCallback(callback, callback_reg, p)

              if (!performHamrIntegration) {
                Util.getComputeEntrypointSourceText(p.feature.properties) match {
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

            val sendMethodName = Util.getEventPortSendReceiveMethodName(p)
            val emit = Util.brand(s"${p.identifier}_emit()")

            interfaces = interfaces :+ st"bool ${sendMethodName}(void);"

            preInits = preInits :+
              st"""// initialise shared counter for event port ${p.identifier}
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

          case _ => halt(s"Unexpected direction ${p.direction}")
        }

        val interface: Option[ST] = if (interfaces.isEmpty) None() else Some(st"${(interfaces, "\n\n")}")
        val implementation: Option[ST] = if (implementations.isEmpty) None() else Some(st"${(implementations, "\n\n")}")
        val cIncludes: ISZ[ST] = ISZ(st"#include ${Util.getSbCounterFilenameForIncludes()}")
        val preInit: Option[ST] = if (preInits.isEmpty) None() else Some(st"${(preInits, "\n\n")}")
        val postInit: Option[ST] = if (postInits.isEmpty) None() else Some(st"${(postInits, "\n\n")}")
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

    val ret: Option[C_SimpleContainer] = p match {
      case a: AadlDataPort => handleDataPort()
      case a: AadlEventPort => handleEventPort()
      case a: AadlEventDataPort => handleEventDataPort()
      case _ => None[C_SimpleContainer]()
    }
    return ret
  }

  def genComponentTypeInterfaceFile(aadlThread: AadlThread, sts: ISZ[ST]): Resource = {
    val name = Util.getClassifier(aadlThread.component.classifier.get)
    val compTypeHeaderFilename = Util.genCHeaderFilename(Util.brand(name))

    val contents = StringTemplate.cHeaderFile(
      filename = compTypeHeaderFilename,
      includes = ISZ(Util.getSbTypeHeaderFilenameForIncludes()),
      entries = sts
    )

    return ResourceUtil.createResource(
      path = s"${PathUtil.getComponentHeaderPath(aadlThread, symbolTable)}/${compTypeHeaderFilename}",
      content = contents,
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

    val path = PathUtil.getComponentSourcePath(aadlThread, symbolTable)
    val componentHeaderFilename = PathUtil.getComponentGlueCodeHeaderFilename(aadlThread)
    val componentImplFilename = PathUtil.getComponentGlueCodeImplementationFilename(aadlThread)

    val isSeL4: B = platform == ActPlatform.SeL4

    val preInit: Option[ST] = StringTemplate.componentPreInitGlueCode(preInits, isSeL4, componentImplFilename)
    val postInit: Option[ST] = StringTemplate.componentPostInitGlueCode(postInits, isSeL4, componentImplFilename)

    val runMethod: ST = StringTemplate.runMethod(
      locals = ISZ(),
      initStmts = gcRunInitStmts,
      preLoopStmts = gcRunPreLoopStmts,
      loopStartStmts = gcRunLoopStartStmts,
      loopBodyStmts = gcRunLoopMidStmts,
      loopEndStmts = gcRunLoopEndStmts,
      postLoopStmts = ISZ(),
      containsFFIs = containsFFIs,
      isSeL4 = isSeL4,
      fileUri = componentImplFilename)

    val glueCodeImpl: ST = StringTemplate.componentTypeImpl(
      componentHeaderFilename = componentHeaderFilename,
      includes = localCIncludes,
      blocks = blocks,
      preInit = preInit,
      postInit = postInit,
      runMethod = runMethod)

    return ResourceUtil.createResource(
      path = s"${path}/${componentImplFilename}",
      content = glueCodeImpl,
      overwrite = T)
  }

  def buildTransportMechanisms(): B = {

    if (!hasErrors) {
      buildMonitors()
    }

    if (!hasErrors) {
      buildSBQueues()
    }

    if (!hasErrors) {
      buildSamplingPortInterfaces()
    }

    if (!hasErrors) {
      sharedData = SharedDataUtil.buildSharedData(symbolTable)
    }

    return !hasErrors
  }

  def sortData(data: ISZ[ir.Component]): ISZ[ir.Component] = {
    def u(_c: ir.Component): String = {
      return Util.getClassifierFullyQualified(_c.classifier.get)
    }

    // build dependence graph so that required data types are processed first
    var graph: Graph[ir.Component, String] = Graph.empty
    for (d <- data) {
      graph = graph * d
      if (TypeUtil.isRecordType(d)) {
        for (s <- d.subComponents) {
          val pair = (d, typeMap.get(u(s)).get)
          graph = graph + pair
        }
      } else if (ActTypeUtil.isArrayDef(d)) {
        val pair = (d, typeMap.get(ActTypeUtil.getArrayBaseType(d).get).get)
        graph = graph + pair
      } else {
        if (!(ActTypeUtil.isBaseType(d) || TypeUtil.isEnumType(d) || TypeUtil.isMissingType(d))) {
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

    def sortByClassifier(s: ISZ[ir.Component]): ISZ[ir.Component] = {
      return ISZOps(s).sortWith((a, b) => u(a) < u(b))
    }

    def sortDependents(c: ir.Component): Unit = {
      if (ISZOps(sorted).contains(c)) {
        return
      }
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

  def processTBConnections(c: ir.Component): (ISZ[ast.Connection], ISZ[ast.Configuration]) = {
    assert(platform == ActPlatform.SeL4_TB)
    val conn = TBConnections(monitors, sharedData, srcQueues, symbolTable, aadlTypes, actOptions)
    return conn.processConnections(c, connectionCounter)
  }

  def processSBConnections(): ConnectionContainer = {
    assert(platform == ActPlatform.SeL4_Only || platform == ActPlatform.SeL4)
    val conn = SBConnections(monitors, sharedData, srcQueues, aadlTypes, actOptions)
    return conn.processConnectionInstances(sbConnectionContainer.values, connectionCounter, symbolTable)
  }
}

