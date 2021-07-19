// #Sireum

package org.sireum.hamr.act.periodic

import org.sireum._
import org.sireum.hamr.act._
import org.sireum.hamr.act.ast.{Consumes, Dataport, Emits}
import org.sireum.hamr.act.connections.ConnectionHolder
import org.sireum.hamr.act.templates.{CAmkESTemplate, ConnectionsSbTemplate}
import org.sireum.hamr.act.util.Util.reporter
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.util.{ExperimentalOptions, ResourceUtil}

@datatype class Pacer(val symbolTable: SymbolTable,
                      val actOptions: ActOptions) extends PeriodicImpl {

  val performHamrIntegration: B = Util.hamrIntegration(actOptions.platform)

  val useCaseConnectors: B = ExperimentalOptions.useCaseConnectors(actOptions.experimentalOptions)

  def handlePeriodicComponents(connectionCounter: Counter,
                               timerAttributeCounter: Counter,
                               headerInclude: String): CamkesAssemblyContribution = {
    if(useCaseConnectors) {
      return handlePeriodicComponents_CASE_Connector(connectionCounter, timerAttributeCounter, headerInclude)
    }

    assert(!useCaseConnectors)

    var imports: ISZ[String] = ISZ()
    var instances: ISZ[ast.Instance] = ISZ()
    var connections: ISZ[ast.Connection] = ISZ()
    var configurations: ISZ[ST]= ISZ()
    var cContainers: ISZ[C_Container] = ISZ()
    var auxResources: ISZ[Resource] = ISZ()

    imports = imports :+ PacerTemplate.pacerImport()

    val threads = symbolTable.getPeriodicThreads()
    
    if(threads.nonEmpty) {

      auxResources = auxResources ++ getSchedule(threads)
      
      // TODO handle components with different periods
      var emits: ISZ[ast.Emits] = ISZ()
      var dataports: ISZ[ast.Dataport] = ISZ()
      var includes: ISZ[String] = ISZ()

      var gcPacerIncludes: ISZ[String] = ISZ()
      var gcPacerImplEntries: ISZ[ST] = ISZ()
      var gcPacerMethods: ISZ[ST] = ISZ(CAmkESTemplate.externGetInstanceName())
      var gcPacerInitEntries: ISZ[ST] = ISZ()

      // add pacer domain configuration
      configurations = configurations :+ PacerTemplate.domainConfiguration(PacerTemplate.PACER_IDENTIFIER, PacerTemplate.PACER_DOMAIN)
      
      // tick/tock connection
      connections = connections :+ Util.createConnection(
        Util.getConnectionName(connectionCounter.increment()),
        Sel4ConnectorTypes.seL4Notification,
        PacerTemplate.PACER_IDENTIFIER, PacerTemplate.PACER_TICK_IDENTIFIER,
        PacerTemplate.PACER_IDENTIFIER, PacerTemplate.PACER_TOCK_IDENTIFIER)

      var requiresEmits = F
      var requiresDataportVMs = F

      for (aadlThread <- threads) {
        val componentId = Util.getCamkesComponentIdentifier(aadlThread, symbolTable)

        val isVM = aadlThread.toVirtualMachine(symbolTable)

        requiresDataportVMs = requiresDataportVMs | isVM
        requiresEmits = requiresEmits | !isVM

        if(isVM) {

          val pacerDataportName = PacerTemplate.pacerVM_PacerPeriodDataportIdentifier(componentId)
          val pacerEmitName = PacerTemplate.pacerVM_PacerPeriodEmitsIdentifier(componentId)

          dataports = dataports :+ dataportPeriod(pacerDataportName)

          if(!useCaseConnectors) {
            emits = emits :+ emitPeriodVM(pacerEmitName)
          }

          gcPacerImplEntries = gcPacerImplEntries :+ gcSendPeriodToVM(componentId)

          val pacerNotificationName: String =
            if(useCaseConnectors) {
              val sig = s"${pacerDataportName}_emit_underlying"
              gcPacerMethods = gcPacerMethods :+ st"extern void ${sig}(void);"
              sig
            } else {
              PacerTemplate.pacerVM_PacerEmitPeriodToVMMethodName(componentId)
            }

          gcPacerMethods = gcPacerMethods :+ PacerTemplate.pacerVM_PacerGcSendPeriodMethod(
            componentId,
            PacerTemplate.pacerDataportQueueElemType(),
            PacerTemplate.pacerDataportQueueSize(),
            pacerNotificationName)

          gcPacerInitEntries = gcPacerInitEntries :+
            PacerTemplate.pacerVM_PacerGcInitMethodEntry(
              componentId,
              PacerTemplate.pacerDataportQueueElemType(),
              PacerTemplate.pacerDataportQueueSize())

          if(!useCaseConnectors) {
            // connect notification to client
            connections = connections :+ Util.createConnection(
              connectionName = Util.getConnectionName(connectionCounter.increment()),
              connectionType = Sel4ConnectorTypes.seL4GlobalAsynch,
              srcComponent = PacerTemplate.PACER_IDENTIFIER,
              srcFeature = pacerEmitName,
              dstComponent = componentId,
              dstFeature = PacerTemplate.pacerVM_ClientPeriodNotificationIdentifier())
          }

          val dataportConnectorType: Sel4ConnectorTypes.Type =
            if(useCaseConnectors) Sel4ConnectorTypes.CASE_AADL_EventDataport
            else Sel4ConnectorTypes.seL4SharedDataWithCaps

          val connectionName: String = Util.getConnectionName(connectionCounter.increment())

          // connect queue to client
          connections = connections :+ Util.createConnection(
            connectionName = connectionName,
            connectionType = dataportConnectorType,
            srcComponent = PacerTemplate.PACER_IDENTIFIER,
            srcFeature = pacerDataportName,
            dstComponent = componentId,
            dstFeature = PacerTemplate.pacerVM_ClientPeriodDataportIdentifier())

          if(useCaseConnectors) {
            configurations = configurations :+
              ConnectionsSbTemplate.caseConnectorConfig_with_signalling(connectionName, T)

            configurations = configurations :+
              ConnectionsSbTemplate.caseConnectorConfig_connection_type(PacerTemplate.PACER_IDENTIFIER, pacerDataportName, F)

            configurations = configurations :+
              ConnectionsSbTemplate.caseConnectorConfig_connection_type(componentId, PacerTemplate.pacerVM_ClientPeriodDataportIdentifier(), T)
          }
        } else {

          // connect period notification to client
          connections = connections :+ Util.createConnection(
            connectionName = Util.getConnectionName(connectionCounter.increment()),
            connectionType = Sel4ConnectorTypes.seL4Notification,
            srcComponent = PacerTemplate.PACER_IDENTIFIER,
            srcFeature = PacerTemplate.pacerPeriodEmitIdentifier(),
            dstComponent = componentId,
            dstFeature = PacerTemplate.pacerClientNotificationIdentifier())
        }
      }

      gcPacerMethods = gcPacerMethods :+ PacerTemplate.pacerVM_PacerGcInitMethod(gcPacerInitEntries)

      if(requiresEmits) {
        emits = emits :+ emitPeriod(PacerTemplate.PACER_PERIOD_EMIT_IDENTIFIER)
        gcPacerImplEntries = gcPacerImplEntries :+ gcEmitPeriod(PacerTemplate.PACER_PERIOD_EMIT_IDENTIFIER)
      }

      if(requiresDataportVMs) {

        includes = includes :+ PacerTemplate.pacerDataportFilenameForIncludes()

        gcPacerIncludes = gcPacerIncludes :+ PacerTemplate.pacerDataportFilenameForIncludes()
      }

      val pacerComponent: ast.Instance = genPacerCamkesComponent(includes, emits, dataports)
      
      instances = instances :+ pacerComponent
      
      val pacerCCode: Resource = genPacerGlueCode(gcPacerIncludes, gcPacerMethods, gcPacerImplEntries)

      val externalLibs: ISZ[String] = ISZ(Util.SBTypeLibrary)

      cContainers = cContainers :+ C_Container(
        instanceName = pacerComponent.component.name,
        componentId = pacerComponent.component.name,
        cSources = ISZ(pacerCCode),
        cIncludes = ISZ(),
        sourceText = ISZ(),
        cmakeSOURCES = ISZ(),
        cmakeINCLUDES = ISZ(),
        cmakeLIBS = externalLibs
      )
    }

    val aadlProcessor = PeriodicUtil.getBoundProcessor(symbolTable)
    val maxDomain: Z = aadlProcessor.getMaxDomain() match {
      case Some(z) => z + 1
      case _ => symbolTable.computeMaxDomain()
    }
    val settingCmakeEntries: ISZ[ST] = ISZ(PacerTemplate.settings_cmake_entries(maxDomain))

    return CamkesAssemblyContribution(imports, instances, connections, configurations, cContainers,
      settingCmakeEntries, auxResources)
  }

  def handlePeriodicComponents_CASE_Connector(connectionCounter: Counter,
                               timerAttributeCounter: Counter,
                               headerInclude: String): CamkesAssemblyContribution = {
    assert(useCaseConnectors)

    var imports: ISZ[String] = ISZ()
    var instances: ISZ[ast.Instance] = ISZ()
    var connections: ISZ[ast.Connection] = ISZ()
    var configurations: ISZ[ST]= ISZ()
    var cContainers: ISZ[C_Container] = ISZ()
    var auxResources: ISZ[Resource] = ISZ()

    imports = imports :+ PacerTemplate.pacerImport()

    val threads = symbolTable.getPeriodicThreads()

    var map: HashSMap[ast.ConnectionEnd, ConnectionHolder] = HashSMap.empty

    def getConnectionHolder(connectionEnd: ast.ConnectionEnd, connectionType: Sel4ConnectorTypes.Type): ConnectionHolder = {
      if (!map.contains(connectionEnd)) {
        val connectionName = Util.getConnectionName(connectionCounter.increment())
        map = map + (connectionEnd ~> ConnectionHolder(connectionName, connectionType, MSZ(), MSZ()))
      }

      return map.get(connectionEnd).get
    }

    def updateHolder(end: ast.ConnectionEnd, holder: ConnectionHolder): Unit = {
      map = map + (end ~> holder)
    }

    if(threads.nonEmpty) {

      auxResources = auxResources ++ getSchedule(threads)

      // TODO handle components with different periods
      var emits: ISZ[ast.Emits] = ISZ()
      var dataports: Set[ast.Dataport] = Set.empty
      var includes: ISZ[String] = ISZ()

      var gcPacerIncludes: ISZ[String] = ISZ()
      var gcPacerImplEntries: ISZ[ST] = ISZ()
      var gcPacerMethods: ISZ[ST] = ISZ(CAmkESTemplate.externGetInstanceName())
      var gcPacerInitEntries: ISZ[ST] = ISZ()

      // add pacer domain configuration
      configurations = configurations :+ PacerTemplate.domainConfiguration(PacerTemplate.PACER_IDENTIFIER, PacerTemplate.PACER_DOMAIN)

      // tick/tock connection
      connections = connections :+ Util.createConnection(
        Util.getConnectionName(connectionCounter.increment()),
        Sel4ConnectorTypes.seL4Notification,
        PacerTemplate.PACER_IDENTIFIER, PacerTemplate.PACER_TICK_IDENTIFIER,
        PacerTemplate.PACER_IDENTIFIER, PacerTemplate.PACER_TOCK_IDENTIFIER)

      var requiresEmits = F
      var requiresDataportVMs = F

      val srcCamkesComponentId = PacerTemplate.PACER_IDENTIFIER
      val srcCamkesVMFeatureQueueName = PacerTemplate.pacerVM_CaseConnectorDataportIdentifier()

      var hasPeriodicVMComponents: B = F

      for (aadlThread <- threads) {

        val dstCamkesComponentId = Util.getCamkesComponentIdentifier(aadlThread, symbolTable)

        val isVM = aadlThread.toVirtualMachine(symbolTable)

        requiresDataportVMs = requiresDataportVMs | isVM
        requiresEmits = requiresEmits | !aadlThread.toVirtualMachine(symbolTable)

        if(isVM) {
          // CASE eventdata port connector
          hasPeriodicVMComponents = T

          val dstCamkesFeatureQueueName = PacerTemplate.pacerVM_ClientPeriodDataportIdentifier()

          val queueConnectorType: Sel4ConnectorTypes.Type =
            Sel4ConnectorTypes.CASE_AADL_EventDataport

          val srcConnectionEnd = Util.createConnectionEnd(T, srcCamkesComponentId, srcCamkesVMFeatureQueueName)
          val dstConnectionEnd = Util.createConnectionEnd(F, dstCamkesComponentId, dstCamkesFeatureQueueName)

          var holder = getConnectionHolder(srcConnectionEnd, queueConnectorType)

          holder = holder(toConnectionEnds = holder.toConnectionEnds :+ dstConnectionEnd)

          holder = holder(configurationEntries = holder.configurationEntries :+
            ConnectionsSbTemplate.caseConnectorConfig_with_signalling(holder.connectionName, T).render)

          holder = holder(configurationEntries = holder.configurationEntries :+
            ConnectionsSbTemplate.caseConnectorConfig_connection_type(srcCamkesComponentId, srcCamkesVMFeatureQueueName, F).render)

          holder = holder(configurationEntries = holder.configurationEntries :+
            ConnectionsSbTemplate.caseConnectorConfig_connection_type(dstCamkesComponentId, dstCamkesFeatureQueueName, T).render)

          updateHolder(srcConnectionEnd, holder)

        } else {
          // sel4 connection

          val connectionType = Sel4ConnectorTypes.seL4Notification
          val srcFeatureName = PacerTemplate.pacerPeriodEmitIdentifier()
          val dstFeatureName = PacerTemplate.pacerClientNotificationIdentifier()

          val srcConnectionEnd = Util.createConnectionEnd(T, srcCamkesComponentId, srcFeatureName)
          val dstConnectionEnd = Util.createConnectionEnd(F, dstCamkesComponentId, dstFeatureName)

          var holder = getConnectionHolder(srcConnectionEnd, connectionType)

          holder = holder(toConnectionEnds = holder.toConnectionEnds :+ dstConnectionEnd)

          updateHolder(srcConnectionEnd, holder)

        }
      }

      if(hasPeriodicVMComponents) {
        dataports = dataports + dataportPeriod(srcCamkesVMFeatureQueueName)

        val sendMethodName = s"send_${srcCamkesVMFeatureQueueName}"

        val emitMethodName = s"${srcCamkesVMFeatureQueueName}_emit_underlying"

        gcPacerMethods = gcPacerMethods :+ st"extern void ${emitMethodName}(void);"

        gcPacerImplEntries = gcPacerImplEntries :+ PacerTemplate.pacerVM_PacerGCSendPeriod_Case_Connector(
          emitMethodName,
          srcCamkesVMFeatureQueueName,
          PacerTemplate.PACER_TICK_COUNT_IDENTIFIER,
          PacerTemplate.pacerDataportQueueElemType(),
          PacerTemplate.pacerDataportQueueSize())

        gcPacerInitEntries = gcPacerInitEntries :+
          PacerTemplate.pacerVM_PacerGcInitMethodEntry_Case_Connector(
            srcCamkesVMFeatureQueueName,
            PacerTemplate.pacerDataportQueueElemType(),
            PacerTemplate.pacerDataportQueueSize())
      }

      gcPacerMethods = gcPacerMethods :+ PacerTemplate.pacerVM_PacerGcInitMethod(gcPacerInitEntries)

      if(requiresEmits) {
        emits = emits :+ emitPeriod(PacerTemplate.PACER_PERIOD_EMIT_IDENTIFIER)
        gcPacerImplEntries = gcPacerImplEntries :+ gcEmitPeriod(PacerTemplate.PACER_PERIOD_EMIT_IDENTIFIER)
      }

      if(requiresDataportVMs) {

        includes = includes :+ PacerTemplate.pacerDataportFilenameForIncludes()

        gcPacerIncludes = gcPacerIncludes :+ PacerTemplate.pacerDataportFilenameForIncludes()
      }

      val pacerComponent: ast.Instance = genPacerCamkesComponent(includes, emits, dataports.elements)

      instances = instances :+ pacerComponent

      val pacerCCode: Resource = genPacerGlueCode(gcPacerIncludes, gcPacerMethods, gcPacerImplEntries)

      val externalLibs: ISZ[String] = ISZ(Util.SBTypeLibrary)

      cContainers = cContainers :+ C_Container(
        instanceName = pacerComponent.component.name,
        componentId = pacerComponent.component.name,
        cSources = ISZ(pacerCCode),
        cIncludes = ISZ(),
        sourceText = ISZ(),
        cmakeSOURCES = ISZ(),
        cmakeINCLUDES = ISZ(),
        cmakeLIBS = externalLibs
      )
    }

    val maxDomain: Z = symbolTable.computeMaxDomain()
    val settingCmakeEntries: ISZ[ST] = ISZ(PacerTemplate.settings_cmake_entries(maxDomain))

    for (connectionHolderEntry <- map.entries) {
      val fromEnd = connectionHolderEntry._1
      val holder = connectionHolderEntry._2

      connections = connections :+ Util.createConnections(
        holder.connectionName,
        holder.connectionType,
        ISZ(fromEnd),
        holder.toConnectionEnds.toIS)

      val filtered = Set.empty[String] ++ holder.configurationEntries.toIS
      configurations = configurations ++ filtered.elements.map((m: String) => st"$m")
    }

    return CamkesAssemblyContribution(imports, instances, connections, configurations, cContainers,
      settingCmakeEntries, auxResources)
  }

  def handlePeriodicComponent(aadlThread: AadlThread): (CamkesComponentContributions, CamkesGlueCodeContributions) = {
    assert(aadlThread.isPeriodic())
    
    val component = aadlThread.component
    val classifier = Util.getClassifier(component.classifier.get)

    var consumes: ISZ[ast.Consumes] = ISZ()
    var dataports: ISZ[ast.Dataport] = ISZ()
    var includes: ISZ[String] = ISZ()

    var gcHeaderMethods: ISZ[ST] = ISZ()

    var gcMethods: ISZ[ST] = ISZ()
    var gcMainPreLoopStms: ISZ[ST] = ISZ()
    var gcMainLoopStartStms: ISZ[ST] = ISZ()
    var gcMainLoopStms: ISZ[ST]= ISZ()

    // initial pacer/period wait
    gcMainPreLoopStms = gcMainPreLoopStms :+ PacerTemplate.pacerWait()

    // pacer/period wait at start of loop
    gcMainLoopStartStms = gcMainLoopStartStms :+ PacerTemplate.pacerWait()

    if(!performHamrIntegration) {
      // get user defined time triggered method 
      Util.getComputeEntrypointSourceText(component.properties) match {
        case Some(handler) =>
          // header method so developer knows required signature
          gcHeaderMethods = gcHeaderMethods :+ st"void ${handler}(const int64_t * in_arg);"
                     
          gcMethods = gcMethods :+ PacerTemplate.wrapPeriodicComputeEntrypoint(classifier, handler) 
          
          gcMainLoopStms = gcMainLoopStms :+ PacerTemplate.callPeriodicComputEntrypoint(classifier, handler)
  
        case _ =>
          reporter.warn(None(), Util.toolName, s"Periodic thread ${classifier} is missing property ${Util.PROP_TB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT} and will not be dispatched")
      }
    }

    if(aadlThread.toVirtualMachine(symbolTable)) {

      // TODO: any reason not to use int8 with size 1 ??
      val queueType = Util.getEventDataSBQueueTypeName(
        PacerTemplate.pacerDataportQueueElemType(),
        PacerTemplate.pacerDataportQueueSize())

      if(!useCaseConnectors) {
        consumes = consumes :+ ast.Consumes(
          name = PacerTemplate.pacerVM_ClientPeriodNotificationIdentifier(),
          typ = PacerTemplate.PACER_PERIOD_VM_TYPE,
          optional = F)
      }

      dataports = dataports :+ ast.Dataport(
        name = PacerTemplate.pacerVM_ClientPeriodDataportIdentifier(),
        typ = queueType,
        optional = F)

    } else {
      consumes = consumes :+ ast.Consumes(
        name = PacerTemplate.pacerClientNotificationIdentifier(),
        typ = PacerTemplate.PACER_PERIOD_TYPE,
        optional = F)
    }

    val shell = ast.Component(
      consumes = consumes,
      dataports = dataports,
      includes = includes,
      // filler
      control = F, hardware = F, name = "", mutexes = ISZ(), binarySemaphores = ISZ(), semaphores = ISZ(),
      imports = ISZ(), emits = ISZ(), uses = ISZ(), provides = ISZ(), attributes = ISZ(),
      preprocessorIncludes = ISZ(), externalEntities = ISZ()
    )

    val componentContributions = CamkesComponentContributions(shell)

    val glueCodeContributions = CamkesGlueCodeContributions(
      CamkesGlueCodeHeaderContributions(includes = ISZ(), methods = gcHeaderMethods),
      CamkesGlueCodeImplContributions(includes = ISZ(), globals = ISZ(), methods = gcMethods, preInitStatements = ISZ(),
        postInitStatements = ISZ(),
        mainPreLoopStatements = gcMainPreLoopStms,
        mainLoopStartStatements = gcMainLoopStartStms, mainLoopStatements = gcMainLoopStms, mainLoopEndStatements = ISZ(),
        mainPostLoopStatements = ISZ()
      )
    )

    return (componentContributions, glueCodeContributions)
  }

  def emitPeriod(id: String): Emits = {
    return Emits(
      name = id,
      typ = PacerTemplate.PACER_PERIOD_TYPE)
  }

  def emitPeriodVM(id: String) : Emits = {
    return Emits(
      name = id,
      typ = PacerTemplate.PACER_PERIOD_VM_TYPE)
  }

  def dataportPeriod(id: String): Dataport = {
    return Dataport(
      name = id,
      typ = PacerTemplate.pacerDataportQueueType(),
      optional = F
    )
  }

  def gcEmitPeriod(id: String): ST = { return st"${id}_emit()" }

  def gcSendPeriodToVM_Case_Connector(sendMethodName: String): ST = {
    return st"${sendMethodName}(&${PacerTemplate.PACER_TICK_COUNT_IDENTIFIER})"
  }


  def gcSendPeriodToVM(vmProcessId: String): ST = {
    return st"${PacerTemplate.pacerVM_PacerSendPeriodToVmMethodName(vmProcessId)}(&${PacerTemplate.PACER_TICK_COUNT_IDENTIFIER})"
  }

  def genPacerCamkesComponent(includes: ISZ[String],
                              periods: ISZ[Emits],
                              dataports: ISZ[Dataport]): ast.Instance = {
    val emits: Emits = Emits(
      name = PacerTemplate.PACER_TICK_IDENTIFIER,
      typ = PacerTemplate.PACER_TICK_TOCK_TYPE)

    val consumes: Consumes = Consumes(
      name = PacerTemplate.PACER_TOCK_IDENTIFIER,
      typ = PacerTemplate.PACER_TICK_TOCK_TYPE,
      optional = F)

    val instance: ast.Instance = ast.Instance(
      address_space = "",
      name = PacerTemplate.PACER_IDENTIFIER,
      component = ast.Component(
        control =  T,
        hardware = F,
        name = PacerTemplate.PACER_COMPONENT_TYPE,
        mutexes = ISZ(),
        binarySemaphores = ISZ(),
        semaphores = ISZ(),
        dataports = dataports,
        emits = periods :+ emits,
        uses = ISZ(),
        consumes = ISZ(consumes),
        provides = ISZ(),
        includes = includes,
        attributes = ISZ(),
        preprocessorIncludes = ISZ(),
        imports = ISZ(),
        externalEntities = ISZ()
      )
    )
    return instance
  }

  def genPacerGlueCode(gcIncludes: ISZ[String],
                       gcMethods: ISZ[ST],
                       gcLoopEntries: ISZ[ST]): Resource = {
    val glueCode = PacerTemplate.pacerGlueCode(gcIncludes, gcMethods, gcLoopEntries)
 
    return ResourceUtil.createStResource(PacerTemplate.pacerGlueCodePath(), glueCode, T)
  }
  
  def getSchedule(allThreads: ISZ[AadlThread]): ISZ[Resource] = {

    val aadlProcessor = PeriodicUtil.getBoundProcessor(symbolTable)

    val path = "kernel/domain_schedule.c"

    val contents: ST = aadlProcessor.getScheduleSourceText() match {
      case Some(path) =>
        if(Os.path(path).exists) {
          val p = Os.path(path)
          st"${p.read}"
        } else {
          actOptions.aadlRootDirectory match {
            case Some(root) =>
              val candidate = Os.path(root) / path
              if(candidate.exists) {
                st"${candidate.read}"
              } else {
                halt(s"Could not locate Schedule_Source_Text ${candidate}")
              }
            case _ => halt(s"Unexpected: Couldn't locate Schedule_Source_Text ${path}")
          }
        }
      case _ => 
        var entries: ISZ[ST] = ISZ()

        val clockPeriod: Z = aadlProcessor.getClockPeriod() match {
          case Some(z) => z
          case _ => halt("Unexpected: Clock_Period not specified")
        }
        
        val framePeriod: Z = aadlProcessor.getFramePeriod() match {
          case Some(z) => z
          case _ => halt("Unexpected: Frame_Period not specified")
        }
        
        val otherLen = z"200"
        entries = entries :+ PacerTemplate.pacerScheduleEntry(z"0", otherLen / clockPeriod, 
          Some(st" // all other seL4 threads, init, ${otherLen}ms"))
        
        val pacerLen = z"10" 
        val pacerDomain = PacerTemplate.PACER_DOMAIN
        entries = entries :+ PacerTemplate.pacerScheduleEntry(pacerDomain, pacerLen / clockPeriod, 
          Some(st" // pacer ${pacerLen}ms.  Should always be in domain ${pacerDomain}"))
        
        var threadComments: ISZ[ST] = ISZ()
        var sumExecutionTime = z"0"
        for(p <- allThreads) {
          val domain = p.getDomain(symbolTable).get
          val computeExecutionTime = p.getMaxComputeExecutionTime()
          val comment = Some(st" // ${p.identifier}  ${computeExecutionTime}ms")

          threadComments = threadComments :+ 
            PacerTemplate.pacerScheduleThreadPropertyComment(p.identifier,
              domain, p.dispatchProtocol, computeExecutionTime, p.period)

          entries = entries :+ PacerTemplate.pacerScheduleEntry(domain, computeExecutionTime / clockPeriod, comment)
          
          sumExecutionTime = sumExecutionTime + computeExecutionTime
        }
        
        val pad: Z = (framePeriod - (otherLen + pacerLen + sumExecutionTime)) / clockPeriod
        entries = entries :+ PacerTemplate.pacerScheduleEntry(z"0", pad, Some(st" // pad rest of frame period"))
        
        PacerTemplate.pacerExampleSchedule(clockPeriod, framePeriod, threadComments, entries)
    }

    return ISZ(ResourceUtil.createStResource(path, contents, aadlProcessor.getScheduleSourceText().nonEmpty))
  }
}
