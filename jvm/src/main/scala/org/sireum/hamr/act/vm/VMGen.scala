// #Sireum
package org.sireum.hamr.act.vm

import org.sireum._
import org.sireum.hamr.act.ast._
import org.sireum.hamr.act.connections.{Connections, SBConnectionContainer, VMConnectionInfo}
import org.sireum.hamr.act.periodic.{Dispatcher, PacerTemplate, PeriodicUtil}
import org.sireum.hamr.act.proof.ProofContainer.CAmkESComponentCategory
import org.sireum.hamr.act.templates.{CMakeTemplate, EventDataQueueTemplate}
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlDataPort, AadlEventDataPort, AadlPort, AadlProcess, AadlThread, AadlVirtualProcessor, Dispatch_Protocol, SymbolTable}
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.codegen.common.{CommonUtil, DirectoryUtil}
import org.sireum.hamr.ir

object VMGen {

  val VM_COMPONENT_TYPE_NAME: String = "VM"
  val VM_ID_PREFIX: String = "vm" // has to be 'vm' as per data61 macros in https://github.com/SEL4PROJ/camkes-arm-vm/blob/6c77a2734ea4c77035f2c3cca5ca7fa72f1f2890/components/VM/configurations/vm.h#L93

  val DIR_VM: String = VM_COMPONENT_TYPE_NAME
  val DIR_VM_APPS: String = "apps"
  val DIR_VM_EXYNOS5422: String = "exynos5422"
  val DIR_VM_OVERLAY_FILES: String = "overlay_files"
  val DIR_VM_OVERLAY_FILES_INIT_SCRIPT: String = s"${DIR_VM_OVERLAY_FILES}/init_scripts"
  val DIR_VM_QEMU_ARM_VIRT: String = "qemu-arm-virt"
  val DIR_VM_SRC: String = "src"

  val CAMKES_PREPROCESSOR_INCLUDES: String = "<configurations/vm.h>" // see https://github.com/SEL4PROJ/camkes-arm-vm/blob/6c77a2734ea4c77035f2c3cca5ca7fa72f1f2890/components/VM/configurations/vm.h

  def getRootVMDir(): String = {
    return s"${Util.DIR_COMPONENTS}/${DIR_VM}"
  }

  def getAuxResources(//threadsToVMs: ISZ[AadlThread],
                      processesToVMs: ISZ[AadlProcess],
                      platform: ActPlatform.Type,
                      symbolTable: SymbolTable): ISZ[Resource] = {
    assert(processesToVMs.nonEmpty, "Expecting 1 or more processes going to VMs")
    var auxResourceFiles: ISZ[Resource] = ISZ()

    val projectRoot = s"$${CMAKE_CURRENT_SOURCE_DIR}/../.."

    var libNames: ISZ[String] = ISZ(Util.SBTypeLibrary)

    var vmVars: ISZ[ST] = ISZ(
      VM_Template.vm_cmake_var(
        VM_Template.makeDirVariable(Util.SBTypeLibrary), s"${projectRoot}/${Util.getTypeRootPath()}"))

    var appAddSubdirs: ISZ[ST] = ISZ(
      CMakeTemplate.cmake_add_subdirectory_binned(
        VM_Template.cmakeReferenceVar(VM_Template.makeDirVariable(Util.SBTypeLibrary)),
        Some(Util.SBTypeLibrary)))

    if (platform == ActPlatform.SeL4) {
      libNames = libNames :+ Util.SlangTypeLibrary

      appAddSubdirs = CMakeTemplate.cmake_add_subdirectory_binned(
        VM_Template.cmakeReferenceVar(VM_Template.makeDirVariable(Util.SlangTypeLibrary)),
        Some(Util.SlangTypeLibrary)) +: appAddSubdirs

      vmVars = VM_Template.vm_cmake_var(
        VM_Template.makeDirVariable(Util.SlangTypeLibrary),
        s"${projectRoot}/${DirectoryUtil.DIR_SLANG_LIBRARIES}/${Util.SlangTypeLibrary}") +: vmVars
    }

    val vmProcessIds = processesToVMs.map((m: AadlProcess) => Util.getCamkesComponentIdentifier(m, symbolTable))


    val declareCamkesArmVMs: ISZ[ST] = processesToVMs.map(m => {
      val id = Util.getCamkesComponentName(m, symbolTable)
      val connectionFilename = s"src/${getCrossVMConnectionsFilename(m, symbolTable)}"
      VM_Template.vm_cmake_DeclareCamkesArmVM(id, ISZ(connectionFilename), libNames)
    })

    auxResourceFiles = auxResourceFiles :+ ResourceUtil.createResource(
      path = s"${getRootVMDir()}/CMakeLists.txt",
      content = VM_Template.vm_cmakelists(vmProcessIds, declareCamkesArmVMs, vmVars),
      overwrite = T)

    auxResourceFiles = auxResourceFiles :+ ResourceUtil.createResource(
      path = s"${getRootVMDir()}/${DIR_VM_EXYNOS5422}/devices.camkes",
      content = VM_Template.vm_exynos5422_devices_camkes(vmProcessIds),
      overwrite = T)

    auxResourceFiles = auxResourceFiles :+ ResourceUtil.createResource(
      path = s"${getRootVMDir()}/${DIR_VM_QEMU_ARM_VIRT}/devices.camkes",
      content = VM_Template.vm_qemu_arm_virt_devices_camkes(vmProcessIds),
      overwrite = T)

    auxResourceFiles = auxResourceFiles :+ ResourceUtil.createResource(
      path = s"${getRootVMDir()}/${DIR_VM_OVERLAY_FILES_INIT_SCRIPT}/cross_vm_module_init",
      content = VM_Template.vm_overlay_scripts__init_scripts__cross_vm_module_init(),
      overwrite = T)

    auxResourceFiles = auxResourceFiles :+ ResourceUtil.createResource(
      path = s"${getRootVMDir()}/${DIR_VM_OVERLAY_FILES_INIT_SCRIPT}/inittab_hvc0",
      content = VM_Template.vm_overlay_script__init_scripts__inittab_hvc0(),
      overwrite = F)

    for (vmProcessID <- vmProcessIds) {

      auxResourceFiles = auxResourceFiles :+ ResourceUtil.createResource(
        path = s"${getRootVMDir()}/${DIR_VM_APPS}/${vmProcessID}/CMakeLists.txt",
        content = VM_Template.vm_cmakelists_app(vmProcessID, libNames, appAddSubdirs),
        overwrite = F)

      auxResourceFiles = auxResourceFiles :+ ResourceUtil.createResource(
        path = s"${getRootVMDir()}/${DIR_VM_APPS}/${vmProcessID}/${Util.genCImplFilename(vmProcessID)}",
        content = VM_Template.vm_app_dummy(vmProcessID, VMGen.getVMAppIncludes()),
        overwrite = F)
    }

    auxResourceFiles = auxResourceFiles ++
      EventDataQueueTemplate.genSbQueueTypeFiles(PacerTemplate.pacerDataportQueueElemType(), PacerTemplate.pacerDataportQueueSize())

    return auxResourceFiles
  }

  def getCrossVMConnectionsFilename(aadlProcess: AadlProcess, symbolTable: SymbolTable): String = {
    val vid = Util.getCamkesComponentIdentifier(aadlProcess, symbolTable)
    return Util.genCImplFilename(s"cross_vm_connections_${vid}")
  }

  def getVMAppIncludes(): ISZ[String] = {
    val ret: ISZ[String] = ISZ(
      Util.getSbTypeHeaderFilenameForIncludes(),
      Util.getSbCounterFilenameForIncludes(),
      PacerTemplate.pacerDataportFilenameForIncludes())

    return ret
  }

  def mergeISZs[T](a: ISZ[T], b: ISZ[T]): ISZ[T] = {
    return (Set.empty[T] ++ a ++ b).elements
  }

  def mergeVMs(assemblies: ISZ[Assembly]): ISZ[Assembly] = {
    // TODO: get rid of macros!

    val vmAssemblies: ISZ[Assembly] = assemblies.filter(_assembly =>
      ops.ISZOps(_assembly.composition.instances).exists(_instance =>
        _instance.component.name == VMGen.VM_COMPONENT_TYPE_NAME))

    if (vmAssemblies.size > 1) {
      // a few sanity checks
      assert(vmAssemblies.size == 2, s"Currently only expecting 2 VMs but given ${vmAssemblies.size}") // TODO: currently only expected a sender/receiver vm
      assert(
        vmAssemblies(0).composition.instances.size == 1 &&
          vmAssemblies(1).composition.instances.size == 1)
      assert(
        vmAssemblies(0).composition.instances(0).component.name == VM_COMPONENT_TYPE_NAME &&
          vmAssemblies(1).composition.instances(0).component.name == VM_COMPONENT_TYPE_NAME)

      val assemblyA = vmAssemblies(0)
      val assemblyB = vmAssemblies(1)

      val componentA = assemblyA.composition.instances(0).component
      val componentB = assemblyB.composition.instances(0).component

      val mergedComponent: CamkesComponent = (componentA, componentB) match {
        case (a: Component, b: Component) =>
          Component(
            control = a.control,
            hardware = a.hardware,

            name = VM_COMPONENT_TYPE_NAME,

            mutexes = mergeISZs(a.mutexes, b.mutexes),
            binarySemaphores = mergeISZs(a.binarySemaphores, b.binarySemaphores),
            semaphores = mergeISZs(a.semaphores, b.semaphores),
            dataports = mergeISZs(a.dataports, b.dataports),
            emits = mergeISZs(a.emits, b.emits),
            uses = mergeISZs(a.uses, b.uses),
            consumes = mergeISZs(a.consumes, b.consumes),
            provides = mergeISZs(a.provides, b.provides),
            includes = mergeISZs(a.includes, b.includes),
            attributes = mergeISZs(a.attributes, b.attributes),
            imports = mergeISZs(a.imports, b.imports),
            preprocessorIncludes = mergeISZs(a.preprocessorIncludes, b.preprocessorIncludes),
            externalEntities = mergeISZs(a.externalEntities, b.externalEntities)
          )
        case _ => halt("Unexpected")
      }


      // two new assemblies, one with the merged components, the other with
      // the first instances with the merged vms, the other with the second
      // entry w/o any compositions

      val assemblyConfigs = vmAssemblies(0).configuration ++ vmAssemblies(1).configuration
      val assemblyConfigMacros = vmAssemblies(0).configurationMacros ++ vmAssemblies(1).configurationMacros

      val newAssemblyA = Assembly(
        configuration = assemblyA.configuration,
        configurationMacros = assemblyA.configurationMacros,
        composition = Composition(
          groups = assemblyA.composition.groups,
          exports = assemblyA.composition.exports,
          instances = ISZ(
            Instance(
              address_space = assemblyA.composition.instances(0).address_space,
              name = assemblyA.composition.instances(0).name,
              component = mergedComponent
            )
          ),
          connections = assemblyA.composition.connections,
          externalEntities = assemblyA.composition.externalEntities
        )
      )

      val newAssemblyB = Assembly(
        configuration = assemblyB.configuration,
        configurationMacros = assemblyB.configurationMacros,
        composition = Composition(
          groups = assemblyB.composition.groups,
          exports = assemblyB.composition.exports,
          instances = ISZ(
            Instance(
              address_space = assemblyB.composition.instances(0).address_space,
              name = assemblyB.composition.instances(0).name,
              component = mergedComponent
            )
          ),
          connections = assemblyB.composition.connections,
          externalEntities = assemblyB.composition.externalEntities
        )
      )

      val nonVMassemblies: ISZ[Assembly] = assemblies.filter(_assembly =>
        !ops.ISZOps(_assembly.composition.instances).exists(_instance =>
          _instance.component.name == VMGen.VM_COMPONENT_TYPE_NAME))

      return nonVMassemblies :+ newAssemblyA :+ newAssemblyB
    } else {
      return assemblies
    }
  }
}

@datatype class MetaPort(aadlPort: AadlPort,
                         sbConnectioncontainer: SBConnectionContainer,
                         vmConnectionInfo: VMConnectionInfo)

@record class VMGen(useDomainScheduling: B,
                    typeMap: HashSMap[String, ir.Component],
                    samplingPorts: HashMap[String, SamplingPortInterface],
                    srcQueues: Map[String, Map[String, QueueObject]],
                    actOptions: ActOptions) {

  val platform: ActPlatform.Type = actOptions.platform
  val performHamrIntegration: B = Util.hamrIntegration(platform)

  val TK1DEVICEFWD: B = F
  val KERNELARMPLATFORM_EXYNOS5410: B = F

  var dataports: ISZ[Dataport] = VM_INIT_DEF.dataports(KERNELARMPLATFORM_EXYNOS5410)
  var emits: ISZ[Emits] = ISZ()
  var uses: ISZ[Uses] = ISZ()
  var consumes: ISZ[Consumes] = ISZ()
  var provides: ISZ[Provides] = ISZ()
  var includes: Set[String] = Set.empty[String]
  var imports: ISZ[String] = ISZ()
  var semaphores: ISZ[Semaphore] = VM_INIT_DEF.semaphores()

  var externalCSources: ISZ[String] = ISZ()
  var externalCIncludeDirs: ISZ[String] = ISZ()

  var preprocessorIncludes: ISZ[String] = ISZ() //ISZ(VMGen.CAMKES_PREPROCESSOR_INCLUDES)
  var externalEntities: ISZ[String] = ISZ(VM_Template.vm_init_macro_expansion().render)

  var auxResources: ISZ[Resource] = ISZ()

  var crossConnGCMethods: ISZ[ST] = ISZ()
  var crossConnConnections: ISZ[ST] = ISZ()

  val useCaseConnectors: B = Connections.useCaseEventDataPortConnector(actOptions.experimentalOptions)

  def genProcess(aadlProcess: AadlProcess, symbolTable: SymbolTable, sbConnections: ISZ[SBConnectionContainer]): (Component, ISZ[Resource]) = {
    val boundProcessor: AadlVirtualProcessor = aadlProcess.getBoundProcessor(symbolTable).get.asInstanceOf[AadlVirtualProcessor]

    provides = VM_INIT_DEF.provides(aadlProcess, symbolTable)
    emits = VM_INIT_DEF.emits(aadlProcess, symbolTable)
    consumes = VM_INIT_DEF.consumes(aadlProcess, symbolTable)
    uses = VM_INIT_DEF.uses(TK1DEVICEFWD, KERNELARMPLATFORM_EXYNOS5410, aadlProcess, symbolTable)

    assert(aadlProcess.toVirtualMachine(symbolTable), s"Process is not in a vm bound process ${aadlProcess.identifier}")

    // TODO: not currently expecting feature access
    //assert(aadlThread.getFeatureAccesses().isEmpty, s"Not currently handling feature accesses in vm bound thread ${aadlThread.identifier}")

    // TODO: currently only supporting SeL4_Only, and SeL4; SeL4_TB will not be supported
    assert(platform == ActPlatform.SeL4_Only || platform == ActPlatform.SeL4, s"Platform ${platform} is not supported for vm bound processes")

    // TODO: will we ever process models where a sporadic thread is isolated in the vm
    // or we're not using the pacer
    assert(PeriodicUtil.requiresPacerArtifacts(boundProcessor, symbolTable, useDomainScheduling),
      s"Expecting a periodic process that will be triggered via a Pacer: ${aadlProcess.identifier}"
    )

    var connectedPorts: ISZ[MetaPort] = ISZ()
    for(conn <- sbConnections) {
      if(conn.srcComponent == aadlProcess) {
        connectedPorts = connectedPorts :+ MetaPort(conn.srcPort, conn, conn.srcVMInfo.get)
      } else if (conn.dstComponent == aadlProcess) {
        connectedPorts = connectedPorts :+ MetaPort(conn.dstPort, conn, conn.dstVMInfo.get)
      }
    }

    for (port <- connectedPorts) {

      port.aadlPort match {
        case a: AadlEventDataPort if !useCaseConnectors => {
          actOptions.platform match {
            case ActPlatform.SeL4_Only =>
              handleEventDataPort(port, aadlProcess, symbolTable)
            case ActPlatform.SeL4 =>
              handleEventDataPort(port, aadlProcess, symbolTable)

            case notyet =>
              // TODO
              halt(s"Platform ${notyet} is not currently handled for vm isolated threads: ${aadlProcess.identifier}.${port.aadlPort.identifier}")
          }
        }
        case a: AadlEventDataPort if useCaseConnectors => {
          actOptions.platform match {
            case ActPlatform.SeL4_Only =>
              handleEventDataPort_CASE_Connectors(port, aadlProcess, symbolTable)
            case ActPlatform.SeL4 =>
              handleEventDataPort_CASE_Connectors(port, aadlProcess, symbolTable)

            case notyet =>
              // TODO
              halt(s"Platform ${notyet} is not currently handled for vm isolated threads: ${aadlProcess.identifier}.${port.aadlPort.identifier}")
          }
        }
        case a: AadlDataPort =>
          handleDataPort(port, aadlProcess, symbolTable)

        case _ =>
          // TODO
          halt(s"Currently expecting vm isolated threads to have only event data ports: ${aadlProcess.identifier}.${port.aadlPort.identifier}")
      }
    }

    val boundVProcessor = aadlProcess.getBoundProcessor(symbolTable).get.asInstanceOf[AadlVirtualProcessor]
    boundVProcessor.dispatchProtocol match {
      case Dispatch_Protocol.Periodic =>
        val (periodicDispatcherComponentContributions, glueCodeContributions) =
          Dispatcher.handlePeriodicComponent(useDomainScheduling, symbolTable, actOptions, aadlProcess)

        consumes = consumes ++ periodicDispatcherComponentContributions.shell.consumes

        dataports = dataports ++ periodicDispatcherComponentContributions.shell.dataports

        // extern method name for pacer dataport queue
        crossConnGCMethods = crossConnGCMethods :+
          VM_Template.vm_cross_conn_extern_dataport_method(PacerTemplate.pacerVM_ClientPeriodDataportIdentifier())

        val notificationPrefix: String =
          if (useCaseConnectors) PacerTemplate.pacerVM_ClientPeriodDataportIdentifier()
          else PacerTemplate.pacerVM_ClientPeriodNotificationIdentifier()

        // extern method name for pacer notification
        crossConnGCMethods = crossConnGCMethods :+
          VM_Template.vm_cross_conn_extern_notification_methods(notificationPrefix)

        // connection creation for pacer dataport/notification
        crossConnConnections = crossConnConnections :+
          VM_Template.vm_cross_conn_Connections(
            methodNamePrefix = PacerTemplate.pacerVM_ClientPeriodDataportIdentifier(),
            emitMethodNamePrefix = None(),
            notificationNamePrefix = Some(notificationPrefix),
            counter = crossConnConnections.size)

      case x =>
        halt(s"Not currently supporting VMs containing ${x} dispatch protocol")
    }

    val vmCrossConns: ST = VM_Template.vm_cross_vm_connections(crossConnGCMethods, crossConnConnections)
    auxResources = auxResources :+ ResourceUtil.createResource(
      path = s"${VMGen.getRootVMDir()}/${VMGen.DIR_VM_SRC}/${VMGen.getCrossVMConnectionsFilename(aadlProcess, symbolTable)}",
      content = vmCrossConns,
      overwrite = T)

    includes = includes + PacerTemplate.pacerDataportFilenameForIncludes()

    val c = Util.createCAmkESComponent(
      aadlThread = None(),
      componentCategory = CAmkESComponentCategory.VM_Refinement,

      control = T,
      hardware = F,
      name = Util.getCamkesComponentName(aadlProcess, symbolTable),
      mutexes = ISZ(),
      binarySemaphores = ISZ(),
      semaphores = semaphores,
      dataports = dataports,
      emits = emits,
      uses = uses,
      consumes = consumes,
      provides = provides,
      includes = includes.elements,
      attributes = ISZ(),

      preprocessorIncludes = preprocessorIncludes,
      imports = imports,

      externalEntities = externalEntities
    )

    auxResources = auxResources :+ ResourceUtil.createExeResource(
      path = PathUtil.CAMKES_ARM_VM_SCRIPT_PATH,
      content = VM_Template.setup_camkes_vm_Script(),
      overwrite = T)

    return (c, auxResources)


  }

  def handleDataPort(port: MetaPort, aadlProcess: AadlProcess, symbolTable: SymbolTable): Unit = {
    val aadlPort = port.aadlPort
    val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(aadlPort.feature.classifier.get)).get
    val sel4TypeName: String = Util.getSel4TypeName(aadlPortType, performHamrIntegration)

    val spi: SamplingPortInterface = samplingPorts.get(sel4TypeName).get

    includes = includes + s"<${spi.headerFilename}>"

    val camkesDataPortId = Util.brand(aadlPort.identifier)

    dataports = dataports :+ Util.createDataport_VMRefinement(
      aadlComponent = aadlProcess,
      metaPort = port,
      symbolTable = symbolTable,

      name = camkesDataPortId,
      typ = spi.structName,
      optional = F)

    crossConnGCMethods = crossConnGCMethods :+
      VM_Template.vm_cross_conn_extern_dataport_method(camkesDataPortId)

    crossConnConnections = crossConnConnections :+
      VM_Template.vm_cross_conn_Connections(
        methodNamePrefix = camkesDataPortId,
        emitMethodNamePrefix = None(),
        notificationNamePrefix = None(),
        counter = crossConnConnections.size)
  }

  def handleEventDataPort(metaPort: MetaPort, aadlProcess: AadlProcess, symbolTable: SymbolTable): Unit = {
    val aadlPort = metaPort.aadlPort
    assert(!useCaseConnectors)

    val fid = aadlPort.identifier

    val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(aadlPort.feature.classifier.get)).get
    val sel4TypeName: String = Util.getSel4TypeName(aadlPortType, performHamrIntegration)

    val queueSize = PropertyUtil.getQueueSize(aadlPort.feature, Util.DEFAULT_QUEUE_SIZE)
    val queueType = Util.getEventDataSBQueueTypeName(sel4TypeName, queueSize)

    includes = includes + s"<${Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, queueSize)}>"

    aadlPort.direction match {
      case ir.Direction.In => {

        /*
        val connections = symbolTable.inConnections.get(aadlPort.path).get
        // TODO: fan ins ????
        if (connections.size != 1) {
          // this would probably be bad if sender is fan-outing to a mix of
          // native and VM components.  Perhaps okay if broadcasting to
          // all native though?
          halt(s"Not currently supporting fan-ins for vm isolated threads ${aadlProcess.identifier}.${aadlPort.identifier}")
        }

         */

        val camkesDataPortId = Util.getEventDataSBQueueDestFeatureName(fid)

        val camkesEventPortId: String = {
          val name = Util.genSeL4NotificationName(aadlPort.identifier, T)
          consumes = consumes :+ Util.createConsumes_VMRefinement(
            aadlComponent = aadlProcess,
            metaPort = metaPort,
            symbolTable = symbolTable,

            name = name,
            typ = Util.EVENT_NOTIFICATION_TYPE,
            optional = F)
          name
        }

        dataports = dataports :+ Util.createDataport_VMRefinement(
          aadlComponent = aadlProcess,
          metaPort = metaPort,
          symbolTable = symbolTable,

          name = camkesDataPortId,
          typ = queueType,
          optional = F)

        crossConnGCMethods = crossConnGCMethods :+
          VM_Template.vm_cross_conn_extern_dataport_method(camkesDataPortId)

        crossConnGCMethods = crossConnGCMethods :+
          VM_Template.vm_cross_conn_extern_notification_methods(camkesEventPortId)

        crossConnConnections = crossConnConnections :+
          VM_Template.vm_cross_conn_Connections(
            methodNamePrefix = camkesDataPortId,
            emitMethodNamePrefix = None(),
            notificationNamePrefix = Some(camkesEventPortId),
            counter = crossConnConnections.size)
      }
      case ir.Direction.Out => {

        /*
        val connections = symbolTable.outConnections.get(aadlPort.path).get
        // TODO: what to do about fan outs?
        if (connections.size != 1) {
          // what if receivers have different queue sizes?  Would need to
          // broadcast.  Need examples
          halt(s"Not currently supporting fan-outs for VM isolated threads ${aadlProcess.identifier}.${aadlPort.identifier}")
        }
        */

        val camkesEventPortId = Util.genSeL4NotificationQueueName(aadlPort.identifier, queueSize)

        emits = emits :+ Util.createEmits_VMRefinement(
          aadlComponent = aadlProcess,
          metaPort = metaPort,
          symbolTable = symbolTable,

          name = camkesEventPortId,
          typ = Util.EVENT_NOTIFICATION_TYPE)

        val camkesDataPortId = Util.getEventDataSBQueueSrcFeatureName(fid, queueSize)

        dataports = dataports :+ Util.createDataport_VMRefinement(
          aadlComponent = aadlProcess,
          metaPort = metaPort,
          symbolTable = symbolTable,

          name = camkesDataPortId,
          typ = queueType,
          optional = F)

        crossConnGCMethods = crossConnGCMethods :+
          VM_Template.vm_cross_conn_extern_dataport_method(camkesDataPortId)

        crossConnGCMethods = crossConnGCMethods :+
          VM_Template.vm_cross_conn_extern_emit_method(camkesEventPortId)

        crossConnConnections = crossConnConnections :+
          VM_Template.vm_cross_conn_Connections(
            methodNamePrefix = camkesDataPortId,
            emitMethodNamePrefix = Some(camkesEventPortId),
            notificationNamePrefix = None(),
            counter = crossConnConnections.size)
      }
      case x => halt(s"Not expecting direction ${x}: ${aadlProcess.identifier}.${aadlPort.identifier}")
    }
  }

  def handleEventDataPort_CASE_Connectors(metaPort: MetaPort, aadlProcess: AadlProcess, symbolTable: SymbolTable): Unit = {
    val aadlPort = metaPort.aadlPort
    assert(useCaseConnectors)

    val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(aadlPort.feature.classifier.get)).get
    val sel4TypeName: String = Util.getSel4TypeName(aadlPortType, performHamrIntegration)

    val queueSize = PropertyUtil.getQueueSize(aadlPort.feature, Util.DEFAULT_QUEUE_SIZE)
    val queueType = Util.getEventDataSBQueueTypeName(sel4TypeName, queueSize)

    includes = includes + s"<${Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, queueSize)}>"

    aadlPort.direction match {
      case ir.Direction.In => {

        val connections = symbolTable.inConnections.get(aadlPort.path).get
        // TODO: fan ins ????
        if (connections.size != 1) {
          // this would probably be bad if sender is fan-outing to a mix of
          // native and VM components.  Perhaps okay if broadcasting to
          // all native though?
          halt(s"Not currently supporting fan-ins for vm isolated threads ${aadlProcess.identifier}.${aadlPort.identifier}")
        }

        val dataportName = Util.getEventDataSBQueueDestFeatureName(aadlPort.identifier)

        val notificationName: String = dataportName

        dataports = dataports :+ Util.createDataport_VMRefinement(
          aadlComponent = aadlProcess,
          metaPort = metaPort,
          symbolTable = symbolTable,

          name = dataportName,
          typ = queueType,
          optional = F)

        crossConnGCMethods = crossConnGCMethods :+
          VM_Template.vm_cross_conn_extern_dataport_method(dataportName)

        crossConnGCMethods = crossConnGCMethods :+
          VM_Template.vm_cross_conn_extern_notification_methods(notificationName)

        crossConnConnections = crossConnConnections :+
          VM_Template.vm_cross_conn_Connections(
            methodNamePrefix = dataportName,
            emitMethodNamePrefix = None(),
            notificationNamePrefix = Some(notificationName),
            counter = crossConnConnections.size)
      }
      case ir.Direction.Out => {

        val connections = symbolTable.outConnections.get(aadlPort.path).get
        // TODO: what to do about fan outs?
        if (connections.size != 1 && !useCaseConnectors) {
          // what if receivers have different queue sizes?  Would need to
          // broadcast.  Need examples
          halt(s"Not currently supporting fan-outs for VM isolated threads ${aadlProcess.identifier}.${aadlPort.identifier}")
        }

        val dataPortName = Util.getEventDataSBQueueSrcFeatureName(aadlPort.identifier, queueSize)

        val emitsName = dataPortName //Util.genSeL4NotificationQueueName(f, queueSize)

        dataports = dataports :+ Util.createDataport_VMRefinement(
          aadlComponent = aadlProcess,
          metaPort = metaPort,
          symbolTable = symbolTable,

          name = dataPortName,
          typ = queueType,
          optional = F)

        crossConnGCMethods = crossConnGCMethods :+
          VM_Template.vm_cross_conn_extern_dataport_method(dataPortName)

        crossConnGCMethods = crossConnGCMethods :+
          VM_Template.vm_cross_conn_extern_emit_method(emitsName)

        crossConnConnections = crossConnConnections :+
          VM_Template.vm_cross_conn_Connections(
            methodNamePrefix = dataPortName,
            emitMethodNamePrefix = Some(emitsName),
            notificationNamePrefix = None(),
            counter = crossConnConnections.size)
      }
      case x => halt(s"Not expecting direction ${x}: ${aadlProcess.identifier}.${aadlPort.identifier}")
    }
  }
}
