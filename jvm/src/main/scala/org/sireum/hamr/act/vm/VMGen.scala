// #Sireum
package org.sireum.hamr.act.vm

import org.sireum._
import org.sireum.hamr.act.ast._
import org.sireum.hamr.act.connections.Connections
import org.sireum.hamr.act.periodic.{Dispatcher, PacerTemplate, PeriodicUtil}
import org.sireum.hamr.act.templates.{CMakeTemplate, EventDataQueueTemplate}
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols.{AadlProcess, AadlThread, Dispatch_Protocol, SymbolTable}
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.codegen.common.{CommonUtil, DirectoryUtil}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.FeatureEnd

object VMGen {

  val VM_COMPONENT_TYPE_NAME: String = "VM"
  val VM_ID_PREFIX: String = "vm" // has to be 'vm' as per data61 macros in https://github.com/SEL4PROJ/camkes-arm-vm/blob/6c77a2734ea4c77035f2c3cca5ca7fa72f1f2890/components/VM/configurations/vm.h#L93

  val DIR_VM: String =                           VM_COMPONENT_TYPE_NAME
  val DIR_VM_APPS: String =                      "apps"
  val DIR_VM_EXYNOS5422: String =                "exynos5422"
  val DIR_VM_OVERLAY_FILES: String =             "overlay_files"
  val DIR_VM_OVERLAY_FILES_INIT_SCRIPT: String = s"${DIR_VM_OVERLAY_FILES}/init_scripts"
  val DIR_VM_QEMU_ARM_VIRT: String =             "qemu-arm-virt"
  val DIR_VM_SRC: String =                       "src"

  val CAMKES_PREPROCESSOR_INCLUDES: String = "<configurations/vm.h>" // see https://github.com/SEL4PROJ/camkes-arm-vm/blob/6c77a2734ea4c77035f2c3cca5ca7fa72f1f2890/components/VM/configurations/vm.h

  def getRootVMDir() : String= {
    return s"${Util.DIR_COMPONENTS}/${DIR_VM}"
  }

  def getAuxResources(threadsToVMs: ISZ[AadlThread],
                      platform: ActPlatform.Type,
                      symbolTable: SymbolTable): ISZ[Resource] = {
    assert(threadsToVMs.nonEmpty, "Expecting 1 or more threads going to VMs")
    var auxResourceFiles: ISZ[Resource] = ISZ()

    val projectRoot = s"$${CMAKE_CURRENT_SOURCE_DIR}/../.."

    var libNames: ISZ[String] = ISZ(Util.SBTypeLibrary)

    var vmVars: ISZ[ST] = ISZ(
      VM_Template.vm_cmake_var(
          VM_Template.makeDirVariable(Util.SBTypeLibrary),s"${projectRoot}/${Util.getTypeRootPath()}"))

    var appAddSubdirs: ISZ[ST] = ISZ(
      CMakeTemplate.cmake_add_subdirectory_binned(
        VM_Template.cmakeReferenceVar(VM_Template.makeDirVariable(Util.SBTypeLibrary)),
        Some(Util.SBTypeLibrary)))

    if(platform == ActPlatform.SeL4) {
      libNames = libNames :+ Util.SlangTypeLibrary

      appAddSubdirs = CMakeTemplate.cmake_add_subdirectory_binned(
        VM_Template.cmakeReferenceVar(VM_Template.makeDirVariable(Util.SlangTypeLibrary)),
        Some(Util.SlangTypeLibrary)) +: appAddSubdirs

      vmVars = VM_Template.vm_cmake_var(
          VM_Template.makeDirVariable(Util.SlangTypeLibrary),
          s"${projectRoot}/${DirectoryUtil.DIR_SLANG_LIBRARIES}/${Util.SlangTypeLibrary}") +: vmVars
    }

    val vmThreadIds = threadsToVMs.map((m: AadlThread) => Util.getCamkesComponentIdentifier(m, symbolTable))

    val declareCamkesArmVMs: ISZ[ST] = threadsToVMs.map(m => {
      val id = Util.getCamkesComponentName(m, symbolTable)
      val connectionFilename = s"src/${getCrossVMConnectionsFilename(m, symbolTable)}"
      VM_Template.vm_cmake_DeclareCamkesArmVM(id, ISZ(connectionFilename), libNames)
    })

    auxResourceFiles = auxResourceFiles :+ ResourceUtil.createResource(
      path = s"${getRootVMDir()}/CMakeLists.txt",
      content = VM_Template.vm_cmakelists(vmThreadIds, declareCamkesArmVMs, vmVars),
      overwrite = T)

    auxResourceFiles = auxResourceFiles :+ ResourceUtil.createResource(
      path = s"${getRootVMDir()}/${DIR_VM_EXYNOS5422}/devices.camkes",
      content = VM_Template.vm_exynos5422_devices_camkes(vmThreadIds),
      overwrite = T)

    auxResourceFiles = auxResourceFiles :+ ResourceUtil.createResource(
      path = s"${getRootVMDir()}/${DIR_VM_QEMU_ARM_VIRT}/devices.camkes",
      content = VM_Template.vm_qemu_arm_virt_devices_camkes(vmThreadIds),
      overwrite = T)

    auxResourceFiles = auxResourceFiles :+ ResourceUtil.createResource(
      path = s"${getRootVMDir()}/${DIR_VM_OVERLAY_FILES_INIT_SCRIPT}/cross_vm_module_init",
      content = VM_Template.vm_overlay_scripts__init_scripts__cross_vm_module_init(),
      overwrite = T)

    auxResourceFiles = auxResourceFiles :+ ResourceUtil.createResource(
      path = s"${getRootVMDir()}/${DIR_VM_OVERLAY_FILES_INIT_SCRIPT}/inittab_hvc0",
      content = VM_Template.vm_overlay_script__init_scripts__inittab_hvc0(),
      overwrite = F)

    for(vmProcessID <- vmThreadIds) {

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

  def getCrossVMConnectionsFilename(aadlThread: AadlThread, symbolTable: SymbolTable): String = {
    val vid = Util.getCamkesComponentIdentifier(aadlThread, symbolTable)
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

    if(vmAssemblies.size > 1) {
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

      val assemblyConfigs =  vmAssemblies(0).configuration ++ vmAssemblies(1).configuration
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

@record class VMGen(useDomainScheduling: B,
                    symbolTable: SymbolTable,
                    typeMap: HashSMap[String, ir.Component],
                    samplingPorts: HashMap[String, SamplingPortInterface],
                    srcQueues: Map[String, Map[String, QueueObject]],
                    actOptions: ActOptions) {

  val platform: ActPlatform.Type = actOptions.platform
  val performHamrIntegration: B = Util.hamrIntegration(platform)

  val TK1DEVICEFWD: B = F
  val KERNELARMPLATFORM_EXYNOS5410: B = F

  var dataports: ISZ[Dataport] = VM_INIT_DEF.dataports(KERNELARMPLATFORM_EXYNOS5410)
  var emits: ISZ[Emits]= VM_INIT_DEF.emits()
  var uses: ISZ[Uses] = VM_INIT_DEF.uses(TK1DEVICEFWD, KERNELARMPLATFORM_EXYNOS5410)
  var consumes: ISZ[Consumes] = VM_INIT_DEF.consumes()
  var provides: ISZ[Provides] = VM_INIT_DEF.provides()
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

  def genThread(aadlThread: AadlThread): (Component, ISZ[Resource]) = {

    assert(aadlThread.toVirtualMachine(symbolTable), s"Thread is not in a vm bound process ${aadlThread.identifier}")

    val parent: AadlProcess = aadlThread.getParent(symbolTable)

    // TODO: not currently expecting feature access
    assert(aadlThread.getFeatureAccesses().isEmpty, s"Not currently handling feature accesses in vm bound thread ${aadlThread.identifier}")

    // TODO: currently expecting exactly 1 thread per vm isolated process
    assert(parent.subComponents.filter(c => CommonUtil.isThread(c.component)).size == 1, s"Expecting exactly one thread per vm bound process ${parent.identifier}")

    // TODO: currently only supporting SeL4_Only, and SeL4; SeL4_TB will not be supported
    assert(platform == ActPlatform.SeL4_Only || platform == ActPlatform.SeL4, s"Platform ${platform} is not supported for vm bound processes")

    // TODO: will we ever process models where a sporadic thread is isolated in the vm
    // or we're not using the pacer
    assert(PeriodicUtil.requiresPacerArtifacts(aadlThread, symbolTable, useDomainScheduling),
      s"Expecting a periodic thread that will be triggered via a Pacer: ${aadlThread.identifier}"
    )

    includes = includes + Util.getSbTypeHeaderFilenameForIncludes()

    for(featureEnd <- aadlThread.getFeatureEnds() if(symbolTable.isConnected(featureEnd))) {
      val fid = CommonUtil.getLastName(featureEnd.identifier)

      featureEnd.category match {
        case ir.FeatureCategory.EventDataPort if !useCaseConnectors => {
          actOptions.platform match {
            case ActPlatform.SeL4_Only =>
              handleEventDataPort(featureEnd, aadlThread)
            case ActPlatform.SeL4 =>
              handleEventDataPort(featureEnd, aadlThread)

            case notyet =>
              // TODO
              halt(s"Platform ${notyet} is not currently handled for vm isolated threads: ${aadlThread.identifier}.${fid}")
          }
        }
        case ir.FeatureCategory.EventDataPort if useCaseConnectors => {
          actOptions.platform match {
            case ActPlatform.SeL4_Only =>
              handleEventDataPort_CASE_Connectors(featureEnd, aadlThread)
            case ActPlatform.SeL4 =>
              handleEventDataPort_CASE_Connectors(featureEnd, aadlThread)

            case notyet =>
              // TODO
              halt(s"Platform ${notyet} is not currently handled for vm isolated threads: ${aadlThread.identifier}.${fid}")
          }
        }
        case ir.FeatureCategory.DataPort =>
          handleDataPort(featureEnd, aadlThread)

        case _ =>
          // TODO
          halt(s"Currently expecting vm isolated threads to have only event data ports: ${aadlThread.identifier}.${fid}")
      }
    }

    aadlThread.dispatchProtocol match {
      case Dispatch_Protocol.Periodic =>
        val (componentContributions, glueCodeContributions) =
          Dispatcher.handlePeriodicComponent(useDomainScheduling, symbolTable, actOptions, aadlThread)

        consumes = consumes ++ componentContributions.shell.consumes

        dataports = dataports ++ componentContributions.shell.dataports

        // extern method name for pacer dataport queue
        crossConnGCMethods = crossConnGCMethods :+
          VM_Template.vm_cross_conn_extern_dataport_method(PacerTemplate.pacerVM_ClientPeriodDataportIdentifier())

        val notificationPrefix: String =
          if(useCaseConnectors) PacerTemplate.pacerVM_ClientPeriodDataportIdentifier()
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
      path = s"${VMGen.getRootVMDir()}/${VMGen.DIR_VM_SRC}/${VMGen.getCrossVMConnectionsFilename(aadlThread, symbolTable)}",
      content = vmCrossConns,
      overwrite = T)

    includes = includes + PacerTemplate.pacerDataportFilenameForIncludes()

    val c = Component(
      control = T,
      hardware = F,
      name = Util.getCamkesComponentName(aadlThread, symbolTable),
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

  def handleDataPort(f: FeatureEnd, parent: AadlThread): Unit = {

    val fid = CommonUtil.getLastName(f.identifier)

    val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(f.classifier.get)).get
    val sel4TypeName: String = Util.getSel4TypeName(aadlPortType, performHamrIntegration)

    val spi: SamplingPortInterface = samplingPorts.get(sel4TypeName).get

    includes = includes + s"<${spi.headerFilename}>"

    val dataPortName = Util.brand(fid)

    dataports = dataports :+ Dataport(
      name = dataPortName,
      typ = spi.structName,
      optional = F)

    crossConnGCMethods = crossConnGCMethods :+
      VM_Template.vm_cross_conn_extern_dataport_method(dataPortName)

    crossConnConnections = crossConnConnections :+
      VM_Template.vm_cross_conn_Connections(
        methodNamePrefix = dataPortName,
        emitMethodNamePrefix = None(),
        notificationNamePrefix = None(),
        counter = crossConnConnections.size)
  }

  def handleEventDataPort(f: FeatureEnd, parent: AadlThread): Unit = {
    assert(!useCaseConnectors)

    val fid = CommonUtil.getLastName(f.identifier)

    val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(f.classifier.get)).get
    val sel4TypeName: String = Util.getSel4TypeName(aadlPortType, performHamrIntegration)

    val queueSize = PropertyUtil.getQueueSize(f, Util.DEFAULT_QUEUE_SIZE)
    val queueType = Util.getEventDataSBQueueTypeName(sel4TypeName, queueSize)

    includes = includes + s"<${Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, queueSize)}>"

    f.direction match {
      case ir.Direction.In => {

        val connections = symbolTable.inConnections.get(CommonUtil.getName(f.identifier)).get
        // TODO: fan ins ????
        if (connections.size != 1) {
          // this would probably be bad if sender is fan-outing to a mix of
          // native and VM components.  Perhaps okay if broadcasting to
          // all native though?
          halt(s"Not currently supporting fan-ins for vm isolated threads ${parent.identifier}.${CommonUtil.getLastName(f.identifier)}")
        }

        val dataportName = Util.getEventDataSBQueueDestFeatureName(fid)

        val notificationName: String = {
          val name = Util.genSeL4NotificationName(f, T)
          consumes = consumes :+ Consumes(
            name = name,
            typ = Util.EVENT_NOTIFICATION_TYPE,
            optional = F)
          name
        }

        dataports = dataports :+ Dataport(
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

        val connections = symbolTable.outConnections.get(CommonUtil.getName(f.identifier)).get
        // TODO: what to do about fan outs?
        if (connections.size != 1) {
          // what if receivers have different queue sizes?  Would need to
          // broadcast.  Need examples
          halt(s"Not currently supporting fan-outs for VM isolated threads ${parent.identifier}.${CommonUtil.getLastName(f.identifier)}")
        }

        val emitsName = Util.genSeL4NotificationQueueName(f, queueSize)

        emits = emits :+ Emits(
          name = emitsName,
          typ = Util.EVENT_NOTIFICATION_TYPE)

        val dataPortName = Util.getEventDataSBQueueSrcFeatureName(fid, queueSize)

        dataports = dataports :+ Dataport(
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
      case x => halt(s"Not expecting direction ${x}: ${parent.identifier}.{fid}")
    }
  }

  def handleEventDataPort_CASE_Connectors(f: FeatureEnd, parent: AadlThread): Unit = {
    assert(useCaseConnectors)

    val fid = CommonUtil.getLastName(f.identifier)

    val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(f.classifier.get)).get
    val sel4TypeName: String = Util.getSel4TypeName(aadlPortType, performHamrIntegration)

    val queueSize = PropertyUtil.getQueueSize(f, Util.DEFAULT_QUEUE_SIZE)
    val queueType = Util.getEventDataSBQueueTypeName(sel4TypeName, queueSize)

    includes = includes + s"<${Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, queueSize)}>"

    f.direction match {
      case ir.Direction.In => {

        val connections = symbolTable.inConnections.get(CommonUtil.getName(f.identifier)).get
        // TODO: fan ins ????
        if (connections.size != 1) {
          // this would probably be bad if sender is fan-outing to a mix of
          // native and VM components.  Perhaps okay if broadcasting to
          // all native though?
          halt(s"Not currently supporting fan-ins for vm isolated threads ${parent.identifier}.${CommonUtil.getLastName(f.identifier)}")
        }

        val dataportName = Util.getEventDataSBQueueDestFeatureName(fid)

        val notificationName: String = dataportName

        dataports = dataports :+ Dataport(
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

        val connections = symbolTable.outConnections.get(CommonUtil.getName(f.identifier)).get
        // TODO: what to do about fan outs?
        if (connections.size != 1 && !useCaseConnectors) {
          // what if receivers have different queue sizes?  Would need to
          // broadcast.  Need examples
          halt(s"Not currently supporting fan-outs for VM isolated threads ${parent.identifier}.${CommonUtil.getLastName(f.identifier)}")
        }

        val dataPortName = Util.getEventDataSBQueueSrcFeatureName(fid, queueSize)

        val emitsName = dataPortName //Util.genSeL4NotificationQueueName(f, queueSize)

        dataports = dataports :+ Dataport(
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
      case x => halt(s"Not expecting direction ${x}: ${parent.identifier}.{fid}")
    }
  }
}
