// #Sireum
package org.sireum.hamr.act.vm

import org.sireum._
import org.sireum.hamr.act.{ActOptions, ActPlatform, ActPrettyPrint, QueueObject, Resource, SamplingPortInterface, Util}
import org.sireum.hamr.ir
import org.sireum.hamr.act.ast._
import org.sireum.hamr.act.periodic.{Dispatcher, PacerTemplate, PeriodicUtil}
import org.sireum.hamr.act.templates.EventDataQueueTemplate
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols.{AadlProcess, AadlThread, Dispatch_Protocol, SymbolTable}
import org.sireum.hamr.ir.FeatureEnd
import org.sireum.message.Reporter

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

  val VM_INIT_DEF: String = "VM_INIT_DEF()" // see https://github.com/SEL4PROJ/camkes-arm-vm/blob/6c77a2734ea4c77035f2c3cca5ca7fa72f1f2890/components/VM/configurations/vm.h#L52

  def getRootVMDir() : String= {
    return s"${Util.DIR_COMPONENTS}/${DIR_VM}"
  }

  def getAuxResources(vmProcessIDs: ISZ[String]): ISZ[Resource] = {
    assert(vmProcessIDs.nonEmpty, "Expecting 1 or more ids of processes going to VMs")
    var auxResourceFiles: ISZ[Resource] = ISZ()

    auxResourceFiles = auxResourceFiles :+ Resource(
      path = s"${getRootVMDir()}/CMakeLists.txt",
      content = VM_Template.vm_cmakelists(vmProcessIDs),
      overwrite = T, makeExecutable = F)

    auxResourceFiles = auxResourceFiles :+ Resource(
      path = s"${getRootVMDir()}/${DIR_VM_EXYNOS5422}/devices.camkes",
      content = VM_Template.vm_exynos5422_devices_camkes(vmProcessIDs),
      overwrite = T, makeExecutable = F)

    auxResourceFiles = auxResourceFiles :+ Resource(
      path = s"${getRootVMDir()}/${DIR_VM_QEMU_ARM_VIRT}/devices.camkes",
      content = VM_Template.vm_qemu_arm_virt_devices_camkes(vmProcessIDs),
      overwrite = T, makeExecutable = F)

    auxResourceFiles = auxResourceFiles :+ Resource(
      path = s"${getRootVMDir()}/${DIR_VM_OVERLAY_FILES_INIT_SCRIPT}/cross_vm_module_init",
      content = VM_Template.vm_overlay_scripts__init_scripts__cross_vm_module_init(),
      overwrite = T, makeExecutable = F)

    auxResourceFiles = auxResourceFiles :+ Resource(
      path = s"${getRootVMDir()}/${DIR_VM_OVERLAY_FILES_INIT_SCRIPT}/inittab_hvc0",
      content = VM_Template.vm_overlay_script__init_scripts__inittab_hvc0(),
      overwrite = T, makeExecutable = F)

    for(vmProcessID <- vmProcessIDs) {
      auxResourceFiles = auxResourceFiles :+ Resource(
        path = s"${getRootVMDir()}/${DIR_VM_APPS}/${vmProcessID}/CMakeLists.txt",
        content = VM_Template.vm_cmakelists_app(vmProcessID),
        overwrite = T, makeExecutable = F)

      auxResourceFiles = auxResourceFiles :+ Resource(
        path = s"${getRootVMDir()}/${DIR_VM_APPS}/${vmProcessID}/${Util.genCImplFilename(vmProcessID)}",
        content = VM_Template.vm_app_dummy(vmProcessID, VMGen.getVMAppIncludes()),
        overwrite = F, makeExecutable = F)
    }

    auxResourceFiles = auxResourceFiles ++
      EventDataQueueTemplate.genSbQueueTypeFiles(PacerTemplate.pacerDataportQueueElemType(), PacerTemplate.pacerDataportQueueSize())

    return auxResourceFiles
  }

  def virtualMachineIdentifier(aadlProcess: AadlProcess): String = {
    return s"${VM_ID_PREFIX}${aadlProcess.identifier}"
  }

  def getCrossVMConnectionsFilename(aadlProcess: AadlProcess): String = {
    val vid = virtualMachineIdentifier(aadlProcess)
    return Util.genCImplFilename(s"cross_vm_connections_${vid}")
  }

  def getVMAppIncludes(): ISZ[String] = {
    val ret: ISZ[String] = ISZ(
      Util.getSbTypeHeaderFilenameForIncludes(),
      Util.getSbCounterFilenameForIncludes(),
      PacerTemplate.pacerDataportFilenameForIncludes())

    return ret
  }

  def getEventDataportName(srcFeature: ir.Feature, dstFeature: ir.Feature, queueSize: Z): String = {
    val s = CommonUtil.getLastName(srcFeature.identifier)
    val d = CommonUtil.getLastName(dstFeature.identifier)
    return Util.getEventDataSBQueueSrcFeatureName(s"${s}_${d}", queueSize)
  }

  def getDataportName(srcFeature: ir.Feature, dstFeature: ir.Feature): String = {
    val s = CommonUtil.getLastName(srcFeature.identifier)
    val d = CommonUtil.getLastName(dstFeature.identifier)
    return Util.brand(s"${s}_${d}")
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

      val mergedComponent: Component = Component(
        control = componentA.control,
        hardware = componentA.hardware,

        name = VM_COMPONENT_TYPE_NAME,

        mutexes = mergeISZs(componentA.mutexes, componentB.mutexes),
        binarySemaphores = mergeISZs(componentA.binarySemaphores, componentB.binarySemaphores),
        semaphores = mergeISZs(componentA.semaphores, componentB.semaphores),
        dataports = mergeISZs(componentA.dataports, componentB.dataports),
        emits = mergeISZs(componentA.emits, componentB.emits),
        uses = mergeISZs(componentA.uses, componentB.uses),
        consumes = mergeISZs(componentA.consumes, componentB.consumes),
        provides = mergeISZs(componentA.provides, componentB.provides),
        includes = mergeISZs(componentA.includes, componentB.includes),
        attributes = mergeISZs(componentA.attributes, componentB.attributes),
        imports = mergeISZs(componentA.imports, componentB.imports),
        preprocessorIncludes = mergeISZs(componentA.preprocessorIncludes, componentB.preprocessorIncludes),
        externalEntities = mergeISZs(componentA.externalEntities, componentB.externalEntities)
      )


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

@record class VMGen(symbolTable: SymbolTable,
                    typeMap: HashSMap[String, ir.Component],
                    samplingPorts: HashMap[String, SamplingPortInterface],
                    srcQueues: Map[String, Map[String, QueueObject]],
                    actOptions: ActOptions,
                    reporter: Reporter) {

  val platform: ActPlatform.Type = actOptions.platform
  val performHamrIntegration: B = Util.hamrIntegration(platform)

  var dataports: ISZ[Dataport] = ISZ()
  var emits: ISZ[Emits]= ISZ()
  var uses: ISZ[Uses] = ISZ()
  var consumes: ISZ[Consumes] = ISZ()
  var provides: ISZ[Provides] = ISZ()
  var includes: Set[String] = Set.empty[String]
  var imports: ISZ[String] = ISZ()

  var externalCSources: ISZ[String] = ISZ()
  var externalCIncludeDirs: ISZ[String] = ISZ()

  var preprocessorIncludes: ISZ[String] = ISZ(VMGen.CAMKES_PREPROCESSOR_INCLUDES)
  var externalEntities: ISZ[String] = ISZ(VMGen.VM_INIT_DEF)

  var auxResources: ISZ[Resource] = ISZ()

  var crossConnGCMethods: ISZ[ST] = ISZ()
  var crossConnConnections: ISZ[ST] = ISZ()

  def genThread(aadlThread: AadlThread): (Component, ISZ[Resource]) = {

    assert(aadlThread.toVirtualMachine(symbolTable), s"Thread is not in a vm bound process ${aadlThread.identifier}")

    val parent: AadlProcess = aadlThread.getParent(symbolTable)

    // TODO: not currently expecting feature access
    assert(aadlThread.getFeatureAccesses().isEmpty, s"Not currently handling feature accesses in vm bound thread ${aadlThread.identifier}")

    // TODO: currently expecting exactly 1 thread per vm isolated process
    assert(parent.subComponents.filter(c => CommonUtil.isThread(c.component)).size == 1, s"Expecting exactly one thread per vm bound process ${parent.identifier}")

    // TODO: currently only supporting SeL4_Only, SeL4 to come; SeL4_TB will not be supported
    assert(platform == ActPlatform.SeL4_Only || platform == ActPlatform.SeL4, s"Platform ${platform} is not supported for vm bound processes")

    // TODO: will we ever process models where a sporadic thread is isolated in the vm
    // or we're not using the pacer
    assert(PeriodicUtil.requiresPacerArtifacts(aadlThread.component, symbolTable, platform),
      s"Expecting a periodic thread that will be triggered via a Pacer ${aadlThread.identifier}"
    )

    includes = includes + Util.getSbTypeHeaderFilenameForIncludes()

    for(featureEnd <- aadlThread.getFeatureEnds() if(symbolTable.isConnected(featureEnd))) {
      val fid = CommonUtil.getLastName(featureEnd.identifier)

      featureEnd.category match {
        case ir.FeatureCategory.EventDataPort =>
          actOptions.platform match {
            case ActPlatform.SeL4_Only =>
              handleEventDataPort(featureEnd, aadlThread)
            case ActPlatform.SeL4 =>
              handleEventDataPort(featureEnd, aadlThread)

            case notyet =>
              // TODO
              halt(s"Platform ${notyet} is not currently handled for vm isolated threads: ${aadlThread.identifier}.${fid}")
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
          Dispatcher.handlePeriodicComponent(symbolTable, actOptions, aadlThread, reporter)

        dataports = dataports ++ componentContributions.shell.dataports

        crossConnGCMethods = crossConnGCMethods :+
          VM_Template.vm_cross_conn_extern_dataport_method(PacerTemplate.pacerClientDataportIdentifier())

        crossConnConnections = crossConnConnections :+
          VM_Template.vm_cross_conn_Connection_Period(PacerTemplate.pacerClientDataportIdentifier(), crossConnConnections.size)

      case x =>
        halt(s"Not currently supporting ${x} dispatch protocol")
    }

    val vmCrossConns: ST = VM_Template.vm_cross_vm_connections(crossConnGCMethods, crossConnConnections)
    auxResources = auxResources :+ Util.createResource(
      path = s"${VMGen.getRootVMDir()}/${VMGen.DIR_VM_SRC}/${VMGen.getCrossVMConnectionsFilename(parent)}",
      contents = vmCrossConns,
      overwrite = T
    )

    includes = includes + PacerTemplate.pacerDataportFilenameForIncludes()

    val c = Component(
      control = F,
      hardware = F,
      name = VMGen.VM_COMPONENT_TYPE_NAME,
      mutexes = ISZ(),
      binarySemaphores = ISZ(),
      semaphores = ISZ(),
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

    return (c, auxResources)
  }

  def handleDataPort(f: FeatureEnd, parent: AadlThread): Unit = {

    val fid = CommonUtil.getLastName(f.identifier)

    val connection: ir.ConnectionInstance = f.direction match {
      case ir.Direction.In =>
        val connections = symbolTable.inConnections.get(CommonUtil.getName(f.identifier)).get
        // TODO: fan ins ????
        if(connections.size != 1) {
          // this would probably be bad if sender is fan-outing to a mix of
          // native and VM components.  Perhaps okay if broadcasting to
          // all native though?
          halt(s"Not currently supporting fan-ins for vm isolated threads ${parent.identifier}.${CommonUtil.getLastName(f.identifier)}")
        }
        connections(0)

      case ir.Direction.Out =>

        val connections = symbolTable.outConnections.get(CommonUtil.getName(f.identifier)).get
        // TODO: what to do about fan outs?
        if(connections.size != 1) {
          // what if receivers have different queue sizes?  Would need to
          // broadcast.  Need examples
          halt(s"Not currently supporting fan-outs for VM isolated threads ${parent.identifier}.${CommonUtil.getLastName(f.identifier)}")
        }
        connections(0)

      case x => halt(s"Not expecting direction ${x} for feature ${fid}")
    }

    val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(f.classifier.get)).get
    val sel4TypeName: String = Util.getSel4TypeName(aadlPortType, performHamrIntegration)

    val spi: SamplingPortInterface = samplingPorts.get(sel4TypeName).get

    includes = includes + s"<${spi.headerFilename}>"

    val srcFeature = symbolTable.getFeatureFromName(connection.src.feature.get).asInstanceOf[ir.FeatureEnd]
    val dstFeature = symbolTable.getFeatureFromName(connection.dst.feature.get).asInstanceOf[ir.FeatureEnd]

    val dataPortName = VMGen.getDataportName(srcFeature, dstFeature)

    dataports = dataports :+ Dataport(
      name = dataPortName,
      typ = spi.structName,
      optional = T)

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
    val aadlPortType: ir.Component = typeMap.get(Util.getClassifierFullyQualified(f.classifier.get)).get
    val sel4TypeName: String = Util.getSel4TypeName(aadlPortType, performHamrIntegration)

    val queueSize = PropertyUtil.getQueueSize(f, Util.DEFAULT_QUEUE_SIZE)
    val queueType = Util.getEventDataSBQueueTypeName(sel4TypeName, queueSize)

    includes = includes + s"<${Util.getEventData_SB_QueueHeaderFileName(sel4TypeName, queueSize)}>"

    def getEventDataPortname(connection: ir.ConnectionInstance): String = {
      val srcFeature = symbolTable.getFeatureFromName(connection.src.feature.get).asInstanceOf[ir.FeatureEnd]
      val dstFeature = symbolTable.getFeatureFromName(connection.dst.feature.get).asInstanceOf[ir.FeatureEnd]
      return VMGen.getEventDataportName(srcFeature, dstFeature, queueSize)
    }

    f.direction match {
      case ir.Direction.In =>

        val connections = symbolTable.inConnections.get(CommonUtil.getName(f.identifier)).get
        // TODO: fan ins ????
        if(connections.size != 1) {
          // this would probably be bad if sender is fan-outing to a mix of
          // native and VM components.  Perhaps okay if broadcasting to
          // all native though?
          halt(s"Not currently supporting fan-ins for vm isolated threads ${parent.identifier}.${CommonUtil.getLastName(f.identifier)}")
        }

        val notificationName = Util.genSeL4NotificationName(f, T)

        consumes = consumes :+ Consumes(
          name = notificationName,
          typ = Util.EVENT_NOTIFICATION_TYPE,
          optional = T)

        val dataportName = getEventDataPortname(connections(0)) //Util.getEventDataSBQueueDestFeatureName(fid)

        dataports = dataports :+ Dataport(
          name = dataportName,
          typ = queueType,
          optional = T)

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

      case ir.Direction.Out =>

        val connections = symbolTable.outConnections.get(CommonUtil.getName(f.identifier)).get
        // TODO: what to do about fan outs?
        if(connections.size != 1) {
          // what if receivers have different queue sizes?  Would need to
          // broadcast.  Need examples
          halt(s"Not currently supporting fan-outs for VM isolated threads ${parent.identifier}.${CommonUtil.getLastName(f.identifier)}")
        }

        val emitsName = Util.genSeL4NotificationQueueName(f, queueSize)

        emits = emits :+ Emits(
          name = emitsName,
          typ = Util.EVENT_NOTIFICATION_TYPE)

        val dataPortName = getEventDataPortname(connections(0))

        dataports = dataports :+ Dataport(
          name = dataPortName,
          typ = queueType,
          optional = T)

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

      case x => halt(s"Not expecting direction ${x}: ${parent.identifier}.{fid}")
    }
  }
}
