// #Sireum

package org.sireum.hamr.act.vm

import org.sireum._
import org.sireum.hamr.act.ast._
import org.sireum.hamr.act.proof.ProofContainer.CAmkESConnectionType
import org.sireum.hamr.act.util.{Counter, LibraryComponents, Sel4ConnectorTypes, Util}
import org.sireum.hamr.codegen.common.symbols.{AadlProcess, SymbolTable}

/**
* Original location of vm.h
*   https://github.com/seL4/camkes-arm-vm/blob/e494a6eee46912fc0d89b5976c8e2d2e94dd6e6c/components/VM/configurations/vm.h
*
* Current location of vm.h as of 2021.01.11
*   https://github.com/seL4/camkes-vm/blob/8050057cb5a5b27568e0ec1e46b32c2a4fc5e2c0/components/VM_Arm/configurations/vm.h
*
*   i.e. this was the version being pointed to via the camkes-arm-vm-manifest
*     https://github.com/seL4/camkes-arm-vm-manifest/blob/91e741bbfd44bd028a6efbb633b7353e8be7fe81/default.xml#L21
*
*     after a "repo init -u https://github.com/SEL4PROJ/camkes-arm-vm-manifest.git --depth=1"
*/


/** expansion of objects in macro https://github.com/SEL4PROJ/camkes-arm-vm/blob/301f7bab6cd66b5cf34d904d19c36ee6f7d0ce27/components/VM/configurations/vm.h#L52-L87
*
* #define VM_INIT_DEF() \
    control; \
    uses FileServerInterface fs; \
    DEF_TK1DEVICEFWD \
    DEF_KERNELARMPLATFORM_EXYNOS5410 \
    maybe consumes restart restart_event; \
    has semaphore vm_sem; \
    maybe uses Batch batch; \
    maybe uses PutChar guest_putchar; \
    maybe uses GetChar serial_getchar; \
    maybe uses VirtQueueDev recv; \
    maybe uses VirtQueueDrv send; \
    consumes HaveNotification notification_ready; \
    emits HaveNotification notification_ready_connector; \
    maybe uses VMDTBPassthrough dtb_self; \
    provides VMDTBPassthrough dtb; \
    attribute int base_prio; \
    attribute int num_vcpus = 1; \
    attribute int num_extra_frame_caps; \
    attribute int extra_frame_map_address; \
    attribute { \
        string linux_ram_base; \
        string linux_ram_paddr_base; \
        string linux_ram_size; \
        string linux_ram_offset; \
        string dtb_addr; \
        string initrd_max_size; \
        string initrd_addr; \
    } linux_address_config; \
    attribute { \
        string linux_name = "linux"; \
        string dtb_name = "linux-dtb"; \
        string initrd_name = "linux-initrd"; \
        string linux_bootcmdline = ""; \
        string linux_stdout = ""; \
    } linux_image_config; \
*/
object VM_INIT_DEF {
  def semaphores(): ISZ[Semaphore] = {
    val ret: ISZ[Semaphore] = ISZ(Semaphore(name = "vm_sem", comments = ISZ()))
    return ret
  }

  def provides(aadlProcess: AadlProcess, symbolTable: SymbolTable): ISZ[Provides] = {
    val ret: ISZ[Provides] = ISZ(
      Util.createProvides_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "dtb",
        typ = "VMDTBPassthrough")
    )
    return ret
  }

  def emits(aadlProcess: AadlProcess, symbolTable: SymbolTable): ISZ[Emits] = {
    val ret: ISZ[Emits] = ISZ(
      Util.createEmits_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "notification_ready_connector",
        typ = "HaveNotification")
    )
    return ret
  }

  def consumes(aadlProcess: AadlProcess, symbolTable: SymbolTable): ISZ[Consumes] = {
    val ret: ISZ[Consumes] = ISZ(
      Util.createConsumes_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "restart_event",
        typ = "restart",
        optional = T),
      Util.createConsumes_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "notification_ready",
        typ = "HaveNotification",
        optional = F)
    )
    return ret
  }

  def dataports(KERNELARMPLATFORM_EXYNOS5410: B): ISZ[Dataport] = {
    var ret: ISZ[Dataport] = ISZ()
    // https://github.com/SEL4PROJ/camkes-arm-vm/blob/301f7bab6cd66b5cf34d904d19c36ee6f7d0ce27/components/VM/configurations/vm.h#L40-L46
    if (KERNELARMPLATFORM_EXYNOS5410) {
      ret = ret ++ ISZ(
        Dataport(
          name = "cmu_cpu",
          typ = "Buf",
          optional = F,
          comments = ISZ()),
        Dataport(
          name = "cmu_top",
          typ = "Buf",
          optional = F,
          comments = ISZ()),
        Dataport(
          name = "gpio_right",
          typ = "Buf",
          optional = F,
          comments = ISZ()),
        Dataport(
          name = "cmu_core",
          typ = "Buf",
          optional = F,
          comments = ISZ())
      )
    }
    return ret
  }

  def uses(TK1DEVICEFWD: B, KERNELARMPLATFORM_EXYNOS5410: B, aadlProcess: AadlProcess, symbolTable: SymbolTable): ISZ[Uses] = {
    var ret: ISZ[Uses] = ISZ(
      Util.createUses_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "fs",
        typ = "FileServerInterface",
        optional = F),
      Util.createUses_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "batch",
        typ = "Batch",
        optional = T),
      Util.createUses_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "guest_putchar",
        typ = "PutChar",
        optional = T),
      Util.createUses_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "serial_getchar",
        typ = "GetChar",
        optional = T),
      Util.createUses_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "recv",
        typ = "VirtQueueDev",
        optional = T),
      Util.createUses_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "send",
        typ = "VirtQueueDrv",
        optional = T),
      Util.createUses_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "dtb_self",
        typ = "VMDTBPassthrough",
        optional = T)
    )

    if (TK1DEVICEFWD) {
      // https://github.com/SEL4PROJ/camkes-arm-vm/blob/301f7bab6cd66b5cf34d904d19c36ee6f7d0ce27/components/VM/configurations/vm.h#L32-L34
      ret = ret ++ ISZ(
        Util.createUses_VMAux(
          aadlProcess = aadlProcess,
          symbolTable = symbolTable,
          name = "uartfwd",
          typ = "gen_fwd_inf",
          optional = F),
        Util.createUses_VMAux(
          aadlProcess = aadlProcess,
          symbolTable = symbolTable,
          name = "clkcarfwd",
          typ = "gen_fwd_inf",
          optional = F)
      )
    }

    if (KERNELARMPLATFORM_EXYNOS5410) {
      //https://github.com/SEL4PROJ/camkes-arm-vm/blob/301f7bab6cd66b5cf34d904d19c36ee6f7d0ce27/components/VM/configurations/vm.h#L40-L46
      ret = ret ++ ISZ(
        Util.createUses_VMAux(
          aadlProcess = aadlProcess,
          symbolTable = symbolTable,
          name = "pwm",
          typ = "pwm_inf",
          optional = F)
      )
    }
    return ret
  }
}

// expansion of objects in macro https://github.com/seL4/camkes-vm/blob/39734d70d38af597e459f4923c75db95508d9661/components/VM_Arm/configurations/vm.h#L87

// #define VM_COMPONENT_CONNECTIONS_DEF(num) \
//    connection seL4RPCDataport fs##num(from vm##num.fs, to fserv.fs_ctrl); \
//    connection seL4GlobalAsynch notify_ready_vm##num(from vm##num.notification_ready_connector, to vm##num.notification_ready); \
object VM_COMPONENT_CONNECTIONS_DEF {

  def connections(processId: String, connectionCounter: Counter): ISZ[Connection] = {
    var connections: ISZ[Connection] = ISZ()

    connections = connections :+ Util.createConnectionC(
      connectionCategory = CAmkESConnectionType.VM,
      connectionCounter = connectionCounter,
      connectionType = Sel4ConnectorTypes.seL4VMDTBPassthrough,
      srcComponent = processId, srcFeature = "dtb_self",
      dstComponent = processId, dstFeature ="dtb"
    )

    // #define VM_COMPONENT_CONNECTIONS_DEF(num) \
    //    connection seL4RPCDataport fs##num(from vm##num.fs, to fserv.fs_ctrl); \
    //    connection seL4GlobalAsynch notify_ready_vm##num(from vm##num.notification_ready_connector, to vm##num.notification_ready); \

    connections = connections :+ Util.createConnectionC(
      connectionCategory = CAmkESConnectionType.VM,
      connectionCounter = connectionCounter,
      connectionType = Sel4ConnectorTypes.seL4RPCDataport,
      srcComponent = processId, srcFeature = "fs",
      dstComponent = LibraryComponents.FileServer.defaultFileServerName,
      dstFeature = LibraryComponents.FileServer.fs_ctrl_port)

    connections = connections :+ Util.createConnectionC(
      connectionCategory = CAmkESConnectionType.VM,
      connectionCounter = connectionCounter,
      connectionType = Sel4ConnectorTypes.seL4GlobalAsynch,
      srcComponent = processId, srcFeature = "notification_ready_connector",
      dstComponent = processId, dstFeature = "notification_ready")

    // connection seL4SerialServer serial_vm##num(from vm##num.batch, to serial.processed_batch); \
    connections = connections :+ Util.createConnectionC(
      connectionCategory = CAmkESConnectionType.VM,
      connectionCounter = connectionCounter,
      connectionType = Sel4ConnectorTypes.seL4SerialServer,
      srcComponent = processId,
      srcFeature = "batch",
      dstComponent = LibraryComponents.SerialServer.defaultSerialServerName,
      dstFeature = LibraryComponents.SerialServer.processed_batch_Port
    )

    // connection seL4SerialServer serial_input_vm##num(from vm##num.serial_getchar, to serial.getchar);
    connections = connections :+ Util.createConnectionC(
      connectionCategory = CAmkESConnectionType.VM,
      connectionCounter = connectionCounter,
      connectionType = Sel4ConnectorTypes.seL4SerialServer,
      srcComponent = processId,
      srcFeature = "serial_getchar",
      dstComponent = LibraryComponents.SerialServer.defaultSerialServerName,
      dstFeature = LibraryComponents.SerialServer.getchar_Port
    )

    return connections
  }
}

// expansion of configurations in https://github.com/SEL4PROJ/camkes-arm-vm/blob/301f7bab6cd66b5cf34d904d19c36ee6f7d0ce27/components/VM/configurations/vm.h#L104-L105
//
// #define VM_GENERAL_CONFIGURATION_DEF() \
//    fserv.heap_size = 165536; \
object VM_GENERAL_CONFIGURATION_DEF {
  def entries(): ISZ[String] = {
    return ISZ("fserv.heap_size = 165536;")
  }
}

// expansion of configurations in https://github.com/SEL4PROJ/camkes-arm-vm/blob/301f7bab6cd66b5cf34d904d19c36ee6f7d0ce27/components/VM/configurations/vm.h#L107-L115
//
// #define VM_CONFIGURATION_DEF(num) \
//    vm##num.fs_shmem_size = 0x100000; \
//    vm##num.global_endpoint_base = 1 << 27; \
//    vm##num.asid_pool = true; \
//    vm##num.simple = true; \
//    vm##num.base_prio = 100; \
//    vm##num._priority = 101; \
//    vm##num.sem_value = 0; \
//    vm##num.heap_size = 0x300000;
object VM_CONFIGURATION_DEF {
  def entries(componentId: String): ISZ[String] = {
    return ISZ(
        s"${componentId}.fs_shmem_size = 0x100000;",
      s"${componentId}.global_endpoint_base = 1 << 27;",
      s"${componentId}.asid_pool = true;",
      s"${componentId}.simple = true;",
      s"${componentId}.base_prio = 100;",
      s"${componentId}._priority = 101;",
      s"${componentId}.sem_value = 0;",
      s"${componentId}.heap_size = 0x300000;")
  }
}

// expansion of configurations in https://github.com/SEL4PROJ/camkes-arm-vm/blob/301f7bab6cd66b5cf34d904d19c36ee6f7d0ce27/components/VM/configurations/vm.h#L135-L138
//
// #define VM_VIRTUAL_SERIAL_GENERAL_CONFIGURATION_DEF() \
//    time_server.timers_per_client = 1; \
////    time_server.priority = 255; \
////    time_server.simple = true;
object VM_VIRTUAL_SERIAL_GENERAL_CONFIGURATION_DEF{
  def entries(): ISZ[String]= {
    return ISZ(
      "time_server.timers_per_client = 1;",
      "time_server.priority = 255;",
      "time_server.simple = true;"
    )
  }
}

// expansion of configurations in https://github.com/SEL4PROJ/camkes-arm-vm/blob/301f7bab6cd66b5cf34d904d19c36ee6f7d0ce27/components/VM/configurations/vm.h#L140-L142
//
// #define PER_VM_VIRTUAL_SERIAL_CONFIGURATION_DEF(num) \
//    vm##num.serial_getchar_shmem_size = 0x1000; \
////    vm##num.batch_shmem_size = 0x1000; \
object PER_VM_VIRTUAL_SERIAL_CONFIGURATION_DEF {
  def entries(componentId: String): ISZ[String]= {
    return ISZ(
      s"${componentId}.serial_getchar_shmem_size = 0x1000;",
      s"${componentId}.batch_shmem_size = 0x1000;"
    )
  }
}
