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
* Current location of vm.h as of 2022.11.18
*   https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h
*
*   i.e. this was the version being pointed to via the camkes-arm-vm-manifest
*     https://github.com/seL4/camkes-arm-vm-manifest/blob/91e741bbfd44bd028a6efbb633b7353e8be7fe81/default.xml#L21
*
*     after a "repo init -u https://github.com/SEL4PROJ/camkes-arm-vm-manifest.git --depth=1"
*/


/** expansion of objects in macro
* https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L50-L86
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
        string dtb_base_name = ""; \
    } linux_image_config; \
*/

object VM_H {
  val vmH_Location: String = "https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h"

  def tag(macroName: String, lineNumber: Z): String = {
    return s"// expansion of macro ${macroName}. See ${vmH_Location}#L${lineNumber}"
  }
}

object VM_INIT_DEF {
  def semaphores(): ISZ[Semaphore] = {
    // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L56
    val ret: ISZ[Semaphore] = ISZ(Semaphore(name = "vm_sem", comments = ISZ()))
    return ret
  }

  def provides(aadlProcess: AadlProcess, symbolTable: SymbolTable): ISZ[Provides] = {
    // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L65
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
    // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L63
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
      // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L55
      Util.createConsumes_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "restart_event",
        typ = "restart",
        optional = T),
      // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L62
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
    if (KERNELARMPLATFORM_EXYNOS5410) {
      ret = ret ++ ISZ(
        // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L41
        Dataport(
          name = "cmu_cpu",
          typ = "Buf",
          optional = F,
          comments = ISZ()),
        // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L42
        Dataport(
          name = "cmu_top",
          typ = "Buf",
          optional = F,
          comments = ISZ()),
        // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L43
        Dataport(
          name = "gpio_right",
          typ = "Buf",
          optional = F,
          comments = ISZ()),
        // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L44
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
      // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L52
      Util.createUses_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "fs",
        typ = "FileServerInterface",
        optional = F),
      // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L57
      Util.createUses_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "batch",
        typ = "Batch",
        optional = T),
      // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L58
      Util.createUses_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "guest_putchar",
        typ = "PutChar",
        optional = T),
      // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L59
      Util.createUses_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "serial_getchar",
        typ = "GetChar",
        optional = T),
      // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L60
      Util.createUses_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "recv",
        typ = "VirtQueueDev",
        optional = T),
      // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L61
      Util.createUses_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "send",
        typ = "VirtQueueDrv",
        optional = T),
      // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L64
      Util.createUses_VMAux(
        aadlProcess = aadlProcess,
        symbolTable = symbolTable,
        name = "dtb_self",
        typ = "VMDTBPassthrough",
        optional = T)
    )

    if (TK1DEVICEFWD) {
      ret = ret ++ ISZ(
        // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L31
        Util.createUses_VMAux(
          aadlProcess = aadlProcess,
          symbolTable = symbolTable,
          name = "uartfwd",
          typ = "gen_fwd_inf",
          optional = F),
        // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L32
        Util.createUses_VMAux(
          aadlProcess = aadlProcess,
          symbolTable = symbolTable,
          name = "clkcarfwd",
          typ = "gen_fwd_inf",
          optional = F)
      )
    }

    if (KERNELARMPLATFORM_EXYNOS5410) {
      ret = ret ++ ISZ(
        // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L40
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

  def attributes(): ST = {
    // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L66-L86
    val ret: ST =
      st"""// expanding attributes in VM_INIT_DEF()
          |// https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L66-L86
          |attribute int base_prio;
          |attribute int num_vcpus = 1;
          |attribute int num_extra_frame_caps;
          |attribute int extra_frame_map_address;
          |attribute {
          |  string linux_ram_base;
          |  string linux_ram_paddr_base;
          |  string linux_ram_size;
          |  string linux_ram_offset;
          |  string dtb_addr;
          |  string initrd_max_size;
          |  string initrd_addr;
          |} linux_address_config;
          |attribute {
          |  string linux_name = "linux";
          |  string dtb_name = "linux-dtb";
          |  string initrd_name = "linux-initrd";
          |  string linux_bootcmdline = "";
          |  string linux_stdout = "";
          |  string dtb_base_name = "";
          |} linux_image_config;
          |// end of attribute expansion in VM_INIT_DEF()"""
    return ret
  }
}

// expansion of objects in macro
// https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L92-L94

// #define VM_COMPONENT_CONNECTIONS_DEF(num) \
//    connection seL4RPCDataport fs##num(from vm##num.fs, to fserv.fs_ctrl); \
//    connection seL4GlobalAsynch notify_ready_vm##num(from vm##num.notification_ready_connector, to vm##num.notification_ready); \
object VM_COMPONENT_CONNECTIONS_DEF {

  def connections(processId: String, connectionCounter: Counter): ISZ[Connection] = {
    var connections: ISZ[Connection] = ISZ()

    // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L93
    val fsConnection = Util.createConnectionC(
      connectionCategory = CAmkESConnectionType.VM,
      connectionCounter = connectionCounter,
      connectionType = Sel4ConnectorTypes.seL4RPCDataport,
      srcComponent = processId, srcFeature = "fs",
      dstComponent = LibraryComponents.FileServer.defaultFileServerName,
      dstFeature = LibraryComponents.FileServer.fs_ctrl_port)

    // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L94
    val notificationConnection = Util.createConnectionC(
      connectionCategory = CAmkESConnectionType.VM,
      connectionCounter = connectionCounter,
      connectionType = Sel4ConnectorTypes.seL4GlobalAsynch,
      srcComponent = processId, srcFeature = "notification_ready_connector",
      dstComponent = processId, dstFeature = "notification_ready")

    val comment = VM_H.tag("VM_COMPONENT_CONNECTIONS_DEF", 92)
    val fsConnectionWithComment = fsConnection(comments = ISZ(Util.createPreComment(comment)))

    connections = connections :+ fsConnectionWithComment :+ notificationConnection(comments = ISZ(Util.createPostComment("")))

    return connections
  }
}

object VM_GENERAL_COMPOSITION_DEF {
  // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L96
  val name: String = "VM_GENERAL_COMPOSITION_DEF"

  def instances(): ISZ[Instance] = {
    return ISZ(
      LibraryComponents.FileServer.defaultFileServerInstance(comments = ISZ(Util.createInlineComment(VM_H.tag(name, 96))))
    )
  }
}

object PER_VM_VIRTUAL_SERIAL_CONNECTIONS_DEF {
  // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L126-L128
  val name: String = "PER_VM_VIRTUAL_SERIAL_CONNECTIONS_DEF"

  def connections(processId: String, connectionCounter: Counter): ISZ[Connection] = {
    // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L127
    // connection seL4SerialServer serial_vm##num(from vm##num.batch, to serial.processed_batch); \
    val batchConnection = Util.createConnectionC(
      connectionCategory = CAmkESConnectionType.VM,
      connectionCounter = connectionCounter,
      connectionType = Sel4ConnectorTypes.seL4SerialServer,
      srcComponent = processId,
      srcFeature = "batch",
      dstComponent = LibraryComponents.SerialServer.defaultSerialServerName,
      dstFeature = LibraryComponents.SerialServer.processed_batch_Port
    )

    // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L128
    // connection seL4SerialServer serial_input_vm##num(from vm##num.serial_getchar, to serial.getchar);
    val getCharConnection = Util.createConnectionC(
      connectionCategory = CAmkESConnectionType.VM,
      connectionCounter = connectionCounter,
      connectionType = Sel4ConnectorTypes.seL4SerialServer,
      srcComponent = processId,
      srcFeature = "serial_getchar",
      dstComponent = LibraryComponents.SerialServer.defaultSerialServerName,
      dstFeature = LibraryComponents.SerialServer.getchar_Port
    )

    return ISZ(batchConnection(comments = ISZ(Util.createPreComment(VM_H.tag(name, 126)))),
      getCharConnection(comments = ISZ(Util.createPostComment(""))))
  }
}

object VM_VIRTUAL_SERIAL_COMPONENTS_DEF {
  // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L121-L124
  val name: String = "VM_VIRTUAL_SERIAL_COMPONENTS_DEF"

  def instances(): ISZ[Instance] = {
    return ISZ(
      // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L122
      LibraryComponents.SerialServer.defaultSerialServerInstance(comments = ISZ(Util.createPreComment(""), Util.createPreComment(VM_H.tag(name, 122)))),
      // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L123
      LibraryComponents.TimeServer.defaultTimeServerInstance(comments = ISZ(Util.createPostComment("")))
    )
  }

  def connections(connectionCounter: Counter): ISZ[Connection] = {
    // https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L124
    val connection: Connection = Util.createConnectionC(
      //connectionCategory = CAmkESConnectionType.TimeServer,
      connectionCategory = CAmkESConnectionType.VM,
      connectionCounter = connectionCounter,
      connectionType = Sel4ConnectorTypes.seL4TimeServer,
      srcComponent = LibraryComponents.SerialServer.defaultSerialServerName,
      srcFeature = LibraryComponents.SerialServer.timeout_Port,
      dstComponent = LibraryComponents.TimeServer.defaultTimeServerName,
      dstFeature = LibraryComponents.TimeServer.the_timer_port)

    return ISZ(connection(comments = ISZ(
      Util.createPreComment(""), Util.createPreComment(VM_H.tag(name, 124)),
      Util.createPostComment(""))))
  }
}

// expansion of configurations in
// https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L103-L104

// #define VM_GENERAL_CONFIGURATION_DEF() \
//    fserv.heap_size = 0x30000; \
object VM_GENERAL_CONFIGURATION_DEF {
  val name: String = "VM_GENERAL_CONFIGURATION_DEF"

  def configurations(): ISZ[Configuration] = {
    val comment = VM_H.tag(name, 104)
    return ISZ(GenericConfiguration("fserv.heap_size = 0x30000;", ISZ(Util.createInlineComment(comment))))
  }
}

// expansion of configurations in
// https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L106-L114

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
  val name: String = "VM_CONFIGURATION_DEF"

  def configurations(componentId: String): ISZ[Configuration] = {
    val comments: ISZ[AstComment] = ISZ(Util.createPreComment(""), Util.createPreComment(VM_H.tag(name, 106)))
    return ISZ(
      GenericConfiguration(s"${componentId}.fs_shmem_size = 0x100000;", comments),
      GenericConfiguration(s"${componentId}.global_endpoint_base = 1 << 27;", ISZ()),
      GenericConfiguration(s"${componentId}.asid_pool = true;", ISZ()),
      GenericConfiguration(s"${componentId}.simple = true;", ISZ()),
      GenericConfiguration(s"${componentId}.base_prio = 100;", ISZ()),
      GenericConfiguration(s"${componentId}._priority = 101;", ISZ()),
      GenericConfiguration(s"${componentId}.sem_value = 0;", ISZ()),
      GenericConfiguration(s"${componentId}.heap_size = 0x300000;", ISZ(Util.createPostComment(""))))
  }
}

// expansion of configurations in
// https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L134-L137

// #define VM_VIRTUAL_SERIAL_GENERAL_CONFIGURATION_DEF() \
//    time_server.timers_per_client = 1; \
//    time_server.priority = 255; \
//    time_server.simple = true;
object VM_VIRTUAL_SERIAL_GENERAL_CONFIGURATION_DEF {
  val name: String = "VM_VIRTUAL_SERIAL_GENERAL_CONFIGURATION_DEF"

  def configurations(): ISZ[Configuration] = {
    val comments: ISZ[AstComment] = ISZ(Util.createPreComment(""), Util.createPreComment(VM_H.tag(name, 134)))
    return ISZ(
      GenericConfiguration("time_server.timers_per_client = 1;", comments),
      GenericConfiguration("time_server.priority = 255;", ISZ()),
      GenericConfiguration("time_server.simple = true;", ISZ(Util.createPostComment("")))
    )
  }
}

// expansion of configurations in
// https://github.com/seL4/camkes-vm/blob/9e09ea46609ea7fadd8212ecf3d0e95f8553f2af/components/VM_Arm/configurations/vm.h#L139-L141

// #define PER_VM_VIRTUAL_SERIAL_CONFIGURATION_DEF(num) \
//    vm##num.serial_getchar_shmem_size = 0x1000; \
//    vm##num.batch_shmem_size = 0x1000; \
object PER_VM_VIRTUAL_SERIAL_CONFIGURATION_DEF {
  val name: String = "PER_VM_VIRTUAL_SERIAL_CONFIGURATION_DEF"

  def configurations(componentId: String): ISZ[Configuration] = {
    val comments: ISZ[AstComment] = ISZ(Util.createPreComment(""), Util.createPreComment(VM_H.tag(name, 139)))
    return ISZ(
      GenericConfiguration(s"${componentId}.serial_getchar_shmem_size = 0x1000;", comments),
      GenericConfiguration(s"${componentId}.batch_shmem_size = 0x1000;", ISZ(Util.createPostComment("")))
    )
  }
}
