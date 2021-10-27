// #Sireum

package org.sireum.hamr.act.vm

import org.sireum._
import org.sireum.hamr.act.ast._
import org.sireum.hamr.act.util.Util

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
    val ret: ISZ[Semaphore] = ISZ(Semaphore(name = "vm_sem"))
    return ret
  }

  def provides(): ISZ[Provides] = {
    val ret: ISZ[Provides] = ISZ(
      Util.createProvides_VM(

        name = "dtb",
        typ = "VMDTBPassthrough")
    )
    return ret
  }

  def emits(): ISZ[Emits] = {
    val ret: ISZ[Emits] = ISZ(
      Emits(
        name = "notification_ready_connector",
        typ = "HaveNotification")
    )
    return ret
  }

  def consumes(): ISZ[Consumes] = {
    val ret: ISZ[Consumes] = ISZ(
      Consumes(
        name = "restart_event",
        typ = "restart",
        optional = T),
      Consumes(
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
          optional = F),
        Dataport(
          name = "cmu_top",
          typ = "Buf",
          optional = F),
        Dataport(
          name = "gpio_right",
          typ = "Buf",
          optional = F),
        Dataport(
          name = "cmu_core",
          typ = "Buf",
          optional = F)
      )
    }
    return ret
  }

  def uses(TK1DEVICEFWD: B, KERNELARMPLATFORM_EXYNOS5410: B): ISZ[Uses] = {
    var ret: ISZ[Uses] = ISZ(
      Uses(
        name = "fs",
        typ = "FileServerInterface",
        optional = F),
      Uses(
        name = "batch",
        typ = "Batch",
        optional = T),
      Uses(
        name = "guest_putchar",
        typ = "PutChar",
        optional = T),
      Uses(
        name = "serial_getchar",
        typ = "GetChar",
        optional = T),
      Uses(
        name = "recv",
        typ = "VirtQueueDev",
        optional = T),
      Uses(
        name = "send",
        typ = "VirtQueueDrv",
        optional = T),
      Uses(
        name = "dtb_self",
        typ = "VMDTBPassthrough",
        optional = T)
    )

    if (TK1DEVICEFWD) {
      // https://github.com/SEL4PROJ/camkes-arm-vm/blob/301f7bab6cd66b5cf34d904d19c36ee6f7d0ce27/components/VM/configurations/vm.h#L32-L34
      ret = ret ++ ISZ(
        Uses(
          name = "uartfwd",
          typ = "gen_fwd_inf",
          optional = F),
        Uses(
          name = "clkcarfwd",
          typ = "gen_fwd_inf",
          optional = F)
      )
    }

    if (KERNELARMPLATFORM_EXYNOS5410) {
      //https://github.com/SEL4PROJ/camkes-arm-vm/blob/301f7bab6cd66b5cf34d904d19c36ee6f7d0ce27/components/VM/configurations/vm.h#L40-L46
      ret = ret ++ ISZ(
        Uses(
          name = "pwm",
          typ = "pwm_inf",
          optional = F)
      )
    }
    return ret
  }
}

// expansion of objects in macro https://github.com/SEL4PROJ/camkes-arm-vm/blob/301f7bab6cd66b5cf34d904d19c36ee6f7d0ce27/components/VM/configurations/vm.h#L97-L98
// #define VM_GENERAL_COMPOSITION_DEF() \
//    component FileServer fserv; \
object VM_GENERAL_COMPOSITION_DEF {
  def instances: ISZ[Instance] = {
    val ret: ISZ[Instance] = ISZ(
      Instance(
        address_space = "",
        name = "fserv",
        component = LibraryComponent(name = "FileServer", ports = ISZ())
      )
    )
    return ret
  }
}

// expansion of objects in macro https://github.com/SEL4PROJ/camkes-arm-vm/blob/301f7bab6cd66b5cf34d904d19c36ee6f7d0ce27/components/VM/configurations/vm.h#L93-L95

// #define VM_COMPONENT_CONNECTIONS_DEF(num) \
//    connection seL4RPCDataport fs##num(from vm##num.fs, to fserv.fs_ctrl); \
//    connection seL4GlobalAsynch notify_ready_vm##num(from vm##num.notification_ready_connector, to vm##num.notification_ready); \
object VM_COMPONENT_CONNECTIONS_DEF {
  def connections(componentId: String): ISZ[Connection] = {
    val ret: ISZ[Connection] = ISZ(
      Connection(
        name = s"fs${componentId}",
        connectionType = "seL4RPCDataport",
        from_ends = ISZ(
          ConnectionEnd(
            isFrom = T,
            component = componentId,
            end = "fs")
        ),
        to_ends = ISZ(
          ConnectionEnd(
            isFrom = F,
            component = "fserv",
            end = "fs_ctrl")
        )),
      Connection(
        name = s"notify_ready_${componentId}",
        connectionType = "seL4GlobalAsynch",
        from_ends = ISZ(
          ConnectionEnd(
            isFrom = T,
            component = componentId,
            end = "notification_ready_connector")),
        to_ends = ISZ(
          ConnectionEnd(
            isFrom = F,
            component = componentId,
            end = "notification_ready")
        ))
    )
    return ret
  }
}

// expansion of objects in macro https://github.com/SEL4PROJ/camkes-arm-vm/blob/301f7bab6cd66b5cf34d904d19c36ee6f7d0ce27/components/VM/configurations/vm.h#L122-L125
//
// #define VM_VIRTUAL_SERIAL_COMPONENTS_DEF() \
//    component SerialServer serial; \
//    component TimeServer time_server; \
//    connection seL4TimeServer serialserver_timer(from serial.timeout, to time_server.the_timer); \
object VM_VIRTUAL_SERIAL_COMPONENTS_DEF {
  val seL4TimeServer: String = "seL4TimeServer"
  val serial: String = "serial"
  val time_server: String = "time_server"

  def instances(): ISZ[Instance] = {
    val ret: ISZ[Instance] = ISZ(
      Instance(
        address_space = "",
        name = serial,
        component = LibraryComponent(name = "SerialServer", ports = ISZ())),
      Instance(
        address_space = "",
        name = time_server,
        component = LibraryComponent(name = "TimeServer", ports = ISZ()))
    )
    return ret
  }

  def connections(): ISZ[Connection] = {
    val ret: ISZ[Connection] = ISZ(
      Connection(
        name = "serialserver_timer",
        connectionType = seL4TimeServer,
        from_ends = ISZ(
          ConnectionEnd(
            isFrom = T,
            component = serial,
            end = "timeout")
        ),
        to_ends = ISZ(
          ConnectionEnd(
            isFrom = F,
            component = time_server,
            end = "the_timer")
        )
      )
    )
    return ret
  }
}

// expansion of objects in macro https://github.com/SEL4PROJ/camkes-arm-vm/blob/301f7bab6cd66b5cf34d904d19c36ee6f7d0ce27/components/VM/configurations/vm.h#L127-L129
//
// #define PER_VM_VIRTUAL_SERIAL_CONNECTIONS_DEF(num) \
//    connection seL4SerialServer serial_vm##num(from vm##num.batch, to serial.processed_batch); \
//    connection seL4SerialServer serial_input_vm##num(from vm##num.serial_getchar, to serial.getchar);
object PER_VM_VIRTUAL_SERIAL_CONNECTIONS_DEF {
  val seL4SerialServer: String = "seL4SerialServer"
  val serial: String = "serial"

  def connections(componentId: String): ISZ[Connection] = {
    val ret: ISZ[Connection] = ISZ(
      // connection seL4SerialServer serial_vm##num(from vm##num.batch, to serial.processed_batch); \
      Connection(
        name = s"serial_${componentId}",
        connectionType = seL4SerialServer,
        from_ends = ISZ(
          ConnectionEnd(
            isFrom = T,
            component = componentId,
            end = "batch")
        ),
        to_ends = ISZ(
          ConnectionEnd(
            isFrom = F,
            component = serial,
            end = "processed_batch")
        )
      ),
      // connection seL4SerialServer serial_input_vm##num(from vm##num.serial_getchar, to serial.getchar);
      Connection(
        name = s"serial_input_${componentId}",
        connectionType = seL4SerialServer,
        from_ends = ISZ(
          ConnectionEnd(
            isFrom = T,
            component = componentId,
            end = "serial_getchar")
        ),
        to_ends = ISZ(
          ConnectionEnd(
            isFrom = F,
            component = serial,
            end = "getchar")
        )
      )
    )
    return ret
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
