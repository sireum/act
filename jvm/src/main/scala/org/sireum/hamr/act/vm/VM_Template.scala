// #Sireum
package org.sireum.hamr.act.vm

import org.sireum._
import org.sireum.hamr.act.ast
import org.sireum.hamr.act.templates._
import org.sireum.hamr.act.util.{CMakeOption, CMakeStandardOption}

object VM_Template {

  val USE_PRECONFIGURED_ROOTFS: String = "USE_PRECONFIGURED_ROOTFS"

  val data61_VM_Macros_Location: String = "<configurations/vm.h>"

  val VM_CMAKE_OPTIONS: ISZ[CMakeOption] = ISZ(
    CMakeStandardOption(USE_PRECONFIGURED_ROOTFS, F, "Use preconfigured rootfs rather than downloading a vanilla linux image")
  )

  def vm_assembly_preprocessor_includes(): ISZ[String] = {
    val ret: ISZ[String] = ISZ(
      data61_VM_Macros_Location
    )
    return ret
  }


  /*
  def vm_composition_macros_expanded(processID: String): ISZ[String] = {
    val ret: ISZ[String] = ISZ(
      st"""// expanding: VM_GENERAL_COMPOSITION_DEF()
          |component FileServer fserv;""".render,
      st"""// expanding: VM_COMPONENT_CONNECTIONS_DEF(${processID})
          |connection seL4RPCDataport fs${processID}(from vm${processID}.fs, to fserv.fs_ctrl);
          |connection seL4GlobalAsynch notify_ready_vm${processID}(from vm${processID}.notification_ready_connector, to vm${processID}.notification_ready);""".render,
      st"""// expanding: VM_VIRTUAL_SERIAL_COMPONENTS_DEF()
          |component SerialServer serial;
          |component TimeServer time_server;
          |connection seL4TimeServer serialserver_timer(from serial.timeout, to time_server.the_timer);""".render,
      st"""// expanding: PER_VM_VIRTUAL_SERIAL_CONNECTIONS_DEF(${processID})
          |connection seL4SerialServer serial_vm${processID}(from vm${processID}.batch, to serial.processed_batch);
          |connection seL4SerialServer serial_input_vm${processID}(from vm${processID}.serial_getchar, to serial.getchar);""".render)
    return ret
  }
   */

  def vm_assembly_configuration_entries(vmProcessID: String): ISZ[ast.Configuration] = {
    val ret: ISZ[ast.Configuration] = ISZ(
      ast.GenericConfiguration(s"${vmProcessID}.cnode_size_bits = 18;", ISZ()),
      ast.GenericConfiguration(s"${vmProcessID}.simple_untyped21_pool = 12;", ISZ()),
      ast.GenericConfiguration(s"${vmProcessID}.simple_untyped12_pool = 12;", ISZ()),
      ast.GenericConfiguration(s"${vmProcessID}.num_extra_frame_caps = 0;", ISZ()),
      ast.GenericConfiguration(s"${vmProcessID}.extra_frame_map_address = 0;", ISZ())
    )
    return ret
  }

  def vm_assembly_configuration_macros(vmProcessId: String): ISZ[ast.Configuration] = {
    val ret: ISZ[ast.Configuration] = ISZ(
      ast.GenericConfiguration("VM_GENERAL_CONFIGURATION_DEF()", ISZ()),
      ast.GenericConfiguration(s"VM_CONFIGURATION_DEF(${vmProcessId})", ISZ()),
      ast.GenericConfiguration(s"VM_VIRTUAL_SERIAL_GENERAL_CONFIGURATION_DEF()", ISZ()),
      ast.GenericConfiguration(s"PER_VM_VIRTUAL_SERIAL_CONFIGURATION_DEF(${vmProcessId})", ISZ())
    )
    return ret
  }

  /*
  def vm_assembly_configuration_macros_expanded(vmProcessId: String): ISZ[String] = {
    val ret: ISZ[String] = ISZ(
      st"""// expanding: VM_GENERAL_CONFIGURATION_DEF()
          |fserv.heap_size = 165536;""".render,
      st"""// expanding: VM_CONFIGURATION_DEF(${vmProcessId})
          |vm${vmProcessId}.fs_shmem_size = 0x100000;
          |vm${vmProcessId}.global_endpoint_base = 1 << 27;
          |vm${vmProcessId}.asid_pool = true; \
          |vm${vmProcessId}.simple = true; \
          |vm${vmProcessId}.base_prio = 100; \
          |vm${vmProcessId}._priority = 101; \
          |vm${vmProcessId}.sem_value = 0; \
          |vm${vmProcessId}.heap_size = 0x300000;""".render,
      st"""// expanding: VM_VIRTUAL_SERIAL_GENERAL_CONFIGURATION_DEF()
          |time_server.timers_per_client = 1; \
          |time_server.priority = 255; \
          |time_server.simple = true;""".render,
      st"""// expanding: PER_VM_VIRTUAL_SERIAL_CONFIGURATION_DEF(${vmProcessId})
          |vm${vmProcessId}.serial_getchar_shmem_size = 0x1000;
          |vm${vmProcessId}.batch_shmem_size = 0x1000;""".render)
    return ret
  }
  */

  def vm_assembly_imports(): ISZ[String] = {
    val ret: ISZ[String] = ISZ(
      "<global-connectors.camkes>",
      "<seL4VMDTBPassthrough.idl4>",
      "<FileServerInterface.camkes>",
      "<FileServer/FileServer.camkes>",
      "<SerialServer/SerialServer.camkes>",
      "<TimeServer/TimeServer.camkes>",
      "<vm-connectors.camkes>",
      "<devices.camkes>")
    return ret
  }

  def vm_cross_conn_extern_dataport_method(dataportName: String): ST = {
    return st"extern dataport_caps_handle_t ${dataportName}_handle;"
  }

  def vm_cross_conn_extern_emit_method(queueName: String): ST = {
    return st"void ${queueName}_emit_underlying(void);"
  }

  def vm_cross_conn_extern_notification_methods(notificationName: String): ST = {
    return st"seL4_Word ${notificationName}_notification_badge(void);"
  }

  def vm_cross_conn_Connections(methodNamePrefix: String,
                                emitMethodNamePrefix: Option[String],
                                notificationNamePrefix: Option[String],
                                counter: Z): ST = {
    val emit_fn: String =
      if(emitMethodNamePrefix.isEmpty) "NULL"
      else s"${emitMethodNamePrefix.get}_emit_underlying"

    val consume_badge: String =
      if(notificationNamePrefix.isEmpty) "-1"
      else s"${notificationNamePrefix.get}_notification_badge()"

    val ret: ST =
      st"""connections[${counter}] = (struct camkes_crossvm_connection) {
          |  .handle = &${methodNamePrefix}_handle,
          |  .emit_fn = ${emit_fn},
          |  .consume_badge = ${consume_badge},
          |  .connection_name = "${methodNamePrefix}"
          |}"""

    return ret
  }

  def vm_cross_vm_connections(glueCodeMethods: ISZ[ST],
                              connections: ISZ[ST]): ST = {
    val numConnections: Z = connections.size

    val ret: ST = st"""/*
                      | * Copyright 2019, Data61
                      | * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
                      | * ABN 41 687 119 230.
                      | *
                      | * This software may be distributed and modified according to the terms of
                      | * the BSD 2-Clause license. Note that NO WARRANTY is provided.
                      | * See "LICENSE_BSD2.txt" for details.
                      | *
                      | * @TAG(DATA61_BSD)
                      | */
                      |
                      |#include <autoconf.h>
                      |#include <camkes.h>
                      |#include <vmlinux.h>
                      |#include <sel4vm/guest_vm.h>
                      |
                      |#include <sel4vmmplatsupport/drivers/cross_vm_connection.h>
                      |#include <sel4vmmplatsupport/drivers/pci_helper.h>
                      |#include <pci/helper.h>
                      |
                      |#ifdef CONFIG_PLAT_QEMU_ARM_VIRT
                      |#define CONNECTION_BASE_ADDRESS 0xDF000000
                      |#else
                      |#define CONNECTION_BASE_ADDRESS 0x3F000000
                      |#endif
                      |
                      |#define NUM_CONNECTIONS ${numConnections}
                      |static struct camkes_crossvm_connection connections[NUM_CONNECTIONS];
                      |
                      |${(glueCodeMethods, "\n")}
                      |
                      |static int consume_callback(vm_t *vm, void *cookie) {
                      |    consume_connection_event(vm, (seL4_Word) cookie, true);
                      |    return 0;
                      |}
                      |
                      |void init_cross_vm_connections(vm_t *vm, void *cookie) {
                      |    ${(connections, ";\n\n")};
                      |
                      |    for (int i = 0; i < NUM_CONNECTIONS; i++) {
                      |        if (connections[i].consume_badge != -1) {
                      |            int err = register_async_event_handler(connections[i].consume_badge, consume_callback, (void *)connections[i].consume_badge);
                      |            ZF_LOGF_IF(err, "Failed to register_async_event_handler for init_cross_vm_connections.");
                      |        }
                      |    }
                      |
                      |    cross_vm_connections_init(vm, CONNECTION_BASE_ADDRESS, connections, ARRAY_SIZE(connections));
                      |}
                      |
                      |DEFINE_MODULE(cross_vm_connections, NULL, init_cross_vm_connections)"""
    return ret
  }

  def settings_cmake_entries(): ISZ[ST] = {
    val ret: ST = st"""# Add virtual PCI device to VMM for registering cross component connectors as
                      |# devices on the PCI bus.
                      |set(VmPCISupport ON CACHE BOOL "" FORCE)
                      |
                      |# Disable libusb from being compiled.
                      |set(LibUSB OFF CACHE BOOL "" FORCE)
                      |
                      |# Enables the option for the VM to open and load a seperate initrd file
                      |set(VmInitRdFile ON CACHE BOOL "" FORCE)
                      |
                      |# Enable virtio console vmm module
                      |set(VmVirtioConsole ON CACHE BOOL "" FORCE)
                      |
                      |# Make VTimers see absolute time rather than virtual time.
                      |set(KernelArmVtimerUpdateVOffset OFF CACHE BOOL "" FORCE)
                      |
                      |# Don't trap WFI or WFE instructions in a VM.
                      |set(KernelArmDisableWFIWFETraps ON CACHE BOOL "" FORCE)
                      |
                      |if("$${PLATFORM}" STREQUAL "qemu-arm-virt")
                      |    set(KernelArmCPU cortex-a53 CACHE STRING "" FORCE)
                      |    set(KernelArmExportPCNTUser ON CACHE BOOL "" FORCE)
                      |    set(KernelArmExportPTMRUser ON CACHE BOOL "" FORCE)
                      |
                      |    set(MIN_QEMU_VERSION "4.0.0")
                      |    execute_process(COMMAND $${QEMU_BINARY} -version OUTPUT_VARIABLE QEMU_VERSION_STR)
                      |    string(
                      |        REGEX
                      |            MATCH
                      |            "[0-9](\\.[0-9])+"
                      |            QEMU_VERSION
                      |            $${QEMU_VERSION_STR}
                      |    )
                      |    if("$${QEMU_VERSION}" VERSION_LESS "$${MIN_QEMU_VERSION}")
                      |        message(WARNING "Warning: qemu version should be at least $${MIN_QEMU_VERSION}")
                      |    endif()
                      |
                      |endif()"""
    return ISZ(ret)
  }

  def vm_cmake_var(varName: String, varValue: String): ST = {
    return st"-D${varName}=${varValue}"
  }

  def vm_cmake_DeclareCamkesArmVM(threadId: String,
                                  connectionFilenames: ISZ[String],
                                  libNames: ISZ[String]): ST = {
    return st"""DeclareCamkesARMVM(
               |  ${threadId}
               |  EXTRA_SOURCES ${(connectionFilenames, "\n")}
               |  EXTRA_LIBS ${(libNames, "\n")})"""
  }

  def vm_cmakelists(vmIDs: ISZ[String],
                    vmCamkesComponents: ISZ[ST],
                    cmakeVars: ISZ[ST]): ST = {

    val ret: ST =st"""${CMakeTemplate.CMAKE_MINIMUM_REQUIRED_VERSION}
                     |
                     |project(arm-vm C)
                     |
                     |includeGlobalComponents()
                     |find_package(camkes-vm REQUIRED)
                     |find_package(camkes-vm-images REQUIRED)
                     |find_package(camkes-arm-vm REQUIRED)
                     |find_package(camkes-vm-linux REQUIRED)
                     |camkes_arm_vm_import_project()
                     |
                     |include($${CAMKES_VM_LINUX_HELPERS_PATH})
                     |include($${CAMKES_VM_LINUX_MODULE_HELPERS_PATH})
                     |include($${CAMKES_VM_LINUX_SOURCE_HELPERS_PATH})
                     |include(ExternalProject)
                     |include(external-project-helpers)
                     |
                     |#MESSAGE("KernelARMPlatform = $${KernelARMPlatform}")
                     |#MESSAGE("CAMKES_ARM_VM_DIR = $${CAMKES_ARM_VM_DIR}")
                     |#MESSAGE("CAMKES_VM_IMAGES_DIR = $${CAMKES_VM_IMAGES_DIR}")
                     |#MESSAGE("CAMKES_VM_LINUX_DIR = $${CAMKES_VM_LINUX_DIR}")
                     |#MESSAGE("CMAKE_CURRENT_BINARY_DIR = $${CMAKE_CURRENT_BINARY_DIR}")
                     |#MESSAGE("CMAKE_CURRENT_SOURCE_DIR = $${CMAKE_CURRENT_SOURCE_DIR}")
                     |#MESSAGE("CMAKE_C_COMPILER = $${CMAKE_C_COMPILER}")
                     |#MESSAGE("BASE_C_FLAGS = $${BASE_C_FLAGS}")
                     |
                     |if("$${KernelARMPlatform}" STREQUAL "qemu-arm-virt" AND (NOT ${USE_PRECONFIGURED_ROOTFS}))
                     |    MESSAGE("Not using preconfigured rootfs, will download a vanilla linux image instead")
                     |
                     |    set(cpp_flags "-DKERNELARMPLATFORM_QEMU-ARM-VIRT")
                     |    set(linux_repo "https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git")
                     |    set(linux_tag "v4.9.189")
                     |    set(linux_arch "arm64")
                     |    set(linux_cross_compile "aarch64-linux-gnu-")
                     |    set(rootfs_file "$${CAMKES_VM_IMAGES_DIR}/$${KernelARMPlatform}/rootfs.cpio.gz")
                     |    # Checkout and configure linux to build crossvm module
                     |    ExternalProject_Add(
                     |        checkout_linux
                     |        GIT_REPOSITORY $${linux_repo}
                     |        GIT_TAG $${linux_tag}
                     |        GIT_SHALLOW TRUE
                     |        GIT_PROGRESS TRUE
                     |        USES_TERMINAL_DOWNLOAD TRUE
                     |        BUILD_COMMAND ""
                     |        INSTALL_COMMAND ""
                     |        CONFIGURE_COMMAND ""
                     |        SOURCE_DIR $${CMAKE_CURRENT_BINARY_DIR}/linux_out
                     |    )
                     |    Message("Done cloning $${linux_repo}")
                     |
                     |    # Linux config and symvers are to be copied to unpacked archive
                     |    set(linux_config "$${CAMKES_VM_IMAGES_DIR}/$${KernelARMPlatform}/linux_configs/config")
                     |    set(linux_symvers "$${CAMKES_VM_IMAGES_DIR}/$${KernelARMPlatform}/linux_configs/Module.symvers")
                     |    # Configure unpacked archive with config and symvers
                     |    ConfigureLinux(
                     |        $${CMAKE_CURRENT_BINARY_DIR}/linux_out
                     |        $${linux_config}
                     |        $${linux_symvers}
                     |        configure_vm_linux
                     |        ARCH
                     |        $${linux_arch}
                     |        CROSS_COMPILE
                     |        $${linux_cross_compile}
                     |        DEPENDS
                     |        checkout_linux
                     |    )
                     |
                     |    # Compile CrossVM Dataport Module
                     |    DefineLinuxModule(
                     |        $${CAMKES_VM_LINUX_DIR}/camkes-linux-artifacts/camkes-linux-modules/camkes-connector-modules/connection
                     |        output_module
                     |        output_module_target
                     |        KERNEL_DIR
                     |        $${CMAKE_CURRENT_BINARY_DIR}/linux_out
                     |        ARCH
                     |        $${linux_arch}
                     |        CROSS_COMPILE
                     |        $${linux_cross_compile}
                     |        DEPENDS
                     |        checkout_linux
                     |        configure_vm_linux
                     |    )
                     |
                     |    AddFileToOverlayDir(
                     |        "connection.ko"
                     |        $${output_module}
                     |        "lib/modules/4.14.87/kernel/drivers/vmm"
                     |        overlay
                     |        DEPENDS
                     |        output_module_target
                     |    )
                     |
                     |    # Add script to initialise dataport module
                     |    AddFileToOverlayDir(
                     |        "S90crossvm_module_init"
                     |        $${CMAKE_CURRENT_SOURCE_DIR}/overlay_files/init_scripts/cross_vm_module_init
                     |        "etc/init.d"
                     |        overlay
                     |    )
                     |else()
                     |    MESSAGE("Using pre-configured rootfs")
                     |
                     |    # Use pre-configured rootfs file with crossvm modules and apps installed
                     |    set(rootfs_file "$${CAMKES_VM_IMAGES_DIR}/$${KernelARMPlatform}/rootfs_crossvm.cpio.gz")
                     |endif()
                     |
                     |# Complile CrossVM Event Apps ${(vmIDs, " and ")}
                     |foreach(item IN ITEMS ${(vmIDs, " ")})
                     |    ExternalProject_Add(
                     |        $${item}
                     |        SOURCE_DIR
                     |        $${CMAKE_CURRENT_SOURCE_DIR}/apps/$${item}
                     |        BINARY_DIR
                     |        $${CMAKE_CURRENT_BINARY_DIR}/$${item}
                     |        INSTALL_COMMAND
                     |        ""
                     |        BUILD_ALWAYS
                     |        ON
                     |        EXCLUDE_FROM_ALL
                     |        CMAKE_ARGS
                     |        -DCMAKE_C_COMPILER=$${CMAKE_C_COMPILER}
                     |        ${(cmakeVars, "\n")}
                     |        -DCMAKE_C_FLAGS=$${BASE_C_FLAGS}
                     |    )
                     |
                     |    AddExternalProjFilesToOverlay(
                     |        $${item}
                     |        $${CMAKE_CURRENT_BINARY_DIR}/$${item}
                     |        overlay
                     |        "usr/bin"
                     |        FILES
                     |        $${item}
                     |    )
                     |endforeach()
                     |
                     |Message("Done compiling CrossVM Event Apps for ${(vmIDs, " and ")}")
                     |
                     |
                     |# Overwrite inittab file for using the virtio console hvc0.
                     |AddFileToOverlayDir(
                     |    "inittab"
                     |    $${CMAKE_CURRENT_SOURCE_DIR}/overlay_files/init_scripts/inittab_hvc0
                     |    "etc"
                     |    overlay
                     |)
                     |
                     |# Use initrd with crossvm kernel module and setup already included.
                     |# Construct new rootfs
                     |AddOverlayDirToRootfs(
                     |    overlay
                     |    $${rootfs_file}
                     |    "buildroot"
                     |    "rootfs_install"
                     |    output_overlayed_rootfs_location
                     |    rootfs_target
                     |    GZIP
                     |)
                     |
                     |AddToFileServer("linux-initrd-vm-client" $${output_overlayed_rootfs_location} DEPENDS rootfs_target)
                     |
                     |# Add linux kernel image to file server
                     |AddToFileServer("linux" "$${CAMKES_VM_IMAGES_DIR}/$${KernelARMPlatform}/linux")
                     |
                     |DefineCAmkESVMFileServer()
                     |
                     |CAmkESAddImportPath($${CMAKE_CURRENT_SOURCE_DIR}/$${KernelARMPlatform}/)
                     |
                     |# Define our VM Component with our cross vm dataports glue code
                     |${(vmCamkesComponents, "\n\n")}
                     |
                     |CAmkESAddCPPInclude($${CAMKES_ARM_VM_DIR}/components/VM)"""
    return ret
  }

  def vm_qemu_arm_virt_devices_camkes(componentIDs: ISZ[String]): ST = {
    assert(componentIDs.size <= 2, "Currently only expecting two VMs (e.g. sender/receiver")

    var i = 1
    val entries: ISZ[ST] = componentIDs.map(componentID => {
      val vmid = s"VM${i}"
      i = i + 1
      st"""${componentID}.linux_address_config = {
          |  "linux_ram_base" : VAR_STRINGIZE(${vmid}_RAM_BASE),
          |  "linux_ram_paddr_base" : VAR_STRINGIZE(${vmid}_RAM_BASE),
          |  "linux_ram_size" : VAR_STRINGIZE(${vmid}_RAM_SIZE),
          |  "linux_ram_offset" : VAR_STRINGIZE(VM_RAM_OFFSET),
          |  "dtb_addr" : VAR_STRINGIZE(${vmid}_DTB_ADDR),
          |  "initrd_max_size" : VAR_STRINGIZE(VM_INITRD_MAX_SIZE),
          |  "initrd_addr" : VAR_STRINGIZE(${vmid}_INITRD_ADDR),
          |};
          |${componentID}.linux_image_config = {
          |  "linux_bootcmdline" : "console=hvc0 nosmp rw debug loglevel=8 pci=nomsi,realloc=off,bios initcall_blacklist=clk_disable_unused",
          |  "linux_stdout" : "hvc0",
          |  "dtb_name" : "",
          |  "initrd_name" : "linux-initrd-vm-client",
          |};
          |${componentID}.mmios = [
          |  "0x8040000:0x1000:12", // Interrupt Controller Virtual CPU interface (Virtual Machine view)
          |];
          |${componentID}.untyped_mmios = [
          |  ${vmid}_RAM_MMIOS_BASE  // RAM
          |];
          |${componentID}.irqs = [];
          |${componentID}.dtb = dtb([{}]);
          |"""
    })
    val ret: ST = st"""/*
                      | * Copyright 2020, Data61
                      | * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
                      | * ABN 41 687 119 230.
                      | *
                      | * This software may be distributed and modified according to the terms of
                      | * the BSD 2-Clause license. Note that NO WARRANTY is provided.
                      | * See "LICENSE_BSD2.txt" for details.
                      | *
                      | * @TAG(DATA61_BSD)
                      | */
                      |
                      |#include <configurations/vm.h>
                      |#define VM_RAM_OFFSET      0x00000000
                      |#define VM_INITRD_MAX_SIZE 0x3200000 // 50 MB
                      |
                      |#define VM1_RAM_BASE       0x48000000
                      |#define VM1_RAM_MMIOS_BASE "0x48000000:27"
                      |#define VM1_RAM_SIZE       0x8000000
                      |#define VM1_DTB_ADDR       0x4f000000  // VM1_RAM_BASE + 0x7000000
                      |#define VM1_INITRD_ADDR    0x4d700000  // VM1_DTB_ADDR - VM_INITRD_MAX_SIZE
                      |
                      |#define VM2_RAM_BASE       0x50000000
                      |#define VM2_RAM_MMIOS_BASE "0x50000000:27"
                      |#define VM2_RAM_SIZE       0x8000000
                      |#define VM2_DTB_ADDR       0x57000000  // VM2_RAM_BASE + 0x7000000
                      |#define VM2_INITRD_ADDR    0x55700000  // VM2_DTB_ADDR - VM_INITRD_MAX_SIZE
                      |
                      |assembly {
                      |  composition {}
                      |  configuration {
                      |    ${(entries, "\n\n")}
                      |  }
                      |}"""
    return ret
  }

  def vm_exynos5422_devices_camkes(componentIDs: ISZ[String]): ST = {
    assert(componentIDs.size <= 2, "Currently only expecting two VMs (e.g. sender/receiver")

    var i = 1
    val entries: ISZ[ST] = componentIDs.map((componentId: String) => {
      val vmid = s"VM${i}"
      i = i + 1
      st"""${componentId}.linux_address_config = {
          |  "linux_ram_base" : VAR_STRINGIZE(${vmid}_RAM_BASE),
          |  "linux_ram_paddr_base" : VAR_STRINGIZE(${vmid}_RAM_BASE),
          |  "linux_ram_size" : VAR_STRINGIZE(${vmid}_RAM_SIZE),
          |  "linux_ram_offset" : VAR_STRINGIZE(VM_RAM_OFFSET),
          |  "dtb_addr" : VAR_STRINGIZE(${vmid}_DTB_ADDR),
          |  "initrd_max_size" : VAR_STRINGIZE(VM_INITRD_MAX_SIZE),
          |  "initrd_addr" : VAR_STRINGIZE(${vmid}_INITRD_ADDR),
          |};
          |${componentId}.linux_image_config = {
          |  "linux_bootcmdline" : "console=hvc0 root=/dev/ram0 nosmp rw debug loglevel=8 pci=nomsi initcall_blacklist=clk_disable_unused",
          |  "linux_stdout" : "hvc0",
          |  "dtb_name" : "",
          |  "initrd_name" : "linux-initrd-vm-client",
          |};
          |${componentId}.mmios = [
          |  "0x10000000:0x1000:12", // CHIP ID
          |  "0x10486000:0x1000:12"  // VCPU
          |];
          |${componentId}.untyped_mmios = [
          |  ${vmid}_RAM_MMIOS_BASE  // RAM
          |];
          |${componentId}.irqs = [];
          |${componentId}.dtb = dtb([{}]);"""
    })
    val ret: ST = st"""/*
                      | * Copyright 2020, Data61
                      | * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
                      | * ABN 41 687 119 230.
                      | *
                      | * This software may be distributed and modified according to the terms of
                      | * the BSD 2-Clause license. Note that NO WARRANTY is provided.
                      | * See "LICENSE_BSD2.txt" for details.
                      | *
                      | * @TAG(DATA61_BSD)
                      | */
                      |
                      |#include <configurations/vm.h>
                      |
                      |#define VM_RAM_OFFSET 0
                      |#define VM_INITRD_MAX_SIZE 0x1900000 // 25 MB
                      |
                      |
                      |#define VM1_RAM_BASE       0x48000000
                      |#define VM1_RAM_MMIOS_BASE "0x48000000:27"
                      |#define VM1_RAM_SIZE       0x8000000
                      |#define VM1_DTB_ADDR       0x4f000000  // VM1_RAM_BASE + 0x7000000
                      |#define VM1_INITRD_ADDR    0x4d700000  // VM1_DTB_ADDR - VM_INITRD_MAX_SIZE
                      |
                      |#define VM2_RAM_BASE       0x50000000
                      |#define VM2_RAM_MMIOS_BASE "0x50000000:27"
                      |#define VM2_RAM_SIZE       0x8000000
                      |#define VM2_DTB_ADDR       0x57000000  // VM2_RAM_BASE + 0x7000000
                      |#define VM2_INITRD_ADDR    0x55700000  // VM2_DTB_ADDR - VM_INITRD_MAX_SIZE
                      |
                      |assembly {
                      |  composition {}
                      |  configuration {
                      |    ${(entries, "\n\n")}
                      |  }
                      |}"""
    return ret
  }

  def vm_overlay_scripts__init_scripts__cross_vm_module_init(): ST = {
    val ret: ST = st"""#!/bin/sh
                      |#
                      |# Copyright 2020, Data61
                      |# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
                      |# ABN 41 687 119 230.
                      |#
                      |# This software may be distributed and modified according to the terms of
                      |# the BSD 2-Clause license. Note that NO WARRANTY is provided.
                      |# See "LICENSE_BSD2.txt" for details.
                      |#
                      |# @TAG(DATA61_BSD)
                      |#
                      |
                      |insmod /lib/modules/4.14.87/kernel/drivers/vmm/connection.ko"""
    return ret
  }

  def vm_overlay_script__init_scripts__inittab_hvc0(): ST = {
    val ret: ST = st"""# @TAG(CUSTOM)
                      |# /etc/inittab
                      |#
                      |${StringTemplate.safeToEditCamkeComment()}
                      |#
                      |# Copyright (C) 2001 Erik Andersen <andersen@codepoet.org>
                      |#
                      |# Note: BusyBox init doesn't support runlevels.  The runlevels field is
                      |# completely ignored by BusyBox init. If you want runlevels, use
                      |# sysvinit.
                      |#
                      |# Format for each entry: <id>:<runlevels>:<action>:<process>
                      |#
                      |# id        == tty to run on, or empty for /dev/console
                      |# runlevels == ignored
                      |# action    == one of sysinit, respawn, askfirst, wait, and once
                      |# process   == program to run
                      |
                      |# Startup the system
                      |::sysinit:/bin/mount -t proc proc /proc
                      |::sysinit:/bin/mount -o remount,rw /
                      |::sysinit:/bin/mkdir -p /dev/pts /dev/shm
                      |::sysinit:/bin/mount -a
                      |::sysinit:/sbin/swapon -a
                      |null::sysinit:/bin/ln -sf /proc/self/fd /dev/fd
                      |null::sysinit:/bin/ln -sf /proc/self/fd/0 /dev/stdin
                      |null::sysinit:/bin/ln -sf /proc/self/fd/1 /dev/stdout
                      |null::sysinit:/bin/ln -sf /proc/self/fd/2 /dev/stderr
                      |::sysinit:/bin/hostname -F /etc/hostname
                      |# now run any rc scripts
                      |::sysinit:/etc/init.d/rcS
                      |
                      |# Put a getty on the serial port
                      |hvc0:2345:respawn:/sbin/getty -L 9600 hvc0
                      |
                      |# Stuff to do for the 3-finger salute
                      |#::ctrlaltdel:/sbin/reboot
                      |
                      |# Stuff to do before rebooting
                      |::shutdown:/etc/init.d/rcK
                      |::shutdown:/sbin/swapoff -a
                      |::shutdown:/bin/umount -a -r"""
    return ret
  }

  /* @param libPathNames should be path, libName pairs
   */
  def vm_cmakelists_app(processID: String,
                        libNames: ISZ[String],
                        addSubDirs: ISZ[ST]): ST = {

    val ret: ST = st"""${CMakeTemplate.CMAKE_MINIMUM_REQUIRED_VERSION}
                      |
                      |project(${processID} C)
                      |
                      |${CMakeTemplate.CMAKE_SET_CMAKE_C_STANDARD}
                      |
                      |${(addSubDirs, "\n\n")}
                      |
                      |add_executable(${processID} ${processID}.c)
                      |
                      |target_link_libraries(${processID}
                      |                      ${(libNames, "\n")}
                      |                      -static-libgcc -static)"""
    return ret
  }

  def vm_app_dummy(vmProcessId: String ,
                   includes: ISZ[String]): ST = {
    val _includes = includes.map((m: String) => st"#include ${m}")

    val ret: ST = st"""#include <stdio.h>
                      |#include <stdlib.h>
                      |#include <fcntl.h>
                      |#include <unistd.h>
                      |#include <assert.h>
                      |#include <string.h>
                      |
                      |#include <sys/types.h>
                      |#include <sys/stat.h>
                      |#include <sys/mman.h>
                      |#include <errno.h>
                      |
                      |${(_includes, "\n")}
                      |
                      |${StringTemplate.safeToEditComment()}
                      |
                      |int main(int argc, char *argv[]) {
                      |  printf("VM App ${vmProcessId} started\n");
                      |  return 0;
                      |}"""
    return ret
  }

  def makeDirVariable(s: String): String = {
    return s"${s}_DIR"
  }

  def cmakeReferenceVar(s: String): String = {
    return s"$${${s}}"
  }

  def setup_camkes_vm_Script(): ST = {
    val CAMKES_ARM_VM_DIR: String = "CAMKES_ARM_VM_DIR"

    val ret: ST = st"""#!/usr/bin/env bash
                      |
                      |set -o errexit -o pipefail -o noclobber
                      |
                      |if [ -n "$$1" ]; then
                      |    ${CAMKES_ARM_VM_DIR}=$$1
                      |else
                      |  CASE_HOME=$$HOME/CASE
                      |
                      |  if [[ ! -d "$${CASE_HOME}" ]]; then
                      |    echo "CASE home not found: $${CASE_HOME}"
                      |    exit 1
                      |  fi
                      |
                      |  ${CAMKES_ARM_VM_DIR}=$${CASE_HOME}/camkes-arm-vm
                      |fi
                      |
                      |if [[ -d "$${${CAMKES_ARM_VM_DIR}}" ]]; then
                      |  read -p "The following directory already exists, replace $${${CAMKES_ARM_VM_DIR}} [Y|y]? " -n 1 -r; echo
                      |  if [[ $$REPLY =~ ^[Yy]$$ ]]; then
                      |    rm -rf $${${CAMKES_ARM_VM_DIR}}
                      |  else
                      |    exit -1
                      |  fi
                      |fi
                      |
                      |mkdir $$${CAMKES_ARM_VM_DIR}
                      |cd $$${CAMKES_ARM_VM_DIR}
                      |
                      |echo "Cloning camkes arm"
                      |repo init -u https://github.com/SEL4PROJ/camkes-arm-vm-manifest.git --depth=1
                      |repo sync -j8
                      |
                      |echo "Cloning Kent's CAmkES"
                      |
                      |(cd projects && rm -rf camkes && git clone https://github.com/kent-mcleod/camkes.git -b kent/aadl)
                      |
                      |cd $$${CAMKES_ARM_VM_DIR}
                      |
                      |ln -sf projects/camkes/easy-settings.cmake
                      |"""
    return ret
  }

  def vm_init_macro_expansion(): ST = {
    val ret: ST =
      st"""// expanding: VM_INIT_DEF()
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
          |} linux_image_config;
          |// end of expansion: VM_INIT_DEF()"""
    return ret
  }
}
