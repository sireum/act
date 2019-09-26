// #Sireum

package org.sireum.aadl.act

import org.sireum._
import org.sireum.aadl.ir
import org.sireum.aadl.ir.Component

object StringTemplate{

  val SB_VERIFY: String = Util.cbrand("VERIFY")

  val MON_READ_ACCESS:String = Util.cbrand("MONITOR_READ_ACCESS")
  val MON_WRITE_ACCESS:String = Util.cbrand("MONITOR_WRITE_ACCESS")

  def tbInterface(macroName: String): ST = {
    val r : ST = st"""#ifdef ${macroName}
                     |#define ${macroName}
                     |
                     |#endif // ${macroName}
                     |"""
    return r
  }

  def tbTypeHeaderFile(macroName: String, typeHeaderFileName: String, defs: ISZ[ST], preventBadging: B): ST = {
    val badges: ST = if(preventBadging) {st""} else {st"""
                                                         |#define $MON_READ_ACCESS 111
                                                         |#define $MON_WRITE_ACCESS 222"""}
    val macroname = s"__${Util.cbrand("AADL")}_${typeHeaderFileName}__H"

    val body = st"""#ifndef ${macroname}
                   |#define ${macroname}
                   |
                   |#include <stdbool.h>
                   |#include <stdint.h>
                   |
                   |#ifndef ${SB_VERIFY}
                   |#include <stddef.h>
                   |#endif // ${SB_VERIFY}
                   |
                   |#define __${Util.cbrand("OS")}_CAMKES__${badges}
                   |
                   |#ifndef ${SB_VERIFY}
                   |#define MUTEXOP(OP)\
                   |if((OP) != 0) {\
                   |  fprintf(stderr,"Operation " #OP " failed in %s at %d.\n",__FILE__,__LINE__);\
                   |  *((int*)0)=0xdeadbeef;\
                   |}
                   |#else
                   |#define MUTEXOP(OP) OP
                   |#endif // ${SB_VERIFY}
                   |#ifndef ${SB_VERIFY}
                   |#define CALLBACKOP(OP)\
                   |if((OP) != 0) {\
                   |  fprintf(stderr,"Operation " #OP " failed in %s at %d.\n",__FILE__,__LINE__);\
                   |  *((int*)0)=0xdeadbeef;\
                   |}
                   |#else
                   |#define CALLBACKOP(OP) OP
                   |#endif // ${SB_VERIFY}
                   |
                   |${(defs, "\n\n")}
                   |
                   |#endif // ${macroname}
                   |"""
    return body
  }

  def tbMissingType() : ST = {
    return st"""// placeholder for unspecified types in the AADL model
               |typedef bool ${Util.MISSING_AADL_TYPE};"""
  }

  def tbMonReadWrite(typeName: String, dim: Z, monitorTypeHeaderFilename: String, typeHeaderFilename: String,
                     preventBadging: B): ST = {
    val read: ST = st"""*m = contents;
                       |return true;"""
    val mon_read: ST = if(preventBadging) { read } else {
      st"""if (mon_get_sender_id() != $MON_READ_ACCESS) {
          |  return false;
          |} else {
          |  ${read}}
          |}"""
    }

    val write: ST = st"""contents = *m;
                        |monsig_emit();
                        |return true;"""
    val mon_write: ST = if(preventBadging) { write } else {
      st"""bool mon_write(const $typeName * m) {
          |  if (mon_get_sender_id() != $MON_WRITE_ACCESS)  {
          |    return false;
          |  } else {
          |    ${write}
          |  }
          |}"""
    }

    val senderSig: ST = if(preventBadging) { st"" } else { st"""
                                                               |int mon_get_sender_id(void);""" }
    val r : ST =
      st"""#include "../../../../${Util.DIR_INCLUDES}/${typeHeaderFilename}.h"
          |#include "../${Util.DIR_INCLUDES}/${monitorTypeHeaderFilename}.h"
          |
          |${senderSig}int monsig_emit(void);
          |
          |static $typeName contents;
          |
          |bool mon_read($typeName * m) {
          |  ${mon_read}
          |}
          |
          |bool mon_write(const $typeName * m) {
          |  ${mon_write}
          |}"""
    return r
  }

  def tbEnqueueDequeue(typeName: String, dim: Z, monitorTypeHeaderFilename: String, typeHeaderFilename: String,
                       preventBadging: B): ST = {

    val mon_dequeue: ST = if(preventBadging) { st"" } else {
      st"""if (mon_get_sender_id() != $MON_READ_ACCESS) {
          |  return false;
          |} else """
    }

    val mon_enqueue: ST = if(preventBadging) { st"" } else {
      st"""if (mon_get_sender_id() != $MON_WRITE_ACCESS) {
          |    return false;
          |} else """
    }

    val r: ST =
      st"""#ifndef $SB_VERIFY
          |#include <stdio.h>
          |#endif // $SB_VERIFY
          |
          |#include "../../../../${Util.DIR_INCLUDES}/${typeHeaderFilename}.h"
          |#include "../${Util.DIR_INCLUDES}/${monitorTypeHeaderFilename}.h"
          |
          |int mon_get_sender_id(void);
          |int monsig_emit(void);
          |
          |${typeName} contents[${dim}];
          |static uint32_t front = 0;
          |static uint32_t length = 0;
          |
          |static bool is_full(void) {
          |  return length == ${dim};
          |}
          |
          |static bool is_empty(void) {
          |  return length == 0;
          |}
          |
          |bool mon_dequeue(${typeName} * m) {
          |  ${mon_dequeue}if (is_empty()) {
          |    return false;
          |  } else {
          |    *m = contents[front];
          |    front = (front + 1) % ${dim};
          |    length--;
          |    return true;
          |  }
          |}
          |
          |bool mon_enqueue(const ${typeName} * m) {
          |  ${mon_enqueue}if (is_full()) {
          |    return false;
          |  } else {
          |    contents[(front + length) % ${dim}] = *m;
          |    length++;
          |    monsig_emit();
          |    return true;
          |  }
          |}
          |"""
    return r
  }

  val AUX_C_SOURCES: String = "AUX_C_SOURCES"
  val AUX_C_INCLUDES: String = "AUX_C_INCLUDES"

  def cmakeList(projectName: String, rootServer: String, components: ISZ[ST], cmakeVersion: String,
                auxCSources: ISZ[String], hasConnectorDefs: B,
                hamrIncludeDirs: ISZ[String], hamrStaticLib: Option[String]
               ): ST = {

    val slangIncludes: ST = if(hamrIncludeDirs.nonEmpty) {
      st"""set(${Util.SLANG_INCLUDES_NAME}
          |  ${(hamrIncludeDirs, "\n")}
          |)
          |"""
    } else {
      st""
    }

    val slangLib: ST = if(hamrStaticLib.nonEmpty) {
      st"""set(${Util.SLANG_LIB_NAME} ${hamrStaticLib.get})
          """
    } else {
      st""
    }

    val aux:ST = if(auxCSources.nonEmpty) {
      st"""set(${AUX_C_SOURCES} ${(auxCSources, " ")})
          |set(${AUX_C_INCLUDES} ${Util.AUX_CODE_DIRECTORY_NAME}/includes)
          |"""
    } else { st"""""" }

    val connectors: ST = if(hasConnectorDefs) { st"""# add path to connector templates
                                                    |CAmkESAddTemplatesPath(../../../../components/templates/)
                                                    |"""
    } else { st"" }

    val r: ST =
      st"""cmake_minimum_required(VERSION ${cmakeVersion})
          |
          |project (${rootServer} C)
          |
          |${slangLib}
          |${slangIncludes}
          |${connectors}
          |${aux}
          |${(components, "\n\n")}
          |
          |DeclareCAmkESRootserver(${rootServer}.camkes)
          |"""
    return r
  }

  def cmakeComponent(componentName: String, sources: ISZ[String], includes: ISZ[String], hasAux: B,
                     hasHamrIncl: B, hasHamrLib: B): ST = {
    var srcs: ISZ[ST] = ISZ()
    if(hasAux) { srcs = srcs :+ st"$${${AUX_C_SOURCES}} " }

    var incls: ISZ[ST] = ISZ()
    if(hasAux) { incls = incls :+ st"$${${AUX_C_INCLUDES}} " }
    if(hasHamrIncl){ incls = incls :+ st"$${${Util.SLANG_INCLUDES_NAME}} "}

    val libs: ST = if(hasHamrLib) { st"LIBS $${${Util.SLANG_LIB_NAME}}"}
    else { st"" }

    val s: ST = if(sources.nonEmpty){
      st"""SOURCES ${(srcs, " ")}${(sources, " ")}"""
    } else{
      st""
    }
    val i: ST = if(includes.nonEmpty){
      st"""INCLUDES ${(incls, " ")}${(includes, " ")}"""
    } else{
      st""
    }

    val r: ST =
      st"""DeclareCAmkESComponent(${componentName}
          |  ${s}
          |  ${i}
          |  ${libs}
          |)"""
    return r
  }

  def configurationPriority(name: String, priority: Z): ST = {
    return st"${name}.priority = ${priority};"
  }

  def configurationControlStackSize(name: String, size: Z): ST = {
    return st"${name}._control_stack_size = ${size};"
  }

  def configurationStackSize(name: String, size: Z): ST = {
    return st"${name}._stack_size = ${size};"
  }

  val SEM_WAIT: String = Util.brand("dispatch_sem_wait")
  val SEM_POST: String = Util.brand("dispatch_sem_post")

  def componentTypeImpl(filename: String, includes: ISZ[ST], blocks: ISZ[ST],
                        preInitComments: ISZ[ST], runPreEntries: ISZ[ST], runLoopEntries: ISZ[ST],
                        isSporadic: B): ST = {
    val initialLock: ST = if(isSporadic) { st"" } else { st"""// Initial lock to await dispatch input.
                                                             |MUTEXOP(${SEM_WAIT}())"""}
    val ret:ST = st"""#include "../${Util.DIR_INCLUDES}/${filename}.h"
                     |${(includes, "\n")}
                     |#include <string.h>
                     |#include <camkes.h>
                     |
                     |${(blocks, "\n\n")}
                     |
                     |void pre_init(void) {
                     |  ${(preInitComments, "\n")}
                     |}
                     |
                     |/************************************************************************
                     | * int run(void)
                     | * Main active thread function.
                     | ************************************************************************/
                     |int run(void) {
                     |  ${(runPreEntries, "\n")}
                     |  ${initialLock}
                     |  for(;;) {
                     |    MUTEXOP(${SEM_WAIT}())
                     |
                     |    ${(runLoopEntries, "\n")}
                     |  }
                     |  return 0;
                     |}
                     |"""
    return ret
  }

  def componentInitializeEntryPoint(componentName: String, methodName: String): (ST, ST) = {
    val init: String = Util.brand(s"entrypoint_${componentName}_initializer")
    val ret: ST =
      st"""/************************************************************************
          | *  ${init}:
          | *
          | * This is the function invoked by an active thread dispatcher to
          | * call to a user-defined entrypoint function.  It sets up the dispatch
          | * context for the user-defined entrypoint, then calls it.
          | *
          | ************************************************************************/
          |void ${init}(const int64_t * in_arg) {
          |  ${methodName}((int64_t *) in_arg);
          |}"""
    val dummy = Util.brand("dummy")
    val runEntry: ST = st"""{
                           |  int64_t ${dummy};
                           |  ${init}(&${dummy});
                           |}"""
    return (ret, runEntry)
  }

  def cEventNotificiationHandler(handlerName: String, regCallback: String): ST = {
    val ret: ST =
      st"""static void ${handlerName}(void * unused) {
          |  MUTEXOP(${SEM_POST}())
          |  CALLBACKOP(${regCallback}(${handlerName}, NULL));
          |}"""
    return ret
  }

  def cRegCallback(handlerName: String, regCallback: String): ST = {
    val ret: ST = st"CALLBACKOP(${regCallback}(${handlerName}, NULL));"
    return ret
  }

  val VAR_PERIODIC_OCCURRED : String = Util.brand("occurred_periodic_dispatcher")
  val VAR_PERIODIC_TIME : String = Util.brand("time_periodic_dispatcher")
  val METHOD_PERIODIC_CALLBACK : String = Util.brand("timer_complete_callback")

  def periodicDispatchElems() : ST = {
    val ret = st"""static bool ${VAR_PERIODIC_OCCURRED};
                  |static int64_t ${VAR_PERIODIC_TIME};
                  |
                  |/************************************************************************
                  | * periodic_dispatcher_write_int64_t
                  | * Invoked from remote periodic dispatch thread.
                  | *
                  | * This function records the current time and triggers the active thread
                  | * dispatch from a periodic event.  Note that the periodic dispatch
                  | * thread is the *only* thread that triggers a dispatch, so we do not
                  | * mutex lock the function.
                  | *
                  | ************************************************************************/
                  |
                  |bool periodic_dispatcher_write_int64_t(const int64_t * arg) {
                  |    ${VAR_PERIODIC_OCCURRED} = true;
                  |    ${VAR_PERIODIC_TIME} = *arg;
                  |    MUTEXOP(${SEM_POST});
                  |    return true;
                  |}
                  |
                  |void ${METHOD_PERIODIC_CALLBACK}(void *_ UNUSED) {
                  |   // we want time in microseconds, not nanoseconds, so we divide by 1000.
                  |   int64_t ${VAR_PERIODIC_TIME} = ${Util.brand("timer_time()")} / 1000LL;
                  |   (void)periodic_dispatcher_write_int64_t(&${VAR_PERIODIC_TIME});
                  |   ${registerPeriodicCallback()}
                  |}
                  |"""
    return ret
  }

  def registerPeriodicCallback(): ST = {
    return st"CALLBACKOP(${Util.brand("timer_complete_reg_callback")}(${METHOD_PERIODIC_CALLBACK}, NULL));"
  }

  def drainPeriodicQueue(componentName: String, userEntrypoint: String): (ST, ST) = {
    val methodName = Util.brand(s"entrypoint_${componentName}_periodic_dispatcher")

    val impl = st"""void ${methodName}(const int64_t * in_arg) {
                   |  ${userEntrypoint}((int64_t *) in_arg);
                   |}"""

    val drain = st"""if(${VAR_PERIODIC_OCCURRED}){
                    |  ${VAR_PERIODIC_OCCURRED} = false;
                    |  ${methodName}(&${VAR_PERIODIC_TIME});
                    |}"""
    return (impl, drain)
  }

  def hamrIntialise(basePackageName: String, componentName: String): ST = {
    return st"""// initialise slang-embedded components/ports
               |${basePackageName}_${componentName}_App_initialise(SF seed);
               |"""
  }

  def hamrInitialiseEntrypoint(basePackageName: String, componentName: String): ST = {
    return st"""// call the component's initialise entrypoint
               |art_Bridge_EntryPoints_initialise_(SF ${basePackageName}_${componentName}_App_entryPoints(SF));
               |"""
  }

  def hamrGetArchId(basePackageName: String, c: ir.Component): String = {
    val n =  org.sireum.ops.ISZOps(c.identifier.name).foldLeft((r: String, s : String) => s"${r}_${s}", "")
    return s"${basePackageName}_Arch${n}"
  }

  def hamrRunLoopEntries(basePackageName: String, c: Component): ISZ[ST] = {
    //val cname = Util.nameToString(c.identifier)
    val cname = Util.getClassifier(c.classifier.get)
    return ISZ(st"transferIncomingDataToArt();", st"", st"${basePackageName}_${cname}_App_compute(SF);")
  }

  def hamrPayload(c : ir.Classifier, base: String) : String = {
    val r = StringUtil.replaceAll(StringUtil.replaceAll(c.name, "::", "_"), ".", "_")
    return s"${base}_${r}_Payload"
  }

  def hamrEnqueue(srcPort: ir.FeatureEnd, dstPort: ir.FeatureEnd, basePackageName: String, typeMap: HashSMap[String, ir.Component]) : ST = {
    val srcEnqueue = Util.brand(s"${Util.getLastName(srcPort.identifier)}_enqueue")
    val camkesType = Util.getClassifierFullyQualified(dstPort.classifier.get)
    val ct = Util.getMonitorWriterParamName(typeMap.get(camkesType).get)

    val payload = hamrPayload(dstPort.classifier.get, basePackageName)
    return st"""${payload} payload = (${payload}) d;
               |
               |// convert Slang type to CAmkES type
               |${ct} val;
               |convertTo_${ct}(payload, &val);
               |
               |// deliver payload via CAmkES
               |${srcEnqueue}(&val);"""
  }

  def hamrDrainQueue(f: ir.FeatureEnd, basePackageName: String, typeMap: HashSMap[String, ir.Component]): ST = {
    val portName = Util.getLastName(f.identifier)
    val camkesId = s"${portName}_id"
    val dequeue = Util.brand(s"${portName}_dequeue")
    val camkesType = Util.getClassifierFullyQualified(f.classifier.get)
    val ct = Util.getMonitorWriterParamName(typeMap.get(camkesType).get)

    val payload = s"${hamrPayload(f.classifier.get, basePackageName)}"

    return st"""{
               |  ${ct} val;
               |  while(${dequeue}((${ct} *) &val)){
               |    // convert to slang payload
               |    DeclNew${payload}(payload);
               |    convertTo_${payload}(val, &payload);
               |
               |    // deliver payload via ART
               |    camkes_In_Port_Data_Transfer(${camkesId}, (art_DataContent) &payload);
               |  }
               |}
               |"""
  }

  def hamrIPC(numPorts: Z, basePackageName: String): (ST, ST) = {
    val impl = st"""#include <all.h>
                   |#include <ipc.h>
                   |
                   |static union Option_8E9F45 camkes_buffer[${numPorts}] = { 0 };
                   |
                   |Z ${basePackageName}_SharedMemory_create(STACK_FRAME Z id) {
                   |  DeclNewNone_964667(t_0);
                   |  None_964667_apply(CALLER &t_0);
                   |  Type_assign((camkes_buffer + id), (&t_0), sizeof(union Option_8E9F45));
                   |
                   |  return -1;
                   |}
                   |
                   |void ${basePackageName}_SharedMemory_receive(STACK_FRAME art_DataContent result, Z port) {
                   |  printf("${basePackageName}_SharedMemory_receive called with port %i -- NOT IMPLEMENTED\n", port);
                   |}
                   |
                   |void ${basePackageName}_SharedMemory_receiveAsync(STACK_FRAME Option_8E9F45 result, Z port) {
                   |  union Option_8E9F45 p = camkes_buffer[port];
                   |
                   |  if (p.type == TSome_D29615) {
                   |      Type_assign(result, &p, sizeOf((Type) &p));
                   |      memset(camkes_buffer + port, 0, sizeof(union Option_8E9F45));
                   |  } else {
                   |      result->type = TNone_964667;
                   |  }
                   |}
                   |
                   |Unit ${basePackageName}_SharedMemory_send(STACK_FRAME Z destid, Z port, art_DataContent d) {
                   |  printf("${basePackageName}_SharedMemory_send called with port %i -- NOT IMPLEMENTED\n", port);
                   |}
                   |
                   |B ${basePackageName}_SharedMemory_sendAsync(STACK_FRAME Z id, Z port, art_DataContent d) {
                   |  camkes_sendAsync(port, d);
                   |
                   |  return T;
                   |}
                   |
                   |Unit ${basePackageName}_SharedMemory_remove(STACK_FRAME Z id) {
                   |  printf("${basePackageName}_SharedMemory_remove called with %i -- NOT IMPLEMENTED\n", id);
                   |}
                   |
                   |Unit ${basePackageName}_Process_sleep(STACK_FRAME Z n) {}
                   |
                   |void camkes_In_Port_Data_Transfer (Z port, art_DataContent d) {
                   |  union Option_8E9F45 p = camkes_buffer[port];
                   |  camkes_buffer[port].type = TSome_D29615;
                   |  Type_assign(&(camkes_buffer[port].Some_D29615.value), d, sizeOf((Type) d));
                   |}
                   |"""

    val header = st"""#ifndef IPC_H
                     |#define IPC_H
                     |#include <all.h>
                     |
                     |static const int seed = 1;
                     |
                     |void camkes_In_Port_Data_Transfer (Z port, art_DataContent d);
                     |
                     |void camkes_sendAsync(Z port, art_DataContent d);
                     |
                     |#endif"""
    return (header, impl)
  }

  def ifEsleHelper(options: ISZ[(ST, ST)], optElse: Option[ST]): ST = {
    val first: Option[(ST, ST)] = if(options.size > 0) { Some(options(0)) } else { None() }
    val rest: ISZ[(ST, ST)] = if(options.size > 1) { org.sireum.ops.ISZOps(options).drop(1) } else { ISZ() }
    return ifElseST(first, rest, optElse)
  }

  def ifElseST(ifbranch: Option[(ST, ST)], elsifs: ISZ[(ST, ST)], els: Option[ST]): ST = {
    val e = elsifs.map((x: (ST, ST)) => st"""else if(${x._1}) {
                                |  ${x._2}
                                |}
                                |""")
    var body = st""

    if(ifbranch.nonEmpty) {
      body = st"""if(${ifbranch.get._1}) {
                 |  ${ifbranch.get._2}
                 |}"""
    }

    if(elsifs.nonEmpty) {
      val ei = elsifs.map((x: (ST, ST)) => st"""else if(${x._1}) {
                                               |  ${x._2}
                                               |}""")
      body = st"""${body}
                 |${ei}"""
    }

    if(els.nonEmpty) {
      if(body.render.size > 0) {
        body = st"""${body}
                   |else {
                   |  ${els.get}
                   |}"""
      } else {
        body = els.get
      }
    }

    return body
  }
}
