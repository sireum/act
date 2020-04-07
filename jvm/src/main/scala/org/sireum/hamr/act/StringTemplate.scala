// #Sireum

package org.sireum.hamr.act

import org.sireum._
import org.sireum.hamr.ir
import org.sireum.hamr.ir.Component

object StringTemplate {

  val SB_VERIFY: String = Util.cbrand("VERIFY")

  val MON_READ_ACCESS: String = Util.cbrand("MONITOR_READ_ACCESS")
  val MON_WRITE_ACCESS: String = Util.cbrand("MONITOR_WRITE_ACCESS")

  val SeqNumName: String = "seqNum"
  val SeqNumType: String = s"${SeqNumName}_t"
  
  def tbInterface(macroName: String): ST = {
    val r : ST = st"""#ifdef ${macroName}
                     |#define ${macroName}
                     |
                     |#endif // ${macroName}
                     |"""
    return r
  }

  def tbTypeHeaderFile(macroName: String, 
                       typeHeaderFileName: String,
                       includes: Option[ST],
                       defs: ISZ[ST], 
                       preventBadging: B): ST = {
    val badges: ST = if(preventBadging) {st""} else {st"""
                                                         |#define $MON_READ_ACCESS 111
                                                         |#define $MON_WRITE_ACCESS 222"""}
    val macroname = s"__${Util.cbrand("AADL")}_${typeHeaderFileName}__H"

    val body = st"""#ifndef ${macroname}
                   |#define ${macroname}
                   |
                   |#include <stdbool.h>
                   |#include <stdint.h>
                   |${includes}
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

  val receivedDataVar: String = "receivedData"
  
  def tbMonReadWrite(typeName: String, dim: Z, monitorTypeHeaderFilename: String, typeHeaderFilename: String,
                     preventBadging: B): ST = {
    val read: ST = st"""*m = contents;
                       |return ${receivedDataVar};"""
    
    val mon_read: ST = if(preventBadging) { 
      read 
    } else {
      st"""if (mon_get_sender_id() != $MON_READ_ACCESS) {
          |  return false;
          |} else {
          |  ${read}}
          |}"""
    }

    val write: ST = st"""${receivedDataVar} = true;
                        |contents = *m;
                        |monsig_emit();
                        |return ${receivedDataVar};"""
    val mon_write: ST = if(preventBadging) { 
      write 
    } else {
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
      st"""#include ${typeHeaderFilename}
          |#include "../${Util.DIR_INCLUDES}/${monitorTypeHeaderFilename}.h"
          |
          |${senderSig}int monsig_emit(void);
          |
          |static $typeName contents;
          |bool ${receivedDataVar} = false;
          |
          |bool mon_is_empty() {
          |  return !${receivedDataVar};
          |}
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
          |#include ${typeHeaderFilename}
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
          |bool mon_is_empty(void) {
          |  return length == 0;
          |}
          |
          |bool mon_dequeue(${typeName} * m) {
          |  ${mon_dequeue}if (mon_is_empty()) {
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

  def tbRaiseGetEvents(queueSize: Z, monitorTypeHeaderFilename: String, 
                       preventBadging: B): ST = {
  var r: ST = 
    st"""#include <camkes.h>
        |#include <stdio.h>
        |#include <string.h>
        |
        |int32_t num_events = 0;
        |
        |static inline void ignore_result(long long int unused_result) { (void) unused_result; }
        |
        |// Send interfaces
        |bool mon_send_enqueue(void) {
        |  int do_emit = 0;
        |  ignore_result(m_lock());
        |  if (num_events < ${queueSize}) {
        |    num_events++;
        |    do_emit = 1;
        |  }
        |  ignore_result(m_unlock());
        |  if (do_emit) {
        |    monsig_emit();
        |  }
        |  return true;
        |}
        |
        |// Receive interfaces 
        |bool mon_receive_is_empty(void) {
        |  return num_events == 0;
        |}
        |
        |bool mon_receive_dequeue(void) {
        |  ignore_result(m_lock());
        |  bool ret = false;
        |  if(num_events > 0){
        |    num_events--;
        |    ret = true;
        |  }
        |  ignore_result(m_unlock());
        |  return ret;
        |}
        |"""
    
    return r
  }
  
  def tbEnqueueDequeueIhor(typeName: String, dim: Z, monitorTypeHeaderFilename: String, typeHeaderFilename: String,
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
          |#include <camkes.h>
          |#include ${typeHeaderFilename}
          |#include "../${Util.DIR_INCLUDES}/${monitorTypeHeaderFilename}.h"
          |
          |struct queue {
          |    int head;
          |    int tail;
          |    int len;
          |    ${typeName} elt[${dim}];
          |} q = {.head=0, .tail=0, .len=0};
          |
          |static bool is_full(void) {
          |  return q.len == ${dim};
          |}
          |
          |bool mon_is_empty(void) {
          |  return q.len == 0;
          |}
          |
          |bool mon_receive_dequeue(${typeName} * m) {
          |  ${mon_dequeue}if (mon_is_empty()) {
          |    return false;
          |  } else {
          |    m_lock();
          |    *m = q.elt[q.tail];
          |    q.tail = (q.tail + 1) % ${dim};
          |    q.len--;
          |    m_unlock();
          |    return true;
          |  }
          |}
          |
          |bool mon_send_enqueue(const ${typeName} * m) {
          |  ${mon_enqueue}if (is_full()) {
          |    return false;
          |  } else {
          |    m_lock();
          |    q.elt[q.head] = *m;
          |    q.head = (q.head + 1) % ${dim};
          |    q.len++;
          |    m_unlock();
          |    monsig_emit();    
          |    return true;
          |  }
          |}
          |"""
    return r
  }
  
  def seqNumHeader(): ST = {
    return st"""#ifndef _SEQNUM_H_
               |#define _SEQNUM_H_
               |
               |// Typedef for seqNum to make it easy to change the type. Keep these consistent!
               |typedef uintmax_t seqNum_t;
               |#define SEQNUM_MAX UINTMAX_MAX
               |#define PRIseqNum PRIuMAX
               |
               |// DIRTY_SEQ_NUM is used to mark a sampling port message as dirty while it is
               |// being writen. DIRTY_SEQ_NUM is not a valid sequence number. Valid sequence
               |// numbers are from 0 to DIRTY_SEQ_NUM-1 is never a valid sequence number.
               |static const seqNum_t DIRTY_SEQ_NUM = SEQNUM_MAX;
               |
               |#endif"""
  }
  
  def sbSamplingPortGlobalVar(spi: SamplingPortInterface, f: ir.FeatureEnd): ST = {
    val portName = Util.getLastName(f.identifier)
    val globalVarName = s"${Util.brand(portName)}_seqNum"
    return st"$globalVarName"
  }
  
  def sbSamplingPortGlobalVarDecl(spi: SamplingPortInterface, f: ir.FeatureEnd): ST = {
    return st"${StringTemplate.SeqNumType} ${sbSamplingPortGlobalVar(spi, f)};"
  }

  def sbSamplingPortInterface(spi: SamplingPortInterface, f: ir.FeatureEnd): ST = {
    assert(f.category == ir.FeatureCategory.DataPort)

    val portName = Util.getLastName(f.identifier)
    val methodNamePrefix = Util.brand(portName)
    
    val ret: ST = f.direction match {
      case ir.Direction.In => st"bool ${methodNamePrefix}_read(${spi.sel4TypeName} * value);"
      case ir.Direction.Out => st"bool ${methodNamePrefix}_write(const ${spi.sel4TypeName} * value);"
      case _ => halt(s"Unexpected direction ${f.direction}")
    }
    return ret
  }
  
  def sbSamplingPortImplementation(spi: SamplingPortInterface, f: ir.FeatureEnd): ST = {
    assert(f.category == ir.FeatureCategory.DataPort)

    val sharedDataVarName = Util.brand(Util.getLastName(f.identifier))
    val globalVarName = sbSamplingPortGlobalVar(spi, f)

    val ret: ST = f.direction match {
      case ir.Direction.In => st"""bool ${sharedDataVarName}_read(${spi.sel4TypeName} * value) {
                                  |  ${StringTemplate.SeqNumType} new_seqNum;
                                  |  if ( read_${spi.name}(${sharedDataVarName}, value, &new_seqNum) ) {
                                  |    ${globalVarName} = new_seqNum;
                                  |    return true;
                                  |  } else {
                                  |    return false;
                                  |  } 
                                  |}"""
        
      case ir.Direction.Out => st"""bool ${sharedDataVarName}_write(const ${spi.sel4TypeName} * value) {
                                   |  return write_${spi.name}(${sharedDataVarName}, value, &${globalVarName});
                                   |}"""
        
      case _ => halt(s"Unexpected direction ${f.direction}")
    }
    return ret
  }
  
  def sbSamplingPortConfigurationEntry(componentVarName: String, spi: SamplingPortInterface, f: ir.FeatureEnd): ST = {
    val portName = Util.getLastName(f.identifier)
    
    val ret: ST = f.direction match {
      case ir.Direction.In => st"""${componentVarName}.${portName}_access = "R";"""
      case ir.Direction.Out => st"""${componentVarName}.${portName}_access = "W";"""
      case _ => halt(s"Unexpected direction ${f.direction}")
    }
    return ret
  }
  
  def sbAccessRestrictionEntry(componentName: String, varName: String, permission: String): ST = {
    return st"""${componentName}.${varName}_access = "${permission}";"""
  }
  
  val AUX_C_SOURCES: String = "AUX_C_SOURCES"
  val AUX_C_INCLUDES: String = "AUX_C_INCLUDES"

  def cmakeHamrIncludesName(instanceName: String): String = {
    return s"${Util.HAMR_INCLUDES_NAME}_${instanceName}"
  }

  def cmakeHamrIncludes(instanceName: String, hamrIncludeDirs: ISZ[String]): ST = {
    val includesName = cmakeHamrIncludesName(instanceName)
    return st"""set(${includesName}
               |  ${(hamrIncludeDirs, "\n")}
               |)"""
  }

  def cmakeAuxSources(auxCSources: ISZ[String], auxHDirectories: ISZ[String]): ST = {
    return st"""set(${AUX_C_SOURCES} ${(auxCSources, " ")})
               |set(${AUX_C_INCLUDES} ${(auxHDirectories, " ")})"""
  }

  def cmakeHamrLibName(instanceName: String): String = {
    return s"${Util.HAMR_LIB_NAME}_${instanceName}"
  }
  
  def cmakeHamrLib(instanceName: String,
                   hamrStaticLib: String): ST = {
    val libName = cmakeHamrLibName(instanceName)
    return st"set(${libName} ${hamrStaticLib})"
  }

  def cmakeHamrExecuteProcess(): ST = {
    return st"""execute_process(COMMAND bash -c "$${CMAKE_CURRENT_LIST_DIR}/bin/compile-hamr-lib.sh")"""
  }

  def cmakeLists(cmakeVersion: String, rootServer: String, entries: ISZ[ST]): ST = {
    return st"""cmake_minimum_required(VERSION ${cmakeVersion})
               |
               |project (${rootServer} C)
               |
               |add_definitions(-DCAMKES)
               |
               |${(entries, "\n\n")}
               |
               |DeclareCAmkESRootserver(${rootServer}.camkes)
               |"""
  }

  def cmakeComponent(componentName: String, 
                     sources: ISZ[String], 
                     includes: ISZ[String], 
                     hasAux: B,
                     hamrLib: Option[HamrLib]): ST = {
    var srcs: ISZ[ST] = ISZ()
    if(hasAux) { srcs = srcs :+ st"$${${AUX_C_SOURCES}} " }
    if(sources.nonEmpty) { srcs = srcs :+ st"""${(sources, " ")}""" }

    var incls: ISZ[ST] = ISZ()
    if(hasAux) { incls = incls :+ st"$${${AUX_C_INCLUDES}} " }
    if(hamrLib.nonEmpty){
      val hamrIncludeName = StringTemplate.cmakeHamrIncludesName(hamrLib.get.instanceName)  
      incls = incls :+ st"$${${hamrIncludeName}} "
    }
    if(includes.nonEmpty) { incls = incls :+ st"""${(includes, " ")}""" }

    val libs: ST = if(hamrLib.nonEmpty) { st"LIBS $${${StringTemplate.cmakeHamrLibName(hamrLib.get.instanceName)}}"}
    else { st"" }

    val r: ST =
      st"""DeclareCAmkESComponent(${componentName}
          |  SOURCES $srcs
          |  INCLUDES $incls
          |  ${libs}
          |)"""
    return r
  }
  
  def runCamkesScript(): ST = {
    val ret: ST = st"""#!/usr/bin/env bash

set -e

export SCRIPT_HOME=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
export PROJECT_HOME=$$( cd "$$( dirname "$$0" )/.." &> /dev/null && pwd )
cd $${PROJECT_HOME}


# location of camkes-projects directory
if [ -n "$$1" ]; then
    CAMKES_DIR=$$1
elif [ -d "/host/camkes-project" ]; then
    CAMKES_DIR="/host/camkes-project"
elif [ -d "$${HOME}/CASE/camkes" ]; then
    CAMKES_DIR="$${HOME}/CASE/camkes"
fi

if [[ -z "$$CAMKES_DIR" || ! -d "$${CAMKES_DIR}" ]]; then
    echo "Directory '$${CAMKES_DIR}' does not exist.  Please specify the location of your CAmkES project directory"
    exit -1
fi


# use the directory name for the CAmkES apps directory name 
HAMR_CAMKES_PROJ=$${PWD##*/}


CAMKES_APPS_DIR=$$CAMKES_DIR/projects/camkes/apps/$$HAMR_CAMKES_PROJ

# create a sym-link to the project in the CAmkES app directory
if [ ! -e "$${CAMKES_APPS_DIR}" ]; then
    ln -sv $$PROJECT_HOME $$CAMKES_APPS_DIR
fi


########################
# run CAmkES/seL4 build
########################

cd $$CAMKES_DIR

BUILD_DIR=build_$$HAMR_CAMKES_PROJ

rm -rf $$BUILD_DIR
mkdir $$BUILD_DIR
cd $$BUILD_DIR

../init-build.sh -DCAMKES_APP=$$HAMR_CAMKES_PROJ && ninja

########################
# simulate via QEMU
########################

./simulate                   
"""
    return ret
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

  def componentTypeImpl(filename: String, 
                        includes: ISZ[ST], 
                        blocks: ISZ[ST],
                        preInits: ISZ[ST],
                        postInits: ISZ[ST],
                        runPreEntries: ISZ[ST], 
                        runLoopEntries: ISZ[ST],
                        isSporadic: B): ST = {
    val initialLock: ST = if(isSporadic) { st"" } else { st"""// Initial lock to await dispatch input.
                                                             |MUTEXOP(${SEM_WAIT}())"""}
    
    val preInit: ST = if(preInits.nonEmpty) {
      st"""
          |void pre_init(void) {
          |  ${(preInits, "\n")}
          |}"""
    } else { st"" }
    
    val postInit: ST = if(postInits.nonEmpty) {
      st"""
          |void post_init(void){
          |  ${(postInits, "\n")}
          |}"""
    } else { st"" }
    
    val ret:ST = st"""#include "../${Util.DIR_INCLUDES}/${filename}.h"
                     |${(includes, "\n")}
                     |#include <string.h>
                     |#include <camkes.h>
                     |
                     |${(blocks, "\n\n")}
                     |${preInit}
                     |${postInit}
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
  val METHOD_PERIODIC_CALLBACK : String = s"${TimerUtil.componentNotificationName(None())}_callback"
  
  def periodicDispatchElems(componentId: String, timerHookedUp: B) : ST = {
    var h: String = s"${Util.brand("timer_time()")} / 1000LL"
    if(!timerHookedUp) {
      h = s"0; // ${h} -- timer connection disabled"
    }
    
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
                  |    MUTEXOP(${SEM_POST}());
                  |    return true;
                  |}
                  |
                  |void ${METHOD_PERIODIC_CALLBACK}(void *_ UNUSED) {
                  |   // we want time in microseconds, not nanoseconds, so we divide by 1000.
                  |   int64_t ${VAR_PERIODIC_TIME} = ${h};
                  |   (void)periodic_dispatcher_write_int64_t(&${VAR_PERIODIC_TIME});
                  |   ${registerPeriodicCallback()}
                  |}
                  |"""
    return ret
  }

  def registerPeriodicCallback(): ST = {
    val notificationName = TimerUtil.componentNotificationName(None())
    return st"CALLBACKOP(${notificationName}_reg_callback(${METHOD_PERIODIC_CALLBACK}, NULL));"
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

  def hamrGetInstanceName(basePackageName: String, c: Component): ST = {
    return st"${basePackageName}_${Util.getName(c.identifier)}"
  }
  
  def hamrIntialiseArchitecture(appName: String): ST = {
    return st"""// initialise slang-embedded components/ports
               |${appName}_initialiseArchitecture(SF_LAST);
               |"""
  }

  def hamrInitialiseEntrypoint(appName: String): ST = {
    return st"""// call the component's initialise entrypoint
               |${appName}_initialiseEntryPoint(SF_LAST);
               |"""
  }

  def hamrRunLoopEntries(appName: String): ISZ[ST] = {
    return ISZ(st"""// call the component's compute entrypoint
                   |${appName}_computeEntryPoint(SF_LAST);""")
  }

  def hamrSlangType(c : ir.Classifier, base: String) : String = {
    val r = StringUtil.replaceAll(StringUtil.replaceAll(c.name, "::", "_"), ".", "_")
    return s"${base}_${r}"
  }
  
  def hamrSlangPayloadType(c : ir.Classifier, base: String) : String = {
    val r = StringUtil.replaceAll(StringUtil.replaceAll(c.name, "::", "_"), ".", "_")
    return s"${base}_${r}_Payload"
  }

  def hamrIsEmptyUnconnected(methodName: String,
                             sel4IsEmptyMethodName: String): ST = {
    val ret: ST = st"""// FIXME: dummy implementation for unconnected incoming port
                      |B ${methodName}(STACK_FRAME_ONLY) {
                      |  return T;
                      |}"""
    return ret
  }

  def hamrIsEmpty(methodName: String,
                  sel4IsEmptyMethodName: String): ST = {
    val ret: ST = st"""B ${methodName}(STACK_FRAME_ONLY) {
                      |  return ${sel4IsEmptyMethodName}();
                      |}"""
    return ret
  }
  
  def hamrReceiveUnconnectedIncomingEventPort(methodName: String): ST = {
    val ret: ST = st"""// FIXME: dummy implementation for unconnected incoming port
                      |Unit ${methodName}(STACK_FRAME
                      |  Option_8E9F45 result) {
                      |  // FIXME: dummy implementation
                      | 
                      |  // put None in result
                      |  DeclNewNone_964667(none);
                      |  Type_assign(result, &none, sizeof(union Option_8E9F45));
                      |}"""
    return ret
  }

  def hamrReceiveIncomingEventPort(comment: ST,
                                   methodName: String,
                                   sel4ReadMethod: String): ST = {
    val ret: ST = st"""${comment}
                      |Unit ${methodName}(STACK_FRAME
                      |  Option_8E9F45 result) {
                      |  if(${sel4ReadMethod}()) {
                      |    // event port - ART requires an Empty payload be sent
                      |    DeclNewart_Empty(payload);
                      | 
                      |    // wrap it in Some and place in result
                      |    DeclNewSome_D29615(some);
                      |    Some_D29615_apply(STACK_FRAME &some, (art_DataContent) &payload);
                      |    Type_assign(result, &some, sizeof(union Option_8E9F45));
                      |  } else {
                      |    // put None in result
                      |    DeclNewNone_964667(none);
                      |    Type_assign(result, &none, sizeof(union Option_8E9F45));
                      |  }
                      |} """
    return ret
  }

  def hamrReceiveIncomingDataPort(comment: ST,
                                  methodName: String,
                                  sel4Type: String,
                                  slangPayloadType: String,
                                  sel4ReadMethod: String): ST = {
    val ret: ST = st"""${comment}
                      |Unit ${methodName}(STACK_FRAME
                      |  Option_8E9F45 result) {
                      |  ${sel4Type} val;
                      |  if(${sel4ReadMethod}((${sel4Type} *) &val)) {
                      |    // wrap payload in Some and place in result
                      |    DeclNewSome_D29615(some);
                      |    Some_D29615_apply(STACK_FRAME &some, (art_DataContent) &val);
                      |    Type_assign(result, &some, sizeof(union Option_8E9F45));
                      |  } else {
                      |    // put None in result
                      |    DeclNewNone_964667(none);
                      |    Type_assign(result, &none, sizeof(union Option_8E9F45));
                      |  }
                      |}
                      |"""
    return ret
  }

  def hamrSendUnconnectedOutgoingDataPort(methodName: String): ST = {
    val ret: ST = st"""// FIXME: dummy implementation for unconnected outgoing port
                      |Unit ${methodName}(STACK_FRAME 
                      |  art_DataContent d) {
                      |  // FIXME: dummy implementation
                      |}"""
    return ret
  }

  def hamrSendOutgoingDataPort(comment: ST,
                               methodName: String,
                               sel4Type: String,
                               slangPayloadType: String,
                               srcEnqueue: String
                              ): ST = {
    val ret: ST = st"""${comment}
                      |Unit ${methodName}(STACK_FRAME 
                      |  art_DataContent d) {
                      |  ${srcEnqueue}(d);
                      |}"""
    return ret
  }

  def hamrSendOutgoingEventPort(comment: ST,
                                methodName: String,
                                srcEnqueue: String
                              ): ST = {
    val ret: ST = st"""${comment}
                      |Unit ${methodName}(STACK_FRAME 
                      |  art_DataContent d) {
                      |
                      |  // event port - can ignore the Slang Empty payload
                      |  art_Empty payload = (art_Empty) d;
                      |
                      |  // send event via CAmkES
                      |  ${srcEnqueue}();
                      |}"""
    return ret
  }
  
  def samplingPortHeader(s: SamplingPortInterface): ST = {
    val spName = s.name
    val macroName = StringUtil.toUpperCase(s"${spName}_h")
    val portType = s.sel4TypeName
    
    val ret = st"""#ifndef ${macroName}
#define ${macroName}

#include "seqNum.h"

// Sampling port message with bool data
typedef struct ${spName} {

  // The sampling port message data.
  ///
  // TODO: How do we handle differnet data types?  Possible options:
  //
  //   - HAMR could generate a dedicated struct for each data port type. In
  //     the long run this may be the best options since AADL can specify the
  //     message type.
  //
  //   - Generalize this struct with some C wizardry. Would it help to split
  //     this into two data parts, one for the data and one for the sequence
  //     number?
  //
  ${portType} data;
  
  // Sequence number incremented by the writer every time the sampling port is
  // written. Read by the reciever to detect dropped messages and incoherant
  // message reads.  An incoherant message is one that is formed of parts of
  // more than one message.  An incoherent message can occure when writing
  // happens durring read. If the component runs long enough, this counter
  // will wrap back to zero.  This causes no problems unless the reciever is
  // delayed for the wrap time. In that case the reciever may not detect
  // dropped or incoherent messags. But if the reciver is delayed for that
  // long the system is probably in a very bad state. Also see DIRTY_SEQ_NUM
  // above.
  //
  // TODO: Currently using ggc builtin _Atomic. Would like to use c11 std, but
  // have not figured out how to do this int the seL4 cmake build environment.
  _Atomic seqNum_t seqNum;  

} ${s.structName};

void init_${spName}(${s.structName} *port, seqNum_t *seqNum);

bool write_${spName}(${s.structName} *port, const ${portType} *data, seqNum_t *seqNum);

bool read_${spName}(${s.structName} *port, ${portType} *data, seqNum_t *seqNum);

#endif
"""
    return ret
  }
  
  def samplingPortImpl(s: SamplingPortInterface): ST = {
    val spName = s.name
    val spType = s"${spName}_t"
    val portType = s.sel4TypeName
    
    val ret = st"""
#include <camkes.h>
#include <stdio.h>
#include <sel4/sel4.h>
#include <utils/util.h>
#include <sel4utils/util.h>
#include <sel4utils/helpers.h>

#include "${spName}.h"

void init_${spName}(${spType} *port, seqNum_t *seqNum) {
  *seqNum = 0; // First message sequence number will be 1.
  port->seqNum = DIRTY_SEQ_NUM;
}

// Write message to a sampling port (data type: int)
//
// Returns true when sucessful. Otherwise returns false. Currently there is no
// way to fail and true is alwasy returned. But this may change in the
// future. seqNum is incremented when a message is succefully sent. seqNum
// should not be modified otherwise.
//
// TODO: Encapsulate this better. seqNum state should be maintained internaly. Possible solutions:
//
//    - Allow write to have read access to dataport. Then seqNum is simply in the data port.
//
//    - Create a wrapper struct.
//
// TODO: Currently using ggc builtin __atomic_thread_fence(__ATOMIC_RELEASE).
// Would like to use c11 std, but have not figured out how to do this int the
// seL4 cmake build environment.
bool write_${spName}(${spType} *port, const ${portType} *data, seqNum_t *seqNum) {
  // Mark the message dirty BEFORE we start writting.
  port->seqNum = DIRTY_SEQ_NUM;
  // Release memory fence - ensure write above to seqNum happens BEFORE reading data
  __atomic_thread_fence(__ATOMIC_RELEASE);
  // Write the data
  port->data = *data;
  // Increment the sequence number. We are the only writer of seqNum, so
  // increment does not have to be atomic.
  *seqNum = (*seqNum + 1) % DIRTY_SEQ_NUM;
  port->seqNum = *seqNum;
  // Release memory fence - ensure write above to seqNum happens BEFORE continuing
  __atomic_thread_fence(__ATOMIC_RELEASE);
  // Can't fail for now.
  return true;
}

// Read a message from a sampling port (data type: int)
//
// Return true upon successful read. Data is updated with the read
// message. The sequence number of the message is also returned. The messaage,
// might be tha same previously read. The sequences number can be used to
// detect rereading the same message or dropped messages.
//
// Return false if we fail to read a message. For now the only way to fail is
// when we detect the possibliliy of a write durring read. In this case data
// may be incoherent and should not be used. Sequence number is set to
// DIRTY_SEQ_NUM;
//
// TODO: Currently using ggc builtin __atomic_thread_fence(__ATOMIC_ACQUIRE).
// Would like to use c11 std, but have not figured out how to do this int the
// seL4 cmake build environment.
bool read_${spName}(${spType} *port, ${portType} *data, seqNum_t *seqNum) {
  seqNum_t newSeqNum = port->seqNum;
  // Acquire memory fence - Read seqNum BEFORE reading data
  __atomic_thread_fence(__ATOMIC_ACQUIRE);
  *data = port->data;
  // Acquire memory fence - Read data BEFORE reading seqNum again 
  //atomic_thread_fence(memory_order_acquire);
  __atomic_thread_fence(__ATOMIC_ACQUIRE);
  // The following logic will NOT catch case where the writer wrapped
  // sequence numbers since our last read. For this to happen, this reader
  // would have to be delayed for the entire time to wrap. 
  if (newSeqNum != DIRTY_SEQ_NUM && newSeqNum == port->seqNum) {
    // Message data is good.  Write did not occure durring read. 
    *seqNum = newSeqNum;
    return true;
  } else {
    // Writer may have updated data while we were reading. Do not use possibly incoherent data.
    *seqNum = DIRTY_SEQ_NUM;
    return false;
  }
}
"""
    return ret
  }
  
  def ifEsleHelper(options: ISZ[(ST, ST)], optElse: Option[ST]): ST = {
    val first: Option[(ST, ST)] = if(options.size > 0) { Some(options(0)) } else { None() }
    val rest: ISZ[(ST, ST)] = if(options.size > 1) { org.sireum.ops.ISZOps(options).drop(1) } else { ISZ() }
    return ifElseST(first, rest, optElse)
  }

  def ifElseST(ifbranch: Option[(ST, ST)], elsifs: ISZ[(ST, ST)], els: Option[ST]): ST = {

    var body = st""

    if(ifbranch.nonEmpty) {
      body = st"""if(${ifbranch.get._1}) {
                 |  ${ifbranch.get._2}
                 |} """
    }

    if(elsifs.nonEmpty) {
      val ei = elsifs.map((x: (ST, ST)) => st"""else if(${x._1}) {
                                               |  ${x._2}
                                               |} """)
      body = st"""${body}${ei}"""
    }

    if(els.nonEmpty) {
      if(ifbranch.nonEmpty) {
        body = st"""${body}else {
                   |  ${els.get}
                   |}"""
      } else {
        body = els.get
      }
    }

    return body
  }
}
