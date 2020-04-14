// #Sireum

package org.sireum.hamr.act.periodic

import org.sireum._
import org.sireum.hamr.act.Util

object PacerTemplate {

  val PACER_COMPONENT_TYPE: String = "Pacer"
  val PACER_IDENTIFIER: String = "pacer"
    
  val PACER_PERIOD_TYPE: String = "Period"
  val PACER_PERIOD_IDENTIFIER: String = "period"
  
  val PACER_DOMAIN_FIELD: String = "_domain"
  val PACER_DOMAIN: Z = z"1" // pacer has to be in domain 1
  
  val PACER_TICK_TOCK_TYPE: String = "TickTock"
  val PACER_TICK_IDENTIFIER: String = "tick"
  val PACER_TOCK_IDENTIFIER: String = "tock"
  
  def pacerPeriodIdentifier(): String = {
    return PACER_PERIOD_IDENTIFIER
  }
  
  def pacerComponentDir(): String= {
    return s"${Util.DIR_COMPONENTS}/${PACER_COMPONENT_TYPE}"
  }

  def pacerImport(): String = {
    return st""""${pacerComponentDir()}/${PACER_COMPONENT_TYPE}.camkes"""".render
  }

  def pacerGlueCodeFilename(): String = { 
    return s"${PACER_COMPONENT_TYPE}.c"
  }
  
  def pacerGlueCodePath(): String = {
    return s"${pacerComponentDir()}/${Util.DIR_SRC}/${pacerGlueCodeFilename()}"
  }
  
  def periodicEntrypointMethodName(classifier: String): String = {
    return Util.brand(s"entrypoint_period_${classifier}")
  }
  
  def callPeriodicComputEntrypoint(classifier: String, handler: String): ST = {
    val methodName = periodicEntrypointMethodName(classifier)
    val dummyVarName = Util.brand("dummy")
    return st"""{ 
               |  int64_t ${dummyVarName} = 0;
               |  ${methodName}(&${dummyVarName});
               |}"""
  }

  def wrapPeriodicComputeEntrypoint(classifier: String, userEntrypoint: String): ST = {
    val methodName = periodicEntrypointMethodName(classifier)
    val IN_ARG_VAR: String = "in_arg"
    
    return st"""void ${methodName}(int64_t *${IN_ARG_VAR}) {
               |  ${userEntrypoint}((int64_t *) ${IN_ARG_VAR}); 
               |}"""
  }
  
  def pacerGlueCode(periodEmits: ISZ[ST]): ST = {
    val _p: ISZ[ST] = periodEmits.map(m => st"${m};")
    val ret: ST = st"""// Copyright 2019 Adventium Labs
                      |
                      |#include <camkes.h>
                      |#include <stdio.h>
                      |#include <sel4/sel4.h>
                      |
                      |int run(void) {
                      |
                      |  // Just used for output
                      |  int tickCount = 0;
                      |
                      |  while (1) {
                      |
                      |    printf("%s: Period tick %d\n", get_instance_name(), ++tickCount);
                      |    tick_emit();
                      |
                      |    ${(_p, "\n")}
                      |
                      |    tock_wait();
                      |  }
                      |  return 0;
                      |}"""
    return ret
  }
  
  def pacerScheduleEntry(domain: Z,
                         length: Z,
                         comment: Option[ST]): ST = {
    return st"{ .domain = ${domain}, .length = ${length} }, ${comment}"
  }
  
  def pacerExampleSchedule(clock_period: Z,
                           frame_period: Z,
                           threadProperties: ISZ[ST],
                           entries: ISZ[ST]): ST = {
    val ret: ST = st"""#include <config.h>
                      |#include <object/structures.h>
                      |#include <model/statedata.h>
                      |
                      |// this file will not be overwritten and is safe to edit
                      |
                      |/************************************************************
                      |
                      |   This is a kernel data structure containing an example schedule.
                      |   The length is in seL4 ticks (${clock_period} ms). 
                      |   This schedule should be generated from the AADL model
                      |   using execution time and data flow latency specifications.
                      |
                      |   Pacer runs at highest rate and should always be in domain 1
                      |
                      |   Properties from AADL Model
                      |   --------------------------
                      |
                      |     Timing_Properties::Clock_Period : ${clock_period} ms 
                      |     Timing_Properties::Frame_Period : ${frame_period} ms
                      |   
                      |     ${(threadProperties, "\n\n")}
                      |     
                      | *********************************************************/
                      |
                      |const dschedule_t ksDomSchedule[] = {
                      |  ${(entries, "\n")}
                      |};
                      |
                      |const word_t ksDomScheduleLength = sizeof(ksDomSchedule) / sizeof(dschedule_t);
                      |"""
    return ret
  }

  def pacerScheduleThreadPropertyComment(componentId: String,
                                         entries: ISZ[ST]): ST = {
    var dashes: String = s""
    for(x <- 0 until componentId.size){ dashes = s"${dashes}-" }
    
    val ret: ST = st"""${componentId}
                      |${dashes}
                      |
                      |  ${(entries, "\n")}"""
    return ret
  }
  
  def pacerWait(): ST = {
    return st"${pacerClientNotificationIdentifier()}_wait();"
  }
  
  def pacerClientNotificationIdentifier(): String = {
    return Util.brand("pacer_notification")
  }
  
  def pacerComponentTickIdentifier(): String = {
    return PACER_TICK_IDENTIFIER
  }
  
  def pacerComponentTockIdentifier(): String = {
    return PACER_TOCK_IDENTIFIER
  }

  def pacerDomainConfiguration(identifier: String, domain: Z): ST = {
    return st"${identifier}.${PACER_DOMAIN_FIELD} = ${domain};" 
  }

  def settingsCmake(numDomains: Z): ST = {
    val ret: ST = st"""# this file will not be overwritten and is safe to edit
                      |
                      |set(KernelDomainSchedule "$${CMAKE_CURRENT_LIST_DIR}/kernel/domain_schedule.c" CACHE INTERNAL "")
                      |set(KernelNumDomains ${numDomains} CACHE STRING "" FORCE)
                      |"""
    return ret
  }
}