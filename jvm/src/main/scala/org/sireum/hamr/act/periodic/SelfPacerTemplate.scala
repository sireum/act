//# Sireum

package org.sireum.hamr.act.periodic

import org.sireum._
import org.sireum.hamr.act.Util
import org.sireum.hamr.act.periodic.PacerTemplate.periodicEntrypointMethodName

object SelfPacerTemplate {

  val SELF_PACER: String = "self_pacer"

  val SELF_PACER_TICK_TOCK_TYPE: String = "TickTock"

  val PACER_DOMAIN_FIELD: String = "_domain"

  def selfPacerTickTockType(): String = {
    return SELF_PACER_TICK_TOCK_TYPE
  }

  def selfPacerClientTickIdentifier(): String = {
    return Util.brand(s"${SELF_PACER}_tick")
  }

  def selfPacerClientTockIdentifier(): String = {
    return Util.brand(s"${SELF_PACER}_tock")
  }

  def selfPacerDomainConfiguration(identifier: String, domain: Z): ST = {
    return st"${identifier}.${PACER_DOMAIN_FIELD} = ${domain};"
  }

  def settings_cmake_entries(numDomains: Z): ST = {
    val ret: ST = st"""set(KernelDomainSchedule "$${CMAKE_CURRENT_LIST_DIR}/kernel/domain_schedule.c" CACHE INTERNAL "")
                      |set(KernelNumDomains ${numDomains} CACHE STRING "" FORCE)
                      |"""
    return ret
  }

  def selfPacerEmit(): ST = {
    return st"${selfPacerClientTickIdentifier()}_emit();"
  }

  def selfPacerWait(): ST = {
    return st"${selfPacerClientTockIdentifier()}_wait();"
  }

  def wrapPeriodicComputeEntrypoint(classifier: String, userEntrypoint: String): ST = {
    val methodName = periodicEntrypointMethodName(classifier)
    val IN_ARG_VAR: String = "in_arg"

    return st"""void ${methodName}(int64_t *${IN_ARG_VAR}) {
               |  ${userEntrypoint}((int64_t *) ${IN_ARG_VAR});
               |}"""
  }

  def callPeriodicComputEntrypoint(classifier: String, handler: String): ST = {
    val methodName = periodicEntrypointMethodName(classifier)
    val dummyVarName = Util.brand("dummy")
    return st"""{
               |  int64_t ${dummyVarName} = 0;
               |  ${methodName}(&${dummyVarName});
               |}"""
  }

  def selfPacerScheduleEntry(domain: Z,
                             length: Z,
                             comment: Option[ST]): ST = {
    return st"{ .domain = ${domain}, .length = ${length} }, ${comment}"
  }

  def selfPacerExampleSchedule(clock_period: Z,
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

  def selfPacerScheduleThreadPropertyComment(componentId: String,
                                             entries: ISZ[ST]): ST = {
    var dashes: String = s""
    for(x <- 0 until componentId.size){ dashes = s"${dashes}-" }

    val ret: ST = st"""${componentId}
                      |${dashes}
                      |
                      |  ${(entries, "\n")}"""
    return ret
  }
}
