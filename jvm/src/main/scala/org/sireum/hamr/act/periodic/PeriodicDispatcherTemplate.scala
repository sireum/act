// #Sireum

package org.sireum.hamr.act.periodic

import org.sireum._
import org.sireum.hamr.act.ast
import org.sireum.hamr.act.templates.StringTemplate
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.util.ResourceUtil

object PeriodicDispatcherTemplate {

  val TIMER_ID: String = Util.brand("timer")
  val TIMER_ID_DISPATCHER: String = "timer"

  // Notification from time server to the periodic dispatcher
  val TIMER_NOTIFICATION_DISPATCHER_ID: String = "timer_complete"

  val TIMER_TYPE: String = "Timer"
  val TIMER_INSTANCE: String = "time_server"

  val TIMER_SERVER_CLASSIFIER: String = "TimeServer"
  val TIMER_SERVER_TIMER_ID: String = "the_timer"
  val TIMER_SERVER_NOTIFICATION_ID: String = "timer_notification"

  val TIMER_SERVER_IMPORT: String = "<TimeServer/TimeServer.camkes>;"

  val DISPATCH_CLASSIFIER: String = "dispatch_periodic"
  val DISPATCH_PERIODIC_INSTANCE: String = "dispatch_periodic_inst"
  val DISPATCH_TIMER_ID: String = "timer"


  def calendar(camkesComponentId: String, period: Z): ST = {
    val notifName = componentNotificationName(Some(camkesComponentId))
    val st = st"""if ((aadl_calendar_counter % (${period} / aadl_tick_interval)) == 0) {
                 |  ${notifName}_emit();
                 |}"""
    return st
  }

  def dispatchComponentCSource(modelTypesHeader: String, calendars: ISZ[ST]): Resource = {
    val THREAD_CALENDAR = Util.brand("thread_calendar")
    val st: ST = st"""#include <string.h>
                     |#include <camkes.h>
                     |#include ${modelTypesHeader}
                     |
                     |// prototypes for clock functions
                     |void clock_init();
                     |void clock_set_interval_in_ms(uint32_t interval);
                     |void clock_start_timer(void);
                     |void clock_irq_callback(void);
                     |uint64_t clock_get_time();
                     |
                     |// Declarations for managing periodic thread dispatch
                     |const uint32_t aadl_tick_interval = 1;
                     |uint32_t aadl_calendar_counter = 0;
                     |
                     |void ${THREAD_CALENDAR}() {
                     |  ${(calendars, "\n")}
                     |
                     |  aadl_calendar_counter++;
                     |}
                     |
                     |void ${TIMER_NOTIFICATION_DISPATCHER_ID}_callback() {
                     |  ${THREAD_CALENDAR}();
                     |}
                     |
                     |// no op under the new time server scheme.
                     |void clock_init() { }
                     |
                     |// Set interrupt interval, in milliseconds.
                     |void clock_set_interval_in_ms(uint32_t interval) {
                     |  timer_periodic(0, ((uint64_t)interval) * NS_IN_MS);
                     |}
                     |
                     |// no op under the new time server scheme
                     |void clock_start_timer(void) { }
                     |
                     |// defer to time server
                     |uint64_t clock_get_time() {
                     |  return (timer_time() / NS_IN_MS);
                     |}
                     |
                     |int run(void) {
                     |  clock_init();
                     |  clock_set_interval_in_ms(1);
                     |  clock_start_timer();
                     |  return 0;
                     |}
                     |"""

    val compTypeFileName:String = Util.brand(DISPATCH_CLASSIFIER)
    return ResourceUtil.createResource(s"${Util.DIR_COMPONENTS}/${DISPATCH_CLASSIFIER}/${Util.DIR_SRC}/${compTypeFileName}.c", st, T)
  }

  def componentNotificationName(camkesComponentId: Option[String]): String = {
    val prefix: String = {
      camkesComponentId match {
        case Some(c) => s"${c}_"
        case _ => ""
      }
    }
    return Util.brand(s"${prefix}periodic_dispatch_notification")
  }

  def configurationTimerAttribute(instanceName: String, i: Z, isDispatcher: B): ast.Configuration = {
    val id: String = if(isDispatcher) {TIMER_ID_DISPATCHER} else {TIMER_ID}
    return ast.GenericConfiguration(s"${instanceName}.${id}_attributes = ${i};", ISZ())
  }

  def configurationTimerGlobalEndpoint(instanceName: String, interfaceName: String, classifier: String, id: String): ast.Configuration = {
    return ast.GenericConfiguration(st"""${instanceName}.${interfaceName}_global_endpoint = "${classifier}_${id}";""".render, ISZ())
  }


  val VAR_PERIODIC_OCCURRED : String = Util.brand("occurred_periodic_dispatcher")
  val VAR_PERIODIC_TIME : String = Util.brand("time_periodic_dispatcher")
  val METHOD_PERIODIC_CALLBACK : String = s"${PeriodicDispatcherTemplate.componentNotificationName(None())}_callback"

  def periodicDispatchElems(timerHookedUp: B) : ST = {
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
                  |    MUTEXOP(${StringTemplate.SEM_POST}());
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
    val notificationName = PeriodicDispatcherTemplate.componentNotificationName(None())
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

}