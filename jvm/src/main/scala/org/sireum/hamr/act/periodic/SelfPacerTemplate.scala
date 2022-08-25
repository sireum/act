//# Sireum

package org.sireum.hamr.act.periodic

import org.sireum._
import org.sireum.hamr.act.periodic.PacerTemplate.periodicEntrypointMethodName
import org.sireum.hamr.act.util._

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

  def selfPacerEmit(): ST = {
    return st"${selfPacerClientTickIdentifier()}_emit();"
  }

  def selfPacerWait(): ST = {
    return st"${selfPacerClientTockIdentifier()}_wait();"
  }

  def wrapPeriodicComputeEntrypoint(classifier: String, userEntrypoint: String): ST = {
    val methodName = periodicEntrypointMethodName(classifier)
    val IN_ARG_VAR: String = "in_arg"

    return (
      st"""void ${methodName}(int64_t *${IN_ARG_VAR}) {
          |  ${userEntrypoint}((int64_t *) ${IN_ARG_VAR});
          |}""")
  }

  def callPeriodicComputEntrypoint(classifier: String, handler: String): ST = {
    val methodName = periodicEntrypointMethodName(classifier)
    val dummyVarName = Util.brand("dummy")
    return (
      st"""{
          |  int64_t ${dummyVarName} = 0;
          |  ${methodName}(&${dummyVarName});
          |}""")
  }
}
