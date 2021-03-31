// #Sireum

package org.sireum.hamr.act.periodic

import org.sireum._
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.ir


@sig trait PeriodicImpl {
  def symbolTable: SymbolTable
  def actOptions: ActOptions

  def handlePeriodicComponents(connectionCounter: Counter,
                               timerAttributeCounter: Counter,
                               headerInclude: String): CamkesAssemblyContribution

  def handlePeriodicComponent(aadlThread: AadlThread): (CamkesComponentContributions, CamkesGlueCodeContributions)
}

@enum object PeriodicDispatchingType {
  'Pacer
  'SelfPacer
  'PeriodicDispatcher
}
