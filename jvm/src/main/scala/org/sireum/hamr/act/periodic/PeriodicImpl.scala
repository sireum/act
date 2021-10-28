// #Sireum

package org.sireum.hamr.act.periodic

import org.sireum._
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.symbols._


@sig trait PeriodicImpl {
  def actOptions: ActOptions

  def handlePeriodicComponents(connectionCounter: Counter,
                               timerAttributeCounter: Counter,
                               headerInclude: String,
                               symbolTable: SymbolTable): CamkesAssemblyContribution

  def handlePeriodicComponent(aadlComponent: AadlComponent, symbolTable: SymbolTable): (CamkesComponentContributions, CamkesGlueCodeContributions)
}

@enum object PeriodicDispatchingType {
  'Pacer
  'SelfPacer
  'PeriodicDispatcher
}
