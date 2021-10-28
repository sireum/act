// #Sireum

package org.sireum.hamr.act.periodic

import org.sireum._
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.symbols._

object Dispatcher {

  def handlePeriodicComponents(useDomainScheduling: B,

                               symbolTable: SymbolTable,
                               actOptions: ActOptions,
                              
                               connectionCounter: Counter,
                               timerAttributeCounter: Counter,
                              
                               headerInclude: String): CamkesAssemblyContribution = {

    val ret: CamkesAssemblyContribution = PeriodicUtil.getDispatchingType(symbolTable, useDomainScheduling) match {
      case PeriodicDispatchingType.Pacer =>
        Pacer(actOptions).handlePeriodicComponents(
          connectionCounter,
          timerAttributeCounter,
          headerInclude,
          symbolTable)

      case PeriodicDispatchingType.SelfPacer =>
        SelfPacer(actOptions).handlePeriodicComponents(
          connectionCounter,
          timerAttributeCounter,
          headerInclude,
          symbolTable)

      case PeriodicDispatchingType.PeriodicDispatcher =>
        PeriodicDispatcher(actOptions).handlePeriodicComponents(
          connectionCounter,
          timerAttributeCounter,
          headerInclude,
          symbolTable)
    }
    return ret
  }

  def handlePeriodicComponent(useDomainScheduling: B,
                              symbolTable: SymbolTable,
                              actOptions: ActOptions,

                              aadlComponent: AadlComponent): (CamkesComponentContributions, CamkesGlueCodeContributions) = {
    
    val ret: (CamkesComponentContributions, CamkesGlueCodeContributions) =
      PeriodicUtil.getDispatchingType(symbolTable, useDomainScheduling) match {
        case PeriodicDispatchingType.Pacer =>
          Pacer(actOptions).handlePeriodicComponent(aadlComponent, symbolTable)

        case PeriodicDispatchingType.SelfPacer =>
          SelfPacer(actOptions).handlePeriodicComponent(aadlComponent, symbolTable)

        case PeriodicDispatchingType.PeriodicDispatcher =>
          PeriodicDispatcher(actOptions).handlePeriodicComponent(aadlComponent, symbolTable)
      }

    return ret
  }
}

