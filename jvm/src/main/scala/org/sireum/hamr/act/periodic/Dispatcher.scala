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
        Pacer(symbolTable, actOptions).handlePeriodicComponents(
          connectionCounter,
          timerAttributeCounter,
          headerInclude)

      case PeriodicDispatchingType.SelfPacer =>
        SelfPacer(symbolTable, actOptions).handlePeriodicComponents(
          connectionCounter,
          timerAttributeCounter,
          headerInclude)

      case PeriodicDispatchingType.PeriodicDispatcher =>
        PeriodicDispatcher(symbolTable, actOptions).handlePeriodicComponents(
          connectionCounter,
          timerAttributeCounter,
          headerInclude)
    }
    return ret
  }

  def handlePeriodicComponent(useDomainScheduling: B,
                              symbolTable: SymbolTable,
                              actOptions: ActOptions,

                              aadlThread: AadlThread): (CamkesComponentContributions, CamkesGlueCodeContributions) = {
    
    val ret: (CamkesComponentContributions, CamkesGlueCodeContributions) =
      PeriodicUtil.getDispatchingType(symbolTable, useDomainScheduling) match {
        case PeriodicDispatchingType.Pacer =>
          Pacer(symbolTable, actOptions).handlePeriodicComponent(aadlThread)

        case PeriodicDispatchingType.SelfPacer =>
          SelfPacer(symbolTable, actOptions).handlePeriodicComponent(aadlThread)

        case PeriodicDispatchingType.PeriodicDispatcher =>
          PeriodicDispatcher(symbolTable, actOptions).handlePeriodicComponent(aadlThread)
      }

    return ret
  }
}

