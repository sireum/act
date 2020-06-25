// #Sireum

package org.sireum.hamr.act.periodic

import org.sireum._
import org.sireum.hamr.act.{ActOptions, CamkesAssemblyContribution, CamkesComponentContributions, CamkesGlueCodeContributions, Counter}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.ir
import org.sireum.message.Reporter

object Dispatcher {

  def handlePeriodicComponents(symbolTable: SymbolTable,
                               actOptions: ActOptions,
                              
                               connectionCounter: Counter,
                               timerAttributeCounter: Counter,
                              
                               headerInclude: String,
                               reporter: Reporter): CamkesAssemblyContribution = {

    val ret: CamkesAssemblyContribution = PeriodicUtil.getDispatchingType(symbolTable, actOptions.platform) match {
      case PeriodicDispatchingType.Pacer =>
        Pacer(symbolTable, actOptions).handlePeriodicComponents(
          connectionCounter,
          timerAttributeCounter,
          headerInclude,
          reporter)

      case PeriodicDispatchingType.SelfPacer =>
        SelfPacer(symbolTable, actOptions).handlePeriodicComponents(
          connectionCounter,
          timerAttributeCounter,
          headerInclude,
          reporter)

      case PeriodicDispatchingType.PeriodicDispatcher =>
        PeriodicDispatcher(symbolTable, actOptions).handlePeriodicComponents(
          connectionCounter,
          timerAttributeCounter,
          headerInclude,
          reporter)
    }
    return ret
  }

  def handlePeriodicComponent(symbolTable: SymbolTable,
                              actOptions: ActOptions,

                              aadlThread: AadlThread,
                              reporter: Reporter): (CamkesComponentContributions, CamkesGlueCodeContributions) = {
    
    val ret: (CamkesComponentContributions, CamkesGlueCodeContributions) =
      PeriodicUtil.getDispatchingType(symbolTable, actOptions.platform) match {
        case PeriodicDispatchingType.Pacer =>
          Pacer(symbolTable, actOptions).handlePeriodicComponent(aadlThread, reporter)

        case PeriodicDispatchingType.SelfPacer =>
          SelfPacer(symbolTable, actOptions).handlePeriodicComponent(aadlThread, reporter)

        case PeriodicDispatchingType.PeriodicDispatcher =>
          PeriodicDispatcher(symbolTable, actOptions).handlePeriodicComponent(aadlThread, reporter)
      }

    return ret
  }
}

