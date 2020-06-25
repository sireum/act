// #Sireum

package org.sireum.hamr.act.periodic

import org.sireum._
import org.sireum.hamr.ir
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.act.{ActOptions, ActPlatform, CamkesAssemblyContribution, CamkesComponentContributions, CamkesGlueCodeContributions, Counter, Util}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.message.Reporter


@sig trait PeriodicImpl {
  def symbolTable: SymbolTable
  def actOptions: ActOptions

  def handlePeriodicComponents(connectionCounter: Counter,
                               timerAttributeCounter: Counter,
                               headerInclude: String,
                               reporter: Reporter): CamkesAssemblyContribution

  def handlePeriodicComponent(aadlThread: AadlThread,
                              reporter: Reporter): (CamkesComponentContributions, CamkesGlueCodeContributions)
}

@enum object PeriodicDispatchingType {
  'Pacer
  'SelfPacer
  'PeriodicDispatcher
}

object PeriodicUtil {

  def getDispatchingType(symbolTable: SymbolTable, actPlatform: ActPlatform.Type): PeriodicDispatchingType.Type = {
    val ret: PeriodicDispatchingType.Type = if(PeriodicUtil.usePacer(symbolTable, actPlatform)) {
      if(symbolTable.hasVM()) {
        PeriodicDispatchingType.Pacer
      } else {
        PeriodicDispatchingType.SelfPacer
      }
    } else {
      PeriodicDispatchingType.PeriodicDispatcher
    }
    return ret
  }

  def requiresTimeServer(symbolTable: SymbolTable, platform: ActPlatform.Type): B = {
    return symbolTable.hasPeriodicThreads() && !usePacer(symbolTable, platform)
  }

  def requiresPacerArtifacts(c: ir.Component, symbolTable: SymbolTable, platform: ActPlatform.Type): B = {
    return CommonUtil.isPeriodic(c) && usePacer(symbolTable, platform)
  }
  
  def usePacer(symbolTable: SymbolTable, platform: ActPlatform.Type) : B = {
    // TODO ??
    val allProcessesHavePacerAnnotations: B = {
      val processes: ISZ[AadlProcess] = symbolTable.getProcesses()
      var ret: B = T
      for(p <- processes) { ret = ret && p.getDomain().nonEmpty }
      ret
    }

    return platform != ActPlatform.SeL4_TB && allProcessesHavePacerAnnotations
  }
}


