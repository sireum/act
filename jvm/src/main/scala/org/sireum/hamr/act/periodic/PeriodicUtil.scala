// #Sireum

package org.sireum.hamr.act.periodic

import org.sireum._
import org.sireum.hamr.act.util.{ActPlatform, Util}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlProcessor, PacerUtil, SymbolTable}
import org.sireum.hamr.codegen.common.util.CodeGenPlatform
import org.sireum.hamr.ir

object PeriodicUtil {

  def getDispatchingType(symbolTable: SymbolTable, usingPacer: B): PeriodicDispatchingType.Type = {
    val ret: PeriodicDispatchingType.Type =
      if(usingPacer) {
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

  def requiresTimeServer(symbolTable: SymbolTable, usingPacer: B): B = {
    return symbolTable.hasPeriodicThreads() && !usingPacer
  }

  def requiresPacerArtifacts(c: ir.Component, symbolTable: SymbolTable, usingPacer: B): B = {
    return CommonUtil.isPeriodic(c) && usingPacer
  }

  def getBoundProcessor(symbolTable: SymbolTable): AadlProcessor = {
    // TODO: assumes useDomainScheduling returned true so all processes
    // should be bound to the same processor.  Perhaps instead have
    // an 'ACT' symbol resolution phase that has sel4/case specific info

    val aadlThread = symbolTable.getThreads()(0)
    val aadlProcess = aadlThread.getParent(symbolTable)
    return symbolTable.getBoundProcessor(aadlProcess).get
  }

  def useDomainScheduling(symbolTable: SymbolTable, platform: ActPlatform.Type): B = {
    val _platform: CodeGenPlatform.Type = platform match {
      case ActPlatform.SeL4_TB => CodeGenPlatform.SeL4_TB
      case ActPlatform.SeL4 => CodeGenPlatform.SeL4
      case ActPlatform.SeL4_Only => CodeGenPlatform.SeL4_Only
    }
    return PacerUtil.canUseDomainScheduling(symbolTable, _platform, Util.reporter)
  }
}
