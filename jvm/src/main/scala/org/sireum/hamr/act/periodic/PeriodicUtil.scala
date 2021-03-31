// #Sireum

package org.sireum.hamr.act.periodic

import org.sireum._
import org.sireum.hamr.act.util.{ActPlatform, Util}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlProcess, AadlProcessor, SymbolTable}
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
    // - platform is seL4 or seL4_Only
    // - all threads must be in separate processes
    // - each process with a thread must have domain info
    // - every process with a thread must be bound to the same processor
    // - the bound processor must have the following annotations
    //      - Frame_Period
    //		  - Clock_Period

    val threads = symbolTable.getThreads()

    var canUseDomainScheduling: B = T

    var mesg: ISZ[ST] = ISZ()

    if(platform == ActPlatform.SeL4_TB) {
      canUseDomainScheduling = F
      mesg = mesg :+ st"Domain scheduling not supported for legacy Trusted Build platform"
    }

    // - all threads must be in separate processes
    var processes: ISZ[AadlProcess] = ISZ()
    for(t <- threads) {
      val p = t.getParent(symbolTable)

      if(ops.ISZOps(processes).contains(p)) {
        canUseDomainScheduling = F
        mesg = mesg :+ st"More than one thread is in process ${p.identifier}"
      }
      processes = processes :+ p
    }

    // - each process with a thread must have domain info
    if(canUseDomainScheduling) {
      var withoutDomain: ISZ[AadlProcess] = ISZ()
      for (p <- processes) {
        if (p.getDomain().isEmpty) {
          withoutDomain = withoutDomain :+ p
        }
      }

      if(withoutDomain.size > 0) {
        canUseDomainScheduling = F
        val withoutDomains = st"${(withoutDomain.map((m: AadlProcess) => m.path), ", ")}".render
        mesg = mesg :+ st"The following processes do not have domain information: ${withoutDomains}"
      }
    }

    var boundProcessor: Option[AadlProcessor] = None()

    // - each process must be bound to the same processor
    if(canUseDomainScheduling) {
      var boundProcessors: Set[AadlProcessor] = Set.empty
      var unboundedProcesses: ISZ[AadlProcess] = ISZ()
      for(p <- processes) {
        symbolTable.getBoundProcessor(p) match {
          case Some(proc) => boundProcessors = boundProcessors + proc
          case None() => unboundedProcesses = unboundedProcesses :+ p
        }
      }

      if(unboundedProcesses.nonEmpty) {
        canUseDomainScheduling = F
        val x = st"${(unboundedProcesses.map((m: AadlProcess) => m.path), ",")}"
        mesg = mesg :+ st"The following processes are not bound to a processor: ${x}"
      }

      if(boundProcessors.size > 1) {
        canUseDomainScheduling = F
        val x = st"${(boundProcessors.elements.map((m: AadlProcessor) => m.path), ",")}"
        mesg = mesg :+ st"""All processes containing threads must be bound to the same processor.
                           |Bind all processes to exactly one of the following: ${x}"""
      } else if(canUseDomainScheduling) {
        boundProcessor = Some(boundProcessors.elements(0))
      }
    }

    if(canUseDomainScheduling) {
      assert(boundProcessor.nonEmpty, "Expecting a single bound processor") // sanity check
      val b = boundProcessor.get
      // - the bound processor must have the following annotations
      //      - Frame_Period
      //		  - Clock_Period

      if(b.getClockPeriod().isEmpty) {
        canUseDomainScheduling = F
        mesg = mesg :+ st"Bound processor missing Clock_Period annotation: ${b.path}"
      }

      if(b.getFramePeriod().isEmpty) {
        canUseDomainScheduling = F
        mesg = mesg :+ st"Bound processor missing Frame_Period annotation: ${b.path}"
      }
    }

    if(mesg.nonEmpty) {
      assert(!canUseDomainScheduling, "Only expecting messages related to why domain scheduling won't be used") // sanity check
      val m = st"""Domain scheduling will not be used due to the following reasons:
                  |  ${(mesg, "\n\n")}"""
      Util.reporter.info(None(), Util.toolName, m.render)
    }

    return canUseDomainScheduling
  }
}
