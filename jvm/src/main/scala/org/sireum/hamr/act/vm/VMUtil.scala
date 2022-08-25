// #Sireum

package org.sireum.hamr.act.vm

import org.sireum._
import org.sireum.hamr.codegen.common.symbols._

object VMUtil {

  def getPeriodicComponents(symbolTable: SymbolTable): ISZ[AadlComponent] = {
    var ret: ISZ[AadlComponent] = ISZ()
    for (c <- symbolTable.componentMap.values) {
      c match {
        case t: AadlThread if t.isPeriodic() && !t.toVirtualMachine(symbolTable) => ret = ret :+ c
        case p: AadlProcess =>
          if (p.toVirtualMachine(symbolTable)) {
            val avp = p.getBoundProcessor(symbolTable).get.asInstanceOf[AadlVirtualProcessor]
            if (avp.isPeriodic()) {
              ret = ret :+ c
            }
          }
        case _ =>
      }
    }
    return ret
  }
}
