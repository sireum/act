// #Sireum

package org.sireum.hamr.act.connections

import org.sireum._
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.ir

object Monitors {
  def getMonitorForConnectionInstance(instance: ir.ConnectionInstance,
                                      monitors: HashSMap[IdPath, Monitor]): Option[Monitor] = {
    for(m <- monitors.values if m.ci == instance){
      return Some(m)
    }
    return None[Monitor]()
  }

  def getMonitorForInPort(featurePath: IdPath,
                          monitors: HashSMap[IdPath, Monitor]): Option[Monitor] = {
    for(m <- monitors.values if m.ci.dst.feature.get.name == featurePath) {
      return Some(m)
    }
    return None[Monitor]()
  }
}
