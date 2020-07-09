// #Sireum

package org.sireum.hamr.act.connections

import org.sireum._
import org.sireum.hamr.act.Monitor
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.ir

object Monitors {
  def getMonitorForConnectionInstance(instance: ir.ConnectionInstance,
                                      monitors: HashSMap[String, Monitor]): Option[Monitor] = {
    for(m <- monitors.values if m.ci == instance){
      return Some(m)
    }
    return None[Monitor]()
  }

  def getMonitorForInPort(end: ir.FeatureEnd,
                          monitors: HashSMap[String, Monitor]): Option[Monitor] = {
    val n = CommonUtil.getName(end.identifier)
    for(m <- monitors.values if CommonUtil.getName(m.ci.dst.feature.get) == n) {
      return Some(m)
    }
    return None[Monitor]()
  }
}
