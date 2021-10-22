// #Sireum
package org.sireum.hamr.act.proof

import org.sireum._
import org.sireum.hamr.act.ast
import org.sireum.hamr.act.periodic.PeriodicDispatcherTemplate.DISPATCH_PERIODIC_INSTANCE
import org.sireum.hamr.act.util.Util
import org.sireum.hamr.codegen.common.Names
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlPort, AadlThread, SymbolTable}

object ProofContainer {

  @enum object CAmkESPortCategory {
    "Refinement"
  }

  @enum object CAmkESComponentCategory {
    "Refinement"
    "VM_Refinement"
    "Pacer"
    "TimeServer"
    "PeriodicDispatcher"
    "Monitor"
  }

  @enum object CAmkESConnectionType {
    "Refinement"
    "SelfPacing"
    "Pacing"
    "PeriodicDispatching"
    "VM"
  }

  @datatype class CAmkESConnection(connType: CAmkESConnectionType.Type,
                                   connection: ast.Connection)

  @enum object SchedulingType {
    "Pacing"
    "SelfPacing"
    "PeriodicDispatching"
  }

  @enum object AadlPortType {
    "AadlDataPort"
    "AadlEventPort"
    "AadlEventDataPort"
  }

  def empty(): ProofContainer = {
    return ProofContainer(
      modelSchedulingType = SchedulingType.PeriodicDispatching,

      camkesInstances = ISZ(),
      camkesComponentTypes = Map.empty,

      camkesConnections = ISZ(),

      portRefinementTypes = Map.empty
    )
  }
}

import ProofContainer._

object ProofUtil {

  var proofContainer: ProofContainer = ProofContainer.empty()

  def reset(): Unit = { proofContainer = ProofContainer.empty() }

  def addCAmkESInstance(aadlOrigin: Option[AadlComponent], component: ast.Instance): Unit = {
    proofContainer.camkesInstances = proofContainer.camkesInstances :+ ((aadlOrigin, component))
  }

  def addCamkesComponent(component: ast.CamkesComponent, componentCategory: CAmkESComponentCategory.Type): Unit = {
    proofContainer.camkesComponentTypes = proofContainer.camkesComponentTypes + (component ~> componentCategory)
  }

  def addCAmkESConnection(connectionCategory: CAmkESConnectionType.Type, c: ast.Connection): Unit = {
    proofContainer.camkesConnections = proofContainer.camkesConnections :+ CAmkESConnection(connectionCategory, c)
  }


  def addPortPeriodicDispatcher(p: ast.CAmkESFeature): Unit = {
    if(!proofContainer.portRefinementTypes.contains(DISPATCH_PERIODIC_INSTANCE)) {
      proofContainer.portRefinementTypes = proofContainer.portRefinementTypes + (DISPATCH_PERIODIC_INSTANCE ~> Map.empty)
    }
  }

  def addPortMonitor(monitorName: String,
                     port: ast.CAmkESFeature): Unit = {
    if(!proofContainer.portRefinementTypes.contains(monitorName)) {
      proofContainer.portRefinementTypes = proofContainer.portRefinementTypes + (monitorName ~> Map.empty)
    }
  }

  def addPortSelfPacing(aadlThread: AadlThread,
                        port: ast.CAmkESFeature,
                        symbolTable: SymbolTable): Unit = {
    val ci = Util.getCamkesComponentIdentifier(aadlThread, symbolTable)
    if(!proofContainer.portRefinementTypes.contains(ci)) {
      proofContainer.portRefinementTypes = proofContainer.portRefinementTypes + (ci ~> Map.empty)
    }
  }

  def addPortRefinement(camkesFeature: ast.CAmkESFeature, aadlThread: AadlThread, aadlPort: AadlPort, symbolTable: SymbolTable): Unit = {
    val cin = Util.getCamkesComponentIdentifier(aadlThread, symbolTable)

    var map: Map[ast.CAmkESFeature, PortRefinement] = proofContainer.portRefinementTypes.get(cin) match {
      case Some(m) => m
      case _ => Map.empty
    }
    assert(!map.contains(camkesFeature))

    map = map + (camkesFeature ~> PortRefinement(aadlThread, aadlPort))

    proofContainer.portRefinementTypes = proofContainer.portRefinementTypes + (cin ~> map)
  }
}

@sig trait PortInfo

@datatype class PortRefinement (aadlThead: AadlThread,
                                aadlPort: AadlPort) extends PortInfo

@record class ProofContainer(var modelSchedulingType: SchedulingType.Type,

                             var camkesComponentTypes: Map[ast.CamkesComponent, CAmkESComponentCategory.Type],
                             var camkesInstances: ISZ[(Option[AadlComponent], ast.Instance)],

                             var camkesConnections: ISZ[CAmkESConnection],

                             var portRefinementTypes: Map[String, Map[ast.CAmkESFeature, PortRefinement]]
                            )