// #Sireum
package org.sireum.hamr.act.proof

import org.sireum._
import org.sireum.hamr.act.ast
import org.sireum.hamr.act.periodic.PeriodicDispatcherTemplate.DISPATCH_PERIODIC_INSTANCE
import org.sireum.hamr.act.util.Util
import org.sireum.hamr.act.vm.MetaPort
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlPort, AadlProcess, SymbolTable}

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
    "VMRefinement"

    "SelfPacing"
    "Pacing"
    "PeriodicDispatching"

    "VM"

    "TimeServer"
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

  def addPortSelfPacing(aadlComponent: AadlComponent,
                        port: ast.CAmkESFeature,
                        symbolTable: SymbolTable): Unit = {
    val ci = Util.getCamkesComponentIdentifier(aadlComponent, symbolTable)
    if(!proofContainer.portRefinementTypes.contains(ci)) {
      proofContainer.portRefinementTypes = proofContainer.portRefinementTypes + (ci ~> Map.empty)
    }
  }

  def addVMPortAux(camkesFeature: ast.CAmkESFeature, aadlProcess: AadlProcess, symbolTable: SymbolTable): Unit = {
    val cin = Util.getCamkesComponentIdentifier(aadlProcess, symbolTable)

    var map: Map[ast.CAmkESFeature, PortInfo] = proofContainer.portRefinementTypes.get(cin) match {
      case Some(m) => m
      case _ => Map.empty
    }
    assert(!map.contains(camkesFeature))

    map = map + (camkesFeature ~> PortVMAux(aadlProcess))

    proofContainer.portRefinementTypes = proofContainer.portRefinementTypes + (cin ~> map)


  }

  def addVMPortRefinement(camkesFeature: ast.CAmkESFeature, aadlComponent: AadlProcess, metaPort: MetaPort, symbolTable: SymbolTable): Unit = {
    val cin = Util.getCamkesComponentIdentifier(aadlComponent, symbolTable)

    var map: Map[ast.CAmkESFeature, PortInfo] = proofContainer.portRefinementTypes.get(cin) match {
      case Some(m) => m
      case _ => Map.empty
    }
    assert(!map.contains(camkesFeature))

    map = map + (camkesFeature ~> PortVMRefinement(aadlComponent, metaPort))

    proofContainer.portRefinementTypes = proofContainer.portRefinementTypes + (cin ~> map)

  }

  def addPortRefinement(camkesFeature: ast.CAmkESFeature, aadlComponent: AadlComponent, aadlPort: AadlPort, symbolTable: SymbolTable): Unit = {
    val cin = Util.getCamkesComponentIdentifier(aadlComponent, symbolTable)

    var map: Map[ast.CAmkESFeature, PortInfo] = proofContainer.portRefinementTypes.get(cin) match {
      case Some(m) => m
      case _ => Map.empty
    }
    assert(!map.contains(camkesFeature))

    map = map + (camkesFeature ~> PortRefinement(aadlComponent, aadlPort))

    proofContainer.portRefinementTypes = proofContainer.portRefinementTypes + (cin ~> map)
  }
}

@sig trait PortInfo

@datatype class PortRefinement (aadlComponent: AadlComponent,
                                aadlPort: AadlPort) extends PortInfo

@datatype class PortVMRefinement (aadlProcess: AadlProcess,
                                  metaPort: MetaPort) extends PortInfo

@datatype class PortVMAux(aadlProcess: AadlProcess) extends PortInfo

@record class ProofContainer(var modelSchedulingType: SchedulingType.Type,

                             var camkesComponentTypes: Map[ast.CamkesComponent, CAmkESComponentCategory.Type],
                             var camkesInstances: ISZ[(Option[AadlComponent], ast.Instance)],

                             var camkesConnections: ISZ[CAmkESConnection],

                             var portRefinementTypes: Map[String, Map[ast.CAmkESFeature, PortInfo]]
                            )