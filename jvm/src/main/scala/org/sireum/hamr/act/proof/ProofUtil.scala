// #Sireum
package org.sireum.hamr.act.proof

import org.sireum._
import org.sireum.hamr.act.ast
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

  type ComponentPath = String
  type PortPath = String
  type CAmkESPortType = String
  type Direction = String

  def empty(): ProofContainer = {
    return ProofContainer(
      modelSchedulingType = SchedulingType.PeriodicDispatching,
      aadlComponents = ISZ(),
      aadlConnections = ISZ(),

      camkesInstances = ISZ(),
      camkesComponents = Map.empty,

      camkesPorts = ISZ(),
      camkesPortConstraints = ISZ(),
      camkesPortConnections = ISZ(),
      camkesConnections = ISZ(),
      componentRefinements = ISZ(),
      portRefinements = ISZ(),

      portRefinementsX = Map.empty
    )
  }
}

import ProofContainer._

object ProofUtil {

  var proofContainer: ProofContainer = ProofContainer.empty()

  def reset(): Unit = { proofContainer = ProofContainer.empty() }

  def addCAmkESSelfPacerPort(aadlThread: AadlThread, camkesPortId: String, symbolTable: SymbolTable): Unit = {
    val camkesInstanceId = Util.getCamkesComponentIdentifier(aadlThread, symbolTable)
    val proofPortId = s"${camkesInstanceId}_${camkesPortId}"
    ProofUtil.addCamkesPortI(proofPortId)
    ProofUtil.addCamkesPortConstraintI(camkesInstanceId, proofPortId)
  }

  def addCamkesPortRefinement(aadlThread: AadlThread, aadlPort: AadlPort,
                              camkesPortId: String, symbolTable: SymbolTable): Unit = {
    val camkesInstanceId = Util.getCamkesComponentIdentifier(aadlThread, symbolTable)
    val proofPortId = s"${camkesInstanceId}_${camkesPortId}"

    ProofUtil.addCamkesPortI(proofPortId)
    ProofUtil.addCamkesPortConstraintI(camkesInstanceId, proofPortId)
    ProofUtil.addPortRefinementI(aadlPort, proofPortId)
  }

  def addAadlComponent(src: AadlThread, names: Names, symbolTable: SymbolTable): Unit = {

    proofContainer.aadlComponents = proofContainer.aadlComponents :+ src
  }

  def addAadlConnection(srcPortPath: String, dstPortPath: String): Unit = {
    val p = (srcPortPath, dstPortPath)
    proofContainer.aadlConnections = proofContainer.aadlConnections :+ p
  }

  def addCAmkESInstance(aadlOrigin: Option[AadlComponent], component: ast.Instance): Unit = {
    proofContainer.camkesInstances = proofContainer.camkesInstances :+ ((aadlOrigin, component))
  }

  def addCamkesComponent(component: ast.CamkesComponent, componentCategory: CAmkESComponentCategory.Type): Unit = {
    proofContainer.camkesComponents = proofContainer.camkesComponents + (component ~> componentCategory)
  }

  def addCamkesPortI(camkesPortPath: PortPath): Unit = {
    proofContainer.camkesPorts = proofContainer.camkesPorts :+ camkesPortPath
  }

  def addCamkesPortConstraintI(componentPath: ComponentPath, portPath: PortPath): Unit = {
    val p = (componentPath, portPath)
    proofContainer.camkesPortConstraints = proofContainer.camkesPortConstraints :+ p
  }

  def addCAmkESConnection(connectionCategory: CAmkESConnectionType.Type, c: ast.Connection): Unit = {
    proofContainer.camkesConnections = proofContainer.camkesConnections :+ CAmkESConnection(connectionCategory, c)
  }

  def addCamkesPortConnection(srcId: String, dstId: String): Unit = {
    val p = (srcId, dstId)
    proofContainer.camkesPortConnections = proofContainer.camkesPortConnections :+ p
  }


  def addComponentRefinement(aadlThread: AadlThread, camkesComponentPath: ComponentPath): Unit = {
    val p = (aadlThread, camkesComponentPath)
    proofContainer.componentRefinements = proofContainer.componentRefinements :+ p
  }

  def addPortRefinementI(aadlPort: AadlPort, camkesPortPath: PortPath): Unit = {
    val p = (aadlPort, camkesPortPath)
    proofContainer.portRefinements = proofContainer.portRefinements :+ p
  }

  def addPortRefinementX(camkesFeature: ast.CAmkESFeature, aadlThread: AadlThread, aadlPort: AadlPort, symbolTable: SymbolTable): Unit = {
    val cin = Util.getCamkesComponentIdentifier(aadlThread, symbolTable)

    var map: Map[ast.CAmkESFeature, PortRefinement] = proofContainer.portRefinementsX.get(cin) match {
      case Some(m) => m
      case _ => Map.empty
    }
    assert(!map.contains(camkesFeature))

    map = map + (camkesFeature ~> PortRefinement(aadlThread, aadlPort))

    proofContainer.portRefinementsX = proofContainer.portRefinementsX + (cin ~> map)
  }
}

@datatype class PortRefinement (aadlThead: AadlThread,
                                aadlPort: AadlPort)

@record class ProofContainer(var modelSchedulingType: SchedulingType.Type,

                             var aadlComponents: ISZ[AadlThread],
                             var aadlConnections: ISZ[(String, String)],

                             var camkesComponents: Map[ast.CamkesComponent, CAmkESComponentCategory.Type],
                             var camkesInstances: ISZ[(Option[AadlComponent], ast.Instance)],

                             var camkesPorts: ISZ[PortPath],
                             var camkesPortConstraints: ISZ[(ComponentPath, PortPath)],
                             var camkesPortConnections: ISZ[(String, String)],

                             var camkesConnections: ISZ[CAmkESConnection],

                             var componentRefinements: ISZ[(AadlThread, ComponentPath)],
                             var portRefinements: ISZ[(AadlPort, PortPath)],

                             var portRefinementsX: Map[String, Map[ast.CAmkESFeature, PortRefinement]]
                            )