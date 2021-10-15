// #Sireum
package org.sireum.hamr.act.proof

import org.sireum._
import org.sireum.hamr.act.util.{Sel4ConnectorTypes, Util}
import org.sireum.hamr.codegen.common.{CommonUtil, Names}
import org.sireum.hamr.codegen.common.symbols.{AadlPort, AadlThread, SymbolTable}

object ProofContainer {

  @datatype class CAmkESConnection(connectionName: String,
                                   sourceCAmkESPort: String,
                                   destCAmkESPort: String,
                                   typ: Sel4ConnectorTypes.Type)

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
      ISZ(), ISZ(),

      ISZ(), ISZ(), ISZ(), ISZ(), ISZ(),

      ISZ(), ISZ()
    )
  }

  def merge(a1: ProofContainer, a2: ProofContainer): Unit = {
    a1.aadlComponents = a1.aadlComponents ++ a2.aadlComponents
    a1.aadlConnections = a1.aadlConnections ++ a2.aadlConnections

    a1.camkesComponents = a1.camkesComponents ++ a2.camkesComponents
    a1.camkesPorts = a1.camkesPorts ++ a2.camkesPorts
    a1.camkesPortConstraints = a1.camkesPortConstraints ++ a2.camkesPortConstraints
    a1.camkesConnections = a1.camkesConnections ++ a2.camkesConnections

    a1.componentRefinements = a1.componentRefinements ++ a2.componentRefinements
    a1.portRefinements = a1.portRefinements ++ a2.portRefinements
  }
}

import ProofContainer._

object ProofUtil {

  var proofContainer: ProofContainer = ProofContainer.empty()

  def reset(): Unit = { proofContainer = ProofContainer.empty() }

  def addCamkesPort(aadlThread: AadlThread, aadlPort: AadlPort,
                    camkesPortId: String, connType: Sel4ConnectorTypes.Type, symbolTable: SymbolTable): Unit = {
      val camkesInstanceId = Util.getCamkesComponentIdentifier(aadlThread, symbolTable)
      val proofPortId = s"${camkesInstanceId}_${camkesPortId}"

      ProofUtil.addCamkesPortI(proofPortId)
      ProofUtil.addCamkesPortConstraintI(camkesInstanceId, proofPortId, CommonUtil.isInFeature(aadlPort.feature), connType)
      ProofUtil.addPortRefinementI(aadlPort, proofPortId)
  }

  def addAadlComponent(src: AadlThread, names: Names, symbolTable: SymbolTable): Unit = {

    proofContainer.aadlComponents = proofContainer.aadlComponents :+ src
  }

  def addAadlConnection(srcPortPath: String, dstPortPath: String): Unit = {
    val p = (srcPortPath, dstPortPath)
    proofContainer.aadlConnections = proofContainer.aadlConnections :+ p
  }


  def addCamkesComponent(camkesComponentPath: ComponentPath): Unit = {
    proofContainer.camkesComponents = proofContainer.camkesComponents :+ camkesComponentPath
  }

  def addCamkesPortI(camkesPortPath: PortPath): Unit = {
    proofContainer.camkesPorts = proofContainer.camkesPorts :+ camkesPortPath
  }

  def addCamkesPortConstraintI(componentPath: ComponentPath, portPath: PortPath, isIn: B, camkesPortType: Sel4ConnectorTypes.Type): Unit = {
    val dir: String = if (isIn) "In" else "Out"
    val p = (componentPath, portPath, dir, camkesPortType.name)
    proofContainer.camkesPortConstraints = proofContainer.camkesPortConstraints :+ p
  }

  def addCamkesConnection(c: CAmkESConnection): Unit = {
    proofContainer.camkesConnections = proofContainer.camkesConnections :+ c
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
}


@record class ProofContainer(var aadlComponents: ISZ[AadlThread],
                             var aadlConnections: ISZ[(String, String)],

                             var camkesComponents: ISZ[ComponentPath],
                             var camkesPorts: ISZ[PortPath],
                             var camkesPortConstraints: ISZ[(ComponentPath, PortPath, Direction, CAmkESPortType)],
                             var camkesPortConnections: ISZ[(String, String)],

                             var camkesConnections: ISZ[CAmkESConnection],

                             var componentRefinements: ISZ[(AadlThread, ComponentPath)],
                             var portRefinements: ISZ[(AadlPort, PortPath)]
                            )