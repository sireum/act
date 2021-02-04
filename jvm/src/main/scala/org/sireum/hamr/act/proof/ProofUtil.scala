// #Sireum
package org.sireum.hamr.act.proof

import org.sireum._
import org.sireum.hamr.codegen.common.{CommonUtil, Names}
import org.sireum.hamr.codegen.common.symbols.{AadlThread, SymbolTable}
import org.sireum.hamr.ir.FeatureEnd

object ProofUtil {

  def addAadlComponent(src: AadlThread, names: Names, symbolTable: SymbolTable) : Unit = {
    val aadlId = names.aadlQualifiedName

    proofContainer.aadlComponents = proofContainer.aadlComponents :+ aadlId

    for(aadlPort <- src.ports){
      aadlPort.feature match {
        case f: FeatureEnd =>
          if(symbolTable.isConnected(f)) {
            val qualifiedPortId = s"${aadlId}_${aadlPort.identifier}"

            addAadlPort(qualifiedPortId)
            addAadlPortConstraints(aadlId, qualifiedPortId, CommonUtil.isInFeature(aadlPort.feature))
          }
        case _ =>
      }
    }
  }

  var proofContainer: ProofContainer = ProofContainer.empty()

  def addAadlPort(aadlId: String): Unit = {
    proofContainer.aadlPorts = proofContainer.aadlPorts :+ aadlId
  }

  def addAadlPortConstraints(compId: String, portId: String, isIn: B): Unit = {
    val dir: String = if(isIn) "In" else "Out"
    val p = (compId, portId, dir)
    proofContainer.aadlPortConstraints = proofContainer.aadlPortConstraints :+ p
  }

  def addAadlConnection(srcPortId: String, dstPortId: String): Unit = {
    val p = (srcPortId, dstPortId)
    proofContainer.aadlConnections = proofContainer.aadlConnections :+ p
  }


  def addCamkesComponent(camkesId: String) : Unit = {
    proofContainer.camkesComponents = proofContainer.camkesComponents :+ camkesId
  }

  def addCamkesPort(camkesId: String): Unit = {
    proofContainer.camkesPorts = proofContainer.camkesPorts :+ camkesId
  }

  def addCamkesPortConstraint(compId: String, portId: String, isIn: B): Unit = {
    val dir: String = if(isIn) "In" else "Out"
    val p = (compId, portId, dir)
    proofContainer.camkesPortConstraints = proofContainer.camkesPortConstraints :+ p
  }

  def addCamkesConnection(srcId: String, dstId: String): Unit = {
    val p = (srcId, dstId)
    proofContainer.camkesConnections = proofContainer.camkesConnections :+ p
  }


  def addComponentRefinement(aadlComponentId: String, camkesComponentId: String): Unit = {
    val p = (aadlComponentId, camkesComponentId)
    proofContainer.componentRefinements = proofContainer.componentRefinements :+ p
  }

  def addPortRefinment(aadlPortId: String, camkesPortId: String): Unit = {
    val p = (aadlPortId, camkesPortId)
    proofContainer.portRefinements = proofContainer.portRefinements :+ p
  }
}


object ProofContainer {
  def empty(): ProofContainer = {
    return ProofContainer(
      ISZ(), ISZ(), ISZ(), ISZ(),

      ISZ(), ISZ(), ISZ(), ISZ(),

      ISZ(), ISZ()
    )
  }

  def merge(a1: ProofContainer, a2: ProofContainer): Unit = {
    a1.aadlComponents = a1.aadlComponents ++ a2.aadlComponents
    a1.aadlPorts = a1.aadlPorts ++ a2.aadlPorts
    a1.aadlPortConstraints = a1.aadlPortConstraints ++ a2.aadlPortConstraints
    a1.aadlConnections = a1.aadlConnections ++ a2.aadlConnections

    a1.camkesComponents = a1.camkesComponents ++ a2.camkesComponents
    a1.camkesPorts = a1.camkesPorts ++ a2.camkesPorts
    a1.camkesPortConstraints = a1.camkesPortConstraints ++ a2.camkesPortConstraints
    a1.camkesConnections = a1.camkesConnections ++ a2.camkesConnections

    a1.componentRefinements = a1.componentRefinements ++ a2.componentRefinements
    a1.portRefinements = a1.portRefinements ++ a2.portRefinements
  }
}

@record class ProofContainer(var aadlComponents: ISZ[String],
                             var aadlPorts: ISZ[String],
                             var aadlPortConstraints: ISZ[(String, String, String)],
                             var aadlConnections: ISZ[(String, String)],

                             var camkesComponents: ISZ[String],
                             var camkesPorts: ISZ[String],
                             var camkesPortConstraints: ISZ[(String, String, String)],
                             var camkesConnections: ISZ[(String, String)],

                             var componentRefinements: ISZ[(String, String)],
                             var portRefinements: ISZ[(String, String)]
                            )