// #Sireum

package org.sireum.hamr.act.proof

import org.sireum._
import org.sireum.hamr.act.proof.ProofContainer.{CAmkESPortType, ComponentPath, Direction, PortPath}
import org.sireum.hamr.act.templates.SMT2Template
import org.sireum.hamr.codegen.common.{CommonUtil, Names}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlEventPort, AadlPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.util.ResourceUtil

object SMT2ProofGen {
  var resources: ISZ[Resource] = ISZ()

  def genSmt2Proof(container: ProofContainer,
                   symbolTable: SymbolTable,
                   outputDir:String): ISZ[Resource] = {
    resources = ISZ()

    val aadlComponents: ISZ[ST] = container.aadlComponents.map((m: AadlThread) => {
      val pos: Option[ST] =
        m.component.identifier.pos match {
          case Some(pos) => Some(st" declared at ${pos.uriOpt} (${pos.beginLine}, ${pos.beginColumn})")
          case _ => None()
        }
      st"(${m.path}); Instance of ${m.component.classifier.get.name}${pos}"
    })

    val (aadlPorts, aadlPortComponents, aadlPortTypes, aadlPortDirection): (ISZ[String], ISZ[ST], ISZ[ST], ISZ[ST]) = {
      var ports: ISZ[String] = ISZ()
      var portComponents: ISZ[ST] = ISZ()
      var portTypes: ISZ[ST] = ISZ()
      var portDirs: ISZ[ST] = ISZ()

      for(thread <- container.aadlComponents;
        port <- thread.getPorts() if symbolTable.isConnected(port.feature)) {
        ports = ports :+ s"(${port.path})"
        portComponents = portComponents :+ SMT2Template.aadlPortComponents(port.path, thread.path)
        portTypes = portTypes :+ {
          val portType = port match {
            case a: AadlDataPort => "DataPort"
            case a: AadlEventDataPort => "EventDataPort"
            case a: AadlEventPort => "EventPort"
            case _ => "UNKNOWN_PORT_TYPE"
          }
          SMT2Template.aadlPortType(port.path, portType)
        }
        portDirs = portDirs :+ {
          val dir: String = if (CommonUtil.isInPort(port.feature)) "In" else "Out"
          SMT2Template.aadlPortDirection(port.path, dir)
        }
      }

      (ports, portComponents, portTypes, portDirs)
    }

    val aadlConnectionFlowTos: ISZ[ST] = container.aadlConnections.
      map((r: (String, String)) => SMT2Template.flowsTo(r._1, r._2))


    val camkesComponents: ISZ[ST] = container.camkesComponents.map((m: String) => st"($m)")

    val camkesPorts: ISZ[ST] = container.camkesPorts.map((s: String) => st"($s)")

    val camkesPortComponents: ISZ[ST] = container.camkesPortConstraints.
      map((r : (ComponentPath, PortPath, Direction, CAmkESPortType)) => SMT2Template.camkesPortComponents(r._2, r._1) )

    val camkesPortTypes: ISZ[ST] = container.camkesPortConstraints.
      map((r : (ComponentPath, PortPath, Direction, CAmkESPortType)) => SMT2Template.camkesPortType(r._2, r._4) )

    val camkesPortDirection: ISZ[ST] = container.camkesPortConstraints.
      map((r : (ComponentPath, PortPath, Direction, CAmkESPortType)) => SMT2Template.camkesPortDirection(r._2, r._3) )

    val camkesConnectionFlowTos: ISZ[ST] = container.camkesConnections.
      map((r: (String, String)) => SMT2Template.flowsTo(r._1, r._2))


    val componentRefinements: ISZ[ST] = container.componentRefinements.
      map((r: (AadlThread, String)) => SMT2Template.componentRefinement(r._1.path, r._2))

    val portRefinements: ISZ[ST] = container.portRefinements.
      map((r: (AadlPort, String)) => SMT2Template.portRefinement(r._1.path, r._2))


    val proof = SMT2Template.proof(
      aadlComponents = aadlComponents,
      aadlPorts = aadlPorts,
      aadlPortComponents = aadlPortComponents,
      aadlPortTypes = aadlPortTypes,
      aadlPortDirection = aadlPortDirection,
      aadlConnectionFlowTos = aadlConnectionFlowTos,

      camkesComponents = camkesComponents,
      camkesPorts = camkesPorts,
      camkesPortComponents = camkesPortComponents,
      camkesPortTypes = camkesPortTypes,
      camkesPortDirection = camkesPortDirection,
      camkesConnectionFlowTos = camkesConnectionFlowTos,

      componentRefinements = componentRefinements,
      portRefinements = portRefinements
    )

    val path: Os.Path = Os.path(outputDir) / "proof" / "smt2_case.smt2"

    resources = resources :+ ResourceUtil.createResource(
      path = path.value,
      content = proof,
      overwrite = T)

    return resources
  }
}
