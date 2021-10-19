// #Sireum

package org.sireum.hamr.act.proof

import org.sireum._
import org.sireum.hamr.act.proof.ProofContainer.{AadlPortType, CAmkESConnection, CAmkESPortType, ComponentPath, Direction, PortPath}
import org.sireum.hamr.act.templates.SMT2Template
import org.sireum.hamr.act.util.Util.reporter
import org.sireum.hamr.act.util.{Sel4ConnectorTypes, Util}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlEventPort, AadlPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.util.ResourceUtil

object SMT2ProofGen {
  var resources: ISZ[Resource] = ISZ()

  def genSmt2Proof(container: ProofContainer,
                   symbolTable: SymbolTable,
                   outputDir:String): ISZ[Resource] = {
    resources = ISZ()

    val (aadlComponents, aadlDispatchProtocols, altAadlDispatchProtocols): (ISZ[ST], ISZ[ST], ST) = {
      var aadlComps: ISZ[ST] = ISZ()
      var aadlDPs: ISZ[ST] = ISZ()
      var altAadlDPEntries: ISZ[ST] = ISZ()
      for(t <- container.aadlComponents) {
        val pos: Option[ST] =
          t.component.identifier.pos match {
            case Some(pos) => Some(st" declared at ${pos.uriOpt} (${pos.beginLine}, ${pos.beginColumn})")
            case _ => None()
          }
        aadlComps = aadlComps :+ st"(${t.path}); Instance of ${t.component.classifier.get.name}${pos}"
        aadlDPs = aadlDPs :+ SMT2Template.aadlDispatchProtocol(t)
        altAadlDPEntries = altAadlDPEntries :+ st"(ite (= _comp ${t.path}) ${t.dispatchProtocol.name}"
      }
      val cparens = altAadlDPEntries.map((m: ST) => ")")
      val x = st"""${(altAadlDPEntries, "\n")}
                  |UNSPECIFIED_DISPATCH_PROTOCOL${(cparens, "")}"""
      (aadlComps, aadlDPs, x)
    }

    val (aadlPorts, aadlPortComponents, aadlPortTypes, aadlPortDirection): (ISZ[String], ISZ[ST], ISZ[ST], ISZ[ST]) = {
      var ports: ISZ[String] = ISZ()
      var portComponents: ISZ[ST] = ISZ()
      var portTypes: ISZ[ST] = ISZ()
      var portDirs: ISZ[ST] = ISZ()

      for(thread <- container.aadlComponents;
        port <- thread.getPorts() if symbolTable.isConnected(port.feature)) {
        ports = ports :+ s"(${port.path})"
        portComponents = portComponents :+ SMT2Template.aadlPortComponents(port.path, thread.path)
        val portType: String = port match {
          case a: AadlDataPort => AadlPortType.AadlDataPort.name
          case a: AadlEventDataPort => AadlPortType.AadlEventDataPort.name
          case a: AadlEventPort => AadlPortType.AadlEventPort.name
          case _ => "UNKNOWN_PORT_TYPE"
        }
        portTypes = portTypes :+ SMT2Template.aadlPortType(port.path, portType)
        val dir: String = if (CommonUtil.isInPort(port.feature)) "In" else "Out"
        portDirs = portDirs :+ SMT2Template.aadlPortDirection(port.path, dir)
      }

      (ports, portComponents, portTypes, portDirs)
    }

    val aadlConnectionFlowTos: ISZ[ST] = container.aadlConnections.
      map((r: (String, String)) => SMT2Template.flowsTo(r._1, r._2))


    val camkesComponents: ISZ[ST] = container.camkesComponents.map((m: String) => st"($m)")

    val camkesPorts: ISZ[ST] = container.camkesPorts.map((s: String) => st"($s)")

    val camkesPortComponents: ISZ[ST] = container.camkesPortConstraints.
      map((r : (ComponentPath, PortPath, Direction, CAmkESPortType)) => SMT2Template.camkesPortComponents(r._2, r._1) )

    val (camkesConnections, camkesConnectionTypes): (ISZ[ST], ISZ[ST]) = {
      var seen: Map[String, Sel4ConnectorTypes.Type] = Map.empty
      var _camkesConTypes:ISZ[ST] = ISZ()
      var _camkesCons: ISZ[ST] = ISZ()
      for(c <- container.camkesConnections) {
        if(seen.contains(c.connectionName)) {
          if(seen.get(c.connectionName).get != c.typ) {
            val msg = s"Found different types for CAmkES connection ${c.connectionName}: ${c.typ} ${seen.get(c.connectionName).get}"
            reporter.error(None(), Util.toolName, msg)
          }
        } else {
          seen = seen + (c.connectionName ~> c.typ)
          _camkesCons = _camkesCons :+ SMT2Template.camkesConnection(c)
          _camkesConTypes = _camkesConTypes :+ SMT2Template.camkesConnectionType(c)
        }
      }
      (_camkesCons, _camkesConTypes)
    }

    val camkesConnectionFlowTos: ISZ[ST] = container.camkesConnections.
      map((r: CAmkESConnection) => SMT2Template.camkesFlowsTo(r))

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
      aadlDispatchProtocols = aadlDispatchProtocols,
      altAadlDispatchProtocols = altAadlDispatchProtocols,

      camkesComponents = camkesComponents,
      camkesPorts = camkesPorts,
      camkesPortComponents = camkesPortComponents,
      camkesConnectionTypes = camkesConnectionTypes,
      camkesConnectionFlowTos = camkesConnectionFlowTos,

      camkesConnections = camkesConnections,

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
