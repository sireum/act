// #Sireum

package org.sireum.hamr.act.proof

import org.sireum._
import org.sireum.hamr.act.ast.ConnectionEnd
import org.sireum.hamr.act.proof.ProofContainer.{AadlPortType, CAmkESConnectionType, CAmkESPortType, ComponentPath, Direction, PortPath, SchedulingType}
import org.sireum.hamr.act.templates.SMT2Template
import org.sireum.hamr.act.util.{Sel4ConnectorTypes}
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
      map((r : (ComponentPath, PortPath)) => SMT2Template.camkesPortComponents(r._2, r._1) )

    val (camkesRefinementConnections,
         camkesRefinementConnectionTypes,
         camkesConnectionRefinementFlowTos,
         camkesSelfPacingConnection): (ISZ[ST], ISZ[ST], ISZ[ST], ISZ[ST]) = {

      var _camkesConTypes:ISZ[ST] = ISZ()
      var _camkesCons: ISZ[ST] = ISZ()
      var _camkesFlowsTo: ISZ[ST] = ISZ()
      var _camkesSelfPacingConns: ISZ[ST]= ISZ()

      for(holder <- container.camkesConnections) {
        assert(holder.connection.from_ends.size == 1)

        val fromEnd: ConnectionEnd = holder.connection.from_ends(0)

        val src = s"${fromEnd.component}_${fromEnd.end}"

        for (dstEnd <- holder.connection.to_ends) {
          val dst = s"${dstEnd.component}_${dstEnd.end}"
          _camkesFlowsTo = _camkesFlowsTo :+ SMT2Template.camkesFlowsTo(holder.connection.name, src, dst)
        }

        _camkesCons = _camkesCons :+ SMT2Template.camkesConnection(holder.connection.name)

        _camkesConTypes = _camkesConTypes :+ SMT2Template.camkesConnectionType(
          holder.connection.name, Sel4ConnectorTypes.byName(holder.connection.connectionType ).get)

        holder.connType match {
          case CAmkESConnectionType.Pacing =>
          case CAmkESConnectionType.SelfPacing =>
            _camkesSelfPacingConns = _camkesSelfPacingConns :+ st"(= _conn ${holder.connection.name})"
          case CAmkESConnectionType.PeriodicDispatching =>
          case _ =>
        }
      }
      (_camkesCons, _camkesConTypes, _camkesFlowsTo, _camkesSelfPacingConns)
    }

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
      camkesConnectionTypes = camkesRefinementConnectionTypes,
      camkesConnectionFlowTos = camkesConnectionRefinementFlowTos,

      camkesConnections = camkesRefinementConnections,
      selfPacingConnections = camkesSelfPacingConnection,
      pacingConnections = ISZ(),
      periodicDispatchingConnections = ISZ(),

      componentRefinements = componentRefinements,
      portRefinements = portRefinements,

      modelSchedulingType = container.modelSchedulingType
    )

    val path: Os.Path = Os.path(outputDir) / "proof" / "smt2_case.smt2"

    resources = resources :+ ResourceUtil.createResource(
      path = path.value,
      content = proof,
      overwrite = T)

    return resources
  }
}
