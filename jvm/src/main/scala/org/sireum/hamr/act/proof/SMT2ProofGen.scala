// #Sireum

package org.sireum.hamr.act.proof

import org.sireum._
import org.sireum.hamr.act.ast
import org.sireum.hamr.act.connections.Connections
import org.sireum.hamr.act.proof.ProofContainer.{AadlPortType, CAmkESComponentCategory, CAmkESConnectionType}
import org.sireum.hamr.act.templates.SMT2Template
import org.sireum.hamr.act.util.{ActContainer, ActPlatform, Sel4ConnectorTypes}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlDataPort, AadlEventDataPort, AadlEventPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir.ConnectionInstance

object SMT2ProofGen {
  var resources: ISZ[Resource] = ISZ()

  def genSmt2Proof(proofContainer: ProofContainer,
                   container: ActContainer,
                   symbolTable: SymbolTable,
                   outputDir:String,
                   hamrPlatform: ActPlatform.Type): ISZ[Resource] = {
    resources = ISZ()

    val aadlInstances: ISZ[AadlThread] = symbolTable.getThreads()


    val (aadlComponents, aadlDispatchProtocols, altAadlDispatchProtocols): (ISZ[ST], ISZ[ST], ST) = {
      var aadlComps: ISZ[ST] = ISZ()
      var aadlDPs: ISZ[ST] = ISZ()
      var altAadlDPEntries: ISZ[ST] = ISZ()
      for(t <- aadlInstances) {
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

      for(thread <- aadlInstances;
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

    val aadlConnectionFlowTos: ISZ[ST] = {
      var ret: ISZ[ST] = ISZ()
      for (entry <- symbolTable.outConnections.entries) {
        val srcPath = entry._1

        val handledConns = entry._2.filter((conn: ConnectionInstance) => Connections.isHandledConnection(conn, symbolTable))

        for (conn <- handledConns) {
          val dstPath = CommonUtil.getName(conn.dst.feature.get)
          ret = ret :+ SMT2Template.flowsTo(srcPath, dstPath)
        }
      }
      ret
    }

    var timeServer: Option[ST] = None()
    var periodicDispatcher: Option[ST] = None()
    var pacer: Option[ST] = None()
    var monitors: ISZ[ST] = ISZ()

    val camkesInstances: ISZ[ast.Instance] = {
      var r: ISZ[ast.Instance] = ISZ()
      for(m <- container.models) {
        m match {
          case a: ast.Assembly => r = r ++ a.composition.instances
          case _ =>
        }
      }
      r
    }

    val camkesComponents: ISZ[ST] = {
      var ret: ISZ[ST] = ISZ()
      for(i <- camkesInstances){
        ret = ret :+ st"(${i.name})"

        proofContainer.camkesComponentTypes.get(i.component) match {
          case Some(CAmkESComponentCategory.TimeServer) => timeServer = Some(st"(= _component ${i.name})")
          case Some(CAmkESComponentCategory.PeriodicDispatcher) => periodicDispatcher = Some(st"(= _component ${i.name})")
          case Some(CAmkESComponentCategory.Pacer) => pacer = Some(st"(= _component ${i.name})")
          case Some(CAmkESComponentCategory.Monitor) => monitors = monitors :+ st"(= _component ${i.name})"
          case Some(CAmkESComponentCategory.Refinement) =>
          case Some(CAmkESComponentCategory.VM_Refinement) =>
          case _ =>
            eprintln(s"What is ${i.component}")
        }
      }
      ret
    }

    var camkesPortComponents: ISZ[ST] = ISZ()
    var portRefinements: ISZ[ST] = ISZ()
    val camkesPorts: ISZ[ST] = {
      var _camkesPorts: ISZ[ST]= ISZ()
      for(i <- camkesInstances) {
        i.component match {
          case c: ast.Component =>
            def process(f: ast.CAmkESFeature): ST = {
              val portName = s"${i.name}_${f.name}"

              proofContainer.portRefinementTypes.get(i.name) match {
                case Some(featureMap) =>
                  camkesPortComponents = camkesPortComponents :+ SMT2Template.camkesPortComponents(i.name, portName)

                  featureMap.get(f) match {
                    case Some(refinement) =>
                      portRefinements = portRefinements :+ SMT2Template.portRefinement(refinement.aadlPort.path, portName)
                    case _ =>
                  }
                case _ =>
                  // TODO: handle VMs, TB
                  eprintln(s"Couldn't find ${i.name} in ${proofContainer.portRefinementTypes.keys}")
              }

              return st"($portName)"
            }

            _camkesPorts = _camkesPorts ++
              (for(z <- c.dataports) yield process(z)) ++
              (for(z <- c.emits) yield process(z)) ++
              (for(z <- c.uses) yield process(z)) ++
              (for(z <- c.consumes) yield process(z)) ++
              (for(z <- c.provides) yield process(z))
          case c: ast.LibraryComponent =>
            for(portName <- c.ports) {
              val qportName = s"${i.name}_${portName}"
              _camkesPorts = _camkesPorts :+ st"(${qportName})"
              camkesPortComponents = camkesPortComponents :+ SMT2Template.camkesPortComponents(i.name, qportName)
            }
        }
      }
      _camkesPorts
    }

    var camkesDataPortAccessRestrictions: ISZ[ST] = ISZ()
    for(m <- container.models) {
      m match {
        case a: ast.Assembly =>
          for(s <- a.configuration) {
            s match {
              case ast.DataPortAccessRestriction(comp, port, v) =>
                camkesDataPortAccessRestrictions = camkesDataPortAccessRestrictions :+
                  st"(assert (= ${v.name} (select CAmkESAccessRestrictions ${comp}_${port})))"
              case _ =>
            }
          }
        case _ =>
      }
    }

    val (camkesRefinementConnections,
         camkesRefinementConnectionTypes,
         camkesConnectionRefinementFlowTos,
         camkesPacingConnections,
         camkesSelfPacingConnections,
         camkesPeriodicDispatchingConnections): (ISZ[ST], ISZ[ST], ISZ[ST], ISZ[ST], ISZ[ST], ISZ[ST]) = {

      var _camkesConTypes:ISZ[ST] = ISZ()
      var _camkesCons: ISZ[ST] = ISZ()
      var _camkesFlowsTo: ISZ[ST] = ISZ()
      var _camkesPacingConns: ISZ[ST]= ISZ()
      var _camkesSelfPacingConns: ISZ[ST]= ISZ()
      var _camkesPDConns: ISZ[ST]= ISZ()

      for(holder <- proofContainer.camkesConnections) {
        assert(holder.connection.from_ends.size == 1)

        val fromEnd: ast.ConnectionEnd = holder.connection.from_ends(0)

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
            _camkesPacingConns = _camkesPacingConns :+ st"(= _conn ${holder.connection.name})"
          case CAmkESConnectionType.SelfPacing =>
            _camkesSelfPacingConns = _camkesSelfPacingConns :+ st"(= _conn ${holder.connection.name})"
          case CAmkESConnectionType.PeriodicDispatching =>
            _camkesPDConns = _camkesPDConns :+ st"(= _conn ${holder.connection.name})"
          case _ =>
        }
      }
      (_camkesCons, _camkesConTypes, _camkesFlowsTo, _camkesPacingConns, _camkesSelfPacingConns, _camkesPDConns)
    }

    val componentRefinements: ISZ[ST] = proofContainer.camkesInstances.filter((f: (Option[AadlComponent], ast.Instance)) => f._1.nonEmpty).
      map((r: (Option[AadlComponent], ast.Instance)) => SMT2Template.componentRefinement(r._1.get.path, r._2.name))


    val proof = SMT2Template.proof(
      mode = hamrPlatform,

      aadlComponents = aadlComponents,
      aadlPorts = aadlPorts,
      aadlPortComponents = aadlPortComponents,
      aadlPortTypes = aadlPortTypes,
      aadlPortDirection = aadlPortDirection,
      aadlConnectionFlowTos = aadlConnectionFlowTos,
      aadlDispatchProtocols = aadlDispatchProtocols,
      altAadlDispatchProtocols = altAadlDispatchProtocols,

      camkesComponents = camkesComponents,
      camkesDataPortAccessRestrictions = camkesDataPortAccessRestrictions,
      periodicDispatcherComponent = periodicDispatcher,
      pacerComponent = pacer,
      timeServerComponent = timeServer,
      monitors = monitors,

      camkesPorts = camkesPorts,

      camkesPortComponents = camkesPortComponents,
      camkesConnectionTypes = camkesRefinementConnectionTypes,
      camkesConnectionFlowTos = camkesConnectionRefinementFlowTos,

      camkesConnections = camkesRefinementConnections,
      selfPacingConnections = camkesSelfPacingConnections,
      pacingConnections = camkesPacingConnections,
      periodicDispatchingConnections = camkesPeriodicDispatchingConnections,

      componentRefinements = componentRefinements,
      portRefinements = portRefinements,

      modelSchedulingType = proofContainer.modelSchedulingType
    )

    val path: Os.Path = Os.path(outputDir) / "proof" / "smt2_case.smt2"

    resources = resources :+ ResourceUtil.createResource(
      path = path.value,
      content = proof,
      overwrite = T)

    return resources
  }
}
