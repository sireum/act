// #Sireum

package org.sireum.hamr.act.proof

import org.sireum._
import org.sireum.hamr.act.ast
import org.sireum.hamr.act.connections.{Connections, SBConnectionContainer}
import org.sireum.hamr.act.proof.ProofContainer.{AadlPortType, CAmkESComponentCategory, CAmkESConnection, CAmkESConnectionType}
import org.sireum.hamr.act.templates.SMT2Template
import org.sireum.hamr.act.util.{ActPlatform, Sel4ConnectorTypes}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlDataPort, AadlDispatchableComponent, AadlEventDataPort, AadlEventPort, AadlPort, AadlProcess, AadlThread, AadlVirtualProcessor, Processor, SymbolTable}
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir.ConnectionInstance

object SMT2ProofGen {
  var resources: ISZ[Resource] = ISZ()

  def genSmt2Proof(proofContainer: ProofContainer,
                   astObjects: ISZ[ast.ASTObject],
                   sbConnectionContainer: Map[String, SBConnectionContainer],
                   symbolTable: SymbolTable,
                   outputDir: String,
                   hamrPlatform: ActPlatform.Type): ISZ[Resource] = {
    resources = ISZ()

    val aadlInstances: ISZ[AadlComponent] = {
      val x: ISZ[AadlComponent] = (for(t <- symbolTable.getThreads()) yield
        if(t.toVirtualMachine(symbolTable)) t.getParent(symbolTable) else t)
      (Set.empty[AadlComponent] ++ x).elements
    }

    val (aadlComponents, aadlBoundProcessors, aadlComponentCategories, aadlDispatchProtocols): (ISZ[ST], ISZ[ST], ISZ[ST], ISZ[ST]) = {
      var aadlComps: ISZ[ST] = ISZ()
      var aadlCategories: ISZ[ST] = ISZ()
      var aadlBPs: ISZ[ST] = ISZ()
      var aadlDPs: ISZ[ST] = ISZ()
      for(t <- aadlInstances) {
        val pos: Option[ST] =
          t.component.identifier.pos match {
            case Some(pos) => Some(st" declared at ${pos.uriOpt} (${pos.beginLine}, ${pos.beginColumn})")
            case _ => None()
          }
        aadlComps = aadlComps :+ st"(${t.path})"
        val dp: AadlDispatchableComponent = t match {
          case a:AadlThread => a
          case p:AadlProcess => p.getBoundProcessor(symbolTable).get.asInstanceOf[AadlVirtualProcessor]
          case _ => halt("Infeasible")
        }
        aadlDPs = aadlDPs :+ SMT2Template.aadlDispatchProtocol(t, dp)
        aadlCategories = aadlCategories :+ SMT2Template.aadlComponentCategory(t)

        val bp: Option[Processor] = t match {
          case at: AadlThread => at.getParent(symbolTable).getBoundProcessor(symbolTable)
          case ap: AadlProcess => ap.getBoundProcessor(symbolTable)
          case _ => halt("infeasible")
        }

        bp match {
          case Some (boundProcessor) =>
            aadlComps = aadlComps :+ st"(${boundProcessor.path})"
            aadlCategories = aadlCategories :+ SMT2Template.aadlComponentCategory(boundProcessor)
            aadlBPs = aadlBPs :+ SMT2Template.aadlBoundProcessor(t, boundProcessor)
          case _ =>
        }
      }

      (aadlComps, aadlBPs, aadlCategories, aadlDPs)
    }

    val (aadlPorts, aadlPortComponents, aadlPortTypes, aadlPortDirection): (ISZ[String], ISZ[ST], ISZ[ST], ISZ[ST]) = {
      var ports: ISZ[String] = ISZ()
      var portComponents: ISZ[ST] = ISZ()
      var portTypes: ISZ[ST] = ISZ()
      var portDirs: ISZ[ST] = ISZ()

      for(aadlInstance <- aadlInstances) {
        val connectedPorts: ISZ[AadlPort] = {
          var _ports: Set[AadlPort] = Set.empty
          for (port <- aadlInstance.getPorts()) {
            for(sb <- sbConnectionContainer.values if port == sb.srcPort || port == sb.dstPort) {
              _ports = _ports + port
            }
          }
          _ports.elements
        }

        for (port <- connectedPorts) {
          ports = ports :+ s"(${port.path})"
          portComponents = portComponents :+ SMT2Template.aadlPortComponents(aadlInstance.path, port.path)
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
      }
      (ports, portComponents, portTypes, portDirs)
    }

    val aadlConnectionFlowTos: ISZ[ST] = {
      sbConnectionContainer.values.map((m: SBConnectionContainer) => SMT2Template.flowsTo(m.srcPort.path, m.dstPort.path))
    }

    var timeServer: Option[ST] = None()
    var periodicDispatcher: Option[ST] = None()
    var pacer: Option[ST] = None()
    var monitors: ISZ[ST] = ISZ()

    val camkesInstances: ISZ[ast.Instance] = {
      var r: ISZ[ast.Instance] = ISZ()
      for(m <- astObjects) {
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
    var portVMAuxsEntries: Map[String, ISZ[ST]] = Map.empty

    val camkesPorts: ISZ[ST] = {
      var _camkesPorts: ISZ[ST]= ISZ()
      for(i <- camkesInstances) {
        i.component match {
          case c: ast.Component =>
            def process(f: ast.CAmkESFeature): ST = {
              val portName = s"${i.name}_${f.name}"

              camkesPortComponents = camkesPortComponents :+ SMT2Template.camkesPortComponents(i.name, portName)

              proofContainer.portRefinementTypes.get(i.name) match {
                case Some(featureMap) =>

                  featureMap.get(f) match {
                    case Some(pf: PortRefinement) =>
                      portRefinements = portRefinements :+ SMT2Template.portRefinement(pf.aadlPort.path, portName)
                    case Some(vmpf: PortVMRefinement) =>
                      portRefinements = portRefinements :+ SMT2Template.portRefinement(vmpf.metaPort.aadlPort.path, portName)
                    case Some(pvma: PortVMAux) =>
                      var entries = portVMAuxsEntries.getOrElse(i.name, ISZ[ST]())
                      entries = entries :+ st"(= cp ${portName})"
                      portVMAuxsEntries = portVMAuxsEntries + (i.name ~> entries)
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

    val portVMAuxs: ISZ[ST] =
      portVMAuxsEntries.entries.map(x => st"(or (= cc ${x._1}) ${(x._2, " ")} false)")

    var camkesDataPortAccessRestrictions: ISZ[ST] = ISZ()
    for(m <- astObjects) {
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

    val (camkesConnections,
         camkesRefinementConnectionTypes,
         camkesConnectionRefinementFlowTos,
         camkesPacingConnections,
         camkesSelfPacingConnections,
         camkesPeriodicDispatchingConnections,
         camkesVMConnections): (ISZ[ST], ISZ[ST], ISZ[ST], ISZ[ST], ISZ[ST], ISZ[ST], ISZ[ST]) = {

      var _camkesConTypes:ISZ[ST] = ISZ()
      var _camkesCons: ISZ[ST] = ISZ()
      var _camkesFlowsTo: ISZ[ST] = ISZ()
      var _camkesPacingConns: ISZ[ST]= ISZ()
      var _camkesSelfPacingConns: ISZ[ST]= ISZ()
      var _camkesPDConns: ISZ[ST]= ISZ()
      var _camkesVMConns: ISZ[ST]= ISZ()

      var seenConns: Set[CAmkESConnection] = Set.empty
      for(holder <- proofContainer.camkesConnections if !seenConns.contains(holder)) {
        assert(holder.connection.from_ends.size == 1)

        seenConns = seenConns + holder

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
          case CAmkESConnectionType.VM =>
            _camkesVMConns = _camkesVMConns :+ st"(= _conn ${holder.connection.name})"

          case CAmkESConnectionType.Refinement =>
          case CAmkESConnectionType.VMRefinement =>
          case _ =>
        }
      }
      (_camkesCons, _camkesConTypes, _camkesFlowsTo, _camkesPacingConns, _camkesSelfPacingConns, _camkesPDConns, _camkesVMConns)
    }

    val componentRefinements: ISZ[ST] = proofContainer.camkesInstances.filter((f: (Option[AadlComponent], ast.Instance)) => f._1.nonEmpty).
      map((r: (Option[AadlComponent], ast.Instance)) => SMT2Template.componentRefinement(r._1.get.path, r._2.name))


    def uniquiIfy(sts: ISZ[ST]): ISZ[ST] = {
      return (Set.empty[String] ++ sts.map((st: ST) => st.render)).elements.map((s: String) => st"$s")
    }

    val proof = SMT2Template.proof(
      mode = hamrPlatform,

      aadlComponents = uniquiIfy(aadlComponents),
      aadlBoundProcessors = aadlBoundProcessors,
      aadlComponentCategories = aadlComponentCategories,
      aadlPorts = aadlPorts,
      aadlPortComponents = aadlPortComponents,
      aadlPortTypes = aadlPortTypes,
      aadlPortDirection = aadlPortDirection,
      aadlConnectionFlowTos = aadlConnectionFlowTos,
      aadlDispatchProtocols = aadlDispatchProtocols,

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

      camkesConnections = camkesConnections,

      selfPacingConnections = camkesSelfPacingConnections,
      pacingConnections = camkesPacingConnections,
      periodicDispatchingConnections = camkesPeriodicDispatchingConnections,
      vmConnections = camkesVMConnections,

      componentRefinements = componentRefinements,
      portRefinements = portRefinements,
      portVMAuxs = portVMAuxs,

      modelSchedulingType = proofContainer.modelSchedulingType
    )

    val path: String= "proof/smt2_case.smt2"

    resources = resources :+ ResourceUtil.createResource(
      path = path,
      content = proof,
      overwrite = T)

    return resources
  }
}
