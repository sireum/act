// #Sireum

package org.sireum.hamr.act.templates

import org.sireum._
import org.sireum.hamr.act.ast
import org.sireum.hamr.act.proof.ProofContainer.{AadlPortType, SchedulingType}
import org.sireum.hamr.act.util.{ActPlatform, Sel4ConnectorTypes}
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlDispatchableComponent, Processor}
import org.sireum.hamr.ir.ComponentCategory

object SMT2Template {
  def portRefinement(aadlPort: String, camkesPort: String): ST = {
    return st"(and (= ap ${aadlPort}) (= cp ${camkesPort}))"
  }

  def componentRefinement(aadlComponent: String, camkesComponent: String): ST = {
    return st"(and (= ac (Some ${aadlComponent})) (= cc ${camkesComponent}))"
  }

  def flowsTo(srcPort: String, dstPort: String): ST = {
    return st"(and (= p1 ${srcPort}) (= p2 ${dstPort}))"
  }

  def aadlPortComponents(aadlComponent: String, aadlPort: String): ST = {
    return st"(assert (= (Some ${aadlComponent}) (select AadlPortComponent ${aadlPort})))"
  }

  def aadlComponentCategory(aadlComponent: AadlComponent): ST = {
    return st"(assert (= (Some ${aadlComponent.component.category.name}) (select AadlComponentCategory ${aadlComponent.path})))"
  }

  def aadlBoundProcessor(aadlComponent: AadlComponent, processor: Processor): ST = {
    return st"(assert (= (Some ${processor.path}) (select ProcessorBindings ${aadlComponent.path})))"
  }

  def aadlDispatchProtocol(aadlComponent: AadlComponent, aadlDispatchableComponent: AadlDispatchableComponent): ST = {
    return st"(assert (= (Some ${aadlDispatchableComponent.dispatchProtocol.name}) (select AadlDispatchProtocol ${aadlComponent.path})))"
  }

  def camkesPortComponents(camkesComponent: String, camkesPort: String): ST = {
    return st"(assert (= ${camkesComponent} (select CAmkESPortComponent ${camkesPort})))"
  }

  def aadlPortType(aadlPort: String, portType: String): ST = {
    return st"(assert (= ${portType} (select AadlPortType ${aadlPort})))"
  }

  def aadlPortDirection(aadlPort: String, direction: String): ST = {
    return st"(assert (= ${direction} (select AadlPortDirection ${aadlPort})))"
  }

  def camkesConnectionType(connName: String, connType: Sel4ConnectorTypes.Type): ST = {
    return st"(assert (= ${connType.name} (select CAmkESConnectionType ${connName})))"
  }

  def camkesFlowsTo(connName: String, src: String, dst: String): ST = {
    return st"(and (= _conn ${connName}) (= _p1 ${src}) (= _p2 ${dst}))"
  }

  def camkesConnection(connName: String): ST = { return st"(${connName})" }

  def proof(mode: ActPlatform.Type,

            aadlComponents: ISZ[ST],
            aadlBoundProcessors: ISZ[ST],
            aadlComponentCategories: ISZ[ST],
            aadlPorts: ISZ[String],
            aadlPortComponents: ISZ[ST],
            aadlPortTypes: ISZ[ST],
            aadlPortDirection: ISZ[ST],
            aadlConnectionFlowTos: ISZ[ST],
            aadlDispatchProtocols: ISZ[ST],

            camkesComponents: ISZ[ST],

            periodicDispatcherComponent: Option[ST],
            pacerComponent: Option[ST],
            timeServerComponent: Option[ST],
            monitors: ISZ[ST],

            camkesPorts: ISZ[ST],
            camkesDataPortAccessRestrictions: ISZ[ST],
            camkesPortComponents: ISZ[ST],
            camkesConnectionTypes: ISZ[ST],
            camkesConnectionFlowTos: ISZ[ST],

            camkesConnections: ISZ[ST],

            selfPacingConnections: ISZ[ST],
            pacingConnections: ISZ[ST],
            periodicDispatchingConnections: ISZ[ST],
            vmConnections: ISZ[ST],

            componentRefinements: ISZ[ST],

            portRefinements: ISZ[ST],
            portVMAuxs: ISZ[ST],

            modelSchedulingType: SchedulingType.Type): ST = {

    val sel4ConnEnums = Sel4ConnectorTypes.elements.map((m: Sel4ConnectorTypes.Type) => st"(${m.name})")

    val aadlPortEnums = AadlPortType.elements.map((m: AadlPortType.Type) => st"(${m.name})")
    val portMatches = AadlPortType.elements.map((m: AadlPortType.Type) =>
      st"(and (= ${m.name} (select AadlPortType src)) (= ${m.name} (select AadlPortType dst)))")

    val schedulingTypes: ISZ[ST] = SchedulingType.elements.map((m: SchedulingType.Type) => st"(${m.name})")

    val accessTypes: ISZ[ST] = ast.AccessType.elements.map((m: ast.AccessType.Type) => st"(${m.name})")

    val modes: ISZ[ST] = ActPlatform.elements.map((m: ActPlatform.Type) => st"(${m.name})")

    val componentCategories: ISZ[ST] = ComponentCategory.elements.map((m: ComponentCategory.Type) => st"(${m.name})")

    val ret: ST =
      st"""(set-logic ALL)
          |
          |(declare-datatypes ((Option 1))
          |  ((par (T) ((Some (value T))
          |             (None)))))
          |
          |(declare-datatypes ((Mode 0)) ((
          |  ${(modes, "\n")})))
          |
          |(declare-datatypes ((ComponentCategory 0)) ((
          |  ${(componentCategories, "\n")})))
          |
          |(declare-datatypes ((DispatchProtocol 0)) ((
          |  (Periodic)
          |  (Sporadic))))
          |
          |(declare-datatypes ((SchedulingType 0)) ((
          |  ${(schedulingTypes, "\n")}
          |  (UNSPECIFIED_SCHEDULING_TYPE))))
          |
          |(declare-datatypes ((Direction 0)) ((
          |  (In)
          |  (Out))))
          |
          |(declare-datatypes ((PortType 0)) ((
          |  ${(aadlPortEnums, "\n")})))
          |
          |
          |(declare-const CodegenMode Mode)
          |(assert (= CodegenMode ${mode}))
          |
          |(declare-const ModelSchedulingType SchedulingType)
          |(assert (= ModelSchedulingType ${modelSchedulingType}))
          |
          |(declare-datatypes ((AadlComponent 0)) ((
          |  ${(aadlComponents, "\n")}
          |)))
          |(declare-const AadlComponent_count Int)
          |(assert (= ${aadlComponents.size} AadlComponent_count))
          |
          |(declare-const AadlComponentCategory (Array AadlComponent (Option ComponentCategory)))
          |  ${(aadlComponentCategories, "\n")}
          |
          |(declare-const ProcessorBindings (Array AadlComponent (Option AadlComponent)))
          |  ${(aadlBoundProcessors, "\n")}
          |
          |(declare-const AadlDispatchProtocol (Array AadlComponent (Option DispatchProtocol)))
          |  ${(aadlDispatchProtocols, "\n")}
          |(declare-const AadlDispatchProtocol_size Int)
          |(assert (= ${aadlDispatchProtocols.size} AadlDispatchProtocol_size))
          |
          |(declare-datatypes ((AadlPort 0)) ((
          |  ${(aadlPorts, "\n")})))
          |(declare-const AadlPort_count Int)
          |(assert (= ${aadlPorts.size} AadlPort_count))
          |
          |(declare-const AadlPortComponent (Array AadlPort (Option AadlComponent)))
          |  ${(aadlPortComponents, "\n")}
          |(declare-const AadlPortComponent_size Int)
          |(assert (= ${aadlPortComponents.size} AadlPortComponent_size))
          |
          |(declare-const AadlPortType (Array AadlPort PortType))
          |  ${(aadlPortTypes, "\n")}
          |(declare-const AadlPortType_size Int)
          |(assert (= ${aadlPortTypes.size} AadlPortType_size))
          |
          |(declare-const AadlPortDirection (Array AadlPort Direction))
          |  ${(aadlPortDirection, "\n")}
          |(declare-const AadlPortDirection_size Int)
          |(assert (= ${aadlPortDirection.size} AadlPortDirection_size))
          |
          |(define-fun AadlConnectionFlowTos ((p1 AadlPort) (p2 AadlPort)) Bool
          |  (or
          |    ${(aadlConnectionFlowTos, "\n")}
          |    false))
          |(declare-const AadlConnectionFlowsTos_count Int)
          |(assert (= ${aadlConnectionFlowTos.size} AadlConnectionFlowsTos_count))
          |
          |
          |(declare-datatypes ((AccessType 0)) ((
          |  ${(accessTypes, "\n")})))
          |
          |(declare-datatypes ((seL4PortType 0)) ((
          |  ${(sel4ConnEnums, "\n")})))
          |
          |(declare-datatypes ((CAmkESComponent 0)) ((
          |  ${(camkesComponents, "\n")})))
          |(declare-const CAmkESComponent_count Int)
          |(assert (= ${camkesComponents.size} CAmkESComponent_count))
          |
          |(define-fun isPeriodicDispatcher ((_component CAmkESComponent)) Bool
          |  (and (= ModelSchedulingType PeriodicDispatching)
          |       (or ${periodicDispatcherComponent}
          |           false)))
          |
          |(define-fun isPacer ((_component CAmkESComponent)) Bool
          |  (and (= ModelSchedulingType Pacing)
          |       (or ${pacerComponent}
          |           false)))
          |
          |(define-fun isTimeServer ((_component CAmkESComponent)) Bool
          |  (and ; TODO - list scenarios where a time server is expected
          |       (or ${timeServerComponent}
          |           false)))
          |
          |(define-fun isMonitor ((_component CAmkESComponent)) Bool
          |  (or ${(monitors, "\n")}
          |      false))
          |(declare-const Monitor_count Int)
          |(assert (= ${monitors.size} Monitor_count))
          |
          |(declare-datatypes ((CAmkESPort 0)) ((
          |  ${(camkesPorts, "\n")})))
          |(declare-const CAmkESPort_count Int)
          |(assert (= ${camkesPorts.size} CAmkESPort_count))
          |
          |(declare-const CAmkESAccessRestrictions (Array CAmkESPort AccessType))
          |  ${(camkesDataPortAccessRestrictions, "\n")}
          |(declare-const CAmkESAccessRestrictions_size Int)
          |(assert (= ${camkesDataPortAccessRestrictions.size} CAmkESAccessRestrictions_size))
          |
          |(declare-datatypes ((CAmkESConnection 0)) ((
          |  ${(camkesConnections, "\n")})))
          |(declare-const CAmkESConnection_count Int)
          |(assert (= ${camkesConnections.size} CAmkESConnection_count))
          |
          |(define-fun isSelfPacingConnection ((_conn CAmkESConnection)) Bool
          |  (and (= ModelSchedulingType SelfPacing)
          |       (or ${(selfPacingConnections, "\n")}
          |           false)))
          |
          |(define-fun isPacingConnection ((_conn CAmkESConnection)) Bool
          |  (and (= ModelSchedulingType Pacing)
          |       (or ${(pacingConnections, "\n")}
          |           false)))
          |
          |(define-fun isPeriodicDispatchingConnection ((_conn CAmkESConnection)) Bool
          |  (and (= ModelSchedulingType PeriodicDispatching)
          |       (or ${(periodicDispatchingConnections, "\n")}
          |           false)))
          |(declare-const PeriodicDispatchingConnection_count Int)
          |(assert (= ${periodicDispatchingConnections.size} PeriodicDispatchingConnection_count))
          |
          |; non Aadl connection refinement connections required by a VM
          |(define-fun isVMAuxConnection ((_conn CAmkESConnection)) Bool
          |  (or ${(vmConnections, "\n")}
          |      false))
          |
          |(declare-const CAmkESConnectionType (Array CAmkESConnection seL4PortType))
          |  ${(camkesConnectionTypes, "\n")}
          |(declare-const CAmkESConnectionType_count Int)
          |(assert (= ${camkesConnectionTypes.size} CAmkESConnectionType_count))
          |
          |(declare-const CAmkESPortComponent (Array CAmkESPort CAmkESComponent))
          |  ${(camkesPortComponents, "\n")}
          |(declare-const CAmkESPortComponent_size Int)
          |(assert (= ${camkesPortComponents.size} CAmkESPortComponent_size))
          |
          |(define-fun CAmkESConnectionFlowTos ((_conn CAmkESConnection) (_p1 CAmkESPort) (_p2 CAmkESPort)) Bool
          |  (or
          |    ${(camkesConnectionFlowTos, "\n")}
          |    false))
          |(declare-const CAmkESConnectionFlowTos_count Int)
          |(assert (= ${camkesConnectionFlowTos.size} CAmkESConnectionFlowTos_count))
          |
          |(define-fun ComponentRefinement ((ac (Option AadlComponent)) (cc CAmkESComponent)) Bool
          |  (or
          |    ${(componentRefinements, "\n")}
          |    false))
          |(declare-const ComponentRefinement_count Int)
          |(assert (= ${componentRefinements.size} ComponentRefinement_count))
          |
          |(define-fun PortRefinement ((ap AadlPort) (cp CAmkESPort)) Bool
          |  (or
          |    ${(portRefinements, "\n")}
          |    false))
          |(declare-const PortRefinement_count Int)
          |(assert (= ${portRefinements.size} PortRefinement_count))
          |
          |(define-fun isVMComponent ((cc CAmkESComponent)) Bool
          |  (exists ((ap AadlComponent))
          |    (and (ComponentRefinement (Some ap) cc)                   ; cc refines ap
          |         (= (Some Process) (select AadlComponentCategory ap)) ; ap is a process
          |         (match (select ProcessorBindings ap) (
          |           ((Some x) (= (Some VirtualProcessor) (select AadlComponentCategory x))) ; ap's bound to virtual processor
          |           (None false))))))
          |
          |(define-fun isVMAuxPort ((cp CAmkESPort)) Bool
          |  (exists ((cc CAmkESComponent))
          |    (and (= cc (select CAmkESPortComponent cp))
          |         (isVMComponent cc)
          |         (or
          |            ${(portVMAuxs, "\n")}
          |            false))))
          |
          |(define-fun AadlFlowDirectionality () Bool
          |  (forall ((p1 AadlPort) (p2 AadlPort))
          |    (=> (AadlConnectionFlowTos p1 p2)
          |        (and (= Out (select AadlPortDirection p1)) (= In (select AadlPortDirection p2))))))
          |
          |(define-fun AadlFlowNoSelfConnection () Bool
          |  (forall ((p1 AadlPort) (p2 AadlPort))
          |    (=> (AadlConnectionFlowTos p1 p2)
          |        (not (= p1 p2)))))
          |
          |(define-fun AadlConnectedPortTypeMatch () Bool
          |  (forall ((src AadlPort) (dst AadlPort))
          |    (=> (AadlConnectionFlowTos src dst)
          |        (or ${(portMatches, "\n")}
          |             false))))
          |(declare-const AadlConnectedPortTypeMatch_count Int)
          |(assert (= ${portMatches.size} AadlConnectedPortTypeMatch_count))
          |
          |(define-fun AadlDispatchProtocolSpecified () Bool
          |  (forall ((_comp AadlComponent))
          |    (match (select AadlComponentCategory _comp) (
          |      ((Some _category_) (
          |        ; threads and virtual processors must have an assigned dispatch protocol, all others are
          |        ; 'don't care' (ie.
          |        match _category_ (
          |          (Thread (not (= (as None (Option DispatchProtocol)) (select AadlDispatchProtocol _comp))))
          |          (VirtualProcessor (not (= (as None (Option DispatchProtocol)) (select AadlDispatchProtocol _comp))))
          |          (_z_ true)
          |        )))
          |      (None false) ; sanity check: all AADL components must have an assigned component category
          |      ))))
          |
          |(define-fun AadlAllPortsAssigned () Bool
          |  (forall ((_p AadlPort))
          |    (not (= (as None (Option AadlComponent)) (select AadlPortComponent _p)))))
          |
          |(define-fun AADLWellFormedness () Bool
          |  (and
          |    (= AadlPort_count AadlPortComponent_size) ; all Aadl ports belong to an Aadl component
          |    AadlAllPortsAssigned
          |    AadlDispatchProtocolSpecified
          |    AadlFlowDirectionality
          |    AadlFlowNoSelfConnection
          |    AadlConnectedPortTypeMatch))
          |
          |
          |(define-fun CAmkESFlowNoSelfConnection () Bool
          |  (forall ((_conn CAmkESConnection) (_p1 CAmkESPort) (_p2 CAmkESPort))
          |    (=> (CAmkESConnectionFlowTos _conn _p1 _p2)
          |        (not (= _p1 _p2)))))
          |
          |(define-fun CAmkESDataPortAccess () Bool
          |  (forall ((_conn CAmkESConnection) (_src CAmkESPort) (_dst CAmkESPort))
          |    (=> (and (CAmkESConnectionFlowTos _conn _src _dst) (= seL4SharedData (select CAmkESConnectionType _conn)))
          |      (and (= W (select CAmkESAccessRestrictions _src))
          |           (= R (select CAmkESAccessRestrictions _dst))))))
          |
          |(define-fun UniqueComponentRefinements () Bool
          |  (forall ((aadlComponent1 AadlComponent) (camkesComponent CAmkESComponent))
          |    (=> (ComponentRefinement (Some aadlComponent1) camkesComponent)
          |        (not (exists ((aadlComponent2 AadlComponent))
          |               (and (not (= aadlComponent1 aadlComponent2))
          |                    (ComponentRefinement (Some aadlComponent2) camkesComponent)))))))
          |
          |(define-fun UniquePortRefinements () Bool
          |  (forall ((aadlPort1 AadlPort) (camkesPort CAmkESPort))
          |    (=> (PortRefinement aadlPort1 camkesPort)
          |        (not (exists ((aadlPort2 AadlPort))
          |               (and (not (= aadlPort1 aadlPort2))
          |                    (PortRefinement aadlPort2 camkesPort)))))))
          |
          |(define-fun CAmkESWellFormedness () Bool
          |  (and
          |    (= CAmkESPort_count CAmkESPortComponent_size) ; all CAmkES ports belong to a CAmkES component
          |    CAmkESDataPortAccess
          |    CAmkESFlowNoSelfConnection))
          |
          |(define-fun SB_DataPortRefinement ((aadlSource AadlPort) (aadlDest AadlPort)) Bool
          |  (exists ((conn CAmkESConnection) (camkesSource CAmkESPort) (camkesDest CAmkESPort))
          |      (and (CAmkESConnectionFlowTos conn camkesSource camkesDest)
          |           (= (select CAmkESConnectionType conn) ${Sel4ConnectorTypes.seL4SharedData.name} )
          |           (PortRefinement aadlSource camkesSource)
          |           (PortRefinement aadlDest  camkesDest)
          |           (ComponentRefinement (select AadlPortComponent aadlSource) (select CAmkESPortComponent camkesSource))
          |           (ComponentRefinement (select AadlPortComponent aadlDest) (select CAmkESPortComponent camkesDest)))))
          |
          |(define-fun SB_EventPortRefinement ((aadlSource AadlPort) (aadlDest AadlPort)) Bool
          |  (exists ((conn CAmkESConnection) (camkesSource CAmkESPort) (camkesDest CAmkESPort))
          |    (and
          |      (CAmkESConnectionFlowTos conn camkesSource camkesDest)
          |      (= (select CAmkESConnectionType conn) ${Sel4ConnectorTypes.seL4Notification.name} )
          |      (PortRefinement aadlSource camkesSource)
          |      (PortRefinement aadlDest camkesDest)
          |      (ComponentRefinement (select AadlPortComponent aadlSource) (select CAmkESPortComponent camkesSource))
          |      (ComponentRefinement (select AadlPortComponent aadlDest) (select CAmkESPortComponent camkesDest)))))
          |
          |(define-fun SB_Refinement ((aadlSource AadlPort) (aadlDest AadlPort)) Bool
          |  (and (or (= CodegenMode ${ActPlatform.SeL4.name}) (= CodegenMode ${ActPlatform.SeL4_Only.name}) false)
          |       (or
          |         (and
          |           (= ${AadlPortType.AadlDataPort.name} (select AadlPortType aadlSource))
          |           (SB_DataPortRefinement aadlSource aadlDest)) ; payload
          |         (and
          |           (= ${AadlPortType.AadlEventPort.name} (select AadlPortType aadlSource))
          |           (SB_DataPortRefinement aadlSource aadlDest)   ; event counter
          |           (SB_EventPortRefinement aadlSource aadlDest)) ; event
          |         (and
          |           (= ${AadlPortType.AadlEventDataPort.name} (select AadlPortType aadlSource))
          |           (SB_DataPortRefinement aadlSource aadlDest)   ; payload
          |           (SB_EventPortRefinement aadlSource aadlDest)) ; event
          |         false)))
          |
          |(define-fun TB_Refinement ((aadlSource AadlPort) (aadlDest AadlPort)) Bool
          |  (and (= CodegenMode ${ActPlatform.SeL4_TB.name})
          |       false))
          |
          |(define-fun ConnectionPreservation () Bool
          |  (forall ((aadlSource AadlPort) (aadlDest AadlPort))
          |    (=> (AadlConnectionFlowTos aadlSource aadlDest)
          |      (or (SB_Refinement aadlSource aadlDest)
          |          (TB_Refinement aadlSource aadlDest)
          |          false))))
          |
          |
          |(define-fun isAadl_SB_ConnectionRefinement ((camkesSource CAmkESPort) (camkesDest CAmkESPort)) Bool
          |  (and (or (= CodegenMode SeL4) (= CodegenMode SeL4_Only) false)
          |       (exists ((aadlSource AadlPort) (aadlDest AadlPort))
          |         (and
          |           (PortRefinement aadlSource camkesSource)
          |           (PortRefinement aadlDest camkesDest)
          |           (ComponentRefinement (select AadlPortComponent aadlSource) (select CAmkESPortComponent camkesSource))
          |           (ComponentRefinement (select AadlPortComponent aadlDest) (select CAmkESPortComponent camkesDest))
          |           (AadlConnectionFlowTos aadlSource aadlDest)))))
          |
          |(define-fun isAadl_TB_ConnectionRefinement ((camkesSource CAmkESPort) (camkesDest CAmkESPort)) Bool
          |  (and (= CodegenMode SeL4_TB)
          |       false)
          |)
          |
          |(define-fun isCAmkESSchedulingConnection ((_conn CAmkESConnection)) Bool
          |  (or
          |    (isSelfPacingConnection _conn)
          |    (isPacingConnection _conn)
          |    (isPeriodicDispatchingConnection _conn)
          |    false))
          |
          |(define-fun NoNewConnections () Bool
          |  (forall ((conn CAmkESConnection) (camkesSource CAmkESPort) (camkesDest CAmkESPort))
          |    (=> (CAmkESConnectionFlowTos conn camkesSource camkesDest)
          |      (or
          |        (isAadl_SB_ConnectionRefinement camkesSource camkesDest)
          |        (isAadl_TB_ConnectionRefinement camkesSource camkesDest)
          |        (isCAmkESSchedulingConnection conn)
          |        (and (isVMAuxConnection conn)
          |             ; sanity check, one of the ports must belong to a vm component
          |             (or (isVMAuxPort camkesSource) (isVMAuxPort camkesDest) false))
          |        false))))
          |
          |
          |(echo "RefinementProof: Shows that there is a model satisfying all the constraints (should be sat):")
          |(push)
          |(assert (and
          |  AADLWellFormedness
          |  CAmkESWellFormedness
          |  ConnectionPreservation
          |  UniqueComponentRefinements
          |  UniquePortRefinements
          |  NoNewConnections
          |))
          |(check-sat)
          |;(get-model)
          |(pop)
          |
          |(echo "AADLWellFormedness: Proves that the generated AADL evidence is well-formed (should be unsat):")
          |(push)
          |(assert (not AADLWellFormedness))
          |(check-sat)
          |(pop)
          |
          |(echo "CAmkESWellFormedness: Proves that the generated CAmkES evidence is well-formed (should be unsat):")
          |(push)
          |(assert (not CAmkESWellFormedness))
          |(check-sat)
          |(pop)
          |
          |(echo "ConnectionPreservation: Proves that the generated CAmkES connections preserve AADL's (should be unsat):")
          |(push)
          |(assert (not ConnectionPreservation))
          |(check-sat)
          |(pop)
          |
          |(echo "NoNewConnections: Proves that the generated CAmkES connections does not contain more than AADL's (should be unsat):")
          |(push)
          |(assert (not NoNewConnections))
          |(check-sat)
          |(pop)
          |
          |
          |(exit)"""

    return ret
  }
}
