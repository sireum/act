// #Sireum

package org.sireum.hamr.act.templates

import org.sireum._
import org.sireum.hamr.act.ast
import org.sireum.hamr.act.proof.ProofContainer.{AadlPortType, CAmkESConnection, SchedulingType}
import org.sireum.hamr.act.util.{ActPlatform, Sel4ConnectorTypes}
import org.sireum.hamr.codegen.common.symbols.AadlThread

object SMT2Template {
  def portRefinement(aadlPort: String, camkesPort: String): ST = {
    return st"(and (= ap ${aadlPort}) (= cp ${camkesPort}))"
  }

  def componentRefinement(aadlComponent: String, camkesComponent: String): ST = {
    return st"(and (= ac ${aadlComponent}) (= cc ${camkesComponent}))"
  }

  def flowsTo(srcPort: String, dstPort: String): ST = {
    return st"(and (= p1 ${srcPort}) (= p2 ${dstPort}))"
  }

  def aadlPortComponents(aadlComponent: String, aadlPort: String): ST = {
    return st"(assert (= ${aadlPort} (select AADLPortComponent ${aadlComponent})))"
  }

  def aadlDispatchProtocol(aadlThread: AadlThread): ST = {
    return st"(assert (= ${aadlThread.dispatchProtocol.name} (select AADLDispatchProtocol ${aadlThread.path})))"
  }

  def camkesPortComponents(camkesComponent: String, camkesPort: String): ST = {
    return st"(assert (= ${camkesComponent} (select CAmkESPortComponent ${camkesPort})))"
  }

  def aadlPortType(aadlPort: String, portType: String): ST = {
    return st"(assert (= ${portType} (select AADLPortType ${aadlPort})))"
  }

  def aadlPortDirection(aadlPort: String, direction: String): ST = {
    return st"(assert (= ${direction} (select AADLPortDirection ${aadlPort})))"
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
            aadlPorts: ISZ[String],
            aadlPortComponents: ISZ[ST],
            aadlPortTypes: ISZ[ST],
            aadlPortDirection: ISZ[ST],
            aadlConnectionFlowTos: ISZ[ST],
            aadlDispatchProtocols: ISZ[ST],

            altAadlDispatchProtocols: ST,

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

            componentRefinements: ISZ[ST],
            portRefinements: ISZ[ST],

            modelSchedulingType: SchedulingType.Type): ST = {

    val sel4ConnEnums = Sel4ConnectorTypes.elements.map((m: Sel4ConnectorTypes.Type) => st"(${m.name})")

    val aadlPortEnums = AadlPortType.elements.map((m: AadlPortType.Type) => st"(${m.name})")
    val portMatches = AadlPortType.elements.map((m: AadlPortType.Type) =>
      st"(and (= ${m.name} (select AADLPortType src)) (= ${m.name} (select AADLPortType dst)))")

    val schedulingTypes: ISZ[ST] = SchedulingType.elements.map((m: SchedulingType.Type) => st"(${m.name})")

    val accessTypes: ISZ[ST] = ast.AccessType.elements.map((m: ast.AccessType.Type) => st"(${m.name})")

    val modes: ISZ[ST] = ActPlatform.elements.map((m: ActPlatform.Type) => st"(${m.name})")

    val ret: ST =
      st"""(set-logic ALL)
          |
          |(declare-datatypes ((Mode 0)) ((
          |  ${(modes, "\n")})))
          |
          |(declare-datatypes ((ComponentType 0)) ((
          |  (AadlComponent)
          |  (AadlVMComponent)
          |  (PacerComponent)
          |  (PeriodicDispatcher))))
          |
          |(declare-datatypes ((DispatchProtocol 0)) ((
          |  (Periodic)
          |  (Sporadic)
          |  (UNSPECIFIED_DISPATCH_PROTOCOL))))
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
          |(declare-datatypes ((AADLComponent 0)) ((
          |  ${(aadlComponents, "\n")}
          |)))
          |(declare-const AADLComponent_count Int)
          |(assert (= ${aadlComponents.size} AADLComponent_count))
          |
          |(declare-const AADLDispatchProtocol (Array AADLComponent DispatchProtocol))
          |  ${(aadlDispatchProtocols, "\n")}
          |(declare-const AADLDispatchProtocol_size Int)
          |(assert (= ${aadlDispatchProtocols.size} AADLDispatchProtocol_size))
          |
          |(define-fun altAADLDispatchProtocol ((_comp AADLComponent)) DispatchProtocol
          |  ${(altAadlDispatchProtocols, "\n")})
          |(declare-const AADLDispatchProtocol_count Int)
          |(assert (= ${aadlDispatchProtocols.size} AADLDispatchProtocol_count))
          |
          |(declare-datatypes ((AADLPort 0)) ((
          |  ${(aadlPorts, "\n")})))
          |(declare-const AADLPort_count Int)
          |(assert (= ${aadlPorts.size} AADLPort_count))
          |
          |(declare-const AADLPortComponent (Array AADLPort AADLComponent))
          |  ${(aadlPortComponents, "\n")}
          |(declare-const AADLPortComponent_size Int)
          |(assert (= ${aadlPortComponents.size} AADLPortComponent_size))
          |
          |(declare-const AADLPortType (Array AADLPort PortType))
          |  ${(aadlPortTypes, "\n")}
          |(declare-const AADLPortType_size Int)
          |(assert (= ${aadlPortTypes.size} AADLPortType_size))
          |
          |(declare-const AADLPortDirection (Array AADLPort Direction))
          |  ${(aadlPortDirection, "\n")}
          |(declare-const AADLPortDirection_size Int)
          |(assert (= ${aadlPortDirection.size} AADLPortDirection_size))
          |
          |(define-fun AADLConnectionFlowTos ((p1 AADLPort) (p2 AADLPort)) Bool
          |  (or
          |    ${(aadlConnectionFlowTos, "\n")}
          |    false))
          |(declare-const AADLConnectionFlowsTos_count Int)
          |(assert (= ${aadlConnectionFlowTos.size} AADLConnectionFlowsTos_count))
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
          |(define-fun ComponentRefinement ((ac AADLComponent) (cc CAmkESComponent)) Bool
          |  (or
          |    ${(componentRefinements, "\n")}
          |    false))
          |(declare-const ComponentRefinement_count Int)
          |(assert (= ${componentRefinements.size} ComponentRefinement_count))
          |
          |(define-fun PortRefinement ((ap AADLPort) (cp CAmkESPort)) Bool
          |  (or
          |    ${(portRefinements, "\n")}
          |    false))
          |(declare-const PortRefinement_count Int)
          |(assert (= ${portRefinements.size} PortRefinement_count))
          |
          |
          |(define-fun AADLFlowDirectionality () Bool
          |  (forall ((p1 AADLPort) (p2 AADLPort))
          |    (=> (AADLConnectionFlowTos p1 p2)
          |        (and (= Out (select AADLPortDirection p1)) (= In (select AADLPortDirection p2))))))
          |
          |(define-fun AADLFlowNoSelfConnection () Bool
          |  (forall ((p1 AADLPort) (p2 AADLPort))
          |    (=> (AADLConnectionFlowTos p1 p2)
          |        (not (= p1 p2)))))
          |
          |(define-fun AADLConnectedPortTypeMatch () Bool
          |  (forall ((src AADLPort) (dst AADLPort))
          |    (=> (AADLConnectionFlowTos src dst)
          |        (or ${(portMatches, "\n")}
          |             false))))
          |(declare-const AADLConnectedPortTypeMatch_count Int)
          |(assert (= ${portMatches.size} AADLConnectedPortTypeMatch_count))
          |
          |(define-fun AADLDispatchProtocolSpecified () Bool
          |  (forall ((_comp AADLComponent))
          |    (not (= UNSPECIFIED_DISPATCH_PROTOCOL (select AADLDispatchProtocol _comp)))))
          |
          |(define-fun altAADLDispatchProtocolSpecified () Bool
          |  (forall ((_comp AADLComponent))
          |    (not (= UNSPECIFIED_DISPATCH_PROTOCOL (altAADLDispatchProtocol _comp)))))
          |
          |(define-fun AADLWellFormedness () Bool
          |  (and
          |    (= AADLPort_count AADLPortComponent_size) ; all AADL ports belong to an AADL component
          |    altAADLDispatchProtocolSpecified
          |    AADLDispatchProtocolSpecified
          |    AADLFlowDirectionality
          |    AADLFlowNoSelfConnection
          |    AADLConnectedPortTypeMatch))
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
          |  (forall ((aadlComponent1 AADLComponent) (camkesComponent CAmkESComponent))
          |    (=> (ComponentRefinement aadlComponent1 camkesComponent)
          |        (not (exists ((aadlComponent2 AADLComponent))
          |               (and (not (= aadlComponent1 aadlComponent2))
          |                    (ComponentRefinement aadlComponent2 camkesComponent)))))))
          |
          |(define-fun UniquePortRefinements () Bool
          |  (forall ((aadlPort1 AADLPort) (camkesPort CAmkESPort))
          |    (=> (PortRefinement aadlPort1 camkesPort)
          |        (not (exists ((aadlPort2 AADLPort))
          |               (and (not (= aadlPort1 aadlPort2))
          |                    (PortRefinement aadlPort2 camkesPort)))))))
          |
          |(define-fun CAmkESWellFormedness () Bool
          |  (and
          |    (= CAmkESPort_count CAmkESPortComponent_size) ; all CAmkES ports belong to a CAmkES component
          |    CAmkESDataPortAccess
          |    CAmkESFlowNoSelfConnection))
          |
          |(define-fun SB_DataPortRefinement ((aadlSource AADLPort) (aadlDest AADLPort)) Bool
          |  (exists ((conn CAmkESConnection) (camkesSource CAmkESPort) (camkesDest CAmkESPort))
          |      (and (CAmkESConnectionFlowTos conn camkesSource camkesDest)
          |           (= (select CAmkESConnectionType conn) ${Sel4ConnectorTypes.seL4SharedData.name} )
          |           (PortRefinement aadlSource camkesSource)
          |           (PortRefinement aadlDest  camkesDest)
          |           (ComponentRefinement (select AADLPortComponent aadlSource) (select CAmkESPortComponent camkesSource))
          |           (ComponentRefinement (select AADLPortComponent aadlDest) (select CAmkESPortComponent camkesDest)))))
          |
          |(define-fun SB_EventPortRefinement ((aadlSource AADLPort) (aadlDest AADLPort)) Bool
          |  (exists ((conn CAmkESConnection) (camkesSource CAmkESPort) (camkesDest CAmkESPort))
          |    (and
          |      (CAmkESConnectionFlowTos conn camkesSource camkesDest)
          |      (= (select CAmkESConnectionType conn) ${Sel4ConnectorTypes.seL4Notification.name} )
          |      (PortRefinement aadlSource camkesSource)
          |      (PortRefinement aadlDest camkesDest)
          |      (ComponentRefinement (select AADLPortComponent aadlSource) (select CAmkESPortComponent camkesSource))
          |      (ComponentRefinement (select AADLPortComponent aadlDest) (select CAmkESPortComponent camkesDest)))))
          |
          |(define-fun SB_Refinement ((aadlSource AADLPort) (aadlDest AADLPort)) Bool
          |  (and (or (= CodegenMode ${ActPlatform.SeL4.name}) (= CodegenMode ${ActPlatform.SeL4_Only.name}) false)
          |       (or
          |         (and
          |           (= ${AadlPortType.AadlDataPort.name} (select AADLPortType aadlSource))
          |           (SB_DataPortRefinement aadlSource aadlDest)) ; payload
          |         (and
          |           (= ${AadlPortType.AadlEventPort.name} (select AADLPortType aadlSource))
          |           (SB_DataPortRefinement aadlSource aadlDest)   ; event counter
          |           (SB_EventPortRefinement aadlSource aadlDest)) ; event
          |         (and
          |           (= ${AadlPortType.AadlEventDataPort.name} (select AADLPortType aadlSource))
          |           (SB_DataPortRefinement aadlSource aadlDest)   ; payload
          |           (SB_EventPortRefinement aadlSource aadlDest)) ; event
          |         false)))
          |
          |(define-fun TB_Refinement ((aadlSource AADLPort) (aadlDest AADLPort)) Bool
          |  (and (= CodegenMode ${ActPlatform.SeL4_TB.name})
          |       false))
          |
          |(define-fun ConnectionPreservation () Bool
          |  (forall ((aadlSource AADLPort) (aadlDest AADLPort))
          |    (=> (AADLConnectionFlowTos aadlSource aadlDest)
          |      (or (SB_Refinement aadlSource aadlDest)
          |          (TB_Refinement aadlSource aadlDest)
          |          false))))
          |
          |
          |(define-fun isAADLConnectionRefinement ((camkesSource CAmkESPort) (camkesDest CAmkESPort)) Bool
          |  (exists ((aadlSource AADLPort) (aadlDest AADLPort))
          |    (and
          |      (PortRefinement aadlSource camkesSource)
          |      (PortRefinement aadlDest camkesDest)
          |      (ComponentRefinement (select AADLPortComponent aadlSource) (select CAmkESPortComponent camkesSource))
          |      (ComponentRefinement (select AADLPortComponent aadlDest) (select CAmkESPortComponent camkesDest))
          |      (AADLConnectionFlowTos aadlSource aadlDest))))
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
          |        (isAADLConnectionRefinement camkesSource camkesDest)
          |        (isCAmkESSchedulingConnection conn)
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
