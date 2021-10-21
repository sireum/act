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
    return st"(assert (= ${camkesPort} (select CAmkESPortComponent ${camkesComponent})))"
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
          |; ${aadlComponents.size} AADLComponent
          |(declare-datatypes ((AADLComponent 0)) ((
          |  ${(aadlComponents, "\n")}
          |)))
          |
          |; ${aadlDispatchProtocols.size} AADLDispatchProtocol
          |(declare-const AADLDispatchProtocol (Array AADLComponent DispatchProtocol))
          |${(aadlDispatchProtocols, "\n")}
          |
          |; ${aadlDispatchProtocols.size} altAADLDispatchProtocol
          |(define-fun altAADLDispatchProtocol ((_comp AADLComponent)) DispatchProtocol
          |  ${altAadlDispatchProtocols})
          |
          |; ${aadlPorts.size} AADLPort
          |(declare-datatypes ((AADLPort 0)) ((
          |  ${(aadlPorts, "\n")})))
          |
          |; ${aadlPortComponents.size} AADLPortComponent
          |(declare-const AADLPortComponent (Array AADLPort AADLComponent))
          |${(aadlPortComponents, "\n")}
          |
          |; ${aadlPortTypes.size} AADLPortType
          |(declare-const AADLPortType (Array AADLPort PortType))
          |${(aadlPortTypes, "\n")}
          |
          |; ${aadlPortDirection.size} AADLPortDirection
          |(declare-const AADLPortDirection (Array AADLPort Direction))
          |${(aadlPortDirection, "\n")}
          |
          |; ${aadlConnectionFlowTos.size} AADLConnectionFlowTos
          |(define-fun AADLConnectionFlowTos ((p1 AADLPort) (p2 AADLPort)) Bool
          |  (or
          |    ${(aadlConnectionFlowTos, "\n")}
          |    false)
          |)
          |
          |
          |(declare-datatypes ((AccessType 0)) ((
          |  ${(accessTypes, "\n")})))
          |
          |(declare-datatypes ((seL4PortType 0)) ((
          |  ${(sel4ConnEnums, "\n")})))
          |
          |; ${camkesComponents.size} CAmkESComponent
          |(declare-datatypes ((CAmkESComponent 0)) ((
          |  ${(camkesComponents, "\n")}
          |)))
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
          |
          |
          |; ${camkesPorts.size} CAmkESPort
          |(declare-datatypes ((CAmkESPort 0)) ((
          |  ${(camkesPorts, "\n")}
          |)))
          |
          |(declare-const CAmkESAccessRestrictions (Array CAmkESPort AccessType))
          |${(camkesDataPortAccessRestrictions, "\n")}
          |
          |; ${camkesConnections.size} CAmkESConnection
          |(declare-datatypes ((CAmkESConnection 0)) ((
          |  ${(camkesConnections, "\n")}
          |)))
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
          |
          |; ${camkesConnectionTypes.size} CAmkESConnectionType
          |(declare-const CAmkESConnectionType (Array CAmkESConnection seL4PortType))
          |${(camkesConnectionTypes, "\n")}
          |
          |; ${camkesPortComponents.size} CAmkESPortComponent
          |(declare-const CAmkESPortComponent (Array CAmkESPort CAmkESComponent))
          |${(camkesPortComponents, "\n")}
          |
          |; ${camkesConnectionFlowTos.size} CAmkESConnectionFlowTos
          |(define-fun CAmkESConnectionFlowTos ((_conn CAmkESConnection) (_p1 CAmkESPort) (_p2 CAmkESPort)) Bool
          |  (or
          |    ${(camkesConnectionFlowTos, "\n")}
          |    false))
          |
          |
          |; ${componentRefinements.size} ComponentRefinement
          |(define-fun ComponentRefinement ((ac AADLComponent) (cc CAmkESComponent)) Bool
          |  (or
          |    ${(componentRefinements, "\n")}
          |    false))
          |
          |; ${portRefinements.size} PortRefinement
          |(define-fun PortRefinement ((ap AADLPort) (cp CAmkESPort)) Bool
          |  (or
          |    ${(portRefinements, "\n")}
          |    false))
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
          |        (or
          |          ${portMatches}
          |          false))))
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
