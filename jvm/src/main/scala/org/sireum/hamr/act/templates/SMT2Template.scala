// #Sireum

package org.sireum.hamr.act.templates

import org.sireum._
import org.sireum.hamr.act.proof.ProofContainer.{CAmkESPortType, PortPath}
import org.sireum.hamr.act.util.Sel4ConnectorTypes

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

  def camkesPortComponents(camkesComponent: String, camkesPort: String): ST = {
    return st"(assert (= ${camkesPort} (select CAMKESPortComponent ${camkesComponent})))"
  }

  def aadlPortType(aadlPort: String, portType: String): ST = {
    return st"(assert (= ${portType} (select AADLPortType ${aadlPort})))"
  }

  def aadlPortDirection(aadlPort: String, direction: String): ST = {
    return st"(assert (= ${direction} (select AADLPortDirection ${aadlPort})))"
  }

  def camkesPortType(camkesPort: PortPath, camkesPortType: CAmkESPortType): ST = {
    return st"(assert (= ${camkesPortType} (select CAMKESPortType ${camkesPort})))"
  }

  def camkesPortDirection(camkesPort: String, direction: String): ST = {
    return st"(assert (= ${direction} (select CAMKESPortDirection ${camkesPort})))"
  }

  def proof(aadlComponents: ISZ[ST],
            aadlPorts: ISZ[String],
            aadlPortComponents: ISZ[ST],
            aadlPortTypes: ISZ[ST],
            aadlPortDirection: ISZ[ST],
            aadlConnectionFlowTos: ISZ[ST],

            camkesComponents: ISZ[ST],
            camkesPorts: ISZ[ST],
            camkesPortComponents: ISZ[ST],
            camkesPortTypes: ISZ[ST],
            camkesPortDirection: ISZ[ST],
            camkesConnectionFlowTos: ISZ[ST],

            componentRefinements: ISZ[ST],
            portRefinements: ISZ[ST]): ST = {

    val sel4ConnTypes = Sel4ConnectorTypes.elements.map((m: Sel4ConnectorTypes.Type) => st"(${m.name})")
    val ret: ST =
      st"""(set-logic ALL)
          |
          |
          |(declare-datatypes ((Direction 0)) ((
          |  (In)
          |  (Out))))
          |
          |(declare-datatypes ((PortType 0)) ((
          |  (DataPort)
          |  (EventPort)
          |  (EventDataPort))))
          |
          |; ${aadlComponents.size} AADLComponent
          |(declare-datatypes ((AADLComponent 0)) ((
          |  ${(aadlComponents, "\n")}
          |)))
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
          |(declare-datatypes ((seL4PortType 0)) ((
          |  ${(sel4ConnTypes, "\n")})))
          |
          |; ${camkesComponents.size} CAMKESComponent
          |(declare-datatypes ((CAMKESComponent 0)) ((
          |  ${(camkesComponents, "\n")}
          |)))
          |
          |; ${camkesPorts.size} CAMKESPort
          |(declare-datatypes ((CAMKESPort 0)) ((
          |  ${(camkesPorts, "\n")}
          |)))
          |
          |; ${camkesPortTypes.size} CAMKESPortType
          |(declare-const CAMKESPortType (Array CAMKESPort seL4PortType))
          |${(camkesPortTypes, "\n")}
          |
          |; ${camkesPortDirection.size} CAMKESPortDirection
          |(declare-const CAMKESPortDirection (Array CAMKESPort Direction))
          |${(camkesPortDirection, "\n")}
          |
          |; ${camkesPortComponents.size} CAMKESPortComponent
          |(declare-const CAMKESPortComponent (Array CAMKESPort CAMKESComponent))
          |${(camkesPortComponents, "\n")}
          |
          |; ${camkesConnectionFlowTos.size} CAMKESConnectionFlowTos
          |(define-fun CAMKESConnectionFlowTos ((p1 CAMKESPort) (p2 CAMKESPort)) Bool
          |  (or
          |    ${(camkesConnectionFlowTos, "\n")}
          |    false))
          |
          |
          |; ${componentRefinements.size} ComponentRefinement
          |(define-fun ComponentRefinement ((ac AADLComponent) (cc CAMKESComponent)) Bool
          |  (or
          |    ${(componentRefinements, "\n")}
          |    false))
          |
          |; ${portRefinements.size} PortRefinement
          |(define-fun PortRefinement ((ap AADLPort) (cp CAMKESPort)) Bool
          |  (or
          |    ${(portRefinements, "\n")}
          |    false))
          |
          |
          |(define-fun AADLFlowDirectionality () Bool
          |  (forall ((p1 AADLPort) (p2 AADLPort))
          |    (=>
          |      (AADLConnectionFlowTos p1 p2)
          |      (and (= Out (select AADLPortDirection p1)) (= In (select AADLPortDirection p2))))))
          |
          |(define-fun AADLFlowNoSelfConnection () Bool
          |  (forall ((p1 AADLPort) (p2 AADLPort))
          |    (=>
          |      (AADLConnectionFlowTos p1 p2)
          |      (not (= p1 p2)))))
          |
          |(define-fun AADLConnectedPortTypeMatch () Bool
          |  (forall ((src AADLPort) (dst AADLPort))
          |    (=>
          |      (AADLConnectionFlowTos src dst)
          |      (or
          |          (and (= DataPort (select AADLPortType src)) (= DataPort (select AADLPortType dst)))
          |          (and (= EventPort (select AADLPortType src)) (= EventPort (select AADLPortType dst)))
          |          (and (= EventDataPort (select AADLPortType src)) (= EventDataPort (select AADLPortType dst)))
          |          false))))
          |
          |(define-fun AADLWellFormedness () Bool
          |  (and
          |    AADLFlowDirectionality
          |    AADLFlowNoSelfConnection
          |    AADLConnectedPortTypeMatch))
          |
          |
          |(define-fun CAMKESFlowDirectionality () Bool
          |  (forall ((p1 CAMKESPort) (p2 CAMKESPort))
          |    (=>
          |      (CAMKESConnectionFlowTos p1 p2)
          |      (and (= Out (select CAMKESPortDirection p1)) (= In (select CAMKESPortDirection p2))))))
          |
          |(define-fun CAMKESFlowNoSelfConnection () Bool
          |  (forall ((p1 CAMKESPort) (p2 CAMKESPort))
          |    (=>
          |      (CAMKESConnectionFlowTos p1 p2)
          |      (not (= p1 p2)))))
          |
          |(define-fun CAMKESConnectedPortTypeMatch () Bool
          |  (forall ((src CAMKESPort) (dst CAMKESPort))
          |    (=>
          |      (CAMKESConnectionFlowTos src dst)
          |      (or
          |          (and (= seL4SharedData (select CAMKESPortType src)) (= seL4SharedData (select CAMKESPortType dst)))
          |          (and (= seL4Notification (select CAMKESPortType src)) (= seL4Notification (select CAMKESPortType dst)))
          |          false))))
          |
          |(define-fun CAMKESWellFormedness () Bool
          |  (and
          |    CAMKESFlowDirectionality
          |    CAMKESFlowNoSelfConnection
          |    CAMKESConnectedPortTypeMatch))
          |
          |
          |(define-fun DataPortRefinement ((aadlSource AADLPort) (aadlDest AADLPort)) Bool
          |  (exists ((camkesSource CAMKESPort) (camkesDest CAMKESPort))
          |    (and
          |      (= seL4SharedData (select CAMKESPortType camkesSource))
          |      (PortRefinement aadlSource camkesSource)
          |      (PortRefinement aadlDest camkesDest)
          |      (ComponentRefinement (select AADLPortComponent aadlSource) (select CAMKESPortComponent camkesSource))
          |      (ComponentRefinement (select AADLPortComponent aadlDest) (select CAMKESPortComponent camkesDest))
          |      (CAMKESConnectionFlowTos camkesSource camkesDest))))
          |
          |(define-fun EventPortRefinement ((aadlSource AADLPort) (aadlDest AADLPort)) Bool
          |  (exists ((camkesSource CAMKESPort) (camkesDest CAMKESPort))
          |    (and
          |      (= seL4Notification (select CAMKESPortType camkesSource))
          |      (PortRefinement aadlSource camkesSource)
          |      (PortRefinement aadlDest camkesDest)
          |      (ComponentRefinement (select AADLPortComponent aadlSource) (select CAMKESPortComponent camkesSource))
          |      (ComponentRefinement (select AADLPortComponent aadlDest) (select CAMKESPortComponent camkesDest))
          |      (CAMKESConnectionFlowTos camkesSource camkesDest))))
          |
          |(define-fun ConnectionPreservation () Bool
          |  (forall ((aadlSource AADLPort) (aadlDest AADLPort))
          |    (=> (AADLConnectionFlowTos aadlSource aadlDest)  ; if there is a flow from src to dst
          |        (or
          |          (and
          |               (= DataPort (select AADLPortType aadlSource)) ; aadl source port is a data port
          |               (DataPortRefinement aadlSource aadlDest))
          |          (and
          |               (= EventPort (select AADLPortType aadlSource)) ; aadl source port is an event port
          |               (DataPortRefinement aadlSource aadlDest) ; event counter
          |               (EventPortRefinement aadlSource aadlDest))
          |          (and
          |               (= EventDataPort (select AADLPortType aadlSource)) ; aadl source port is an event data port
          |               (DataPortRefinement aadlSource aadlDest)
          |               (EventPortRefinement aadlSource aadlDest))
          |           false))))
          |
          |(define-fun NoNewConnections () Bool
          |  (forall ((camkesSource CAMKESPort) (camkesDest CAMKESPort))
          |    (=> (CAMKESConnectionFlowTos camkesSource camkesDest)
          |        (exists ((aadlSource AADLPort) (aadlDest AADLPort))
          |          (and
          |            (PortRefinement aadlSource camkesSource)
          |            (PortRefinement aadlDest camkesDest)
          |            (ComponentRefinement (select AADLPortComponent aadlSource) (select CAMKESPortComponent camkesSource))
          |            (ComponentRefinement (select AADLPortComponent aadlDest) (select CAMKESPortComponent camkesDest))
          |            (AADLConnectionFlowTos aadlSource aadlDest))))))
          |
          |
          |(echo "RefinementProof: Shows that there is a model satisfying all the constraints (should be sat):")
          |(push)
          |(assert (and
          |  AADLWellFormedness
          |  CAMKESWellFormedness
          |  ConnectionPreservation
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
          |(echo "CAMKESWellFormedness: Proves that the generated CAMKES evidence is well-formed (should be unsat):")
          |(push)
          |(assert (not CAMKESWellFormedness))
          |(check-sat)
          |(pop)
          |
          |(echo "ConnectionPreservation: Proves that the generated CAMKES connections preserve AADL's (should be unsat):")
          |(push)
          |(assert (not ConnectionPreservation))
          |(check-sat)
          |(pop)
          |
          |(echo "NoNewConnections: Proves that the generated CAMKES connections does not contain more than AADL's (should be unsat):")
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
