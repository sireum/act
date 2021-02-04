// #Sireum

package org.sireum.hamr.act.templates

import org.sireum._

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

  def aadlPortDirection(aadlPort: String, direction: String): ST = {
    return st"(assert (= ${direction} (select AADLPortDirection ${aadlPort})))"
  }

  def camkesPortDirection(camkesPort: String, direction: String): ST = {
    return st"(assert (= ${direction} (select CAMKESPortDirection ${camkesPort})))"
  }

  def proof(aadlComponents: ISZ[ST],
            aadlPorts: ISZ[ST],
            aadlPortComponents: ISZ[ST],
            aadlPortDirection: ISZ[ST],
            aadlConnectionFlowTos: ISZ[ST],

            camkesComponents: ISZ[ST],
            camkesPorts: ISZ[ST],
            camkesPortComponents: ISZ[ST],
            camkesPortDirection: ISZ[ST],
            camkesConnectionFlowTos: ISZ[ST],

            componentRefinements: ISZ[ST],
            portRefinements: ISZ[ST]): ST = {

    val ret: ST =
      st"""(set-logic ALL)
          |
          |
          |(declare-datatypes ((Direction 0)) ((
          |  (In)
          |  (Out))))
          |
          |; ${aadlComponents.size} AADLComponent
          |(declare-datatypes ((AADLComponent 0)) ((
          |  ${(aadlComponents, "\n")})))
          |
          |; ${aadlPorts.size} AADLPort
          |(declare-datatypes ((AADLPort 0)) ((
          |  ${(aadlPorts, "\n")})))
          |
          |; ${aadlPortComponents.size} AADLPortComponent
          |(declare-const AADLPortComponent (Array AADLPort AADLComponent))
          |${(aadlPortComponents, "\n")}
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
          |(define-fun AADLWellFormedness () Bool
          |  (and
          |    AADLFlowDirectionality
          |    AADLFlowNoSelfConnection))
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
          |(define-fun CAMKESWellFormedness () Bool
          |  (and
          |    CAMKESFlowDirectionality
          |    CAMKESFlowNoSelfConnection))
          |
          |
          |(define-fun ConnectionPreservation () Bool
          |  (forall ((aadlSource AADLPort) (aadlDest AADLPort))
          |    (=> (AADLConnectionFlowTos aadlSource aadlDest)
          |        (exists ((camkesSource CAMKESPort) (camkesDest CAMKESPort))
          |          (and
          |            (PortRefinement aadlSource camkesSource)
          |            (PortRefinement aadlDest camkesDest)
          |            (ComponentRefinement (select AADLPortComponent aadlSource) (select CAMKESPortComponent camkesSource))
          |            (ComponentRefinement (select AADLPortComponent aadlDest) (select CAMKESPortComponent camkesDest))
          |            (CAMKESConnectionFlowTos camkesSource camkesDest))))))
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
          |(echo "Shows that there is a model satisfying all the constraints (should be sat):")
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
          |(echo "Proves that the generated AADL evidence is well-formed (should be unsat):")
          |(push)
          |(assert (not AADLWellFormedness))
          |(check-sat)
          |(pop)
          |
          |(echo "Proves that the generated CAMKES evidence is well-formed (should be unsat):")
          |(push)
          |(assert (not CAMKESWellFormedness))
          |(check-sat)
          |(pop)
          |
          |(echo "Proves that the generated CAMKES connections preserve AADL's (should be unsat):")
          |(push)
          |(assert (not ConnectionPreservation))
          |(check-sat)
          |(pop)
          |
          |(echo "Proves that the generated CAMKES connections does not contain more than AADL's (should be unsat):")
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
