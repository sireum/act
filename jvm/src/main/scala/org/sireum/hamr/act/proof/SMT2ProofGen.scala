// #Sireum

package org.sireum.hamr.act.proof

import org.sireum._
import org.sireum.hamr.act.templates.SMT2Template
import org.sireum.hamr.act.util.Util
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.util.ResourceUtil

object SMT2ProofGen {
  var resources: ISZ[Resource] = ISZ()

  def genSmt2Proof(container: ProofContainer,
                   outputDir:String): ISZ[Resource] = {
    resources = ISZ()

    val aadlComponents: ISZ[ST] = container.aadlComponents.map((m: String) => st"($m)")

    val aadlPorts: ISZ[ST] = container.aadlPorts.map((s: String) => st"($s)")

    val aadlPortComponents: ISZ[ST] = container.aadlPortConstraints.
      map((r : (String, String, String)) => SMT2Template.aadlPortComponents(r._2, r._1) )

    val aadlPortDirection: ISZ[ST] = container.aadlPortConstraints.
      map((r : (String, String, String)) => SMT2Template.aadlPortDirection(r._2, r._3) )

    val aadlConnectionFlowTos: ISZ[ST] = container.aadlConnections.
      map((r: (String, String)) => SMT2Template.flowsTo(r._1, r._2))


    val camkesComponents: ISZ[ST] = container.camkesComponents.map((m: String) => st"($m)")

    val camkesPorts: ISZ[ST] = container.camkesPorts.map((s: String) => st"($s)")

    val camkesPortComponents: ISZ[ST] = container.camkesPortConstraints.
      map((r : (String, String, String)) => SMT2Template.camkesPortComponents(r._2, r._1) )

    val camkesPortDirection: ISZ[ST] = container.camkesPortConstraints.
      map((r : (String, String, String)) => SMT2Template.camkesPortDirection(r._2, r._3) )

    val camkesConnectionFlowTos: ISZ[ST] = container.camkesConnections.
      map((r: (String, String)) => SMT2Template.flowsTo(r._1, r._2))


    val componentRefinements: ISZ[ST] = container.componentRefinements.
      map((r: (String, String)) => SMT2Template.componentRefinement(r._1, r._2))

    val portRefinements: ISZ[ST] = container.portRefinements.
      map((r: (String, String)) => SMT2Template.portRefinement(r._1, r._2))


    val proof = SMT2Template.proof(
      aadlComponents = aadlComponents,
      aadlPorts = aadlPorts,
      aadlPortComponents = aadlPortComponents,
      aadlPortDirection = aadlPortDirection,
      aadlConnectionFlowTos = aadlConnectionFlowTos,

      camkesComponents = camkesComponents,
      camkesPorts = camkesPorts,
      camkesPortComponents = camkesPortComponents,
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
