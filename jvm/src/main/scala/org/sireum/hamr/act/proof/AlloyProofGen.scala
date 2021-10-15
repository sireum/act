// #Sireum
package org.sireum.hamr.act.proof

import org.sireum._
import org.sireum.hamr.act.proof.ProofContainer.{CAmkESPortType, ComponentPath, Direction, PortPath}
import org.sireum.hamr.act.templates.AlloyTemplate
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.util.ResourceUtil

object AlloyProofGen {

  var resources: ISZ[Resource] = ISZ()

  def genAlloyProof(container: ProofContainer,
                    symbolTable: SymbolTable,
                    outputDir: String): ISZ[Resource] = {
    resources = ISZ()

    val base = AlloyTemplate.base()

    val aadlComponents: ISZ[String] = container.aadlComponents.map((m: AadlThread) => m.path)

    val (aadlPorts, aadlPortConstraints): (ISZ[String], ISZ[ST]) = {
      var ports: ISZ[String] = ISZ()
      var portConstraints: ISZ[ST] = ISZ()

      for(thread <- container.aadlComponents;
          port <- thread.getPorts() if symbolTable.isConnected(port.feature)) {
        ports = ports :+ port.path
        val dir: String = if (CommonUtil.isInPort(port.feature)) "In" else "Out"
        portConstraints = portConstraints :+
          st"""${port.path}.component = ${thread.path}
              |${port.path}.direction = ${dir}"""
      }
      (ports, portConstraints)
    }

    val aadlConenctions: ISZ[ST] = container.aadlConnections.
      map((r: (String, String)) => st"${r._1} -> ${r._2}")

    val camkesComponents: ISZ[ST] = container.camkesComponents.map((m: String) => st"$m")

    val camkesPorts: ISZ[ST] = container.camkesPorts.map((s: String) => st"$s")

    val camkesPortConstraints: ISZ[ST] = container.camkesPortConstraints.
      map((r : (ComponentPath, PortPath, Direction, CAmkESPortType)) =>
        st"""${r._2}.component = ${r._1}
            |${r._2}.direction = ${r._3}""")

    val camkesConnections: ISZ[ST] = container.camkesPortConnections.
      map((r: (String, String)) => st"${r._1} -> ${r._2}")


    val componentsRefinements: ISZ[ST] = container.componentRefinements.
      map((r: (AadlThread, String)) => st"${r._1.path} -> ${r._2}")

    val portRefinements: ISZ[ST] = container.portRefinements.
      map((r: (AadlPort, String)) => st"${r._1.path} -> ${r._2}")


    val proof = AlloyTemplate.proof(
      aadlComponents,
      aadlPorts,
      aadlPortConstraints,
      aadlConenctions,
      aadlComponents.size,
      aadlPorts.size,

      camkesComponents,
      camkesPorts,
      camkesPortConstraints,
      camkesConnections,
      camkesComponents.size,
      camkesPorts.size,

      componentsRefinements,
      portRefinements)

    val als: ST = AlloyTemplate.als(base, proof)

    /*
    val frameworkPath: Os.Path = Os.path(outputDir) / "proof" / "alloy_case_framework.als"

    resources = resources :+ Util.createResource(
      path = frameworkPath.value,
      contents = AlloyTemplate.base(),
      overwrite = T)
    */

    val path: Os.Path = Os.path(outputDir) / "proof" / "alloy_case.als"

    resources = resources :+ ResourceUtil.createResource(
      path = path.value,
      content = als,
      overwrite = T)

    val thmPath: Os.Path = Os.path(outputDir) / "proof" / "alloy_case.thm"

    resources = resources :+ ResourceUtil.createResource(
      path = thmPath.value,
      content = AlloyTemplate.theme(),
      overwrite = T)

    return resources
  }
}
