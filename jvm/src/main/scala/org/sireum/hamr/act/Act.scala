package org.sireum.hamr.act

import org.sireum._
import org.sireum.hamr.act.periodic.PeriodicUtil
import org.sireum.hamr.act.proof.{ProofUtil, SMT2ProofGen}
import org.sireum.hamr.act.templates.StringTemplate
import org.sireum.hamr.act.util.Util.reporter
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.Names
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{ExperimentalOptions, ResourceUtil}
import org.sireum.hamr.ir
import org.sireum.message.Reporter

object Act {

  def run(model: ir.Aadl, options: ActOptions, aadlTypes: AadlTypes, symbolTable: SymbolTable, sysReporter: Reporter): ActResult = {
    Util.reporter.setMessages(ISZ())
    ProofUtil.reset()
    val results = runInternal(model, options, aadlTypes, symbolTable)
    sysReporter.reports(Util.reporter.messages)
    return results
  }

  private def runInternal(model: ir.Aadl, options: ActOptions, aadlTypes: AadlTypes, symbolTable: SymbolTable) : ActResult = {

    var resources: ISZ[Resource] = ISZ()

    if (model.components.isEmpty) {
      reporter.error(None(), Util.toolName, "Model is empty")
      return ActResult(resources)
    }

    val basePackageName: String = options.hamrBasePackageName match {
      case Some(b) => b
      case _ => ""
    }

    val auxFiles: ISZ[(String, String)] = options.auxFiles.entries.map(m => {

      val resourceName = s"${options.outputDir}/${Util.AUX_CODE_DIRECTORY_NAME}/${m._1}"
      resources = resources :+ ResourceUtil.createStringResource(resourceName, m._2, T)

      val relName = s"${Util.AUX_CODE_DIRECTORY_NAME}/${m._1}"
      (relName, m._2)
    })

    val auxCFiles: ISZ[String] = auxFiles.filter(f => Os.path(f._1).ext == string"c").map(m => m._1)
    val auxHFiles: ISZ[String] = auxFiles.filter(f => Os.path(f._1).ext == string"h").map(m => m._1)
    val auxHeaderDirectories = (Set.empty ++ auxHFiles.map(m => Os.path(m).up.value)).elements

    val container = Gen(model, symbolTable, aadlTypes, options).process(auxHFiles)

    val slangLibInstanceNames: ISZ[String] = options.platform match {
      case ActPlatform.SeL4 =>
        symbolTable.getThreads().map(m => Names(m.component, basePackageName).componentSingletonType) :+ Util.SlangTypeLibrary
      case _ => ISZ()
    }

    if (ExperimentalOptions.generateRefinementProof(options.experimentalOptions)) {
      //resources = resources ++ AlloyProofGen.genAlloyProof(ProofUtil.proofContainer, symbolTable, options.outputDir)
      ProofUtil.proofContainer.modelSchedulingType = PeriodicUtil.getSchedulingType(symbolTable, options.platform)
      resources = resources ++ SMT2ProofGen.genSmt2Proof(ProofUtil.proofContainer, symbolTable, options.outputDir)
    }

    container match {
      case Some(container) =>
        val rootDir: String = options.aadlRootDirectory match {
          case Some(f) => Os.path(f).abs.value
          case _ => "."
        }
        resources = resources ++ ActPrettyPrint().tempEntry(
          destDir = options.outputDir,
          container = container,
          cFiles = auxCFiles,
          cHeaderDirectories = auxHeaderDirectories,
          aadlRootDir = rootDir,
          slangLibInstanceNames: ISZ[String],
          symbolTable = symbolTable,
          options = options
        )
      case _ =>
    }

    if (!reporter.hasError) {

      val camkesArmVmScript: Option[String] = {
        val c = resources.filter(p => ops.StringOps(p.dstPath).endsWith(PathUtil.CAMKES_ARM_VM_SCRIPT_PATH))
        if (c.nonEmpty) Some(c(0).dstPath)
        else None()
      }

      val runCamkesScript: String = {
        val c = resources.filter(p => ops.StringOps(p.dstPath).endsWith(PathUtil.RUN_CAMKES_SCRIPT_PATH))
        if (c.nonEmpty) c(0).dstPath
        else "??"
      }

      val cakeMLAssemblyLocations = resources.filter(p => ops.StringOps(p.dstPath).endsWith(".S"))
        .map((r: Resource) => r.dstPath)

      reporter.info(None(), Util.ACT_INSTRUCTIONS_MESSAGE_KIND,
        StringTemplate.postGenInstructionsMessage(
          camkesProjDirectory = options.outputDir,
          camkesArmVmScript = camkesArmVmScript,
          cakeMLAssemblyLocations = cakeMLAssemblyLocations,
          runCamkesScript = runCamkesScript,
          hasVM = symbolTable.hasVM()).render)
    }

    return ActResult(resources)
  }

  def error(msg: String): Nothing = {
    throw new RuntimeException(msg.native)
  }
}
