// #Sireum
package org.sireum.hamr.act

import org.sireum._
import org.sireum.hamr.act.proof.ProofUtil
import org.sireum.hamr.act.templates.StringTemplate
import org.sireum.hamr.act.util.Util.reporter
import org.sireum.hamr.act.util._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{NameUtil, ResourceUtil}
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

  def runInternal(model: ir.Aadl, options: ActOptions, aadlTypes: AadlTypes, symbolTable: SymbolTable): ActResult = {

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

    val auxCFiles: ISZ[String] = auxFiles.filter(f => Os.path(f._1).ext == "c").map(m => m._1)
    val auxHFiles: ISZ[String] = auxFiles.filter(f => Os.path(f._1).ext == "h").map(m => m._1)
    val auxHeaderDirectories = (Set.empty[String] ++ auxHFiles.map((m: String) => Os.path(m).up.value)).elements

    val container = Gen(model, symbolTable, aadlTypes, options).process(auxHFiles)

    val slangLibInstanceNames: ISZ[String] = options.platform match {
      case ActPlatform.SeL4 =>
        symbolTable.getThreads().map((m: AadlThread) => NameUtil.getAirNameProvider(m.component, basePackageName).componentSingletonType) :+ Util.SlangTypeLibrary
      case _ => ISZ()
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
          slangLibInstanceNames = slangLibInstanceNames,
          symbolTable = symbolTable,
          options = options
        )

      case _ =>
    }

    if (!reporter.hasError) {

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
          cakeMLAssemblyLocations = cakeMLAssemblyLocations,
          runCamkesScript = runCamkesScript,
          hasVM = symbolTable.hasVM()).render)
    }

    return ActResult(resources)
  }

  def error(msg: String): Unit = {
    halt(msg)
  }
}
