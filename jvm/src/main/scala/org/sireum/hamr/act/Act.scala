package org.sireum.hamr.act

import org.sireum._
import org.sireum.Os
import org.sireum.hamr.ir
import org.sireum.hamr.ir.Transformer
import org.sireum.message.Reporter
import org.sireum.hamr.act.Util.reporter

object Act {

  def run(m: ir.Aadl, options: ActOptions, reporter: Reporter): ActResult = {
    Util.reporter = reporter
    return runInternal(m, options)
  }

  private def runInternal(m: ir.Aadl, options: ActOptions) : ActResult = {

    var resources: ISZ[Resource] = ISZ()

    if(m.components.isEmpty) {
      reporter.error(None(), Util.toolName, "Model is empty")
      return ActResult(resources)
    }
    
    val m1 = if(Util.DEVELOPER_MODE) {
      Transformer(Transformers.UnboundedIntegerRewriter()).transformAadl(F, m).resultOpt match {
        case Some(mod) => mod
        case _ => m
      }
    } else {
      m
    }

    val result = ir.Transformer(Transformers.MissingTypeRewriter()).transformAadl(Transformers.CTX(F, F), m1)
    val m2 = if(result.resultOpt.nonEmpty) result.resultOpt.get else m1

    if(!result.ctx.hasErrors) {
      
      val auxFiles: ISZ[(String, String)] = options.auxFiles.entries.map(m => {
        val resourceName = s"${options.outputDir}/${Util.AUX_CODE_DIRECTORY_NAME}/${m._1}"
        resources = resources :+ Util.createResource(resourceName, st"${m._2}", T)
        
        val relName = s"${Util.AUX_CODE_DIRECTORY_NAME}/${m._1}"
        (relName, m._2)
      })

      val auxCFiles: ISZ[String] = auxFiles.filter(f => Os.path(f._1).ext == string"c").map(m => m._1)
      val auxHFiles: ISZ[String] = auxFiles.filter(f => Os.path(f._1).ext == string"h").map(m => m._1)
      val auxHeaderDirectories = (Set.empty ++ auxHFiles.map(m => Os.path(m).up.value)).elements
      
      val (container, r) = Gen(m2, options.platform, options.hamrBasePackageName, reporter).process(auxHFiles)
      reporter.reports(r.messages)
      
      container match {
        case Some(container) =>
          val rootDir: String = options.aadlRootDirectory match {
            case Some(f) => Os.path(f).abs.value
            case _ => "."
          }
          resources = resources ++ ActPrettyPrint().tempEntry(
            options.outputDir, container, auxCFiles, auxHeaderDirectories, rootDir,
            options.hamrLibs, options.platform)
        case _ =>
      }
    }

    return ActResult(resources)
  }

  def error(msg: String): Nothing = {
    throw new RuntimeException(msg.native)
  }
}
