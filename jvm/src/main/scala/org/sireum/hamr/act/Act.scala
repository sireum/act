package org.sireum.hamr.act

import org.sireum._

import org.sireum.ops.StringOps
import org.sireum.Os
import org.sireum.hamr.ir
import org.sireum.hamr.ir.Transformer
import org.sireum.message.Reporter
import org.sireum.hamr.act.Util.reporter

object Act {

  def run(optOutputDir: Option[String], m: ir.Aadl, auxDirectories: ISZ[String], aadlRootDir: Option[String],
          platform: ActPlatform.Type, hamrIncludeDirs: ISZ[String], hamrStaticLib: Option[String], hamrBasePackageName: Option[String],
          reporter: Reporter) : ActResult = {

    Util.reporter = reporter

    return runInternal(optOutputDir, m, auxDirectories, aadlRootDir,
      platform, hamrIncludeDirs, hamrStaticLib, hamrBasePackageName)
  }

  private def runInternal(optOutputDir: Option[String], m: ir.Aadl, auxDirectories: ISZ[String], aadlRootDir: Option[String],
                          platform: ActPlatform.Type, hamrIncludeDirs: ISZ[String], hamrStaticLib: Option[String], hamrBasePackageName: Option[String],
                          ) : ActResult = {
    var resources: ISZ[Resource] = ISZ()

    val outDir: String = if(optOutputDir.nonEmpty) optOutputDir.get else "."

    val destDir = Os.path(outDir)
    if(!destDir.exists) {
      destDir.mkdirAll()
    }
    if (!destDir.isDir) {
      reporter.error(None(), Util.toolName, s"Path ${destDir.value} is not a directory")
      return ActResult(resources)
    }

    if(m.components.isEmpty) {
      reporter.error(None(), Util.toolName, "Model is empty")
      return ActResult(resources)
    }

    var cFiles: ISZ[String] = ISZ()
    var hFiles: ISZ[String] = ISZ()

    if(auxDirectories.nonEmpty) {
      val auxSrcDir = Os.path(s"${destDir}/${Util.AUX_CODE_DIRECTORY_NAME}/src")
      val auxIncludesDir = Os.path(s"${destDir}/${Util.AUX_CODE_DIRECTORY_NAME}/includes")

      auxSrcDir.mkdirAll()
      auxIncludesDir.mkdirAll()

      def processDir(dir: Os.Path): Unit = {
        dir.list.filter(p => p.ext == String("c")).foreach(f => {
          val destFile = Os.path(s"${auxSrcDir}/${f.name}")
          resources = resources :+ Util.createResource(destFile.canon.value, st"${f.read}", T)

          cFiles = cFiles :+ s"${Util.AUX_CODE_DIRECTORY_NAME}/src/${destFile.name}"
        })

        dir.list.filter(p => p.ext == String("h")).foreach(f => {
          val destFile = Os.path(s"${auxIncludesDir}/${f.name}")
          resources = resources :+ Util.createResource(destFile.canon.value, st"${f.read}", T)

          hFiles = hFiles :+ s"${Util.AUX_CODE_DIRECTORY_NAME}/includes/${destFile.name}"
        })

        dir.list.foreach(f => if(f.isDir) processDir(f))
      }

      for (d <- auxDirectories) {
        val dir = Os.path(d)
        if (dir.isDir) processDir(dir)
      }
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

    def locateCResources(dirName: String): ISZ[String] = {
      val dir = Os.path(dirName)
      if(!dir.exists || !dir.isDir) {
        reporter.error(None(), Util.toolName, s"${dirName} does not exist or is not a directory")
        return ISZ()
      }

      var dirs:ISZ[String] = dir.list.filter(p => p.isDir && !StringOps(p.name).contains("CMakeFiles")).flatMap(d => locateCResources(d.abs.value))

      if(dir.list.filter(f => f.isFile && (f.ext == String("c") || f.ext == String("h"))).nonEmpty){
        dirs = dir.abs.value +: dirs
      }

      return dirs
    }

    if(!result.ctx.hasErrors) {
      val pathSep: org.sireum.C = '/'
      val _hamrIncludes:ISZ[String] = hamrIncludeDirs.flatMap(d => locateCResources(d)).map(m => {
        val _m = Os.path(m).canon.value
        var rm = Util.relativizePaths(destDir.canon.value, _m, pathSep, "")
        if(rm != _m && StringOps(rm).startsWith(s"${pathSep}")) {
          // was able to relativize, but has a leading path sep so remove it
          rm = StringOps(rm).substring(1, rm.size)
        }
        rm
      })
      val _hamrStaticLib: Option[String] = if(hamrStaticLib.nonEmpty) {
        val f = Os.path(hamrStaticLib.get)
        Some(Util.relativizePaths(destDir.canon.value, f.canon.value, pathSep, "${CMAKE_CURRENT_LIST_DIR}"))
      } else {
        None()
      }

      Gen(m2, platform, hamrBasePackageName, reporter).process(hFiles) match {
        case Some(con) =>
          val rootDir: String = aadlRootDir match {
            case Some(f) => Os.path(f).abs.value
            case _ => "'"
          }
          resources = resources ++ ActPrettyPrint().tempEntry(destDir.abs.value, con, cFiles, rootDir, _hamrIncludes, _hamrStaticLib, platform)
        case _ =>
      }
    }

    return ActResult(resources)
  }

  def path2fileOpt(pathFor: Predef.String, path: Option[String], checkExist: B): scala.Option[Os.Path] = {
    if (path.isEmpty) return scala.None
    val f = Os.path(path.get)
    if (checkExist && !f.exists) {
      error(s"File '$path' does not exist.")
    }

    return scala.Some(f.canon)
  }

  def error(msg: String): Nothing = {
    throw new RuntimeException(msg.native)
  }
}
