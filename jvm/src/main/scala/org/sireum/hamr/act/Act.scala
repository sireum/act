package org.sireum.hamr.act

import org.sireum._

import java.io.File
import java.nio.file.StandardCopyOption

import org.sireum.hamr.ir
import org.sireum.hamr.ir.Transformer
import org.sireum.message.Reporter
import org.sireum.hamr.act.Util.reporter

object Act {

  def run(optOutputDir: Option[String], m: ir.Aadl, auxDirectories: ISZ[String], aadlRootDir: Option[String],
          hamrIntegration: B, hamrIncludeDirs: ISZ[String], hamrStaticLib: Option[String], hamrBasePackageName: Option[String],
          reporter: Reporter) : ACTResult = {

    Util.reporter = reporter

    val files = runInternal(optOutputDir, m, auxDirectories, aadlRootDir,
      hamrIntegration, hamrIncludeDirs, hamrStaticLib, hamrBasePackageName)

    return ACTResult(files)
  }

  private def runInternal(optOutputDir: Option[String], m: ir.Aadl, auxDirectories: ISZ[String], aadlRootDir: Option[String],
                          hamrIntegration: B, hamrIncludeDirs: ISZ[String], hamrStaticLib: Option[String], hamrBasePackageName: Option[String],
                          ) : HashSMap[String, ST] = {

    var files: HashSMap[String, ST] = HashSMap.empty

    val outDir: String = if(optOutputDir.nonEmpty) optOutputDir.get else "."

    val destDir: File = new File(outDir.native)
    if(!destDir.exists()) {
      if(!destDir.mkdirs()){
        reporter.error(None(), Util.toolName, s"Could not create directory ${destDir.getPath}")
        return files
      }
    }
    if (!destDir.isDirectory) {
      reporter.error(None(), Util.toolName, s"Path ${destDir.getPath} is not a directory")
      return files
    }

    if(m.components.isEmpty) {
      reporter.error(None(), Util.toolName, "Model is empty")
      return files
    }

    var cFiles: ISZ[org.sireum.String] = ISZ()
    var hFiles: ISZ[org.sireum.String] = ISZ()

    if(auxDirectories.nonEmpty) {
      val auxSrcDir = new File(destDir, s"${Util.AUX_CODE_DIRECTORY_NAME}/src")
      val auxIncludesDir = new File(destDir, s"${Util.AUX_CODE_DIRECTORY_NAME}/includes")

      auxSrcDir.mkdirs()
      auxIncludesDir.mkdirs()

      def processDir(dir: File): Unit = {
        dir.listFiles().filter(p => p.getName.endsWith(".c")).foreach(f => {
          val dfile = new File(auxSrcDir, f.getName)
          java.nio.file.Files.copy(f.toPath, dfile.toPath, StandardCopyOption.REPLACE_EXISTING)
          cFiles = cFiles :+ s"${Util.AUX_CODE_DIRECTORY_NAME}/src/${dfile.getName}"
        })
        dir.listFiles().filter(p => p.getName.endsWith(".h")).foreach(f => {
          val dfile = new File(auxIncludesDir, f.getName)
          java.nio.file.Files.copy(f.toPath, dfile.toPath, StandardCopyOption.REPLACE_EXISTING)
          hFiles = hFiles :+ s"${Util.AUX_CODE_DIRECTORY_NAME}/includes/${dfile.getName}"
        })
        dir.listFiles().foreach(f => if (f.isDirectory) processDir(f))
      }

      for (d <- auxDirectories) {
        val dir = new File(d.native)
        if (dir.isDirectory) processDir(dir)
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

    def locateCResources(dirName: String): ISZ[org.sireum.String] = {
      val dir = new File(dirName.native)
      if(!dir.exists() || !dir.isDirectory) {
        reporter.error(None(), Util.toolName, s"${dirName} does not exist or is not a directory")
        return ISZ()
      }
      var dirs = dir.listFiles().filter(p => p.isDirectory && !p.getName.contains("CMakeFiles")).flatMap(d => locateCResources(d.getAbsolutePath).elements)
      if(dir.listFiles().filter(f => f.isFile && (f.getName.endsWith(".c") || f.getName.endsWith(".h"))).nonEmpty){
        dirs = org.sireum.String(dir.getAbsolutePath) +: dirs
      }
      ISZ(dirs:_*)
    }

    if(!result.ctx.hasErrors) {
      val pathSep: org.sireum.C = '/'
      val _hamrIncludes:ISZ[org.sireum.String] = hamrIncludeDirs.flatMap(d => locateCResources(d)).map(m => {
        val _m = org.sireum.String(new File(m.native).getCanonicalPath)
        var rm = Util.relativizePaths(destDir.getCanonicalPath, _m, pathSep, "")
        if(rm != _m && org.sireum.ops.StringOps(rm).startsWith(s"${pathSep}")) {
          // was able to relativize, but has a leading path sep so remove it
          rm = org.sireum.ops.StringOps(rm).substring(1, rm.size)
        }
        rm
      })
      val _hamrStaticLib: Option[org.sireum.String] = if(hamrStaticLib.nonEmpty) {
        val f = new File(hamrStaticLib.get.native)
        Some(Util.relativizePaths(destDir.getCanonicalPath, f.getCanonicalPath, pathSep, "${CMAKE_CURRENT_LIST_DIR}"))
      } else {
        None()
      }

      Gen(m2, hamrIntegration, hamrBasePackageName, reporter).process(hFiles) match {
        case Some(con) =>
          val rootDir = aadlRootDir match {
            case Some(f) => new File(f.native).getAbsolutePath
            case _ => ""
          }
          ActPrettyPrint().tempEntry(destDir.getAbsolutePath, con, cFiles, rootDir, _hamrIncludes, _hamrStaticLib, hamrIntegration)
          return files
        case _ =>
      }
    }

    return files
  }

  def path2fileOpt(pathFor: Predef.String, path: Option[Predef.String], checkExist: B): scala.Option[File] = {
    if (path.isEmpty) return scala.None
    val f = new File(path.get)
    if (checkExist && !f.exists) {
      error(s"File '$path' does not exist.")
    }

    return scala.Some(f.getCanonicalFile.getAbsoluteFile)
  }

  def error(msg: Predef.String): Nothing = {
    throw new RuntimeException(msg)
  }
}
