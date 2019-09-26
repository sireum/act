package org.sireum.aadl.act

import org.sireum.aadl.ir
import java.io.File
import java.nio.file.StandardCopyOption
import org.sireum.String

import org.sireum.{B, Either, F, ISZ, None, Option, Some, T, Z}

object Act {

  def main(args: Array[Predef.String]): Unit = {
    val inputFile = path2fileOpt("input file", Some(args(0)), F)
    val input = scala.io.Source.fromFile(inputFile.get).getLines.mkString

    ir.JSON.toAadl(input) match {
      case Either.Left(m) => run(None(), m, ISZ(), None())
      case Either.Right(m) =>
        Console.println(s"Json deserialization error at (${m.line}, ${m.column}): ${m.message}")
    }
  }

  def run(optOutputDir: Option[Predef.String], m: ir.Aadl, auxDirectories: ISZ[Predef.String], aadlRootDir: Option[Predef.String]) : Int = {
    run(optOutputDir, m, auxDirectories, aadlRootDir,
      F, ISZ(), None(), None())
  }

  def run(optOutputDir: Option[Predef.String], m: ir.Aadl, auxDirectories: ISZ[Predef.String], aadlRootDir: Option[Predef.String],
          hamrIntegration: B, hamrIncludeDirs: ISZ[Predef.String], hamrStaticLib: Option[Predef.String], hamrBasePackageName: Option[Predef.String]) : Int = {

    val outDir: String = if(optOutputDir.nonEmpty) optOutputDir.get else "."

    val destDir: File = new File(outDir.native)
    if(!destDir.exists()) {
      if(!destDir.mkdirs()){
        Console.err.println(s"Could not create directory ${destDir.getPath}")
        return -1
      }
    }
    if (!destDir.isDirectory) {
      Console.err.println(s"Path ${destDir.getPath} is not a directory")
      return -1
    }


    if(m.components.isEmpty) {
      Console.err.println("Model is empty")
      return -1
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
        val dir = new File(d)
        if (dir.isDirectory) processDir(dir)
      }
    }

    val m1 = if(Util.DEVELOPER_MODE) {
      ir.Transformer(Transformers.UnboundedIntegerRewriter()).transformAadl(F, m).resultOpt match {
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
        Console.err.println(s"${dirName} does not exist or is not a directory")
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
        val f = new File(hamrStaticLib.get)
        Some(Util.relativizePaths(destDir.getCanonicalPath, f.getCanonicalPath, pathSep, "${CMAKE_CURRENT_LIST_DIR}"))
      } else {
        None()
      }
      val _hamrBasePackageName: Option[org.sireum.String] = hamrBasePackageName.map(x => org.sireum.String(x))

      Gen(m2, hamrIntegration, _hamrBasePackageName).process(hFiles) match {
        case Some(con) =>
          val rootDir = aadlRootDir match {
            case Some(f) => new File(f).getAbsolutePath
            case _ => ""
          }
          val out = ActPrettyPrint().tempEntry(destDir.getAbsolutePath, con, cFiles, rootDir, _hamrIncludes, _hamrStaticLib)
          return 0
        case _ =>
      }
    }

    return 1
  }

  def path2fileOpt(pathFor: Predef.String, path: Option[Predef.String], checkExist: B): scala.Option[File] = {
    if (path.isEmpty) return scala.None
    val f = new File(path.get)
    if (checkExist && !f.exists) error(s"File '$path' does not exist.")
    return scala.Some(f.getCanonicalFile.getAbsoluteFile)
  }

  def error(msg: Predef.String): Nothing = {
    throw new RuntimeException(msg)
  }
}
