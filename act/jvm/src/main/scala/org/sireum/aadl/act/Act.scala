package org.sireum.aadl.act

import org.sireum.aadl.ir
import java.io.File
import java.nio.file.StandardCopyOption

import org.sireum.{B, Either, F, ISZ, Option, Some, String, T, Z}

object Act {

  def main(args: Array[scala.Predef.String]): Unit = {
    val inputFile = path2fileOpt("input file", Some(args(0)), F)
    val input = scala.io.Source.fromFile(inputFile.get).getLines.mkString
    val destDir = new File(args(1))

    ir.JSON.toAadl(input) match {
      case Either.Left(m) => run(destDir, m, ISZ())
      case Either.Right(m) =>
        Console.println(s"Json deserialization error at (${m.line}, ${m.column}): ${m.message}")
    }
  }

  def run(destDir: File, m: ir.Aadl, auxDirectories: ISZ[String]) : Int = {

    if(m.components.isEmpty) {
      Console.err.println("Model is empty")
      return -1
    }

    val _destDir = destDir

    if(!_destDir.exists()) {
      Console.err.println(s"${_destDir} does not exist")
      return -1
    }

    var cFiles: ISZ[String] = ISZ()
    var hFiles: ISZ[String] = ISZ()

    if(auxDirectories.nonEmpty) {
      val auxSrcDir = new File(_destDir, "aux/src")
      val auxIncludesDir = new File(_destDir, "aux/includes")

      auxSrcDir.mkdirs()
      auxIncludesDir.mkdirs()

      def processDir(dir: File): Unit = {
        dir.listFiles().filter(p => p.getName.endsWith(".c")).foreach(f => {
          val dfile = new File(auxSrcDir, f.getName)
          java.nio.file.Files.copy(f.toPath, dfile.toPath, StandardCopyOption.REPLACE_EXISTING)
          cFiles = cFiles :+ s"aux/src/${dfile.getName}"
        })
        dir.listFiles().filter(p => p.getName.endsWith(".h")).foreach(f => {
          val dfile = new File(auxIncludesDir, f.getName)
          java.nio.file.Files.copy(f.toPath, dfile.toPath, StandardCopyOption.REPLACE_EXISTING)
          hFiles = hFiles :+ s"aux/includes/${dfile.getName}"
        })
        dir.listFiles().foreach(f => if (f.isDirectory) processDir(f))
      }

      for (d <- auxDirectories) {
        val dir = new File(d.native)
        if (dir.isDirectory) processDir(dir)
      }
    }

    Gen().process(m, hFiles) match {
      case Some(con) =>
        val out = BijiPrettyPrint ().tempEntry (destDir.getAbsolutePath, con, cFiles)
        return 0
      case _ => return 1
    }
  }

  def path2fileOpt(pathFor: String, path: Option[String], checkExist: B): scala.Option[File] = {
    if (path.isEmpty) return scala.None
    val f = new File(path.get.value)
    if (checkExist && !f.exists) error(s"File '$path' does not exist.")
    return scala.Some(f.getCanonicalFile.getAbsoluteFile)
  }

  def error(msg: Predef.String): Nothing = {
    throw new RuntimeException(msg)
  }
}
