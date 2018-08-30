package org.sireum.aadl.act

import org.sireum.aadl.ir
import java.io.File
import org.sireum.cli.Sireum.path2fileOpt
import org.sireum.{B, F, T, Z, ISZ, Either, String, Some}

object Act {

  def main(args: Array[scala.Predef.String]): Unit = {
    val inputFile = path2fileOpt("input file", Some(args(0)), F)
    val input = scala.io.Source.fromFile(inputFile.get).getLines.mkString
    val destDir = new File(args(1))

    ir.JSON.toAadl(input) match {
      case Either.Left(m) => run(destDir, m)
      case Either.Right(m) =>
        Console.println(s"Json deserialization error at (${m.line}, ${m.column}): ${m.message}")
    }
  }

  def run(destDir: File, m: ir.Aadl) : Int = {

    if(m.components.isEmpty) {
      Console.err.println("Model is empty")
      return -1
    }

    val _destDir = destDir

    if(!_destDir.exists()) {
      Console.err.println(s"${_destDir} does not exist")
      return -1
    }

    val objs = Gen().gen(m)

    //objs.foreach(a => println(JSON.fromASTObject(a, F)))

    val out = BijiPrettyPrint().tempEntry(destDir.getAbsolutePath, objs)

    //out.foreach(o => println(s"${o._1} -> \n${o._2.render}"))

    0
  }
}
