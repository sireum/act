/*
 Copyright (c) 2018, Jason Belt, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package org.sireum

import java.io.File
import Cli.{Format, Mode}
import org.sireum.aadl.ir.{Aadl, JSON, MsgPack}

object Act extends scala.App{
  Cli(File.pathSeparatorChar).parseSireum(ISZ(args.toSeq.map(s => s: String):_ *), 0) match {
    case Some(o: Cli.ActOption) => act(o)
    case Some(_: Cli.HelpOption) => 0
    case _ => -1
  }

  def act(o: Cli.ActOption): Int = {
    o.args.size match {
      case z"0" => println(o.help); return 0
      case _ =>
    }

    val outputDir = path2fileOpt("output directory", o.outputDir, F).get
    if(!outputDir.exists()) {
      if(!outputDir.mkdirs()) {
        eprintln(s"Could not create directory: ${outputDir.getPath}")
        return -1
      }
    }
    if(!outputDir.isDirectory) {
      eprintln(s"${outputDir.getPath} is not a directory")
      return -1
    }

    o.input match {
      case Format.Aadl =>
        eprintln("AADL is not currently supported")
        return -1
      case Format.Air =>
        if(o.args.length > 1) {
          eprintln(s"Only expecting a single AIR input file")
          return -1
        }
        val input: String = path2fileOpt("input file", Some(o.args(0)), T) match {
          case Some(file) => scala.io.Source.fromFile(file).getLines().mkString
          case _ => return -1
        }
        val aadl: Aadl = o.mode match {
          case Mode.Json =>
            JSON.toAadl(input) match {
              case Either.Left(m) => m
              case Either.Right(m) =>
                eprintln(s"Json deserialization error at (${m.line}, ${m.column}: ${m.message}")
                return -1
            }
          case Mode.Msgpack =>
            conversions.String.fromBase64(input) match {
              case Either.Left(u) =>
                MsgPack.toAadl(u) match {
                  case Either.Left(m) => m
                  case Either.Right(m) =>
                    eprintln(s"MsgPack deserialization error at offset (${m.offset}: ${m.message}")
                    return -1
                }
              case Either.Right(m) =>
                eprintln(s"Input is not a valid Base64 encoded string: ${m}")
                return -1
            }
        }

        org.sireum.aadl.act.Act.run(outputDir, aadl)

      case Format.Camkesir =>
        eprintln("CAmkES IR is not currently supported")
        -1
    }

  }

  def path2fileOpt(pathFor: String, path: Option[String], checkExist: B): Option[File] = {
    if (path.isEmpty) return None()
    val f = new File(path.get.value)
    if (checkExist && !f.exists) {
      eprintln(s"File '${path}' does not exist.")
      return None()
    }
    return Some(f.getCanonicalFile.getAbsoluteFile)
  }
}