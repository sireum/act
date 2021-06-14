/*
 Copyright (c) 2017-2021, Robby, Kansas State University
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

import mill._
import ammonite.ops._
import $file.Act
import $file.air.Air
import $file.hamr_codegen.Codegen
import $file.runtime.Runtime

import mill.scalalib.ScalaModule
import org.sireum.mill.SireumModule

object runtime extends mill.Module {

  object macros extends Runtime.Module.Macros

  object test extends Runtime.Module.Test {
    override def macrosObject = macros
  }

  trait testProvider extends Runtime.Module.TestProvider {
    override def testObject = test
  }

  object library extends Runtime.Module.Library with testProvider {
    override def macrosObject = macros
  }

}

object air extends Air.Module with runtime.testProvider {
  final override def libraryObject = runtime.library
}

object hamr_codegen extends Module {

  object common extends Codegen.Module.Common {
    final override def airObject = air
  }

}

object act extends Act.Module {
  final override def millSourcePath = super.millSourcePath / up

  final override def airObject = air

  final override def commonObject = hamr_codegen.common

}


object bin extends ScalaModule {
  final override def scalaVersion = SireumModule.scalaVersion
  final override def moduleDeps = Seq(runtime.library.jvm)
}
