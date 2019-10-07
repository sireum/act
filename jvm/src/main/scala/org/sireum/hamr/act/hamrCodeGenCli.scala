/*
 Copyright (c) 2019, Jason Belt, Kansas State University
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

package org.sireum.hamr.act

import org.sireum._
import org.sireum.cli.CliOpt._

object hamrCodeGenCli {
  val codeGenTool: Tool = Tool(
    name = "hamrCodeGen",
    command = "code-gen",
    description = "Generate code from AADL IR",
    header = "Code Generator",
    usage = "<option>* air-file",
    opts = ISZ(
      Opt(name = "json", longKey = "json", shortKey = Some('j'),
        tpe = Type.Flag(F), description = "Input serialized using Json (otherwise MsgPack assumed)"),
      Opt(name = "verbose", longKey = "verbose", shortKey = None(),
        tpe = Type.Flag(F), description = "Enable verbose mode"),
      Opt(name = "platform", longKey = "platform", shortKey = None(),
        tpe = Type.Choice(name = "HamrPlatform", sep = None(), elements = ISZ("JVM", "Linux", "Cygwin", "MacOS", "seL4", "seL4_Only", "seL4_TB")),
        description = "Target platform")
    ),
    groups = ISZ(
      OptGroup(name = "Slang", opts = ISZ(
        Opt(name = "outputDir", longKey = "output-dir", shortKey = Some('o'),
          tpe = Type.Path(multiple = F, default = Some(".")), description = "Output directory for the generated project files"),
        Opt(name = "packageName", longKey = "package-name", shortKey = None(),
          tpe = Type.Str(sep = None(), default = None()), description = "Base package name for Slang project (output-dir's simple name used if not provided)"),
        Opt(name = "embedArt", longKey = "embed-art", shortKey = None(),
          tpe = Type.Flag(F), description = "Embed ART project files"),
        Opt(name = "devicesAsThreads", longKey = "devices-as-thread", shortKey = None(),
          tpe = Type.Flag(F), description = "Treat AADL devices as threads")
      )),
      OptGroup(name = "Transpiler", opts = ISZ(
        Opt(name = "ipc", longKey = "ipc-mechanism", shortKey = None(),
          tpe = Type.Choice(name = "HamrIpcMechanism", sep = None(), elements = ISZ("SharedMemory", "MessageQueue")),
          description = "IPC communication mechanism (requires 'trans' option)"),
        Opt(name = "slangAuxCodeDirs", longKey = "slang-aux-code-dirs", shortKey = None(),
          tpe = Type.Path(multiple = T, default = None()),
          description = "Auxiliary C source code directory"),
        Opt(name = "slangOutputCDir", longKey = "slang-output-c-dir", shortKey = None(),
          tpe = Type.Path(multiple = F, default = None()),
          description = "Output directory for C artifacts"),
        Opt(name = "excludeComponentImpl", longKey = "exclude-component-impl", shortKey = None(),
          tpe = Type.Flag(F),
          description = "Exclude Slang component implementations"),
        Opt(name = "bitWidth", longKey = "bit-width", shortKey = Some('b'),
          tpe = Type.NumChoice(None(), ISZ(64, 32, 16, 8)),
          description = "Default bit-width for unbounded integer types (e.g., Z)"),
        Opt(name = "maxStringSize", longKey = "max-string-size", shortKey = None(),
          tpe = Type.Num(None(), 100, None(), None()),
          description = "Maximum string size"),
        Opt(name = "maxArraySize", longKey = "max-array-size", shortKey = None(),
          tpe = Type.Num(None(), 100, None(), None()),
          description = "Default maximum sequence size"),
      )),
      OptGroup(name = "CAmkES", opts = ISZ(
        Opt(name = "camkesOutputDir", longKey = "camkes-output-dir", shortKey = Some('o'),
          tpe = Type.Path(multiple = F, default = Some(".")),
          description = "Output directory for the generated CAmkES project files"
        ),
        Opt(name = "camkesAuxCodeDirs", longKey = "camkes-aux-code-dirs", shortKey = Some('a'),
          tpe = Type.Path(multiple = T, default = None()),
          description = "Directories containing C files to be included in CAmkES build"
        ),
        Opt(name = "aadlRootDir", longKey = "aadl-root-dir", shortKey = Some('r'),
          tpe = Type.Path(multiple = F, default = None()),
          description = "Root directory containing the AADL project"
        )
      ))
    )
  )
}
