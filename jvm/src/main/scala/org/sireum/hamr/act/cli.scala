/*
 Copyright (c) 2017-2024, Jason Belt, Kansas State University
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

object cli {

  val actTool: Tool = Tool(
    name = "act",
    command = "act",
    description = "AADL to CAmkES translator",
    header = "Sireum ACT: An AADL-to-CAmkES Translator",
    usage = "<option>* <file>+",
    usageDescOpt = None(),
    opts = ISZ(
      Opt(name = "input", longKey = "input", shortKey = Some('i'),
        tpe = Type.Choice(name = "actFormat", sep = None(), elements = ISZ("air", "camkesir", "aadl")),
        description = "Input format"
      ),
      Opt(name = "mode", longKey = "mode", shortKey = Some('m'),
        tpe = Type.Choice(name = "actMode", sep = None(), elements = ISZ("json", "msgpack")),
        description = "Serialization method (only valid for air/camkesir input"
      ),
      Opt(name = "outputDir", longKey = "output-dir", shortKey = Some('o'),
        tpe = Type.Path(multiple = F, default = Some(".")),
        description = "Output directory for the generated project files"
      ),
      Opt(name = "auxDirs", longKey = "aux-directories", shortKey = Some('a'),
        tpe = Type.Path(multiple = T, default = None()),
        description = "Directories containing C files to be included in build"
      ),
      Opt(name = "aadlRootDir", longKey = "root-dir", shortKey = Some('r'),
        tpe = Type.Path(multiple = F, default = None()),
        description = ""
      ),
      Opt(name = "hamr", longKey = "hamr", shortKey = Some('h'),
        tpe = Type.Flag(F),
        description = "Perform HAMR integration"
      ),
      Opt(name = "hamrIncludeDirs", longKey = "hamr-include-dirs", shortKey = None(),
        tpe = Type.Path(multiple = T, default = None()),
        description = "Directory to scan for C header files"
      ),
      Opt(name = "hamrStaticLib", longKey = "hamr-static-lib", shortKey = None(),
        tpe = Type.Path(multiple = F, default = None()),
        description = "Path to HARM static library"
      ),
      Opt(name = "hamrBasePackageName", longKey = "hamr-base-package-name", shortKey = None(),
        tpe = Type.Str(sep = None(), default = None()),
        description = "Slang base package name")
    ),
    groups = ISZ()
  )
}
