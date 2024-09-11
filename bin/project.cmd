::/*#! 2> /dev/null                                   #
@ 2>/dev/null # 2>nul & echo off & goto BOF           #
if [ -z "${SIREUM_HOME}" ]; then                      #
  echo "Please set SIREUM_HOME env var"               #
  exit -1                                             #
fi                                                    #
exec "${SIREUM_HOME}/bin/sireum" slang run "$0" "$@"  #
:BOF
setlocal
if not defined SIREUM_HOME (
  echo Please set SIREUM_HOME env var
  exit /B -1
)
"%SIREUM_HOME%\bin\sireum.bat" slang run "%0" %*
exit /B %errorlevel%
::!#*/
// #Sireum

import org.sireum._
import org.sireum.project.ProjectUtil._
import org.sireum.project.Project

val common = "hamr-common"

val act = "hamr-act"

val homeDir = Os.slashDir.up.canon

val actJvm = moduleJvmPub(
  id = act,
  baseDir = homeDir,
  jvmDeps = ISZ(common),
  jvmIvyDeps = ISZ(),
  pubOpt = pub(
    desc = "HAMR AADL-to-CAmkES Translator (ACT)",
    url = "github.com/sireum/act",
    licenses = bsd2,
    devs = ISZ(jasonBelt)
  )
)

val project = Project.empty + actJvm

projectCli(Os.cliArgs, project)