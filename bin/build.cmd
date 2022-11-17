::#! 2> /dev/null                                                                                           #
@ 2>/dev/null # 2>nul & echo off & goto BOF                                                                 #
export SIREUM_HOME=$(cd -P $(dirname "$0")/.. && pwd -P)                                                    #
if [ ! -z ${SIREUM_PROVIDED_SCALA++} ]; then                                                                #
  SIREUM_PROVIDED_JAVA=true                                                                                 #
fi                                                                                                          #
"${SIREUM_HOME}/bin/init.sh"                                                                                #
if [ -n "$COMSPEC" -a -x "$COMSPEC" ]; then                                                                 #
  export SIREUM_HOME=$(cygpath -C OEM -w -a ${SIREUM_HOME})                                                 #
  if [ -z ${SIREUM_PROVIDED_JAVA++} ]; then                                                                 #
    export PATH="${SIREUM_HOME}/bin/win/java":"${SIREUM_HOME}/bin/win/z3":"$PATH"                           #
    export PATH="$(cygpath -C OEM -w -a ${JAVA_HOME}/bin)":"$(cygpath -C OEM -w -a ${Z3_HOME}/bin)":"$PATH" #
  fi                                                                                                        #
elif [ "$(uname)" = "Darwin" ]; then                                                                        #
  if [ -z ${SIREUM_PROVIDED_JAVA++} ]; then                                                                 #
    export PATH="${SIREUM_HOME}/bin/mac/java/bin":"${SIREUM_HOME}/bin/mac/z3/bin":"$PATH"                   #
  fi                                                                                                        #
elif [ "$(expr substr $(uname -s) 1 5)" = "Linux" ]; then                                                   #
  if [ -z ${SIREUM_PROVIDED_JAVA++} ]; then                                                                 #
    if [ "$(uname -m)" = "aarch64" ]; then                                                                  #
      export PATH="${SIREUM_HOME}/bin/linux/arm/java/bin":"$PATH"                                           #
    else                                                                                                    #
      export PATH="${SIREUM_HOME}/bin/linux/java/bin":"${SIREUM_HOME}/bin/linux/z3/bin":"$PATH"             #
    fi                                                                                                      #
  fi                                                                                                        #
fi                                                                                                          #
if [ -f "$0.com" ] && [ "$0.com" -nt "$0" ]; then                                                           #
  exec "$0.com" "$@"                                                                                        #
else                                                                                                        #
  rm -fR "$0.com"                                                                                           #
  exec "${SIREUM_HOME}/bin/sireum" slang run -n "$0" "$@"                                                #
fi                                                                                                          #
:BOF
setlocal
set SIREUM_HOME=%~dp0../
call "%~dp0init.bat"
if defined SIREUM_PROVIDED_SCALA set SIREUM_PROVIDED_JAVA=true
if not defined SIREUM_PROVIDED_JAVA set PATH=%~dp0win\java\bin;%~dp0win\z3\bin;%PATH%
set NEWER=False
if exist %~dpnx0.com for /f %%i in ('powershell -noprofile -executionpolicy bypass -command "(Get-Item %~dpnx0.com).LastWriteTime -gt (Get-Item %~dpnx0).LastWriteTime"') do @set NEWER=%%i
if "%NEWER%" == "True" goto native
del "%~dpnx0.com" > nul 2>&1
"%~dp0sireum.bat" slang run -n "%0" %*
exit /B %errorlevel%
:native
%~dpnx0.com %*
exit /B %errorlevel%
::!#
// #Sireum
import org.sireum._

def usage(): Unit = {
  println("ACT /build")
  println("Usage: ( compile | test )+")
}


if (Os.cliArgs.isEmpty) {
  usage()
  Os.exit(0)
}


val homeBin: Os.Path = Os.slashDir
val home: Os.Path = homeBin.up
val sireum: Os.Path = homeBin / (if (Os.isWin) "sireum.bat" else "sireum")

val proyekName: String = "sireum-proyek"
val project: Os.Path = homeBin / "project4testing.cmd"

val versions = (home / "versions.properties").properties

val cache: Os.Path = Os.env("SIREUM_CACHE") match {
  case Some(p) =>
    val d = Os.path(p)
    if (!d.exists) {
      d.mkdirAll()
    }
    d
  case _ => Os.home / "Downloads" / "sireum"
}

def installCoursier(): Unit = {
  val version = versions.get("org.sireum.version.coursier").get
  val ver = home / "lib" / "coursier.jar.ver"
  if (ver.exists && ver.read == version) {
    return
  }

  val drop = cache / s"coursier-$version.jar"
  if (!drop.exists) {
    println(s"Downloading Coursier $version ...")
    val url = s"https://github.com/coursier/coursier/releases/download/v$version/coursier.jar"
    drop.downloadFrom(url)
    println()
  }

  val coursierJar = home / "lib" / "coursier.jar"
  drop.copyOverTo(coursierJar)

  ver.writeOver(version)
}

def clone(repo: String): Unit = {
  val clean = ops.StringOps(repo).replaceAllChars('-', '_')
  if (!(home / clean).exists) {
    Os.proc(ISZ("git", "clone", "--depth=1", s"https://github.com/sireum/$repo", clean)).at(home).console.runCheck()
  } else {
    Os.proc(ISZ("git", "pull")).at(home / clean).console.runCheck()
  }
  println()
}

def cloneProjects(): Unit = {
  /* Also clone hamr-codgen in order to get the 'common' object.  Kind of
 * strange as hamr-codgen has ACT as a sub-module, though it isn't
 * recursively cloned
 */
  for (m <- ISZ("air", "hamr-codegen", "runtime", "slang")) {
    clone(m)
  }
}

def tipe(): Unit = {
  println("Slang type checking ...")
  Os.proc(ISZ(sireum.string, "proyek", "tipe", "--project", project.string, "--par", "--strict-aliasing", home.string)).
    at(home).console.runCheck()
  println()
}


def compile(): Unit = {
  tipe()

  println("Compiling ...")
  proc"$sireum proyek compile --project $project -n $proyekName --par --sha3 .".at(home).console.runCheck()
  println()
}


def test(): Unit = {
  tipe()

  println("Testing ...")
  val packageNames: String = "org.sireum.hamr.act"
  val names: String = "org.sireum.hamr.act"

  proc"$sireum proyek test --project $project -n $proyekName --par --sha3 --packages $packageNames . $names".at(home).console.runCheck()
  println()
}

installCoursier()

for (i <- 0 until Os.cliArgs.size) {
  Os.cliArgs(i) match {
    case string"compile" =>
      cloneProjects()
      compile()
    case string"test" =>
      cloneProjects()
      test()
    case cmd =>
      usage()
      eprintln(s"Unrecognized command: $cmd")
      Os.exit(-1)
  }
}
