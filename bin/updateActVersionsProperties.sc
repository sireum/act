::/*#! 2> /dev/null                                           #
@ 2>/dev/null # 2>nul & echo off & goto BOF                   #
if [ -f "$0.com" ] && [ "$0.com" -nt "$0" ]; then             #
  exec "$0.com" "$@"                                          #
fi                                                            #
rm -f "$0.com"                                                #
if [ -z "${SIREUM_HOME}" ]; then                              #
  echo "Please set SIREUM_HOME env var"                       #
  exit -1                                                     #
fi                                                            #
"exec ${SIREUM_HOME}/bin/sireum" slang run -n "$0" "$@"       #
:BOF
if not defined SIREUM_HOME (
  echo Please set SIREUM_HOME env var
  exit /B -1
)
"%SIREUM_HOME%\bin\sireum.bat" slang run -n "%0" %*
exit /B %errorlevel%
::!#*/
// #Sireum

import org.sireum._

val SIREUM_HOME = Os.path(Os.env("SIREUM_HOME").get)
val sireum = SIREUM_HOME / "bin" / "sireum"
val actVersionsProps = SIREUM_HOME / "hamr"/ "codegen" / "act" / "resources" / "act_versions.properties"

val vm_repo_dir = Os.home / "CASE" / "camkes-arm-vm" / "projects" / "camkes-vm"

def assertResourceExists(o: ISZ[Os.Path]): Unit = { o.foreach((x: Os.Path) => assert(x.exists, s"${x} doesn't exist")) }
assertResourceExists(ISZ(SIREUM_HOME, actVersionsProps, vm_repo_dir))

var props = actVersionsProps.properties

def runGit(args: ISZ[String], path: Os.Path): String = {
  val p = org.sireum.Os.proc(args).at(path).runCheck()
  return ops.StringOps(p.out).trim
}

def update(key: String, currentVersion: String): B = {
  val propsVersion = if(!props.contains(key)) "MISSING" else props.get(key).get
  if(propsVersion != currentVersion) {
    println(s"Updating ${key}: ${propsVersion} -> ${currentVersion}")
    val escaped = ops.StringOps(currentVersion).replaceAllChars(' ', '\u0020')
    props = props + (key ~> escaped)
    return T
  } else {
    return F
  }
}

val cur_vm_h_version = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%h", "components/VM_Arm/configurations/vm.h"), vm_repo_dir)

var updated = update("vm.h", cur_vm_h_version)

if(updated) {
  val pst = st"""${(props.entries.map(m => st"${m._1}=${m._2}"), "\n")}""".render
  actVersionsProps.writeOver(pst)
  println(s"$actVersionsProps updated -- expect error to follow")

  Os.exit(1) // return 1 to indicate versions have changed
} else {
  println(s"No ACT updates needed")
  Os.exit(0)
}

