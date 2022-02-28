// #Sireum
package org.sireum.hamr.act.templates

import org.sireum._
import org.sireum.hamr.act.vm.VM_Template

object ScriptTemplate {

  def runCamkesScript(hasVM: B): ST = {
    val camkesDir: String = if(hasVM) { "camkes-vm-examples" } else { "camkes" }
    val camkesGitLoc: String = if(hasVM) { "https://github.com/camkes-vm-examples" } else { "https://docs.sel4.systems/projects/camkes" }

    val CAMKES_DIR: String = "CAMKES_DIR"
    val NON_INTERACTIVE: String = "NON_INTERACTIVE"
    val SIMULATE: String = "SIMULATE"
    val CAMKES_OPTIONS: String = "CAMKES_OPTIONS"

    val bt: String = "\\"

    val buildSim: ST = if(hasVM) {
      st"""../init-build.sh $${${CAMKES_OPTIONS}} ${bt}
          |    -DPLATFORM=qemu-arm-virt ${bt}
          |    -DQEMU_MEMORY=2048 ${bt}
          |    -DARM_HYP=ON ${bt}
          |    -DCAMKES_VM_APP=$$HAMR_CAMKES_PROJ
          |
          |ninja"""
    } else {
      st"""../init-build.sh $${${CAMKES_OPTIONS}} -DCAMKES_APP=$$HAMR_CAMKES_PROJ
          |
          |ninja"""
    }
    val qemu: ST = if(hasVM) {
      st"""qemu-system-aarch64 \\
          |    -machine virt,virtualization=on,highmem=off,secure=off \\
          |    -cpu cortex-a53 \\
          |    -nographic \\
          |    -m size=2048 \\
          |    -kernel images/capdl-loader-image-arm-qemu-arm-virt"""
    } else {
      st"""qemu-system-x86_64 \\
          |    -cpu Nehalem,-vme,+pdpe1gb,-xsave,-xsaveopt,-xsavec,-fsgsbase,-invpcid,enforce \\
          |    -nographic \\
          |    -serial mon:stdio \\
          |    -m size=512M \\
          |    -kernel images/kernel-x86_64-pc99 \\
          |    -initrd images/capdl-loader-image-x86_64-pc99"""
    }

    val CAMKES_APPS_DIR: ST =
      if(hasVM) st"$${${CAMKES_DIR}}/projects/vm-examples/apps/Arm/$$HAMR_CAMKES_PROJ"
      else st"$${${CAMKES_DIR}}/projects/camkes/apps/$$HAMR_CAMKES_PROJ"

    val ret: ST = st"""#!/usr/bin/env bash
                      |
                      |set -o errexit -o pipefail -o nounset
                      |
                      |export SCRIPT_HOME=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
                      |export PROJECT_HOME=$$( cd "$$( dirname "$$0" )/.." &> /dev/null && pwd )
                      |cd $${PROJECT_HOME}
                      |
                      |! getopt --test > /dev/null
                      |if [[ $${PIPESTATUS[0]} -ne 4 ]]; then
                      |  echo '`getopt --test` failed in this environment.'
                      |  exit 1
                      |fi
                      |
                      |${NON_INTERACTIVE}=false
                      |${CAMKES_DIR}=""
                      |${SIMULATE}=false
                      |${CAMKES_OPTIONS}=""
                      |
                      |OPTIONS=c:no:sh
                      |LONGOPTS=camkes-dir:,non-interactive,camkes-options:,simulate,help
                      |
                      |function usage {
                      |  echo ""
                      |  echo "Usage: <option>*"
                      |  echo ""
                      |  echo "Available Options:"
                      |  echo "-c, --camkes-dir       Location of CAmkES project"
                      |  echo "-n, --non-interactive  Non-interactive mode.  Symlink in apps directory will be replaced"
                      |  echo "                         if present"
                      |  echo "-o, --camkes-options   CAmkES options (e.g -o ${bt}"-DWITH_LOC=ON -DCapDLLoaderMaxObjects=40000${bt}")"
                      |  echo "-s, --simulate         Simulate via QEMU"
                      |  echo "-h, --help             Display this information"
                      |}
                      |
                      |! PARSED=$$(getopt --options=$$OPTIONS --longoptions=$$LONGOPTS --name "$$0" -- "$$@")
                      |if [[ $${PIPESTATUS[0]} -ne 0 ]]; then
                      |    usage
                      |    exit 2
                      |fi
                      |
                      |eval set -- "$$PARSED"
                      |
                      |while true; do
                      |  case "$$1" in
                      |    -c|--camkes-dir) ${CAMKES_DIR}="$$2"; shift 2 ;;
                      |    -n|--non-interactive) ${NON_INTERACTIVE}=true; shift ;;
                      |    -o|--camkes-options) ${CAMKES_OPTIONS}="$$2"; shift 2 ;;
                      |    -s|--simulate) ${SIMULATE}=true; shift ;;
                      |    -h|--help) usage; exit 0 ;;
                      |    --) shift; break ;;
                      |  esac
                      |done
                      |
                      |# handle non-option arguments
                      |if [[ $$# -ne 0 ]]; then
                      |  echo "$$0: Unexpected non-option arguments"
                      |  usage
                      |  exit 3
                      |fi
                      |
                      |# if CAMKES_DIR option not set then look in some common locations
                      |if [[ -z "$${${CAMKES_DIR}}" && -d "/host/camkes-project" ]]; then
                      |  # docker location
                      |  ${CAMKES_DIR}="/host/camkes-project"
                      |elif [[ -z "$$${CAMKES_DIR}" && -d "$${HOME}/CASE/${camkesDir}" ]]; then
                      |  # CASE Vagrant VM location
                      |  ${CAMKES_DIR}="$${HOME}/CASE/${camkesDir}"
                      |fi
                      |
                      |if [[ -z "$${${CAMKES_DIR}}" || ! -d "$${${CAMKES_DIR}}" ]]; then
                      |  echo "Directory '$${${CAMKES_DIR}}' does not exist.  Please specify the location of your ${camkesDir} project directory."
                      |  echo "See ${camkesGitLoc}"
                      |  exit -1
                      |fi
                      |
                      |
                      |# use the directory name for the CAmkES apps directory name
                      |HAMR_CAMKES_PROJ=$${PWD##*/}
                      |
                      |
                      |CAMKES_APPS_DIR=$CAMKES_APPS_DIR
                      |
                      |# create a sym-link to the project in the CAmkES app directory
                      |if [ -e "$${CAMKES_APPS_DIR}" ]; then
                      |  if [ "$${${NON_INTERACTIVE}}" = true ]; then
                      |    rm -rf $${CAMKES_APPS_DIR}
                      |  else
                      |    read -p "The following app directory already exists, replace $${CAMKES_APPS_DIR} [Y|y]? " -n 1 -r; echo
                      |    if [[ $$REPLY =~ ^[Yy]$$ ]]; then
                      |      rm -rf $${CAMKES_APPS_DIR}
                      |    else
                      |      exit -1
                      |    fi
                      |  fi
                      |fi
                      |
                      |ln -sv $$PROJECT_HOME $$CAMKES_APPS_DIR
                      |
                      |########################
                      |# run CAmkES/seL4 build
                      |########################
                      |
                      |BUILD_DIR=$${${CAMKES_DIR}}/build_$$HAMR_CAMKES_PROJ
                      |
                      |if [ -e "$${BUILD_DIR}" ]; then
                      |  if [ "$${${NON_INTERACTIVE}}" = true ];then
                      |    rm -rf $${BUILD_DIR}
                      |    mkdir $${BUILD_DIR}
                      |  else
                      |    read -p "The following build directory already exists, replace $${BUILD_DIR} [Y|y]? " -n 1 -r; echo
                      |    if [[ $$REPLY =~ ^[Yy]$$ ]]; then
                      |      rm -rf $${BUILD_DIR}
                      |      mkdir $${BUILD_DIR}
                      |    fi
                      |  fi
                      |else
                      |  mkdir $${BUILD_DIR}
                      |fi
                      |
                      |cd $${BUILD_DIR}
                      |
                      |${buildSim}
                      |
                      |########################
                      |# simulate via QEMU
                      |########################
                      |
                      |cat >$${BUILD_DIR}/sim << EOL
                      |#!/usr/bin/env bash
                      |
                      |export SCRIPT_HOME=${bt}$$( cd "${bt}$$( dirname "${bt}$$0" )" &> /dev/null && pwd )
                      |cd ${bt}$${SCRIPT_HOME}
                      |
                      |# console output from simulation disappears when QEMU shuts down when run from
                      |# the CAmkES generated ./simulate script. Instead call QEMU directly using the
                      |# default values ./simulate would pass
                      |
                      |${qemu}
                      |EOL
                      |
                      |chmod 700 $${BUILD_DIR}/sim
                      |echo "Wrote: $${BUILD_DIR}/sim"
                      |
                      |if [ "$${${SIMULATE}}" = true ]; then
                      |  # $${BUILD_DIR}/simulate
                      |  $${BUILD_DIR}/sim
                      |fi
                      |"""
    return ret
  }
}
