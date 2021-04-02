// #Sireum
package org.sireum.hamr.act.templates

import org.sireum._
import org.sireum.hamr.act.vm.VM_Template

object ScriptTemplate {

  def runCamkesScript(hasVM: B): ST = {
    val camkesDir: String = if(hasVM) { "camkes-arm-vm" } else { "camkes" }
    val camkesGitLoc: String = if(hasVM) { "https://github.com/SEL4PROJ/camkes-arm-vm" } else { "https://docs.sel4.systems/projects/camkes" }

    val CAMKES_DIR: String = "CAMKES_DIR"
    val NON_INTERACTIVE: String = "NON_INTERACTIVE"
    val SIMULATE: String = "SIMULATE"
    val CAMKES_OPTIONS: String = "CAMKES_OPTIONS"

    val buildSim: ST = if(hasVM) {
      st"""../init-build.sh $${${CAMKES_OPTIONS}} \
          |    -DPLATFORM=qemu-arm-virt \
          |    -DARM_HYP=ON \
          |    -DCAMKES_APP=$$HAMR_CAMKES_PROJ
          |
          |ninja"""
    } else {
      st"""../init-build.sh $${${CAMKES_OPTIONS}} -DCAMKES_APP=$$HAMR_CAMKES_PROJ
          |
          |ninja"""
    }
    val simulate: ST = if(hasVM) {
      st"""qemu-system-aarch64 \
          |    -machine virt,virtualization=on,highmem=off,secure=off \
          |    -cpu cortex-a53 \
          |    -nographic \
          |    -m size=1024 \
          |    -kernel images/capdl-loader-image-arm-qemu-arm-virt"""
    } else {
      st"""# ./simulate
          |
          |# console output from simulation disappears when QEMU shuts down when run from
          |# generated ./simulate script. Instead call QEMU directly using the default
          |# values ./simulate would pass
          |
          |qemu-system-x86_64 \
          |    -cpu Nehalem,-vme,+pdpe1gb,-xsave,-xsaveopt,-xsavec,-fsgsbase,-invpcid,enforce \
          |    -nographic \
          |    -serial mon:stdio \
          |    -m size=512M \
          |    -kernel images/kernel-x86_64-pc99 \
          |    -initrd images/capdl-loader-image-x86_64-pc99"""
    }

    val ret: ST = st"""#!/usr/bin/env bash
                      |
                      |set -o errexit -o pipefail -o noclobber -o nounset
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
                      |OPTIONS=c:no:s
                      |LONGOPTS=camkes-dir:,non-interactive,camkes-options:,simulate
                      |
                      |function usage {
                      |  echo ""
                      |  echo "Usage: <option>*"
                      |  echo ""
                      |  echo "Available Options:"
                      |  echo "  -c, --camkes-dir      Location of CAmkES project"
                      |  echo "  -n, --non-interactive Non-interactive mode.  Symlink in apps directory will be replaced"
                      |  echo "                        if present, CAmkES build directory will not be deleted"
                      |  echo "  -o, --camkes-options  CAmkES options (e.g -o \"-DWITH_LOC=ON -DCapDLLoaderMaxObjects=40000\")"
                      |  echo "  -s, --simulate        Simulate via QEMU"
                      |  exit 2
                      |}
                      |
                      |! PARSED=$$(getopt --options=$$OPTIONS --longoptions=$$LONGOPTS --name "$$0" -- "$$@")
                      |if [[ $${PIPESTATUS[0]} -ne 0 ]]; then
                      |    usage
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
                      |    --) shift; break ;;
                      |  esac
                      |done
                      |
                      |# handle non-option arguments
                      |if [[ $$# -ne 0 ]]; then
                      |  echo "$$0: Unexpected non-option arguments"
                      |  usage
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
                      |CAMKES_APPS_DIR=$${${CAMKES_DIR}}/projects/camkes/apps/$$HAMR_CAMKES_PROJ
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
                      |    # if the project contains VMs and USE_PRECONFIGURED_ROOTFS=OFF then
                      |    # deleting the build directory would force the linux image to be
                      |    # re-downloaded, which can take a long time
                      |    echo "Non-interactive mode so not deleting existing $${BUILD_DIR}"
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
                      |if [ "$${${SIMULATE}}" = true ]; then
                      |  ${simulate}
                      |fi
                      |"""
    return ret
  }
}