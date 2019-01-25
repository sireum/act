#!/bin/bash -e
export ACT_HOME=$( cd "$( dirname "$0" )"/.. &> /dev/null && pwd )
export SIREUM_HOME=${ACT_HOME}/sireum
cd ${ACT_HOME}
git submodule update --init --recursive --remote
bin/prelude.sh
source sireum/bin/platform.sh
${ACT_HOME}/sireum/bin/mill all \
  tipe \
  cli.assembly \
  act.jvm.tests \
  cli.tests
if [[ ${PLATFORM} = "win" ]]; then
  cp out/cli/assembly/dest/out.jar bin/act.bat
  chmod +x bin/act.bat
else
  cp out/cli/assembly/dest/out.jar bin/act
  chmod +x bin/act
fi
