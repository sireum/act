#!/bin/bash -e
export ACT_HOME=$( cd "$( dirname "$0" )"/.. &> /dev/null && pwd )
cd ${ACT_HOME}
git submodule update --init --recursive --remote
bin/prelude.sh
export JAVA_HOME=${ACT_HOME}/sireum/bin/java
export PATH=${JAVA_HOME}/bin:$PATH
${ACT_HOME}/sireum/bin/mill/mill-standalone version
${ACT_HOME}/sireum/bin/mill/mill-standalone all \
  cli.assembly \
  act.jvm.tests \
  cli.tests
if [ -n "$COMSPEC" -a -x "$COMSPEC" ]; then
  cp out/cli/assembly/dest/out.jar bin/act.bat
else
  cp out/cli/assembly/dest/out.jar bin/act
  chmod +x bin/act
fi
