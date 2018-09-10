#!/usr/bin/env bash
set -e
export ACT_HOME=$( cd "$( dirname "$0" )"/.. &> /dev/null && pwd )
. ${ACT_HOME}/bin/prelude.sh
cd ${ACT_HOME}
git submodule update --init --recursive --remote
export JAVA_HOME=${ACT_HOME}/bin/${PLATFORM}/jdk
export PATH=${JAVA_HOME}/bin:$PATH
${ACT_HOME}/bin/mill all \
  cli.assembly \
  act.jvm.tests \
  cli.tests
cp out/cli/assembly/dest/out.jar bin/act
