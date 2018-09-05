#!/usr/bin/env bash
set -e
export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
${SCRIPT_HOME}/bin/prelude.sh
git submodule update --init --recursive --remote
$SCRIPT_HOME/bin/mill all \
  cli.assembly \
  act.jvm.tests \
  cli.tests
