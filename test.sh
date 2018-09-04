#!/bin/bash -e
export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
rm -fR mill-standalone versions.properties
curl -Lo mill-standalone http://files.sireum.org/mill-standalone
chmod +x mill-standalone
curl -Lo versions.properties https://raw.githubusercontent.com/sireum/kekinian/master/versions.properties
git submodule update --init --recursive --remote
$SCRIPT_HOME/mill-standalone -i all \
  cli.assembly \
  act.jvm.tests \
  cli.tests
