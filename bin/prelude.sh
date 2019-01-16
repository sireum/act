#!/bin/bash -e
: ${ACT_HOME:=$( cd "$( dirname "$0" )"/.. &> /dev/null && pwd )}
export ZULU_VERSION=10.3+5-jdk10.0.2
cd ${ACT_HOME}
SIREUM_SOURCE_BUILD=true sireum/bin/build.sh
if [[ ! -e versions.properties ]]; then
  ln -s sireum/versions.properties .
fi
