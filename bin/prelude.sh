#!/usr/bin/env bash
set -e
export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
MILL_URL=http://files.sireum.org/mill-standalone # see https://github.com/sireum/mill-build
MILL_SHA3=46c97fe338e08e0dbf42ed72f0bbb5e803bce1391b4cc0e84ea265cd0311cf70
MILL=${SCRIPT_HOME}/mill
LIB_URL=https://raw.githubusercontent.com/sireum/kekinian/1a5428ef964b5d8a631ce874cdf7e662e54995da/versions.properties
LIB_SHA3=2df75aa996a9fba5b40429402961018645de5a0c4689b68a876d4e3dfcb443d3
LIB=${SCRIPT_HOME}/../versions.properties
if [ -z "${PLATFORM}" ]; then
  if [ -n "$COMSPEC" -a -x "$COMSPEC" ]; then
    PLATFORM=win
    SHA3=${SCRIPT_HOME}/win/sha3.exe
    SHA3_URL=http://files.sireum.org/win/sha3.exe
  elif [ "$(uname)" == "Darwin" ]; then
    PLATFORM=mac
    SHA3=${SCRIPT_HOME}/mac/sha3
    SHA3_URL=http://files.sireum.org/mac/sha3
  elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    PLATFORM=linux
    SHA3=${SCRIPT_HOME}/linux/sha3
    SHA3_URL=http://files.sireum.org/linux/sha3
  fi
fi
if [ ! -f ${SHA3} ]; then
  echo "Downloading sha3 ..."
  mkdir -p "$( dirname "${SHA3}" )"
  curl -Lo ${SHA3} ${SHA3_URL}
  chmod +x ${SHA3}
fi
if [ ! -f ${MILL} ] || [ "$(${SHA3} 256 < ${MILL})" != ${MILL_SHA3} ]; then
  echo "Downloading mill ..."
  curl -Lo ${MILL} ${MILL_URL}
  chmod +x ${MILL}
  if [ "$(${SHA3} 256 < ${MILL})" != ${MILL_SHA3} ]; then
    echo "Mill version mismatch; please notify ACT maintainers."
    exit -1
  fi
fi
if [ ! -f ${LIB} ] || [ "$(${SHA3} 256 < ${LIB})" != ${LIB_SHA3} ]; then
  echo "Downloading versions.properties ..."
  curl -Lo ${LIB} ${LIB_URL}
  if [ "$(${SHA3} 256 < ${LIB})" != ${LIB_SHA3} ]; then
    echo "Library dependency versions mismatch; please notify ACT maintainers."
    exit -1
  fi
fi
