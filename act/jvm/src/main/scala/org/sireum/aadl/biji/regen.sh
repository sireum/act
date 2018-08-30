#!/usr/bin/env bash
SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
SIREUM_HOME=$SCRIPT_HOME/../../../../../../../../../..
cd $SCRIPT_HOME
$SIREUM_HOME/platform/java/bin/java -jar $SIREUM_HOME/bin/sireum.jar tools sergen -l $SIREUM_HOME/license.txt -m json,msgpack BijiAst.scala
$SIREUM_HOME/platform/java/bin/java -jar $SIREUM_HOME/bin/sireum.jar tools transgen -l $SIREUM_HOME/license.txt -m immutable,mutable BijiAST.scala