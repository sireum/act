#!/usr/bin/env bash
SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
#SIREUM_HOME=$SCRIPT_HOME/../../../../../../../../../..
cd $SCRIPT_HOME
java -jar $SIREUM_HOME/bin/sireum.jar tools sergen -l $SIREUM_HOME/license.txt -m json,msgpack ActAst.scala
java -jar $SIREUM_HOME/bin/sireum.jar tools transgen -l $SIREUM_HOME/license.txt -m immutable,mutable ActAST.scala