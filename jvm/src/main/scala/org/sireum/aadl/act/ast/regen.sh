#!/usr/bin/env bash
SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
SIREUM=$SCRIPT_HOME/../../../../../../../../../../bin/sireum

$SIREUM tools sergen -l $SIREUM_HOME/license.txt -m json,msgpack ActAst.scala
$SIREUM tools transgen -l $SIREUM_HOME/license.txt -m immutable,mutable ActAST.scala