#!/usr/bin/env bash

$SIREUM_HOME/bin/sireum tools sergen -l $SIREUM_HOME/license.txt -m json,msgpack ActAst.scala
$SIREUM_HOME/bin/sireum tools trafo -l $SIREUM_HOME/license.txt -m immutable,mutable ActAst.scala