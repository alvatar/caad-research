#!/bin/sh

declare -x PATH_TO_SOURCE=`dirname $0`/..
declare -x PROGRAM_NAME=ensanche-core
declare -x COMPILER="bsc -:d- -exe -o $PATH_TO_SOURCE/__deploy/bin/$PROGRAM_NAME main"

cd $PATH_TO_SOURCE/__deploy
if [[ ! -d bin ]]; then mkdir bin; fi;
cd $PATH_TO_SOURCE
$COMPILER
