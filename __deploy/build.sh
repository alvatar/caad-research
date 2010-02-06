#!/bin/sh

declare -x PROJECT_DIR="/data/projects/ensanche-core"
declare -x PROGRAM="bsc -:d- -exe -o $PROJECT_DIR/__deploy/bin/ensanche-core main"

cd $PROJECT_DIR/__deploy
if [[ ! -d bin ]]; then mkdir bin; fi;
cd $PROJECT_DIR
$PROGRAM
