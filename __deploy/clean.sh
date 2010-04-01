#!/bin/sh

declare -x PATH_TO_SOURCE=`dirname $0`/..
declare -x PROGRAM_NAME=ensanche-core

cd $PATH_TO_SOURCE
if [[ `find . -name '*.o[0-9]*' 2> /dev/null` ]]; then echo "Object files removed"; find . -name '*.o[0-9]*' | xargs rm; fi;
if [[ -a $PATH_TO_SOURCE/__deploy/bin/$PROGRAM_NAME ]]; then rm -R $PATH_TO_SOURCE/__deploy/bin/$PROGRAM_NAME; fi
