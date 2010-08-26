#!/bin/sh

declare -x PATH_TO_SOURCE=`dirname $0`/..
declare -x PROGRAM_NAME=ensanche-core

declare -x DIRS_TO_CLEAN=". core fields geometry math components"
cd $PATH_TO_SOURCE
if [[ `find $DIRS_TO_CLEAN -maxdepth 1 -name '*.o[0-9]*' 2> /dev/null` ]]; then echo "Object files removed"; find $DIRS_TO_CLEAN -maxdepth 1 -name '*.o[0-9]*' | xargs rm; fi;
if [[ -a $PATH_TO_SOURCE/__scripts/bin/$PROGRAM_NAME ]]; then rm -R $PATH_TO_SOURCE/__scripts/bin/$PROGRAM_NAME; fi

if [[ -d xml-output ]]; then rm -R xml-output; fi
