#!/bin/sh

declare -x PATH_TO_SOURCE=`dirname $0`/..
declare -x DIRS_ARGS=". components core ds ffi fields geometry math"

cd $PATH_TO_SOURCE
if [[ -e TAGS ]]; then rm -R TAGS; fi
etags `(find $DIRS_ARGS -maxdepth 1 -name '*.scm')`
