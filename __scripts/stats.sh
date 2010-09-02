#!/bin/sh

declare -rx PATH_TO_SCRIPT=`dirname $0`

cd $PATH_TO_SCRIPT/..
cloc *.scm components/*.scm core/*.scm core/*/*.scm geometry/*.scm math/*.scm
