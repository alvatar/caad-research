#!/bin/sh

declare -x PATH_TO_SOURCE=`dirname $0`/..

cd $PATH_TO_SOURCE
bsc -e "(module-compile-to-standalone \"ensanche-core\" 'main)"
