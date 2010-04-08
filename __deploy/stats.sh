#!/bin/sh

PATH_TO_SCRIPT=`dirname $0`

cd $PATH_TO_SCRIPT
cloc ../*.scm ../strategies/*.scm ../fields/*.scm
