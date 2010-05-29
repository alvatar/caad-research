#!/bin/sh

PATH_TO_SCRIPT=`dirname $0`

cd $PATH_TO_SCRIPT
cloc ../*.scm ../dev/*.scm ../components/*.scm ../fields/*.scm ../core/*.scm ../geometry/*.scm ../math/*.scm ../ds/*.scm
