#/bin/sh


declare -x PATH_TO_SOURCE=`dirname $0`/..
declare -x PROGRAM="bsc -:d- -i main"

cd $PATH_TO_SOURCE
$PROGRAM
