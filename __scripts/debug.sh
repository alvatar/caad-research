#/bin/sh


declare -x PROJECT_DIR="/data/projects/ensanche-core"
declare -x PROGRAM="bsc -:-1,f1,t1,dar,~~DIR=/usr/ -i main @System: "`uname -s`

cd $PROJECT_DIR
$PROGRAM
