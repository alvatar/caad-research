#!/bin/sh

declare -x PROJECT_DIR="/data/projects/ensanche-core"

cd $PROJECT_DIR
if [[ `find . -name '*.o[0-9]' 2> /dev/null` ]]; then echo "Object files removed"; find . -name '*.o[0-9]' | xargs rm; fi;
if [[ -a $PROJECT_DIR/__deploy/bin/ensanche-core ]]; then rm -R $PROJECT_DIR/__deploy/bin/ensanche-core; fi
