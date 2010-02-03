#!/bin/sh

if [[ ! -d bin ]]; then mkdir bin; fi;
#csc -s -I/usr/include/SDL/ -lSDL sdl-cairo.scm
GSC_EXT_OPTIONS=" -:d-" PIPE_OUTPUT=" > /dev/null" make 
if [[ `ls *di 2> /dev/null` ]]; then rm -R *di; fi;
