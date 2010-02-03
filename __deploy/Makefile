## Default options
UI=sdl
OS=linux
VERSION=1.0

## Files
GL_FILES = opengl.scm glu.scm
SDL_FILES = sdl.scm
DEVEL_FILES = $(GL_FILES) $(SDL_FILES)
PROJECT_FILES = $(DEVEL_FILES) #new-engine.scm

## Compilers
GSC=$(PATH_TO_GAMBIT)/bin/gsc
CC=gcc

## Gambit-c
PATH_TO_GAMBIT=/usr
GAMBIT_LIB=$(PATH_TO_GAMBIT)/lib
GAMBIT_INCLUDE=$(PATH_TO_GAMBIT)/include
GSC_OPTIONS= $(GSC_EXT_OPTIONS) -debug

## Bundled files
PLAN_FILES = $(wildcard plans/*.ppm) $(wildcard plans/*.bmp)

#LD_OPTIONS_LIN = -lutil -lSDL -lSDL_mixer -lglut
#LD_OPTIONS_COMMON =-L$(GAMBIT_LIB) -L$(GL_LIB) -L$(SDL_LIB) -L$(SDL_mixer_LIB) -lgambc 

## C library paths
PATH_TO_GL=/usr
GL_INCLUDE=$(PATH_TO_GL)/include/GL
GL_LIB=$(PATH_TO_GL)/lib

GLUT_INCLUDE=/usr/include/GL

PATH_TO_SDL=/usr
SDL_INCLUDE=$(PATH_TO_SDL)/include/SDL
SDL_LIB=$(PATH_TO_SDL)/lib
PATH_TO_SDL_mixer=/usr
SDL_mixer_INCLUDE=$(PATH_TO_SDL_mixer)/include/SDL
SDL_mixer_LIB=$(PATH_TO_SDL_mixer)/lib

CAIRO_INCLUDE=`pkg-config --cflags cairo`
CAIRO_LIBS=`pkg-config --libs cairo`

LD_OPTIONS="$(CAIRO_LIBS)"
ALL_SDL_INCLUDE=-I$(SDL_INCLUDE) -I$(SDL_mixer_INCLUDE)
LD_OPTIONS = $(LD_OPTIONS_COMMON) $(LD_OPTIONS_LIN)

INCLUDE_OPTIONS=-I$(GAMBIT_INCLUDE) -I$(GL_INCLUDE) $(ALL_SDL_INCLUDE)

## Scheme sources
OOSYSTEM_PATH=..
SCMLIB_PATH=..
THRDSIM_PATH=..

PATH_TO_SRCS=..


.SUFFIXES:
.SUFFIXES: .c .scm .o .o1 .m
.PHONY: all clean shared-objects tarball welcome

all: gl sdl cairo ssax

## Welcome banner
 welcome:
	@echo "*** Currently using following paths ***"
	@echo
	@echo PATH_TO_GAMBIT=$(PATH_TO_GAMBIT)
	@echo PATH_TO_GL=$(PATH_TO_GL)
ifeq ($(UI), sdl)
	@echo PATH_TO_SDL=$(PATH_TO_SDL)
	@echo PATH_TO_SDL_mixer=$(PATH_TO_SDL_mixer)
endif
	@echo
	@echo "*** Beginning Compilation ***"

# rule used for 'make devel'
#new-engine.scm: thread-simulation-macro.scm class.scm thread-simulation.scm

#build: $(addprefix $(PATH_TO_SRCS)/, $(PROJECT_FILES:.scm=.o1))
sdl: gl
	@echo
	@echo "*** Compiling SDL loadable library ***"
	@echo
	$(GSC) $(GSC_OPTIONS) -link -flat -cc-options "$(INCLUDE_OPTIONS)" -o sdl.o1.c $(addprefix $(PATH_TO_SRCS)/, $(SDL_FILES)) $(PIPE_OUTPUT)
	$(CC) -w -shared -fPIC -D___DYNAMIC $(INCLUDE_OPTIONS) $(addprefix $(PATH_TO_SRCS)/, $(SDL_FILES:.scm=.c)) sdl.o1.c -o $(PATH_TO_SRCS)/sdl.o1 -lSDL
	rm $(addprefix $(PATH_TO_SRCS)/, $(SDL_FILES:.scm=.c)) sdl.o1.c

gl:
	@echo
	@echo "*** Compiling GL loadable library ***"
	@echo
	$(GSC) $(GSC_OPTIONS) -link -flat -cc-options "$(INCLUDE_OPTIONS)" -o gl.o1.c $(addprefix $(PATH_TO_SRCS)/, $(GL_FILES)) $(PIPE_OUTPUT) 
	$(CC) -w -shared -fPIC -D___DYNAMIC $(INCLUDE_OPTIONS) $(addprefix $(PATH_TO_SRCS)/, $(GL_FILES:.scm=.c)) gl.o1.c -o $(PATH_TO_SRCS)/gl.o1 -lGL -lGLU
	rm $(addprefix $(PATH_TO_SRCS)/, $(GL_FILES:.scm=.c)) gl.o1.c

cairo:
	@echo
	@echo "*** Compiling Cairo loadable library ***"
	@echo
	$(GSC) $(GSC_OPTIONS) \
		-cc-options "-w $(CAIRO_INCLUDE)" \
		-ld-options "$(CAIRO_LIBS)" -f $(PATH_TO_SRCS)/cairo/cairo.scm $(PIPE_OUTPUT)
	mv $(PATH_TO_SRCS)/cairo/cairo.o1 $(PATH_TO_SRCS)

ssax:
	@echo
	@echo "*** Compiling SSAX-SXML loadable library ***"
	@echo
	cd $(PATH_TO_SRCS)/ssax-sxml; make; mv ssax-sxml.o1 ..

## "included" macro dependant scheme source files
user-interface-images.c: user-interface-images.scm texture-macro.scm font-macro.scm scm-lib-macro.scm
	$(GSC) -c user-interface-images.scm 

user-interface.c: user-interface.scm scm-lib-macro.scm opengl-header.scm
	$(GSC) -c user-interface.scm 

sdl-user-interface.c: sdl-user-interface.scm scm-lib-macro.scm opengl-header.scm
	$(GSC) -c sdl-user-interface.scm 

new-engine.c: new-engine.scm thread-simulation-macro.scm class.scm thread-simulation.scm
	$(GSC) -c new-engine.scm

# Opengl interface interdependance
opengl.c: opengl.scm opengl-header.scm
	$(GSC) -c opengl.scm

glu.c: glu.scm glu-header.scm
	$(GSC) -c glu.scm

glut.c: glut.scm glut-header.scm
	$(GSC) -c glut.scm


# External Scheme library dependencies
#  class.scm: $(OOSYSTEM_PATH)/class.scm
	#  cp $(OOSYSTEM_PATH)/class.scm .

#  scm-lib.scm: $(SCMLIB_PATH)/scm-lib.scm
	#  cp $(SCMLIB_PATH)/scm-lib.scm .

#  scm-lib-macro.scm: $(SCMLIB_PATH)/scm-lib-macro.scm
	#  cp $(SCMLIB_PATH)/scm-lib-macro.scm .

#  thread-simulation.scm: $(THRDSIM_PATH)/thread-simulation.scm
	#  cp $(THRDSIM_PATH)/thread-simulation.scm .

#  thread-simulation-macro.scm: $(THRDSIM_PATH)/thread-simulation-macro.scm
	#  cp $(THRDSIM_PATH)/thread-simulation-macro.scm .

#.scm.o1
#$(GSC) -link -flat -cc-options  "$(INCLUDE_OPTIONS)" -ld-options "$(LD_OPTIONS)" -debug-source -o $*.o1 $*.scm
#$(GSC) -cc-options  "$(INCLUDE_OPTIONS)" -ld-options "$(LD_OPTIONS)" -debug-source -o bin/$(notdir $*).o1 $*.scm


ALL_SCM = $(wildcard *.scm)
clean:
	rm -f $(addprefix $(PATH_TO_SRCS)/, $(PROJECT_FILES:.scm=.c)) $(PATH_TO_SRCS)/*.o* *.o *.o1 *.c *.tar.gz *.tgz *.~*~ *.zip
#rm -f  *_.c *.o* space-invaders.exe *.tar.gz *.tgz *.~*~ *.zip
  # external libs
#rm -f class.scm scm-lib.scm scm-lib-macro.scm
#$(MAKE) clean -C doc

tarball: makefile README $(wildcard *.scm) $(PLAN_FILES) $(FONT_FILES) $(DOC_FILES) $(SOUND_FILES)
	tar cvzf space-invaders-src-v$(VERSION).tgz $(foreach file, $^, ../space-invaders/$(file))

release: space-invaders.exe $(PLAN_FILES) $(FONT_FILES) $(SOUND_FILES)
ifeq ($(OS), win)
	zip -r space-invaders-$(OS).zip $(foreach file, $^ SDL.dll SDL_mixer.dll, ../space-invaders/$(file))
else
	tar cvzf space-invaders-$(OS).tar.gz $(foreach file, $^, ../space-invaders/$(file))
endif
