.PHONY: all build run clean

SRCDIR=src
OUTDIR=out

MAIN_MODULE=Main

GHC_OPTIONS = -threaded -rtsopts -with-rtsopts=-N -odir ${OUTDIR} -hidir ${OUTDIR} -o ${OUTDIR}/main -i${SRCDIR}
GHC_OPTIONS+= -Wall -Wno-name-shadowing -Wno-unused-do-bind
GHC_OPTIONS+= -Werror=incomplete-patterns

all: clean build

build:
	mkdir -p ${OUTDIR}
	ghc -O2 ${GHC_OPTIONS} ${MAIN_MODULE} -main-is ${MAIN_MODULE}.main

run:
	${OUTDIR}/main

clean:
	rm -rf ${OUTDIR}
