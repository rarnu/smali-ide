#!/bin/sh

TYPHON_ROOT=/usr/local/codetyphon
TYPHON=${TYPHON_ROOT}/typhon
ARCH=x86_64-linux
FPC=${TYPHON_ROOT}/fpc/fpc64/bin/${ARCH}/fpc

${FPC} -MObjFPC -Scghi -Cg -O1 -g -gl -l -vewnhibq \
	-Fibuild/${ARCH} \
	-FUbuild/${ARCH} \
	-Fu. -Fuform -Fubase -Fuutil -Fuview -Fuview/synedit -Fuapi \
	-Fu${TYPHON}/lcl/units/${ARCH}/gtk2 \
	-Fu${TYPHON}/lcl/units/${ARCH} \
	-Fu${TYPHON}/components/BaseUtils/lib/${ARCH} \
	-Fu${TYPHON}/packager/units/${ARCH} \
	-dLCL -dLCLgtk2 \
	smaliide.ppr


