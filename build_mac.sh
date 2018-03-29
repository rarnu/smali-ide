#!/bin/sh

TYPHON_ROOT=/usr/local/codetyphon
FPC=${TYPHON_ROOT}/fpc/fpc32/bin/i386-darwin/fpc
TYPHON=${TYPHON_ROOT}/typhon
FW=/System/Library/Frameworks
ARCH=i386-darwin

# clean
rm -fr smaliide.app

mkdir -p build/${ARCH}
# compile
${FPC} -MObjFPC -Scghi -O1 -gw -gl -l -vewnhibq  \
	-k-framework -kCarbon -k-framework -kOpenGL \
	-k-dylib_file -k${FW}/OpenGL.framework/Versions/A/Libraries/libGL.dylib:${FW}/OpenGL.framework/Versions/A/Libraries/libGL.dylib \
	-Fibuild/${ARCH} \
	-FUbuild/${ARCH} \
	-Fu . -Fuform -Fubase -Fuutil -Fuview -Fuview/synedit -Fuapi \
	-Fu${TYPHON}/lcl/units/${ARCH}/carbon \
	-Fu${TYPHON}/lcl/units/${ARCH} \
	-Fu${TYPHON}/components/BaseUtils/lib/${ARCH} \
	-Fu${TYPHON}/packager/units/${ARCH} \
	-dLCL -dLCLcarbon \
	smaliide.ppr

# bundle
mkdir smaliide.app
mkdir smaliide.app/Contents
mkdir smaliide.app/Contents/MacOS
mkdir smaliide.app/Contents/Resources
cp PkgInfo smaliide.app/Contents/PkgInfo
cp Info.plist smaliide.app/Contents/Info.plist
mv smaliide smaliide.app/Contents/MacOS/
cp -r bin smaliide.app/Contents/MacOS/
cp -r lib smaliide.app/Contents/MacOS/
cp -r style  smaliide.app/Contents/MacOS/
cp -r template smaliide.app/Contents/MacOS/
cp -r image smaliide.app/Contents/MacOS/
cp smaliide.icns smaliide.app/Contents/Resources/

