#!/bin/sh -eu

echo ${MESON_INSTALL_PREFIX}

mkdir -p "${MESON_INSTALL_PREFIX}"/Contents/libs

dylibbundler -od -b -x "${MESON_INSTALL_PREFIX}"/Contents/MacOS/Zoa -d "${MESON_INSTALL_PREFIX}"/Contents/libs -s /usr/local/lib/ -s /usr/local/gfortran/lib

codesign --force --deep --preserve-metadata=entitlements,requirements,flags,runtime --entitlements "${MESON_SOURCE_ROOT}"/MacOS/zoa.entitlements --sign - "${MESON_INSTALL_PREFIX}"/Contents/MacOS/Zoa


#! dylibbundler -od -b -x Zoa.app/Contents/MacOS/zoa -d Zoa.app/Contents/libs
#! cp -R /Library/Frameworks/SDL2.framework \
#!         ${MESON_INSTALL_PREFIX}/Contents/Frameworks

#! Then it needs to alter the library search path of our executable(s). This tells OSX that the libraries your 
#! app needs are inside your bundle. In the case of SDL2, the invocation goes like this:

#! $ install_name_tool -change @rpath/SDL2.framework/Versions/A/SDL2 \
#!    @executable_path/../FrameWorks/SDL2.framework/Versions/A/SDL2 \
#!    ${MESON_INSTALL_PREFIX}/Contents/MacOS/myapp
