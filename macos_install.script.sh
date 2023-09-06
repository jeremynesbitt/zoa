#!/bin/sh -eu

echo ${MESON_INSTALL_PREFIX}

#mkdir -p "${MESON_INSTALL_PREFIX}"/Contents/libs
xattr -cr "${MESON_INSTALL_PREFIX}"
echo "${MESON_SOURCE_ROOT}/bundle-dylibs.sh" "${MESON_INSTALL_PREFIX}"
dylibbundler -od -b -x "${MESON_INSTALL_PREFIX}"/Contents/MacOS/Zoa -d "${MESON_INSTALL_PREFIX}"/Contents/libs -s /usr/local/gfortran/lib

#dylibbundler -od -b -x "${MESON_INSTALL_PREFIX}"/Contents/MacOS/Zoa -d "${MESON_INSTALL_PREFIX}"/Contents/Frameworks/ -p @executable_path/../Frameworks/ -s /usr/local/gfortran/lib


codesign --force --deep --preserve-metadata=entitlements,requirements,flags,runtime --entitlements "${MESON_SOURCE_ROOT}"/MacOS/app.entitlements --sign - "${MESON_INSTALL_PREFIX}"/Contents/MacOS/Zoa


