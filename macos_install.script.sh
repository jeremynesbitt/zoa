#!/bin/sh -eu
#
# Finishes `meson install` on macOS: the install prefix is the Zoa.app
# bundle (meson setup --prefix=/path/to/Zoa.app --bindir=Contents/MacOS).
# Clears extended attributes in the bundle, copies the dylibs it needs into
# Contents/libs and ad-hoc signs it.

PREFIX="${MESON_INSTALL_DESTDIR_PREFIX:-${MESON_INSTALL_PREFIX}}"

# Refuse anything but an app bundle: the steps below rewrite the whole tree
# under the prefix (xattr -cr), which must never be e.g. /opt/homebrew.
case "${MESON_INSTALL_PREFIX}" in
  *.app) ;;
  *)
    echo "macos_install.script.sh: the install prefix is ${MESON_INSTALL_PREFIX}," >&2
    echo "  not a .app bundle.  meson install builds Zoa.app; configure with" >&2
    echo "    meson configure <builddir> --prefix=/path/to/Zoa.app --bindir=Contents/MacOS" >&2
    exit 1
    ;;
esac
if [ ! -x "${PREFIX}/Contents/MacOS/Zoa" ]; then
  echo "macos_install.script.sh: ${PREFIX}/Contents/MacOS/Zoa is missing;" >&2
  echo "  the bindir must be Contents/MacOS (meson configure <builddir> --bindir=Contents/MacOS)" >&2
  exit 1
fi

echo "Finishing ${PREFIX}"
xattr -cr "${PREFIX}"
dylibbundler -od -b -x "${PREFIX}"/Contents/MacOS/Zoa -d "${PREFIX}"/Contents/libs -s /usr/local/gfortran/lib -s /opt/homebrew/lib/gcc/current

codesign --force --deep --preserve-metadata=entitlements,requirements,flags,runtime --entitlements "${MESON_SOURCE_ROOT}"/MacOS/app.entitlements --sign - "${PREFIX}"/Contents/MacOS/Zoa
