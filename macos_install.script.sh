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
# Zoa and (when built, with libzmq) the Jupyter server zoa_server share
# Contents/libs: one dylibbundler run fixes both.
BINS="${PREFIX}/Contents/MacOS/Zoa"
SERVER="${PREFIX}/Contents/MacOS/zoa_server"
[ -x "$SERVER" ] && BINS="$BINS $SERVER"
set --
for b in $BINS; do set -- "$@" -x "$b"; done
dylibbundler -od -b "$@" -d "${PREFIX}"/Contents/libs -s /usr/local/gfortran/lib -s /opt/homebrew/lib/gcc/current

# dylibbundler rewrites every rpath it replaces to @executable_path/../libs/,
# so a binary linked with several (gfortran links three gcc library
# folders) ends up with the same LC_RPATH more than once -- which dyld
# refuses at launch ("duplicate LC_RPATH").  Keep one of each.
dedupe_rpaths() {
  otool -l "$1" | awk '/cmd LC_RPATH/{getline; getline; print $2}' | sort | uniq -c |
  while read -r count rpath; do
    while [ "$count" -gt 1 ]; do
      install_name_tool -delete_rpath "$rpath" "$1"
      count=$((count - 1))
    done
  done
}
for b in $BINS; do dedupe_rpaths "$b"; done
for lib in "${PREFIX}"/Contents/libs/*.dylib; do
  dedupe_rpaths "$lib"
  codesign --force --sign - "$lib"
done

codesign --force --deep --preserve-metadata=entitlements,requirements,flags,runtime --entitlements "${MESON_SOURCE_ROOT}"/MacOS/app.entitlements --sign - "${PREFIX}"/Contents/MacOS/Zoa
if [ -x "$SERVER" ]; then
  codesign --force --preserve-metadata=entitlements,requirements,flags,runtime --entitlements "${MESON_SOURCE_ROOT}"/MacOS/app.entitlements --sign - "$SERVER"
fi
