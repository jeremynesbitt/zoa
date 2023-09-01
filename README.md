# Zoa Optical Analysis

Zoa is intended to be a feature rich, cross platform optical designa and analysis tool.  It is built on KDP-2 (original code located http://www.ecalculations.com) with the primary changes being to migtrate the UI from using a commercial GUI toolset to gtk4 via the gtk-fortran language binding.  This change makes it possible to build and run the application using open source software, thereby reducing the barrier to develop new features.

Zoa was primarily written on MacOS, but the code will also run in Windows but has not been tested.

# Using

For Mac and Windows, the best way to use Zoa is to download the installer here.  

For Linux it should be possible to build and run baes on the MacOS build instructions below, but it has not been tested.

# Build
The following packages are needed to build Zoa:
gtk4
gtk-fortran (4.4.1)
plplot

To build the source code, you will need cmake (gtk-fortran and plplot) and meson

Instructions using the Homebrew package manager:
brew install gtk4

For windows:

Install gtk4 using gvsbuild, following the instructions here

gtk-fortran:
Build with the following command in the Intel OneAPI
set FC=ifort
cmake .. -G "NMake Makefiles" -DPKG_CONFIG_EXECUTABLE=C:\gtk-build\gtk\x64\release\bin\pkg-config.exe -DCMAKE_INSTALL_PREFIX=C:\gtk-build\gtk\x64\release
nmake
nmake install

do something similar for PLPLOT
cmake .. -G "NMake Makefiles" -DBUILD_TEST=ON -DBUILD_SHARED_LIBS=ON -DPKG_CONFIG_EXECUTABLE=C:\gtk-build\gtk\x64\release\bin\pkg-config.exe -DCMAKE_INSTALL_PREFIX=C:\gtk-build\gtk\x64\release -DENABLE_fortran=ON -DTEST_DYNDRIVERS=ON
nmake
nmake install

for Zoa
meson setup .. --pkg-config-path=C:\gtk-build\gtk\x64\release\lib\pkgconfig 
