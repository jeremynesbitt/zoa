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
