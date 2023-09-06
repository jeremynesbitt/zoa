# Zoa Optical Analysis

Zoa is intended to be a feature rich, cross platform optical designa and analysis tool.  It is built on KDP-2 (original code located http://www.ecalculations.com) with the primary changes being to migtrate the UI from using a commercial GUI toolset to gtk4 via the gtk-fortran language binding.  This change makes it possible to build and run the application using open source software, thereby reducing the barrier to develop new features and/or fixing issues.

Zoa was primarily written on MacOS, but the code will also run in Windows but has not been tested thoroughly.

# Using

For Mac and Windows, the best way to use Zoa is to download the installer here.  

For Linux it should be possible to build and run baes on the MacOS build instructions below, but it has not been tested.

# Build

## MacOS

As mentioned above, the best way to get started is to download a release.  But if you want to build it yourself for development purposes, instructions are below:

The following packages are needed to build Zoa:   
* gtk4   
* gtk-fortran (minimum 4.4.1)   
* plplot (with fortran bindings)   

To build the source code, you will need cmake (gtk-fortran and plplot) and meson.  

Instructions using the Homebrew package manager:  
**gt4**   
```brew install gtk4```  
**PlPlot**  
download the latest version here:  https://plplot.sourceforge.net To build, recommend the following from the plplot source directory:   
```mkdir build && cd build```      
```cmake -DBUILD_SHARED_LIBS=ON -DENABLE_DYNDRIVERS=OFF -DUSE_RPATH=OFF -DPLD_xwin=OFF -DPL_USE_PTHREADS_XWIN=OFF -DPLD_xcairo=OFF -DENABLE_cxx=OFF ..```         
```make```        
```sudo make install```       

Notes:  
-a few of these cmake options are only needed if you are going to make an app bundle using the scripts in the repositorty.  If you just want to build the application most of these options are not necessary.      
-While plplot can be installed via homebrew, due to the specific options I want to use for CMAKE I end up
building it when needed.

**gtk-fortran**    
-Download latest release from https://github.com/vmagnin/gtk-fortran To build, recommend the following from the gtk-fortran source directory:   
```mkdir build && cd build```    
```cmake .. -DCMAKE_BUILD_WITH_INSTALL_NAME_DIR=ON -DCMAKE_INSTALL_NAME_DIR="/usr/local/lib"```    
```make```     
```sudo make install```    
Note:  
As with plplot, the options in cmake are to make it easier to make a MacOS installer with the solution I am using.
important note:  you will get a pkg-config error on MacOS when building if you do nothing else. To fix it,
need to change one letter in the file.   
```sudo nano /usr/local/lib/pkgconfig/gtk-4-fortran.pc```    
In the very last line you will see ```-R${libdir}```  Change it to ```-L${libdir}```, save and close.

**Zoa**
Zoa uses meson, and the build instructions are as follows from the source directory:   
```mkdir build && cd build```   
```meson setup --prefix=$PWD/Zoa.app --bindir=Contents/MacOS --buildtype release ..```    
```meson compile```

**Install**   
If you wish to create an app bundle, you will need to have dylibbundler (https://github.com/auriamg/macdylibbundler)   
```brew install dylibbundler```   
```meson install```   

This should create an app bundle in the build directory that should work.  If you want to install it using the existing installer, you would need to download Packages (http://s.sudre.free.fr/Software/Packages/about.html).  Once you have it installed you will find a Package Config File in /MacOS directory in the repository.  Open it, build and run the project and the installer should start.  The installer copies the app bundle to the Applications directory and installs the resources.

Note:  Zoa requires resources in ~/Library/Application Support/Zoa to properly work (eg glass libraries).  The release installer will do this for you, but if you want to do it yourself you can copy everything in the Library folder in the git repository to the folder above.  


## Windows   
As mentioned above, the best way to get started is to download a release.  But if you want to build it yourself for development purposes, instructions are below:   

At present, for Windows the Intel Fortran classic compiler is required due to the issue reported here (https://github.com/vmagnin/gtk-fortran/issues/244); UIs such as the lens editor dialog box will crash the program using gfortran.  This prevents the theorectically easier path of using MSYS2 to get Zoa up and running.

To install on Windows, the following instructions need to be followed:   
Install Visual Studio 2022   
Install Intel oneAPI Fortran Classic Compiler   
**gtk4**   
Follow the instructions here to install gtk4 via gvsbuuild https://github.com/wingtk/gvsbuild . This is a nice piece of work that makes the subsequent steps much easier.  Not only does it install a dependency we need, but it also sets up meson and pkg-config which greatly simplifies the subsequent steps.   
For the remaining command line steps, use the Intel oneAPI command prompt for Intel 64 for Visual Studio 2022 (should be in start menu if everything was installed correctly)   
**PlPlot**   
Download latest plplot build from https://plplot.sourceforge.net   
Place the source somewhere (I chose c:\dev\plplot)   
```mkdir build```    
```cd build```    
```set FC=ifort```    
```cmake .. -G "NMake Makefiles" -DBUILD_TEST=ON -DBUILD_SHARED_LIBS=ON -DPKG_CONFIG_EXECUTABLE=C:\gtk-build\gtk\x64\release\bin\pkg-config.exe -DCMAKE_INSTALL_PREFIX=C:\gtk-build\gtk\x64\release -DENABLE_fortran=ON -DTEST_DYNDRIVERS=ON```    
```nmake```   
```nmake install```    
These commands will point cmake to the pkg-config files set up by gvsbuild for gtk4, and it will also install Plplot in the same location, making it easier for the downstream dependencies to find it.

Note:  the set FC=ifort command is needed because cmake will find the newer Intel ifx compiler first if you don't, and it will cause cmake to fail with the plplot config. 

**gtk-fortran**  
The steps here are very similar to plplot.  Download latest source from https://github.com/vmagnin/gtk-fortran   
Place the source somewhere (I chose c:\dev\gtk-fortran)    
```mkdir build```   
```cd build```   
```set FC=ifort```    (if not set previously) 
```cmake .. -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release -DPKG_CONFIG_EXECUTABLE=C:\gtk-build\gtk\x64\release\bin\pkg-config.exe -DCMAKE_INSTALL_PREFIX=C:\gtk-build\gtk\x64\release```   
```nmake```   
```nmake install```    
This the same trick as plplot
Important note:  there is an issue when building gtk-fortran with the Cmake debug flag with ifort, so recommend building it as release as shown above.

**Zoa**   
Download the latest release of Zoa, and put it somewhere (eg c:\dev\zoa)  
```mkdir build```   
```cd build```   
```meson setup .. --pkg-config-path=C:\gtk-build\gtk\x64\release\lib\pkgconfig```    
```meson compile```   

If everything is done correctly (and my instructions above are accurate) everything should build.

**Install**   
Zoa uses Inno (https://jrsoftware.org/isinfo.php).  If you download this application, you can load the config file in the /Windows directory of the repository, compile and run and it should install it for you.  

Zoa needs resource files in a specific location to run (eg glass libraries).  The installer will do this for you, but if you want to do it yourself you can copy the files to C:\Users\currUser\OneDrive\Documents\Zoa

Note:  The Windows build uses a trick to hide the command prompt window when it is built in release mode only.  If you want to hide the debugging information, add ```--buildtype=release``` to the meson setup command

# Known Issues

In short, there are lots of issues.  If something isn't working, feel free to submit an issue.  A few general notes:   
-The vast majority of the original KDP2 commands should work.  I have not tested all of them, but the source code has been mostly left untouched so it should work as well as it did in the past.  
-Known exceptions are commands that use files.  Many of the file I/O has had to be rewritten to get it to work in both ifort and gfortran, and not everything has been tested.  Also, some of the file I/O (such as exporting plots to images) used WINTERACTER and is not broken with gtk4/plplot.     
-Another annoying issue is that when you run macros, sometimes not all of the commands will execute.  Usually running it again will cause it to complete.  The issue is annoying and eventually there will be a fix. 
