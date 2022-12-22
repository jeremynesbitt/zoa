cp ./bin/ZOA ./build/Zoa.app/Contents/MacOS/Zoa         
install_name_tool -change "@rpath/libgtk-4-fortran.4.3.0.dylib" /usr/local/lib/libgtk-4-fortran.4.3.0.dylib ./build/Zoa.app/Contents/MacOS/Zoa
install_name_tool -change "@rpath/libplplot.17.dylib" /usr/local/lib/libplplot.17.dylib ./build/Zoa.app/Contents/MacOS/Zoa
install_name_tool -change "@rpath/libplplotfortran.0.dylib" /usr/local/lib/libplplotfortran.0.dylib ./build/Zoa.app/Contents/MacOS/Zoa
dylibbundler -od -b -x ./build/Zoa.app/Contents/MacOS/Zoa -d ./build/Zoa.app/Contents/libs
xattr -cr build/Zoa.app    
codesign --force --deep --preserve-metadata=entitlements,requirements,flags,runtime --entitlements ./build/app.entitlements --sign - "./build/Zoa.app/Contents/MacOS/Zoa"
cd build
open -a Zoa.app 
