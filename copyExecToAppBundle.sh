cp ./bin/ZOA ./build/Zoa.app/Contents/MacOS/Zoa         
dylibbundler -od -b -x ./build/Zoa.app/Contents/MacOS/Zoa -d ./build/Zoa.app/Contents/libs
xattr -cr build/Zoa.app    
codesign --force --deep --preserve-metadata=entitlements,requirements,flags,runtime --entitlements ./build/app.entitlements --sign - "./build/Zoa.app/Contents/MacOS/Zoa"
cd build
open -a Zoa.app 