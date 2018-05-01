rmdir /Q /S bin
rmdir /Q /S build
mkdir build
cd build 

cmake -DINSTALL_VLC=1 -DPROFILE=1 ..