rmdir /Q /S bin
rmdir /Q /S build
mkdir build
cd build 

cmake -DINSTALL_VLC=1 -DBOOST_ROOT=C:\ms\boost_1_67_0 ..