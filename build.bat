rmdir /Q /S bin
rmdir /Q /S build
mkdir build
cd build 

cmake -DCMAKE_INSTALL_PREFIX=../bin ..