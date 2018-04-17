rem seems 2017 studio doesnt export this var :(
rem call "%VS140COMNTOOLS%VsDevCmd.bat"
call "C:\Program Files\Microsoft Visual Studio\2017\Community\Common7\Tools\VsDevCmd.bat"

rmdir /Q /S bin
rmdir /Q /S build
mkdir build
cd build 

rem cmake ..
rem devenv libviril_plugin.sln /build
rem cmake -G "NMake Makefiles" -D _BOOST_="C:/ms/boost_1_66_0" -D _BUILD_TO_="F:/test" ..
cmake -G "NMake Makefiles" -D _BOOST_="C:/ms/boost_1_66_0" ..
nmake install
rmdir /Q /S build
