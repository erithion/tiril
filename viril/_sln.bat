rem seems 2017 studio doesnt export this var :(
rem call "%VS140COMNTOOLS%VsDevCmd.bat"
call "C:\Program Files\Microsoft Visual Studio\2017\Community\Common7\Tools\VsDevCmd.bat"

rmdir /Q /S bin
rmdir /Q /S build
mkdir build
cd build 

cmake -D _BOOST_="C:/ms/boost_1_66_0" ..
