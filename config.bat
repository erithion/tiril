mkdir build
cd build 

:: Available flags
::  -DTIRIL_BUILDDIR - the flag controls the output folder for the built binaries. Default is 'bin'.
::  -DVIRIL=1 - the flag instructs to include Viril into the build. 
::      To unset the flag simply delete it from the parameters.
::      If unset, all VIRIL_ options will have no effect and Viril will not be built.
::  -DVIRIL_BOOSTROOT - the flag controls the location of Boost library for Viril. 
::  -DVIRIL_WITHVLC=1 - use this flag if you want to copy built Viril into the bin folder along with VLC.
::      To unset the flag simply delete it from the parameters.
::      Useful if you want to debug Viril by running it within VLC at once. Otherwise only the built Viril is copied.
::  -DSIRIL=1 - the flag instructs to include Siril into the build. 
::      To unset the flag simply delete it from the parameters.
::      If unset, all SIRIL_ options will have no effect and Siril will not be built.
::  -DSIRIL_WITHPROFILE=1 - set this flag if you want to build Siril with profiling.
::      To unset the flag simply delete it from the parameters.
::      Useful if you want to profile Haskell part of Tiril.

cmake -DSIRIL=1 -DVIRIL=1 -DVIRIL_WITHVLC=1 -DVIRIL_BOOSTROOT=C:\ms\boost_1_67_0 ..