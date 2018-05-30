cmake_minimum_required( VERSION 3.11.0 )
include(FetchContent)
# Variables the user may re-define from the outside: 
#   SIRIL                   - if defined, turns on Siril build.
#   VIRIL                   - if defined, turns on Viril build.
#   TIRIL_BUILDDIR          - controls the build output folder
#   VIRIL_BOOSTROOT         - the root folder of the Boost library
#   VIRIL_WITHVLC           - if defined, Viril will be built into VLC and installed into the output folder along with it
#   SIRIL_WITHPROFILE       - if defined, Siril is built with profiling information within

if( NOT DEFINED TIRIL_BUILDDIR )
    set                         ( TIRIL_BUILDDIR    ${CMAKE_CURRENT_SOURCE_DIR}/bin )
    message                     ( STATUS            "TIRIL_BUILDDIR is undefined. Setting it to ${TIRIL_BUILDDIR}. Use -DTIRIL_BUILDDIR=[path] to redefine it" )
endif()

if( DEFINED SIRIL )
    message                     ( STATUS            "SIRIL is set to ${SIRIL}. Starting Siril build..." )
    add_subdirectory            ( siril )
else()
    message                     ( STATUS            "SIRIL is undefined. All other options starting with SIRIL_ will be ignored" )
endif()

if( DEFINED SIRIL )
    message                     ( STATUS            "VIRIL is set to ${VIRIL}. Starting Viril build..." )
    add_subdirectory            ( viril )
else()
    message                     ( STATUS            "VIRIL is undefined. All other options starting with VIRIL_ will be ignored" )
endif()

