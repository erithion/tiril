cmake_minimum_required(VERSION 3.11.0)
include(FetchContent)
include(FindBoost)

set    ( PROJECT_NAME                libviril_plugin )
set    ( VIRIL_BUILD_DIR             ${TIRIL_BUILD_DIR}/vlc )
set    ( EXTERNAL_LIBS_DIR           ${CMAKE_CURRENT_SOURCE_DIR}/depends/ )
set    ( CMAKE_CXX_FLAGS             "${CMAKE_CXX_FLAGS} /EHc /std:c++latest" )
set    ( CMAKE_SHARED_LINKER_FLAGS   "${CMAKE_SHARED_LINKER_FLAGS} /SAFESEH:NO" )

file   ( GLOB 
         SRC_FILES                   "*.cpp" )
file   ( GLOB 
         INC_FILES                   "*.h" )
file   ( GLOB
         LUA_FILES                   "*.lua" )
         
project( ${PROJECT_NAME} )
message( STATUS "CXX flags ${CMAKE_CXX_FLAGS}" )
message( STATUS "Linker flags ${CMAKE_SHARED_LINKER_FLAGS}" )

# External projects Viril depends on
set                         ( Boost_USE_STATIC_LIBS       ON ) # only find static libs
set                         ( Boost_USE_MULTITHREADED     ON )
set                         ( Boost_USE_STATIC_RUNTIME    OFF )
find_package                ( Boost 1.67.0 REQUIRED COMPONENTS filesystem )

message( STATUS "Fetching libass ..." )
FetchContent_Declare        ( libass
                              GIT_REPOSITORY https://github.com/libass/libass
                              GIT_TAG 0.14.0 )
FetchContent_GetProperties  ( libass )
if( NOT libass_POPULATED )
    FetchContent_Populate   ( libass )
endif()

message( STATUS "Fetching UTF8-CPP ..." )
FetchContent_Declare        ( utf8_cpp
                              GIT_REPOSITORY https://github.com/nemtrif/utfcpp
                              GIT_TAG v2.3.5 )
FetchContent_GetProperties  ( utf8_cpp )
if( NOT utf8_cpp_POPULATED )
    FetchContent_Populate   ( utf8_cpp )
endif()

if( "${CMAKE_SIZEOF_VOID_P}" STREQUAL "4" )
    message                     ( STATUS "Fetching VLC 3.0 32bit SDK ..." )
    FetchContent_Declare        ( vlc30
                                  URL http://download.videolan.org/pub/videolan/vlc/3.0.0/win32/vlc-3.0.0-win32.7z
                                  URL_HASH SHA256=ba1bceff15adc5fc3a600b5a793f54c1d72bfc832422de77223b19f97351515d )
else()
    message                     ( STATUS "Fetching VLC 3.0 64bit SDK ..." )
    FetchContent_Declare        ( vlc30
                                  URL http://download.videolan.org/pub/videolan/vlc/3.0.0/win64/vlc-3.0.0-win64.7z
                                  URL_HASH SHA256=3eb580a7aa466acf0d15f70f863baf84883e2908a0b8f8a4bce94652faaf536a )
endif()
FetchContent_GetProperties  ( vlc30 )
if( NOT vlc30_POPULATED )
    FetchContent_Populate   ( vlc30 )
endif()

add_definitions      ( -D_CRT_NONSTDC_NO_DEPRECATE ) # warning C4996: 'strdup': The POSIX name for this item is deprecated. 
add_definitions      ( -DBOOST_CONFIG_SUPPRESS_OUTDATED_MESSAGE )
add_definitions      ( -D_SILENCE_CXX17_ITERATOR_BASE_CLASS_DEPRECATION_WARNING )

include_directories  ( ${CMAKE_CURRENT_SOURCE_DIR}/
                       ${Boost_INCLUDE_DIRS}
                       ${EXTERNAL_LIBS_DIR}
                       ${libass_SOURCE_DIR}
                       ${vlc30_SOURCE_DIR}/sdk/include/vlc 
                       ${vlc30_SOURCE_DIR}/sdk/include/vlc/plugins
                       ${vlc30_SOURCE_DIR}/sdk/include 
                       ${utf8_cpp_SOURCE_DIR}/source )
                       
link_directories     ( ${vlc30_SOURCE_DIR}/sdk/lib 
                       ${Boost_LIBRARY_DIR} )

add_library          ( ${PROJECT_NAME} SHARED 
                       ${SRC_FILES} 
                       ${INC_FILES} 
                       ${LUA_FILES} 
                       ${EXTERNAL_LIBS_DIR}/poll.cpp )

target_link_libraries( ${PROJECT_NAME} libvlc.lib )
target_link_libraries( ${PROJECT_NAME} libvlccore.lib )
target_link_libraries( ${PROJECT_NAME} Ws2_32.lib )
target_link_libraries( ${PROJECT_NAME} ${Boost_LIBRARIES} )

if( DEFINED INSTALL_VLC )
    message ( STATUS              "INSTALL_VLC is ${INSTALL_VLC}. Viril will be installed along with VLC" )
    set     ( VIRIL_BUILD_DIR     ${TIRIL_BUILD_DIR}/vlc30 )
    
    install ( DIRECTORY           ${vlc30_SOURCE_DIR}/  
              DESTINATION         ${VIRIL_BUILD_DIR} )
endif()

install( TARGETS        ${PROJECT_NAME}                         
         DESTINATION    ${VIRIL_BUILD_DIR}/plugins/viril )
         
install( FILES          ${CMAKE_CURRENT_SOURCE_DIR}/viril.lua
         DESTINATION    ${VIRIL_BUILD_DIR}/lua/extensions )