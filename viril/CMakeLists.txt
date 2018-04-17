cmake_minimum_required(VERSION 2.8.9)
set(            _NAME_          libviril_plugin)
if(NOT DEFINED  _BUILD_TO_)
set(            _BUILD_TO_           ${CMAKE_SOURCE_DIR}/bin/copy_to_vlc)
endif(NOT DEFINED _BUILD_TO_)
#set(            _BOOST_         C:/ms/boost_1_66_0 )
set(            _DEPENDS_       ${CMAKE_SOURCE_DIR}/depends/)
file(GLOB       _SRC_           "*.cpp")

project(${_NAME_})

message(STATUS "Setting MSVC flags")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /EHc /std:c++latest")

# suppress warning C4996: 'strdup': The POSIX name for this item is deprecated. 
add_definitions(-D_CRT_NONSTDC_NO_DEPRECATE)
add_definitions(-DBOOST_CONFIG_SUPPRESS_OUTDATED_MESSAGE)
add_definitions(-D_SILENCE_CXX17_ITERATOR_BASE_CLASS_DEPRECATION_WARNING)

set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /SAFESEH:NO")

include_directories(${CMAKE_SOURCE_DIR}/
                    ${_BOOST_}
                    ${_DEPENDS_}/libass 
                    ${_DEPENDS_}/vlc-3.0.0/sdk/include/vlc 
                    ${_DEPENDS_}/vlc-3.0.0/sdk/include/vlc/plugins
                    ${_DEPENDS_}/vlc-3.0.0/sdk/include 
                    ${_DEPENDS_}/utf8_v2_3_4/source)
link_directories(   ${CMAKE_SOURCE_DIR}/depends/vlc-3.0.0/sdk/lib 
                    ${_BOOST_}/lib32-msvc-14.1)


add_library(${_NAME_} SHARED ${_SRC_})

target_link_libraries(${_NAME_} libvlc.lib)
target_link_libraries(${_NAME_} libvlccore.lib)
target_link_libraries(${_NAME_} Ws2_32.lib)

install(TARGETS ${_NAME_} DESTINATION ${_BUILD_TO_}/plugins/viril)
configure_file(${CMAKE_SOURCE_DIR}/viril.lua ${_BUILD_TO_}/lua/extensions/viril.lua COPYONLY)