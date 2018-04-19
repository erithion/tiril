cmake_minimum_required(VERSION 2.8.9)

# To call it outside the root cmakefile define BOOST_DIR and VIRIL_BUILD_DIR variables

set(PROJECT_NAME                libviril_plugin)
set(EXTERNAL_LIBS_DIR           ${CMAKE_CURRENT_SOURCE_DIR}/depends/)
set(CMAKE_CXX_FLAGS             "${CMAKE_CXX_FLAGS} /EHc /std:c++latest")
set(CMAKE_SHARED_LINKER_FLAGS   "${CMAKE_SHARED_LINKER_FLAGS} /SAFESEH:NO")
file(GLOB 
    SRC_FILES                   "*.cpp")
file(GLOB 
    INC_FILES                   "*.h")

project(${PROJECT_NAME})
message(STATUS "CXX flags ${CMAKE_CXX_FLAGS}")
message(STATUS "Linker flags ${CMAKE_SHARED_LINKER_FLAGS}")

add_definitions(-D_CRT_NONSTDC_NO_DEPRECATE) # warning C4996: 'strdup': The POSIX name for this item is deprecated. 
add_definitions(-DBOOST_CONFIG_SUPPRESS_OUTDATED_MESSAGE)
add_definitions(-D_SILENCE_CXX17_ITERATOR_BASE_CLASS_DEPRECATION_WARNING)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/
                    ${BOOST_DIR}
                    ${EXTERNAL_LIBS_DIR}
                    ${EXTERNAL_LIBS_DIR}/libass 
                    ${EXTERNAL_LIBS_DIR}/vlc-3.0.0/sdk/include/vlc 
                    ${EXTERNAL_LIBS_DIR}/vlc-3.0.0/sdk/include/vlc/plugins
                    ${EXTERNAL_LIBS_DIR}/vlc-3.0.0/sdk/include 
                    ${EXTERNAL_LIBS_DIR}/utf8_v2_3_4/source)
link_directories(   ${CMAKE_CURRENT_SOURCE_DIR}/depends/vlc-3.0.0/sdk/lib 
                    ${BOOST_DIR}/lib32-msvc-14.1)

add_library(${PROJECT_NAME} SHARED ${SRC_FILES} ${INC_FILES} ${EXTERNAL_LIBS_DIR}/poll.cpp)

target_link_libraries(${PROJECT_NAME} libvlc.lib)
target_link_libraries(${PROJECT_NAME} libvlccore.lib)
target_link_libraries(${PROJECT_NAME} Ws2_32.lib)

install(TARGETS ${PROJECT_NAME}                         DESTINATION ${VIRIL_BUILD_DIR}/plugins/viril)
install(FILES   ${CMAKE_CURRENT_SOURCE_DIR}/viril.lua   DESTINATION ${VIRIL_BUILD_DIR}/lua/extensions)