cmake_minimum_required(VERSION 2.8.9)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
message(FATAL_ERROR "CMAKE_INSTALL_PREFIX is set to default ${CMAKE_INSTALL_PREFIX}. Run `cmake.exe -DCMAKE_INSTALL_PREFIX=<<path to the installation folder of the build>> ..` to fix this")
else()
message(STATUS "CMAKE_INSTALL_PREFIX is ${CMAKE_INSTALL_PREFIX}")
endif(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)

set(_BOOST_         C:/ms/boost_1_66_0 )
add_subdirectory(viril)

execute_process(COMMAND stack setup WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tiril)
add_custom_target(tiril ALL stack build WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tiril)

install(CODE "execute_process (COMMAND stack install --local-bin-path ${CMAKE_INSTALL_PREFIX}/server WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tiril)")