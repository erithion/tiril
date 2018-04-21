cmake_minimum_required(VERSION 2.8.9)
# Variables the user may re-define from the outside: 
#   BOOST_DIR                   - the root folder of the Boost library
#   VIRIL_BUILD_DIR             - controls the folder within CMAKE_INSTALL_PREFIX to build Viril into
#   TIRIL_BUILD_DIR             - controls the folder within CMAKE_INSTALL_PREFIX to build Tiril into

if(NOT DEFINED BOOST_DIR)
    set(BOOST_DIR         C:/ms/boost_1_66_0)
    message(AUTHOR_WARNING  "BOOST_DIR is undefined. Setting it to ${BOOST_DIR}. Use -DBOOST_DIR=<<path>> to redefine it\n")
endif(NOT DEFINED BOOST_DIR)

if(NOT DEFINED VIRIL_BUILD_DIR)
    set(VIRIL_BUILD_DIR         ${CMAKE_CURRENT_SOURCE_DIR}/bin/copy_to_vlc)
    message(AUTHOR_WARNING  "VIRIL_BUILD_DIR is undefined. Setting it to ${VIRIL_BUILD_DIR}. Use -DVIRIL_BUILD_DIR=<<path>> to redefine it\n")
endif(NOT DEFINED VIRIL_BUILD_DIR)

if(NOT DEFINED TIRIL_BUILD_DIR)
    set(TIRIL_BUILD_DIR         ${CMAKE_CURRENT_SOURCE_DIR}/bin/server)
    message(AUTHOR_WARNING  "TIRIL_BUILD_DIR is undefined. Setting it to ${TIRIL_BUILD_DIR}. Use -DTIRIL_BUILD_DIR=<<path>> to redefine it\n")
endif(NOT DEFINED TIRIL_BUILD_DIR)

add_subdirectory(viril)

execute_process(COMMAND stack setup WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tiril)
add_custom_target(tiril ALL stack build WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tiril)

install(CODE "execute_process (COMMAND stack install --local-bin-path ${TIRIL_BUILD_DIR} WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tiril)")
install(DIRECTORY   ${CMAKE_CURRENT_SOURCE_DIR}/tiril/static   DESTINATION ${TIRIL_BUILD_DIR})