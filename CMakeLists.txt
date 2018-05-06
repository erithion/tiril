cmake_minimum_required( VERSION 3.11.0 )
include(FetchContent)
# Variables the user may re-define from the outside: 
#   BOOST_DIR             - the root folder of the Boost library
#   TIRIL_BUILD_DIR             - controls the build output folder
#   INSTALL_VLC           - if defined, Viril will be built into VLC and installed into the output folder along with it
#   PROFILE               - if defined, Tiril is built with profiling information within

if( NOT DEFINED BOOST_DIR )
    set                         ( BOOST_DIR          C:/ms/boost_1_66_0 )
    message                     ( STATUS             "BOOST_DIR is undefined. Setting it to ${BOOST_DIR}. Use -DBOOST_DIR=[path] to redefine it" )
endif()

if( NOT DEFINED TIRIL_BUILD_DIR )
    set                         ( TIRIL_BUILD_DIR    ${CMAKE_CURRENT_SOURCE_DIR}/bin )
    message                     ( STATUS             "TIRIL_BUILD_DIR is undefined. Setting it to ${TIRIL_BUILD_DIR}. Use -DTIRIL_BUILD_DIR=[path] to redefine it" )
endif()

add_subdirectory    ( viril )

# External projects Tiril depends on
####################################### Bootstrap
message                         ( STATUS "Fetching Bootstrap ..." )
FetchContent_Declare            ( bootstrap
                                  URL https://github.com/twbs/bootstrap/releases/download/v4.1.0/bootstrap-4.1.0-dist.zip )
FetchContent_GetProperties      ( bootstrap )
if( NOT bootstrap_POPULATED )
    FetchContent_Populate       ( bootstrap )
endif()

####################################### Sortable
message                         ( STATUS "Fetching Sortable ..." )
FetchContent_Declare            ( sortable
                                  GIT_REPOSITORY https://github.com/RubaXa/Sortable
                                  GIT_TAG 1.4.0 )
FetchContent_GetProperties      ( sortable )
if( NOT sortable_POPULATED )
    FetchContent_Populate       ( sortable )
endif()

####################################### Perfect-scrollbar
message                         ( STATUS "Fetching perfect-scrollbar ..." )
FetchContent_Declare            ( scrollbar
                                  GIT_REPOSITORY https://github.com/utatti/perfect-scrollbar
                                  GIT_TAG 1.3.0 )
FetchContent_GetProperties      ( scrollbar )
if( NOT scrollbar_POPULATED )
    FetchContent_Populate       ( scrollbar )
endif()

#######################################

execute_process             ( COMMAND stack setup WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tiril )
if (DEFINED PROFILE)
    message                 ( STATUS "PROFILE flag is set to ${PROFILE}. Tiril will be built with profile information" )
    add_custom_target       ( tiril ALL stack build --profile --ghc-options="-auto-all -caf-all -fforce-recomp -O2" WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tiril )
    install                 ( CODE "execute_process (COMMAND stack install --verbosity error --profile --ghc-options=\"-auto-all -caf-all -fforce-recomp -O2\" --local-bin-path ${TIRIL_BUILD_DIR}/server WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tiril)" )
else()
    add_custom_target       ( tiril ALL stack build WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tiril )
    install                 ( CODE "execute_process (COMMAND stack install --verbosity error --local-bin-path ${TIRIL_BUILD_DIR}/server WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tiril)" )
endif()

install                     ( DIRECTORY   ${CMAKE_CURRENT_SOURCE_DIR}/tiril/js/   
                              DESTINATION ${TIRIL_BUILD_DIR}/server/static )
                      
install                     ( DIRECTORY   ${bootstrap_SOURCE_DIR}/css/   
                              DESTINATION ${TIRIL_BUILD_DIR}/server/static )
install                     ( DIRECTORY   ${bootstrap_SOURCE_DIR}/js/   
                              DESTINATION ${TIRIL_BUILD_DIR}/server/static )

install                     ( FILES       ${sortable_SOURCE_DIR}/Sortable.js
                              DESTINATION ${TIRIL_BUILD_DIR}/server/static )
install                     ( FILES       ${sortable_SOURCE_DIR}/Sortable.min.js
                              DESTINATION ${TIRIL_BUILD_DIR}/server/static )
                              
install                     ( FILES       ${scrollbar_SOURCE_DIR}/dist/perfect-scrollbar.min.js
                              DESTINATION ${TIRIL_BUILD_DIR}/server/static )
install                     ( FILES       ${scrollbar_SOURCE_DIR}/css/perfect-scrollbar.css
                              DESTINATION ${TIRIL_BUILD_DIR}/server/static )                                                    