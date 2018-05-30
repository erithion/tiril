cmake_minimum_required( VERSION 3.11.0 )
include(FetchContent)

set                             ( SIRIL_BUILD_DIR             ${TIRIL_BUILDDIR}/siril )

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

####################################### Selectize.js
message                         ( STATUS "Fetching selectize.js ..." )
FetchContent_Declare            ( selectize
                                  GIT_REPOSITORY https://github.com/selectize/selectize.js
                                  GIT_TAG v0.12.4 )
FetchContent_GetProperties      ( selectize )
if( NOT selectize_POPULATED )
    FetchContent_Populate       ( selectize )
endif()

####################################### Collapsible
message                         ( STATUS "Fetching collapsible ..." )
FetchContent_Declare            ( collapsible
                                  GIT_REPOSITORY https://github.com/jordnkr/collapsible
                                  GIT_TAG v1.2.0 )
FetchContent_GetProperties      ( collapsible )
if( NOT collapsible_POPULATED )
    FetchContent_Populate       ( collapsible )
endif()

####################################### jquery-toggles
message                         ( STATUS "Fetching jquery-toggles ..." )
FetchContent_Declare            ( toggles
                                  GIT_REPOSITORY https://github.com/simontabor/jquery-toggles
                                  GIT_TAG v4.0.0 )
FetchContent_GetProperties      ( toggles )
if( NOT toggles_POPULATED )
    FetchContent_Populate       ( toggles )
endif()

####################################### please-wait
message                         ( STATUS "Fetching please-wait ..." )
FetchContent_Declare            ( wait
                                  GIT_REPOSITORY https://github.com/Pathgather/please-wait
                                  GIT_TAG v0.0.5 )
FetchContent_GetProperties      ( wait )
if( NOT wait_POPULATED )
    FetchContent_Populate       ( wait )
endif()

#######################################

execute_process             ( COMMAND stack setup WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} )
if (DEFINED SIRIL_WITHPROFILE)
    message                 ( STATUS "SIRIL_WITHPROFILE flag is set to ${SIRIL_WITHPROFILE}. Siril will be built with profile information" )
    add_custom_target       ( siril ALL stack build --profile --ghc-options="-auto-all -caf-all -fforce-recomp -O2" WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} )
    install                 ( CODE "execute_process (COMMAND stack install --verbosity error --profile --ghc-options=\"-auto-all -caf-all -fforce-recomp -O2\" --local-bin-path ${TIRIL_BUILD_DIR}/siril WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})" )
else()
    add_custom_target       ( siril ALL stack build WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} )
    install                 ( CODE "execute_process (COMMAND stack install --verbosity error --local-bin-path ${TIRIL_BUILD_DIR}/siril WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})" )
endif()

install                     ( DIRECTORY   ${CMAKE_CURRENT_SOURCE_DIR}/js/   
                              DESTINATION ${SIRIL_BUILD_DIR}/static )
                      
install                     ( DIRECTORY   ${bootstrap_SOURCE_DIR}/css/   
                              DESTINATION ${SIRIL_BUILD_DIR}/static )
install                     ( DIRECTORY   ${bootstrap_SOURCE_DIR}/js/   
                              DESTINATION ${SIRIL_BUILD_DIR}/static )

install                     ( FILES       ${sortable_SOURCE_DIR}/Sortable.js
                              DESTINATION ${SIRIL_BUILD_DIR}/static )
install                     ( FILES       ${sortable_SOURCE_DIR}/Sortable.min.js
                              DESTINATION ${SIRIL_BUILD_DIR}/static )
                              
install                     ( FILES       ${scrollbar_SOURCE_DIR}/dist/perfect-scrollbar.min.js
                              DESTINATION ${SIRIL_BUILD_DIR}/static )
install                     ( FILES       ${scrollbar_SOURCE_DIR}/css/perfect-scrollbar.css
                              DESTINATION ${SIRIL_BUILD_DIR}/static )                                                    
                              
install                     ( FILES       ${toggles_SOURCE_DIR}/css/toggles-full.css
                              DESTINATION ${SIRIL_BUILD_DIR}/static )
install                     ( FILES       ${toggles_SOURCE_DIR}/toggles.min.js
                              DESTINATION ${SIRIL_BUILD_DIR}/static )                                                                                  
                              
install                     ( FILES       ${collapsible_SOURCE_DIR}/jquery.collapsible.js
                              DESTINATION ${SIRIL_BUILD_DIR}/static )

install                     ( FILES       ${selectize_SOURCE_DIR}/dist/css/selectize.default.css
                              DESTINATION ${SIRIL_BUILD_DIR}/static )
install                     ( FILES       ${selectize_SOURCE_DIR}/dist/js/standalone/selectize.min.js
                              DESTINATION ${SIRIL_BUILD_DIR}/static )                                                                                  

install                     ( FILES       ${wait_SOURCE_DIR}/build/please-wait.css
                              DESTINATION ${SIRIL_BUILD_DIR}/static )
install                     ( FILES       ${wait_SOURCE_DIR}/build/please-wait.min.js
                              DESTINATION ${SIRIL_BUILD_DIR}/static )                                                                                  
                              