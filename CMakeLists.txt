cmake_minimum_required (VERSION 2.8.1)

project (sqlb)

option(BUILD_TESTS "build unit tests for sad" OFF)

set_property (GLOBAL PROPERTY USE_FOLDERS ON)
set (CMAKE_INCLUDE_DIRECTORIES_PROJECT_BEFORE ON)
set (CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)
set_directory_properties(PROPERTIES CLEAN_NO_CUSTOM ON)

if (BUILD_TESTS)
   include (FindSQLite3)
   add_subdirectory (tests)
endif ()
