cmake_minimum_required(VERSION 3.11)
project(
    flist 
    LANGUAGES Fortran
    VERSION 1.0.0
)

# Debug or Release?
set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build.")
set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release")

# Build a static or shared library?
option(BUILD_SHARED_LIBS "Build shared libraries" off)

# Export all symbols on Windows when building shared libraries
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS true)

# Locate the mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/mod)

# Deal with dependencies
add_subdirectory(dependencies)

# Source
add_subdirectory(src)

# Testing
option(BUILD_TESTING "Build tests")
include(CTest)
message(STATUS "Build tests: ${BUILD_TESTING}")
if (BUILD_TESTING)
    enable_testing()
    add_subdirectory(tests)
endif()