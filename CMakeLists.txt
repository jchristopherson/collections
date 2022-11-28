cmake_minimum_required(VERSION 3.10)
project(
    flist 
    LANGUAGES Fortran
    VERSION 1.0.0
)

# Version
set(FLIST_MAJOR_VERSION 1)
set(FLIST_MINOR_VERSION 0)
set(FLIST_PATCH_VERSION 0)
set(FLIST_VERSION ${FLIST_MAJOR_VERSION}.${FLIST_MINOR_VERSION}.${FLIST_PATCH_VERSION})

# Debug or Release?
set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build.")
set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release")

# Build a static or shared library?
option(BUILD_SHARED_LIBS "Build shared libraries" off)

# Export all symbols on Windows when building shared libraries
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS true)

# Locate the mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/mod)

# Locate FERROR
find_package(ferror)
if (FERROR_FOUND)
    message(STATUS "FERROR library found.")
else()
    # Not found, download and install the proper version
    message(STATUS "FERROR library not found.")
endif()

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