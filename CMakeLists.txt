cmake_minimum_required(VERSION 3.17)
project(
    collections 
    LANGUAGES Fortran
    VERSION 1.0.0
)

# Utilize the GNU installation structure
include(GNUInstallDirs)

# Get the macros and functions we'll need
include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

# Configure everything
add_subdirectory(configure)

# Deal with dependencies
find_package(ferror 1.4.0)
add_subdirectory(dependencies)

# Source
add_subdirectory(src)
add_fortran_library(
    ${PROJECT_NAME}
    ${PROJECT_INCLUDE_DIR}
    ${CMAKE_INSTALL_INCLUDEDIR}
    ${PROJECT_VERSION}
    ${PROJECT_VERSION_MAJOR}
    ${COLLECTIONS_SOURCES}
)
link_library(${PROJECT_NAME} ${ferror_LIBRARY} ${ferror_INCLUDE_DIR})


# Testing
option(BUILD_TESTING "Build tests")
include(CTest)
message(STATUS "Build tests: ${BUILD_TESTING}")
if (BUILD_TESTING)
    enable_testing()
    add_subdirectory(tests)
endif()

# Examples
option(BUILD_collections_EXAMPLES "Build examples")
message(STATUS "Build collections examples: ${BUILD_collections_EXAMPLES}")
if (BUILD_collections_EXAMPLES)
    add_subdirectory(examples)
endif()