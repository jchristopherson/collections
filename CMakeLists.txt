cmake_minimum_required(VERSION 3.24)
project(
    collections 
    LANGUAGES Fortran
    VERSION 1.0.4
)
set(CMAKE_Fortran_STANDARD 2018)
set(CMAKE_Fortran_STANDARD_REQUIRED TRUE)

# Configure everything
add_subdirectory(configure)

# Source
add_subdirectory(src)

# Installation Instructions
install(
    EXPORT ${PROJECT_NAME}-targets
    NAMESPACE ${PROJECT_NAME}::
    DESTINATION "${CMAKE_INSALL_LIBDIR}/cmake/${PROJECT_NAME}"
)

# Testing
option(BUILD_TESTING "Build tests")
include(CTest)
message(STATUS "Build tests: ${BUILD_TESTING}")
if (BUILD_TESTING)
    enable_testing()
    add_subdirectory(test)
endif()

# Examples
option(BUILD_COLLECTIONS_EXAMPLES "Build collections examples")
message(STATUS "Build COLLECTIONS examples: ${BUILD_COLLECTIONS_EXAMPLES}")
if (${BUILD_COLLECTIONS_EXAMPLES})
    add_subdirectory(examples)
endif()