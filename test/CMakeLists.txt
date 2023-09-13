include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

set(collections_test_sources
    linked_list_test.f90
    list_test.f90
    collections_test.f90
)

add_executable(collections_test ${collections_test_sources})
link_library(collections_test ${PROJECT_NAME} ${PROJECT_INCLUDE_DIR})
add_test(
    NAME collections_test
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:collections_test>
)