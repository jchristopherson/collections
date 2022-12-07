include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

# list_example.f90
add_executable(list_example list_example.f90)
link_library(list_example collections ${PROJECT_BINARY_DIR}/include)

# linked_list_example.f90
add_executable(linked_list_example linked_list_example.f90)
link_library(linked_list_example collections ${PROJECT_BINARY_DIR}/include)