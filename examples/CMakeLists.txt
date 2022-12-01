# list_example.f90
add_executable(list_example list_example.f90)
target_link_libraries(list_example collections)

# linked_list_example.f90
add_executable(linked_list_example linked_list_example.f90)
target_link_libraries(linked_list_example collections)