# list_example.f90
add_executable(list_example list_example.f90)
target_link_libraries(list_example collections)

# linked_list_example.f90
add_executable(linked_list_example linked_list_example.f90)
target_link_libraries(linked_list_example collections)

if (${BUILD_SHARED_LIBS} AND WIN32)
    add_custom_command(
        TARGET list_example
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy 
        $<TARGET_RUNTIME_DLLS:list_example> 
        $<TARGET_FILE_DIR:list_example>
        COMMAND_EXPAND_LISTS
    )
endif()
