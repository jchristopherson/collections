set(collections_test_sources
    linked_list_test.f90
    list_test.f90
    collections_test.f90
)

add_executable(collections_test ${collections_test_sources})
target_link_libraries(collections_test collections)
add_test(
    NAME collections_test
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:collections_test>
)

if (${BUILD_SHARED_LIBS} AND WIN32)
    add_custom_command(
        TARGET collections_test
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy 
        $<TARGET_RUNTIME_DLLS:collections_test> 
        $<TARGET_FILE_DIR:collections_test>
        COMMAND_EXPAND_LISTS
    )
endif()
