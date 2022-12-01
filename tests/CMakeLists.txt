macro(create_test testname codename)
    add_executable(${testname} ${codename})
    target_link_libraries(${testname} collections)
    add_test(
        NAME ${testname}
        WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
        COMMAND $<TARGET_FILE:${testname}>
    )
endmacro()

create_test(list_test list_test.f90)
create_test(linked_list_test linked_list_test.f90)