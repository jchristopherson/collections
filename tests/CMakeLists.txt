macro(create_test testname codename)
    add_executable(${testname} ${codename})
    target_link_libraries(${testname} flist)
    add_test(
        NAME ${testname}
        WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
        COMMAND $<TARGET_FILE:${testname}>
    )
endmacro()

create_test(list_test list_test.f90)