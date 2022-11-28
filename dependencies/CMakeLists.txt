include(FetchContent)

FetchContent_Declare(
    ferror
    GIT_REPOSITORY "https://github.com/jchristopherson/ferror"
)
add_subdirectory(ferror)

if (WIN32)
    if (BUILD_SHARED_LIBS)
        add_custom_command(
            TARGET ${PROJECT_NAME} POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $<TARGET_FILE:ferror>
            $<TARGET_FILE_DIR:${PROJECT_NAME}
        )
    endif()
endif()