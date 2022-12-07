set(dir ${CMAKE_CURRENT_SOURCE_DIR})
list(
    APPEND COLLECTIONS_SOURCES
    ${dir}/collections.f90
    ${dir}/collections_container.f90
    ${dir}/collections_list.f90
    ${dir}/collections_linked_list.f90
)
set(COLLECTIONS_SOURCES ${COLLECTIONS_SOURCES} PARENT_SCOPE)

# add_library(collections ${source_files})
# target_include_directories(collections PRIVATE ${ferror_SOURCE_DIR}/include)
# target_link_libraries(collections ferror)
