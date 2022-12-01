set(source_files
    collections.f90
    collections_container.f90
    collections_list.f90
    collections_linked_list.f90
)

add_library(collections ${source_files})
target_include_directories(collections PRIVATE ${ferror_SOURCE_DIR}/include)
target_link_libraries(collections ferror)
