set(source_files
    flist.f90
    flist_container.f90
    flist_list.f90
)

add_library(flist ${source_files})
target_include_directories(flist PRIVATE ${ferror_SOURCE_DIR}/include)
target_link_libraries(flist ferror)
