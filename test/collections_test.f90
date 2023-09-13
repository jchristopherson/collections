program main
    use iso_fortran_env
    use list_test
    use linked_list_test
    implicit none

    ! Variables
    logical :: local

    ! Tests
    local = test_list()
    if (.not. local) stop 1

    local = test_linked_list()
    if (.not. local) stop 2

    ! End
end program