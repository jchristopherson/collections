program list_test
    use flist
    use iso_fortran_env
    implicit none

    ! Variables
    integer(int32) :: flag
    type(list) :: x


    ! Initialization
    flag = 0


    ! End
    call exit(flag)
end program