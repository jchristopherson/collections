program linked_list_test
    use flist
    use iso_fortran_env
    implicit none

    ! Variables
    integer(int32), parameter :: n = 20
    integer(int32) :: i, flag
    type(linked_list) :: x
    class(*), pointer :: ptr
    logical :: check

    ! Initialization
    flag = 0

    ! Fill the list
    do i = 1, n
        call x%push(i)
    end do

    ! Check
    if (x%count() /= n) then
        flag = -1
        go to 100
    end if

    do i = 1, n
        ptr => x%get()
        select type (ptr)
        type is (integer(int32))
            if (ptr /= i) then
                flag = i
                go to 100
            end if
        end select

        check = x%next()
        if (.not.check) exit
    end do


    ! End
100 continue
    call exit(flag)
end program