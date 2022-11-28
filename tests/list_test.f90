program list_test
    use flist
    use iso_fortran_env
    implicit none

    ! Variables
    integer(int32), parameter :: n = 20
    integer(int32) :: i, flag
    type(list) :: x
    class(*), pointer :: ptr

    ! Initialization
    flag = 0

    ! Fill the list with integers
    do i = 1, n
        call x%push(i)
    end do

    ! Verify the count
    if (x%count() /= n) then
        flag = -1
        go to 100
    end if

    ! Verify each item
    do i = 1, n
        ptr => x%get(i)
        select type (ptr)
        type is (integer(int32))
            if (ptr /= i) then
                flag = i
                go to 100
            end if
        end select
    end do

    ! Remove the last item from the list
    call x%pop()
    if (x%count() /= n - 1) then
        flag = -2
        go to 100
    end if

    do i = 1, n - 1
        ptr => x%get(i)
        select type (ptr)
        type is (integer(int32))
            if (ptr /= i) then
                flag = i * 10
                go to 100
            end if
        end select
    end do

    ! Reverse the list
    call x%reverse()
    do i = n - 1, 1, -1
        ptr => x%get(i)
        select type (ptr)
        type is (integer(int32))
            if (ptr /= i) then
                flag = i * 100
                go to 100
            end if
        end select
    end do

    ! End
100 continue
    call exit(flag)
end program