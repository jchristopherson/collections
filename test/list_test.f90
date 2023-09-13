module list_test
    use collections
    use iso_fortran_env
    implicit none

contains

function test_list() result(rst)
    ! Arguments
    logical :: rst

    ! Variables
    integer(int32), parameter :: n = 20
    integer(int32) :: i, flag, ref(n), ind
    type(list) :: x
    class(*), pointer :: ptr

    ! Initialization
    flag = 0
    rst = .true.

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
                ! go to 100
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

    ! Insert
    ind = 10
    ref = [1, 2, 3, 4, 5, 6, 7, 8, 9, 100, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
    call x%insert(ind, ref(ind))
    if (x%count() /= n) then
        flag = -3
        go to 100
    end if

    do i = 1, n
        ptr => x%get(i)
        select type (ptr)
        type is (integer(int32))
            if (ptr /= ref(i)) then
                flag = i * 100
                go to 100
            end if
        end select
    end do

    ! Remove
    call x%remove(ind)
    if (x%count() /= n - 1) then
        flag = -4
        go to 100
    end if

    do i = 1, n - 1
        ptr => x%get(i)
        select type (ptr)
        type is (integer(int32))
            if (ptr /= i) then
                flag = i * 1000
                go to 100
            end if
        end select
    end do

    ! Clear the list
    call x%clear()
    if (x%count() /= 0) then
        flag = -5
        go to 100
    end if

    ! End
    return

    ! Failed Test
100 continue
    rst = .false.
end function

end module