module linked_list_test
    use collections
    use iso_fortran_env
    implicit none
contains

function test_linked_list() result(rst)
    ! Arguments
    logical :: rst

    ! Variables
    integer(int32), parameter :: n = 20
    integer(int32) :: i, flag
    type(linked_list) :: x
    class(*), pointer :: ptr
    logical :: check

    ! Initialization
    flag = 0
    rst = .true.

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

    ! Pop an item of the back end
    call x%move_to_last() ! This is to test a feature in the pop code that resets the iterator position
    call x%pop()

    if (x%count() /= n - 1) then
        flag = -2
        go to 100
    end if

    ! Iterate around the list - currently the iterator should be at the end of 
    ! the list
    ptr => x%get()
    select type (ptr)
    type is (integer(int32))
        if (ptr /= n - 1) then
            flag = -3
            go to 100
        end if
    end select

    ! Move to the front of the list
    call x%move_to_first()
    ptr => x%get()
    select type (ptr)
    type is (integer(int32))
        if (ptr /= 1) then
            flag = -4
            go to 100
        end if
    end select

    ! Set an item into the list
    call x%set(100)
    ptr => x%get()
    select type (ptr)
    type is (integer(int32))
        if (ptr /= 100) then
            flag = -5
            go to 100
        end if
    end select

    ! Clear the list
    call x%clear()
    if (x%count() /= 0) then
        flag = -6
        go to 100
    end if

    ! End
    return

    ! Test Failure
100 continue
    rst = .false.
end function

end module