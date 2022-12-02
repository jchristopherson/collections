program linked_list_example
    use collections
    use iso_fortran_env
    implicit none

    ! Variables
    integer(int32), parameter :: n = 10
    integer(int32) :: i
    logical :: check
    type(linked_list) :: x
    class(*), pointer :: ptr

    ! Create a list
    do i = 1, n
        call x%push(i)
    end do

    ! Print it out
    print '(A)', "***** Original List *****"
    check = associated(x%get())
    do while (check)
        ptr => x%get()

        ! The list uses unlimited polymorphic types; therefore, we need to
        ! use the select type construct.
        select type (ptr)
        type is (integer(int32))
            print *, ptr
        end select

        ! Move to the next item
        check = x%next()
    end do

    ! Print out the item at the current iterator position
    print '(A)', new_line('a') // "***** Current Iterator Position *****"
    ptr => x%get()
    select type(ptr)
    type is (integer(int32))
        print *, ptr
    end select

    ! Move to the beginning of the collection
    print '(A)', new_line('a') // "***** Beginning *****"
    call x%move_to_first()
    ptr => x%get()
    select type (ptr)
    type is (integer(int32))
        print *, ptr
    end select
end program