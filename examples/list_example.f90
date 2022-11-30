program list_example
    use flist
    use iso_fortran_env
    implicit none

    ! Variables
    integer(int32), parameter :: n = 10
    integer(int32) :: i
    type(list) :: x
    class(*), pointer :: ptr

    ! Create a list
    do i = 1, n
        call x%push(2 * i)
    end do

    ! Print it out to the command line
    print '(A)', "***** Original List *****"
    do i = 1, n
        ptr => x%get(i)
        
        ! The list uses unlimited polymorphic types; therefore, we need to
        ! use the select type construct.
        select type (ptr)
        type is (integer(int32))
            print *, ptr
        end select
    end do

    ! Insert the integer value of 100 into the 5th slot in the list
    call x%insert(5, 100)

    ! Print it out again to illustrate the change
    print '(A)', new_line('a') // "***** After Insertion *****"
    do i = 1, x%count()
        ptr => x%get(i)
        select type (ptr)
        type is (integer(int32))
            print *, ptr
        end select
    end do
end program