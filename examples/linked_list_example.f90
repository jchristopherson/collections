! Copyright (c) 2022-2026 Jason Christopherson
! SPDX-License-Identifier: MIT
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

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