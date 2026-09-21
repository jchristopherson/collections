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
    return
end function

end module