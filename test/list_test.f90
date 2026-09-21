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

    ! Decrease the list capacity
    call x%set_capacity(n / 2)
    if (x%count() /= n / 2) then
        flag = -5
        go to 100
    end if
    do i = 1, n / 2
        ptr => x%get(i)
        select type (ptr)
        type is (integer(int32))
            if (ptr /= i) then
                flag = i * 100000
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