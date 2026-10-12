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

    type finalization_probe
        integer(int32) :: value
    contains
        final :: finalize_probe
    end type

    integer(int32) :: finalized_probe_count = 0

contains

subroutine finalize_probe(this)
    type(finalization_probe), intent(inout) :: this
    finalized_probe_count = finalized_probe_count + 1
end subroutine

function test_list() result(rst)
    ! Arguments
    logical :: rst

    ! Variables
    integer(int32), parameter :: n = 20
    integer(int32) :: i, flag, ref(n), ind, baseline
    type(list) :: x
    type(list) :: ownership_test
    type(finalization_probe) :: probe
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

    call x%set(5, 500)
    ptr => x%get(5)
    select type (ptr)
    type is (integer(int32))
        if (ptr /= 500) then
            flag = -12
            go to 100
        end if
    end select
    call x%set(5, 5)
    if (x%get_capacity() < x%count()) then
        flag = -13
        go to 100
    end if

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

    ! Reuse the slot vacated by removal
    call x%push(200)
    if (x%count() /= n) then
        flag = -6
        go to 100
    end if
    do i = 1, n - 1
        ptr => x%get(i)
        select type (ptr)
        type is (integer(int32))
            if (ptr /= i) then
                flag = i * 10000
                go to 100
            end if
        end select
    end do
    ptr => x%get(n)
    select type (ptr)
    type is (integer(int32))
        if (ptr /= 200) then
            flag = -7
            go to 100
        end if
    end select

    ! Decrease the list capacity
    call x%set_capacity(n + 5)
    if (x%get_capacity() /= n + 5 .or. x%count() /= n) then
        flag = -14
        go to 100
    end if
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

    ! Verify removal does not release the item shifted into the removed slot
    finalized_probe_count = 0
    call ownership_test%push(probe)
    call ownership_test%push(probe)
    call ownership_test%push(probe)
    baseline = finalized_probe_count
    call ownership_test%remove(2)
    if (finalized_probe_count - baseline /= 1) then
        flag = -8
        go to 100
    end if
    call ownership_test%push(probe)
    if (finalized_probe_count - baseline /= 1) then
        flag = -9
        go to 100
    end if
    ptr => ownership_test%get(2)
    select type (ptr)
    type is (finalization_probe)
        if (ptr%value /= probe%value) then
            flag = -10
            go to 100
        end if
    class default
        flag = -11
        go to 100
    end select

    ! End
    return

    ! Failed Test
100 continue
    rst = .false.
end function

end module