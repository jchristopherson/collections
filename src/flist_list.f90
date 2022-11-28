submodule (flist) flist_list
    implicit none

    ! Default buffer size
    integer(int32), parameter :: DEFAULT_BUFFER_SIZE = 10

contains
! ------------------------------------------------------------------------------
pure module function list_get_count(this) result(rst)
    class(list), intent(in) :: this
    integer(int32) :: rst
    rst = this%m_count
end function

! ------------------------------------------------------------------------------
pure module function list_get_capacity(this) result(rst)
    class(list), intent(in) :: this
    integer(int32) :: rst
    if (allocated(this%m_list)) then
        rst = size(this%m_list)
    else
        rst = 0
    end if
end function

! ------------------------------------------------------------------------------
module subroutine list_set_capacity(this, n, err)
    ! Arguments
    class(list), intent(inout) :: this
    integer(int32), intent(in) :: n
    class(errors), intent(inout), optional, target :: err

    ! Local Variables
    integer(int32) :: m
    integer(int32) :: flag
    type(container), allocatable :: copy(:)
    class(errors), pointer :: errmgr
    type(errors), target :: deferr
    character(len = 256) :: errmsg
    
    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if
    m = this%count()

    ! Input Check
    if (n < 1) then
        call errmgr%report_error("list_set_capacity", &
            "The requested capacity must be larger than 1.", &
            FL_INVALID_ARGUMENT_ERROR)
        return
    end if

    ! Quick Return
    if (m == n) return

    ! Process
    if (.not.allocated(this%m_list)) then
        allocate(this%m_list(n), stat = flag)
        if (flag /= 0) go to 100
        return
    end if

    if (n > m) then
        ! Increase capacity
        allocate(copy(m), stat = flag)
        if (flag /= 0) go to 100

        copy = this%m_list

        deallocate(this%m_list)
        allocate(this%m_list(n), stat = flag)
        if (flag /= 0) go to 100

        this%m_list(1:m) = copy
    else
        ! Decrease capacity
        allocate(copy(n), stat = flag)   ! We only need to keep the first n items
        if (flag /= 0) go to 100

        copy = this%m_list(1:n)

        deallocate(this%m_list)
        allocate(this%m_list(n), stat = flag)
        if (flag /= 0) go to 100

        this%m_list = copy
    end if
    return

    ! Memory Error Handling
100 continue
    if (flag /= 0) then
        write(errmsg, 10) &
            "A memory allocation error was encountered.  Flag ", &
            flag, "."
        call errmgr%report_error("list_set_capacity", trim(errmsg), &
            FL_OUT_OF_MEMORY_ERROR)
    end if
    return

    ! Formatting
10  format(A, I0, A)
end subroutine

! ------------------------------------------------------------------------------
module subroutine list_push(this, x, manage, err)
    ! Arguments
    class(list), intent(inout) :: this
    class(*), intent(in), target :: x
    logical, intent(in), optional :: manage
    class(errors), intent(inout), optional, target :: err

    ! Local Variables
    integer(int32) :: index, cap
    class(errors), pointer :: errmgr
    type(errors), target :: deferr
    
    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if
    index = this%count() + 1
    cap = this%get_capacity()

    ! Ensure there's space for the item
    if ((cap == 0) .or. &
        (cap - index <= 0)) &
    then
        ! We need more room
        call this%set_capacity(cap + DEFAULT_BUFFER_SIZE, errmgr)
        if (errmgr%has_error_occurred()) return
    end if

    ! Store the item
    this%m_count = index    ! must be before the set routine
    call this%set(index, x, manage, errmgr)
    if (errmgr%has_error_occurred()) return
end subroutine

! ------------------------------------------------------------------------------
module subroutine list_pop(this)
    ! Arguments
    class(list), intent(inout) :: this

    ! Process
    integer(int32) :: index
    index = this%count()
    if (index == 0) return

    call this%m_list(index)%free()
    this%m_count = index - 1
end subroutine

! ------------------------------------------------------------------------------
module function list_get(this, i, err) result(rst)
    ! Arguments
    class(list), intent(in) :: this
    integer(int32), intent(in) :: i
    class(errors), intent(inout), optional, target :: err
    class(*), pointer :: rst

    ! Local Variables
    class(errors), pointer :: errmgr
    type(errors), target :: deferr
    character(len = 256) :: errmsg
    
    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if
    nullify(rst)

    ! Input Check
    if (i < 1 .or. i > this%count()) then
        write(errmsg, 100) "The supplied index of ", i, &
            " is outside the bounds of the array [1, ", this%count(), "]."
        call errmgr%report_error("list_get", &
            trim(errmsg), FL_INDEX_OUT_OF_RANGE_ERROR)
        return
    end if

    ! Process
    rst => this%m_list(i)%get()
    return

    ! Formatting
100 format(A, I0, A, I0, A)
end function

! ------------------------------------------------------------------------------
module subroutine list_set(this, i, x, manage, err)
    ! Arguments
    class(list), intent(inout) :: this
    integer(int32), intent(in) :: i
    class(*), intent(in), target :: x
    logical, intent(in), optional :: manage
    class(errors), intent(inout), optional, target :: err

    ! Local Variables
    logical :: mng
    class(*), pointer :: clone
    integer(int32) :: flag
    class(errors), pointer :: errmgr
    type(errors), target :: deferr
    character(len = 256) :: errmsg
    
    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if
    mng = .true.
    if (present(manage)) mng = manage

    ! Input Check
    if (i < 1 .or. i > this%count()) then
        write(errmsg, 101) "The supplied index of ", i, &
            " is outside the bounds of the array [1, ", this%count(), "]."
        call errmgr%report_error("list_set", &
            trim(errmsg), &
            FL_INDEX_OUT_OF_RANGE_ERROR)
        return
    end if

    ! Store the item
    call this%m_list(i)%free()
    if (mng) then
        this%m_list(i)%m_delete = .true.
        allocate(clone, source = x, stat = flag)
        if (flag /= 0) then
            write(errmsg, 100) &
                "A memory allocation error was encountered.  Flag ", &
                flag, "."
            call errmgr%report_error("list_set", trim(errmsg), &
                FL_OUT_OF_MEMORY_ERROR)
            return
        end if
        this%m_list(i)%m_item => clone
    else
        this%m_list(i)%m_delete = .false.
        this%m_list(i)%m_item => x
    end if
    return

    ! Formatting
100 format(A, I0, A)
101 format(A, I0, A, I0, A)
end subroutine

! ------------------------------------------------------------------------------
module subroutine list_reverse(this)
    ! Arguments
    class(list), intent(inout) :: this

    ! Local Variables
    integer(int32) :: i, j, m, n
    type(container) :: temp

    ! Initialization
    n = this%count()

    ! Quick Return
    if (n < 2) return
    
    ! Process
    if (mod(n, 2) == 0) then
        m = n / 2
    else
        m = (n - 1) / 2
    end if
    j = n
    do i = 1, m
        temp = this%m_list(j)
        this%m_list(j) = this%m_list(i)
        this%m_list(i) = temp
        j = j - 1
    end do
end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule