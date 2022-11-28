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
    integer(int32), intent(out), optional, target :: err

    ! Local Variables
    integer(int32) :: m
    integer(int32), target :: flag
    integer(int32), pointer :: e
    type(container), allocatable :: copy(:)

    ! Initialization
    m = this%count()
    if (present(err)) then
        e => err
    else
        e => flag
    end if
    e = 0

    ! Input Check
    if (n < 1) then
        e = -2
        return
    end if

    ! Quick Return
    if (m == n) return

    ! Process
    if (.not.allocated(this%m_list)) then
        allocate(this%m_list(n), stat = e)
        return
    end if

    if (n > m) then
        ! Increase capacity
        allocate(copy(m), stat = e)
        if (e /= 0) return

        copy = this%m_list

        deallocate(this%m_list)
        allocate(this%m_list(n), stat = e)
        if (e /= 0) return

        this%m_list(1:m) = copy
    else
        ! Decrease capacity
        allocate(copy(n), stat = e)   ! We only need to keep the first n items
        if (e /= 0) return

        copy = this%m_list(1:n)

        deallocate(this%m_list)
        allocate(this%m_list(n), stat = e)
        if (e /= 0) return

        this%m_list = copy
    end if
end subroutine

! ------------------------------------------------------------------------------
module subroutine list_push(this, x, manage, err)
    ! Arguments
    class(list), intent(inout) :: this
    class(*), intent(in), target :: x
    logical, intent(in), optional :: manage
    integer(int32), intent(out), optional, target :: err

    ! Local Variables
    integer(int32) :: index, cap
    integer(int32), target :: flag
    integer(int32), pointer :: e

    ! Initialization
    if (present(err)) then
        e => err
    else
        e => flag
    end if
    e = 0

    index = this%count()
    cap = this%get_capacity()

    ! Ensure there's space for the item
    if ((cap == 0) .or. &
        (cap - index <= 0)) &
    then
        ! We need more room
        call this%set_capacity(cap + DEFAULT_BUFFER_SIZE, e)
        if (e /= 0) return
    end if

    ! Store the item
    this%m_count = index + 1    ! must be before the set routine
    call this%set(index, x, manage, e)
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
    integer(int32), intent(out), optional, target :: err
    class(*), pointer :: rst

    ! Local Variables
    integer(int32), target :: flag
    integer(int32), pointer :: e

    ! Initialization
    nullify(rst)
    if (present(err)) then
        e => err
    else
        e => flag
    end if
    e = 0

    ! Input Check
    if (i < 1 .or. i > this%count()) then
        err = -2
        return
    end if

    ! Process
    rst => this%m_list(i)%get()
end function

! ------------------------------------------------------------------------------
module subroutine list_set(this, i, x, manage, err)
    ! Arguments
    class(list), intent(inout) :: this
    integer(int32), intent(in) :: i
    class(*), intent(in), target :: x
    logical, intent(in), optional :: manage
    integer(int32), intent(out), optional, target :: err

    ! Local Variables
    integer(int32), target :: flag
    integer(int32), pointer :: e
    logical :: mng
    class(*), pointer :: clone

    ! Initialization
    if (present(err)) then
        e => err
    else
        e => flag
    end if
    e = 0
    mng = .true.
    if (present(manage)) mng = manage

    ! Input Check
    if (i < 1 .or. i > this%count()) then
        err = -2
        return
    end if

    ! Store the item
    call this%m_list(i)%free()
    if (mng) then
        this%m_list(i)%m_delete = .true.
        allocate(clone, source = x, stat = e)
        if (e /= 0) return
        this%m_list(i)%m_item => clone
    else
        this%m_list(i)%m_delete = .false.
        this%m_list(i)%m_item => x
    end if
end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule