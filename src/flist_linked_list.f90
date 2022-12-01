submodule (flist) flist_linked_list
contains
! ------------------------------------------------------------------------------
pure module function ll_count(this) result(rst)
    class(linked_list), intent(in) :: this
    integer(int32) :: rst
    rst = this%m_count
end function

! ------------------------------------------------------------------------------
module subroutine ll_move_to_first(this)
    class(linked_list), intent(inout) :: this
    if (associated(this%m_first)) then
        this%m_current => this%m_first
    end if
end subroutine

! ------------------------------------------------------------------------------
module subroutine ll_move_to_last(this)
    class(linked_list), intent(inout) :: this
    if (associated(this%m_last)) then
        this%m_current => this%m_last
    end if
end subroutine

! ------------------------------------------------------------------------------
module function ll_move_to_next(this) result(rst)
    ! Arguments
    class(linked_list), intent(inout) :: this
    logical :: rst

    ! Process
    if (.not.associated(this%m_current)) then
        rst = .false.
        return
    end if
    if (associated(this%m_current%next)) then
        this%m_current => this%m_current%next
        rst = .true.
    else
        rst = .false.
    end if
end function

! ------------------------------------------------------------------------------
module function ll_move_to_previous(this) result(rst)
    ! Arguments
    class(linked_list), intent(inout) :: this
    logical :: rst

    ! Process
    if (.not.associated(this%m_current)) then
        rst = .false.
        return
    end if
    if (associated(this%m_current%previous)) then
        this%m_current => this%m_current%previous
        rst = .true.
    else
        rst = .false.
    end if
end function

! ------------------------------------------------------------------------------
module function ll_get(this) result(rst)
    ! Arguments
    class(linked_list), intent(in) :: this
    class(*), pointer :: rst

    ! Process
    if (associated(this%m_current)) then
        rst => this%m_current%get()
    else
        rst => null()
    end if
end function

! ------------------------------------------------------------------------------
module subroutine ll_set(this, x, manage, err)
    ! Arguments
    class(linked_list), intent(inout) :: this
    class(*), intent(in), target :: x
    logical, intent(in), optional :: manage
    class(errors), intent(inout), optional, target :: err

    ! Local Variables
    logical :: mng
    integer(int32) :: flag
    class(*), pointer :: clone
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

    ! Ensure we're at a valid node
    if (.not.associated(this%m_current)) then
        call errmgr%report_error("ll_set", &
            "The current list position is invalid.", &
            FL_INVALID_ITERATOR_ERROR)
        return
    end if

    ! Process
    call this%m_current%free()
    if (mng) then
        allocate(clone, source = x, stat = flag)
        if (flag /= 0) then
            write(errmsg, 100) &
                "A memory allocation error was encountered.  Flag ", &
                flag, "."
            call errmgr%report_error("ll_set", trim(errmsg), &
                FL_OUT_OF_MEMORY_ERROR)
        end if
        this%m_current%m_item => clone
    else
        this%m_current%m_item => x
        this%m_current%m_delete = .false.
    end if

    ! Formatting
100 format(A, I0, A)
end subroutine

! ------------------------------------------------------------------------------
module subroutine ll_push(this, x, manage, err)
    ! Arguments
    class(linked_list), intent(inout) :: this
    class(*), intent(in), target :: x
    logical, intent(in), optional :: manage
    class(errors), intent(inout), optional, target :: err

    ! Local Variables
    integer(int32) :: n, flag
    logical :: mng
    type(node), pointer :: nd, temp, newnode
    class(*), pointer :: clone
    class(errors), pointer :: errmgr
    type(errors), target :: deferr
    character(len = 256) :: errmsg
    
    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if
    flag = 0
    n = this%count()
    mng = .true.
    if (present(manage)) mng = manage

    ! Process
    if (n == 0) then
        ! This is the first node
        if (associated(this%m_first)) then
            ! This shouldn't ever be needed, but is a kind of catch-all
            call this%m_first%free()
            deallocate(this%m_first)
        end if
        
        allocate(this%m_first, stat = flag)
        if (flag /= 0) go to 100

        this%m_first%next => null()
        this%m_first%previous => null()
        nd => this%m_first

        ! As this is the first, it's also the last and the current
        this%m_last => this%m_first
        this%m_current => this%m_first
    else
        ! We have a new last item
        allocate(newnode, stat = flag)
        if (flag /= 0) go to 100

        temp => this%m_last

        this%m_last => newnode
        this%m_last%previous => temp
        this%m_last%next => null()
        nd => this%m_last
    end if

    ! Store the item
    if (mng) then
        allocate(clone, source = x, stat = flag)
        if (flag /= 0) go to 100
        nd%m_item => clone
    else
        nd%m_item => x
        nd%m_delete = .false.
    end if

    ! Index the count
    this%m_count = this%m_count + 1

    ! End
    return

    ! Memory Error Handling
100 continue
    if (flag /= 0) then
        write(errmsg, 101) &
            "A memory allocation error was encountered.  Flag ", &
            flag, "."
        call errmgr%report_error("ll_push", trim(errmsg), &
            FL_OUT_OF_MEMORY_ERROR)
    end if

    ! Formatting
101 format(A, I0, A)
end subroutine

! ------------------------------------------------------------------------------
module subroutine ll_pop(this)
    ! Arguments
    class(linked_list), intent(inout) :: this

    ! Local Variables
    type(node), pointer :: temp

    ! Quick Return
    if (this%count() == 0) return
    if (.not.associated(this%m_last)) return

    ! Process
    if (this%count() == 1) then
        call this%clear()
    else
        temp => this%m_last%previous
        nullify(temp%next)
        if (associated(this%m_current, this%m_last)) then
            ! If the iterator is referencing the last item shift it to the 
            ! "new" last item
            this%m_current => temp
        end if
        call this%m_last%free()
        deallocate(this%m_last)
        this%m_last => temp
        this%m_count = this%m_count - 1
    end if
end subroutine

! ------------------------------------------------------------------------------
module subroutine ll_clear(this)
    ! Arguments
    class(linked_list), intent(inout) :: this

    ! Local Variables
    integer(int32) :: i, n
    type(node), pointer :: currentNode, nextNode

    ! Initialization
    n = this%count()

    ! Quick Return
    if (n == 0) return

    ! Process
    currentNode => this%m_current
    do i = 1, n
        if (.not.associated(currentNode)) cycle
        
        nextNode => currentNode%next
        call currentNode%free()
        deallocate(currentNode)
        nullify(currentNode)

        currentNode => nextNode
    end do
    this%m_count = 0
    nullify(this%m_first)
    nullify(this%m_last)
    nullify(this%m_current)
end subroutine

! ------------------------------------------------------------------------------
module subroutine ll_destroy(this)
    type(linked_list), intent(inout) :: this
    call this%clear()
end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule