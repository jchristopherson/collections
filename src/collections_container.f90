submodule (collections) collections_container
    implicit none
contains
! ------------------------------------------------------------------------------
module function c_get(this) result(rst)
    class(container), intent(in) :: this
    class(*), pointer :: rst
    rst => this%m_item
end function

! ------------------------------------------------------------------------------
pure module function c_get_delete(this) result(rst)
    class(container), intent(in) :: this
    logical :: rst
    rst = this%m_delete
end function

! ------------------------------------------------------------------------------
module subroutine c_destroy(this)
    type(container), intent(inout) :: this
    call this%free()
end subroutine

! ------------------------------------------------------------------------------
module subroutine c_cleanup(this)
    class(container), intent(inout) :: this
    if (this%delete_on_cleanup() .and. associated(this%m_item))  then
        deallocate(this%m_item)
        nullify(this%m_item)
    else
        nullify(this%m_item)
    end if
end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule