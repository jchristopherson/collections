!> @mainpage
!!
!! @section intro_sec Introduction
!! COLLECTIONS is  a library providing a set of types supporting collections in 
!! Fortran.  The collection types within this library are object-oriented by 
!! design, and utilize Fortran's unlimited polymorphic variable functionallity
!! allowing storage of any data type.
!!
!! @par Example
!! One of the collection types this library provides is a generic, dynamically
!! sizeable type referred to simply as list.  A simple example illustrating 
!! basic usage of the list type is as follows.
!!
!! @code{.f90}
!! program list_example
!!     use collections
!!     use iso_fortran_env
!!     implicit none
!!
!!     ! Variables
!!     integer(int32), parameter :: n = 10
!!     integer(int32) :: i
!!     type(list) :: x
!!     class(*), pointer :: ptr
!!
!!     ! Create a list
!!     do i = 1, n
!!         call x%push(2 * i)
!!     end do
!!
!!     ! Print it out to the command line
!!     print '(A)', "***** Original List *****"
!!     do i = 1, n
!!         ptr => x%get(i)
!!
!!         ! The list uses unlimited polymorphic types; therefore, we need to
!!         ! use the select type construct.
!!         select type (ptr)
!!         type is (integer(int32))
!!             print *, ptr
!!         end select
!!     end do
!!
!!     ! Insert the integer value of 100 into the 5th slot in the list
!!     call x%insert(5, 100)
!!
!!     ! Print it out again to illustrate the change
!!     print '(A)', new_line('a') // "***** After Insertion *****"
!!     do i = 1, x%count()
!!         ptr => x%get(i)
!!         select type (ptr)
!!         type is (integer(int32))
!!             print *, ptr
!!         end select
!!     end do
!! @endcode
!! 
!! This program generates the following output.
!! @code{.txt}
!! ***** Original List *****
!!            2
!!            4
!!            6
!!            8
!!           10
!!           12
!!           14
!!           16
!!           18
!!           20
!!
!! ***** After Insertion *****
!!            2
!!            4
!!            6
!!            8
!!          100
!!           10
!!           12
!!           14
!!           16
!!           18
!!           20
!! @endcode

!> A module containing various collections using Fortran's unlimited polymorphic
!! functionallity.
module collections
    use iso_fortran_env
    implicit none
    private
    public :: list
    public :: linked_list

! ------------------------------------------------------------------------------
    integer(int32), parameter :: FL_NO_ERROR = 0
    integer(int32), parameter :: FL_INVALID_ARGUMENT_ERROR = 1000
    integer(int32), parameter :: FL_OUT_OF_MEMORY_ERROR = 1001
    integer(int32), parameter :: FL_INDEX_OUT_OF_RANGE_ERROR = 1002
    integer(int32), parameter :: FL_INVALID_ITERATOR_ERROR = 1003

    integer(int32), private, parameter :: DEFAULT_BUFFER_SIZE = 10

! ------------------------------------------------------------------------------
    type container
        !! A container for an unlimited polymorphic variable allowing storage
        !! of any type.
        class(*), private, pointer :: m_item => null()
            ! The pointer to the stored item.
        logical, private :: m_delete = .true.
            ! Set to true to delete m_item when this container object goes out
            ! of scope; else, set to false to persist.

    contains
        procedure, public :: get => c_get
        procedure, public :: delete_on_cleanup => c_get_delete
        final :: c_destroy
        procedure, public :: free => c_cleanup
    end type

! ------------------------------------------------------------------------------
    type, extends(container) :: node
        ! A node in a linked list container.
        type(node), private, pointer :: next => null()
            ! A pointer to the next node in the collection.
        type(node), private, pointer :: previous => null()
            ! A pointer to the previous node in the collection.
    end type

! ------------------------------------------------------------------------------
    type list
        !! Defines a generic, dynamically sizable list.
        type(container), private, allocatable, dimension(:) :: m_list
            ! A collection of container objects.
        integer(int32), private :: m_count = 0
            ! The actual number of items in m_list.
    contains
        procedure, public :: count => list_get_count
        procedure, public :: get_capacity => list_get_capacity
        procedure, public :: set_capacity => list_set_capacity
        procedure, public :: push => list_push
        procedure, public :: pop => list_pop
        procedure, public :: get => list_get
        procedure, public :: set => list_set
        procedure, public :: insert => list_insert
        procedure, public :: remove => list_remove
        procedure, public :: clear => list_clear
    end type

! ------------------------------------------------------------------------------
    type linked_list
        !! Defines a generic, linked-list container.
        integer(int32), private :: m_count = 0
            ! The number of nodes in the container.
        type(node), private, pointer :: m_first => null()
            ! A pointer to the first node in the container.
        type(node), private, pointer :: m_last => null()
            ! A pointer to the last node in the container.
        type(node), private, pointer :: m_current => null()
            ! A pointer to the current node selected by the user.
    contains
        procedure, public :: count => ll_count
        procedure, public :: move_to_first => ll_move_to_first
        procedure, public :: move_to_last => ll_move_to_last
        procedure, public :: next => ll_move_to_next
        procedure, public :: previous => ll_move_to_previous
        procedure, public :: get => ll_get
        procedure, public :: set => ll_set
        procedure, public :: push => ll_push
        procedure, public :: pop => ll_pop
        procedure, public :: clear => ll_clear
        final :: ll_destroy
    end type

contains
! ******************************************************************************
! CONTAINER MEMBERS
! ------------------------------------------------------------------------------
function c_get(this) result(rst)
    !! Gets a pointer to the stored unlimited polymorphic object.
    class(container), intent(in) :: this
        !! The [[container]] object.
    class(*), pointer :: rst
        !! The requested pointer.
    rst => this%m_item
end function

! ------------------------------------------------------------------------------
pure function c_get_delete(this) result(rst)
    !! Gets a value determining if the object should clean up after itself
    !! by freeing the resources allocated by the stored object when this
    !! container goes out of scope.
    class(container), intent(in) :: this
        !! The [[container]] object.
    logical :: rst
        !! Returns true if the stored item will be cleaned up; else,
        !! false denoting the item will persist.
    rst = this%m_delete
end function

! ------------------------------------------------------------------------------
subroutine c_destroy(this)
    !! Frees the resources held by the stored item.
    type(container), intent(inout) :: this
        !! The [[container]] object.
    call this%free()
end subroutine

! ------------------------------------------------------------------------------
subroutine c_cleanup(this)
    !! Frees the resources held by the stored item.
    class(container), intent(inout) :: this
        !! The [[container]] object.
    if (this%delete_on_cleanup() .and. associated(this%m_item))  then
        deallocate(this%m_item)
        nullify(this%m_item)
    else
        nullify(this%m_item)
    end if
end subroutine

! ******************************************************************************
! LIST MEMBERS
! ------------------------------------------------------------------------------
pure function list_get_count(this) result(rst)
    !! Gets the number of items stored in the list.
    class(list), intent(in) :: this
        !! The [[list]] object.
    integer(int32) :: rst
        !! The number of items stored in the list.
    rst = this%m_count
end function

! ------------------------------------------------------------------------------
pure function list_get_capacity(this) result(rst)
    !! Gets the capacity of the list.
    !!
    !! The capacity is the available "space" in the collection for adding
    !! additional items without resizing the internal data store.  This
    !! capacity includes the currently utilized space.  To obtain a count
    !! of the actual number of items stored in the list use the count
    !! routine.
    class(list), intent(in) :: this
        !! The [[list]] object.
    integer(int32) :: rst
        !! The capacity of the list.
    if (allocated(this%m_list)) then
        rst = size(this%m_list)
    else
        rst = 0
    end if
end function

! ------------------------------------------------------------------------------
subroutine list_set_capacity(this, n)
    !! Sets the capacity of the list.
    !!
    !! The capacity is the available "space" in the collection for adding
    !! additional items without resizing the internal data store.  This
    !! capacity includes the currently utilized space.  To obtain a count
    !! of the actual number of items stored in the list use the count
    !! routine.
    class(list), intent(inout) :: this
        !! The [[list]] object.
    integer(int32), intent(in) :: n
        !! The new capacity of the list.  This value must be greater than or 
        !! equal to 1.

    ! Local Variables
    integer(int32) :: m
    type(container), allocatable :: copy(:)
    
    ! Initialization
    m = this%count()

    ! Input Check
    if (n < 1) error stop FL_INVALID_ARGUMENT_ERROR

    ! Quick Return
    if (m == n) return

    ! Process
    if (.not.allocated(this%m_list)) then
        allocate(this%m_list(n))
    end if

    if (n > m) then
        ! Increase capacity
        call move_alloc(this%m_list, copy)
        allocate(this%m_list(n))
        this%m_list(1:m) = copy(1:m)
    else
        ! Decrease capacity
        allocate(copy(n), source = this%m_list(1:n))
        call move_alloc(copy, this%m_list)
        this%m_count = n
    end if
end subroutine

! ------------------------------------------------------------------------------
subroutine list_push(this, x, manage)
    !! Pushes an item onto the back of the list.
    class(list), intent(inout) :: this
        !! The [[list]] object.
    class(*), intent(in), target :: x
        !! The object to store.
    logical, intent(in), optional :: manage
        !! An optional input used to determine if the list should manage 
        !! memory for this object.  If set to true a clone of x is stored and 
        !! the list will handle management of resources held by the clone.  If 
        !! false, the list will not manage resources held by x and x itself 
        !! will be stored.  Notice, in this manner it is possible for x to go
        !! out of scope while the list still persists thereby resulting in a 
        !! potentially undefined behavior.  It is recommended to use the 
        !! default value of true except for very specific and well controlled
        !! edge cases.

    ! Local Variables
    integer(int32) :: index, cap
    
    ! Initialization
    index = this%count() + 1
    cap = this%get_capacity()

    ! Ensure there's space for the item
    if ((cap == 0) .or. &
        (cap - index <= 0)) &
    then
        ! We need more room
        call this%set_capacity(cap + DEFAULT_BUFFER_SIZE)
    end if

    ! Store the item
    this%m_count = index    ! must be before the set routine
    call this%set(index, x, manage)
end subroutine

! ------------------------------------------------------------------------------
subroutine list_pop(this)
    !! Pops the last item off the back of the list.
    class(list), intent(inout) :: this
        !! The [[list]] object.

    ! Process
    integer(int32) :: index
    index = this%count()
    if (index == 0) return

    call this%m_list(index)%free()
    this%m_count = index - 1
end subroutine

! ------------------------------------------------------------------------------
function list_get(this, i) result(rst)
    !! Gets the requested item from the list.
    class(list), intent(in) :: this
        !! The [[list]] object.
    integer(int32), intent(in) :: i
        !! The one-based index of the item to retrieve.
    class(*), pointer :: rst
        !! A pointer to the requested object.
    
    ! Initialization
    nullify(rst)

    ! Input Check
    if (i < 1 .or. i > this%count()) then
        error stop FL_INDEX_OUT_OF_RANGE_ERROR
    end if

    ! Process
    rst => this%m_list(i)%get()
end function

! ------------------------------------------------------------------------------
subroutine list_set(this, i, x, manage)
    !! Sets the specified item into the list.
    class(list), intent(inout) :: this
        !! The [[list]] object.
    integer(int32), intent(in) :: i
        !! The one-based index defining where to put the item.
    class(*), intent(in), target :: x
        !! The object to store.
    logical, intent(in), optional :: manage
        !! An optional input used to determine if the list should manage 
        !! memory for this object.  If set to true a clone of x is stored and 
        !! the list will handle management of resources held by the clone.  If 
        !! false, the list will not manage resources held by x and x itself 
        !! will be stored.  Notice, in this manner it is possible for x to go
        !! out of scope while the list still persists thereby resulting in a 
        !! potentially undefined behavior.  It is recommended to use the 
        !! default value of true except for very specific and well controlled
        !! edge cases.

    ! Local Variables
    logical :: mng
    class(*), pointer :: clone
    
    ! Initialization
    mng = .true.
    if (present(manage)) mng = manage

    ! Input Check
    if (i < 1 .or. i > this%count()) then
        error stop FL_INDEX_OUT_OF_RANGE_ERROR
    end if

    ! Store the item
    call this%m_list(i)%free()
    if (mng) then
        this%m_list(i)%m_delete = .true.
        allocate(clone, source = x)
        this%m_list(i)%m_item => clone
    else
        this%m_list(i)%m_delete = .false.
        this%m_list(i)%m_item => x
    end if
end subroutine

! ------------------------------------------------------------------------------
subroutine list_insert(this, i, x, manage)
    !! Inserts an item into the list.
    class(list), intent(inout) :: this
        !! The [[list]] object.
    integer(int32) :: i
        !! The one-based index defining where to put the item.
    class(*), intent(in) :: x
        !! The object to store.
    logical, intent(in), optional :: manage
        !! An optional input used to determine if the list should manage 
        !! memory for this object.  If set to true a clone of x is stored and 
        !! the list will handle management of resources held by the clone.  If 
        !! false, the list will not manage resources held by x and x itself 
        !! will be stored.  Notice, in this manner it is possible for x to go
        !! out of scope while the list still persists thereby resulting in a 
        !! potentially undefined behavior.  It is recommended to use the 
        !! default value of true except for very specific and well controlled
        !! edge cases.

    ! Local Variables
    logical :: mng
    integer(int32) :: j, n
    
    ! Initialization
    mng = .true.
    if (present(manage)) mng = manage
    n = this%count()

    ! Input Checking
    if (i < 1 .or. i > n + 1) then
        error stop FL_INDEX_OUT_OF_RANGE_ERROR
    end if

    ! Ensure there's capacity
    if (this%get_capacity() <= n + 1) then
        call this%set_capacity(n + DEFAULT_BUFFER_SIZE)
    end if

    ! Shift everything back by one element and insert the item
    this%m_count = this%m_count + 1
    do j = n, i, -1
        call this%set(j + 1, this%get(j), mng)
    end do
    call this%set(i, x, mng)
end subroutine

! ------------------------------------------------------------------------------
subroutine list_remove(this, i)
    !! Removes an item from the list.
    class(list), intent(inout) :: this
        !! The [[list]] object.
    integer(int32) :: i
        !! The one-based index defining which item to remove.

    ! Local Variables
    integer(int32) :: n
    
    ! Initialization
    n = this%count()

    ! Quick Return
    if (n == 0) return

    ! Input Checking
    if (i < 1 .or. i > n) then
        error stop FL_INDEX_OUT_OF_RANGE_ERROR
    end if

    ! Process
    if (n == 1) then
        call this%clear()
    else
        call this%m_list(i)%free()
        this%m_list(i:n-1) = this%m_list(i+1:n)
        this%m_count = this%m_count - 1
    end if
end subroutine

! ------------------------------------------------------------------------------
subroutine list_clear(this)
    !! Clears the entire list.
    class(list), intent(inout) :: this
        !! The [[list]] object.

    ! Local Variables
    integer(int32) :: i, n

    ! Process
    n = this%count()
    do i = 1, n
        call this%m_list(i)%free()
    end do
    this%m_count = 0
end subroutine

! ******************************************************************************
! LINKED_LIST MEMBERS
! ------------------------------------------------------------------------------
pure function ll_count(this) result(rst)
    !! Gets the number of items in the list.
    class(linked_list), intent(in) :: this
        !! The [[linked_list]] object.
    integer(int32) :: rst
    rst = this%m_count
end function

! ------------------------------------------------------------------------------
subroutine ll_move_to_first(this)
    !! Moves the current position in the list to the first item.
    class(linked_list), intent(inout) :: this
        !! The [[linked_list]] object.
    if (associated(this%m_first)) then
        this%m_current => this%m_first
    end if
end subroutine

! ------------------------------------------------------------------------------
subroutine ll_move_to_last(this)
    !! Moves the current position in the list to the last item.
    class(linked_list), intent(inout) :: this
        !! The [[linked_list]] object.
    if (associated(this%m_last)) then
        this%m_current => this%m_last
    end if
end subroutine

! ------------------------------------------------------------------------------
function ll_move_to_next(this) result(rst)
    !! Moves to the next item in the list.
    class(linked_list), intent(inout) :: this
        !! The [[linked_list]] object.
    logical :: rst
        !! Returns true if the move was successful; else, returns false.
        !! Typically a false value indicates the end of the list; however, a
        !! false value can be encountered if the list is emtpy.

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
function ll_move_to_previous(this) result(rst)
    !! Moves to the previous item in the list.
    class(linked_list), intent(inout) :: this
        !! The [[linked_list]] object.
    logical :: rst
        !! Returns true if the move was successful; else, returns false.
        !! Typically a false value indicates the end of the list; however, a
        !! false value can be encountered if the list is emtpy.

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
function ll_get(this) result(rst)
    !! Gets the current item.
    class(linked_list), intent(in) :: this
        !! The [[linked_list]] object.
    class(*), pointer :: rst
        !! The currently referenced item from the list.  This may be 
        !! null if the list is empty.

    ! Process
    if (associated(this%m_current)) then
        rst => this%m_current%get()
    else
        rst => null()
    end if
end function

! ------------------------------------------------------------------------------
subroutine ll_set(this, x, manage)
    !! Replaces the current item in the list with the supplied item.
    class(linked_list), intent(inout) :: this
        !! The [[linked_list]] object.
    class(*), intent(in), target :: x
        !! The object to store.
    logical, intent(in), optional :: manage
        !! An optional input used to determine if the list should manage 
        !! memory for this object.  If set to true a clone of x is stored and 
        !! the list will handle management of resources held by the clone.  If 
        !! false, the list will not manage resources held by x and x itself 
        !! will be stored.  Notice, in this manner it is possible for x to go
        !! out of scope while the list still persists thereby resulting in a 
        !! potentially undefined behavior.  It is recommended to use the 
        !! default value of true except for very specific and well controlled
        !! edge cases.

    ! Local Variables
    logical :: mng
    class(*), pointer :: clone
    
    ! Initialization
    mng = .true.
    if (present(manage)) mng = manage

    ! Ensure we're at a valid node
    if (.not.associated(this%m_current)) then
        error stop FL_INVALID_ITERATOR_ERROR
    end if

    ! Process
    call this%m_current%free()
    if (mng) then
        allocate(clone, source = x)
        this%m_current%m_item => clone
    else
        this%m_current%m_item => x
        this%m_current%m_delete = .false.
    end if
end subroutine

! ------------------------------------------------------------------------------
subroutine ll_push(this, x, manage)
    !! Pushes an item onto the end of the list.
    class(linked_list), intent(inout) :: this
        !! The [[linked_list]] object.
    class(*), intent(in), target :: x
        !! The object to store.
    logical, intent(in), optional :: manage
        !! An optional input used to determine if the list should manage 
        !! memory for this object.  If set to true a clone of x is stored and 
        !! the list will handle management of resources held by the clone.  If 
        !! false, the list will not manage resources held by x and x itself 
        !! will be stored.  Notice, in this manner it is possible for x to go
        !! out of scope while the list still persists thereby resulting in a 
        !! potentially undefined behavior.  It is recommended to use the 
        !! default value of true except for very specific and well controlled
        !! edge cases.

    ! Local Variables
    integer(int32) :: n
    logical :: mng
    type(node), pointer :: nd, temp, newnode
    class(*), pointer :: clone
    
    ! Initialization
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
        
        allocate(this%m_first)

        this%m_first%next => null()
        this%m_first%previous => null()
        nd => this%m_first

        ! As this is the first, it's also the last and the current
        this%m_last => this%m_first
        this%m_current => this%m_first
    else
        ! We have a new last item
        allocate(newnode)

        temp => this%m_last
        temp%next => newnode
        this%m_last => newnode
        this%m_last%previous => temp
        this%m_last%next => null()
        nd => this%m_last
    end if

    ! Store the item
    if (mng) then
        allocate(clone, source = x)
        nd%m_item => clone
    else
        nd%m_item => x
        nd%m_delete = .false.
    end if

    ! Index the count
    this%m_count = this%m_count + 1
end subroutine

! ------------------------------------------------------------------------------
subroutine ll_pop(this)
    !! Pops an item off the back of the list.
    class(linked_list), intent(inout) :: this
        !! The [[linked_list]] object.

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
subroutine ll_clear(this)
    !! Clears the entire list.
    class(linked_list), intent(inout) :: this
        !! The [[linked_list]] object.

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
subroutine ll_destroy(this)
    !! Finalizer for the linked_list type responsible for clean-up duties
    !! when the list goes out of scope.
    type(linked_list), intent(inout) :: this
        !! The [[linked_list]] object.
    call this%clear()
end subroutine

! ------------------------------------------------------------------------------
end module