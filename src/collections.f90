!> A module containing various collections using Fortran's unlimited polymorphic
!! functionallity.
module collections
    use iso_fortran_env
    use ferror
    implicit none
    private
    public :: container
    public :: node
    public :: list
    public :: linked_list

! ------------------------------------------------------------------------------
    integer(int32), parameter :: FL_NO_ERROR = 0
    integer(int32), parameter :: FL_INVALID_ARGUMENT_ERROR = 1000
    integer(int32), parameter :: FL_OUT_OF_MEMORY_ERROR = 1001
    integer(int32), parameter :: FL_INDEX_OUT_OF_RANGE_ERROR = 1002
    integer(int32), parameter :: FL_INVALID_ITERATOR_ERROR = 1003

! ------------------------------------------------------------------------------
    !> A container type allowing storage of any Fortran type.
    type container
        ! A pointer to an unlimited polymorphic variable allowing storage of
        ! any type.
        class(*), private, pointer :: m_item => null()
        ! Set to true to delete @ref item when this container object goes out
        ! of scope; else, set to false to persist.
        logical, private :: m_delete = .true.

    contains
        !> Gets a pointer to the stored unlimited polymorphic object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(*) pointer function get(class(container) this)
        !! @endcode
        !!
        !! @param[in] The container object.
        !! @return The requested pointer.
        procedure, public :: get => c_get
        !> Gets a value determining if the object should clean up after itself
        !! by freeing the resources allocated by the stored object when this
        !! container goes out of scope.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical function delete_on_cleanup(class(container) this)
        !! @endcode
        !!
        !! @param[in] The container object.
        !! @return Returns true if the stored item will be cleaned up; else,
        !! false denoting the item will persist.
        procedure, public :: delete_on_cleanup => c_get_delete
        !> Finalizer routine that performs the cleanup of the container.
        final :: c_destroy
        !> Frees the resources held by the stored item.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine free(class(container) this)
        !! @endcode
        !!
        !! @param[in,out] this The container object.
        procedure, public :: free => c_cleanup
    end type

    !> A node in a linked list container.
    type, extends(container) :: node
        !> A pointer to the next node in the collection.
        type(node), private, pointer :: next => null()
        !> A pointer to the previous node in the collection.
        type(node), private, pointer :: previous => null()
    end type

    ! collections_container.f90
    interface
        module function c_get(this) result(rst)
            class(container), intent(in) :: this
            class(*), pointer :: rst
        end function

        pure module function c_get_delete(this) result(rst)
            class(container), intent(in) :: this
            logical :: rst
        end function

        module subroutine c_destroy(this)
            type(container), intent(inout) :: this
        end subroutine

        module subroutine c_cleanup(this)
            class(container), intent(inout) :: this
        end subroutine
    end interface

    !> Defines a generic, dynamically sizable list.
    !!
    !! @par Example
    !! The following example illustrates basic usage of the list.
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
    !! end program
    !! @endcode
    !! The above program generates the following output.
    !! @code{.txt}
    !! ***** Original List *****
    !!     2
    !!     4
    !!     6
    !!     8
    !!    10
    !!    12
    !!    14
    !!    16
    !!    18
    !!    20
    !!
    !! ***** After Insertion *****
    !!     2
    !!     4
    !!     6
    !!     8
    !!   100
    !!    10
    !!    12
    !!    14
    !!    16
    !!    18
    !!    20
    !! @endcode
    type list
        ! A collection of container objects.
        type(container), private, allocatable, dimension(:) :: m_list
        ! The actual number of items in m_list.
        integer(int32), private :: m_count = 0
    contains
        !> Gets the number of items stored in the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) get_count(class(list) this)
        !! @endcode
        !!
        !! @param[in] this The list object.
        !! @return The number of items stored in the list.
        procedure, public :: count => list_get_count
        !> Gets the capacity of the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) get_capacity(class(list) this)
        !! @endcode
        !!
        !! @param[in] this The list object.
        !! @return The capacity of the list.
        !!
        !! @remarks
        !! The capacity is the available "space" in the collection for adding
        !! additional items without resizing the internal data store.  This
        !! capacity includes the currently utilized space.  To obtain a count
        !! of the actual number of items stored in the list use the @ref count
        !! routine.
        procedure, public :: get_capacity => list_get_capacity
        !> Sets the capacity of the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_capacity(class(list) this, integer(int32) n, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The list object.
        !! @param[in] n The new capacity of the list.  This value must be 
        !!  greater than or equal to 1.
        !! @param[in,out] err An optional output that can be used to track the
        !!  error status of the routine.  Possible error codes are as follows.
        !!  - FL_NO_ERROR: No error.
        !!  - FL_INVALID_ARGUMENT_ERROR: Invalid value for @p n.
        !!  - FL_OUT_OF_MEMORY_ERROR: Memory allocation error.
        procedure, public :: set_capacity => list_set_capacity
        !> Pushes an item onto the back of the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine push(class(list) this, class(*) x, optional logical manage, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The list object.
        !! @param[in] x The object to store.
        !! @param[in] manage An optional input used to determine if the list 
        !!  should manage memory for this object.  If set to true a clone of
        !!  @p x is stored and the list will handle management of resources
        !!  held by the clone.  If false, the list will not manage resources
        !!  held by x and x itself will be stored.  Notice, in this manner it 
        !!  is possible for x to go out of scope while the list still persists
        !!  thereby resulting in a potentially undefined behavior.  It is 
        !!  recommended to use the default value of true except for very 
        !!  specific and well controlled edge cases.
        !! @param[in,out] err An optional output that can be used to track the
        !!  error status of the routine.  Possible error codes are as follows.
        !!  - FL_NO_ERROR: No error.
        !!  - otherwise: Memory allocation error.
        procedure, public :: push => list_push
        !> Pops the last item off the back of the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine pop(class(list) this)
        !! @endcode
        !!
        !! @param[in,out] this The list object.
        procedure, public :: pop => list_pop
        !> Gets the requested item from the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(*) pointer function get(class(list) this, integer(int32) i, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The list object.
        !! @param[in] i The one-based index of the item to retrieve.
        !! @param[in,out] err An optional output that can be used to track the
        !!  error status of the routine.  Possible error codes are as follows.
        !!  - FL_NO_ERROR: No error.
        !!  - FL_INDEX_OUT_OF_RANGE_ERROR: The index parameter @p i is outside 
        !!      the bounds of the list.
        !!
        !! @return A pointer to the requested object.
        procedure, public :: get => list_get
        !> Sets the specified item into the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set(class(list) this, integer(int32) i, class(*) x, optional logical manage, optional class(errors) err))
        !! @endcode
        !!
        !! @param[in,out] this The list object.
        !! @param[in] i The one-based index defining where to put the item.
        !! @param[in] x The object to store.
        !! @param[in] manage An optional input used to determine if the list 
        !!  should manage memory for this object.  If set to true a clone of
        !!  @p x is stored and the list will handle management of resources
        !!  held by the clone.  If false, the list will not manage resources
        !!  held by x and x itself will be stored.  Notice, in this manner it 
        !!  is possible for x to go out of scope while the list still persists
        !!  thereby resulting in a potentially undefined behavior.  It is 
        !!  recommended to use the default value of true except for very 
        !!  specific and well controlled edge cases.
        !! @param[in,out] err An optional output that can be used to track the
        !!  error status of the routine.  Possible error codes are as follows.
        !!  - FL_NO_ERROR: No error.
        !!  - FL_INDEX_OUT_OF_RANGE_ERROR: The index parameter @p i is outside 
        !!      the bounds of the list.
        !!  - FL_OUT_OF_MEMORY_ERROR: Memory allocation error.
        procedure, public :: set => list_set
        !> Inserts an item into the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine insert(class(list) this, integer(int32) i, class(*) x, optional logical manage, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The list object.
        !! @param[in] i The one-based index defining where to put the item.
        !! @param[in] x The object to store.
        !! @param[in] manage An optional input used to determine if the list 
        !!  should manage memory for this object.  If set to true a clone of
        !!  @p x is stored and the list will handle management of resources
        !!  held by the clone.  If false, the list will not manage resources
        !!  held by x and x itself will be stored.  Notice, in this manner it 
        !!  is possible for x to go out of scope while the list still persists
        !!  thereby resulting in a potentially undefined behavior.  It is 
        !!  recommended to use the default value of true except for very 
        !!  specific and well controlled edge cases.
        !! @param[in,out] err An optional output that can be used to track the
        !!  error status of the routine.  Possible error codes are as follows.
        !!  - FL_NO_ERROR: No error.
        !!  - FL_INDEX_OUT_OF_RANGE_ERROR: The index parameter @p i is outside 
        !!      the bounds of the list.
        !!  - FL_OUT_OF_MEMORY_ERROR: Memory allocation error.
        procedure, public :: insert => list_insert
        !> Removes an item from the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine remove(class(list) this, integer(int32) i, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The list object.
        !! @param[in] i The one-based index defining which item to remove.
        !! @param[in,out] err An optional output that can be used to track the
        !!  error status of the routine.  Possible error codes are as follows.
        !!  - FL_NO_ERROR: No error.
        !!  - FL_INDEX_OUT_OF_RANGE_ERROR: The index parameter @p i is outside 
        !!      the bounds of the list.
        procedure, public :: remove => list_remove
        !> Clears the entire list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine clear(class(list) this)
        !! @endcode
        !!
        !! @param[in,out] this The list object.
        procedure, public :: clear => list_clear
    end type

    ! collections_list.f90
    interface
        pure module function list_get_count(this) result(rst)
            class(list), intent(in) :: this
            integer(int32) :: rst
        end function

        pure module function list_get_capacity(this) result(rst)
            class(list), intent(in) :: this
            integer(int32) :: rst
        end function

        module subroutine list_set_capacity(this, n, err)
            class(list), intent(inout) :: this
            integer(int32), intent(in) :: n
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine list_push(this, x, manage, err)
            class(list), intent(inout) :: this
            class(*), intent(in), target :: x
            logical, intent(in), optional :: manage
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine list_pop(this)
            class(list), intent(inout) :: this
        end subroutine

        module function list_get(this, i, err) result(rst)
            class(list), intent(in) :: this
            integer(int32), intent(in) :: i
            class(errors), intent(inout), optional, target :: err
            class(*), pointer :: rst
        end function

        module subroutine list_set(this, i, x, manage, err)
            class(list), intent(inout) :: this
            integer(int32), intent(in) :: i
            class(*), intent(in), target :: x
            logical, intent(in), optional :: manage
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine list_insert(this, i, x, manage, err)
            class(list), intent(inout) :: this
            integer(int32) :: i
            class(*), intent(in) :: x
            logical, intent(in), optional :: manage
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine list_remove(this, i, err)
            class(list), intent(inout) :: this
            integer(int32) :: i
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine list_clear(this)
            class(list), intent(inout) :: this
        end subroutine
    end interface

! ------------------------------------------------------------------------------
    !> Defines a generic, linked-list container.
    type linked_list
        ! The number of nodes in the container.
        integer(int32), private :: m_count = 0
        ! A pointer to the first node in the container.
        type(node), private, pointer :: m_first => null()
        ! A pointer to the last node in the container.
        type(node), private, pointer :: m_last => null()
        ! A pointer to the current node selected by the user.
        type(node), private, pointer :: m_current => null()
    contains
        !> Gets the number of items in the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function count(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in] this The linked_list object.
        !! @return The number of items in the list.
        procedure, public :: count => ll_count
        !> Moves the current position in the list to the first item.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine move_to_first(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        procedure, public :: move_to_first => ll_move_to_first
        !> Moves the current position in the list to the last item.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine move_to_last(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        procedure, public :: move_to_last => ll_move_to_last
        !> Moves to the next item in the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical function next(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        !! @return Returns true if the move was successful; else, returns false.
        !! Typically a false value indicates the end of the list; however, a
        !! false value can be encountered if the list is emtpy.
        procedure, public :: next => ll_move_to_next
        !> Moves to the previous item in the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical function previous(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        !! @return Returns true if the move was successful; else, returns false.
        !! Typically a false value indicates the end of the list; however, a
        !! false value can be encountered if the list is emtpy.
        procedure, public :: previous => ll_move_to_previous
        !> Gets the current item.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(*) pointer function get(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in] this The linked_list object.
        !! @return The currently referenced item from the list.  This may be 
        !!  null if the list is empty.
        procedure, public :: get => ll_get
        !> Replaces the current item in the list with the supplied item.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set(class(linked_list) this, class(*) x, optional logical manage, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        !! @param[in] x The object to store.
        !! @param[in] manage An optional input used to determine if the list 
        !!  should manage memory for this object.  If set to true a clone of
        !!  @p x is stored and the list will handle management of resources
        !!  held by the clone.  If false, the list will not manage resources
        !!  held by x and x itself will be stored.  Notice, in this manner it 
        !!  is possible for x to go out of scope while the list still persists
        !!  thereby resulting in a potentially undefined behavior.  It is 
        !!  recommended to use the default value of true except for very 
        !!  specific and well controlled edge cases.
        !! @param[in,out] err An optional output that can be used to track the
        !!  error status of the routine.  Possible error codes are as follows.
        !!  - FL_NO_ERROR: No error.
        !!  - FL_OUT_OF_MEMORY_ERROR: Memory allocation error.
        procedure, public :: set => ll_set
        !> Pushes an item onto the end of the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine push(class(linked_list) this, class(*) x, optional logical manage, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        !! @param[in] x The object to store.
        !! @param[in] manage An optional input used to determine if the list 
        !!  should manage memory for this object.  If set to true a clone of
        !!  @p x is stored and the list will handle management of resources
        !!  held by the clone.  If false, the list will not manage resources
        !!  held by x and x itself will be stored.  Notice, in this manner it 
        !!  is possible for x to go out of scope while the list still persists
        !!  thereby resulting in a potentially undefined behavior.  It is 
        !!  recommended to use the default value of true except for very 
        !!  specific and well controlled edge cases.
        !! @param[in,out] err An optional output that can be used to track the
        !!  error status of the routine.  Possible error codes are as follows.
        !!  - FL_NO_ERROR: No error.
        !!  - FL_OUT_OF_MEMORY_ERROR: Memory allocation error.
        procedure, public :: push => ll_push
        !> Pops an item off the back of the list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine pop(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        procedure, public :: pop => ll_pop
        !> Clears the entire list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine clear(class(linked_list) this)
        !! @endcode
        !!
        !! @param[in,out] this The linked_list object.
        procedure, public :: clear => ll_clear
        !> Finalizer for the linked_list type responsible for clean-up duties
        !! when the list goes out of scope.
        final :: ll_destroy
    end type

    ! collections_linked_list.f90
    interface
        pure module function ll_count(this) result(rst)
            class(linked_list), intent(in) :: this
            integer(int32) :: rst
        end function

        module subroutine ll_move_to_first(this)
            class(linked_list), intent(inout) :: this
        end subroutine

        module subroutine ll_move_to_last(this)
            class(linked_list), intent(inout) :: this
        end subroutine

        module function ll_move_to_next(this) result(rst)
            class(linked_list), intent(inout) :: this
            logical :: rst
        end function

        module function ll_move_to_previous(this) result(rst)
            class(linked_list), intent(inout) :: this
            logical :: rst
        end function

        module function ll_get(this) result(rst)
            class(linked_list), intent(in) :: this
            class(*), pointer :: rst
        end function

        module subroutine ll_set(this, x, manage, err)
            class(linked_list), intent(inout) :: this
            class(*), intent(in), target :: x
            logical, intent(in), optional :: manage
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine ll_push(this, x, manage, err)
            class(linked_list), intent(inout) :: this
            class(*), intent(in), target :: x
            logical, intent(in), optional :: manage
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine ll_pop(this)
            class(linked_list), intent(inout) :: this
        end subroutine

        module subroutine ll_clear(this)
            class(linked_list), intent(inout) :: this
        end subroutine

        module subroutine ll_destroy(this)
            type(linked_list), intent(inout) :: this
        end subroutine
    end interface
end module