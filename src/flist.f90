!> A module containing various collections using Fortran's unlimited polymorphic
!! functionallity.
module flist
    use iso_fortran_env
    use ferror
    implicit none
    private
    public :: container
    public :: node
    public :: list

! ------------------------------------------------------------------------------
    integer(int32), parameter :: FL_NO_ERROR = 0
    integer(int32), parameter :: FL_INVALID_ARGUMENT_ERROR = 1000
    integer(int32), parameter :: FL_OUT_OF_MEMORY_ERROR = 1001
    integer(int32), parameter :: FL_INDEX_OUT_OF_RANGE_ERROR = 1002

! ------------------------------------------------------------------------------
    !> A container type allowing storage of any Fortran type.
    type container
    private
        ! A pointer to an unlimited polymorphic variable allowing storage of
        ! any type.
        class(*), pointer :: m_item => null()
        ! Set to true to delete @ref item when this container object goes out
        ! of scope; else, set to false to persist.
        logical :: m_delete = .true.

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
    type node
        !> A pointer to an unlimited polymorphic variable allowing storage of
        !! any type.
        class(*), pointer :: item => null()
        !> A pointer to the next node in the collection.
        type(node), pointer :: next => null()
        !> A pointer to the previous node in the collection.
        type(node), pointer :: previous => null()
        !> Set to true to delete @ref item when this node object goes out
        !! of scope; else, set to false to persist.
        logical :: delete = .true.
    end type

    ! flist_container.f90
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
        !! @param[out] err An optional output that can be used to track the
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
        !! @param[out] err An optional output that can be used to track the
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
        !! @param[out] err An optional output that can be used to track the
        !!  error status of the routine.  Possible error codes are as follows.
        !!  - FL_NO_ERROR: No error.
        !!  - FL_INDEX_OUT_OF_RANGE_ERROR: The index parameter @p i is outside 
        !!      the bounds of the list.
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
        !! @param[out] err An optional output that can be used to track the
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
        !! @param[out] err An optional output that can be used to track the
        !!  error status of the routine.  Possible error codes are as follows.
        !!  - FL_NO_ERROR: No error.
        !!  - FL_INDEX_OUT_OF_RANGE_ERROR: The index parameter @p i is outside 
        !!      the bounds of the list.
        !!  - FL_OUT_OF_MEMORY_ERROR: Memory allocation error.
        procedure, public :: insert => list_insert
    end type

    ! flist_list.f90
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
    end interface
end module