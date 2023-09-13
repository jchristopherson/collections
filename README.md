# collections
A set of types supporting collections in Fortran.  Currently, the library contains a generic, dynamically sizable list and a generic linked-list type.

## Description
The collections library contains generic and dynamically sizeable collection types using unlimited polymorphic types with an object-oriented design.

## Build
[CMake](https://cmake.org/) is the preferred build system for this library.  See [Running CMake](https://cmake.org/runningcmake/) for instructions on how to build using CMake.

[FPM](https://github.com/fortran-lang/fpm) can also be used to build this library using the provided fpm.toml.
```txt
fpm build
```
The COLLECTIONS library can be used within your FPM project by adding the following to your fpm.toml file.
```
[dependencies]
collections = { git = "https://github.com/jchristopherson/collections/git" }
```

## Dependencies
This library depends upon the [FERROR](https://github.com/jchristopherson/ferror) library.

## Documentation
The documentation can be found [here](https://jchristopherson.github.io/collections/).

## Examples
The collections library provides a generic, dynamically sizeable type referred to simply as list.  A simple example illustrating basic usage of the list type is as follows.
```fortran
program list_example
    use collections
    use iso_fortran_env
    implicit none

    ! Variables
    integer(int32), parameter :: n = 10
    integer(int32) :: i
    type(list) :: x
    class(*), pointer :: ptr

    ! Create a list
    do i = 1, n
        call x%push(2 * i)
    end do

    ! Print it out to the command line
    print '(A)', "***** Original List *****"
    do i = 1, n
        ptr => x%get(i)
        
        ! The list uses unlimited polymorphic types; therefore, we need to
        ! use the select type construct.
        select type (ptr)
        type is (integer(int32))
            print *, ptr
        end select
    end do

    ! Insert the integer value of 100 into the 5th slot in the list
    call x%insert(5, 100)

    ! Print it out again to illustrate the change
    print '(A)', new_line('a') // "***** After Insertion *****"
    do i = 1, x%count()
        ptr => x%get(i)
        select type (ptr)
        type is (integer(int32))
            print *, ptr
        end select
    end do
end program
```
This program generates the following output.
```
***** Original List *****
           2
           4
           6
           8
          10
          12
          14
          16
          18
          20

***** After Insertion *****
           2
           4
           6
           8
         100
          10
          12
          14
          16
          18
          20
```
The collections library also provides a generic linked-list type.  A simple example illustrating basic usage of the linked-list type is as follows.
```fortran
program linked_list_example
    use collections
    use iso_fortran_env
    implicit none

    ! Variables
    integer(int32), parameter :: n = 10
    integer(int32) :: i
    logical :: check
    type(linked_list) :: x
    class(*), pointer :: ptr

    ! Create a list
    do i = 1, n
        call x%push(i)
    end do

    ! Print it out
    print '(A)', "***** Original List *****"
    check = associated(x%get())
    do while (check)
        ptr => x%get()

        ! The list uses unlimited polymorphic types; therefore, we need to
        ! use the select type construct.
        select type (ptr)
        type is (integer(int32))
            print *, ptr
        end select

        ! Move to the next item
        check = x%next()
    end do

    ! Print out the item at the current iterator position
    print '(A)', new_line('a') // "***** Current Iterator Position *****"
    ptr => x%get()
    select type(ptr)
    type is (integer(int32))
        print *, ptr
    end select

    ! Move to the beginning of the collection
    print '(A)', new_line('a') // "***** Beginning *****"
    call x%move_to_first()
    ptr => x%get()
    select type (ptr)
    type is (integer(int32))
        print *, ptr
    end select
end program
```
This program generates the following output.
```
***** Original List *****
           1
           2
           3
           4
           5
           6
           7
           8
           9
          10

***** Current Iterator Position *****
          10

***** Beginning *****
           1
```