!---- Written for the loadbalancer, but could be used throughout the code
!---- Perhaps it should be merged with inca_error
      module LOADBALANCER_DEBUG
        use INCA_ERROR, only : NOTBREMSE
!----
!     Usage:
!       Both production and debug mode:
!         call ASSERT(x)
!----


      interface ASSERT
        module procedure ASSERT_1
        module procedure ASSERT_2
        module procedure ASSERT_3
        module procedure ASSERT_4
      end interface

      contains

      subroutine ASSERT_1(x)
        logical :: x
        if( .not.x )then
          call NOTBREMSE("Assertion Error")
        end if
      end subroutine ASSERT_1

      subroutine ASSERT_2(x, message)
        logical :: x
        character ( len = * ) :: message
        if( .not.x )then
          call NOTBREMSE("Assertion Error: "//message)
        end if
      end subroutine ASSERT_2

      subroutine ASSERT_3 (x, filename, linenumber)
        logical :: x
        integer :: linenumber
        character ( len = * ) :: filename
        if( .not.x )then
          call NOTBREMSE(filename, linenumber, "Assertion Error")
        end if
      end subroutine ASSERT_3

      subroutine ASSERT_4 (x, filename, linenumber, message)
        logical :: x
        integer :: linenumber
        character ( len = * ) :: filename
        character ( len = * ) :: message
        if( .not.x )then
          call NOTBREMSE(filename, linenumber,
     &        "Assertion Error: "//message)
        end if
      end subroutine ASSERT_4

      end module LOADBALANCER_DEBUG
