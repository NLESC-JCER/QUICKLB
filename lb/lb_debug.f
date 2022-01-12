      module LOADBALANCER_DEBUG
!----
!     Usage:
!       Both production and debug mode:
!         call ASSERT(x)
!----


      interface ASSERT
        module procedure ASSERT_1
        module procedure ASSERT_2
      end interface

      contains

      subroutine ASSERT_1(x)
        logical :: x
        if( .not.x )then
          error stop "Assertion Error"
        end if
      end subroutine ASSERT_1

      subroutine ASSERT_2(x, message)
        logical :: x
        character ( len = * ) :: message
        if( .not.x )then
          error stop "Assertion Error: "//message
        end if
      end subroutine ASSERT_2

      end module LOADBALANCER_DEBUG
