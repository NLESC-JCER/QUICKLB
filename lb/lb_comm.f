      module LOADBALANCER_COMMUNICATION
      use iso_fortran_env
      type t_lb_comm_entry
        integer(int32)                           :: proc
        integer(int32)                           :: length
        integer(int32)                           :: data_buf_start
        integer(int32)                           :: data_buf_end
!---- These entries are for communication the other way around
        integer(int32)                           :: result_buf_start
        integer(int32)                           :: result_buf_end
      end type        


      type t_lb_comm
        integer(int32)                           :: exports_length
        integer(int32)                           :: imports_length
        type(t_lb_comm_entry), allocatable       :: exports(:)
        type(t_lb_comm_entry), allocatable       :: imports(:)
      end type

      type BYTEsliceptr
        BYTE, pointer,contiguous, dimension(:) :: p
      end type

      end module LOADBALANCER_COMMUNICATION
