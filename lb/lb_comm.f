      module LOADBALANCER_COMMUNICATION
      type t_lb_comm_entry
        integer                                  :: proc
        integer                                  :: length
        integer                                  :: data_buf_start
        integer                                  :: data_buf_end
!---- These entries are for communication the other way around
        integer                                  :: result_buf_start
        integer                                  :: result_buf_end
      end type


      type t_lb_comm
        integer                                  :: exports_length
        integer                                  :: imports_length
        type(t_lb_comm_entry), allocatable       :: exports(:)
        type(t_lb_comm_entry), allocatable       :: imports(:)
      end type

      type BYTEsliceptr
        BYTE, pointer,contiguous, dimension(:) :: p
      end type

      end module LOADBALANCER_COMMUNICATION
