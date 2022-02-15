      subroutine INIT(communicator)
      ! Initialize MPI if its not yet initialized
      ! and return the global communicator 
        use iso_c_binding
        use iso_fortran_env
        use mpi_f08
        implicit none
        integer(c_int32_t), intent(out)        :: communicator
        integer                                :: err
        logical                                :: initialized

        call MPI_Initialized(initialized, err)
        if (err /= MPI_success) then
          error stop "There is something wrong with the MPI"
        end if
        if (.not. initialized) then
          call MPI_Init(err)
          if (err /= MPI_success) then
            error stop "There is something wrong with the MPI"
          end if
        end if


        communicator = MPI_comm_world%MPI_val
      end subroutine


      subroutine CREATE ( loadbalancer_
     &                  , data_block_bytes
     &                  , result_block_bytes
     &                  , nblocks
     &                  , block_npoints
     &                  , communicator_)
        use iso_c_binding 
        use iso_fortran_env
        use mpi_f08
        use LOADBALANCER, only: t_loadbalancer
        implicit none
        integer(c_intptr_t),intent(out)        :: loadbalancer_
        integer(c_int32_t) ,intent(in)         :: data_block_bytes
        integer(c_int32_t) ,intent(in)         :: result_block_bytes
        integer(c_int32_t) ,intent(in)         :: nblocks
        integer(c_int32_t) ,intent(in)         :: block_npoints
        integer(c_int32_t) ,intent(in)         :: communicator_

        type(t_loadbalancer), pointer          :: loadbalancer
        type(MPI_comm)                         :: communicator
        communicator%MPI_val = communicator_


        allocate(loadbalancer) 
        call loadbalancer% CREATE( data_block_bytes
     &                , result_block_bytes
     &                , nblocks
     &                , block_npoints
     &                , communicator)
        loadbalancer_ = transfer(c_loc(loadbalancer),loadbalancer_)
      end subroutine

      subroutine SET_IMPORT_EXPORT_ROUTINES( success
     &                                     , loadbalancer_
     &                                     , export_data) 
        use iso_c_binding
        use iso_fortran_env
        use LOADBALANCER, only: t_loadbalancer
cf2py   use __user__routines, only: export_data => serialize
        implicit none
        external                               :: export_data
        integer(c_intptr_t), intent(in)        :: loadbalancer_
        logical            , intent(out)       :: success

        type(t_loadbalancer), pointer          :: loadbalancer
        type(c_ptr)                            :: loadbalancer_c_ptr
        integer*1, allocatable                 :: buffer(:)
        integer(c_int64_t) :: id
        integer(c_int32_t) :: buffer_size

        loadbalancer_c_ptr = transfer(loadbalancer_, loadbalancer_c_ptr)
        call c_f_pointer(loadbalancer_c_ptr, loadbalancer)  
        id = 4
        buffer_size = 4
        allocate(buffer(buffer_size))
        buffer = 42
        call export_data(buffer, id, buffer_size)
        write(*,*) allocated(buffer)
        write(*,*) size(buffer),buffer
        success = .true.

      end subroutine

