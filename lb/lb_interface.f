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
        integer(c_int32_t) ,intent(in)         :: communicator_

        type(t_loadbalancer), pointer          :: loadbalancer
        type(MPI_comm)                         :: communicator
        communicator%MPI_val = communicator_


        allocate(loadbalancer) 
        call loadbalancer% CREATE( data_block_bytes
     &                , result_block_bytes
     &                , nblocks
     &                , communicator)
        loadbalancer_ = transfer(c_loc(loadbalancer),loadbalancer_)
      end subroutine

      subroutine SET_PARTITION_ALGORITHM( loadbalancer_, name
     &                                  , max_rel_li, max_abs_li
     &                                  , max_it)     
        use iso_c_binding
        use iso_fortran_env
        use LOADBALANCER, only: t_loadbalancer
        use LOADBALANCER_SET_PARTITION, only: LB_SET_PARTITION_ALGORITHM
     &                                        => SET_PARTITION_ALGORITHM
        implicit none
        integer(c_intptr_t), intent(in)           :: loadbalancer_
        type(c_ptr)                               :: loadbalancer_c_ptr
        type(t_loadbalancer), pointer             :: loadbalancer
        character(len=20),  intent(in)            :: name
        real(c_float),      intent(in)            :: max_rel_li
        real(c_float),      intent(in)            :: max_abs_li
        integer(c_int32_t), intent(in)            :: max_it
        loadbalancer_c_ptr = transfer(loadbalancer_, loadbalancer_c_ptr)
        call c_f_pointer(loadbalancer_c_ptr, loadbalancer)  


        call LB_SET_PARTITION_ALGORITHM( loadbalancer, name, max_rel_li
     &                                 , max_abs_li, max_it )

      end subroutine

      subroutine PARTITION( loadbalancer_ )
        use iso_c_binding
        use iso_fortran_env
        use LOADBALANCER, only: t_loadbalancer
        implicit none
        integer(c_intptr_t), intent(in)           :: loadbalancer_
        type(c_ptr)                               :: loadbalancer_c_ptr
        type(t_loadbalancer), pointer             :: loadbalancer
        loadbalancer_c_ptr = transfer(loadbalancer_, loadbalancer_c_ptr)
        call c_f_pointer(loadbalancer_c_ptr, loadbalancer)  
        call loadbalancer% partition()
      end subroutine

      subroutine SET_WEIGHTS( loadbalancer_, weights)
        use iso_c_binding
        use iso_fortran_env
        use LOADBALANCER, only: t_loadbalancer
        implicit none
        integer(c_intptr_t), intent(in)           :: loadbalancer_
        type(c_ptr)                               :: loadbalancer_c_ptr
        type(t_loadbalancer), pointer             :: loadbalancer
        real(c_float), intent(in)                 :: weights(:)
        loadbalancer_c_ptr = transfer(loadbalancer_, loadbalancer_c_ptr)
        call c_f_pointer(loadbalancer_c_ptr, loadbalancer)  
!---- Allocate Arrays for loadbalancing
        if ( size(weights) /= loadbalancer% nblocks) then
          write(*,*) "error info: nblocks=", loadbalancer% nblocks
          error stop "Weights array must be exactly nblocks long"
        end if
        loadbalancer% weights = weights
       

      end subroutine

      subroutine INFO( loadbalancer_)
        use iso_c_binding
        use iso_fortran_env
        use LOADBALANCER, only: t_loadbalancer
        use LOADBALANCER_OUTPUT, only: USED_LOADBALANCING_INFO
        implicit none
        integer(c_intptr_t), intent(in)           :: loadbalancer_
        type(c_ptr)                               :: loadbalancer_c_ptr
        type(t_loadbalancer), pointer             :: loadbalancer
        loadbalancer_c_ptr = transfer(loadbalancer_, loadbalancer_c_ptr)
        call c_f_pointer(loadbalancer_c_ptr, loadbalancer)  

        call USED_LOADBALANCING_INFO(loadbalancer)

      end subroutine

      subroutine PARTITIONING_INFO( loadbalancer_, detailed)
        use iso_c_binding
        use iso_fortran_env
        use LOADBALANCER, only: t_loadbalancer
        use LOADBALANCER_OUTPUT, only: OUTPUT_PARTITIONING
        implicit none
        integer(c_intptr_t), intent(in)           :: loadbalancer_
        logical, intent(in)                       :: detailed
        type(c_ptr)                               :: loadbalancer_c_ptr
        type(t_loadbalancer), pointer             :: loadbalancer
        loadbalancer_c_ptr = transfer(loadbalancer_, loadbalancer_c_ptr)
        call c_f_pointer(loadbalancer_c_ptr, loadbalancer)  
        loadbalancer% lb_log = .true.
        loadbalancer% lb_log_detailed = detailed

        call OUTPUT_PARTITIONING(loadbalancer)
      end subroutine

      subroutine COMMUNICATE_DATA_POINT   ( loadbalancer_ 
     &                                     , export_data_point
     &                                     , import_data_point)
        use iso_c_binding
        use iso_fortran_env
        use LOADBALANCER, only: t_loadbalancer
        implicit none
        integer(c_intptr_t), intent(in)           :: loadbalancer_
        type(c_ptr)                               :: loadbalancer_c_ptr
        type(t_loadbalancer), pointer             :: loadbalancer
#include "lb_callback.fi"
        procedure(serialize_point)                :: export_data_point
        procedure(deserialize_point)              :: import_data_point
cf2py   integer(c_int32_t) :: buffer_size
cf2py   integer*1, allocatable, dimension(buffer_size) :: buffer
cf2py   integer(c_int64_t) :: id
cf2py   call export_data_point(buffer, id, buffer_size)
cf2py   call import_data_point(buffer, id, buffer_size)
        loadbalancer_c_ptr = transfer(loadbalancer_, loadbalancer_c_ptr)
        call c_f_pointer(loadbalancer_c_ptr, loadbalancer)  
        loadbalancer% export_data_point => export_data_point
        loadbalancer% import_data_point => import_data_point
        call loadbalancer% COMMUNICATE_DATA_POINT()
      end subroutine

      subroutine COMMUNICATE_RESULT_POINT  ( loadbalancer_ 
     &                                     , export_result_point
     &                                     , import_result_point)
        use iso_c_binding
        use iso_fortran_env
        use LOADBALANCER, only: t_loadbalancer
        implicit none
        integer(c_intptr_t), intent(in)           :: loadbalancer_
        type(c_ptr)                               :: loadbalancer_c_ptr
        type(t_loadbalancer), pointer             :: loadbalancer
#include "lb_callback.fi"
        procedure(serialize_point)             :: export_result_point
        procedure(deserialize_point)           :: import_result_point
cf2py   integer(c_int32_t) :: buffer_size
cf2py   integer*1, allocatable, dimension(buffer_size) :: buffer
cf2py   integer(c_int64_t) :: id
cf2py   call export_result_point(buffer, id, buffer_size)
cf2py   call import_result_point(buffer, id, buffer_size)
        loadbalancer_c_ptr = transfer(loadbalancer_, loadbalancer_c_ptr)
        call c_f_pointer(loadbalancer_c_ptr, loadbalancer)  
        loadbalancer% export_result_point => export_result_point
        loadbalancer% import_result_point => import_result_point
        call loadbalancer% COMMUNICATE_RESULT_POINT()
      end subroutine

      subroutine COMMUNICATE_DATA          ( loadbalancer_ 
     &                                     , export_data
     &                                     , import_data)
        use iso_c_binding
        use iso_fortran_env
        use LOADBALANCER, only: t_loadbalancer
        implicit none
        integer(c_intptr_t), intent(in)           :: loadbalancer_
        type(c_ptr)                               :: loadbalancer_c_ptr
        type(t_loadbalancer), pointer             :: loadbalancer
#include "lb_callback.fi"
        procedure(serialize)                      :: export_data
        procedure(deserialize)                    :: import_data
cf2py   integer(c_int32_t) :: buffer_size
cf2py   integer(c_int32_t) :: ids_size
cf2py   integer*1, allocatable, dimension(buffer_size,ids_size):: buffer
cf2py   integer(c_int64_t), dimension(ids_size) :: ids
cf2py   call export_data(buffer, ids, buffer_size, ids_size)
cf2py   call import_data(buffer, ids, buffer_size, ids_size)
        loadbalancer_c_ptr = transfer(loadbalancer_, loadbalancer_c_ptr)
        call c_f_pointer(loadbalancer_c_ptr, loadbalancer)  
        loadbalancer% export_data => export_data
        loadbalancer% import_data => import_data
        call loadbalancer% COMMUNICATE_DATA()
      end subroutine

      subroutine COMMUNICATE_RESULT        ( loadbalancer_ 
     &                                     , export_result
     &                                     , import_result)
        use iso_c_binding
        use iso_fortran_env
        use LOADBALANCER, only: t_loadbalancer
        implicit none
        integer(c_intptr_t), intent(in)           :: loadbalancer_
        type(c_ptr)                               :: loadbalancer_c_ptr
        type(t_loadbalancer), pointer             :: loadbalancer
#include "lb_callback.fi"
        procedure(serialize)                   :: export_result
        procedure(deserialize)                 :: import_result
cf2py   integer(c_int32_t) :: buffer_size
cf2py   integer(c_int32_t) :: ids_size
cf2py   integer*1, allocatable, dimension(buffer_size,ids_size):: buffer
cf2py   integer(c_int64_t), dimension(ids_size) :: ids
cf2py   call export_result(buffer, ids, buffer_size, ids_size)
cf2py   call import_result(buffer, ids, buffer_size, ids_size)
        loadbalancer_c_ptr = transfer(loadbalancer_, loadbalancer_c_ptr)
        call c_f_pointer(loadbalancer_c_ptr, loadbalancer)  
        loadbalancer% export_result => export_result
        loadbalancer% import_result => import_result
        call loadbalancer% COMMUNICATE_RESULT()
      end subroutine

