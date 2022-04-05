      module QUICKLB

      use iso_c_binding
      use iso_fortran_env
      use mpi_f08
      use loadbalancer, only: t_loadbalancer_ => t_loadbalancer
      use loadbalancer_set_partition, only: lb_set_partition_algorithm 
     &                                   => set_partition_algorithm
      use loadbalancer_output, only: used_loadbalancing_info
     &                             , output_partitioning

      implicit none

#include "lb_callback.fi"

      ! Cleaner wrapper of the messy stuff
      type t_loadbalancer
        type(t_loadbalancer_) :: lb
        contains
          procedure, pass     :: set_partition_algorithm
     &                        => set_partition_algorithm
          procedure, pass     :: set_weights => set_weights
          procedure, pass     :: set_communication_routines
     &                        => set_communication_routines
          procedure, pass     :: info => info
          procedure, pass     :: communicate_data => communicate_data
          procedure, pass     :: communicate_result =>communicate_result
          procedure, pass     :: partitioning_info => partitioning_info 
      end type

      private

      public :: create_loadbalancer

      contains

      function create_loadbalancer( data_block_bytes
     &                            , result_block_bytes
     &                            , nblocks
     &                            , communicator_) result(loadbalancer)
        implicit none
        type(t_loadbalancer)                   :: loadbalancer
        integer(c_int32_t) ,intent(in)         :: data_block_bytes       
        integer(c_int32_t) ,intent(in)         :: result_block_bytes     
        integer(c_int32_t) ,intent(in)         :: nblocks                
        type(mpi_comm), intent(in), optional   :: communicator_

        type(mpi_comm)                         :: communicator
        logical                                :: initialized
        integer                                :: err


        if (.not.present(communicator_)) then
          ! initialize MPI if we've forgotten
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
          communicator = mpi_comm_world
        else
          communicator = communicator_
        end if

        call loadbalancer% lb% create( data_block_bytes
     &                               , result_block_bytes
     &                               , nblocks
     &                               , communicator)
      end function

      subroutine set_partition_algorithm( loadbalancer
     &                                  , name, max_rel_li, max_abs_li
     &                                  , max_it )
        class(t_loadbalancer), intent(inout)      :: loadbalancer
        character(len=20),  intent(in)            :: name
        real(c_float),      intent(in)            :: max_rel_li
        real(c_float),      intent(in)            :: max_abs_li
        integer(c_int32_t), intent(in)            :: max_it

        call lb_set_partition_algorithm( loadbalancer% lb, name
     &                                 , max_rel_li, max_abs_li, max_it)
      end subroutine

      subroutine set_weights( loadbalancer
     &                      , weights )
        class(t_loadbalancer), intent(inout)      :: loadbalancer
        real(c_float), target,intent(in)          :: weights(:)
        
        if ( size(weights) /= loadbalancer% lb% nblocks) then
          write(*,*) "error info: nblocks=", loadbalancer% lb% nblocks
          error stop "Weights array must be exactly nblocks long"
        end if
        
        loadbalancer% lb% weights => weights
      end subroutine 

      subroutine set_communication_routines ( loadbalancer
     &     , export_data, import_data, export_result, import_result )
        class(t_loadbalancer), intent(inout)     :: loadbalancer
        procedure(serialize)                     :: export_data
        procedure(serialize)                     :: export_result
        procedure(deserialize)                   :: import_data
        procedure(deserialize)                   :: import_result

        call loadbalancer% lb% set_export_import_routines ( export_data
     &      , import_data, export_result, import_result )       
      end subroutine
      
      subroutine partitioning_info (loadbalancer, detailed)
        class(t_loadbalancer), intent(inout)    :: loadbalancer
        logical, intent(in), optional           :: detailed 
        loadbalancer% lb% lb_log = .true.
        if (present(detailed)) then
          loadbalancer% lb% lb_log_detailed = detailed
        endif
        call output_partitioning(loadbalancer% lb)


      end subroutine
      subroutine info( loadbalancer )
        class(t_loadbalancer), intent(inout)    :: loadbalancer
        call used_loadbalancing_info(loadbalancer% lb)
      end subroutine

      subroutine communicate_data ( loadbalancer )
        class(t_loadbalancer), intent(inout)    :: loadbalancer
        call loadbalancer% lb% communicate_data() 
      end subroutine

      subroutine communicate_result ( loadbalancer )
        class(t_loadbalancer), intent(inout)    :: loadbalancer
        call loadbalancer% lb% communicate_result() 
      end subroutine

      end module
