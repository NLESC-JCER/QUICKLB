      subroutine LOADBALANCER_CREATE ( loadbalancer_
     &                               , data_block_bytes
     &                               , result_block_bytes
     &                               , nblocks
     &                               , block_npoints
     &                               , communicator_)
        use iso_c_binding
        use iso_fortran_env
        use mpi_f08
        use LOADBALANCER, only: LB_CREATE => LOADBALANCER_CREATE
     &                        , t_loadbalancer
        implicit none
        integer(c_intptr_t),intent(out)         :: loadbalancer_
        integer(c_int32_t) ,value,intent(in)    :: data_block_bytes
        integer(c_int32_t) ,value,intent(in)    :: result_block_bytes
        integer(c_int32_t) ,value,intent(in)    :: nblocks
        integer(c_int32_t) ,value,intent(in)    :: block_npoints
        integer(c_int32_t) ,value,intent(in)    :: communicator_

        type(t_loadbalancer), pointer           :: loadbalancer
        type(MPI_COMM)                          :: communicator
        communicator%MPI_VAL = communicator_

        allocate(loadbalancer)
        call LB_CREATE( loadbalancer
     &                , data_block_bytes
     &                , result_block_bytes
     &                , nblocks
     &                , block_npoints
     &                , communicator)
        loadbalancer_ = loc(loadbalancer)
      end subroutine
