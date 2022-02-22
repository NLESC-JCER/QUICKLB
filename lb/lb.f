      module LOADBALANCER
!---- Create non-pure subroutines whith _pure_ in DEBUG_CHECKS mode
!---- Necessary for writing output (which is not possible in pure subroutines)
!---- Eventually move this to the makefile?
#ifdef DEBUG_CHECKS
#define _pure_
#else
#define _pure_ pure
#endif

!---- include asserts, perhaps merge this with inca_error in some way
      use LOADBALANCER_DEBUG, only : ASSERT
      use LOADBALANCER_COMMUNICATION
      use MPI_F08
      use iso_fortran_env
#ifdef ENABLE_ZOLTAN
      use ZOLTAN, only                 : ZOLTAN_STRUCT
#endif

!---- Declare everything private, then declare public attributes
      private
      public :: LOADBALANCER_CREATE
      public :: t_loadbalancer

      type t_lb_info
        character(30) ::
     &        name                    = "DEFAULT"
     &      , export_data             = "associated, but no info set"
     &      , import_data             = "associated, but no info set"
     &      , export_result           = "associated, but no info set"
     &      , import_result           = "associated, but no info set"
     &      , partition               = "associated, but no info set"
      end type

!---- Loadbalancer, should be created through LOADBALANCER_CREATE
      type t_loadbalancer
!---- Info
        integer(int32)                           :: mpi_rank
        type(MPI_COMM)                           :: comm
        integer(int32)                           :: mpi_nnode
        logical                                  :: lb_log = .true.
        logical                                  :: lb_log_detailed
     &                                              = .false.

!---- Parameters
        integer(int32)                           :: data_block_bytes
        integer(int32)                           :: result_block_bytes
!---- Needed for (de)serialization and communication of blocks
        integer(int32)                           :: export_num_ids
        integer(int64), allocatable              :: export_ids(:)
!---- Ids should be grouped together such that we can neatly subdivide
!---- and send buffered information to our neighbours
        integer(int32), allocatable              :: export_proc_ids(:)
        BYTE, pointer, contiguous                :: data_send_buffer(:)
     &      => null()
        BYTE, pointer, contiguous                ::result_send_buffer(:)
     &      => null()

        integer(int32)                           :: import_num_ids
        integer(int64), allocatable              :: import_ids(:)
        integer(int32), allocatable              :: import_proc_ids(:)

        integer(int32)                           :: local_num_ids
        integer(int64), allocatable              :: local_ids(:)

        BYTE, pointer, contiguous              :: data_receive_buffer(:)
     &      => null()
        BYTE, pointer, contiguous            :: result_receive_buffer(:)
     &      => null()

!---- Administration for local blocks and weights.
!---- Needed for evaluation
        integer(int32)                           :: nblocks
        integer(int64)                           :: total_nblocks
        integer(int64)                           :: offset

!---- which blocks we can find on which processor?
        integer(int32), allocatable              :: vtxdist(:)
!---- How many compute nodes do we use?
        integer(int32)                           :: pc_nodes = 0
!---- Which node is ours?
        integer(int32)                           :: pc_node_id = 0
!---- What is its entry in vxtdist?
        integer(int32)                           :: pc_vtxdist_id = 0
!---- Real(4) is used so we can use MPI_FLOAT
        real(real32), allocatable                :: weights(:)
        real(real32), allocatable                :: weights_prev(:)

!---- Communications structure for fast communication
        type(t_lb_comm)                          :: communication

!---- Set this to false when we can not blindly send data back as result
!---- TODO implement
        logical                                  :: data_equals_result
     &      = .true.


#ifdef ENABLE_ZOLTAN
!---- Pointer to zoltan information, if used
        type(ZOLTAN_STRUCT), pointer             :: zoltan => null()
#endif

!---- Information about the used routines
        type(t_lb_info), pointer                   :: lb_info

#ifdef DEBUG_CHECKS
        logical                                  :: initialized =.false.
#endif


!---- Information for the partitioning algorithm
        real(real32)                           :: max_rel_li = .0! max load imbalance
        real(real32)                           :: max_abs_li = .0! max load imbalance
        integer(int32)                         :: max_it = 512  ! max iterations

!----
!     User defined functions that perform the serialization and
!     Deserialization.
        procedure(SERIALIZE_POINT)
     &      , pointer, nopass                  :: EXPORT_DATA_POINT
     &      => null()
        procedure(DESERIALIZE_POINT)
     &      , pointer, nopass                  :: IMPORT_DATA_POINT
     &      => null()
        procedure(SERIALIZE_POINT)
     &      , pointer, nopass                  :: EXPORT_RESULT_POINT
     &      => null()
        procedure(DESERIALIZE_POINT)
     &      , pointer, nopass                  :: IMPORT_RESULT_POINT
     &      => null()
        procedure(SERIALIZE)
     &      , pointer, nopass                  :: EXPORT_DATA
     &      => null()
        procedure(DESERIALIZE)
     &      , pointer, nopass                  :: IMPORT_DATA
     &      => null()
        procedure(SERIALIZE)
     &      , pointer, nopass                  :: EXPORT_RESULT
     &      => null()
        procedure(DESERIALIZE)
     &      , pointer, nopass                  :: IMPORT_RESULT
     &      => null()
!---- Partition functions should be exchangeable
        procedure(LOADBALANCER_PARTITION)
     &      , pointer, pass                    :: PARTITION
     &      => null()
      contains
        procedure,public,pass :: CREATE => LOADBALANCER_CREATE
        procedure,public,pass :: REALLOCATE_BUFFERS
     &       => LOADBALANCER_REALLOCATE_BUFFERS
        procedure,public,pass :: SET_EXPORT_IMPORT_ROUTINES
     &      => SET_EXPORT_IMPORT_ROUTINES
        procedure,public,pass :: COMMUNICATE_DATA
     &      => LOADBALANCER_COMMUNICATE_DATA
        procedure,public,pass :: COMMUNICATE_RESULT
     &      => LOADBALANCER_COMMUNICATE_RESULT
        procedure,public,pass :: COMMUNICATE_DATA_POINT
     &      => LOADBALANCER_COMMUNICATE_DATA_POINT
        procedure,public,pass :: COMMUNICATE_RESULT_POINT
     &      => LOADBALANCER_COMMUNICATE_RESULT_POINT
        procedure,public,pass :: CALCULATE_GID_OFFSET
     &      => LOADBALANCER_CALCULATE_GID_OFFSET
        procedure,public,pass :: CREATE_COMMUNICATION
     &      => LOADBALANCER_CREATE_COMMUNICATION
        final                 :: LOADBALANCER_DESTROY

      end type

#include "lb_callback.fi"
      interface
        subroutine LOADBALANCER_PARTITION ( this )
          import :: t_loadbalancer
          implicit none
          class(t_loadbalancer), intent(inout)  :: this
        end subroutine LOADBALANCER_PARTITION
      end interface

      contains
      subroutine LOADBALANCER_REALLOCATE_BUFFERS ( this )
        class(t_loadbalancer)                   :: this

        if( associated(this%data_send_buffer))
     &      deallocate(this%data_send_buffer)
        if( associated(this%data_receive_buffer))
     &      deallocate(this%data_receive_buffer)

        if( .not.this%data_equals_result )then
          if( associated(this% result_send_buffer))
     &        deallocate(this% result_send_buffer)
          if( associated(this%result_receive_buffer))
     &        deallocate(this%result_receive_buffer)
        end if

        allocate(this%data_receive_buffer(this%import_num_ids
     &                                   *this%data_block_bytes))
        allocate(this%data_send_buffer(this%export_num_ids
     &                                   *this%data_block_bytes))
        if( this%data_equals_result )then
          this% result_send_buffer => this%data_receive_buffer
          this% result_receive_buffer => this%data_send_buffer
        else
          allocate(this%result_receive_buffer(this%export_num_ids
     &                                     *this%result_block_bytes))
          allocate(this%result_send_buffer(this%import_num_ids
     &                                     *this%result_block_bytes))
        end if
#ifdef DEBUG_CHECKS
        this%data_receive_buffer = -2
        this%data_send_buffer = -1
#endif

      end subroutine


      subroutine LOADBALANCER_CREATE_COMMUNICATION ( this )
        implicit none
        class (t_loadbalancer)                        :: this
        integer(int32)                                :: n, nn, proc
        integer(int32)                                :: temp1
        integer(int64)                                :: temp2
        integer(int32)                                :: nn_prev
        proc = -1

        associate (comm => this%communication)

        if( allocated(comm% exports)) deallocate(comm% exports)
        if( allocated(comm% imports)) deallocate(comm% imports)

!---- Rearrange im and exports to be of increasing order, simple sort
!---- Also sort on block_id
        do n = 1, this%export_num_ids-1
          do nn = 1, this%export_num_ids-n
            if( 
     &         this%export_proc_ids(nn)>this%export_proc_ids(nn+1) )then
              temp1                     = this%export_proc_ids(nn)
              temp2                     = this%export_ids(nn)
              this%export_proc_ids(nn)  = this%export_proc_ids(nn+1)
              this%export_ids(nn)       = this%export_ids(nn+1)
              this%export_proc_ids(nn+1)= temp1
              this%export_ids(nn+1)     = temp2
            end if
            if( this%export_proc_ids(nn) == this%export_proc_ids(nn+1))
     &          then
              if( this%export_ids(nn) > this%export_ids(nn+1) )then
                temp2 = this%export_ids(nn)
                this%export_ids(nn) = this%export_ids(nn+1)
                this%export_ids(nn+1) = temp2
              end if
            end if
          end do
        end do
        do n = 1, this%import_num_ids-1
          do nn = 1, this%import_num_ids-n
            if( 
     &         this%import_proc_ids(nn)>this%import_proc_ids(nn+1) )then
              temp1                     = this%import_proc_ids(nn)
              temp2                     = this%import_ids(nn)
              this%import_proc_ids(nn)  = this%import_proc_ids(nn+1)
              this%import_ids(nn)       = this%import_ids(nn+1)
              this%import_proc_ids(nn+1)= temp1
              this%import_ids(nn+1)     = temp2
            end if
            if( this%import_proc_ids(nn) == this%import_proc_ids(nn+1))
     &          then
              if( this%import_ids(nn) > this%import_ids(nn+1) )then
                temp2 = this%import_ids(nn)
                this%import_ids(nn) = this%import_ids(nn+1)
                this%import_ids(nn+1) = temp2
              end if
            end if
          end do
        end do

        comm% exports_length = 0
        if( this% export_num_ids > 0 )then
          proc = this % export_proc_ids(1)
          comm% exports_length = 1
        end if

        do n = 1, this % export_num_ids
          if( this % export_proc_ids(n) /= proc )then
            comm% exports_length = comm% exports_length + 1
            proc = this % export_proc_ids(n)
          end if
        end do

        allocate(comm%exports(comm% exports_length))

        nn_prev = 1
        do n = 1, comm% exports_length
          do nn = nn_prev+1, this% export_num_ids
            if( this % export_proc_ids(nn)
     &          /= this % export_proc_ids(nn-1) )then
              exit
            end if
          end do

          comm% exports(n) % data_buf_start
     &        = ((nn_prev-1)*this% data_block_bytes) + 1
          comm% exports(n) % data_buf_end
     &        = ((nn-1)*this% data_block_bytes)
          comm% exports(n) % result_buf_start
     &        = ((nn_prev-1)*this% result_block_bytes) + 1
          comm% exports(n) % result_buf_end
     &        = ((nn-1)*this% result_block_bytes)
          comm%exports(n)% length = nn - nn_prev
          comm%exports(n)% proc = this% export_proc_ids(nn-1)

          nn_prev = nn
        end do

        comm% imports_length = 0
        if( this% import_num_ids > 0 )then
          proc = this % import_proc_ids(1)
          comm% imports_length = 1
        end if

        do n = 1, this % import_num_ids
          if( this % import_proc_ids(n) /= proc )then
            comm% imports_length = comm% imports_length + 1
            proc = this % import_proc_ids(n)
          end if
        end do

        allocate(comm%imports(comm% imports_length))

        nn_prev = 1
        do n = 1, comm% imports_length
          do nn = nn_prev+1, this% import_num_ids
            if( this % import_proc_ids(nn)
     &          /= this % import_proc_ids(nn-1) )then
              exit
            end if
          end do

          comm% imports(n) % data_buf_start
     &        = ((nn_prev-1)*this% data_block_bytes) + 1
          comm% imports(n) % data_buf_end
     &        = ((nn-1)*this% data_block_bytes)
          comm% imports(n) % result_buf_start
     &        = ((nn_prev-1)*this% result_block_bytes) + 1
          comm% imports(n) % result_buf_end
     &        = ((nn-1)*this% result_block_bytes)
          comm%imports(n)% length = nn - nn_prev
          comm%imports(n)% proc = this% import_proc_ids(nn-1)

          nn_prev = nn
        end do
        end associate

      end subroutine

!****f* LOADBALANCER/LOADBALANCER_CREATE
!  NAME
!     LOADBALANCER_CREATE
!  DESCRIPTION
!     Initialize a loadbalancer type
!     If you want to use a loadbalancer, this should be the first routine
!     to call to initialize the loadbalancer
!  AUTHOR
!     Victor Azizi
!  CREATION DATE
!     2020
!  SOURCE
!
      subroutine LOADBALANCER_CREATE ( loadbalancer
     &                               , data_block_bytes
     &                               , result_block_bytes
     &                               , nblocks
     &                               , communicator)
        use iso_c_binding, only : c_size_t
        implicit none
        class(t_loadbalancer)                   :: loadbalancer
        integer(int32)                          :: data_block_bytes
        integer(int32)                          :: result_block_bytes
        integer(int32)                          :: nblocks
        type(MPI_COMM)                          :: communicator

        loadbalancer%comm = communicator
        call MPI_COMM_RANK(communicator, loadbalancer%mpi_rank)
        call MPI_COMM_SIZE(communicator, loadbalancer%mpi_nnode)

        call ASSERT(data_block_bytes > 0)
        loadbalancer%data_block_bytes = data_block_bytes
        call ASSERT(result_block_bytes > 0)
        loadbalancer%result_block_bytes = result_block_bytes
!---- Allocate Arrays for loadbalancing
        allocate(loadbalancer% weights(nblocks))
        loadbalancer% weights = 1.

        call loadbalancer% CALCULATE_GID_OFFSET(nblocks)

        allocate(loadbalancer%lb_info)

      end subroutine LOADBALANCER_CREATE
!****

!****f* LOADBALANCER/LOADBALANCER_CALCULATE_GID_OFFSET
!  NAME
!     LOADBALANCER_CALCULATE_GID_OFFSET
!  DESCRIPTION
!     If we know how many points are local to a processor, use the mpi
!     ordering to find out what the offset is for each processor to retrieve
!     the global id of each local point
!  AUTHOR
!     Victor Azizi
!  CREATION DATE
!     2020
!  SOURCE
!
      subroutine LOADBALANCER_CALCULATE_GID_OFFSET( this ,nblocks)
        implicit none
        class(t_loadbalancer), intent(inout)      :: this
        integer(int32), intent(in)                :: nblocks
        integer(int32)                            :: i, err

        this%nblocks = nblocks
        allocate(this%vtxdist(this%mpi_nnode+1))
        this%vtxdist(1) = 0

        call MPI_ALLGATHER( nblocks, 1, MPI_INTEGER4
     &                    , this%vtxdist(2:), 1, MPI_INTEGER4
     &                    , this%comm, err)

        call ASSERT(err == MPI_SUCCESS, "Error in mpi allgather")
        ! Correct data in vtxdist
        do i = 1, this%mpi_nnode
          this%vtxdist(i+1) = this%vtxdist(i) + this%vtxdist(i+1)
        end do

        this%total_nblocks = this%vtxdist(this%mpi_rank+1)
        this%offset = this%vtxdist(this%mpi_rank+1)
      end subroutine

      subroutine SET_EXPORT_IMPORT_ROUTINES ( this
     &                                      , export_data
     &                                      , import_data
     &                                      , export_result
     &                                      , import_result )
        implicit none
        class(t_loadbalancer)                    :: this
        procedure(SERIALIZE)        :: export_data
        procedure(DESERIALIZE)      :: import_data
        procedure(SERIALIZE)        :: export_result
        procedure(DESERIALIZE)      :: import_result

        this% EXPORT_DATA => export_data
        this% IMPORT_DATA => import_data
        this% EXPORT_RESULT => export_result
        this% IMPORT_RESULT => import_result
      end subroutine

      subroutine LOADBALANCER_COMMUNICATE_DATA ( this )
        implicit none
        class(t_loadbalancer), intent(inout)      :: this
        integer(int64)                            :: n
        type(MPI_REQUEST):: send_reqs(this%communication%exports_length)
        type(MPI_REQUEST):: recv_reqs(this%communication%imports_length)
        type(BYTEsliceptr)::
     &    send_slices(this%communication%exports_length)
        type(BYTEsliceptr)::
     &     recv_slices(this%communication%imports_length)
        integer(int32)                            :: err
        integer*1, pointer, dimension(:,:)        :: buffer
        type(MPI_COMM) :: comm_inca
        comm_inca = this%comm

        buffer(1:this%data_block_bytes,1:this%export_num_ids) => 
     &      this%data_send_buffer
        call this%export_data( buffer, this%export_ids
     &     , this%data_block_bytes, this%export_num_ids)


        associate ( comm => this% communication)
        ! MPI SEND
        do n = 1, comm%exports_length
          send_slices(n)%p =>
     &         this%data_send_buffer( comm%exports(n)%data_buf_start
     &                             : comm%exports(n)%data_buf_end)
          call MPI_ISEND(
     &          send_slices(n)%p
     &        , this% data_block_bytes*comm%exports(n)%length
     &        , MPI_BYTE
     &        , comm%exports(n)%proc, 413
     &        , comm_inca, send_reqs(n), err)
        end do
        ! MPI RECEIVE
        do n = 1, comm%imports_length
          recv_slices(n)%p =>
     &        this%data_receive_buffer( comm%imports(n)%data_buf_start
     &                                : comm%imports(n)%data_buf_end)
          call MPI_IRECV(
     &          recv_slices(n)%p
     &        , this% data_block_bytes*comm%imports(n)%length
     &        , MPI_BYTE
     &        , comm%imports(n)%proc, 413
     &        , comm_inca, recv_reqs(n), err)
        end do

        call MPI_WAITALL( this%communication%imports_length
     &                  , recv_reqs, MPI_STATUSES_IGNORE, err)

!       do n = 1, this%import_num_ids
!         call this% import_data_point( this%data_receive_buffer(
!    &          (n-1)*this% data_block_bytes+1:n*this% data_block_bytes)
!    &                          , n 
!    &                          , this% data_block_bytes)

!       end do

        call MPI_WAITALL( this%communication%exports_length
     &                  , send_reqs, MPI_STATUSES_IGNORE, err)
        end associate

      end subroutine LOADBALANCER_COMMUNICATE_DATA

!---- Inverse of communicate data
      subroutine LOADBALANCER_COMMUNICATE_RESULT ( this )
        implicit none
        class(t_loadbalancer)                     :: this

        integer(int64)                            :: n
        type(MPI_REQUEST):: recv_reqs(this%communication%exports_length)
        type(MPI_REQUEST):: send_reqs(this%communication%imports_length)
        type(BYTEsliceptr)::
     &    recv_slices(this%communication%exports_length)
        type(BYTEsliceptr)::
     &    send_slices(this%communication%imports_length)
        integer(int32)                             :: err
        type(MPI_COMM) :: comm_inca
        comm_inca = this%comm

!       do n = 1, this%import_num_ids
!         call this% export_result_point( this%result_send_buffer(
!    &      (n-1)*this% result_block_bytes+1:n*this% result_block_bytes)
!    &                          , n 
!    &                          , this% result_block_bytes)

!        end do

        associate ( comm => this% communication)
        ! MPI SEND
        do n = 1, comm%imports_length
          send_slices(n)%p =>
     &         this%result_send_buffer( comm%imports(n)%result_buf_start
     &                                : comm%imports(n)%result_buf_end)
          call MPI_ISEND(
     &          send_slices(n)%p
     &        , this% result_block_bytes*comm%imports(n)%length
     &        , MPI_BYTE
     &        , comm%imports(n)%proc, 4242
     &        , comm_inca, send_reqs(n), err)
        end do
        ! MPI RECEIVE
        do n = 1, comm%exports_length
          recv_slices(n)%p => this%result_receive_buffer(
     &                                  comm%exports(n)%result_buf_start
     &                                : comm%exports(n)%result_buf_end)
          call MPI_IRECV(
     &          recv_slices(n)%p
     &        , this% result_block_bytes*comm%exports(n)%length
     &        , MPI_BYTE
     &        , comm%exports(n)%proc, 4242
     &        , comm_inca, recv_reqs(n), err)
        end do

        call MPI_WAITALL( this%communication%exports_length
     &                  , recv_reqs, MPI_STATUSES_IGNORE, err)

!       do n = 1, this%export_num_ids
!         call this% import_result_point( this%result_receive_buffer(
!    &      (n-1)*this% result_block_bytes+1:n*this% result_block_bytes)
!    &                          , this%export_ids(n) 
!    &                          , this% result_block_bytes)

!       end do

        call MPI_WAITALL( this%communication%imports_length
     &                  , send_reqs, MPI_STATUSES_IGNORE, err)
        end associate

      end subroutine LOADBALANCER_COMMUNICATE_RESULT

      subroutine LOADBALANCER_COMMUNICATE_DATA_POINT ( this )
        implicit none
        class(t_loadbalancer), intent(inout)      :: this
        integer(int64)                            :: n
        type(MPI_REQUEST):: send_reqs(this%communication%exports_length)
        type(MPI_REQUEST):: recv_reqs(this%communication%imports_length)
        type(BYTEsliceptr)::
     &    send_slices(this%communication%exports_length)
        type(BYTEsliceptr)::
     &     recv_slices(this%communication%imports_length)
        integer(int32)                            :: err
        type(MPI_COMM) :: comm_inca
        comm_inca = this%comm

        do n = 1, this%export_num_ids
          call this% export_data_point( this%data_send_buffer(
     &          (n-1)*this% data_block_bytes+1:n*this% data_block_bytes)
     &                          , this%export_ids(n) 
     &                          , this% data_block_bytes)

        end do

        associate ( comm => this% communication)
        ! MPI SEND
        do n = 1, comm%exports_length
          send_slices(n)%p =>
     &         this%data_send_buffer( comm%exports(n)%data_buf_start
     &                             : comm%exports(n)%data_buf_end)
          call MPI_ISEND(
     &          send_slices(n)%p
     &        , this% data_block_bytes*comm%exports(n)%length
     &        , MPI_BYTE
     &        , comm%exports(n)%proc, 413
     &        , comm_inca, send_reqs(n), err)
        end do
        ! MPI RECEIVE
        do n = 1, comm%imports_length
          recv_slices(n)%p =>
     &        this%data_receive_buffer( comm%imports(n)%data_buf_start
     &                                : comm%imports(n)%data_buf_end)
          call MPI_IRECV(
     &          recv_slices(n)%p
     &        , this% data_block_bytes*comm%imports(n)%length
     &        , MPI_BYTE
     &        , comm%imports(n)%proc, 413
     &        , comm_inca, recv_reqs(n), err)
        end do

        call MPI_WAITALL( this%communication%imports_length
     &                  , recv_reqs, MPI_STATUSES_IGNORE, err)

        do n = 1, this%import_num_ids
          call this% import_data_point( this%data_receive_buffer(
     &          (n-1)*this% data_block_bytes+1:n*this% data_block_bytes)
     &                          , n 
     &                          , this% data_block_bytes)

        end do

        call MPI_WAITALL( this%communication%exports_length
     &                  , send_reqs, MPI_STATUSES_IGNORE, err)
        end associate

      end subroutine LOADBALANCER_COMMUNICATE_DATA_POINT

!---- Inverse of communicate data
      subroutine LOADBALANCER_COMMUNICATE_RESULT_POINT ( this )
        implicit none
        class(t_loadbalancer)                     :: this

        integer(int64)                            :: n
        type(MPI_REQUEST):: recv_reqs(this%communication%exports_length)
        type(MPI_REQUEST):: send_reqs(this%communication%imports_length)
        type(BYTEsliceptr)::
     &    recv_slices(this%communication%exports_length)
        type(BYTEsliceptr)::
     &    send_slices(this%communication%imports_length)
        integer(int32)                             :: err
        type(MPI_COMM) :: comm_inca
        comm_inca = this%comm

        do n = 1, this%import_num_ids
          call this% export_result_point( this%result_send_buffer(
     &      (n-1)*this% result_block_bytes+1:n*this% result_block_bytes)
     &                          , n 
     &                          , this% result_block_bytes)

        end do

        associate ( comm => this% communication)
        ! MPI SEND
        do n = 1, comm%imports_length
          send_slices(n)%p =>
     &         this%result_send_buffer( comm%imports(n)%result_buf_start
     &                                : comm%imports(n)%result_buf_end)
          call MPI_ISEND(
     &          send_slices(n)%p
     &        , this% result_block_bytes*comm%imports(n)%length
     &        , MPI_BYTE
     &        , comm%imports(n)%proc, 4242
     &        , comm_inca, send_reqs(n), err)
        end do
        ! MPI RECEIVE
        do n = 1, comm%exports_length
          recv_slices(n)%p => this%result_receive_buffer(
     &                                  comm%exports(n)%result_buf_start
     &                                : comm%exports(n)%result_buf_end)
          call MPI_IRECV(
     &          recv_slices(n)%p
     &        , this% result_block_bytes*comm%exports(n)%length
     &        , MPI_BYTE
     &        , comm%exports(n)%proc, 4242
     &        , comm_inca, recv_reqs(n), err)
        end do

        call MPI_WAITALL( this%communication%exports_length
     &                  , recv_reqs, MPI_STATUSES_IGNORE, err)

        do n = 1, this%export_num_ids
          call this% import_result_point( this%result_receive_buffer(
     &      (n-1)*this% result_block_bytes+1:n*this% result_block_bytes)
     &                          , this%export_ids(n) 
     &                          , this% result_block_bytes)

        end do

        call MPI_WAITALL( this%communication%imports_length
     &                  , send_reqs, MPI_STATUSES_IGNORE, err)
        end associate

      end subroutine LOADBALANCER_COMMUNICATE_RESULT_POINT

      subroutine LOADBALANCER_DESTROY ( this )
        implicit none
        type(t_loadbalancer)                     :: this

        if (allocated(this%export_ids)) deallocate(this%export_ids)
        if (allocated(this%export_proc_ids))
     &    deallocate(this%export_proc_ids)
        if (associated(this%data_send_buffer))
     &    deallocate(this%data_send_buffer)
        if (associated(this%data_receive_buffer))
     &    deallocate(this%data_receive_buffer)
        if (associated(this%result_send_buffer))
     &    deallocate(this%result_send_buffer)
        if (associated(this%result_receive_buffer))
     &    deallocate(this%result_receive_buffer)
        if (allocated(this%import_ids)) deallocate(this%import_ids)
        if (allocated(this%import_proc_ids))
     &    deallocate(this%import_proc_ids)
        if (allocated(this%local_ids))
     &    deallocate(this%local_ids)
        if (allocated(this%vtxdist)) deallocate(this%vtxdist)
        if (allocated(this%weights)) deallocate(this%weights)
        if (allocated(this%weights_prev)) deallocate(this%weights_prev)
#ifdef ENABLE_ZOLTAN
        if (associated(this%zoltan)) deallocate(this%zoltan)
#endif
        if (associated(this%lb_info)) deallocate(this%lb_info)

      end subroutine LOADBALANCER_DESTROY

      end module LOADBALANCER
