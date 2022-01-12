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
!---- Parameters
        integer                                  :: block_npoints
        integer                                  :: data_block_bytes
        integer                                  :: result_block_bytes
!---- Needed for (de)serialization and communication of blocks
        integer                                  :: export_num_ids
        integer, allocatable                     :: export_ids(:)
!---- Ids should be grouped together such that we can neatly subdivide
!---- and send buffered information to our neighbours
        integer, allocatable                     :: export_proc_ids(:)
        BYTE, pointer, contiguous                :: data_send_buffer(:)
     &      => null()
        BYTE, pointer, contiguous                ::result_send_buffer(:)
     &      => null()

        integer                                  :: import_num_ids
        integer, allocatable                     :: import_ids(:)
        integer, allocatable                     :: import_proc_ids(:)

        integer                                  :: local_num_ids
        integer, allocatable                     :: local_ids(:)

        BYTE, pointer, contiguous              :: data_receive_buffer(:)
     &      => null()
        BYTE, pointer, contiguous            :: result_receive_buffer(:)
     &      => null()

!---- Administration for local blocks and weights.
!---- Needed for evaluation
        integer                                  :: nblocks
        integer                                  :: total_nblocks
        integer                                  :: offset

!---- which blocks we can find on which processor?
        integer, allocatable                     :: vtxdist(:)
!---- How many compute nodes do we use?
        integer                                  :: pc_nodes = 0
!---- Which node is ours?
        integer                                  :: pc_node_id = 0
!---- What is its entry in vxtdist?
        integer                                  :: pc_vtxdist_id = 0
!---- Real(4) is used so we can use MPI_FLOAT
        real(4), allocatable                     :: weights(:)
        real(4), allocatable                     :: weights_prev(:)

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
        real                                   :: max_rel_li = .0! max load imbalance
        real                                   :: max_abs_li = .0! max load imbalance
        integer                                :: max_it = 512  ! max iterations

!----
!     User defined functions that perform the serialization and
!     Deserialization.
        procedure(LOADBALANCER_SERIALIZE)
     &      , pointer, pass                    :: EXPORT_DATA
     &      => null()
        procedure(LOADBALANCER_DESERIALIZE)
     &      , pointer, pass                    :: IMPORT_DATA
     &      => null()
        procedure(LOADBALANCER_SERIALIZE)
     &      , pointer, pass                    :: EXPORT_RESULT
     &      => null()
        procedure(LOADBALANCER_DESERIALIZE)
     &      , pointer, pass                    :: IMPORT_RESULT
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
        procedure,public,pass :: EVALUATE => LOADBALANCER_EVALUATE
        procedure,public,pass :: CALCULATE_GID_OFFSET
     &      => LOADBALANCER_CALCULATE_GID_OFFSET
        procedure,public,pass :: CREATE_COMMUNICATION
     &      => LOADBALANCER_CREATE_COMMUNICATION
        final                 :: LOADBALANCER_DESTROY

      end type

      interface
        subroutine LOADBALANCER_SERIALIZE ( this
     &                                    , block_bytes
     &                                    , serialize_num_ids
     &                                    , serialize_ids
     &                                    , buffer )
          import :: t_loadbalancer
          implicit none
          class(t_loadbalancer), intent(inout)     :: this
          integer, intent(in)                     :: block_bytes
          integer, intent(in)                     :: serialize_num_ids
          integer, intent(in)                     :: serialize_ids(:)
!---- This cannot be intent out because the thermodynamics does not actually
!---- fill the buffer via its argument, but uses other pointers that point to it
          BYTE, pointer, intent(inout)            :: buffer (:)
        end subroutine LOADBALANCER_SERIALIZE
        subroutine LOADBALANCER_DESERIALIZE ( this
     &                                      , block_bytes
     &                                      , deserialize_num_ids
     &                                      , deserialize_ids
     &                                      , buffer )
          import :: t_loadbalancer
          implicit none
          class(t_loadbalancer), intent(inout)     :: this
          integer, intent(in)                    :: block_bytes
          integer, intent(in)                    :: deserialize_num_ids
          integer, intent(in)                    :: deserialize_ids(:)
          BYTE, pointer, intent(inout)           :: buffer (:)
        end subroutine LOADBALANCER_DESERIALIZE

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
        integer                                       :: n, nn, proc
        integer                                       :: temp1, temp2
        integer                                       :: nn_prev

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
                temp1 = this%export_ids(nn)
                this%export_ids(nn) = this%export_ids(nn+1)
                this%export_ids(nn+1) = temp1
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
                temp1 = this%import_ids(nn)
                this%import_ids(nn) = this%import_ids(nn+1)
                this%import_ids(nn+1) = temp1
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
     &                               , block_npoints)
        use mpi_f08
        use iso_c_binding, only : c_size_t
        implicit none
        class(t_loadbalancer)                   :: loadbalancer
        integer                                 :: data_block_bytes
        integer                                 :: result_block_bytes
        integer                                 :: nblocks
        integer                                 :: block_npoints
        integer                                 :: alloc_stat
        call ASSERT(data_block_bytes > 0
     &             , __FILE__
     &             , __LINE__ )
        loadbalancer%data_block_bytes = data_block_bytes
        call ASSERT(result_block_bytes > 0
     &             , __FILE__
     &             , __LINE__ )
        loadbalancer%result_block_bytes = result_block_bytes
        call ASSERT(block_npoints > 0
     &             , __FILE__
     &             , __LINE__ )
        loadbalancer% block_npoints = block_npoints
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
        use mpi_f08
        implicit none
        class(t_loadbalancer), intent(inout)      :: this
        integer, intent(in)                      :: nblocks
        integer                                  :: err
        integer                                  :: i,newrank,buf
        integer                                  :: newcomm

        this%nblocks = nblocks
        allocate(this%vtxdist(mpi_nnode+1))
        this%vtxdist(1) = 0

        call MPI_ALLGATHER( nblocks, 1, MPI_INT
     &                    , this%vtxdist(2:), 1, MPI_INT
     &                    , mpi_comm_inca, err)

        call ASSERT(err == MPI_SUCCESS
     &      , __FILE__
     &      , __LINE__
     &      , "Error in mpi allgather")
        ! Correct data in vtxdist
        do i = 1, mpi_nnode
          this%vtxdist(i+1) = this%vtxdist(i) + this%vtxdist(i+1)
        end do

        this%total_nblocks = this%vtxdist(mpi_nnode+1)
        this%offset = this%vtxdist(whoami+1)
      end subroutine

      subroutine SET_EXPORT_IMPORT_ROUTINES ( this
     &                                      , export_data
     &                                      , import_data
     &                                      , export_result
     &                                      , import_result )
        implicit none
        class(t_loadbalancer)                    :: this
        procedure(LOADBALANCER_SERIALIZE)        :: export_data
        procedure(LOADBALANCER_DESERIALIZE)      :: import_data
        procedure(LOADBALANCER_SERIALIZE)        :: export_result
        procedure(LOADBALANCER_DESERIALIZE)      :: import_result

        this% EXPORT_DATA => export_data
        this% IMPORT_DATA => import_data
        this% EXPORT_RESULT => export_result
        this% IMPORT_RESULT => import_result
      end subroutine

      subroutine LOADBALANCER_EVALUATE ( this )
        implicit none
        class(t_loadbalancer)                     :: this

      end subroutine LOADBALANCER_EVALUATE

      subroutine LOADBALANCER_COMMUNICATE_DATA ( this )
        use mpi_f08
        implicit none
        class(t_loadbalancer), intent(inout)      :: this
        integer                                   :: n
        type(MPI_REQUEST):: send_reqs(this%communication%exports_length)
        type(MPI_REQUEST):: recv_reqs(this%communication%imports_length)
        type(BYTEsliceptr)::
     &    send_slices(this%communication%exports_length)
        type(BYTEsliceptr)::
     &     recv_slices(this%communication%imports_length)
        integer                                   :: err
        type(MPI_COMM) :: comm_inca
        comm_inca %MPI_VAL = mpi_comm_inca


        call this%EXPORT_DATA( this%block_npoints,
     &                         this%export_num_ids,
     &                         this%export_ids,
     &                         this%data_send_buffer )


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

        call this%IMPORT_DATA( this%block_npoints
     &                       , this%import_num_ids
     &                       , this%import_ids
     &                       , this%data_receive_buffer )

        call MPI_WAITALL( this%communication%exports_length
     &                  , send_reqs, MPI_STATUSES_IGNORE, err)
        end associate

      end subroutine LOADBALANCER_COMMUNICATE_DATA

!---- Inverse of communicate data
      subroutine LOADBALANCER_COMMUNICATE_RESULT ( this )
        use mpi_f08
        implicit none
        class(t_loadbalancer)                     :: this

        integer                                   :: n
        type(MPI_REQUEST):: recv_reqs(this%communication%exports_length)
        type(MPI_REQUEST):: send_reqs(this%communication%imports_length)
        type(BYTEsliceptr)::
     &    recv_slices(this%communication%exports_length)
        type(BYTEsliceptr)::
     &    send_slices(this%communication%imports_length)
        integer                                   :: err
        type(MPI_COMM) :: comm_inca
        comm_inca %MPI_VAL = mpi_comm_inca
        call this%EXPORT_RESULT( this%block_npoints,
     &                           this%import_num_ids,
     &                           this%import_ids,
     &                           this%result_send_buffer )
        associate ( comm => this% communication)
        ! MPI SEND
        do n = 1, comm%imports_length
          send_slices(n)%p =>
     &         this%result_send_buffer( comm%imports(n)%result_buf_start
     &                                : comm%imports(n)%result_buf_end)
          call MPI_ISEND(
     &          send_slices(n)%p
     &        , this% data_block_bytes*comm%imports(n)%length
     &        , MPI_BYTE
     &        , comm%imports(n)%proc, 4242
     &        , comm_inca, send_reqs(n), err)
        end do
        ! MPI RECEIVE
        do n = 1, comm%exports_length
          recv_slices(n)%p => this%result_receive_buffer(
     &                                  comm%exports(n)%result_buf_start
     &                                : comm%exports(n)%data_buf_end)
          call MPI_IRECV(
     &          recv_slices(n)%p
     &        , this% data_block_bytes*comm%exports(n)%length
     &        , MPI_BYTE
     &        , comm%exports(n)%proc, 4242
     &        , comm_inca, recv_reqs(n), err)
        end do

        call MPI_WAITALL( this%communication%exports_length
     &                  , recv_reqs, MPI_STATUSES_IGNORE, err)

        call this%IMPORT_RESULT( this%block_npoints
     &                         , this%import_num_ids
     &                         , this%import_ids 
     &                         , this%data_receive_buffer )

        call MPI_WAITALL( this%communication%imports_length
     &                  , send_reqs, MPI_STATUSES_IGNORE, err)
        end associate

      end subroutine LOADBALANCER_COMMUNICATE_RESULT

      subroutine LOADBALANCER_DESTROY ( this )
        implicit none
        type(t_loadbalancer)                     :: this

      end subroutine LOADBALANCER_DESTROY

      end module LOADBALANCER
