      module LOADBALANCER_PARTITION
!---- Create non-pure subroutines whith _pure_ in DEBUG_CHECKS mode
!---- Necessary for writing output (which is not possible in pure subroutines)
!---- Eventually move this to the makefile?
#ifdef DEBUG_CHECKS
#define _pure_
#else
#define _pure_ pure
#endif

      use LOADBALANCER
      use LOADBALANCER_DEBUG, only : ASSERT
      use mpi_f08
      use iso_fortran_env
      use iso_c_binding, only : c_f_pointer, c_ptr, c_loc

      private

      type(MPI_COMM)                      :: comm_local  = MPI_COMM_NULL
      integer(int32)                      :: whoami_local= -1
      integer(int32)                      :: nnode_local = -1
      type(MPI_GROUP)                     :: group_local =MPI_GROUP_NULL
      type(MPI_GROUP)                     :: group_global=MPI_GROUP_NULL

#ifdef ENABLE_ZOLTAN
      integer(int32)                      :: zolt_int_pointer_size = 0
      logical                             :: zoltan_initialized =.false.
      public :: PARTITION_ZOLTAN
#endif
      public  :: PARTITION_GREEDY, PARTITION_SORT
      public  :: PARTITION_SORT2

      contains

      pure recursive subroutine QUICKSORT(a, b, first, last)
        implicit none
        real(real32), intent(inout)              :: a(:)
        integer(int32), intent(inout)            :: b(:)
        integer(int32), intent(in)               :: first, last
        integer(int32)                           :: t_i
        real(real32)                             :: t_r, x
        integer(int32)                           :: i, j

        x = a((first+last)/2)
        i = first
        j = last
        do
         do while( a(i) < x)
          i=i+1
         end do
         do while( x < a(j))
          j=j-1
         end do
         if( i >= j )EXIT
         t_r  = a(i)
         a(i) = a(j)
         a(j) = t_r
         t_i  = b(i)
         b(i) = b(j)
         b(j) = t_i
         i=i+1
         j=j-1
        end do
        if( first < i-1 )call quicksort(a, b, first, i-1)
        if( j+1 < last )call quicksort(a, b, j+1, last)
      end subroutine QUICKSORT

      subroutine PARTITION_SORT_ALGORITHM ( this, comm, nnode, whoami
     &                                    , cur_li, points_left
     &                                    , points_sent, cum_time
     &                                    , imported_time
     &                                    , avg_time )
        implicit none
        class(t_loadbalancer), intent(inout)    :: this
        type(MPI_COMM), intent(in)              :: comm
        integer(int32), intent(in)              :: nnode
        integer(int32), intent(in)              :: whoami
        real(real32), intent(in)                :: cur_li
        integer(int32), intent(inout)           :: points_left
        integer(int32), intent(inout)           :: points_sent
        real(real32), intent(inout)             :: cum_time
        real(real32), intent(inout)             :: imported_time
        real(real32), intent(in)                :: avg_time
        integer(int32)                          :: err
        integer(int32)                          :: n, nn, x
        integer(int32)                          :: n_p(1), nn_p(1)
        integer(int32)                          :: points_recv
        integer(int64), allocatable             :: import_ids_temp(:)
        integer(int32), allocatable             :: import_proc_temp(:)
        integer(int32)                          :: imported_points
        integer(int32)                          :: rank
        integer(int64), allocatable             :: points_buf(:)
        real(real32), allocatable               :: weight_buf(:)
        integer(int32), allocatable             :: all_rank(:)
        real(real32), allocatable               :: all_li(:)
        real(real32)                            :: my_li, partner_li
        real(real32)                            :: prev_li
        type(MPI_STATUS)                        :: mpi_stat
        integer(int32)                          :: partner_mpi_rank
        integer(int32)                          :: partner_mpi_rank_g(1)
        integer(int32)                          :: partner_rank
        real(real32)                            :: temp
        logical                             :: communicate
        real(real32)                            :: cur_li_p(1)

        imported_points = this%import_num_ids

!---- Static buffers
        allocate(all_rank(nnode))
        do n = 1, nnode
          all_rank(n) = n-1
        end do
        allocate(all_li(nnode))

!---- Communicate our current surplus/deficit to everyone
        cur_li_p = cur_li
        call MPI_ALLGATHER( cur_li_p,1,MPI_REAL4,all_li,1,MPI_REAL4
     &               , comm, err)

        prev_li = maxval(all_li)
        do x = 1, this% max_it
!---- Rank all Procs according to li
          call QUICKSORT(all_li, all_rank, 1, nnode)

!---- Get our weighted rank (x)
          do rank = 1, nnode
            if( all_rank(rank) == whoami )EXIT
          end do

!---- Get our partners mpi rank from weight rank
          partner_rank = nnode-rank+1
          partner_mpi_rank = all_rank(partner_rank)
!---- If this is not the comm find the global partner rank
          if( this%comm /= comm )then
          call MPI_GROUP_TRANSLATE_RANKS(group_local, 1
     &     , (/partner_mpi_rank/), group_global
     &     , partner_mpi_rank_g, err)
          else
            partner_mpi_rank_g = partner_mpi_rank
          end if
          my_li = all_li(rank)
          partner_li = all_li(partner_rank)

      ! We have the li, if both have the same sign, do nothing
          communicate = .true.
          if( (my_li < 0) .eqv.
     &        (partner_li < 0) )then
             communicate = .false.
          end if
          if( (my_li == 0.) .or. (partner_li == 0.) )then
            communicate = .false.
          end if

!---- Update Load imbalances (for next iteration)
          do n = 1, nnode/2
            nn = nnode - n + 1
            if( (all_li(n) < 0) .eqv.(all_li(nn)<0) )then
              exit
            end if
            temp = min(abs(all_li(n)), abs(all_li(nn)))
            if( all_li(n) < 0 )then
              all_li(n) = all_li(n) + temp
              all_li(nn) = all_li(nn) - temp
            else
              all_li(n) = all_li(n) - temp
              all_li(nn) = all_li(nn) + temp
            end if
          end do

!---- Check if we are done
          if( prev_li == 0. )EXIT
          if( maxval(all_li)  < this% max_abs_li  )then
            exit
          end if
          if( (1. -  maxval(all_li)/prev_li ) < this% max_rel_li  )then
            exit
          end if
          prev_li = maxval(all_li)

          if( .not. communicate )then
            cycle
          end if
!---- Calculate send/recv blocks and communicate this
          if( my_li > 0 )then
            call MPI_SEND( this%export_ids(points_sent+1
     &                    : this%export_num_ids)
     &          , points_left, MPI_INTEGER8, partner_mpi_rank, 42
     &          , comm, err)
            call MPI_SEND(this%weights(this%local_num_ids+points_sent+1
     &                  : this%nblocks)
     &          , points_left, MPI_REAL4, partner_mpi_rank, 43
     &          , comm, err)
            call MPI_RECV(nn_p, 1, MPI_INTEGER4, partner_mpi_rank, 44
     &          , comm, MPI_STATUS_IGNORE, err)
            nn = nn_p(1)
!---- Update import/export arrays
            if( nn > 0 )then
              do n = points_sent+1,points_sent+nn
                this%export_proc_ids(n) = partner_mpi_rank_g(1)
              end do
              points_sent = points_sent + nn
              points_left = points_left - nn
            end if
          else
            call MPI_PROBE(partner_mpi_rank, 42, comm
     &          , mpi_stat, err)
            call MPI_GET_COUNT(mpi_stat, MPI_INTEGER8, points_recv, err)
            allocate(points_buf(points_recv))
            allocate(weight_buf(points_recv))
            call MPI_RECV(points_buf, points_recv, MPI_INTEGER8
     &          , partner_mpi_rank
     &          , 42, comm, MPI_STATUS_IGNORE, err)
            call MPI_RECV(weight_buf, points_recv, MPI_REAL4
     &          , partner_mpi_rank
     &          , 43, comm, MPI_STATUS_IGNORE, err)
!---- Check if we have room
            if( cum_time < avg_time .and. points_recv > 0 )then
              do n = 1, points_recv
                imported_time = imported_time+weight_buf(n)
                cum_time = cum_time + weight_buf(n)
                if( cum_time > avg_time )then
                  exit
                end if
              end do
              if( n > points_recv) n = points_recv
              allocate(import_ids_temp(imported_points+n))
              allocate(import_proc_temp(imported_points+n))
              if( imported_points > 0 )then
                import_ids_temp(1:imported_points) = this%import_ids
              import_proc_temp(1:imported_points) = this%import_proc_ids
              end if
              call move_alloc(from=import_ids_temp, to=this%import_ids)
          call move_alloc(from=import_proc_temp,to=this%import_proc_ids)
              this% import_ids(imported_points+1:imported_points+n)
     &            = points_buf(1:n)
              this% import_proc_ids(imported_points+1:imported_points+n)
     &             = partner_mpi_rank_g(1)
            else
              n = 0
            end if
            imported_points = imported_points + n
            deallocate(points_buf)
            deallocate(weight_buf)
            n_p = n
            call MPI_SEND(n_p, 1, MPI_INTEGER4, partner_mpi_rank, 44
     &          , comm, err)
          end if
        end do

        this% import_num_ids = imported_points

!---- cleanup
        deallocate(all_rank)
        deallocate(all_li)
      end subroutine

      subroutine PARTITION_SORT_SETUP ( this, cum_time, avg_time
     &                                , cur_li, points_left)
        implicit none
        class(t_loadbalancer), intent(inout)    :: this
        real(real32)                            :: local_time(1)
        real(real32), intent(out)               :: avg_time(1)
        real(real32), intent(out)               :: cur_li(1)
        real(real32), intent(out)               :: cum_time
        integer(int32), intent(out)             :: points_left
        integer(int32)                          :: n, nn, err
        cum_time = 0.

!---- Deallocated what we are going to allocate
        if( allocated(this% local_ids)) deallocate(this%local_ids)
        if( allocated(this% export_ids)) deallocate(this%export_ids)
        if( allocated(this% export_proc_ids))
     &      deallocate(this%export_proc_ids)
        if( allocated(this% import_ids)) deallocate(this%import_ids)
        if( allocated(this% import_proc_ids))
     &      deallocate(this%import_proc_ids)

!---- First get the total time of local blocks
        local_time(1) = sum(this % weights)
        call MPI_ALLREDUCE(local_time, avg_time, 1, MPI_REAL4, MPI_SUM
     &                    , this%comm, err)
#ifdef DEBUG_CHECKS
        call ASSERT(err == MPI_SUCCESS
     &        , __FILE__
     &        , __LINE__
     &        , "AllReduce Failed")
#endif
        avg_time = avg_time / this% mpi_nnode

!---- Then we know what we should keep and what we should send away
        do n = 1, this%nblocks
          cum_time = cum_time + this%weights(n)
          if( cum_time > avg_time(1) )then
            exit
          end if
        end do
        if( n > this%nblocks) n = this%nblocks ! Happens when we complete the loop

!---- Assign the local blocks
        this% local_num_ids = n
        allocate(this%local_ids(n))
        do nn = 1, n
          this%local_ids(nn) = nn
        end do

!---- Assign if we have to export
        this% export_num_ids = this%nblocks-n
        allocate(this%export_ids(this%export_num_ids))
        allocate(this%export_proc_ids(this%export_num_ids))
        do nn = 1, this% export_num_ids
          this%export_ids(nn) = nn+n
          this%export_proc_ids(nn) = -1
        end do

!---- Start with zero imports
        this%import_num_ids = 0
        allocate(this%import_ids(0))
        allocate(this%import_proc_ids(0))

        points_left = this%export_num_ids

!---- Get our load imbalance
        cur_li(1)= (sum(this%weights)
     &         - avg_time(1))
     &         / avg_time(1)

!---- Create node local comm
      if( comm_local == MPI_COMM_NULL )then
        call MPI_COMM_SPLIT_TYPE ( this%comm, MPI_COMM_TYPE_SHARED
     &      , this% mpi_rank, MPI_INFO_NULL, comm_local, err )
#ifdef DEBUG_CHECKS
        call ASSERT(err == MPI_SUCCESS
     &        , __FILE__
     &        , __LINE__
     &        , "SPLIT Failed")
#endif
        call MPI_COMM_RANK(comm_local, whoami_local, err)
        call MPI_COMM_SIZE(comm_local, nnode_local, err)
        call MPI_COMM_GROUP(comm_local,group_local,err)
#ifdef DEBUG_CHECKS
        call ASSERT(err == MPI_SUCCESS
     &        , __FILE__
     &        , __LINE__
     &        , "COMM_GROUP Failed")
#endif
        call MPI_COMM_GROUP(this%comm,group_global,err)
#ifdef DEBUG_CHECKS
        call ASSERT(err == MPI_SUCCESS
     &        , __FILE__
     &        , __LINE__
     &        , "COMM GROUP Failed")
#endif
      end if

      end subroutine

      subroutine PARTITION_SORT ( this)
        implicit none
        class(t_loadbalancer), intent(inout)    :: this
        real(real32)                            :: avg_time(1)
        real(real32)                            :: cur_li(1)
        real(real32)                            :: cum_time
        real(real32)                            :: imported_time
        integer(int32)                          :: points_left
        integer(int32)                          :: points_sent
        integer(int32)                          :: n 
        integer(int64), allocatable             :: local_ids_temp(:)

        call PARTITION_SORT_SETUP( this,  cum_time, avg_time, cur_li
     &                           , points_left)
        imported_time = 0.
        points_sent = 0

!---- partition over every process
        call PARTITION_SORT_ALGORITHM( this, this%comm, this% mpi_nnode
     &                               , this% mpi_rank, cur_li(1)
     &                               , points_left
     &                               , points_sent
     &                               , cum_time, imported_time
     &                               , avg_time(1) )

!---- Assign our stray blocks to ourselves
        if( points_left > 0 )then
          allocate(local_ids_temp(this%local_num_ids+points_left))
          local_ids_temp(1:this%local_num_ids) = this%local_ids
          do n = 1,points_left
            local_ids_temp(this%local_num_ids+n)
     &          = (this%nblocks-points_left)+n
          end do
          this% local_num_ids = this%local_num_ids + points_left
          call move_alloc(from=local_ids_temp, to=this%local_ids)
          this% export_num_ids = this % export_num_ids - points_left
        end if

!---- cleanup
        call this%REALLOCATE_BUFFERS()
        call this%CREATE_COMMUNICATION()

      end subroutine

      subroutine PARTITION_SORT2 ( this)
        implicit none
        class(t_loadbalancer), intent(inout)    :: this
        real(real32)                            :: avg_time(1)
        real(real32)                            :: cur_li(1)
        real(real32)                            :: cum_time
        real(real32)                            :: imported_time
        integer(int32)                          :: points_left
        integer(int32)                          :: points_sent
        integer(int32)                          :: n
        integer(int64), allocatable             :: local_ids_temp(:)

        call PARTITION_SORT_SETUP( this,  cum_time, avg_time, cur_li
     &                           , points_left )
        imported_time = 0
        points_sent = 0
!---- First partition on a single node
        call PARTITION_SORT_ALGORITHM( this, comm_local, nnode_local
     &                            , whoami_local, cur_li(1), points_left
     &                               , points_sent
     &                               , cum_time, imported_time
     &                               , avg_time(1) )
!---- Get our load imbalance
        cur_li(1)= (sum(this%weights(1:this%nblocks-points_sent))
     &         + imported_time
     &         - avg_time(1))
     &         / avg_time(1)

!---- Then partition the residual over every process
        call PARTITION_SORT_ALGORITHM( this, this%comm, this% mpi_nnode
     &                               , this% mpi_rank, cur_li(1)
     &                               , points_left
     &                               , points_sent
     &                               , cum_time, imported_time
     &                               , avg_time(1) )

!---- Assign our stray blocks to ourselves
        if( points_left > 0 )then
          allocate(local_ids_temp(this%local_num_ids+points_left))
          local_ids_temp(1:this%local_num_ids) = this%local_ids
          do n = 1,points_left
            local_ids_temp(this%local_num_ids+n)
     &          = (this%nblocks-points_left)+n
          end do
          this% local_num_ids = this%local_num_ids + points_left
          call move_alloc(from=local_ids_temp, to=this%local_ids)
          this% export_num_ids = this % export_num_ids - points_left
        end if

!---- cleanup
        call this%REALLOCATE_BUFFERS()
        call this%CREATE_COMMUNICATION()

      end subroutine

      subroutine PARTITION_GREEDY ( this )
        implicit none
        class(t_loadbalancer), intent(inout)    :: this
        real(real32)                            :: local_time(1)
        real(real32)                            :: avg_time(1)
        real(real32)                            :: cur_li(1)
        real(real32)                            :: prev_li
        real(real32)                            :: temp
        integer(int32)                          :: err
        real(real32)                            :: cum_time
        integer(int32)                          :: n
        integer(int32)                          :: nn, next_proc, x
        integer(int32)                          :: nn_p(1),n_p(1)
        integer(int32)                          :: prev_proc
        integer(int32)                          :: points_left
        integer(int32)                          :: points_send
        integer(int32)                          :: points_recv
        integer(int32)                          :: imported_points
        integer(int64), allocatable             :: local_ids_temp(:)
        integer(int64), allocatable             :: import_ids_temp(:)
        integer(int32), allocatable             :: import_proc_temp(:)
        logical                                 :: finished
        type(MPI_REQUEST)                       :: send_req(2)
        integer(int64), allocatable             :: points_buf(:)
        real(real32), allocatable               :: weight_buf(:)
        real(real32), allocatable               :: all_li(:)
        type(MPI_STATUS)                        :: mpi_stat

        cum_time = 0.
        finished = .false.

!---- Deallocated what we are going to allocate
        if( allocated(this% local_ids)) deallocate(this%local_ids)
        if( allocated(this% export_ids)) deallocate(this%export_ids)
        if( allocated(this% export_proc_ids))
     &      deallocate(this%export_proc_ids)
        if( allocated(this% import_ids)) deallocate(this%import_ids)
        if( allocated(this% import_proc_ids))
     &      deallocate(this%import_proc_ids)


!---- First get the total time of local blocks
        local_time(1) = sum(this % weights)
        call MPI_ALLREDUCE( local_time, avg_time, 1, MPI_REAL4, MPI_SUM
     &                    , this%comm, err)
#ifdef DEBUG_CHECKS
        call ASSERT(err == MPI_SUCCESS
     &        , __FILE__
     &        , __LINE__
     &        , "AllReduce Failed")
#endif
        avg_time = avg_time / this% mpi_nnode

        allocate(all_li(this% mpi_nnode))

!---- Get our load imbalance
        cur_li(1)= (local_time(1)
     &         - avg_time(1))
     &         / avg_time(1)

!---- Communicate our current surplus/deficit to everyone
        call MPI_ALLGATHER( cur_li,1,MPI_REAL4,all_li,1,MPI_REAL4
     &               , this%comm, err)

        prev_li = maxval(all_li)

!---- Then we know what we should keep and what we should send away
        do n = 1, this%nblocks
          cum_time = cum_time + this%weights(n)
          if( cum_time > avg_time(1) )then
            exit
          end if
        end do
        if( n > this%nblocks) n = this%nblocks ! Happens when we complete the loop

        this% local_num_ids = n
        allocate(this%local_ids(n))
        do nn = 1, n
          this%local_ids(nn) = nn
        end do

        this% export_num_ids = this%nblocks-n
        allocate(this%export_ids(this%export_num_ids))
        allocate(this%export_proc_ids(this%export_num_ids))
        do nn = 1, this% export_num_ids
          this%export_ids(nn) = nn+n
          this%export_proc_ids(nn) = -1
        end do

        allocate(this%import_ids(0))
        allocate(this%import_proc_ids(0))

!---- Send/Recv in Circle mode, Check if we are done after each iteration
        points_left = this%export_num_ids
        points_send = 0
        imported_points = 0
        next_proc = mod((this% mpi_rank + 1),this% mpi_nnode)
        prev_proc = mod((this% mpi_rank-1)
     &       + this%mpi_nnode, this%mpi_nnode)
        do x = 1, min(this% max_it,this% mpi_nnode-1)

!---- Update Load imbalances (for next iteration)
          do n = 1, this% mpi_nnode
            nn = mod(n + x - 1,this% mpi_nnode) + 1
            if( all_li(n) <= 0. )then
              cycle
            end if
            if( (all_li(n) < 0.) .eqv.(all_li(nn)<0.) )then
              cycle
            end if
            temp = min(abs(all_li(n)), abs(all_li(nn)))

            all_li(n) = all_li(n) - temp
            all_li(nn) = all_li(nn) + temp
          end do

!---- Check if we are done
          if( prev_li == 0. )EXIT
          if( maxval(all_li) < this% max_abs_li  )then
            exit
          end if
          prev_li = maxval(all_li)

          associate(buf => this%export_ids(points_send+1
     &                   : this%export_num_ids))
          call MPI_ISEND( buf
     &        , points_left, MPI_INTEGER8, next_proc, 42
     &        , this%comm, send_req(1), err)
         associate(buf2 => this%weights(this%local_num_ids+points_send+1
     &                  : this%nblocks))

          call MPI_ISEND(buf2
     &        , points_left, MPI_REAL4, next_proc, 43
     &        , this%comm, send_req(2), err)
          call MPI_PROBE(prev_proc, 42, this%comm, mpi_stat, err)
          call MPI_GET_COUNT(mpi_stat, MPI_INTEGER8, points_recv, err)
          allocate(points_buf(points_recv))
          allocate(weight_buf(points_recv))
          call MPI_RECV(points_buf, points_recv, MPI_INTEGER8, prev_proc
     &                 , 42, this%comm, MPI_STATUS_IGNORE, err)
          call MPI_RECV(weight_buf, points_recv, MPI_REAL4, prev_proc
     &                 , 43, this%comm, MPI_STATUS_IGNORE, err)
          call MPI_WAITALL(2,send_req,MPI_STATUSES_IGNORE, err)
          end associate
          end associate

!---- Check if we have room
          if( cum_time < avg_time(1) )then
            do n = 1, points_recv
              cum_time = cum_time + weight_buf(n)
              if( cum_time > avg_time(1) )then
                exit
              end if
            end do
            if( n > points_recv) n = points_recv
            allocate(import_ids_temp(imported_points+n))
            allocate(import_proc_temp(imported_points+n))
            if( imported_points > 0 )then
              import_ids_temp(1:imported_points) = this%import_ids
              import_proc_temp(1:imported_points) = this%import_proc_ids
            end if
            call move_alloc(from=import_ids_temp, to=this%import_ids)
          call move_alloc(from=import_proc_temp,to=this%import_proc_ids)

            do nn = 1, n
              this% import_ids(imported_points+nn) = points_buf(nn)
              this% import_proc_ids(imported_points+nn) = prev_proc
            end do
          else
            n = 0
          end if
          imported_points = imported_points + n

          deallocate(points_buf)
          deallocate(weight_buf)

          n_p(1) = n
          call MPI_ISEND(n_p, 1, MPI_INTEGER4, prev_proc, 44
     &        , this%comm, send_req(1), err)
          call MPI_RECV(nn_p, 1, MPI_INTEGER4, next_proc, 44, this%comm
     &        , MPI_STATUS_IGNORE, err)
          nn = nn_p(1)
          call MPI_WAIT(send_req(1),MPI_STATUS_IGNORE, err)

          if( nn > 0 )then
            do n = points_send+1,points_send+nn
              this%export_proc_ids(n) = next_proc
            end do
            points_send = points_send + nn
            points_left = points_left - nn
          end if
#ifdef DEBUG_CHECKS
          call ASSERT(next_proc /= this% mpi_rank
     &          , "Unable to distribute all points over blocks")
#endif
          next_proc = mod((next_proc + 1),this% mpi_nnode)
          prev_proc = mod((prev_proc - 1)+this%mpi_nnode,this%mpi_nnode)
        end do
        this% import_num_ids = imported_points

!---- Assign our stray blocks to ourselves
        if( points_left > 0 )then
          allocate(local_ids_temp(this%local_num_ids+points_left))
          local_ids_temp(1:this%local_num_ids) = this%local_ids
          do n = 1,points_left
            local_ids_temp(this%local_num_ids+n)
     &          = (this%nblocks-points_left)+n
          end do
          this% local_num_ids = this%local_num_ids + points_left
          call move_alloc(from=local_ids_temp, to=this%local_ids)
          this% export_num_ids = this % export_num_ids - points_left
        end if

        call this%REALLOCATE_BUFFERS()
        call this%CREATE_COMMUNICATION()

      end subroutine PARTITION_GREEDY

#ifdef ENABLE_ZOLTAN
      function ZOLT_NUM_OBJ(indata, ierr) result(n_obj)
        use ZOLTAN
        implicit none
        integer(ZOLTAN_INT),dimension(*),intent(in) :: indata
        integer(ZOLTAN_INT), intent(out)       :: ierr
        integer(ZOLTAN_INT)                    :: n_obj
        type(c_ptr)                            :: temp
        type(t_loadbalancer), pointer         :: this
        temp = transfer(indata(1:zolt_int_pointer_size+1), temp)
        call c_f_pointer(temp, this)
        n_obj = this%nblocks
        ierr = ZOLTAN_OK
      end function

      subroutine ZOLT_OBJ_LIST( indata, num_gid_entries, num_lid_entries
     &                      , global_ids, local_ids
     &                      , wgt_dim, obj_wgts, ierr)
        use ZOLTAN
        implicit none
        integer(ZOLTAN_INT), intent(in),dimension(*) :: indata
        integer(ZOLTAN_INT), intent(in)        :: num_gid_entries
        integer(ZOLTAN_INT), intent(in)        :: num_lid_entries
        integer(ZOLTAN_INT), intent(out), dimension(*):: global_ids
        integer(ZOLTAN_INT),  dimension(*):: local_ids
        integer(ZOLTAN_INT), intent(in)        :: wgt_dim
        real(ZOLTAN_FLOAT),  intent(out), dimension(*):: obj_wgts
        integer(ZOLTAN_INT), intent(out)       :: ierr
        integer(int32)                         :: n, nn
        type(c_ptr)                            :: temp
        type(t_loadbalancer), pointer         :: this
        temp = transfer(indata(1:zolt_int_pointer_size+1), temp)
        call c_f_pointer(temp, this)

        do n = 1, this%nblocks
          global_ids(1 + (n-1)*num_gid_entries) = this%offset + n
        end do
        obj_wgts(1:this%nblocks) = this%weights(1:this%nblocks)

        ierr = ZOLTAN_OK
      end subroutine
      subroutine ZOLT_HG_SIZE_CS( indata,num_lists, num_pins, format
     &                          , ierr)
        use ZOLTAN
        implicit none
        integer(ZOLTAN_INT), intent(in),dimension(*) :: indata
        integer(Zoltan_INT), intent(out)       :: num_lists
        integer(Zoltan_INT), intent(out)       :: num_pins
        integer(Zoltan_INT), intent(out)       :: format
        integer(Zoltan_INT), intent(out)       :: ierr
        type(c_ptr)                            :: temp
        type(t_loadbalancer), pointer         :: this
        temp = transfer(indata(1:zolt_int_pointer_size+1), temp)
        call c_f_pointer(temp, this)

        num_lists = 1 !TODO add node info
        num_pins = this%nblocks
        format = ZOLTAN_COMPRESSED_EDGE
        ierr = ZOLTAN_OK
      end subroutine
      subroutine ZOLT_HG_CS( indata, num_gid_entries, num_vtx_edge
     &                     , num_pins, format, vtxedge_GID
     &                     , vtxedge_ptr, pin_GID, ierr)
        use ZOLTAN
        implicit none
        integer(ZOLTAN_INT), INTENT(IN),dimension(*) :: indata
        integer(ZOLTAN_INT), intent(in)        :: num_gid_entries
        integer(ZOLTAN_INT), intent(in)        :: num_vtx_edge
        integer(ZOLTAN_INT), intent(in)        :: num_pins
        integer(ZOLTAN_INT), intent(in)        :: format
        integer(ZOLTAN_INT), intent(out) ,dimension(*)  :: vtxedge_GID
        integer(ZOLTAN_INT), intent(out) ,dimension(*)  :: vtxedge_ptr
        integer(ZOLTAN_INT), intent(out)  ,dimension(*) :: pin_GID
        integer(ZOLTAN_INT), intent(out)       :: ierr
        integer(int32)                         :: n
        type(c_ptr)                            :: temp
        type(t_loadbalancer), pointer          :: this
        temp = transfer(indata(1:zolt_int_pointer_size+1), temp)
        call c_f_pointer(temp, this)

        vtxedge_GID(1) = 1
        vtxedge_ptr(1) = 0
        do n = 1, this%nblocks
          pin_GID(n) = this%offset+n
        end do
        ierr = ZOLTAN_OK
      end subroutine

      subroutine PARTITION_ZOLTAN ( this )
        use ZOLTAN, only : ZOLTAN_INT, ZOLTAN_FLOAT, ZOLTAN_SET_PARAM
     &                   , ZOLTAN_LB_FREE_PART, ZOLTAN_NUM_OBJ_FN_TYPE
     &                   , ZOLTAN_CREATE, ZOLTAN_LB_PARTITION
     &                   , ZOLTAN_HG_CS_FN_TYPE
     &                   , ZOLTAN_HG_SIZE_CS_FN_TYPE
     &                   , ZOLTAN_SET_FN, ZOLTAN_OBJ_LIST_FN_TYPE
     &                   , ZOLTAN_HG_CS_FN_TYPE, ZOLTAN_OK
     &                   , ZOLTAN_INITIALIZE, ZOLTAN_DESTROY
        implicit none
        class(t_loadbalancer), intent(inout),pointer   :: this
        integer(ZOLTAN_INT)                    :: error
        real(ZOLTAN_FLOAT)                     :: version
        integer(ZOLTAN_INT),pointer            :: addr_ints(:)
        type(c_ptr)                            :: c_addr

        integer(int32)                         :: n,nn,nnn
        logical                                :: changed
        integer(ZOLTAN_INT)                    :: nge, nle
        integer(ZOLTAN_INT)                    :: num_import, num_export
        integer(ZOLTAN_INT),pointer,dimension(:):: import_global_ids
     &      => null()
        integer(ZOLTAN_INT),pointer,dimension(:):: export_global_ids
     &      => null()
        integer(ZOLTAN_INT),pointer,dimension(:):: import_local_ids
     &      => null()
        integer(ZOLTAN_INT),pointer,dimension(:):: export_local_ids
     &      => null()
        integer(ZOLTAN_INT),pointer,dimension(:):: import_procs
     &      => null()
        integer(ZOLTAN_INT),pointer,dimension(:):: export_procs
     &      => null()
        integer(ZOLTAN_INT),pointer,dimension(:):: import_to_part
     &      => null()
        integer(ZOLTAN_INT),pointer,dimension(:):: export_to_part
     &      => null()
        type(t_loadbalancer), pointer           :: this_np

        this_np => this

      call MPI_BARRIER(this%comm, mpi_err)

        if( .not.zoltan_initialized )then
          error = ZOLTAN_INITIALIZE(version)
          call ASSERT( error == ZOLTAN_OK
     &        , __FILE__
     &        , __LINE__
     &        , "Error initializing ZOLTAN")
          zoltan_initialized = .true.
        end if
          this%zoltan => ZOLTAN_CREATE(this%comm)
          error = ZOLTAN_SET_PARAM(this%zoltan,"DEBUG_LEVEL","0")
          error = ZOLTAN_SET_PARAM(this%zoltan,"CHECK_HYPERGRAPH","0")
          error = ZOLTAN_SET_PARAM(this%zoltan,"LB_METHOD","HYPERGRAPH")
          error = ZOLTAN_SET_PARAM(this%zoltan,"NUM_LID_ENTRIES","0")
          error = ZOLTAN_SET_PARAM(this%zoltan,"NUM_GID_ENTRIES","1")
          error = ZOLTAN_SET_PARAM(this%zoltan,"OBJ_WEIGHT_DIM","1")
          error = ZOLTAN_SET_PARAM(this%zoltan,"LB_APPROACH","REFINE")
          error = ZOLTAN_SET_PARAM(this%zoltan,"PHG_REFINEMENT_QUALITY"
     &                                        ,"0.5")

          ! This is in the order of the weights precision, So either ns
          ! or µs, assume its in between, discourage large communication
          ! a bit
       error=ZOLTAN_SET_PARAM(this%zoltan,"PHG_REPART_MULTIPLIER","0")
          call ASSERT( error == ZOLTAN_OK
     &        , __FILE__
     &        , __LINE__
     &        , "Error Setting ZOLTAN parameters")
!---- For the query functions we can overload int lists with the pointer
!---- To this, to avoid having to manipulate zoltan while building it
          c_addr = c_loc(this_np)
          zolt_int_pointer_size = sizeof(c_addr)/sizeof(error)
          if( mod(sizeof(c_addr),sizeof(error)) /= 0 )then
            zolt_int_pointer_size = zolt_int_pointer_size + 1
          end if
          allocate(addr_ints(zolt_int_pointer_size))
          addr_ints = transfer(c_addr,addr_ints,zolt_int_pointer_size)
          error = ZOLTAN_SET_FN(this%zoltan
     &          , ZOLTAN_NUM_OBJ_FN_TYPE, ZOLT_NUM_OBJ, addr_ints)
          error = ZOLTAN_SET_FN(this%zoltan
     &          , ZOLTAN_OBJ_LIST_FN_TYPE, ZOLT_OBJ_LIST, addr_ints)
          error = ZOLTAN_SET_FN(this%zoltan
     &          , ZOLTAN_HG_SIZE_CS_FN_TYPE, ZOLT_HG_SIZE_CS, addr_ints)
          error = ZOLTAN_SET_FN(this%zoltan
     &          , ZOLTAN_HG_CS_FN_TYPE, ZOLT_HG_CS, addr_ints)

        error = ZOLTAN_LB_PARTITION(this%zoltan, changed
     &      , nge, nle, num_import, import_global_ids, import_local_ids
     &      , import_procs, import_to_part
     &      , num_export, export_global_ids, export_local_ids
     &      , export_procs, export_to_part)
        call ASSERT( error == ZOLTAN_OK
     &      , __FILE__
     &      , __LINE__
     &      , "Error running Zoltan Partition")

        if( allocated(this% local_ids)) deallocate(this%local_ids)
        if( allocated(this% export_ids)) deallocate(this%export_ids)
        if( allocated(this% export_proc_ids))
     &      deallocate(this%export_proc_ids)
        if( allocated(this% import_ids)) deallocate(this%import_ids)
        if( allocated(this% import_proc_ids))
     &      deallocate(this%import_proc_ids)

        this%local_num_ids = this%nblocks - num_export
        this%export_num_ids = num_export
        this%import_num_ids = num_import

        allocate(this%local_ids(this%local_num_ids))
        allocate(this%export_ids(this%export_num_ids))
        allocate(this%export_proc_ids(this%export_num_ids))
        allocate(this%import_ids(this%import_num_ids))
        allocate(this%import_proc_ids(this%import_num_ids))

        if( num_export /= 0 )then
          this%export_proc_ids = export_procs
          this%export_ids = export_global_ids
          this%export_ids = this%export_ids - this%offset
        error = ZOLTAN_LB_FREE_PART( export_global_ids, export_local_ids
     &                             , export_procs, export_to_part )
        end if

        if( num_import /= 0 )then
          this%import_proc_ids =  import_procs
          this%import_ids = import_global_ids
        error = ZOLTAN_LB_FREE_PART( import_global_ids, import_local_ids
     &                             , import_procs, import_to_part )
        end if

        nn = 1
        nnn = 1
        do n = 1, this%nblocks
          if( nn <= size(this%export_ids) )then
            if( this%export_ids(nn) == n )then
              nn = nn + 1
              cycle
            end if
          end if
          this%local_ids(nnn) = n
          nnn = nnn+1
        end do

        call MPI_BARRIER(this%comm,mpi_err)
        call ZOLTAN_DESTROY(this%zoltan)
        call MPI_BARRIER(this%comm,mpi_err)
        call this%REALLOCATE_BUFFERS()
        call this%CREATE_COMMUNICATION()

      end subroutine
#endif

      end module
