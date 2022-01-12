      module LOADBALANCER_OUTPUT

!   - formatting
      character (len=24) ,parameter, private
     &    :: fn  = "('         - ',A,': ',A)"
     &     , fw  = "('WARNING  - ',A,': ',A)"
     &     , fi  = "('INFO     - ',A,': ',A)"
     &     , fd  = "('DEBUG    - ',A,': ',A)"
     &     , fe  = "('ERROR    - ',A,': ',A)"
      character (len=10) ,parameter,private :: ft  = '          '
      character (len=50) ,parameter,private :: fl  =
     &          '--------------------------------------------------'
      character (len=50) ,parameter,private :: fdl =
     &          '=================================================='

!   - Output file handle
      integer                                    :: fhandle = -1

!   - For comms
      type t_tot_data
        integer :: id
        integer :: local_num_ids
        integer :: export_num_ids
        integer :: import_num_ids
        real(4) :: total_weight
        real(4) :: local_weight
        real(4) :: export_weight
      end type

      contains

      subroutine OPEN_LOGFILE
        implicit none
        character(32)                        :: filename
        integer                              :: err
        write(filename,'(A,I2,A)') "loadbalance.info"
        open( newunit=fhandle, file = filename,
     &        action = 'write', iostat = err )
        write(fhandle,'(A)')fdl
        write(fhandle,'(A)')"       Loadbalancing Library  Info:     "
        write(fhandle,'(A)')fdl
        write(fhandle,'(A)')"  Brought to you by: "
        write(fhandle,'(A)')"  - Technical University Delft and "
        write(fhandle,'(A)')"  - Netherlands Escience Center "
        write(fhandle,'(A)')fl
      end subroutine

      subroutine USED_LOADBALANCING_INFO(lb)
        use MPI_F08
        use LOADBALANCER, only                : t_loadbalancer
        implicit none
        type(t_loadbalancer)                 :: lb

        if( lb%mpi_rank /= 0 )RETURN

        if( fhandle == -1 )then
          call OPEN_LOGFILE
        end if

!     Print our info if associated


        if( associated(lb%lb_info) )then
          associate(info => lb%lb_info)
          write(fhandle,'(A)') ""
          write(fhandle,'("Loadbalancer: ",A)') info%name
          write(fhandle,'(A)')  fl
          if( associated(lb%export_data) )then
            write(fhandle,'(A,A)')"EXPORT_DATA     => "
     &                                        ,info%export_data
          else
            write(fhandle,'(A)')
     &          "EXPORT_DATA     => Not associated"
          end if
          if( associated(lb%import_data) )then
            write(fhandle,'(A,A)')"IMPORT_DATA     => "
     &                                        ,info%import_data
          else
            write(fhandle,'(A)')
     &          "IMPORT_DATA     => Not associated"
          end if
          if( associated(lb%export_result) )then
            write(fhandle,'(A,A)')"EXPORT_RESULT   => "
     &                                        ,info%export_result
          else
            write(fhandle,'(A)')
     &          "EXPORT_RESULT   => Not associated"
          end if
          if( associated(lb%import_result) )then
            write(fhandle,'(A,A)')"IMPORT_RESULT   => "
     &                                        ,info%import_result
          else
            write(fhandle,'(A)')
     &          "IMPORT_RESULT   => Not associated"
          end if
          if( associated(lb%partition) )then
            write(fhandle,'(A,A)')"PARTITION       => "
     &                                        ,info%partition
          else
            write(fhandle,'(A)')
     &          "PARTITION       => Not associated"
          end if
          end associate

!---- Static info
          write(fhandle,'(A,I20)') "Total Number of blocks = "
     &        , lb%total_nblocks

       end if
      end subroutine
      subroutine OUTPUT_PARTITIONING(lb, timestep)
        use LOADBALANCER_DEBUG, only          : ASSERT
        use MPI_F08
        use LOADBALANCER, only                : t_loadbalancer
        implicit none
        type(t_loadbalancer)                 :: lb
        integer                              :: timestep
        integer                              :: err, n, nn
        integer                              :: max_block
        integer                              :: recv_count
        integer                              :: total_local_size
        integer, pointer, contiguous         :: buf_int(:)
        real(4), pointer, contiguous         :: buf_real(:)
!---- Root process writes this, receiving per process would require less memory
!---- But is probably slower
        type(t_tot_data), pointer,contiguous  :: totals(:)
        type(t_tot_data)                      :: total_local(1)


        if( .not. lb%lb_log )RETURN


        if( lb%mpi_rank == 0 )then
          allocate(totals(lb%mpi_nnode),stat=err)
          call ASSERT(err == 0, "Error in allocation")

          if( fhandle == -1 )then
            call OPEN_LOGFILE
          end if
          write(fhandle,'(A)') ""
          write(fhandle,'("Loadbalancer : ",A
     &,"Partitioning on timestep:",I20)') lb%lb_info%name, timestep
          write(fhandle,'(A)')  fl
        end if

        total_local%id = lb%mpi_rank
        total_local%local_num_ids = lb%local_num_ids
        total_local%export_num_ids = lb% export_num_ids
        total_local%import_num_ids = lb% import_num_ids
        total_local%total_weight = sum(lb% weights)
        total_local%export_weight = 0
        do n = 1, lb%export_num_ids
        total_local%export_weight = total_local%export_weight
     &                            + lb%weights(lb%export_ids(n))
        end do
        total_local%local_weight = 0
        do n = 1, lb%local_num_ids
        total_local%local_weight = total_local%local_weight
     &                            + lb%weights(lb%local_ids(n))
        end do
        total_local_size = sizeof(total_local)
        if( lb%mpi_rank == 0 )then
          call MPI_GATHER(
     &          total_local, total_local_size, MPI_BYTE
     &        , totals, total_local_size, MPI_BYTE
     &        , 0, lb%comm, err)
        else
!---- Fortran complains about recv event when it is not used
          call MPI_GATHER(
     &          total_local, total_local_size, MPI_BYTE
     &        , total_local, total_local_size, MPI_BYTE
     &        , 0, lb%comm, err)
        end if
#ifdef DEBUG_CHECKS
        call ASSERT(err == MPI_SUCCESS
     &        , __FILE__
     &        , __LINE__
     &        , "GATHER Failed")
#endif

        if( lb%mpi_rank == 0 )then
!---- CSV like header
          write(fhandle,'(A)') "id, num_local_blocks"
     &        //", num_exported_blocks, num_imported_blocks, tot_weight"
     &        //", local_weight, export_weight"
          do n = 1, lb%mpi_nnode
          write(fhandle,'(4(I12,",")2(E12.4E3,",")E12.4E3)')
     &            totals(n)%id
     &          , totals(n)%local_num_ids
     &          , totals(n)%export_num_ids
     &          , totals(n)%import_num_ids
     &          , totals(n)%total_weight
     &          , totals(n)%local_weight
     &          , totals(n)%export_weight


          end do
          write(fhandle,'(A)')  fl
          deallocate(totals)
        end if

        if( .not. lb%lb_log_detailed )RETURN
        max_block = 0
        do n = 2, size(lb%vtxdist)
          if( lb%vtxdist(n)-lb%vtxdist(n-1) > max_block )then
            max_block = lb%vtxdist(n)-lb%vtxdist(n-1)
          end if
        end do
        allocate(buf_int(1:max_block))

        buf_int = lb%mpi_rank
        do n = 1, lb%export_num_ids
          buf_int(lb%export_ids(n)) = lb%export_proc_ids(n)
        end do
        if( lb%mpi_rank /= 0 )then
          call MPI_SEND( lb%weights, lb%nblocks, MPI_FLOAT
     &                 , 0, 32, lb%comm, err)
          call MPI_SEND( buf_int, lb%nblocks, MPI_INT
     &                 , 0, 198, lb%comm, err)
        end if
        if( lb%mpi_rank == 0 )then
          allocate(buf_real(max_block))
          write(fhandle,'(A)') "Detailed information per block:"
          write(fhandle,'(A)') "id, weight, source_proc, compute_proc"
!---- Write self
          do nn = 1, lb%nblocks
            write(fhandle,'(I10,",",E12.4E3,",",I10,",",I10)') nn
     &          , lb%weights(nn), 0, buf_int(nn)
          end do
          do n = 1, lb%mpi_nnode-1
            recv_count = lb%vtxdist(n+2)-lb%vtxdist(n+1)
            call MPI_RECV( buf_real, recv_count, MPI_FLOAT
     &                   , n, 32, lb%comm, MPI_STATUS_IGNORE, err)
            call MPI_RECV( buf_int, recv_count, MPI_INT
     &                   , n, 198, lb%comm,MPI_STATUS_IGNORE, err)
!---- Write others
            do nn = 1, recv_count
              write(fhandle,'(I10,",",E12.4E3,",",I10,",",I10)')
     &            nn+lb%vtxdist(n+1), buf_real(nn), n, buf_int(nn)
            end do
          end do
          deallocate(buf_real)
        end if

        call MPI_BARRIER(lb%comm, err)

        deallocate(buf_int)
      end subroutine

      end module
