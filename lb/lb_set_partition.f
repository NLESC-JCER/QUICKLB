      module LOADBALANCER_SET_PARTITION
      contains

      subroutine SET_PARTITION_ALGORITHM ( this , name, max_rel_li
     &                                   , max_abs_li, max_it)
        use LOADBALANCER_PARTITION
        use LOADBALANCER
        implicit none
        class(t_loadbalancer), intent(inout)   :: this
        character(len=20), intent(in) :: name
        real, intent(in) :: max_rel_li
        real, intent(in) :: max_abs_li
        integer, intent(in) :: max_it

        this% max_rel_li = max_rel_li
        this% max_abs_li = max_abs_li
        this% max_it = max_it

        if( name == "GREEDY" )then
          this%PARTITION => PARTITION_GREEDY
          this% lb_info % partition = "PARTITION_GREEDY"
        else if( name == "SORT" )then
          this%PARTITION => PARTITION_SORT
          this% lb_info % partition = "PARTITION_SORT"
        else if( name == "SORT2" )then
          this%PARTITION => PARTITION_SORT2
          this% lb_info % partition = "PARTITION_SORT2"
          this% max_it = max(1, this% max_it / 2)
#ifdef ENABLE_ZOLTAN
        else if( name == "ZOLTAN" )then
          this%PARTITION => PARTITION_ZOLTAN
          this% lb_info % partition = "PARTITION_ZOLTAN"
#endif
        else
          error stop "Partition Strategy :"//name//" not available"
        end if

      end subroutine
      end module
