! single-process stubs that would be actual parallel operations 
! in an MPI implementation if you want real versions of these, 
! link them in from WRF

MODULE module_dm

  CONTAINS

REAL FUNCTION wrf_dm_max_real ( inval )
   IMPLICIT NONE
   REAL inval
   wrf_dm_max_real = inval
END FUNCTION wrf_dm_max_real

END MODULE module_dm

SUBROUTINE wrf_dm_bcast_integer( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
   INTEGER  buf(*)
   RETURN
END SUBROUTINE wrf_dm_bcast_integer

SUBROUTINE wrf_dm_bcast_real( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
   REAL  buf(*)
   RETURN
END SUBROUTINE wrf_dm_bcast_real

SUBROUTINE wrf_dm_bcast_double( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
   REAL  buf(*)
   RETURN
END SUBROUTINE wrf_dm_bcast_double

LOGICAL FUNCTION wrf_dm_on_monitor()
  wrf_dm_on_monitor = .true.
END FUNCTION wrf_dm_on_monitor

