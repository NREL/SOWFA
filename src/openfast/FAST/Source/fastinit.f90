!=======================================================================
SUBROUTINE fastinit(tstep , turbnum)

USE                             ADAMSInput
USE                             DOFs
USE                             General
USE                             InitCond
USE                             Linear
USE                             SimCont
USE                             NWTC_Library
USE                             AeroDyn
USE                             FAST_IO_Subs       ! Begin(), Input(), PrintSum(), RunTimes()
USE                             FASTsubs           ! Initialize(), TimeMarch()
USE                             FAST2ADAMSSubs     ! MakeAdm(), MakeACF(), MakeACF_Lin
USE                             FAST_Lin_Subs      ! CalcSteady(), Linearize()
USE                             HydroDyn
USE                             Noise

IMPLICIT                        NONE

! argument
REAL(ReKi) tstep
Integer turbnum

CALL DATE_AND_TIME ( Values=StrtTime )   ! Let's time the whole simulation

CALL CPU_TIME ( UsrTime1 )               ! Initial time (this zeros the start time when used as a MATLAB function)
   
CALL SetVersion

CALL NWTC_Init()     ! sets the pi constants

!CALL DispNVD()
! DispNVD prints out the following:
! Running FAST (v7.00.01a-bjj, 5-Nov-2010).
!  Linked with NWTC Subroutine Library (v1.03.00, 05-Nov-2010).

! Open and read input files, initialize global parameters.
CALL Begin

DT=tstep
CALL Input
DT=tstep

! Set up initial values for all degrees of freedom.
CALL Initialize

CALL WrOutHdr(turbnum)

! Start simulation.  Initialize the simulation status.
!CALL WrScr1 ( '' )
!CALL SimStatus

END SUBROUTINE fastinit
!=======================================================================
