!=======================================================================
SUBROUTINE fastrun( )

USE                             DOFs
USE                             Features
USE                             Output
USE                             SimCont
USE                             FAST_IO_Subs       ! WrOutHdr(),  SimStatus(), WrOutput()
USE                             FASTsubs           ! Initialize(), TimeMarch()
USE                             NOISE              ! PredictNoise(), WriteAveSpecOut()

USE                             FASTSUBITERATE 


IMPLICIT                        NONE

! Local variables.
REAL(ReKi)                   :: TiLstPrn  = 0.0                                 ! The time of the last print.
INTEGER                      :: I 



! loop over as many as CFD_tstep/FAST_tstep
DO I = 1, FAST_subIter

  ! Call predictor-corrector routine:
  CALL Solver

  ! Make sure the rotor azimuth is not greater or equal to 360 degrees:
  IF ( ( Q(DOF_GeAz,IC(1)) + Q(DOF_DrTr,IC(1)) ) >= TwoPi )  THEN
     Q(DOF_GeAz,IC(1)) = Q(DOF_GeAz,IC(1)) - TwoPi
  ENDIF

  ! Advance time:
  Step  = Step + 1
  ZTime = Step*DT

  ! Compute all of the output channels and fill in the OutData() array:
  CALL CalcOuts

  ! Check to see if we should output data this time step:
  IF ( ZTime >= TStart )  THEN
    IF ( CompNoise                 )  CALL PredictNoise
    IF ( MOD( Step, DecFact ) == 0 )  CALL WrOutput
  ENDIF

  ! Display simulation status every SttsTime-seconds:
  !   IF ( ZTime - TiLstPrn >= SttsTime )  THEN
  !      TiLstPrn = ZTime
  !      CALL SimStatus
  !   ENDIF

ENDDO


END SUBROUTINE fastrun
!=======================================================================
