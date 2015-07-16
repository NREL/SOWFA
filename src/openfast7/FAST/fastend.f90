!=======================================================================
SUBROUTINE fastend( )

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

INTEGER                      :: ErrStat

CALL FAST_Terminate( ErrStat )
CALL AD_Terminate(   ErrStat )
CALL Hydro_Terminate( )
CALL Noise_Terminate( )
IF ( BEEP ) CALL UsrAlarm
CALL NormStop( )

END SUBROUTINE fastend

