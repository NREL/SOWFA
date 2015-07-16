!=======================================================================
SUBROUTINE Set_FAST_Params


   ! Set variables based on AeroDyn inputs
   ! Called at the start of the simulation to set up FAST variables based
   !   on AeroDyn parameters


   ! AeroDyn Modules:

USE                           Blade,      ONLY: C, DR
USE                           AeroDyn


   ! FAST Modules:

USE                           Blades
USE                           EnvCond
USE                           Features
USE                           Noise
USE                           SimCont !DT, TMax
USE                           TurbConf

USE                           AeroElem !, ONLY: NumADBldNodes, AD_AeroMarker


IMPLICIT                      NONE


   ! Local variables:

INTEGER(4)                 :: IELM
INTEGER(4)                 :: Sttus                                     ! Status returned from an allocation request.

REAL(ReKi)                 :: InpPosition(3)
REAL(ReKi)                 :: AD_RefHt
INTEGER                    :: ErrStat




   ! Write data read in from ADFile into MODULEs used by FAST:

BldNodes = NumADBldNodes

IF (.NOT. ALLOCATED(RNodes)) THEN
   ALLOCATE ( RNodes(BldNodes) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the RNodes array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED(DRNodes)) THEN
   ALLOCATE ( DRNodes(BldNodes) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the DRNodes array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED(Chord)) THEN
   ALLOCATE ( Chord(BldNodes) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the Chord array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED(AeroTwst)) THEN
   ALLOCATE ( AeroTwst(BldNodes) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the AeroTwst array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED(CAeroTwst)) THEN
   ALLOCATE ( CAeroTwst(BldNodes) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the CAeroTwst array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED(SAeroTwst)) THEN
   ALLOCATE ( SAeroTwst(BldNodes) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the SAeroTwst array.' )
   ENDIF
ENDIF

RNodes   = ADAeroMarkers%Blade(:,1)%Position(3) + HubRad         ! ADAeroMarkers contains relative markers after initialization
DRNodes(1) = 2.0*( RNodes(1) - HubRad )
DO IElm = 2,NumADBldNodes
   DRNodes(IElm) = 2.0*( RNodes(IElm) - RNodes(IElm-1) ) - DRNodes(IElm-1)
END DO
Chord    = C
CAeroTwst(:) = ADAeroMarkers%Blade(:,1)%Orientation(1,1)
SAeroTwst(:) = ADAeroMarkers%Blade(:,1)%Orientation(2,1)
AeroTwst( :) = ATAN2( SAeroTwst(:), CAeroTwst(:) )


AD_RefHt = AD_GetConstant('RefHt', ErrStat)
AirDens  = AD_GetConstant('AirDensity',   ErrStat)


  ! Let's compute the turbulence intensity and average wind speed for the
  !   turbulent inflow noise calculation:

IF ( CompNoise )  THEN  ! Yes, noise will be computed.
   InpPosition = (/ 0.0, 0.0, FASTHH /)

   CALL Noise_CalcTI( REAL(0.0, ReKi), TMax, DT, InpPosition )

   KinViscosity = AD_GetConstant( 'KinVisc', ErrStat )      ! this variable stored in the Noise module.  The Noise module should be rewritten so that this is part of an initialization routine.

ENDIF


   ! Set up other parameters only if we need them

IF ( CompAero )  THEN

   ! Let's see if the hub-height in AeroDyn and FAST are within 10%:

   IF ( ABS( FASTHH - AD_RefHt ) > 0.1*( FASTHH ) )  THEN  !bjj: I believe that this should not be done in the future

      CALL ProgWarn( ' The FAST hub height ('//TRIM(Flt2LStr( FASTHH ))//') and AeroDyn input'// &
                    ' reference hub height ('//TRIM(Flt2LStr(AD_RefHt))//') differ by more than 10%.' )
   ENDIF

ENDIF


RETURN
END SUBROUTINE Set_FAST_Params
!=======================================================================
SUBROUTINE TFinAero( TFinCPxi, TFinCPyi, TFinCPzi, TFinCPVx, TFinCPVy, TFinCPVz )


   ! This routine computes the tail fin aerodynamic loads TFinKFx, TFinKFy,
   !   and TFinKMz.


   ! FAST Modules:

USE                             CoordSys
USE                             DOFs
USE                             General
USE                             RtHndSid
USE                             SimCont
USE                             TailAero
USE                             TurbConf
USE                             EnvCond


   ! AeroDyn Modules

USE                             Rotor
USE                             AeroSubs
USE                             AeroDyn


IMPLICIT                        NONE


   ! Passed Variables:

REAL(ReKi), INTENT(IN )      :: TFinCPVx                                        ! Absolute Velocity of the tail center-of-pressure along tail fin chordline pointing toward tail fin trailing edge (m/s)
REAL(ReKi), INTENT(IN )      :: TFinCPVy                                        ! Absolute Velocity of the tail center-of-pressure normal to plane of tail fin pointing towards suction surface    (m/s)
REAL(ReKi), INTENT(IN )      :: TFinCPVz                                        ! Absolute Velocity of the tail center-of-pressure in plane of tail fin normal to chordline and nominally upward   (m/s)
REAL(ReKi), INTENT(IN )      :: TFinCPxi                                        ! Downwind distance from the inertial frame origin to the tail fin center-of-pressure (m)
REAL(ReKi), INTENT(IN )      :: TFinCPyi                                        ! Lateral  distance from the inertial frame origin to the tail fin center-of-pressure (m)
REAL(ReKi), INTENT(IN )      :: TFinCPzi                                        ! Vertical distance from the inertial frame origin to the tail fin center-of-pressure (m)


   ! Local variables:

REAL(ReKi)                   :: CTFinAOA                                        ! = COS( TFinAOA )
REAL(ReKi)                   :: STFinAOA                                        ! = SIN( TFinAOA )
REAL(ReKi)                   :: TFinQArea                                       ! (dynamic pressure)*(tail fin area) of the relative wind velocity
REAL(ReKi)                   :: TFinVrelx                                       ! Wind velocity at, and relative to, the tail fin center-of-pressure along tail fin chordline pointing toward tail fin trailing edge
REAL(ReKi)                   :: TFinVrely                                       ! Wind velocity at, and relative to, the tail fin center-of-pressure normal to plane of tail fin pointing towards suction surface
REAL(ReKi)                   :: TFinVrelz                                       ! Wind velocity at, and relative to, the tail fin center-of-pressure in plane of tail fin normal to chordline and nominally upward
REAL(ReKi)                   :: TFinWndVx                                       ! Tail fin wind velocity along tail fin chordline pointing toward tail fin trailing edge = Dot_Product( WindVelEK,  p1 )
REAL(ReKi)                   :: TFinWndVy                                       ! Tail fin wind velocity normal to plane of tail fin pointing towards suction surface    = Dot_Product( WindVelEK, -p3 )
REAL(ReKi)                   :: TFinWndVz                                       ! Tail fin wind velocity in plane of tail fin normal to chordline and nominally upward   = Dot_Product( WindVelEK,  p2 )
REAL(ReKi)                   :: WindVelEK(3)                                    ! Wind velocity at the tail fin center-of-pressure (point K) in the FAST ground (a) coordinate system

REAL(ReKi)                   :: TmpVar
INTEGER                      :: ErrStat


SELECT CASE ( TFinMod ) ! Which tail fin aerodynamics model are we using?

CASE ( 0 )              ! None!


   ! Do nothing here since TFinKFx, TFinKFy, and TFinKMz are all
   !   initialized to zero.


CASE ( 1 )              ! Standard (using inputs from the FAST furling input file).


   ! Compute wind velocity at tail fin center-of-pressure in AeroDyn ground
   !   coordinate system:


   WindVelEK(:) = AD_WindVelocityWithDisturbance( (/TFinCPxi,  TFinCPyi, TFinCPzi - PtfmRef /) )

      ! Convert wind velocity at tail fin center-of-pressure to FAST inertial coordinate system:

   TmpVar       = WindVelEK(2)
   WindVelEK(2) = WindVelEK(3)
   WindVelEK(3) = -1.0*TmpVar


   ! Decrease the wind velocity at tail fin center-of-pressure in the shaft
   !   direction (c1) by the average rotor axial induction if necessary:
!JASON: IS THERE ANY WAY OF DETERMINING WHETHER OR NOT THE TAIL FIN IS ACTUALLY IN THE WAKE?

   IF ( SubAxInd )  THEN
      WindVelEK = WindVelEK - AD_GetCurrentValue('AvgInfl',ErrStat)*c1
      IF (ErrStat /= 0) WindVelEK = 0.0
   END IF

   ! Convert the wind velocity at tail fin center-of-pressure to tail fin
   !   coordinate system:
   TFinWndVx    =      DOT_PRODUCT( WindVelEK, p1 )   ! Tail fin wind velocity along tail fin chordline pointing toward tail fin trailing edge
   TFinWndVy    = -1.0*DOT_PRODUCT( WindVelEK, p3 )   ! Tail fin wind velocity normal to plane of tail fin pointing towards suction surface
   TFinWndVz    =      DOT_PRODUCT( WindVelEK, p2 )   ! Tail fin wind velocity in plane of tail fin normal to chordline and nominally upward


   ! Compute the wind velocity relative to the tail fin at the tail fin
   !   center-of-pressure by subtracting out the tail fin CP velocity:

   TFinVrelx    = TFinWndVx - TFinCPVx ! Wind velocity at, and relative to, the tail fin center-of-pressure along tail fin chordline pointing toward tail fin trailing edge
   TFinVrely    = TFinWndVy - TFinCPVy ! Wind velocity at, and relative to, the tail fin center-of-pressure normal to plane of tail fin pointing towards suction surface
   TFinVrelz    = TFinWndVz - TFinCPVz ! Wind velocity at, and relative to, the tail fin center-of-pressure in plane of tail fin normal to chordline and nominally upward


   ! Compute the dynamic pressure of the relative wind velocity:

   TFinQ        = 0.5*AirDens*( TFinVrelx*TFinVrelx + TFinVrely*TFinVrely )
   TFinQArea    = TFinQ*TFinArea


   ! Compute the angle-of-attack between the relative wind velocity and the
   !   tail fin chordline as well as its sine and cosine:

   TFinAOA      = ATAN2( TFinVrely, TFinVrelx )
   CTFinAOA     = COS( TFinAOA )
   STFinAOA     = SIN( TFinAOA )


   ! Compute the lift, drag, and pitching moment coefficients:
   ! NOTE: The size of NFOIL equals the number of blade elements.  We need to
   !       fool the program into making it think that the tail fin is a blade
   !       element for this computation.

!JASON: THIS CALL DOES NOT TAKE INTO ACCOUNT MULTIPLE AIRFOIL TABLE INTERPOLATION BASED ON REYNOLDS NUMBER (OR OTHER PARAMETERS) BECAUSE I'M NOT SURE HOW THAT CODE WORKS.
   CALL CLCD ( TFinAOA, TFinCL, TFinCD, TFinCM, TFinNFoil, ErrStat )   ! Use the NFoil for blade element 1 to represent the tail fin airfoil


   ! Compute the resulting aerodynamic forces acting on the tail fin at its
   !   center-of-pressure:

   TFinKFx   = ( TFinCD*CTFinAOA - TFinCL*STFinAOA )*TFinQArea
   TFinKFy   = ( TFinCD*STFinAOA + TFinCL*CTFinAOA )*TFinQArea


CASE ( 2 )              ! User-defined tail fin aerodynamics model.


   CALL UserTFin ( QT(DOF_TFrl), QDT(DOF_TFrl), ZTime, DirRoot, &
                   TFinCPxi, TFinCPyi, ( TFinCPzi - PtfmRef ),  &
                   TFinCPVx, TFinCPVy,   TFinCPVz,              &
                   TFinAOA , TFinQ   ,                          &
                   TFinCL  , TFinCD  ,                          &
                   TFinKFx , TFinKFy                              )


ENDSELECT



RETURN
END SUBROUTINE TFinAero
!=======================================================================
