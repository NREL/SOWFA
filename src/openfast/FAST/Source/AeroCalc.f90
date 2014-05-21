!BJJ Start of proposed change vXX NWTC_Lib
!MODULE AeroInterface
!
!   USE NWTC_Library
!   IMPLICIT NONE
!
!CONTAINS
!bjj End of proposed change
!=======================================================================
!BJJ START OF PROPOSED CHANGE
!SUBROUTINE AeroCalc ( ZTime, ErrStat )
!
!
!      ! AeroDyn MODULES
!   USE                              AeroDyn
!!   USE                              AeroTime,     ONLY: DT, DTaero, TimFlag
!
!      ! FAST MODULES
!   USE                              AeroElem
!   USE                              Blades,       ONLY: DRNodes, BldNodes
!
!   IMPLICIT NONE
!
!   REAL(ReKi), INTENT(IN)        :: ZTime
!   INTEGER,    INTENT(OUT)       :: ErrStat
!
!   INTEGER                       :: J
!   INTEGER                       :: K
!
!
!   ErrStat = 0
!
!!   IF ( ADFirstLoop .OR. ( ZTime - Prev_Aero_t ) >= DTAERO )  THEN
!!         ! It's time to update the aero forces
!!
!!         ! First we reset the DTAERO parameters for next time
!!      DT = ZTime - Prev_Aero_t
!!      Prev_Aero_t = ZTime
!!
!!         ! Reset TIMFLAG to indicate we have completed a forward time step and met or exceeded the aero time step
!!      TIMFLAG = .TRUE.
!!
!!      ADFirstLoop = .FALSE.
!!   ENDIF
!
!
!!      ! This is a new time step, proceed with the aero force calculation
!!   IF ( TIMFLAG ) THEN
!
!      ADAeroLoads = AD_CalculateLoads( ZTime, ADAeroMarkers, ADCurrentTurbineState, ErrStat )
!
!         ! FAST wants [force/length], instead of [force].
!
!!      DO J=1,BldNodes
!!
!!         ADCurrentOutputs(J, :)%DFN = ADCurrentOutputs(J, :)%DFN / DRNodes(J)
!!         ADCurrentOutputs(J, :)%DFT = ADCurrentOutputs(J, :)%DFT / DRNodes(J)
!!         ADCurrentOutputs(J, :)%PMA = ADCurrentOutputs(J, :)%PMA / DRNodes(J)
!!
!!      END DO !J - BldNodes
!!
!!      DO K=1,SIZE(ADCurrentOutputs,2)  !TurbConf.NumBl
!!
!!         ADCurrentOutputs(:, K)%DFN = ADCurrentOutputs(:, K)%DFN / DRNodes(:)
!!         ADCurrentOutputs(:, K)%DFT = ADCurrentOutputs(:, K)%DFT / DRNodes(:)
!!         ADCurrentOutputs(:, K)%PMA = ADCurrentOutputs(:, K)%PMA / DRNodes(:)
!!
!!      END DO !J - BldNodes
!
!
!!      TIMFLAG = .FALSE.
!!
!!   END IF
!
!END SUBROUTINE !AeroCalc
!----------------------------------------------------------------------------------------------------
!!RMSUBROUTINE AeroCalc (t, IB, JEl, CurElFrc)
!SUBROUTINE AeroCalc (t, IB, JEl, LinVelES, CurElFrc)
!!BJJ END of proposed change
! ! Get the aerodynamic forces on each blade
! ! This is where the aero forces on the blades are actually calculated for
! !  each time step in the simulation.
!
! ! FAST Modules
!!USE                               Noise
!!bjj start of proposed change
!USE                               TurbConf,     ONLY: NumBl
!USE                               TurbCont,     ONLY: BlPitch
!USE                               AeroElem,     ONLY: ElAeroLoc, ElRad, NumADBldNodes
!
!USE                               Constants,    ONLY: TwoPiNB
!USE                               DOFs,         ONLY: DOF_DrTr, DOF_GeAz, DOF_RFrl, DOF_Yaw, DOF_Y
!USE                               RtHndSid,     ONLY: QT, QDT, PLinVelEP
!USE                               CoordSys,     ONLY: m1, m2, m3, c1
!USE                               Blades,       ONLY: AeroTwst, DRNodes
!!bjj end of proposed change
!
! ! AeroDyn Modules
!!BJJ Start of proposed change AD_v12.70
!!rmUSE                               AeroTime
!!rmUSE                               Blade
!!rmUSE                               Constant
!!rmUSE                               Element
!!rmUSE                               ElemInflow
!!rmUSE                               Precision
!USE                               AeroTime,     ONLY: DT, DTaero, TimFlag !, TIME
!!USE                               Blade,        ONLY: DR !, IBlade
!!USE                               Element,      ONLY: Twist !, JElem
!USE                               ElemInflow,   ONLY: ALPHA, W2
!
!USE                               AeroDyn
!!BJJ End of proposed change
!
!IMPLICIT                          NONE
!
!
!REAL(ReKi)                     :: CurElFrc(3)         ! Array of the current element forces.
!!BJJ RM:REAL(ReKi)                     :: DFN
!!BJJ RM:REAL(ReKi)                     :: DFT
!!bjj start of proposed change
!!RMREAL(ReKi), ALLOCATABLE, SAVE  :: ElAeroFrc(:,:,:) ! Array of DFN, DFT, PMA aero forces and moment on elements
!!bjj END of proposed change
!REAL(ReKi)                     :: PMA
!REAL(ReKi), SAVE               :: Prev_Aero_t = 0.0   ! Last time aero forces were updated - set to -1 so aero calcs are done at t = 0
!REAL(ReKi)                     :: t  !time
!
!INTEGER(4)                     :: IB
!INTEGER(4)                     :: JEl
!
!
!INTEGER(4)                     :: Sttus
!
!!bjj start of proposed change
!INTEGER                        :: ErrStat
!REAL(ReKi)                     :: LinVelEPYaw(3) ! This is the linear velocity of the hub in the inertia frame due solely to yaw and rotor-furl effects
!
!!TYPE(Marker)                   :: ADCurrentMarker
!!TYPE(CalcInptTurbine)          :: ADCurrentTurbineState
!!TYPE(CalcOutput)               :: ADCurrentOutputs
!
!REAL(ReKi), INTENT(IN)         :: LinVelES  (3)                                   ! Linear velocity of current point on the current blade (point S) in the inertia frame.
!!bjj end of proposed change
!
!!bjj chg: LOGICAL(1), SAVE               :: FirstLoop = .TRUE.
!!bjj rmLOGICAL,    SAVE               :: FirstLoop = .TRUE.
!
!
!!BJJ start test:
!!CALL WrNR( Flt2LStr(Prev_Aero_t)//' ' )
!!bjj end
!
!!bjj start of proposed change
!!rm ! Allocate the array on the first pass
!!rmIF ( .NOT. ALLOCATED (ElAeroFrc) ) THEN
!!rm!bjj start of proposed change
!!rm!rm   ALLOCATE ( ElAeroFrc ( NB, NELM, 3) , STAT=Sttus )
!!rm   ALLOCATE ( ElAeroFrc ( NumBl, NumADBldNodes, 3) , STAT=Sttus )
!!rm!bjj end of proposed change
!!rm   IF ( Sttus /= 0 ) CALL ProgAbort ( ' Error allocating memory for ElAeroFrc array.' )
!!rm   ElAeroFrc(:,:,:) = 0.0
!!rmENDIF
!!bjj end of proposed change
!
!!bjj start of proposed change
!!rm ! Set the AeroDyn TIME variable
!!rmTIME = t
!!bjj end of proposed change
!
!IF ( FirstLoop .OR. ( t - Prev_Aero_t ) >= DTAERO )  THEN
!!BJJ start test:
!!CALL WrScr( 'XXX' )
!!bjj end
!
! ! It's time to update the aero forces
!
! ! First we reset the DTAERO parameters for next time
!   DT = t - Prev_Aero_t
!   Prev_Aero_t = t
!
! ! Reset TIMFLAG to indicate we have completed a forward time step
! !  and met or exceeded the aero time step
!   TIMFLAG = .TRUE.
!ENDIF
!
! ! This is a new time step, proceed with the aero force calculation
!IF ( TIMFLAG ) THEN
!
!
!  ! Get the aero loads on one blade element
!!bjj start of proposed change
!!RM ! Tell AeroDyn what blade and element we are analyzing
!!RM   IBlade = IB
!!RM   JElem  = JEl
!!RM
!!RM ! This is the gateway into AeroDyn for the aero foces on an element
!!RM !  Send :
!!RM !   FirstLoop - true until we loop through all blades once
!!RM !   JElem  - element number (integer)
!!RM !  It returns:
!!RM !   DFN - element aerodynamic normal force (N or lbf)
!!RM !   DFT - element aerodynamic tangential force (N or lbf)
!!RM !   PMA - element aerodynamic pitching moment (N-m or lbf-ft)
!!RM !  All forces are in the blade coordinate system (not the element)
!!RM ! NOTE: AeroDyn will call subroutines in the dynamics code to provide
!!RM !       rotor, blade and element parameters needed for velocity calculations.
!!RM
!!rm   CALL AeroFrcIntrface( FirstLoop, JElem, DFN, DFT, PMA )
!
!
!!   ADCurrentInputs%CurrentBlade       = IB
!!   ADCurrentInputs%CurrentElement     = JEl
!   ADAeroMarkers%Blade(JE,IB)%Position(:)        = ElAeroLoc(:)
!   ADAeroMarkers%Blade(JE,IB)%Orientation(1,:)   = (/ m1(IB,JEl,1), -1.*m1(IB,JEl,3), m1(IB,JEl,2) /)
!   ADAeroMarkers%Blade(JE,IB)%Orientation(2,:)   = (/ m2(IB,JEl,1), -1.*m2(IB,JEl,3), m2(IB,JEl,2) /)
!   ADAeroMarkers%Blade(JE,IB)%Orientation(3,:)   = (/ m3(IB,JEl,1), -1.*m3(IB,JEl,3), m3(IB,JEl,2) /)
!   ADAeroMarkers%Blade(JE,IB)%TranslationVel(:)  = (/ LinVelES(1),  -1.*LinVelES(3),  LinVelES(2)  /)  !AeroDyn's coordinates
!
!
!   ADCurrentTurbineState%RotorSpeed   = ABS( QDT(DOF_DrTr) + QDT(DOF_GeAz) )
!   ADCurrentTurbineState%YawAngle     = ATAN2( c1(3), c1(1) ) ! NOTE: c1(X) = DotProd( c1, zX ) where X = 1,2,3
!
!   !  HubVDue2Yaw is the hub's in-plane, horozontal velocity due to
!   !   the yaw rate, used in the DiskVel subroutine as YAWVEL
!   !  The nacelle-yaw, platform-yaw, and furl rates contribute to this velocity
!   !  <--This is the velocity of the hub due to nacelle- and platform-yaw and rotor-furling effects projected in the plane of rotation and parallel to xi-yi plane
!   LinVelEPYaw = PLinVelEP(DOF_RFrl,0,:)*QDT(DOF_RFrl) & ! This is the linear velocity of the hub in the inertia frame due solely to nacelle-and platform-yaw and rotor-furl effects
!               + PLinVelEP(DOF_Yaw ,0,:)*QDT(DOF_Yaw ) &
!               + PLinVelEP(DOF_Y   ,0,:)*QDT(DOF_Y   )
!
!   ADCurrentTurbineState%HubVDue2Yaw  = -1.*LinVelEPYaw(1)*SIN( ADCurrentTurbineState%YawAngle ) + LinVelEPYaw(3)*COS( ADCurrentTurbineState%YawAngle )  !JASON: WITH ROTOR-FURL, THE ROTOR PLANE CAN TRANSLATE AN ANY DIRECTION DUE TO YAW RATES AND ROTOR-FURL RATES; THUS, WE SHOULD REALLY SPECIFY HUB VELOCITIES IN THE PLANE OF THE DISK IN TWO DIRECTIONS (HORIZONTAL AND VERTICAL) AND ADD A HUB VELOCITY NORMAL TO THE DISK.  THE ONLY VELOCITY CURRENTLY TAKEN INTO ACCOUNT IS THE HORIZONTAL COMPONENET WITHIN THE DISK (THROUGH VARIABLE HubVDue2Yaw)
!
!
!   ADCurrentTurbineState%TiltAngle    = ATAN2( c1(2), SQRT( c1(1)*c1(1) + c1(3)*c1(3) ) )  ! NOTE: c1(X) = DotProd( c1, zX ) where X = 1,2,3
!   ADCurrentTurbineState%AzimuthAngle(IB) = QT(DOF_DrTr) + QT(DOF_GeAz) + 1.5*Pi + ( IB - 1 )*TwoPiNB
!!bjj not necessary, but done for comparing w/ Adams
!!CALL Mpi2Pi( ADCurrentTurbineState%AzimuthAngle )
!
!   ADCurrentTurbineState%ElementPitch(JEl,IB) = BlPitch( IB ) + AeroTwst( JEl )
!   ADCurrentTurbineState%RLocal(JEl,IB)       = ElRad
!!   ADCurrentTurbineState%SetMulTabLoc = .FALSE.
!!   ADCurrentTurbineState%FirstLoop    = FirstLoop
!
!!write( *, '(F10.5,x,I2,x,I3,x,L,15(x,G15.6),7(x,G15.6),x,L,x,G15.6)') t, ADCurrentInputs, ADCurrentTurbineState
!
!   CALL AD_CalculateLoads( t, ADAeroMarkers, ADCurrentTurbineState, ADCurrentOutputs, ErrStat )
!
!!bjj end of proposed change
!
!
! ! FAST wants [force/length], instead of [force].
!!BJJ START OF PROPOSED CHANGE
!!RM   ElAeroFrc ( IB, JEl, 1) = DFN / DR(JEl)
!!RM   ElAeroFrc ( IB, JEl, 2) = DFT / DR(JEl)
!!RM   ElAeroFrc ( IB, JEl, 3) = PMA / DR(JEl)
!!   ElAeroFrc ( IB, JEl, 1) = ADCurrentOutputs%DFN / DR(JEl)
!!   ElAeroFrc ( IB, JEl, 2) = ADCurrentOutputs%DFT / DR(JEl)
!!   ElAeroFrc ( IB, JEl, 3) = ADCurrentOutputs%PMA / DR(JEl)
!
!   ADCurrentOutputs(JEl, :)%DFN = ADCurrentOutputs(JEl, :)%DFN / DRNodes(JEl)
!   ADCurrentOutputs(JEl, :)%DFT = ADCurrentOutputs(JEl, :)%DFT / DRNodes(JEl)
!   ADCurrentOutputs(JEl, :)%PMA = ADCurrentOutputs(JEl, :)%PMA / DRNodes(JEl)
!!BJJ END OF PROPOSED CHANGE
!
!!bjj moved this to Noise module:
!!rm ! Save total velocity and alpha values for noise calculation:
!!rm!bjj Start of proposed change AeroDyn v12.70
!!rm   AlphaNoise( IB, JEl   ) = ABS( ALPHA * RtoD )   ! Noise models only valid for positive angles of attack (airfoil assumed symmetric)
!!rm   AlphaNoise( IB, JEl   ) = ABS( ALPHA * R2D )   ! Noise models only valid for positive angles of attack (airfoil assumed symmetric)
!!rm!bjj end of proposed change AeroDyn v12.70
!!rm   UNoise    ( IB, JEl   ) = SQRT( W2 )
!!bjj end of proposed change
!
! ! Turn off FirstLoop flag if this is the last element on the last blade
! ! Set the TIMFLAG to false after we've looped through all the blades.
! !  This method assumes that blades and elements appear in the data set
! !  in ascending order, as is the case in the current version of FAST.
!!bjj start of proposed change
!!rm   IF ( IBLADE == NB .AND. JElem == NELM ) THEN
!!rm      IF (FirstLoop) FirstLoop = .FALSE.
!   IF ( IB == NumBl .AND. JEl == NumADBldNodes ) THEN
!!     FirstLoop = .FALSE.
!!      ADCurrentTurbineState%FirstLoop = .FALSE.
!!bjj end of proposed change
!      TIMFLAG = .FALSE.
!   ENDIF
!
!
!ENDIF ! TIMFLAG
!
! !Put the forces in the vector to return to FAST
!!BJJ start of proposed change
!!rmCurElFrc = ElAeroFrc ( IB, JEl, :)
!
!CurElFrc = (/ ADCurrentOutputs(JEl, IB)%DFN &
!              ADCurrentOutputs(JEl, IB)%DFT &
!              ADCurrentOutputs(JEl, IB)%PMA   /)
!!bjj end of proposed change
!
!RETURN
!END SUBROUTINE AeroCalc
!BJJ END OF PROPOSED CHANGE
!=======================================================================
!bjj start of proposed change ADv13.00a
! replace this subroutine:
!SUBROUTINE AeroInput
! !  Get the AeroDyn inputs
! !  Sets up the simulation parameters for operation with or without AeroDyn
!
!
! ! AeroDyn Modules
!!BJJ Start of proposed change AD_v12.70
!!rmUSE               Identify
!!rmUSE               Switch
!USE               Identify, ONLY: DynProg, DynVer, DynProgRoot, AeroProg, AeroVer
!USE               Switch,   ONLY: SIUnit, ELEMPRN
!USE               AeroSubs, ONLY: AD_InputGate
!USE               NWTC_Library
!USE               AeroGenSubs, ONLY: ElemOpen, SetProgName
!!BJJ End of proposed change
!
! ! FAST Modules
!USE               General     !Holds ProgName, ProgVer and RootName
!USE               Features    !Holds CompAero switch
!
!!BJJ Start of proposed change AD_v12.70w
!USE               TurbConf
!!BJJ End of proposed change AD_v12.70w
!
!IMPLICIT          NONE
!
!!bjj start of proposed change AD v12.70w
!REAL(ReKi)        :: R
!!bjj end of proposed change
!
!
!DynProg = TRIM(ProgName)      !The name of the dynamics program (FAST)
!DynVer  = TRIM(ProgVer)       !The version of the dynamics program (see SetVersion)
!IF ( Cmpl4SFun )  THEN     ! FAST has been compiled as an S-Function for Simulink
!   DynProgRoot = TRIM(RootName)//'_SFunc' !The name of the primary input file
!ELSE                       ! FAST has been compiled normally
!   DynProgRoot = TRIM(RootName)           !The name of the primary input file
!ENDIF
!
!Call SetProgName ! Sets the program name string for output
!
! ! The FAST gurus say the aerodyn input must be read regardless of CompAero
! ! Set values known by FAST and needed by AeroDyn
!CALL Set_AD_Params
!
! ! This is the AeroDyn Input gateway
!!bjj start of proposed change AD v12.70w
!!rmCALL AD_InputGate (ADFile) !The new routine has the input file name as an argument
!
!R = TipRad * CosPreC(1) ! let's send the rotor radius here, instead of in Set_FAST_Params
!
!CALL AD_InputGate (ADFile, R) !The new routine has the input file name as an argument
!!bjj end of proposed change
!
! ! Set FAST variables based on AeroDyn inputs
!CALL Set_FAST_Params
!
! ! Check the units for consistency
!IF (.NOT. SIUnit) THEN
!   CALL ProgAbort ( ' Units in AeroDyn must be "SI" to work with FAST.' )
!ENDIF
!
! ! If we are computing aerodynamics, tell the user, and set up the element file for writing
!IF (CompAero) THEN
!   CALL WrScr1 ( ' Aerodynamics loads calculated using '//TRIM(AeroProg)//TRIM(AeroVer) )
!
! !Set up the Aero outputs if selected
!IF ( ELEMPRN )  THEN
!   IF ( Cmpl4SFun )  THEN     ! FAST has been compiled as an S-Function for Simulink
!      CALL ElemOpen (TRIM( RootName )//'_SFunc.elm')
!   ELSE                       ! FAST has been compiled normally
!      CALL ElemOpen (TRIM( RootName )//'.elm')
!   ENDIF
!ENDIF
!
!ENDIF
!
!
!
!RETURN
!END SUBROUTINE AeroInput
!=======================================================================
!bjj end of proposed change
!bjj start of proposed change
!SUBROUTINE GetBladeParams (AziAng)
!
! !  This subroutine returns blade parameters used by the AeroDyn routines.
! !  Called by AeroDyn once for each blade
!
!
! ! FAST Modules
!USE                           Constants
!USE                           DOFs
!!bjj rm NWTC_Library: USE                           Precision
!USE                           RtHndSid ! for the QT array
!
! ! AeroDyn Modules
!USE                           Blade
!
!!bjj Start of proposed change vXX NWTC_Lib
!USE                               NWTC_Library
!!bjj End of proposed change vXX NWTC_Lib
!
!
!IMPLICIT                      NONE
!
!
!   ! Passed Variables:
!
!REAL(ReKi)                 :: AziAng
!
!
!
! ! Get blade azimuth angle from QT array.
! !  The current blade azimuth is stored in the sum of QT(DOF_DrTr) and
! !  QT(DOF_GeAz).  Adjust so 0 is at 6 o'clock (AeroDyn).  It's 3 o'clock in
! !  FAST.
!AziAng = QT(DOF_DrTr) + QT(DOF_GeAz) + 1.5*Pi + ( IBlade - 1 )*TwoPiNB
!
!
!
!RETURN
!END SUBROUTINE GetBladeParams
!!=======================================================================
!!bjj start of proopsed change v13.00b
!!rmSUBROUTINE GetElemParams (MulTabLoc, PitAng, ElemRad, ElemXLoc, ElemYLoc, ElemZLoc)
!SUBROUTINE GetElemParams (MulTabLoc, PitAng, ElemRad, ElemPosition)
!!bjj start of proopsed change v13.00b
!
! !  This subroutine returns element parameters used by the AeroDyn routines.
! !  Called by AeroDyn once for each element
!
! ! FAST Modules
!USE                           AeroElem ! for ElRad, ElAeroLoc
!USE                           TurbCont ! for BlPitch
! ! AeroDyn Modules
!USE                           Blade   ! for IBlade
!USE                           Element ! for TWIST, JElem
!!bjj rm NWTC_Library: USE                           Precision
!!bjj Start of proposed change vXX NWTC_Lib
!USE                               NWTC_Library
!!bjj End of proposed change vXX NWTC_Lib
!
!
!IMPLICIT                      NONE
!
!
!   ! Passed Variables:
!
!!JASON: WHAT SHOULD THIS VARIABLE, MulTabLoc, BE SET TO?
!REAL(ReKi)                 :: MulTabLoc
!REAL(ReKi)                 :: PitAng
!!bjj start of proopsed change v13.00b
!!rmREAL(ReKi)                 :: ElemXLoc
!!rmREAL(ReKi)                 :: ElemYLoc
!!rmREAL(ReKi)                 :: ElemZLoc
!REAL(ReKi)                 :: ElemPosition(3)
!!bjj end of proposed change
!REAL(ReKi)                 :: ElemRad
!
!
!
!!JASON: SHOULD THIS CALCULATION OF PitAng INCLUDE THE EFFECTS OF TEETER W/ DELTA-3?
! ! PitAng is the local blade element pitch (including twist)
!
!PitAng = BlPitch( IBlade ) + TWIST( JElem )
!
! ! ElemRad is the radius of the blade element
! !  measured from the axis of rotation
!
!ElemRad = ElRad
!
!
! ! Get the Cartesian coordinates of the blade element
! !  in the ground (inertial) reference frame
! !  Ignore small flap angle in calculation of coords.
!
! ! Get coordinates of blade element relative to
!!rm bjj !  the undeflected tower top (in inertia ref. frame)
! !  the undeflected tower base (in inertia ref. frame)
!
!!bjj start of proopsed change AD v13.00b
!!rmElemXLoc = ElAeroLoc(1)
!!rmElemYLoc = ElAeroLoc(2)
!!rmElemZLoc = ElAeroLoc(3)
!ElemPosition(:) = ElAeroLoc(:)
!!bjj start of proopsed change v13.00b
!
!
!
!RETURN
!END SUBROUTINE GetElemParams
!!=======================================================================
!SUBROUTINE GetRotorParams (RotorSpeed, YawAngle, HubVDue2Yaw, TiltAngle)
!
! !  This subroutine returns rotor parameters used by the AeroDyn routines.
! !  Called by AeroDyn once for each time step
!
!
! ! FAST Modules:
!
!USE                           CoordSys
!USE                           DOFs
!USE                           RtHndSid ! for the QDT array
! ! AeroDyn Modules:
!
!!bjj rm NWTC_Library: USE                           Precision
!!bjj Start of proposed change vXX NWTC_Lib
!USE                               NWTC_Library
!!bjj End of proposed change vXX NWTC_Lib
!
!
!IMPLICIT                      NONE
!
!
!REAL(ReKi)                 :: HubVDue2Yaw
!REAL(ReKi)                 :: LinVelEPYaw(3) ! This is the linear velocity of the hub in the inertia frame due solely to yaw and rotor-furl effects
!REAL(ReKi)                 :: RotorSpeed
!REAL(ReKi)                 :: TiltAngle
!REAL(ReKi)                 :: YawAngle
!
!
! ! Get rotor speed
!! The rotor speed passed to AeroDyn must always be positive.  In FAST,
!!   it is possible for the rotor speed to change sign (e.g., when the
!!   high-speed shaft brake locks the high-speed shaft, the inertia of
!!   the rotor will cause it oscillate about the locked generator
!!   position when the drive train DOF is enabled).
!RotorSpeed = ABS( QDT(DOF_DrTr) + QDT(DOF_GeAz) )
!
!
! ! Get the yaw angle in radians (opposite convention in AeroDyn)
! ! This angle includes anything that skews the rotor shaft about -zi from xi
!YawAngle = ATAN2( c1(3), c1(1) ) ! NOTE: c1(X) = DotProd( c1, zX ) where X = 1,2,3
!
!
! !  HubVDue2Yaw is the hub's in-plane, horozontal velocity due to
! !   the yaw rate, used in the DiskVel subroutine as YAWVEL
! !  The nacelle-yaw, platform-yaw, and furl rates contribute to this velocity
! !  <--This is the velocity of the hub due to nacelle- and platform-yaw and
! !     rotor-furling effects projected in the plane of rotation and parallel to
! !     xi-yi plane
!LinVelEPYaw = PLinVelEP(DOF_RFrl,0,:)*QDT(DOF_RFrl) & ! This is the linear velocity of the hub in
!            + PLinVelEP(DOF_Yaw ,0,:)*QDT(DOF_Yaw ) & ! the inertia frame due solely to nacelle-
!            + PLinVelEP(DOF_Y   ,0,:)*QDT(DOF_Y   )   ! and platform-yaw and rotor-furl effects
!HubVDue2Yaw = - LinVelEPYaw(1)*SIN( YawAngle ) + LinVelEPYaw(3)*COS( YawAngle )
!!JASON: WITH ROTOR-FURL, THE ROTOR PLANE CAN TRANSLATE AN ANY DIRECTION DUE TO YAW RATES AND ROTOR-FURL RATES; THUS, WE SHOULD REALLY SPECIFY HUB VELOCITIES IN THE PLANE OF THE DISK IN TWO DIRECTIONS (HORIZONTAL AND VERTICAL) AND ADD A HUB VELOCITY NORMAL TO THE DISK.  THE ONLY VELOCITY CURRENTLY TAKEN INTO ACCOUNT IS THE HORIZONTAL COMPONENET WITHIN THE DISK (THROUGH VARIABLE HubVDue2Yaw)
!
!
! ! TILT = Tilt angle of rotor hub with respect to ground
! ! This angle includes anything that skews the rotor shaft out of the xi-yi plane
!TiltAngle = ATAN2( c1(2), SQRT( c1(1)*c1(1) + c1(3)*c1(3) ) )  ! NOTE: c1(X) = DotProd( c1, zX ) where X = 1,2,3
!
!
!
!RETURN
!END SUBROUTINE GetRotorParams
!!=======================================================================
!SUBROUTINE GetVNVT (VX, VY, VZ, VTTotal, VNWind, VNElement)
!
! !  This subroutine is called by AeroDyn and returns velocities of the wind
! !   and the element in the ground reference frame.
! !  AeroDyn supplies VX, VY, and VZ - the wind in the ground reference frame
! !  FAST returns the total tangential and normal wind velocities
! !   (due to wind and blade motion) relative to the element
!
! ! FAST Modules
!USE                           AeroElem
!USE                           CoordSys
!
! ! AeroDyn Modules
!USE                           Blade
!USE                           Element
!!bjj rm NWTC_Library: USE                           Precision
!!bjj Start of proposed change vXX NWTC_Lib
!USE                               NWTC_Library
!!bjj End of proposed change vXX NWTC_Lib
!
!
!IMPLICIT                      NONE
!
!
!REAL(ReKi)                 :: VNElement
!REAL(ReKi)                 :: VNWind
!REAL(ReKi)                 :: VTElement
!REAL(ReKi)                 :: VTTotal
!REAL(ReKi)                 :: VTWind
!REAL(ReKi)                 :: VW_G    ( 3 )
!REAL(ReKi)                 :: VX
!REAL(ReKi)                 :: VY
!REAL(ReKi)                 :: VZ
!
!
!
! ! Convert from AeroDyn's ground coordinate system to FAST's ground system.
!VW_G(1) =  VX
!VW_G(2) =  VZ
!VW_G(3) = -VY
!
!
! ! Get normal and tangential velocity components
! !  treat wind and blade motions separately
!
! !  VTWind = Tangential component of wind velocity relative to ground
! !  VNWind = Normal component of wind velocity relative to ground
! !  VTElement = Tangential component of element velocity rel. to ground
! !  VNElement = Normal component of blade element velocity rel. to ground
!
! ! Resolve blade and wind velocity components into in-plane-of-rotation
! !  and normal-to-span coordinates
!
! ! NOTE: I am not actually using the DotProd() FUNCTION since it is really
! !       slow when it is used in a different file from where it is defined.
!VTWind    =  (   VW_G(1)*m2(IBlade,JElem,1) &   ! =  DotProd( VW_G, m2(IBlade,JElem,:) )
!               + VW_G(2)*m2(IBlade,JElem,2) &
!               + VW_G(3)*m2(IBlade,JElem,3)   )
!VTElement = -VES(2)
!
!
!VTTotal   =  VTWind + VTElement
!
! ! NOTE: I am not actually using the DotProd() FUNCTION since it is really
! !       slow when it is used in a different file from where it is defined.
!VNWind    =  (   VW_G(1)*m1(IBlade,JElem,1) &   ! =  DotProd( VW_G, m1(IBlade,JElem,:) )
!               + VW_G(2)*m1(IBlade,JElem,2) &
!               + VW_G(3)*m1(IBlade,JElem,3)   )
!VNElement = -VES(1)
!
!
!
!RETURN
!END SUBROUTINE GetVNVT
!bjj end of proposed change
!=======================================================================
!bjj start of proposed change AD v13.00
!bjj: remove this subroutine:
!SUBROUTINE Set_AD_Params
! !  Set AeroDyn variables based on FAST variables
! !  Called at the start of a simulation to set up simulation parameters for AeroDyn
!
!
! ! AeroDyn Modules
!USE               Blade
!USE               AD_IOParams
!
! ! FAST Modules
!USE               General
!USE               Output
!USE               TurbConf
!
!!bjj start of proposed change vXX NWTC_Library
!USE               NWTC_Library
!!bjj end of proposed change
!
!IMPLICIT          NONE
!
!
!NB       = NumBl
!B        = REAL(NB)
!
! ! Set AeroDyn I/O parameters
!WrEchoFile = Echo
!WrOptFile  = SumPrint
!
!
!
!RETURN
!END SUBROUTINE Set_AD_Params
!bjj end of proposed change
!=======================================================================
SUBROUTINE Set_FAST_Params


   ! Set variables based on AeroDyn inputs
   ! Called at the start of the simulation to set up FAST variables based
   !   on AeroDyn parameters


   ! AeroDyn Modules:

!USE                           Blade
!USE                           Element
!bjj rm NWTC_Library: USE                           Precision
!USE                           Rotor
!bjj rm:USE                           Switch
!USE                           Wind
USE                           Blade,      ONLY: C, DR
!USE                           Element,    ONLY: RElm, Twist
!USE                           Rotor,      ONLY: HH
!USE                           Wind,       ONLY: RHO
USE                           AeroDyn
!BJJ Start of proposed change AD_v12.70
!USE                           AeroSubs
!USE                           NWTC_Library
!BJJ End of proposed change


   ! FAST Modules:

USE                           Blades
USE                           EnvCond
USE                           Features
USE                           Noise
USE                           SimCont !DT, TMax
USE                           TurbConf

!bjj start:
USE                           AeroElem !, ONLY: NumADBldNodes, AD_AeroMarker
!bjj end


IMPLICIT                      NONE


   ! Local variables:

!bjj rm not used:REAL(ReKi)                 :: DHub
!bjj rm not used:REAL(ReKi)                 :: DTip
!bjj rm not used:REAL(ReKi)                 :: ElemRad
!bjj rm not used:REAL(ReKi)                 :: RHub

INTEGER(4)                 :: IELM
INTEGER(4)                 :: Sttus                                     ! Status returned from an allocation request.

!bjj rmCHARACTER( 50)             :: Frmt
!bjj rmCHARACTER(110)             :: MESAGE
!BJJ Start of proposed change AD_v12.70w
REAL(ReKi)                 :: InpPosition(3)
!bjj rm not used:REAL(ReKi)                 :: Inf_TI(3)
REAL(ReKi)                 :: AD_RefHt
INTEGER                    :: ErrStat
!bjj end of proposed change


   ! Global functions:

!bjj rm AD 12.70b CHARACTER( 15), EXTERNAL   :: Flt2LStr



   ! Write data read in from ADFile into MODULEs used by FAST:

!bjj start of proposed change
!rm BldNodes = NELM
BldNodes = NumADBldNodes
!bjj end of proposed change

!BJJ! ADDED CHECKS ON ALLOCATION HERE:
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

!bjj: these should be returned from AD_Init
!BJJ START OF PROPOSED CHANGE
!RM RNodes   = RELM
!rm DRNodes  = DR
RNodes   = ADAeroMarkers%Blade(:,1)%Position(3) + HubRad         ! ADAeroMarkers contains relative markers after initialization
DRNodes(1) = 2.0*( RNodes(1) - HubRad )
DO IElm = 2,NumADBldNodes
   DRNodes(IElm) = 2.0*( RNodes(IElm) - RNodes(IElm-1) ) - DRNodes(IElm-1)
END DO
!BJJ END OF PROPOSED CHANGE
Chord    = C
!BJJ START OF PROPOSED CHANGE
!RMAeroTwst = TWIST
CAeroTwst(:) = ADAeroMarkers%Blade(:,1)%Orientation(1,1)
SAeroTwst(:) = ADAeroMarkers%Blade(:,1)%Orientation(2,1)
AeroTwst( :) = ATAN2( SAeroTwst(:), CAeroTwst(:) )
!BJJ END OF PROPOSED CHANGE

!bjj start of proposed change
!RMRefHH    = HH + PtfmRef  !bjj: Use turbine HH instead of AeroDyn wind reference height (HH) here?
!RMAirDens  = RHO
!rmDO IELM = 1,JElm  ! Loop through all blade elements
!rm   CAeroTwst(IELM) = COS( TWIST(IELM) )
!rm   SAeroTwst(IELM) = SIN( TWIST(IELM) )
!rmENDDO             ! IELM - all blade elements

AD_RefHt = AD_GetConstant('RefHt', ErrStat)
!bjj start of proposed change
!rmRefHH    = AD_RefHt + PtfmRef  !bjj: Use turbine HH instead of AeroDyn wind reference height (HH) here?
!bjj end of proposed change
AirDens  = AD_GetConstant('AirDensity',   ErrStat)

!BJJ START OF PROPOSED CHANGE
!RMDO IELM = 1,NumADBldNodes  ! Loop through all blade elements
!RM   CAeroTwst(IELM) = COS( AeroTwst(IELM) )
!RM   SAeroTwst(IELM) = SIN( AeroTwst(IELM) )
!RMENDDO             ! IELM - all blade elements
!BJJ END OF PROPOSED CHANGE
!bjj end of proposed change


  ! Let's compute the turbulence intensity and average wind speed for the
  !   turbulent inflow noise calculation:

IF ( CompNoise )  THEN  ! Yes, noise will be computed.
!bjj start of proposed change v12.70w
!rm   IF     ( CTWindFlag )  THEN   ! Coherent turbulence wind input selected.
!rm
!rm      CALL ProgAbort ( ' Noise predictions cannot be made when using coherent turbulence wind input.'// &
!rm                   '  Either set CompNoise to False or use hub-height or full-field wind input.' )
!rm
!rm   ELSEIF ( FDWindFlag )  THEN   ! 4-D wind input selected.
!rm
!rm      CALL ProgAbort ( ' Noise predictions cannot be made when using 4-D wind input.'// &
!rm                   '  Either set CompNoise to False or use hub-height or full-field wind input.' )
!rm
!rm   ELSEIF ( UsrWndFlag )  THEN   ! User-defined wind input selected.
!rm
!rm      CALL ProgAbort ( ' Noise predictions cannot be made when using user-defined wind input.'// &
!rm                   '  Either set CompNoise to False or use hub-height or full-field wind input.' )
!rm
!rm   ELSEIF ( FFWindFlag )  THEN   ! Full-field wind input selected.
!rm
!rm
!rm      CALL CalcFFTI (TMax,DT)    ! Use Pat Moriarty's algorithm to calculate mean wind speed and turbulence intensity from the hub-height wind data file.
!rm
!rm   ELSE                          ! Hub-height wind input selected. (Don't check HHWindFlag here since this flag can be switched to .FALSE. from SUBROUTINE GetHHWind().)
!rm
!rm      CALL CalcHHTI              ! Use Pat Moriarty's algorithm to calculate mean wind speed and turbulence intensity from the hub-height wind data file.
!rm
!rm   ENDIF
   InpPosition = (/ 0.0, 0.0, FASTHH /)

   CALL Noise_CalcTI( REAL(0.0, ReKi), TMax, DT, InpPosition )

   KinViscosity = AD_GetConstant( 'KinVisc', ErrStat )      ! this variable stored in the Noise module.  The Noise module should be rewritten so that this is part of an initialization routine.

!bjj end of proposed change v12.70w

ENDIF


   ! Set up other parameters only if we need them

IF ( CompAero )  THEN


!bjj start of proposed change ad v12.70w
! since we know this already (sent to AD in initialization), let's not recalculate it
!rm   R        = TipRad * CosPreC(1)
!rm   RHub     = HubRad * CosPreC(1)
!bjj end of proposed change


   ! Let's see if the hub-height in AeroDyn and FAST are within 10%:

!bjj start of proposed change
!rm   IF ( ABS( FASTHH - HH ) > 0.1*( FASTHH ) )  THEN
   IF ( ABS( FASTHH - AD_RefHt ) > 0.1*( FASTHH ) )  THEN  !bjj: I believe that this should not be done in the future
!bjj end of proposed change

!bjj start of proposed change AD v12.70-bjj
!rm      MESAGE = ' The FAST hub height ('//TRIM(Flt2LStr( FASTHH ))//') and aerodyn.ipt'// &
!rm               ' reference hub height ('//TRIM(Flt2LStr(HH))//')'
!rm      Frmt   = '(A,/2x,''differ by more than 10%.'')'
!rm      CALL ErrLog( MESAGE, Frmt, 'Set_FAST_Params', 7, 'WARN' )
      CALL ProgWarn( ' The FAST hub height ('//TRIM(Flt2LStr( FASTHH ))//') and AeroDyn input'// &
                    ' reference hub height ('//TRIM(Flt2LStr(AD_RefHt))//') differ by more than 10%.' )
!bjj end of proposed change
   ENDIF

!bjj start of proposed change AD v13.00a
!rm   ! Calculate the tip-loss constant for each element:
!rm
!rm   DO IELM = 1,NELM  ! Loop through all blade elements
!rm
!rm      ElemRad = RELM(IELM)*CosPreC(1)     ! Use only the precone angle of blade 1 (assumed very similar to other blades)
!rm
!rm      IF( ElemRad == 0.0 )  THEN
!rm         DTip         = R - DR(IELM) / 2.
!rm         TLCNST(IELM) = 0.5 * B * DTIP / DR(IELM)/2.
!rm      ELSE
!rm         DTip         = R - ElemRad
!rm         TLCNST(IELM) = 0.5 * B * DTip / ElemRad
!rm      ENDIF
!rm
!rm      IF ( TLCNST(IELM) < 0.001 )  THEN
!rm         TLCNST(IELM) = 0.125*B*DR(IELM)*CosPreC(1)/ElemRad      ! Use 25% of DR at tip.
!rm      ENDIF
!rm
!rm   ENDDO             ! IELM - all blade elements
!rm
!rm
!rm   ! Calculate the hub-loss constant for each element:
!rm
!rm   IF ( RHub > 0.001 )  THEN
!rm
!rm      DO IELM = 1,NELM  ! Loop through all blade elements
!rm
!rm         ElemRad = RELM(IELM)*CosPreC(1)  ! Use only the precone angle of blade 1 (assumed very similar to other blades)
!rm
!rm         DHub         = ElemRad - RHub
!rm         HLCNST(IELM) = 0.5 * B * DHub / RHub
!rm
!rm      ENDDO             ! IELM - all blade elements
!rm
!rm   ENDIF
!rm
!bjj end of proposed change
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
!bjj start of proposed change
USE                             EnvCond
!bjj end of proposed change


   ! AeroDyn Modules

!bjj rm:USE                             Airfoil
!bjj rm NWTC_Library: USE                             Precision
USE                             Rotor
!USE                             Wind
!USE                             Wind,     ONLY: VX, VY, VZ
!BJJ Start of proposed change AD_v12.70
USE                             AeroSubs
USE                             AeroDyn
!BJJ End of proposed change

!bjj Start of proposed change vXX NWTC_Lib
!USE                               NWTC_Library
!bjj End of proposed change vXX NWTC_Lib


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
!bjj rm:REAL(ReKi)                   :: VNROTOR2                                        ! The square of the wind velocity at XGRND, YGRND, ZGRND in the direction normal to the rotor plane (m^2/s^2); not used here
REAL(ReKi)                   :: WindVelEK(3)                                    ! Wind velocity at the tail fin center-of-pressure (point K) in the FAST ground (a) coordinate system

!BJJ START OF PROPOSED CHANGE
REAL(ReKi)                   :: TmpVar
INTEGER                      :: ErrStat
!bjj end of proposed change

!bjj rm:INTEGER(4)                   :: TmpNFOIL1                                       ! The value of NFOIL(1), the NFoil for blade element 1, upon entering this routine


SELECT CASE ( TFinMod ) ! Which tail fin aerodynamics model are we using?

CASE ( 0 )              ! None!


   ! Do nothing here since TFinKFx, TFinKFy, and TFinKMz are all
   !   initialized to zero.


CASE ( 1 )              ! Standard (using inputs from the FAST furling input file).


   ! Compute wind velocity at tail fin center-of-pressure in AeroDyn ground
   !   coordinate system:

!bjj start of proposed change v13.00b
!rm   XGRND = TFinCPxi           ! = the distance from the undeflected tower centerline                                     to the tail fin center-of-pressure in the xi ( z1) direction
!rm   YGRND = TFinCPyi           ! = the distance from the undeflected tower centerline                                     to the tail fin center-of-pressure in the yi (-z3) direction
!rm   ZGRND = TFinCPzi - RefHH   ! = the distance from the nominal hub position (i.e., the undeflected position of the hub) to the tail fin center-of-pressure in the zi ( z2) direction
!rm
!rmbjj start of proposed change v12.70w
!rmrm   CALL VWrel2G ( VNROTOR2, .FALSE. )
!rm   CALL VWrel2G ( (/TFinCPxi,  TFinCPyi, TFinCPzi /), VNROTOR2 ) !get VX, VY, and VZ
!rm!bjj end of proposed change v12.70w
!rm!rm
!rm!rm   ! Convert wind velocity at tail fin center-of-pressure to FAST inertial
!rm!rm   !   coordinate system:
!rm!rm
!rm!rm   WindVelEK(1) =  VX
!rm!rm   WindVelEK(2) =  VZ
!rm!rm   WindVelEK(3) = -VY
   
   WindVelEK(:) = AD_WindVelocityWithDisturbance( (/TFinCPxi,  TFinCPyi, TFinCPzi - PtfmRef /) )
   !WindVelEK(:) = AD_WindVelocityWithDisturbance( (/TFinCPxi,  TFinCPyi, TFinCPzi - PtfmRef /), 1, 1 )
   

      ! Convert wind velocity at tail fin center-of-pressure to FAST inertial coordinate system:

   TmpVar       = WindVelEK(2)
   WindVelEK(2) = WindVelEK(3)
   WindVelEK(3) = -1.0*TmpVar
!bjj end of proposed change


   ! Decrease the wind velocity at tail fin center-of-pressure in the shaft
   !   direction (c1) by the average rotor axial induction if necessary:
!JASON: IS THERE ANY WAY OF DETERMINING WHETHER OR NOT THE TAIL FIN IS ACTUALLY IN THE WAKE?

!bjj start of proposed change
!rm   IF ( SubAxInd )  WindVelEK = WindVelEK - AVGINFL*c1
   IF ( SubAxInd )  THEN
      WindVelEK = WindVelEK - AD_GetCurrentValue('AvgInfl',ErrStat)*c1
      IF (ErrStat /= 0) WindVelEK = 0.0
   END IF
!bjj end of proposed change

   ! Convert the wind velocity at tail fin center-of-pressure to tail fin
   !   coordinate system:
!bjj start of proposed change
!rm   ! NOTE: I am not actually using the DotProd() FUNCTION since it is really
!rm   !       slow when it is used in a different file from where it is defined.
!rm
!rm   TFinWndVx    =  WindVelEK(1)*p1(1) + WindVelEK(2)*p1(2) + WindVelEK(3)*p1(3)  ! Tail fin wind velocity along tail fin chordline pointing toward tail fin trailing edge = DotProd( WindVelEK,  p1 )
!rm   TFinWndVy    = -WindVelEK(1)*p3(1) - WindVelEK(2)*p3(2) - WindVelEK(3)*p3(3)  ! Tail fin wind velocity normal to plane of tail fin pointing towards suction surface    = DotProd( WindVelEK, -p3 )
!rm   TFinWndVz    =  WindVelEK(1)*p2(1) + WindVelEK(2)*p2(2) + WindVelEK(3)*p2(3)  ! Tail fin wind velocity in plane of tail fin normal to chordline and nominally upward   = DotProd( WindVelEK,  p2 )
   TFinWndVx    =      DOT_PRODUCT( WindVelEK, p1 )   ! Tail fin wind velocity along tail fin chordline pointing toward tail fin trailing edge
   TFinWndVy    = -1.0*DOT_PRODUCT( WindVelEK, p3 )   ! Tail fin wind velocity normal to plane of tail fin pointing towards suction surface
   TFinWndVz    =      DOT_PRODUCT( WindVelEK, p2 )   ! Tail fin wind velocity in plane of tail fin normal to chordline and nominally upward
!bjj end of proposed change


   ! Compute the wind velocity relative to the tail fin at the tail fin
   !   center-of-pressure by subtracting out the tail fin CP velocity:

   TFinVrelx    = TFinWndVx - TFinCPVx ! Wind velocity at, and relative to, the tail fin center-of-pressure along tail fin chordline pointing toward tail fin trailing edge
   TFinVrely    = TFinWndVy - TFinCPVy ! Wind velocity at, and relative to, the tail fin center-of-pressure normal to plane of tail fin pointing towards suction surface
   TFinVrelz    = TFinWndVz - TFinCPVz ! Wind velocity at, and relative to, the tail fin center-of-pressure in plane of tail fin normal to chordline and nominally upward


   ! Compute the dynamic pressure of the relative wind velocity:

!bjj start of proposed change
!rm   TFinQ        = 0.5*RHO*( TFinVrelx*TFinVrelx + TFinVrely*TFinVrely )
   TFinQ        = 0.5*AirDens*( TFinVrelx*TFinVrelx + TFinVrely*TFinVrely )
!bjj end of proposed change
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

!BJJ START OF PROPOSED CHANGE
!rm   TmpNFOIL1 = NFOIL(1)                               ! Save current value of NFOIL(1), the NFoil for blade element 1
!rm   NFOIL(1)  = TFinNFoil                              ! Overwrite NFOIL(1) to be the tail fin airfoil number
!rm
!rm!JASON: THIS CALL DOES NOT TAKE INTO ACCOUNT MULTIPLE AIRFOIL TABLE INTERPOLATION BASED ON REYNOLDS NUMBER (OR OTHER PARAMETERS) BECAUSE I'M NOT SURE HOW THAT CODE WORKS.
!rm   CALL CLCD ( TFinAOA, TFinCL, TFinCD, TFinCM, 1 )   ! Use the NFoil for blade element 1 to represent the tail fin airfoil
!rm
!rm   NFOIL(1)  = TmpNFOIL1                              ! Replace NFOIL(1) back to its original value for blade element 1
!JASON: THIS CALL DOES NOT TAKE INTO ACCOUNT MULTIPLE AIRFOIL TABLE INTERPOLATION BASED ON REYNOLDS NUMBER (OR OTHER PARAMETERS) BECAUSE I'M NOT SURE HOW THAT CODE WORKS.
   CALL CLCD ( TFinAOA, TFinCL, TFinCD, TFinCM, TFinNFoil, ErrStat )   ! Use the NFoil for blade element 1 to represent the tail fin airfoil
!BJJ end OF PROPOSED CHANGE


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
!bjj start of proposed change
!rmSUBROUTINE USRMES ( FLAG, MESSAGE, ID, ACTION )
!rm!  All programs using the AeroDyn code must have a subroutine USRMES to mimic
!rm!   the intrinsic ADAMS routine.  Any updates should be made in all programs.
!rm
!rm!  Rewritten by M. Buhl to duplicate the functionality of the
!rm!   ADAMS version.  [11/94]
!rm! Changed to free-format by D. Laino [4/00]
!rm! Shortened to make porting easier.   D Laino [02/01]
!rm
!rm
!rm!bjj Start of proposed change vXX NWTC_Lib
!rmUSE                               NWTC_Library
!rm!bjj End of proposed change vXX NWTC_Lib
!rm
!rm
!rmIMPLICIT        NONE
!rm
!rm
!rm   ! Passed variables:
!rm
!rmINTEGER(4)   :: ID
!rm
!rm!bjj chg: LOGICAL(1)   :: FLAG
!rmLOGICAL   :: FLAG
!rm
!rmCHARACTER(*) :: ACTION
!rmCHARACTER(*) :: MESSAGE
!rm
!rm
!rm
!rm ! FLAG is always true, but we'll check
!rmIF (FLAG) THEN
!rm
!rm   IF ( ACTION == 'ERROR' .OR. ACTION == 'WARN' )  THEN
!rm
!rm!bjj start of proposed change
!rm!rm      WRITE (*,'( / 1X , A )')  'Program continues...'
!rm      CALL WrScr1( ' Program continues...' )
!rm!bjj end of proposed change
!rm
!rm   ELSEIF ( ACTION == 'FAULT' ) THEN
!rm
!rm!bjj start of proposed change
!rm!rm      WRITE (*,'( / 1X , A )')      'Program will abort due to error.'
!rm!rm      WRITE (*,'(   1X , A, I2 )')  'ID number = ', ID
!rm      CALL WrScr1( ' Program will abort due to error.' )
!rm      CALL WrScr ( ' ID number = '//TRIM(Int2LStr(ID)) )
!rm!bjj end of proposed change
!rm
!rm      CALL ProgAbort ( '' )
!rm
!rm   ENDIF
!rm
!rmENDIF
!rm
!rm
!rm
!rmRETURN
!rmEND SUBROUTINE USRMES
!bjj end of proposed change
! ********* End of File **********

!BJJ Start of proposed change vXX NWTC_Lib
!END MODULE AeroInterface
!bjj end of proposed change