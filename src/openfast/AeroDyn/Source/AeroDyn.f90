MODULE AeroDyn
!  ..................................................................................................
!  AeroDyn
!  ..................................................................................................
!  National Wind Technology Center
!  National Renewable Energy Laboratory
!  Golden, Colorado, USA
!
!  Originally created by
!  Windward Engineering, LC
!  ..................................................................................................
!  v13.00.00    31 Mar 2010         B. Jonkman           NREL/NWTC
!----------------------------------------------------------------------------------------------------
   USE                        NWTC_Library
   USE                        SharedTypes

   USE                        InflowWind
   USE                        SharedInflowDefns


   IMPLICIT NONE
   PUBLIC  !BJJ: note that this is a little different than the typical "PRIVATE" unless explicitly
           !stated.  The reason is that now we can just say "USE AeroDyn" and include all the public
           !components of InflowWind, ShareTypes, SharedInflowDefs, etc instead of having to
           !explicitly include those modules, too.  However, care must be taken to explicitly state
           !which variables should be PRIVATE!


   !-------------------------------------------------------------------------------------------------
   ! Public data
   !-------------------------------------------------------------------------------------------------
   TYPE(ProgDesc), PARAMETER, PUBLIC :: AD_Prog = ProgDesc( 'AeroDyn', '(v13.00.00a-bjj, 31-Mar-2010)' )   ! the name/version/date of the Aerodynamics program


   !-------------------------------------------------------------------------------------------------
   ! Public types and procedures (subroutines/functions)
   !-------------------------------------------------------------------------------------------------
   PUBLIC :: AD_Init
   PUBLIC :: AD_CalculateLoads
   PUBLIC :: AD_GetUndisturbedWind
   PUBLIC :: AD_GetCurrentValue
   PUBLIC :: AD_GetConstant
   PUBLIC :: AD_Terminate

!   TYPE, PUBLIC :: TurbineConfig_Old
!      INTEGER                    :: NumBlades
!      REAL(ReKi)                 :: TipRadius               ! in meters
!      REAL(ReKi)                 :: HubRadius
!      REAL(ReKi)                 :: PreconeAngle            ! in radians
!   END TYPE TurbineConfig_Old

   TYPE, PUBLIC :: AD_InitOptions
      CHARACTER(1024)            :: ADInputFile             ! Name of the AeroDyn input file
      CHARACTER(1024)            :: OutRootName             ! Root name of the AeroDyn summary and element files
      LOGICAL                    :: WrSumFile               ! T/F: Write an AeroDyn summary file
   END TYPE AD_InitOptions



!temporary types to work on loops...
   TYPE, PUBLIC :: AeroLoadsOptions
!      REAL(ReKi)                 :: RotorSpeed           ! REVS from GetRotorParams()  rotor speed [rad/sec]
!      REAL(ReKi)                 :: YawAngle             ! YawAng from GetRotorParams() - nacelle yaw angle (+ clockwise from above) [rad]
!      REAL(ReKi)                 :: HubVDue2Yaw          ! YAWVEL from GetRotorParams() - hub velocity due solely to yaw rate (yaw rate * rotor sling) [m/sec]; positive yaw rate (+ cw from above) leads to positive YawVel
!      REAL(ReKi)                 :: TiltAngle            ! TILT from GetRotorParams()   - tilt angle of the rotor shaft to the horizontal plane [rad]; (+ tilt lowers the upwind end of the nacelle)
!      REAL(ReKi),ALLOCATABLE     :: AzimuthAngle(:)      ! Psi from GetBladeParams(); azimuth angle measured from 0 down, positive clockwise looking downwind[rad]
!      REAL(ReKi),ALLOCATABLE     :: ElementPitch(:,:)    ! PITNOW from GetElemParams() includes aerodynamic twist w/ blade pitch
!      REAL(ReKi),ALLOCATABLE     :: RLocal(:,:)          ! RLocal from GetElemParams()
      LOGICAL   ,ALLOCATABLE     :: SetMulTabLoc(:,:)
      REAL(ReKi),ALLOCATABLE     :: MulTabLoc(:,:)                      ! MulTabLoc from GetElemParams()
      LOGICAL                    :: LinearizeFlag
   END TYPE AeroLoadsOptions

   !-------------------------------------------------------------------------------------------------
   ! Internal variables and types
   !-------------------------------------------------------------------------------------------------
   LOGICAL, PRIVATE, SAVE            :: Initialized       = .FALSE.
   LOGICAL, PRIVATE, SAVE            :: NoLoadsCalculated = .TRUE.

   REAL(ReKi), PRIVATE               :: TwoPiNB                         ! 2*pi/number of blades
   TYPE(AllAeroLoads), PRIVATE       :: ADCurrentLoads                  ! copy of current loads to return


!====================================================================================================
CONTAINS
!====================================================================================================
FUNCTION AD_Init(ADOptions, TurbineComponents, ErrStat)
! The AeroDyn initialization subroutine
!----------------------------------------------------------------------------------------------------

   USE               Switch,        ONLY: ELEMPRN, DynInfl
   USE               AD_IOParams,   ONLY: WrOptFile, UnADopt, UnADIn
   USE               AeroSubs,      ONLY: AD_GetInput, ADOut, CheckRComp
   USE               AeroTime,      ONLY: Time, OldTime, dtAero
   USE               AeroGenSubs,   ONLY: ElemOpen
   USE               Blade,         ONLY: NB, R, DR
   USE               Element,       ONLY: HLCNST, TLCNST, NELM, RELM, TWIST
   USE               Rotor,         ONLY: HH, AvgInfl
   USE               InducedVel,    ONLY: SumInfl
   USE               ElemInflow,    ONLY: W2, Alpha


      ! Passed variables

   TYPE(AD_InitOptions),INTENT(IN)  :: ADOptions               ! Options for AeroDyn
!   TYPE(TurbineConfig_Old), INTENT(IN)  :: TurbineParams           ! Turbine parameters, necessary for initialization
   TYPE(AeroConfig),    INTENT(IN)  :: TurbineComponents       ! initial configuration of the turbine components
   INTEGER,             INTENT(OUT) :: ErrStat                 ! Determines if an error was encountered


      ! Function definition
   TYPE(AllAeroMarkers)             :: AD_Init


      ! Internal variables
   REAL(ReKi)                       :: CosPrecone
   REAL(ReKi)                       :: DTip, ElemRad, Dhub, Rhub     ! variables for calculating hub- and tip-loss constants
   REAL(ReKi)                       :: HubRadius
   REAL(ReKi)                       :: MeanWind
   REAL(ReKi)                       :: TipRadius
   REAL(ReKi)                       :: TmpVar

   INTEGER                          :: IB
   INTEGER                          :: Ielm

   TYPE(InflInitInfo)               :: InitWindInfl

   CHARACTER(1024)                  :: Title

   !-------------------------------------------------------------------------------------------------
   ! Check that the module hasn't already been initialized.
   !-------------------------------------------------------------------------------------------------
   IF ( Initialized ) THEN
      CALL WrScr( ' AeroDyn has already been initialized.' )
      ErrStat = 1
      RETURN
   ELSE
      ErrStat = 0
      CALL NWTC_Init()
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Set the program description variables stored in MODULE Identify
   !-------------------------------------------------------------------------------------------------
!   AeroProg    = AD_Prog%Name
!   AeroVer     = AD_Prog%Ver

!   DynProg     = CallingProg%Name
!   DynVer      = CallingProg%Ver


!   WRITE(Prog,'(A)') TRIM(AeroProg)//' '//TRIM(AeroVer)//' in '//TRIM(DynProg)//' '//TRIM(DynVer)
!   WRITE(Prog,'(A)') TRIM(AeroProg)//' '//TRIM(AeroVer)

   CALL WrScr1 ( ' Aerodynamic loads calculated using '//TRIM(AD_Prog%Name)//' '//TRIM(AD_Prog%Ver)//'.' )

   !-------------------------------------------------------------------------------------------------
   ! Set up AD variables
   !-------------------------------------------------------------------------------------------------
   NB = SIZE( TurbineComponents%Blade )
   
!   NB          = TurbineParams%NumBlades
   WrOptFile   = ADOptions%WrSumFile

   IF ( NB < 1 ) THEN
      CALL ProgWarn( ' Error: AeroDyn cannot run without blades in the model.' )
      ErrStat = 1
      RETURN
   END IF

!   !-------------------------------------------------------------------------------------------------
!   ! Open the echo file if desired !bjj: there is a conflict with FAST having its echo file open already...
!   !-------------------------------------------------------------------------------------------------
!   IF (WrOptFile) THEN
!      CALL OpenEcho( UnADopt, TRIM(ADOptions%OutRootName)//'.opt', ErrStat)
!      IF (ErrStat /= 0 ) RETURN
!
!      WRITE (UnEc,"(/'This file was generated by ', A , ' on ', A, ' at ', A , '.')")  &
!                     TRIM(AD_Prog%Name)//' '//TRIM( AD_Prog%Ver ), CurDate(), CurTime()
!   END IF

   !-------------------------------------------------------------------------------------------------
   ! Read the AeroDyn input file and open the output file if requested
   ! bjj: these should perhaps be combined
   !-------------------------------------------------------------------------------------------------

   CALL AD_GetInput(UnADin, ADOptions%ADInputFile, InitWindInfl%WindFileName, Title, ErrStat )
   IF (ErrStat /= 0 ) RETURN

!------
!bjj: these should all be placed in one nice subroutine...
!   CALL AllocateAeroDynArrays()
   IF (.NOT. ALLOCATED(W2) ) THEN
      ALLOCATE(W2(NELM,NB), STAT=ErrStat)
      IF (ErrStat /= 0 ) THEN
         CALL WrScr( ' Error in AeroDyn allocating memory for W2.')
         RETURN
      END IF
   END IF

   IF (.NOT. ALLOCATED(Alpha) ) THEN
      ALLOCATE(Alpha(NELM,NB), STAT=ErrStat)
      IF (ErrStat /= 0 ) THEN
         CALL WrScr( ' Error in AeroDyn allocating memory for Alpha.')
         RETURN
      END IF
   END IF
!------

   IF ( ElemPrn )  CALL ElemOpen (TRIM( ADOptions%OutRootName )//'.elm')

   !-------------------------------------------------------------------------------------------------
   ! Calculate the rotor and hub radaii from the input values
   !-------------------------------------------------------------------------------------------------
   HubRadius = DOT_PRODUCT( TurbineComponents%Blade(1)%Position(:) - TurbineComponents%Hub%Position(:), &
                            TurbineComponents%Blade(1)%Orientation(3,:) )
!   HubRadius = SQRT( SUM( (TurbineComponents%Blade(1)%Position(:) - TurbineComponents%Hub%Position(:))**2 ) )
                            
                            
      
   DO IB = 2,NB
      TmpVar = DOT_PRODUCT( TurbineComponents%Blade(IB)%Position(:) - TurbineComponents%Hub%Position(:), &
                            TurbineComponents%Blade(IB)%Orientation(3,:) )
!      TmpVar = SQRT( SUM( (TurbineComponents%Blade(IB)%Position(:) - TurbineComponents%Hub%Position(:))**2 ) )

      IF ( ABS( TmpVar - HubRadius ) > 0.001 ) THEN ! within 1 mm
         CALL ProgWarn( ' AeroDyn\AD_Init() calculated HubRadius is not the same for all '// &
                           'blades. Using value from blade 1.' )
         EXIT                           
      END IF
   END DO !IBld
   
   TipRadius = TurbineComponents%BladeLength + HubRadius
   
   CosPrecone = ASIN( DOT_PRODUCT( TurbineComponents%Blade(1)%Orientation(3,:), &
                                   TurbineComponents%Hub%Orientation(1,:) ) )  ! this is the precone angle -- we'll take the COS later
   
   DO IB = 2,NB
      TmpVar  = ASIN( DOT_PRODUCT( TurbineComponents%Blade(IB)%Orientation(3,:), &
                                   TurbineComponents%Hub%Orientation(1,:) ) )
      IF ( ABS( TmpVar - CosPrecone ) > 0.009 ) THEN     ! within ~ 1/2 degree ( I won't worry about the discontinuity because we don't want 90 degree precone, anyway. )
         CALL ProgWarn( ' AeroDyn\AD_Init() calculated precone angle is not the same for all'// &
                           ' blades. Using value from blade 1.' )
         EXIT                           
      END IF
   END DO !IBld
   
   CosPrecone = COS( CosPrecone )
     
!print *, HubRadius, TipRadius, ACOS(CosPrecone)   
     
!   CosPrecone = COS(TurbineParams%PreconeAngle)
!
!   IF ( TurbineParams%TipRadius < 0 ) THEN
!      TipRadius = RELM(NELM) + 0.5 * DR(NELM)         !estimate it
!   ELSE
!      TipRadius = TurbineParams%TipRadius
!   END IF
   R = TipRadius * CosPrecone


!   IF ( TurbineParams%HubRadius < 0 ) THEN
!      HubRadius = ( RELM(1) - 0.5 * DR(1) )           !estimate it
!   ELSE
!      HubRadius = TurbineParams%HubRadius
!   END IF
   RHub = HubRadius * CosPrecone

      ! Check that the AeroDyn input DR and RElm match (use the HubRadius and TipRadius to verify)
      ! before using them to calculate the tip- and hub-loss constants
   CALL CheckRComp( ADOptions%ADInputFile, HubRadius, TipRadius, ErrStat )
   IF ( ErrStat /= 0 ) RETURN

   !-------------------------------------------------------------------------------------------------
   ! Calculate tip-loss constants
   !-------------------------------------------------------------------------------------------------
   DO IElm = 1,NElm  ! Loop through all blade elements

      ElemRad = RELM(IElm)*CosPrecone

      IF( ElemRad == 0.0 )  THEN  !BJJ: should this be 0.001 (or another small number) instead of exactly 0.0?
!         DTip         = R - 0.5 * DR(IElm)*CosPrecone
!         TLCNST(IElm) = NB * DTip / DR(IElm)
         CALL WrScr( 'Error calculating tip loss constant for element '//TRIM(Int2LStr(IElm))//'. Division by zero.' )
         ErrStat = 1
         RETURN
      ELSE
         DTip         = R - ElemRad
         TLCNST(IElm) = 0.5 * NB * DTip / ElemRad
      ENDIF

!      IF ( TLCNST(IElm) < 0.001 )  THEN
!         TLCNST(IElm) = 0.125*NB*DR(IElm)*CosPrecone/ElemRad      ! Use 25% of DR at tip.  !bjj: is that really the tip?
!      ENDIF

   ENDDO             ! IELM - all blade elements


   !-------------------------------------------------------------------------------------------------
   ! Calculate hub-loss constants
   !-------------------------------------------------------------------------------------------------
   IF ( RHub > 0.001 )  THEN

      DO Ielm = 1,NELM  ! Loop through all blade elements

         ElemRad = RELM(Ielm)*CosPrecone  ! Use only the precone angle of blade 1 (assumed very similar to other blades)

         DHub         = ElemRad - RHub
         HLCNST(Ielm) = 0.5 * NB * DHub / RHub

      ENDDO             ! IELM - all blade elements

   ELSE

      HLCNST(:) = 0.0

   ENDIF

   !-------------------------------------------------------------------------------------------------
   ! Write the opt file, then close it
   !-------------------------------------------------------------------------------------------------

   IF (WrOptFile) THEN
      CALL OpenFOutFile( UnADopt, TRIM(ADOptions%OutRootName)//'.opt', ErrStat)
      IF (ErrStat /= 0 ) RETURN

      WRITE (UnADopt,"(/'This file was generated by ', A , ' on ', A, ' at ', A , '.')")  &
                     TRIM(AD_Prog%Name)//' '//TRIM( AD_Prog%Ver ), CurDate(), CurTime()

      CALL ADOut( TITLE, RHub, InitWindInfl%WindFileName )

      CLOSE(UnADopt)
   ENDIF

!bjj: perhaps this should be in a summary/echo file (combine WrOptFile and WrEchoFile)
!
!   IF (WrOptFile) THEN
!      CALL ADOut( TITLE )
!
!      CALL CloseEcho()
!   ENDIF


   !-------------------------------------------------------------------------------------------------
   ! Initialize the wind inflow module
   !-------------------------------------------------------------------------------------------------
   InitWindInfl%WindFileType    = DEFAULT_Wind  ! determine the type from the file name; replace with new input later
   InitWindInfl%ReferenceHeight = HH            ! HH is read from the AeroDyn input file
   InitWindInfl%Width           = 2 * R

   CALL WindInf_Init( InitWindInfl, ErrStat )
   IF (ErrStat /= 0) RETURN


   !-------------------------------------------------------------------------------------------------
   ! Turn off dynamic inflow for wind less than 8 m/s (per DJL: 8 m/s is really just an empirical guess)
   ! DJL: Comment out this code when using new proposed GDW check in ELEMFRC
   ! BJJ: FIX THIS!!!!
   !-------------------------------------------------------------------------------------------------

   IF (DynInfl) THEN

!      MeanWind = WindInf_GetMean( REAL(0.0, ReKi), REAL(600.0, ReKi), dtAero, &  !BJJ: the end time should not be hard coded, but I don't have a very good way of determining it now...
!                                  REAL( (/0.0, 0.0, HH/), ReKi ),  ErrStat )

      MeanWind = WindInf_ADhack_DIcheck( ErrStat )

      IF (ErrStat /=0) THEN
         CALL ProgWarn( ' Error getting mean velocity in AeroDyn/AD_Init(). '// &
                          'Dynamic inflow will not check for low mean wind speed.' )
         ErrStat = 0
      ELSE IF ( MeanWind < 8.0 ) THEN
         DynInfl = .FALSE.
         CALL ProgWarn( ' Estimated average wind speed in wind file is less than 8 m/s. Dynamic Inflow will be turned off.' )
      END IF

   ENDIF


   !-------------------------------------------------------------------------------------------------
   ! Allocate variable to store current loads return values here
   !-------------------------------------------------------------------------------------------------
   IF (.NOT. ALLOCATED( ADCurrentLoads%Blade ) ) THEN
      ALLOCATE ( ADCurrentLoads%Blade(NElm, NB), STAT=ErrStat )
      IF ( ErrStat /= 0 ) THEN
         CALL WrScr( ' Error allocating memory for ADCurrentLoads.')
         RETURN
      END IF
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Set the AD_Init return values here
   !-------------------------------------------------------------------------------------------------
   IF (.NOT. ALLOCATED( AD_Init%Blade ) ) THEN
      ALLOCATE ( AD_Init%Blade(NElm, NB), STAT=ErrStat )
      IF ( ErrStat /= 0 ) THEN
         CALL WrScr( ' Error allocating memory for AD_Init%Blade elements.')
         RETURN
      END IF
   END IF

      ! RELATIVE POSITION OF BLADE ELEMENTS

   AD_Init%Blade(:,:)%Position(1) = 0.0
   AD_Init%Blade(:,:)%Position(2) = 0.0

   DO IB = 1, NB
      AD_Init%Blade(:,IB)%Position(3) = RElm(:) - HubRadius
   END DO

      ! RELATIVE ORIENTATION OF BLADE ELEMENTS

   DO IB = 1,NB
      AD_Init%Blade(:,IB)%Orientation(1,1) = COS( TWIST(:) )
      AD_Init%Blade(:,IB)%Orientation(2,1) = SIN( TWIST(:) )
      AD_Init%Blade(:,IB)%Orientation(3,1) = 0.0

      AD_Init%Blade(:,IB)%Orientation(1,2) =                 -1.*AD_Init%Blade(:,IB)%Orientation(2,1)
      AD_Init%Blade(:,IB)%Orientation(2,2) =                     AD_Init%Blade(:,IB)%Orientation(1,1)
      AD_Init%Blade(:,IB)%Orientation(3,2) =                  0.0

      AD_Init%Blade(:,IB)%Orientation(1,3) =                                                                 0.0
      AD_Init%Blade(:,IB)%Orientation(2,3) =                                                                 0.0
      AD_Init%Blade(:,IB)%Orientation(3,3) =                                                                 1.0
   END DO



!   ALLOCATE ( AD_Init%Hub(1),          STAT=ErrStat )
!   IF ( ErrStat /= 0 ) THEN
!      CALL WrScr( ' Error allocating memory for AD_Init%Hub.')
!      RETURN
!   END IF
!
!   ALLOCATE ( AD_Init%Nacelle(1),      STAT=ErrStat )
!   IF ( ErrStat /= 0 ) THEN
!      CALL WrScr( ' Error allocating memory for AD_Init%Nacelle.')
!      RETURN
!   END IF
!
!   ALLOCATE ( AD_Init%Tower(1),        STAT=ErrStat )
!   IF ( ErrStat /= 0 ) THEN
!      CALL WrScr( ' Error allocating memory for AD_Init%Tower.')
!      RETURN
!   END IF
!
!   ALLOCATE ( AD_Init%Tail(1),         STAT=ErrStat )
!   IF ( ErrStat /= 0 ) THEN
!      CALL WrScr( ' Error allocating memory for AD_Init%Tail.')
!      RETURN
!   END IF


   !-------------------------------------------------------------------------------------------------
   ! Initialize AeroDyn variables not initialized elsewhere (except in module initialization)
   ! and return
   !-------------------------------------------------------------------------------------------------
   SumInfl     = 0.0
   AvgInfl     = 0.0
   Time        = 0.0
   OldTime     = 0.0
   
   TwoPiNB     = TwoPi / REAL( NB, ReKi )

   Initialized = .TRUE.
   NoLoadsCalculated = .TRUE.
   RETURN


END FUNCTION AD_Init

!====================================================================================================
FUNCTION AD_CalculateLoads( CurrentTime, InputMarkers, TurbineComponents, CurrentADOptions, ErrStat )
! The main AeroDyn procedure, it calculates loads of the elements given by InputMarkers
!----------------------------------------------------------------------------------------------------

   USE                           AeroTime !,   ONLY: Time, OldTime, DT, DTAero
   !USE                           AeroGenSubs,ONLY: Cross_Product
   USE                           Airfoil,    ONLY: MulTabLoc
   USE                           AeroSubs
   USE                           Blade,      ONLY: R, NB, DR
   USE                           Element,    ONLY: PitNow, NElm, Twist
   USE                           ElOutParams,ONLY: SaveVX, SaveVY, SaveVZ, VXSAV, VYSAV, VZSAV, WndElPrList
   USE                           InducedVel, ONLY: SumInfl
   USE                           Rotor,      ONLY: Tilt, YawAng, YawVel, Revs, AvgInfl, CTilt, CYaw, STilt, SYaw
   USE                           Switch,     ONLY: DStall, Wake, DynInfl, DynInit, ElemPrn

   USE                           UserWind

      ! Passed parameters

   REAL( ReKi ),           INTENT(IN)  :: CurrentTime             ! Current simulation time
   TYPE( AllAeroMarkers ), INTENT(IN)  :: InputMarkers            ! The input state of all aerodynamic markers
   TYPE( AeroLoadsOptions),INTENT(IN)  :: CurrentADOptions
   TYPE( AeroConfig ),     INTENT(IN)  :: TurbineComponents       ! The markers defining the current location of the turbine
  
   TYPE( AllAeroLoads )                :: AD_CalculateLoads       ! the aerodynamic loads calculated at the input marker locations
   
   REAL(ReKi), PARAMETER               :: OnePlusEpsilon = 1 + EPSILON(CurrentTime)
   INTEGER,                INTENT(OUT) :: ErrStat                 ! Determines if an error was encountered

      ! Function definition

!   TYPE( AllAeroLoads  )               :: AD_CalculateLoads

      ! Local variables

   REAL(ReKi)                 :: VNElement
   REAL(ReKi)                 :: VelNormalToRotor2
   REAL(ReKi)                 :: VNWind
   REAL(ReKi)                 :: VTTotal
   REAL(ReKi)                 :: DFN
   REAL(ReKi)                 :: DFT
   REAL(ReKi)                 :: PMA
   REAL(ReKi)                 :: SPitch                     ! sine of PitNow
   REAL(ReKi)                 :: CPitch                     ! cosine of PitNow

   REAL(ReKi)                 :: AvgVelNacelleRotorFurlYaw
   REAL(ReKi)                 :: AvgVelTowerBaseNacelleYaw
   REAL(ReKi)                 :: AvgVelTowerBaseYaw
   REAL(ReKi)                 :: AzimuthAngle
   REAL(ReKi)                 :: rNacelleHub   (2)
   REAL(ReKi)                 :: rLocal
   REAL(ReKi)                 :: rRotorFurlHub (2)
   REAL(ReKi)                 :: rTowerBaseHub (2)
   
   REAL(ReKi)                 :: tmpVector     (3)
   REAL(ReKi)                 :: VelocityVec   (3)

   INTEGER                    :: IBlade
   INTEGER                    :: IElement

   !-------------------------------------------------------------------------------------------------
   ! Check that the module has been initialized.
   !-------------------------------------------------------------------------------------------------
   IF ( .NOT. Initialized ) THEN
      CALL WrScr( 'AeroDyn must be initialized before trying to calculate aerodynamic loads.' )
      ErrStat = 1
      RETURN
   ELSE
      ErrStat = 0
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Determine if loads should be recalculated or just returned
   !-------------------------------------------------------------------------------------------------
      ! NOTE: CurrentTime is scaled by OnePlusEps to ensure that loads are calculated at every
   !       time step when DTAero = DT, even in the presence of numerical precision errors.


   IF ( NoLoadsCalculated .OR. ( CurrentTime*OnePlusEpsilon - OldTime ) >= DTAERO )  THEN
         ! It's time to update the aero forces

         ! First we reset the DTAERO parameters for next time
      DT      = CurrentTime - OldTime     !bjj: DT = 0 on first step, but the subroutines that use DT check for NoLoadsCalculated (or time > 0)
      OldTime = CurrentTime

   ELSE IF ( .NOT. CurrentADOptions%LinearizeFlag ) THEN

         ! Return the previously-calculated loads

!      CurrentOutputs = ADCurrentLoads
      
      AD_CalculateLoads = ADCurrentLoads
      RETURN

   ENDIF


   !-------------------------------------------------------------------------------------------------
   ! Calculate the forces and moments for the blade: SUBROUTINE AeroFrcIntrface( FirstLoop, JElemt, DFN, DFT, PMA )
   !-------------------------------------------------------------------------------------------------
   Time      = CurrentTime

      ! calculate rotor speed
      ! note: Subtracting the RotorFurl rotational velocity for REVS is needed to get the
      ! same answers as before v13.00.00. RotorFurl shouldn't be needed.
      
   REVS      = ABS( DOT_PRODUCT( TurbineComponents%Hub%RotationVel(:) - TurbineComponents%RotorFurl%RotationVel(:), &
                                 TurbineComponents%Hub%Orientation(1,:) ) )


      ! calculate yaw angle
      ! note: YawAng should use the Hub instead of the RotorFurl, but it is calculated this way to
      ! get the same answers as previous version.
   YawAng    = ATAN2( -1.*TurbineComponents%RotorFurl%Orientation(1,2), TurbineComponents%RotorFurl%Orientation(1,1) ) 
   SYaw      = SIN( YawAng )
   CYaw      = COS( YawAng )

      ! tilt angle
      ! note: tilt angle should use the Hub instead of RotorFurl, but it needs hub to get the same
      ! answers as the version before v13.00.00
      
   Tilt      = ATAN2( TurbineComponents%RotorFurl%Orientation(1,3), &
                SQRT( TurbineComponents%RotorFurl%Orientation(1,1)**2 + &
                      TurbineComponents%RotorFurl%Orientation(1,2)**2 ) )
          
   CTilt     = COS( Tilt )
   STilt     = SIN( Tilt )

      
      ! HubVDue2Yaw - yaw velocity due solely to yaw
      
  AvgVelNacelleRotorFurlYaw = TurbineComponents%RotorFurl%RotationVel(3) - TurbineComponents%Nacelle%RotationVel(3)
  AvgVelTowerBaseNacelleYaw = TurbineComponents%Nacelle%RotationVel(3)   - TurbineComponents%Tower%RotationVel(3)
  AvgVelTowerBaseYaw        = TurbineComponents%Tower%RotationVel(3)    
      
  rRotorFurlHub(1:2)        = TurbineComponents%Hub%Position(1:2) - TurbineComponents%RotorFurl%Position(1:2)
  rNacelleHub(1:2)          = TurbineComponents%Hub%Position(1:2) - TurbineComponents%Nacelle%Position(1:2)
  rTowerBaseHub(1:2)        = TurbineComponents%Hub%Position(1:2) - TurbineComponents%Tower%Position(1:2)
      
  YawVel =   ( AvgVelNacelleRotorFurlYaw * rRotorFurlHub(2) + AvgVelTowerBaseNacelleYaw * rNacelleHub(2) &
                    + AvgVelTowerBaseYaw * rTowerBaseHub(2) ) * SYaw &
           - ( AvgVelNacelleRotorFurlYaw * rRotorFurlHub(1) + AvgVelTowerBaseNacelleYaw * rNacelleHub(1) &
                    + AvgVelTowerBaseYaw * rTowerBaseHub(1) ) * CYaw
                               
!print *, 'YawAng=', YawAng, CurrentTurbineState%YawAngle
!print *, 'REVS=', REVS, CurrentTurbineState%RotorSpeed
!print *, 'Tilt=', Tilt, CurrentTurbineState%TiltAngle
!print *, 'YAWVEL=', YAWVEL, CurrentTurbineState%HubVDue2Yaw
   
   !.................................................................................................
   ! start of NewTime routine
   !.................................................................................................
   !CALL GetRotorParams (REVS, YawAng, YAWVEL, TILT)

   AvgInfl = SumInfl * 2.0 / (R*R*NB)        ! Compute average inflow from the previous time step
   SumInfl = 0.0                             ! reset to sum for the current time step

   CALL DiskVel()                            ! Get a sort of "Average velocity" - sets a bunch of stored variables...

   IF ( DStall ) CALL BedUpdate()            ! update the old values to hold "current" values from last time step

   ! Enter the dynamic inflow routines here
   IF ( Wake )  CALL Inflow()  !bjj: perhaps we should send NoLoadsCalculated to initialize dynamic inflow [subroutine Infinit()] instead of the check that time > 0...?

   !.................................................................................................
   ! end of NewTime routine
   !.................................................................................................


   DO IBlade = 1,NB
   
         ! calculate the azimuth angle ( we add pi because AeroDyn defines 0 as pointing downward)
         ! note: the equation below should use TurbineComponents%Blade markers, but this is used to get the 
         ! same answers as the previous version (before v13.00.00)                          
         
      AzimuthAngle = ATAN2( -1.*DOT_PRODUCT( TurbineComponents%Hub%Orientation(3,:),         & 
                                             TurbineComponents%RotorFurl%Orientation(2,:) ), &
                                DOT_PRODUCT( TurbineComponents%Hub%Orientation(3,:),         &
                                             TurbineComponents%RotorFurl%Orientation(3,:) )  ) + pi + (IBlade - 1)*TwoPiNB
      
      
!print *, 'Azimuth=', AzimuthAngle, CurrentTurbineState%AzimuthAngle(         IBlade)                                            

   
      DO IElement = 1,NElm
            
            ! calculate element pitch
                                            
         PitNow    = -1.*ATAN2( -1.*DOT_PRODUCT( TurbineComponents%Blade(IBlade)%Orientation(1,:),    &
                                                 InputMarkers%Blade(IElement,IBlade)%Orientation(2,:) ) , &
                                    DOT_PRODUCT( TurbineComponents%Blade(IBlade)%Orientation(1,:),    &
                                                 InputMarkers%Blade(IElement,IBlade)%Orientation(1,:) )   )
                    
         SPitch    = SIN( PitNow )
         CPitch    = COS( PitNow )

         
            ! calculate distance between hub and element
            
         tmpVector = InputMarkers%Blade(IElement,IBlade)%Position(:) - TurbineComponents%Hub%Position(:)
         rLocal = SQRT(   DOT_PRODUCT( tmpVector, TurbineComponents%Hub%Orientation(2,:) )**2  &
                        + DOT_PRODUCT( tmpVector, TurbineComponents%Hub%Orientation(3,:) )**2  )




!print *, 'IBlade, IElement= ', IBlade, IElement
!print *, 'PitNow= ', PitNow,       CurrentTurbineState%ElementPitch(IElement,IBlade)                                            
!print *, 'rLocal= ', rLocal,       CurrentTurbineState%rLocal(      IElement,IBlade)                                            
                      
            ! determine if MulTabLoc should be set.  
            ! bjj: I have no idea how this is really (supposed to be) used!!! .... 
            
         IF ( CurrentADOptions%SetMulTabLoc(IElement,IBlade)  ) THEN
            MulTabLoc = CurrentADOptions%MulTabLoc(IElement,IBlade)
         END IF
         

         !-------------------------------------------------------------------------------------------
         ! Get wind velocity components; calculate velocity normal to the rotor squared
         ! Save variables for printing in a file later;
         !-------------------------------------------------------------------------------------------
         VelocityVec(:)    = UsrWnd_GetWindVec(IElement, IBlade)
		 !VelocityVec(:)    = AD_WindVelocityWithDisturbance( InputMarkers%Blade(IElement,IBlade)%Position(:) )
         !VelocityVec(:)    = AD_WindVelocityWithDisturbance( InputMarkers%Blade(IElement,IBlade)%Position(:), IElement, IBlade )
		 VelNormalToRotor2 = ( VelocityVec(3) * STilt + (VelocityVec(1) * CYaw - VelocityVec(2) * SYaw) * CTilt )**2


         !-------------------------------------------------------------------------------------------
         ! reproduce GetVNVT routine:
         !-------------------------------------------------------------------------------------------
         tmpVector =  -1.*SPitch*InputMarkers%Blade(IElement,IBlade)%Orientation(1,:) +  &
                                       CPitch*InputMarkers%Blade(IElement,IBlade)%Orientation(2,:)
         VTTotal   =     DOT_PRODUCT( tmpVector, VelocityVec - InputMarkers%Blade(IElement,IBlade)%TranslationVel  )

         tmpVector =     CPitch*InputMarkers%Blade(IElement,IBlade)%Orientation(1,:) +   &
                                        SPitch*InputMarkers%Blade(IElement,IBlade)%Orientation(2,:)
         VNWind    =     DOT_PRODUCT( tmpVector, VelocityVec )
         VNElement = -1.*DOT_PRODUCT( tmpVector, InputMarkers%Blade(IElement,IBlade)%TranslationVel )  ! = DOT_PRODUCT( CurrentInputs%TransVel, -1.*Orientation )



! using old orientation:
!         VTTotal   =     DOT_PRODUCT( InputMarkers%Blade(IElement,IBlade)%Orientation(2,:),            &
!                                      VelocityVec - InputMarkers%Blade(IElement,IBlade)%TranslationVel  )
!         VNWind    =     DOT_PRODUCT( InputMarkers%Blade(IElement,IBlade)%Orientation(1,:), VelocityVec )
!         VNElement = -1.*DOT_PRODUCT( InputMarkers%Blade(IElement,IBlade)%Orientation(1,:),            &
!                                      InputMarkers%Blade(IElement,IBlade)%TranslationVel                )  ! = DOT_PRODUCT( CurrentInputs%TransVel, -1.*CurrentInputs%DCM(1,:) )


         !-------------------------------------------------------------------------------------------
         ! Get blade element forces and induced velocity
         !-------------------------------------------------------------------------------------------
         CALL ELEMFRC( AzimuthAngle, rLocal, IElement, IBlade, VelNormalToRotor2, VTTotal, VNWind, &
                       VNElement, DFN, DFT, PMA, NoLoadsCalculated )  

         !-------------------------------------------------------------------------------------------
         ! Set up dynamic inflow parameters
         !-------------------------------------------------------------------------------------------
         IF ( DynInfl .OR. DynInit ) THEN
            CALL GetRM (rLocal, DFN, DFT, AzimuthAngle, IElement, IBlade)
         ENDIF

         ADCurrentLoads%Blade(IElement,IBlade)%Force(1)  = ( DFN*CPitch + DFT*SPitch ) / DR(IElement)
         ADCurrentLoads%Blade(IElement,IBlade)%Force(2)  = ( DFN*SPitch - DFT*CPitch ) / DR(IElement)
         ADCurrentLoads%Blade(IElement,IBlade)%Force(3)  = 0.0

         ADCurrentLoads%Blade(IElement,IBlade)%Moment(1) = 0.0
         ADCurrentLoads%Blade(IElement,IBlade)%Moment(2) = 0.0
         ADCurrentLoads%Blade(IElement,IBlade)%Moment(3) = PMA / DR(IElement)


            ! save velocities for output, if requested

         IF ( WndElPrList(IElement) > 0 ) THEN
            SaveVX( WndElPrList(IElement), IBlade ) = VelocityVec(1)
            SaveVY( WndElPrList(IElement), IBlade ) = VelocityVec(2)
            SaveVZ( WndElPrList(IElement), IBlade ) = VelocityVec(3)
         ENDIF
         

      END DO !IElement

      IF ( IBlade == 1 .AND. ElemPrn ) THEN
         VXSAV  = VelocityVec(1)
         VYSAV  = VelocityVec(2)
         VZSAV  = VelocityVec(3)
      ENDIF


   END DO !IBlade

   NoLoadsCalculated = .FALSE.
   
   AD_CalculateLoads = ADCurrentLoads


END FUNCTION  AD_CalculateLoads
!====================================================================================================
FUNCTION AD_GetConstant(VarName, ErrStat)
!  This function returns a real scalar value whose name is listed in the VarName input argument.
!  If the name is not recognized, an error is returned in ErrStat.
!----------------------------------------------------------------------------------------------------

   USE                              AD_IOParams,   ONLY: UnADin
   USE                              AeroTime,      ONLY: DTAero
   USE                              Rotor,         ONLY: HH
   USE                              Wind,          ONLY: KinVisc, RHO

   CHARACTER(*),   INTENT(IN)    :: VarName
   INTEGER,        INTENT(OUT)   :: ErrStat
   REAL(ReKi)                    :: AD_GetConstant

   CHARACTER(20)                 :: VarNameUC


   !-------------------------------------------------------------------------------------------------
   ! Get the name of the requested variable in upper case
   !-------------------------------------------------------------------------------------------------
   VarNameUC = VarName
   CALL Conv2UC( VarNameUC )


   !-------------------------------------------------------------------------------------------------
   ! Check for parameter values that can be returned without checking initialization
   !-------------------------------------------------------------------------------------------------
   SELECT CASE ( TRIM(VarNameUC) )

      CASE ( 'UNADIN', 'ADUNIT' )
         AD_GetConstant = UnADin
         ErrStat = 0
         RETURN

   END SELECT


   !-------------------------------------------------------------------------------------------------
   ! Check that the module has been initialized.
   !-------------------------------------------------------------------------------------------------

   IF ( .NOT. Initialized ) THEN
      CALL WrScr( ' Initialialize AeroDyn before calling its subroutines.' )
      ErrStat = 1
      RETURN
   ELSE
      ErrStat = 0
   END IF

   !-------------------------------------------------------------------------------------------------
   ! Return the requested NON-PARAMETER values.
   !-------------------------------------------------------------------------------------------------


   SELECT CASE ( TRIM(VarNameUC) )

      CASE ( 'REFHT', 'HH' )  !BJJ: REFWINDHT  ???
         AD_GetConstant = HH

      CASE ( 'DT','DTAERO' )
         AD_GetConstant = DTAero

      CASE ( 'AIRDENSITY', 'RHO' )
         AD_GetConstant = RHO

      CASE ( 'KINVISC' )   !bjj: more descriptive name?
         AD_GetConstant = KinVisc

      CASE DEFAULT
         CALL WrScr( ' Invalid variable name in AD_GetConstant().' )
         ErrStat        = 1
         AD_GetConstant = 0.0

   END SELECT

END FUNCTION AD_GetConstant
!====================================================================================================
FUNCTION AD_GetCurrentValue(VarName, ErrStat, IBlade, IElement)
!  This function returns a real scalar value whose name is listed in the VarName input argument.
!  If the name is not recognized, an error is returned in ErrStat.
!----------------------------------------------------------------------------------------------------

   USE                           :: Rotor,      ONLY: AvgInfl
   USE                           :: ElemInflow, ONLY: W2, Alpha


   CHARACTER(*),      INTENT(IN) :: VarName
   INTEGER, OPTIONAL, INTENT(IN) :: IBlade
   INTEGER, OPTIONAL, INTENT(IN) :: IElement
   INTEGER,           INTENT(OUT):: ErrStat
   REAL(ReKi)                    :: AD_GetCurrentValue


   CHARACTER(20)                 :: VarNameUC



   !-------------------------------------------------------------------------------------------------
   ! Check that the module has been initialized.
   !-------------------------------------------------------------------------------------------------

   IF ( .NOT. Initialized ) THEN
      CALL WrScr( ' Initialialize AeroDyn before calling its subroutines.' )
      ErrStat = 1
      AD_GetCurrentValue = 0.0
      RETURN
   ELSE IF ( NoLoadsCalculated ) THEN
      CALL WrScr( ' Calculate aerodynamic loads before trying to return current values.' )
      ErrStat = 1
      AD_GetCurrentValue = 0.0
      RETURN
   ELSE
      ErrStat = 0
   END IF

   !-------------------------------------------------------------------------------------------------
   ! Return the requested values.
   !-------------------------------------------------------------------------------------------------

   VarNameUC = VarName
   CALL Conv2UC( VarNameUC )

   SELECT CASE ( TRIM(VarNameUC) )

      CASE ( 'AVGINFL', 'AVGINFLOW' )     !bjj this might not be necessary when tail fin aero is removed from structural code
         AD_GetCurrentValue = AvgInfl

      CASE ( 'W2' )
         IF (.NOT. PRESENT(IBlade) .OR. .NOT. PRESENT(IElement) ) THEN
            CALL WrScr( 'IBlade and IElement parameters are required to return W2 in AeroDyn/AD_GetCurrentValue')
            ErrStat = 1
            AD_GetCurrentValue = 0
         ELSE
            AD_GetCurrentValue = W2(IElement, IBlade )
         END IF

      CASE ( 'ALPHA' )
         IF (.NOT. PRESENT(IBlade) .OR. .NOT. PRESENT(IElement) ) THEN
            CALL WrScr( 'IBlade and IElement parameters are required to return ALPHA in AeroDyn/AD_GetCurrentValue')
            ErrStat = 1
            AD_GetCurrentValue = 0
         ELSE
            AD_GetCurrentValue = ALPHA(IElement, IBlade )
         END IF


      CASE DEFAULT
         CALL WrScr( ' Invalid variable name in AD_GetCurrentValue().' )
         ErrStat = 1
         AD_GetCurrentValue = 0.0

   END SELECT

END FUNCTION AD_GetCurrentValue
!====================================================================================================
FUNCTION AD_GetUndisturbedWind ( Time, InputPosition, ErrStat)
! This function returns the U-V-W wind speeds at the specified time and X-Y-Z location
!----------------------------------------------------------------------------------------------------

      ! Passed variables

   REAL(ReKi), INTENT(IN)   :: Time
   REAL(ReKi), INTENT(IN)   :: InputPosition(3)
   INTEGER,    INTENT(OUT)  :: ErrStat

      ! function definition

   REAL(ReKi)               :: AD_GetUndisturbedWind(3)

      ! local variables

   TYPE(InflIntrpOut)       :: InflowVel


   !-------------------------------------------------------------------------------------------------
   ! get the wind speed (the wind inflow module will check that it's initialized)
   !-------------------------------------------------------------------------------------------------
   InflowVel = WindInf_GetVelocity( Time, InputPosition, ErrStat)
   !InflowVel = WindInf_GetVelocity( Time, InputPosition, 1, 1, ErrStat)
   
   IF (ErrStat /=0) CALL ProgWarn( ' Error getting velocity in AeroDyn/AD_GetUndisturbedWind().' )

   AD_GetUndisturbedWind(:) = InflowVel%Velocity(:)

END FUNCTION AD_GetUndisturbedWind
!====================================================================================================
SUBROUTINE AD_Terminate(ErrStat)
! This subroutine is called at program termination.  It deallocates variables and closes files.
!----------------------------------------------------------------------------------------------------

   USE            AeroSubs
   USE            ElOutParams, ONLY: UnElem, UnWndOut

   INTEGER,       INTENT(OUT) :: ErrStat                    ! Determines if an error was encountered

   !-------------------------------------------------------------------------------------------------
   ! Terminate other modules
   !-------------------------------------------------------------------------------------------------
   CALL WindInf_Terminate( ErrStat )

   CALL AeroDyn_Terminate( )          ! rename this when we can

   !-------------------------------------------------------------------------------------------------
   ! Deallocate arrays
   !-------------------------------------------------------------------------------------------------

   IF ( ALLOCATED( ADCurrentLoads%Blade ) ) DEALLOCATE( ADCurrentLoads%Blade )

   !-------------------------------------------------------------------------------------------------
   ! Close any open files
   !-------------------------------------------------------------------------------------------------
   CLOSE( UnElem )
   CLOSE( UnWndOut )

   CALL CloseEcho()

   !-------------------------------------------------------------------------------------------------
   ! Reset the initialization flag
   !-------------------------------------------------------------------------------------------------
   Initialized       = .FALSE.
   NoLoadsCalculated = .TRUE.

   ErrStat     = 0


END SUBROUTINE AD_Terminate

!====================================================================================================

END MODULE AeroDyn
