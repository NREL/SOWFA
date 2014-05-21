! AeroDyn Modules

!BJJ: A new type of wind field has been added to AeroDyn in v12.57. When CTWindFlag is .TRUE.,
!BJJ    coherent turbulence is added to a background wind field. Thus, one other XXWindFlag
!BJJ    should also be .TRUE. when CTWindFlag is .TRUE. (Currently, FFWindFlag is the
!BJJ    only type that can be set when CTWindFlag is set, but that may change in the future.)
!BJJ    Please modify your codes accordingly. (Sorry!)

!DJL I moved Precision back to the top since it needs to be there
!DJL  in order to compile correctly in CVF GUI environment
!=======================================================================
!bjj Start of proposed change AD v12.70b-bjj
!rmMODULE Precision
!rm
!rm
!rm   ! This module stores constants to specify the KIND of variables.
!rm
!rm
!rmINTEGER(4), PARAMETER        :: DbKi     =  8                                   ! Default kind for double-precision numbers.
!rmINTEGER(4), PARAMETER        :: ReKi     =  4                                   ! Default kind for real numbers.
!rm ! NOTE: Use compile option "/real_size:64" (or "/4R8") when using ReKi = 8
!rm
!rmEND MODULE Precision
!bjj End of proposed change
!=======================================================================
MODULE AD_IOParams

!bjj: why aren't these parameters?

   ! Contains input/output parameters.


!not used, so rm: INTEGER(4)                   :: UnADec  = 94 ! Echo file
INTEGER(4)                   :: UnADin  = 90 ! ipt file
INTEGER(4)                   :: UnADopt = 92 ! opt file
INTEGER(4)                   :: UnAirfl = 93 ! Airfoil data file
INTEGER(4)                   :: UnWind  = 91 ! HH or FF wind file

!bjj start of proposed change (hopefully this file can be removed)
!BJJ RM INTEGER(4)                   :: UnErrLog = 99 ! The Error Log
!bjj end of proposed change

!bjj start of proposed change for pjm
!INTEGER                      :: UnTwr   = 89 ! tower aero data file
!bjj end proposed change for pjm



LOGICAL                      :: WrOptFile  = .TRUE.   ! Write the .opt file?
!bjj rm (not used): LOGICAL                      :: WrEchoFile = .FALSE.  ! Write an echo file?


END MODULE AD_IOParams
!=======================================================================
MODULE AeroTime


   ! Contains aero calc information.


USE                             Precision


!bjj start of proposed change:
!rmREAL(DbKi)                   :: OLDTIME   ! USED ONLY IN ADAMS2AD: The previous time AeroDyn was called
REAL(ReKi)                   :: OLDTIME   ! The previous time AeroDyn's loads were calculated
!bjj end of proposed change

REAL(DbKi)                   :: TIME      ! Current time simulation time

REAL(ReKi)                   :: DT        ! actual difference between Time and OldTime when loads are calculated
REAL(ReKi)                   :: DTAERO    ! desired time interval for aerodynamics calculations

!bjj rm:LOGICAL                      :: TIMFLAG   ! USED ONLY IN STRUCTURAL CODES: AeroDyn (force calcs) is called when this is TRUE


END MODULE AeroTime
!=======================================================================
MODULE Airfoil


   ! Contains airfoil information.


USE                             Precision


REAL(ReKi), ALLOCATABLE      :: AL    ( :, : )        ! Table of angles of attack
REAL(ReKi), ALLOCATABLE      :: CD    ( :, :, : )     ! Table of drag coefficients
REAL(ReKi), ALLOCATABLE      :: CL    ( :, :, : )     ! Table of lift coefficients
REAL(ReKi), ALLOCATABLE      :: CM    ( :, :, : )     ! Table of pitching moment coefficients
REAL(ReKi)                   :: MulTabLoc = 0.0
REAL(ReKi), ALLOCATABLE      :: MulTabMet ( :, :)
REAL(ReKi)                   :: PMC

INTEGER(4), PARAMETER        :: MAXTABLE = 10 !bjj: pjm increased this to 20
INTEGER(4), ALLOCATABLE      :: NFOIL ( : )           ! indices of the airfoil data file used for each element
INTEGER(4), ALLOCATABLE      :: NLIFT ( : )           ! Number of aerodata points in each airfoil file
INTEGER(4), ALLOCATABLE      :: NTables  ( : )        ! number of airfoil data tables
INTEGER(4)                   :: NumCL                 ! maximum number of aerodata points in all airfoil files {=max(NFoil(:)}
INTEGER(4)                   :: NumFoil               ! number of different airfoil files used

!bjj start of proposed change
!rmCHARACTER(80), ALLOCATABLE   :: FOILNM ( : )          ! names of the data files that contain airfoil data
CHARACTER(1024), ALLOCATABLE   :: FOILNM ( : )          ! names of the data files that contain airfoil data
!bjj end of proposed change

END MODULE Airfoil
!=======================================================================
MODULE Bedoes


   ! Contains Beddoes dynamic stall info.

!bjj: some "constants" could probably be parameters instead of set in BedDat()

USE                             Precision


REAL(ReKi), ALLOCATABLE      :: ADOT  ( :, : )
REAL(ReKi), ALLOCATABLE      :: ADOT1 ( :, : )
REAL(ReKi), ALLOCATABLE      :: AFE   ( :, : )
REAL(ReKi), ALLOCATABLE      :: AFE1  ( :, : )
REAL(ReKi)                   :: AN
REAL(ReKi), ALLOCATABLE      :: ANE   ( :, : )
REAL(ReKi), ALLOCATABLE      :: ANE1  ( :, : )
REAL(ReKi), ALLOCATABLE      :: AOD   ( :, : )
REAL(ReKi), ALLOCATABLE      :: AOL   ( :, : )
REAL(ReKi)                   :: AS              ! Speed of sound for Mach number calculation
REAL(ReKi)                   :: CC
REAL(ReKi), ALLOCATABLE      :: CDO   ( :, : )
REAL(ReKi)                   :: CMI
REAL(ReKi)                   :: CMQ
REAL(ReKi)                   :: CN
REAL(ReKi), ALLOCATABLE      :: CNA   ( :, : )
REAL(ReKi)                   :: CNCP
REAL(ReKi)                   :: CNIQ
REAL(ReKi), ALLOCATABLE      :: CNP   ( :, : )
REAL(ReKi), ALLOCATABLE      :: CNP1  ( :, : )
REAL(ReKi), ALLOCATABLE      :: CNPD  ( :, : )
REAL(ReKi), ALLOCATABLE      :: CNPD1 ( :, : )
REAL(ReKi), ALLOCATABLE      :: CNPOT ( :, : )
REAL(ReKi), ALLOCATABLE      :: CNPOT1( :, : )
REAL(ReKi), ALLOCATABLE      :: CNS   ( :, : )
REAL(ReKi), ALLOCATABLE      :: CNSL  ( :, : )
REAL(ReKi), ALLOCATABLE      :: CNV   ( :, : )
REAL(ReKi), ALLOCATABLE      :: CVN   ( :, : )
REAL(ReKi), ALLOCATABLE      :: CVN1  ( :, : )
REAL(ReKi), ALLOCATABLE      :: DF    ( :, : )
REAL(ReKi), ALLOCATABLE      :: DFAFE ( :, : )
REAL(ReKi), ALLOCATABLE      :: DFAFE1( :, : )
REAL(ReKi), ALLOCATABLE      :: DFC   ( :, : )
REAL(ReKi), ALLOCATABLE      :: DN    ( :, : )
REAL(ReKi), ALLOCATABLE      :: DPP   ( :, : )
REAL(ReKi), ALLOCATABLE      :: DQ    ( :, : )
REAL(ReKi), ALLOCATABLE      :: DQP   ( :, : )
REAL(ReKi), ALLOCATABLE      :: DQP1  ( :, : )
REAL(ReKi)                   :: DS
REAL(ReKi)                   :: FK
REAL(ReKi)                   :: FP
REAL(ReKi)                   :: FPC
REAL(ReKi), ALLOCATABLE      :: FSP   ( :, : )
REAL(ReKi), ALLOCATABLE      :: FSP1  ( :, : )
REAL(ReKi), ALLOCATABLE      :: FSPC  ( :, : )
REAL(ReKi), ALLOCATABLE      :: FSPC1 ( :, : )
REAL(ReKi), ALLOCATABLE      :: FTB   ( :, :, : )
REAL(ReKi), ALLOCATABLE      :: FTBC  ( :, :, : )
REAL(ReKi), ALLOCATABLE      :: OLDCNV( :, : )
REAL(ReKi), ALLOCATABLE      :: OLDDF ( :, : )
REAL(ReKi), ALLOCATABLE      :: OLDDFC( :, : )
REAL(ReKi), ALLOCATABLE      :: OLDDN ( :, : )
REAL(ReKi), ALLOCATABLE      :: OLDDPP( :, : )
REAL(ReKi), ALLOCATABLE      :: OLDDQ ( :, : )
REAL(ReKi), ALLOCATABLE      :: OLDTAU( :, : )
REAL(ReKi), ALLOCATABLE      :: OLDXN ( :, : )
REAL(ReKi), ALLOCATABLE      :: OLDYN ( :, : )
REAL(ReKi), ALLOCATABLE      :: QX    ( :, : )
REAL(ReKi), ALLOCATABLE      :: QX1   ( :, : )
REAL(ReKi), ALLOCATABLE      :: TAU   ( :, : )
REAL(ReKi)                   :: TF              ! Time constant applied to location of the separation point
REAL(ReKi)                   :: TP              ! Time constant for pressure lag
REAL(ReKi)                   :: TV              ! Time constant for strength of shed vortex
REAL(ReKi)                   :: TVL             ! Non-dimensional time of transit for the vortex moving across the airfoil surface
REAL(ReKi), ALLOCATABLE      :: XN    ( :, : )
REAL(ReKi), ALLOCATABLE      :: YN    ( :, : )

LOGICAL,    ALLOCATABLE      :: BEDSEP ( :, : )
LOGICAL,    ALLOCATABLE      :: OLDSEP ( :, : )
LOGICAL                      :: SHIFT
LOGICAL                      :: VOR


END MODULE Bedoes
!=======================================================================
!MODULE Blade

   ! Contains blade information.

!USE                             Precision

!REAL(ReKi), ALLOCATABLE      :: C       (:)     ! Chord of each blade element (FROM INPUT FILE)
!REAL(ReKi), ALLOCATABLE      :: DR      (:)     ! Span-wise width of the element (length of the element, centered at RELM(i)) (FROM INPUT FILE)
!REAL(ReKi)                   :: R               ! rotor radius
!INTEGER(4)                   :: NB              ! number of blades

!END MODULE Blade
!=======================================================================
!bjj Start of proposed change AD v12.70b-bjj
!rmMODULE Constant
!rm
!rm
!rm   ! Defines constant values.
!rm
!rm
!rmUSE                             Precision
!rm
!rm!bjj Start of proposed change v12.70
!rm!bjj: change the names of these values in preparation for NWTC_Library
!rm!rmREAL(DbKi)                   :: DtoR !bjj: Replace with D2R_D for DbKi
!rmREAL(DbKi)                   :: D2R
!rm!bjj End of proposed change
!rmREAL(DbKi)                   :: PI      !bjj: Replace with Pi_D for double precision
!rmREAL(DbKi)                   :: PIBY2   !bjj: Replace with PiBy2_D
!rm!bjj Start of proposed change AeroDyn v12.70
!rm!rmREAL(DbKi)                   :: RtoD    !bjj: Replace with R2D_D
!rmREAL(DbKi)                   :: R2D    !bjj: Replace with R2D_D
!rm!bjj End of proposed change AeroDyn v12.70
!rmREAL(DbKi)                   :: TWOPI   !bjj: Replace with TwoPi_D
!rm
!rmCHARACTER(1),PARAMETER       :: TAB = CHAR(9)
!rm
!rm
!rmEND MODULE Constant
!bjj End of proposed change AD v12.70b-bjj
!=======================================================================
!bjj start of proposed change v12.70w
!remove this module:
!MODULE CT_Wind
!
!
!   ! Defines variables used to hold coherent turbulent wind field, which will
!   ! be added to a background wind field to form the complete wind field.
!
!
!USE                            Precision
!
!REAL(ReKi)                   :: DelYCTgrid                                 ! The nondimensional distance between grid points in the y direction.
!REAL(ReKi)                   :: DelZCTgrid                                 ! The nondimensional distance between grid points in the z direction.
!REAL(ReKi)                   :: CTDistSc                                   ! Disturbance scale (ratio of wave height to rotor diameter).
!REAL(ReKi)                   :: CTOffset (3)                               ! Offsets to convert integer data to actual wind speeds.
!REAL(ReKi)                   :: CTScale  (3)                               ! Scaling factors to convert integer data to actual wind speeds.
!REAL(ReKi)                   :: CTTime   (2)                               ! Times for the CT wind files.
!REAL(ReKi), ALLOCATABLE      :: CTu      (:,:,:)                           ! The u-component array of CT wind data.
!REAL(ReKi), ALLOCATABLE      :: CTv      (:,:,:)                           ! The v-component array of CT wind data.
!REAL(ReKi), ALLOCATABLE      :: CTw      (:,:,:)                           ! The w-component array of CT wind data.
!REAL(ReKi)                   :: CTWind   (3)                               ! Three components of wind at an analysis point.
!REAL(ReKi)                   :: CTLy                                       ! Fractional location of tower centerline from right (looking downwind) to left side of the dataset.
!REAL(ReKi)                   :: CTLz                                       ! Fractional location of hub height from bottom to top of dataset.
!REAL(ReKi)                   :: CTScaleVel                                   ! Scaling velocity, U0.  2*U0 is the difference in wind speed between the top and bottom of the wave.
!REAL(ReKi), ALLOCATABLE      :: TimesCT   (:)                              ! The list of times for the CT-wind input files.
!REAL(ReKi)                   :: T_CT_En                                    ! Time at which the wave event ends.
!REAL(ReKi)                   :: T_CT_St                                    ! Time at which the wave event starts.
!REAL(ReKi)                   :: UOff                                       ! u-component offset for converting from integers to reals.
!REAL(ReKi)                   :: UScl                                       ! u-component scale factor for converting from integers to reals.
!REAL(ReKi)                   :: VOff                                       ! v-component offset for converting from integers to reals.
!REAL(ReKi)                   :: VScl                                       ! v-component scale factor for converting from integers to reals.
!REAL(ReKi)                   :: WOff                                       ! w-component offset for converting from integers to reals.
!REAL(ReKi)                   :: WScl                                       ! w-component scale factor for converting from integers to reals.
!REAL(ReKi)                   :: CTYHWid                                    ! The half the width of the background dataset, used to compute the CTwind time offset
!REAL(ReKi)                   :: CTYmax                                     ! The dimensional lateral width of the dataset.
!REAL(ReKi)                   :: CTYt                                       ! Distance of the tower from the right side of the dataset (looking downwind).
!REAL(ReKi)                   :: CTZmax                                     ! The dimensional vertical height of the dataset.
!REAL(ReKi)                   :: CTZt                                       ! Distance of the hub from the bottom of the dataset.
!REAL(ReKi)                   :: MeanCTWS                                   ! The mean hub height wind speed for the CT wind data
!REAL(ReKi)                   :: InvMCTWS                                   ! The multiplicative inverse of MeanCTWS
!
!INTEGER(4)                   :: CT_DF_Y                                    ! The decimation factor for the CT wind data in the y direction.
!INTEGER(4)                   :: CT_DF_Z                                    ! The decimation factor for the CT wind data in the z direction.
!INTEGER(4)                   :: CTFileNo                                   ! The CT wind file number.
!INTEGER(4)                   :: CTIndxNo                                   ! The CT wind file index number.
!INTEGER(4)                   :: CTRecL                                     ! The length, in bytes, of the LE binary records.
!INTEGER(4)                   :: IndCTnew                                   ! Index of the newest CT wind file.
!INTEGER(4)                   :: IndCTold                                   ! Index of the older CT wind file.
!INTEGER(4)                   :: NumCTt                                     ! The number of CT wind grids, no more than one grid per time step.
!INTEGER(4)                   :: NumCTy                                     ! The number of CT wind grid points in the y direction.
!INTEGER(4)                   :: NumCTyD                                    ! The decimated number of CT wind grid points in the y direction.
!INTEGER(4)                   :: NumCTyD1                                   ! The decimated number of CT wind grid points in the y direction minus 1.
!INTEGER(4)                   :: NumCTz                                     ! The number of CT wind grid points in the z direction.
!INTEGER(4)                   :: NumCTzD                                    ! The decimated number of CT wind grid points in the z direction.
!INTEGER(4)                   :: NumCTzD1                                   ! The decimated number of CT wind grid points in the z direction minus 1.
!INTEGER(4), ALLOCATABLE      :: TimeStpCT (:)                              ! The list of time steps from the original LE simulation, associated with the CT-wind times.
!
!LOGICAL                      :: CTVertShft                                 ! Flag to indicate whether or not to shift the z values for the w component.
!
!CHARACTER(99)                :: CTbackgr                                   ! The name of the background wind data
!CHARACTER(3)                 :: CText                                      ! The extension used for coherent turbulence data files. (usually "les" or "dns")
!CHARACTER(3)                 :: CTSCext                                    ! The extension used for coherent turbulence scale files.(usually "les", "dns", or "dat")
!CHARACTER(99)                :: CTPFile                                    ! The name of the CT wind parameter file.
!CHARACTER(99)                :: CTSpath                                    ! The path to the CT wind files.
!CHARACTER(99)                :: CTTSfile                                   ! The name of the file containing the time-step history of the wind files.
!
!
!END MODULE CT_Wind
!bjj end of proposed change v12.70w
!=======================================================================
MODULE DynInflow


   ! Contains dynamic inflow information.


USE                             Precision


INTEGER(4), PARAMETER        :: MAXINFL  = 6
INTEGER(4), PARAMETER        :: MAXINFL0 = 2
INTEGER(4)                   :: MminR    ( maxInfl, maxInfl )
INTEGER(4)                   :: MminusR  ( maxInfl, maxInfl )
INTEGER(4)                   :: MplusR   ( maxInfl, maxInfl )
INTEGER(4)                   :: MRvector ( maxInfl )
INTEGER(4)                   :: NJvector ( maxInfl )

REAL(ReKi)                   :: dAlph_dt ( maxInfl, 4 )
REAL(ReKi)                   :: dBeta_dt ( maxInfl0+1 : maxInfl, 4 )
REAL(ReKi)                   :: DT0
REAL(ReKi)                   :: GAMMA    ( maxInfl, maxInfl )
REAL(ReKi)                   :: old_Alph (              maxInfl )
REAL(ReKi)                   :: old_Beta ( maxInfl0+1 : maxInfl )
REAL(ReKi)                   :: old_LmdM
REAL(ReKi)                   :: oldKai
REAL(ReKi)                   :: PhiLqC   (              maxInfl )
REAL(ReKi)                   :: PhiLqS   ( maxInfl0+1 : maxInfl )
REAL(ReKi)                   :: Pzero
REAL(ReKi), ALLOCATABLE      :: RMC_SAVE( : , :, : )  !Store element parameters for GDW
REAL(ReKi), ALLOCATABLE      :: RMS_SAVE( : , :, : )
REAL(ReKi)                   :: TipSpeed
REAL(ReKi)                   :: totalInf
REAL(ReKi)                   :: Vparam
REAL(ReKi)                   :: Vtotal
REAL(ReKi)                   :: xAlpha   (              maxInfl )
REAL(ReKi)                   :: xBeta    ( maxInfl0+1 : maxInfl )
REAL(ReKi)                   :: xKai
REAL(ReKi)                   :: XLAMBDA_M
REAL(ReKi)                   :: xLcos    ( maxInfl, maxInfl )
REAL(ReKi)                   :: xLsin    ( maxInfl0+1 : maxInfl , maxInfl0+1 : maxInfl )
REAL(ReKi)                   :: xMinv    ( maxInfl )


END MODULE DynInflow
!=======================================================================
MODULE Element


   ! Contains element specific information.


USE                             Precision


REAL(ReKi), ALLOCATABLE      :: A       (:,:)       ! induction factor?
REAL(ReKi), ALLOCATABLE      :: AP      (:,:)
REAL(ReKi), ALLOCATABLE      :: HLCNST  (:)         ! Hub-loss constant at each element
REAL(ReKi)                   :: PITNOW
REAL(ReKi), ALLOCATABLE      :: RELM    (:)         ! Location of the center of the element; measured from the blade root. (INPUT FILE) Supposedly ignored by ADAMS.
REAL(ReKi), ALLOCATABLE      :: TLCNST  (:)         ! Tip-loss constant at each element
REAL(ReKi), ALLOCATABLE      :: TWIST   (:)         ! Twist of each blade element  (INPUT FILE)

INTEGER(4)                   :: NELM                ! Number of elements per blade (INPUT FILE)
!bjj rmINTEGER(4)                   :: JElem               ! Current element number


END MODULE Element
!=======================================================================
MODULE ElemInflow


   ! Contains element specific information associated with the inflow.


USE                             Precision

!BJJ START OF PROPOSED CHANGE
!RMREAL(ReKi)                   :: ALPHA                                           ! Angle of attack                       of the inflow for the current blade, element, and time step.
!RMREAL(ReKi)                   :: W2                                              ! The square of the relative wind speed of the inflow for the current blade, element, and time step.

REAL(ReKi), ALLOCATABLE      :: ALPHA(:,:)                                      ! Angle of attack                       of the inflow for the current blade, element, and time step.
REAL(ReKi), ALLOCATABLE      :: W2(:,:)                                         ! The square of the relative wind speed of the inflow for the current blade, element, and time step.
!BJJ end of proposed change

END MODULE ElemInflow
!=======================================================================
MODULE ElOutParams


   ! Contains element output information.


USE                             Precision


REAL(ReKi), ALLOCATABLE      :: AAA     ( : )
REAL(ReKi), ALLOCATABLE      :: AAP     ( : )
REAL(ReKi), ALLOCATABLE      :: ALF     ( : )
REAL(ReKi), ALLOCATABLE      :: CDD     ( : )
REAL(ReKi), ALLOCATABLE      :: CLL     ( : )
REAL(ReKi), ALLOCATABLE      :: CMM     ( : )
REAL(ReKi), ALLOCATABLE      :: CNN     ( : )
REAL(ReKi), ALLOCATABLE      :: CTT     ( : )
REAL(ReKi), ALLOCATABLE      :: DFNSAV  ( : )
REAL(ReKi), ALLOCATABLE      :: DFTSAV  ( : )
REAL(ReKi), ALLOCATABLE      :: DynPres ( : )
REAL(ReKi), ALLOCATABLE      :: PITSAV  ( : )
REAL(ReKi), ALLOCATABLE      :: PMM     ( : )
REAL(ReKi), ALLOCATABLE      :: ReyNum  ( : )
REAL(ReKi)                   :: VXSAV
REAL(ReKi)                   :: VYSAV
REAL(ReKi)                   :: VZSAV

REAL(ReKi), ALLOCATABLE      :: SaveVX  ( :,: )         ! The velocity in the x direction at requested element, on each blade
REAL(ReKi), ALLOCATABLE      :: SaveVY  ( :,: )         ! The velocity in the y direction at requested element, on each blade
REAL(ReKi), ALLOCATABLE      :: SaveVZ  ( :,: )         ! The velocity in the z direction at requested element, on each blade

!bjj start of proposed change (this number is duplicated in Adams!)
!INTEGER(4)                   :: UnWndOut = 95           ! The unit for wind output at each element on each blade
INTEGER(4)                   :: UnWndOut = 96           ! The unit for wind output at each element on each blade
!bjj end of proposed change
INTEGER(4)                   :: NumWndElOut             ! Number of wind elements to print
INTEGER(4), ALLOCATABLE      :: WndElPrList (:)
INTEGER(4), ALLOCATABLE      :: WndElPrNum  (:)

INTEGER(4), ALLOCATABLE      :: ElPrList (:)
INTEGER(4), ALLOCATABLE      :: ElPrNum  (:)
INTEGER(4)                   :: NumElOut
INTEGER(4)                   :: UnElem = 94


END MODULE ElOutParams
!=======================================================================
MODULE ErrCount


   ! Contains error counters.


INTEGER(4)                   :: NumErr
INTEGER(4)                   :: NumWarn


END MODULE ErrCount
!=======================================================================
!bjj start of proposed change v12.70w
!bjj: remove this module:
!MODULE FD_Wind
!
!
!   ! Defines variables used to hold large-eddy wind data & info.
!
!
!USE                             Precision
!
!!bjj: NOTE: CoefTE, Grav, and Ri are not used anywhere except when they're read from the input file.
!!bjj:       FDTimStp, Inv4DdT, Inv4DdX, Inv4DdY, Inv4DdZ, Zbot are computed, but never used elsewhere
!!bjj:       Zm_maxo is no longer necessary
!
!REAL(ReKi)                   :: CoefTE                                     ! Coefficient of thermal expansion.
!REAL(ReKi)                   :: DelXgrid                                   ! The nondimensional distance between grid points in the x direction.
!REAL(ReKi)                   :: DelYgrid                                   ! The nondimensional distance between grid points in the y direction.
!REAL(ReKi)                   :: DelZgrid                                   ! The nondimensional distance between grid points in the z direction.
!REAL(ReKi)                   :: DistScal                                   ! Disturbance scale (ratio of wave height to rotor diameter).
!REAL(ReKi)                   :: FDper                                      ! Total time in dataset.
!REAL(ReKi)                   :: FDTime   (2)                               ! Times for the 4D wind files.
!REAL(ReKi)                   :: FDTimStp                                   ! Average time step for 4D wind data.
!REAL(ReKi), ALLOCATABLE      :: FDu      (:,:,:,:)                         ! The u-component array of 4D wind data.
!REAL(ReKi), ALLOCATABLE      :: FDv      (:,:,:,:)                         ! The v-component array of 4D wind data.
!REAL(ReKi), ALLOCATABLE      :: FDw      (:,:,:,:)                         ! The w-component array of 4D wind data.
!REAL(ReKi), ALLOCATABLE      :: FDuData  (:,:,:,:)                         ! The u-component array of all 4D wind data when used with advection.
!REAL(ReKi), ALLOCATABLE      :: FDvData  (:,:,:,:)                         ! The v-component array of all 4D wind data when used with advection.
!REAL(ReKi), ALLOCATABLE      :: FDwData  (:,:,:,:)                         ! The w-component array of all 4D wind data when used with advection.
!REAL(ReKi)                   :: FDWind   (3)                               ! Three components of wind at an analysis point.
!REAL(ReKi)                   :: Grav                                       ! Gravitational acceleration.
!REAL(ReKi)                   :: Inv4DdT                                    ! Inverse of normalized time step.
!REAL(ReKi)                   :: Inv4DdX                                    ! Inverse of normalized delta X.
!REAL(ReKi)                   :: Inv4DdY                                    ! Inverse of normalized delta Y.
!REAL(ReKi)                   :: Inv4DdZ                                    ! Inverse of normalized delta Z.
!REAL(ReKi)                   :: LenScale                                   ! Length scale (h).
!REAL(ReKi)                   :: Lx                                         ! Fractional location of tower centerline from upwind end to downwind end of the dataset.
!REAL(ReKi)                   :: Ly                                         ! Fractional location of tower centerline from right (looking downwind) to left side of the dataset.
!REAL(ReKi)                   :: Lz                                         ! Fractional location of hub height from bottom to top of dataset.
!REAL(ReKi)                   :: Offsets  (3)                               ! Offsets to convert integer data to actual wind speeds.
!REAL(ReKi)                   :: Ri                                         ! Richardson number.
!REAL(ReKi)                   :: RotDiam                                    ! Rotor diameter.
!REAL(ReKi)                   :: ScalFact (3)                               ! Scaling factors to convert integer data to actual wind speeds.
!REAL(ReKi)                   :: ScaleVel                                   ! Scaling velocity, U0.  2*U0 is the difference in wind speed between the top and bottom of the wave.
!REAL(ReKi), ALLOCATABLE      :: Times4D  (:)                               ! The list of times for the 4D-wind input files.
!REAL(ReKi)                   :: Tm_max                                     ! The total nondimensional time of the dataset.
!REAL(ReKi)                   :: TSclFact                                   ! Scale factor for time (h/U0).
!REAL(ReKi)                   :: T_4D_En                                    ! Time at which the wave event ends.
!REAL(ReKi)                   :: T_4D_St                                    ! Time at which the wave event starts.
!REAL(ReKi)                   :: Ubot                                       ! Steady u-component wind speed at the bottom of the wave.
!REAL(ReKi)                   :: UOff                                       ! u-component offset for converting from integers to reals.
!REAL(ReKi)                   :: UScl                                       ! u-component scale factor for converting from integers to reals.
!REAL(ReKi)                   :: VOff                                       ! v-component offset for converting from integers to reals.
!REAL(ReKi)                   :: VScl                                       ! v-component scale factor for converting from integers to reals.
!REAL(ReKi)                   :: WOff                                       ! w-component offset for converting from integers to reals.
!REAL(ReKi)                   :: WScl                                       ! w-component scale factor for converting from integers to reals.
!REAL(ReKi)                   :: Xm_max                                     ! The nondimensional downwind length of the dataset.
!REAL(ReKi)                   :: Xmax                                       ! The dimensional downwind length of the dataset.
!REAL(ReKi)                   :: Xt                                         ! Distance of the tower from the upwind end of the dataset.
!REAL(ReKi)                   :: Ym_max                                     ! The nondimensional lateral width of the dataset.
!REAL(ReKi)                   :: Ymax                                       ! The dimensional lateral width of the dataset.
!REAL(ReKi)                   :: Yt                                         ! Distance of the tower from the right side of the dataset (looking downwind).
!REAL(ReKi)                   :: Zbot                                       ! Height of the bottom of the wave.
!REAL(ReKi)                   :: Zm_max                                     ! The nondimensional vertical height of the dataset.
!REAL(ReKi)                   :: Zm_maxo                                    ! The nondimensional vertical height of the untrimmed dataset.
!REAL(ReKi)                   :: Zmax                                       ! The dimensional vertical height of the dataset.
!REAL(ReKi)                   :: Zt                                         ! Distance of the hub from the bottom of the dataset.
!
!INTEGER(4)                   :: FD_DF_X                                    ! The decimation factor for the 4D wind data in the x direction.
!INTEGER(4)                   :: FD_DF_Y                                    ! The decimation factor for the 4D wind data in the y direction.
!INTEGER(4)                   :: FD_DF_Z                                    ! The decimation factor for the 4D wind data in the z direction.
!INTEGER(4)                   :: FDFileNo                                   ! The 4D wind file number.
!INTEGER(4)                   :: FDRecL                                     ! The length, in bytes, of the LE binary records.
!INTEGER(4)                   :: Ind4DAdv = 1                               ! Index of the file to be used in advection
!INTEGER(4)                   :: Ind4Dnew                                   ! Index of the newest 4D wind file.
!INTEGER(4)                   :: Ind4Dold                                   ! Index of the older 4D wind file.
!INTEGER(4)                   :: Num4Dt                                     ! The number of 4D wind grids, one grid per time step.
!INTEGER(4)                   :: Num4DtD = 2                                ! The number of 4D wind grids stored in memory, normally 2
!INTEGER(4)                   :: Num4Dx                                     ! The number of 4D wind grid points in the x direction.
!INTEGER(4)                   :: Num4DxD                                    ! The decimated number of 4D wind grid points in the x direction.
!INTEGER(4)                   :: Num4DxD1                                   ! The decimated number of 4D wind grid points in the x direction minus 1.
!INTEGER(4)                   :: Num4Dy                                     ! The number of 4D wind grid points in the y direction.
!INTEGER(4)                   :: Num4DyD                                    ! The decimated number of 4D wind grid points in the y direction.
!INTEGER(4)                   :: Num4DyD1                                   ! The decimated number of 4D wind grid points in the y direction minus 1.
!INTEGER(4)                   :: Num4Dz                                     ! The number of 4D wind grid points in the z direction.
!INTEGER(4)                   :: Num4DzD                                    ! The decimated number of 4D wind grid points in the z direction.
!INTEGER(4)                   :: Num4DzD1                                   ! The decimated number of 4D wind grid points in the z direction minus 1.
!INTEGER(4)                   :: NumAdvect                                  ! Number of frozen timesteps to advect past the turbine
!INTEGER(4)                   :: Shft4Dnew                                  ! Number of times the x-data needs to be shifted for advection
!INTEGER(4), ALLOCATABLE      :: Times4DIx (:)                              ! Index number of the 4D time files (used for advection)
!
!LOGICAL                      :: Advect                                     ! Flag to indicate whether or not to advect a given data set or to just use the time step files
!LOGICAL                      :: VertShft                                   ! Flag to indicate whether or not to shift the z values for the w component.
!
!CHARACTER(5), ALLOCATABLE    :: AdvFiles (:)
!CHARACTER(99)                :: FDPFile                                    ! The name of the 4D wind parameter file.
!CHARACTER(99)                :: FDSpath                                    ! The path to the 4D wind files.
!CHARACTER(99)                :: FDTSfile                                   ! The name of the file containing the time-step history of the wind files.
!
!
!END MODULE FD_Wind
!bjj end of proposed change v12.70w
!=======================================================================
!bjj start of proposed change v12.70w
!bjj removed this module:
!MODULE FF_Wind
!
!
!   ! Defines variables used to hold full-field wind data & info.
!
!
!USE                             Precision
!
!
!REAL(ReKi), ALLOCATABLE      :: FFData  (:,:,:,:)
!REAL(ReKi)                   :: FFDTime
!REAL(ReKi)                   :: FFRate
!REAL(ReKi)                   :: FFStep
!REAL(ReKi)                   :: FFWind  (3)
!REAL(ReKi)                   :: FFYHWid
!REAL(ReKi)                   :: FFZHWid
!REAL(ReKi)                   :: InvFFYD
!REAL(ReKi)                   :: InvFFZD
!REAL(ReKi)                   :: InvMFFWS
!REAL(ReKi)                   :: MeanFFWS
!REAL(ReKi)                   :: TI      (3)
!REAL(ReKi)                   :: ZGOffset ! The vertical offset of the turbine on rectangular grid
!
!INTEGER(4)                   :: NFFComp
!INTEGER(4)                   :: NFFSteps
!INTEGER(4)                   :: NYGrids
!INTEGER(4)                   :: NZGrids
!
!
!END MODULE FF_Wind
!bjj end  of proposed change v12.70w
!=======================================================================
!bjj start of proposed change
!rmMODULE Identify
!rm
!rm
!rm   ! Defines variables to indicate program & version.
!rm
!rm
!rm!bjj rm:CHARACTER(  8)               :: AeroProg
!rm!bjj rm:CHARACTER( 25)               :: AeroVer
!rmCHARACTER( 23)               :: Creator
!rm!bjj rmCHARACTER(  7)               :: DynProg
!rm!bjj rmCHARACTER( 99)               :: DynProgRoot   ! The root name of the dynamic program input and output files.
!rm!bjj rmCHARACTER( 26)               :: DynVer
!rm!bjj rmCHARACTER(100)               :: Prog
!rm
!rm
!rmEND MODULE Identify
!bjj end of proposed change
!=======================================================================
MODULE InducedVel


   ! Contains induced velocity information.


USE                             Precision


REAL(ReKi)                   :: ATOLER                                   ! Convergence tolerance for induction factor
REAL(ReKi)                   :: EqAIDmult                                ! Multiplier for the drag term in the axial-induction equation.
REAL(ReKi)                   :: SumInfl = 0.0                            ! Initialize this value here for the first pass


END MODULE InducedVel
!=======================================================================
MODULE Rotor


   ! Contains rotor configuration information.


USE                             Precision


REAL(ReKi)                   :: AVGINFL         ! average induduced velocity at the previous time
REAL(ReKi)                   :: CTILT
REAL(ReKi)                   :: CYaw
REAL(ReKi)                   :: HH
REAL(ReKi)                   :: REVS
REAL(ReKi)                   :: STILT
REAL(ReKi)                   :: SYaw
REAL(ReKi)                   :: TILT
REAL(ReKi)                   :: YawAng
REAL(ReKi)                   :: YAWVEL


END MODULE Rotor
!=======================================================================
MODULE Switch


   ! Defines variables to control program options.


!bjj start of proposed change v12.70w
!rmLOGICAL                      :: CTWindFlag = .FALSE.
!bjj end of proposed change v12.70w
LOGICAL                      :: DSTALL       ! Dynamic stall model: TRUE = BEDDOES; FALSE = STEADY
LOGICAL                      :: DYNINFL      ! Dynamic inflow: TRUE = DYNIN; FALSE = EQUIL
LOGICAL                      :: DYNINIT
LOGICAL                      :: ELEMPRN
LOGICAL                      :: EquilDA
LOGICAL                      :: EquilDT
!bjj start of proposed change v12.70w
!rmLOGICAL                      :: FDWindFlag = .FALSE.
!rmLOGICAL                      :: FFWindFlag = .FALSE.
!bjj end of proposed change v12.70w
LOGICAL                      :: GTECH
!bjj start of proposed change v12.70w
!rmLOGICAL                      :: HHWindFlag = .FALSE.
!bjj end of proposed change v12.70w
LOGICAL                      :: HLOSS        ! Hub loss: TRUE = PRAND; FALSE = NONE
LOGICAL                      :: MultiTab
LOGICAL                      :: PMOMENT      ! Pitching moment: TRUE = USE_CM; FALSE = NO_CM
LOGICAL                      :: Reynolds
LOGICAL                      :: SIUNIT       ! TRUE = scientific units; FALSE = english
LOGICAL                      :: SKEW
LOGICAL                      :: SWIRL
LOGICAL                      :: TLOSS

!bjj start of proposed change v12.70w
!rmLOGICAL                      :: UsrWndFlag = .FALSE.
!bjj end of proposed change v12.70w
LOGICAL                      :: WAKE


END MODULE Switch
!bjj start of proposed change for pjm
!Begin change v12.58pjm-ce
!=======================================================================
MODULE TwrProps


   ! Contains tower aero information.


USE                             Precision


REAL(ReKi), ALLOCATABLE      :: TwrHtFr ( : )
REAL(ReKi), ALLOCATABLE      :: TwrWid  ( : )
REAL(ReKi), ALLOCATABLE      :: TwrCD   ( :, : )
REAL(ReKi), ALLOCATABLE      :: TwrRe   ( : )

REAL(ReKi)                   :: VTwr(3)
REAL(ReKi)                   :: Tower_Wake_Constant   ! Constant for tower wake model = 0 full potential flow = 0.1 model of Bak et al.


INTEGER,    ALLOCATABLE      :: NTwrCDCol (:)         ! The tower CD column that represents a particular tower height
INTEGER                      :: NTwrHt                ! The number of tower height rows in the table
INTEGER                      :: NTwrRe                ! The number of tower Re entry rows in the table
INTEGER                      :: NTwrCD                ! The number of tower CD columns in the table

LOGICAL                      :: TwrPotent             ! Tower potential flow calculation
LOGICAL                      :: TwrShadow             ! Tower Shadow calculation
!bjj rm:LOGICAL                      :: TwrRead               ! Read in tower drag file
!bjj rm:LOGICAL                      :: TwrGrid               ! Tower FF file existence


!************bjj start of proposed change for pjm************************
!These were the OLD values, stored in WIND
REAL(ReKi)                   :: SHADHWID                                !
REAL(ReKi)                   :: TSHADC1                                 !
REAL(ReKi)                   :: TSHADC2                                 !
REAL(ReKi)                   :: TWRSHAD                                 !

REAL(ReKi)                   :: T_Shad_Refpt !This was a local variable -- with new tower influence, it should be removed

!************end of proposed change for pjm************************

LOGICAL                      :: PJM_Version = .FALSE.

!bjj start of proposed change
!rmCHARACTER(300)               :: TwrFile               ! Name of the tower properties input file
CHARACTER(1024)              :: TwrFile               ! Name of the tower properties input file
!bjj end of proposed change

END MODULE TwrProps
!End change v12.58pjm-ce
!bjj end of proposed change for pjm
!=======================================================================

MODULE Wind


   ! Module Wind is used for wind variables.


USE                             Precision


REAL(ReKi)                   :: ANGFLW                                  !
REAL(ReKi)                   :: CDEL                                    !
!bjj start of proposed change v12.70w
!rmREAL(ReKi)                   :: DELTA                                   ! HH Wind direction (angle)
!rmREAL(ReKi)                   :: HSHR                                    ! HH Horizontal linear shear
!bjj end of proposed change v12.70w
REAL(ReKi)                   :: KinVisc                                 ! KINEMATIC VISCOSITY   Units^2/SEC
REAL(ReKi)                   :: RHO                                     ! Ambient Air Density
REAL(ReKi)                   :: SDEL                                    !
!************bjj start of proposed change for pjm************************
!REAL(ReKi)                   :: SHADHWID                                !
!REAL(ReKi)                   :: TSHADC1                                 !
!REAL(ReKi)                   :: TSHADC2                                 !
!REAL(ReKi)                   :: TWRSHAD                                 !
!************end start of proposed change for pjm************************

!bjj start of proposed change v12.70w
!rmREAL(ReKi)                   :: UWmeanU                                 ! Possibly instantaneous, disk-averaged wind speeds from UserWind().
!rmREAL(ReKi)                   :: UWmeanV                                 !
!rmREAL(ReKi)                   :: UWmeanW                                 !
!rmREAL(ReKi)                   :: V                                       ! HH horizontal wind speed
!rmREAL(ReKi)                   :: VGUST                                   ! HH wind gust
!rmREAL(ReKi)                   :: VLINSHR                                 ! HH vertical linear shear
!bjj end of proposed change v12.70w
REAL(ReKi)                   :: VROTORX                                 !
REAL(ReKi)                   :: VROTORY                                 !
REAL(ReKi)                   :: VROTORZ                                 !
!bjj start of proposed change v12.70w
!rmREAL(ReKi)                   :: VSHR                                    ! HH vertical shear exponent
!bjj end of proposed change v12.70w

!bjj rm with new AD_CalculateLoads:
!rmREAL(ReKi)                   :: VX                                      ! wind, including tower shadow, along the X axis
!rmREAL(ReKi)                   :: VY                                      ! wind, including tower shadow, along the Y axis
!rmREAL(ReKi)                   :: VZ                                      ! wind, including tower shadow, along the Z axis
!bjj end rm with new AD_CalculateLoads

!rm bjj: REAL(ReKi)                   :: XGRND                                   ! Location of aerodynamic analysis point along X axis wrt hub height on the tower centerline.
!rm bjj: REAL(ReKi)                   :: YGRND                                   ! Location of aerodynamic analysis point along X axis wrt hub height on the tower centerline.
!rm bjj: REAL(ReKi)                   :: ZGRND                                   ! Location of aerodynamic analysis point along X axis wrt hub height on the tower centerline.

!bjj start of proposed change v12.70w
!rmCHARACTER(96 )               :: FFWindFile                              ! Name of the FF wind file
!rmCHARACTER(100)               :: HHWindFile                              ! Name of the HH wind file
!RMCHARACTER(300)               :: WindFile                                 ! Name of the wind file
!bjj end of proposed change v12.70w


END MODULE Wind
!=======================================================================


