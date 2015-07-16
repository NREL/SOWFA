! AeroDyn Modules
!=======================================================================
MODULE AD_IOParams

!bjj: why aren't these parameters?

   ! Contains input/output parameters.

INTEGER(4)                   :: UnADin  = 90 ! ipt file
INTEGER(4)                   :: UnADopt = 92 ! opt file
INTEGER(4)                   :: UnAirfl = 93 ! Airfoil data file
INTEGER(4)                   :: UnWind  = 91 ! HH or FF wind file

LOGICAL                      :: WrOptFile  = .TRUE.   ! Write the .opt file?


END MODULE AD_IOParams
!=======================================================================
MODULE AeroTime


   ! Contains aero calc information.


USE                             Precision


REAL(ReKi)                   :: OLDTIME   ! The previous time AeroDyn's loads were calculated
REAL(DbKi)                   :: TIME      ! Current time simulation time

REAL(ReKi)                   :: DT        ! actual difference between Time and OldTime when loads are calculated
REAL(ReKi)                   :: DTAERO    ! desired time interval for aerodynamics calculations


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

INTEGER   , PARAMETER        :: MAXTABLE = 10 !bjj: pjm increased this to 20
INTEGER   , ALLOCATABLE      :: NFOIL ( : )           ! indices of the airfoil data file used for each element
INTEGER   , ALLOCATABLE      :: NLIFT ( : )           ! Number of aerodata points in each airfoil file
INTEGER   , ALLOCATABLE      :: NTables  ( : )        ! number of airfoil data tables
INTEGER                      :: NumCL                 ! maximum number of aerodata points in all airfoil files {=max(NFoil(:)}
INTEGER                      :: NumFoil               ! number of different airfoil files used

CHARACTER(1024), ALLOCATABLE   :: FOILNM ( : )          ! names of the data files that contain airfoil data

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

!INTEGER                      :: NB              ! number of blades


!END MODULE Blade
!=======================================================================
MODULE DynInflow


   ! Contains dynamic inflow information.


USE                             Precision


INTEGER   , PARAMETER        :: MAXINFL  = 6
INTEGER   , PARAMETER        :: MAXINFL0 = 2
INTEGER                      :: MminR    ( maxInfl, maxInfl )
INTEGER                      :: MminusR  ( maxInfl, maxInfl )
INTEGER                      :: MplusR   ( maxInfl, maxInfl )
INTEGER                      :: MRvector ( maxInfl )
INTEGER                      :: NJvector ( maxInfl )

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

INTEGER                      :: NELM                ! Number of elements per blade (INPUT FILE)

END MODULE Element
!=======================================================================
MODULE ElemInflow


   ! Contains element specific information associated with the inflow.


USE                             Precision

REAL(ReKi), ALLOCATABLE      :: ALPHA(:,:)                                      ! Angle of attack                       of the inflow for the current blade, element, and time step.
REAL(ReKi), ALLOCATABLE      :: W2(:,:)                                         ! The square of the relative wind speed of the inflow for the current blade, element, and time step.

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

INTEGER                      :: UnWndOut = 96           ! The unit for wind output at each element on each blade
INTEGER                      :: NumWndElOut             ! Number of wind elements to print
INTEGER   , ALLOCATABLE      :: WndElPrList (:)
INTEGER   , ALLOCATABLE      :: WndElPrNum  (:)
INTEGER   , ALLOCATABLE      :: ElPrList (:)
INTEGER   , ALLOCATABLE      :: ElPrNum  (:)
INTEGER                      :: NumElOut
INTEGER                      :: UnElem = 94


END MODULE ElOutParams
!=======================================================================
MODULE ErrCount


   ! Contains error counters.


INTEGER                      :: NumErr
INTEGER                      :: NumWarn


END MODULE ErrCount
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


LOGICAL                      :: DSTALL       ! Dynamic stall model: TRUE = BEDDOES; FALSE = STEADY
LOGICAL                      :: DYNINFL      ! Dynamic inflow: TRUE = DYNIN; FALSE = EQUIL
LOGICAL                      :: DYNINIT
LOGICAL                      :: ELEMPRN
LOGICAL                      :: EquilDA
LOGICAL                      :: EquilDT
LOGICAL                      :: GTECH
LOGICAL                      :: HLOSS        ! Hub loss: TRUE = PRAND; FALSE = NONE
LOGICAL                      :: MultiTab
LOGICAL                      :: PMOMENT      ! Pitching moment: TRUE = USE_CM; FALSE = NO_CM
LOGICAL                      :: Reynolds
LOGICAL                      :: SIUNIT       ! TRUE = scientific units; FALSE = english
LOGICAL                      :: SKEW
LOGICAL                      :: SWIRL
LOGICAL                      :: TLOSS
LOGICAL                      :: WAKE


END MODULE Switch
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


REAL(ReKi)                   :: SHADHWID                                !
REAL(ReKi)                   :: TSHADC1                                 !
REAL(ReKi)                   :: TSHADC2                                 !
REAL(ReKi)                   :: TWRSHAD                                 !

REAL(ReKi)                   :: T_Shad_Refpt !This was a local variable -- with new tower influence, it should be removed


LOGICAL                      :: PJM_Version = .FALSE.

CHARACTER(1024)              :: TwrFile               ! Name of the tower properties input file

END MODULE TwrProps
!=======================================================================

MODULE Wind


   ! Module Wind is used for wind variables.


USE                             Precision


REAL(ReKi)                   :: ANGFLW                                  !
REAL(ReKi)                   :: CDEL                                    !
REAL(ReKi)                   :: KinVisc                                 ! KINEMATIC VISCOSITY   Units^2/SEC
REAL(ReKi)                   :: RHO                                     ! Ambient Air Density
REAL(ReKi)                   :: SDEL                                    !
REAL(ReKi)                   :: VROTORX                                 !
REAL(ReKi)                   :: VROTORY                                 !
REAL(ReKi)                   :: VROTORZ                                 !

END MODULE Wind
!=======================================================================

