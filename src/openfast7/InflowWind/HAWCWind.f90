MODULE HAWCWind
!
!  This module uses full-field binary wind files to determine the wind inflow.
!  This module assumes that the origin, (0,0,0), is located at the tower centerline at ground level,
!  and that all units are specified in the metric system (using meters and seconds).
!  Data is assumed periodic in the X direction (and thus not shifted like FFWind files are).
!
!  Created 25-June-2010 by B. Jonkman, National Renewable Energy Laboratory
!     using subroutines and modules from AeroDyn v12.58
!
!----------------------------------------------------------------------------------------------------

   USE      NWTC_Library
   USE      SharedInflowDefns

   IMPLICIT NONE

   PRIVATE                                                        ! By default, everything in HAWCWind is private (methods, data, types, etc.)


   REAL(ReKi), ALLOCATABLE          :: WindData  (:,:,:,:)        ! Array of FF data for all 3 wind components

   REAL(ReKi)                       :: deltaXInv                  ! multiplicative inverse of delta X
   REAL(ReKi)                       :: deltaYInv                  ! multiplicative inverse of delta Y
   REAL(ReKi)                       :: deltaZInv                  ! multiplicative inverse of delta Z

   INTEGER, PARAMETER               :: NC = 3                     ! number of wind components
   INTEGER                          :: NX                         ! number of points in the X direction
   INTEGER                          :: NY                         ! number of points in the Y direction
   INTEGER                          :: NZ                         ! number of points in the Z direction

   REAL(ReKi)                       :: GridBase                   ! the height of the bottom of the grid (Z direction) in meters
   REAL(ReKi)                       :: LengthX                    ! the grid length in the X direction (distance between point 1 and the next point 1 [because it is periodic])
   REAL(ReKi)                       :: LengthYHalf                ! half the grid width
   REAL(ReKi)                       :: RefHt                      ! the reference (hub) height of the grid in meters
   REAL(ReKi)                       :: URef                       ! the mean wind speed in m/s at height RefHt meters (as defined in the input file)


   LOGICAL, SAVE                    :: Initialized = .FALSE.      ! flag that determines if the module has been initialized


   PUBLIC                           :: HW_Init                    ! initialization subroutine to read the FF grids
   PUBLIC                           :: HW_GetWindSpeed            ! interpolation function that returns velocities at specified time and space
   PUBLIC                           :: HW_GetValue                ! interface to return requested values
   PUBLIC                           :: HW_Terminate               ! subroutine that deallocates memory stored in the FFWind module

CONTAINS
!====================================================================================================
SUBROUTINE HW_Init ( UnWind, InpFileName, ErrStat )
!  This routine is used read the full-field turbulence data stored in HAWC format.
!----------------------------------------------------------------------------------------------------

   IMPLICIT                       NONE


      ! Passed Variables:

   INTEGER,      INTENT(IN)    :: UnWind                       ! unit number for reading wind files
   INTEGER,      INTENT(OUT)   :: ErrStat                      ! determines if an error has been encountered

   CHARACTER(*), INTENT(IN)    :: InpFileName                  ! Name of the input text file

      ! Local Variables:

   REAL(SiKi)                  :: DumReal                      ! real variable to temporarially store values read from binary file

   REAL(ReKi)                  :: dx
   REAL(ReKi)                  :: dy
   REAL(ReKi)                  :: dz
   REAL(ReKi)                  :: PLExp                        ! Power law exponent, for the PL mean wind profile type
   REAL(ReKi)                  :: SF        (3)                ! The turbulence scale factors for the three components.
   REAL(ReKi)                  :: U                            ! The mean wind speed
   REAL(ReKi)                  :: Z                            ! The height above ground/sea level
   REAL(ReKi)                  :: Z0                           ! Surface layer roughness length in meters, used for LOG profile type


   INTEGER                     :: IC                           ! Loop counter for the number of wind components
   INTEGER                     :: IX                           ! Loop counter for the number of grid points in the X direction
   INTEGER                     :: IY                           ! Loop counter for the number of grid points in the Y direction
   INTEGER                     :: IZ                           ! Loop counter for the number of grid points in the Z direction

   CHARACTER( 1024 )           :: DataFiles ( 3 )              ! Names of the files containing the 3 wind components
   CHARACTER(3)                :: WindProfileType              ! character code of mean wind profile type


   !-------------------------------------------------------------------------------------------------
   ! Check that the module hasn't already been initialized.
   !-------------------------------------------------------------------------------------------------

   IF ( Initialized ) THEN
      CALL WrScr( ' HAWCWind has already been initialized.' )
      ErrStat = 1
      RETURN
   ELSE
      ErrStat = 0
      CALL NWTC_Init()
   END IF

! bjj: this (reading the file) should perhaps be in a subroutine...

   !-------------------------------------------------------------------------------------------------
   ! Open the text file
   !-------------------------------------------------------------------------------------------------

   CALL OpenFInpFile ( UnWind, TRIM(InpFileName), ErrStat )
   IF (ErrStat /= 0) RETURN


   !-------------------------------------------------------------------------------------------------
   ! Read some header information in the text file
   !-------------------------------------------------------------------------------------------------

   CALL ReadCom( UnWind, InpFileName, 'Header 1', ErrStat )
   IF (ErrStat /= 0) RETURN

   CALL ReadCom( UnWind, InpFileName, 'Header 2', ErrStat )
   IF (ErrStat /= 0) RETURN

   CALL ReadCom( UnWind, InpFileName, 'Header 3', ErrStat )
   IF (ErrStat /= 0) RETURN

   CALL ReadCom( UnWind, InpFileName, 'Header 4', ErrStat )
   IF (ErrStat /= 0) RETURN

   CALL ReadCom( UnWind, InpFileName, 'Parameters for HAWC-format binary files', ErrStat )
   IF (ErrStat /= 0) RETURN


   !-------------------------------------------------------------------------------------------------
   ! Read file names and scaling info from the file
   !-------------------------------------------------------------------------------------------------

   CALL ReadVar( UnWind, InpFileName, DataFiles(1), 'FileName_u', 'Name of the u-component binary file', ErrStat )
   IF (ErrStat /= 0) RETURN

   CALL ReadVar( UnWind, InpFileName, DataFiles(2), 'FileName_v', 'Name of the v-component binary file', ErrStat )
   IF (ErrStat /= 0) RETURN

   CALL ReadVar( UnWind, InpFileName, DataFiles(3), 'FileName_w', 'Name of the w-component binary file', ErrStat )
   IF (ErrStat /= 0) RETURN


   CALL ReadVar( UnWind, InpFileName, NX, 'nx', 'Number of grid points in the X direction', ErrStat )
   IF (ErrStat /= 0) RETURN
   IF ( NX < 1 ) THEN
      CALL WrScr ( ' HAWCWind error reading "nx": number of grid points in the X direction must be at least 1.')
      ErrStat = 1
      RETURN
   END IF


   CALL ReadVar( UnWind, InpFileName, NY, 'ny', 'Number of grid points in the Y direction', ErrStat )
   IF (ErrStat /= 0) RETURN
   IF ( NY < 1 ) THEN
      CALL WrScr ( ' HAWCWind error reading "ny": number of grid points in the Y direction must be at least 1.')
      ErrStat = 1
      RETURN
   END IF


   CALL ReadVar( UnWind, InpFileName, NZ, 'nz', 'Number of grid points in the Z direction', ErrStat )
   IF (ErrStat /= 0) RETURN
   IF ( NZ < 1 ) THEN
      CALL WrScr ( ' HAWCWind error reading "nz": number of grid points in the Z direction must be at least 1.')
      ErrStat = 1
      RETURN
   END IF


   CALL ReadVar( UnWind, InpFileName, dx, 'dx', 'Distance between two points in the X direction', ErrStat )
   IF (ErrStat /= 0) RETURN
   IF ( DX < 0.0_ReKi ) THEN
      CALL WrScr ( ' HAWCWind error reading "dx": the grid spacing in the X direction must be larger than 0.')
      ErrStat = 1
      RETURN
   END IF


   CALL ReadVar( UnWind, InpFileName, dy, 'dy', 'Distance between two points in the Y direction', ErrStat )
   IF (ErrStat /= 0) RETURN
   IF ( DY < 0.0_ReKi ) THEN
      CALL WrScr ( ' HAWCWind error reading "dy": the grid spacing in the Y direction must be larger than 0.')
      ErrStat = 1
      RETURN
   END IF


   CALL ReadVar( UnWind, InpFileName, dz, 'dz', 'Distance between two points in the Z direction', ErrStat )
   IF (ErrStat /= 0) RETURN
   IF ( DZ < 0.0_ReKi ) THEN
      CALL WrScr ( ' HAWCWind error reading "dz": the grid spacing in the Z direction must be larger than 0.')
      ErrStat = 1
      RETURN
   END IF


   CALL ReadVar( UnWind, InpFileName, RefHt, 'RefHt', 'Grid reference height', ErrStat )
   IF (ErrStat /= 0) RETURN
   IF ( RefHt < 0.0_ReKi ) THEN
      CALL WrScr ( ' HAWCWind error reading "RefHt": the grid reference height must be larger than 0.')
      ErrStat = 1
      RETURN
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Read the section to determine the turbulence scaling factors
   !-------------------------------------------------------------------------------------------------

   CALL ReadCom( UnWind, InpFileName, 'scaling parameters for turbulence', ErrStat )
   IF (ErrStat /= 0) RETURN


   CALL ReadVar( UnWind, InpFileName, SF(1), 'SF1', 'Turbulence scaling factor for the x direction', ErrStat )
   IF (ErrStat /= 0) RETURN
   IF ( SF(1) < 0.0_ReKi ) THEN
      CALL WrScr ( ' HAWCWind error: The turbulence scaling factor for the x direction, SF1, must not be negative.' )
      ErrStat = 1
      RETURN
   END IF


   CALL ReadVar( UnWind, InpFileName, SF(2), 'SF2', 'Turbulence scaling factor for the y direction', ErrStat )
   IF (ErrStat /= 0) RETURN
   IF ( SF(2) < 0.0_ReKi ) THEN
      CALL WrScr ( ' HAWCWind error: The turbulence scaling factor for the y direction, SF2, must not be negative.' )
      ErrStat = 1
      RETURN
   END IF


   CALL ReadVar( UnWind, InpFileName, SF(3), 'SF3', 'Turbulence scaling factor for the z direction', ErrStat )
   IF (ErrStat /= 0) RETURN
   IF ( SF(3) < 0.0_ReKi ) THEN
      CALL WrScr ( ' HAWCWind error: The turbulence scaling factor for the x direction, SF3, must not be negative.' )
      ErrStat = 1
      RETURN
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Read the section to determine the mean wind profile
   !-------------------------------------------------------------------------------------------------

   CALL ReadCom( UnWind, InpFileName, 'mean wind profile parameters (added to HAWC-format files)', ErrStat )
   IF (ErrStat /= 0) RETURN


   CALL ReadVar( UnWind, InpFileName, WindProfileType, 'WindProfileType', 'Wind profile type', ErrStat )
   IF (ErrStat /= 0) RETURN
   CALL Conv2UC ( WindProfileType )
   IF ( ( WindProfileType /= 'LOG' ) .AND. ( TRIM( WindProfileType ) /= 'PL' ) )  THEN
       CALL WrScr ( ' HAWCWind error: the wind profile type, WindProfileType, must be either "LOG" or "PL".' )
      ErrStat = 1
      RETURN
   END IF



   CALL ReadVar( UnWind, InpFileName, URef, 'URef', 'Reference wind speed', ErrStat )
   IF (ErrStat /= 0) RETURN
   IF ( URef < 0.0_ReKi ) THEN
      CALL WrScr ( ' HAWCWind error: the reference wind speed, URef, must not be negative.' )
      ErrStat = 1
      RETURN
   END IF


   CALL ReadVar( UnWind, InpFileName, PLExp, 'PLExp', 'Power law exponent', ErrStat )
   IF (ErrStat /= 0) RETURN


   CALL ReadVar( UnWind, InpFileName, Z0, 'Z0', 'Surface roughness length', ErrStat )
   IF (ErrStat /= 0) RETURN
   IF ( ( WindProfileType == 'LOG' ) .AND. ( Z0 <= EPSILON(Z0) ) ) THEN
      CALL WrScr ( ' HAWCWind error: the surface roughness length, Z0, must be greater than zero.' )
      ErrStat = 1
      RETURN
   END IF

   !-------------------------------------------------------------------------------------------------
   ! Close the file.
   !-------------------------------------------------------------------------------------------------

   CLOSE ( UnWind )


   !-------------------------------------------------------------------------------------------------
   ! Set some internal module parameters based on input file values
   !-------------------------------------------------------------------------------------------------

   LengthX     = dx*nx !(nx-1)   !because the turbulence box is periodic in the X direction, we need to consider the length between point 1 and the next point 1 (instead of between points 1 and nx)
   LengthYHalf = 0.5*dy*(ny-1)
   GridBase    = RefHt - 0.5*(nz-1)*dz

   IF ( GridBase <= 0.0 ) THEN
      CALL WrScr( ' HAWCWind error: the bottom of the grid is located at a height of '//&
                      TRIM( Num2LStr(GridBase) )//' meters, which is below the ground.' )
      ErrStat = 1
      RETURN
   END IF


   deltaXInv   = 1.0 / dx
   deltaYInv   = 1.0 / dy
   deltaZInv   = 1.0 / dz


   !-------------------------------------------------------------------------------------------------
   ! Allocate space for the wind arrays.
   !-------------------------------------------------------------------------------------------------

   IF (.NOT. ALLOCATED(WindData) ) THEN
      ALLOCATE( WindData( NZ, NY, NX, NC ), STAT=ErrStat )
      IF ( ErrStat /= 0 ) THEN
         CALL WrScr ( " Error allocating space for HAWCWind's WindData array." )
         RETURN
      END IF
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Read the 3 files containg the turbulent wind speeds.
   !-------------------------------------------------------------------------------------------------
!bjj: check these indices... they do not seem to be very consistant between the WAsP IEC Turbulence
!     simulator and documentation of OC3 file formats... the current implementation is from the
!     OC3/Kenneth Thompson documentation.

      ! The array must be filled so that x(i) < x(i+1), y(i) < y(i+1), and z(i) < z(i+1)
      ! Also, note that the time axis is the negative x axis.

   DO IC = 1,NC

      CALL OpenBInpFile ( UnWind, DataFiles(IC), ErrStat )

      DO IX = NX,1,-1                  ! Time is the opposite of X ....
         DO IY = NY,1,-1
            DO IZ = 1,NZ

               READ( UnWind, IOSTAT=ErrStat ) DumReal

               WindData( IZ, IY, IX, IC ) = SF(IC)*DumReal    ! possible type conversion here

               IF (ErrStat /= 0) THEN
                  CALL WrScr( ' Error reading binary data from "'//TRIM(DataFiles(IC))//'".' )
                  CALL WrScr( ' I/O error '//TRIM(Num2LStr(ErrStat))//' occurred at IZ='//TRIM(Num2LStr(IZ))//&
                                                   ', IY='//TRIM(Num2LStr(IY))//', IX='//TRIM(Num2LStr(IX))//'.' )
                  CLOSE ( UnWind )
                  RETURN
               END IF

            END DO
         END DO
      END DO

      CLOSE ( UnWind )

   END DO


   !-------------------------------------------------------------------------------------------------
   ! Add the mean wind speed to the u component.
   !-------------------------------------------------------------------------------------------------

   IF ( RefHt > 0.0 ) THEN

      DO IZ = 1,NZ

         Z = GridBase  + ( IZ - 1 )*dz

         SELECT CASE ( TRIM(WindProfileType) )

            CASE ( 'PL' )
               U = URef*( Z / RefHt )**PLExp      ! [IEC 61400-1 6.3.1.2 (10)]

            CASE ( 'LOG' )

               IF ( Z /= Z0 ) THEN
                  U = URef*( LOG( Z / Z0 ) )/( LOG( RefHt / Z0 ) )
               ELSE
                  U = 0.0
               ENDIF

            CASE DEFAULT

               CALL WrScr( ' Invalid wind profile type in HAWCWind.' )
               ErrStat = 1
               RETURN

         END SELECT

         WindData( IZ, :, :, 1 ) = WindData( IZ, :, :, 1 ) + U


      END DO ! IZ
   END IF ! RefHt


   !-------------------------------------------------------------------------------------------------
   ! Set initialized flag and return
   !-------------------------------------------------------------------------------------------------

   Initialized = .TRUE.

   RETURN

END SUBROUTINE HW_Init
!====================================================================================================
FUNCTION HW_GetValue(RVarName, ErrStat)
!  This function returns a real scalar value whose name is listed in the RVarName input argument.
!  If the name is not recognized, an error is returned in ErrStat.
!----------------------------------------------------------------------------------------------------

   CHARACTER(*),   INTENT(IN)    :: RVarName
   INTEGER,        INTENT(OUT)   :: ErrStat
   REAL(ReKi)                    :: HW_GetValue


   CHARACTER(20)                 :: VarNameUC


   !-------------------------------------------------------------------------------------------------
   ! Check that the module has been initialized.
   !-------------------------------------------------------------------------------------------------

   IF ( .NOT. Initialized ) THEN
      CALL WrScr( ' Initialialize the HAWCWind module before calling its subroutines.' )
      ErrStat = 1
      RETURN
   ELSE
      ErrStat = 0
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Return the requested values.
   !-------------------------------------------------------------------------------------------------

   VarNameUC = RVarName
   CALL Conv2UC( VarNameUC )

   SELECT CASE ( TRIM(VarNameUC) )

      CASE ( 'REFHEIGHT' )
         HW_GetValue = RefHt

      CASE ('GRIDWIDTH' )
         HW_GetValue = LengthYHalf*2

      CASE ('GRIDHEIGHT' )
         HW_GetValue = NZ/deltaZInv

      CASE ('UREF' )
         HW_GetValue = URef

      CASE DEFAULT
         CALL WrScr( ' HAWCWind error: invalid variable name in HW_GetRValue().' )
         ErrStat = 1

   END SELECT

END FUNCTION HW_GetValue
!!====================================================================================================
FUNCTION HW_GetWindSpeed(Time, InputPosition, ErrStat)
! This function receives time and position (in InputInfo) where (undisturbed) velocities are are
! requested.  It determines if the point is on the FF grid or tower points and calls the
! corresponding interpolation routine, which returns the velocities at the specified time and space.
!----------------------------------------------------------------------------------------------------

   REAL(ReKi),        INTENT(IN) :: Time
   REAL(ReKi),        INTENT(IN) :: InputPosition(3)
   INTEGER,           INTENT(OUT):: ErrStat
   TYPE(InflIntrpOut)            :: HW_GetWindSpeed


   !-------------------------------------------------------------------------------------------------
   ! Check that the module has been initialized.
   !-------------------------------------------------------------------------------------------------

   IF ( .NOT. Initialized ) THEN
      CALL WrScr( ' Initialialize the FFWind module before calling its subroutines.' )
      ErrStat = 1
      RETURN
   ELSE
      ErrStat = 0
   END IF


   !-------------------------------------------------------------------------------------------------
   ! interpolate and return the value.
   !-------------------------------------------------------------------------------------------------

    HW_GetWindSpeed%Velocity = HW_LinearInterp(Time,InputPosition, ErrStat)


END FUNCTION HW_GetWindSpeed
!====================================================================================================
FUNCTION HW_LinearInterp(Time, Position, ErrStat)
!    This function is used to interpolate into the full-field wind array for the given inputs. It receives
!    X, Y, Z and TIME from the calling routine.  It then computes a time shift in the X axis based upon
!    the average windspeed.  The modified position is used to decide which pair of X grids to interpolate
!    within and between.  After finding the two X slices, it decides which four grid points bound the
!    (Y,Z) pair.  It does a bilinear interpolation for each X slice. Linear interpolation is then used
!    to interpolate between the X slices.  This routine assumes that X is downwind, Y is to the left when
!    looking downwind and Z is up.  It also assumes that no extrapolation will be needed.
!
!    If tower points are used, it assumes the velocity at the ground is 0.  It interpolates between
!    heights and between time slices, but ignores the Y input.
!
!    09/23/09 - Modified by B. Jonkman to use arguments instead of modules to determine time and position.
!               Height is now relative to the ground
!
!----------------------------------------------------------------------------------------------------

   IMPLICIT                      NONE

   REAL(ReKi),      INTENT(IN) :: Position(3)       ! takes the place of XGrnd, YGrnd, ZGrnd
   REAL(ReKi),      INTENT(IN) :: Time
   REAL(ReKi)                  :: HW_LinearInterp(3)      ! The U, V, W velocities

   INTEGER,         INTENT(OUT):: ErrStat

      ! Local Variables:

   REAL(ReKi)                  :: ShiftedXPosition
   REAL(ReKi),PARAMETER        :: Tol = 1.0E-3      ! a tolerance for determining if two reals are the same (for extrapolation)
   REAL(ReKi)                  :: W_YH_Z
   REAL(ReKi)                  :: W_YH_ZH
   REAL(ReKi)                  :: W_YH_ZL
   REAL(ReKi)                  :: W_YL_Z
   REAL(ReKi)                  :: W_YL_ZH
   REAL(ReKi)                  :: W_YL_ZL
   REAL(ReKi)                  :: Wnd      (2)
   REAL(ReKi)                  :: X                 ! a value between 0 and 1 that indicates a relative location between IXLO and IXHI
   REAL(ReKi)                  :: XGRID             ! the position in the X direction relative to the first grid point
   REAL(ReKi)                  :: Y                 ! a value between 0 and 1 that indicates a relative location between IYLO and IYHI
   REAL(ReKi)                  :: YGRID             ! the position in the Y direction relative to the first grid point
   REAL(ReKi)                  :: Z                 ! a value between 0 and 1 that indicates a relative location between IZLO and IZHI
   REAL(ReKi)                  :: ZGRID             ! the position in the Z direction relative to the first grid point

   INTEGER                     :: IC                ! loop counter for number of grid points
   INTEGER                     :: IG                ! loop counter for X grids
   INTEGER                     :: IX                ! variable to store IXLO and IXHI while looping
   INTEGER                     :: IXHI              ! high index into the array in the X dimension
   INTEGER                     :: IXLO              ! low  index into the array in the X dimension
   INTEGER                     :: IYHI              ! high index into the array in the Y dimension
   INTEGER                     :: IYLO              ! low  index into the array in the Y dimension
   INTEGER                     :: IZHI              ! high index into the array in the Z dimension
   INTEGER                     :: IZLO              ! low  index into the array in the Z dimension


   !-------------------------------------------------------------------------------------------------
   ! Initialize variables
   !-------------------------------------------------------------------------------------------------

   HW_LinearInterp(:)    = 0.0                         ! the output velocities (in case NFFComp /= 3)
   Wnd(:)                = 0.0                         ! just in case we're on an end point

   !-------------------------------------------------------------------------------------------------
   ! Find the bounding X slices.
   !-------------------------------------------------------------------------------------------------

! bjj: should we shift by MIN(YHalfWid,FFZHWid)?

         ! Assume Taylor's Frozen Turbulence Hypothesis applies: u(X,Y,Z,t) = u( X-U*t, Y, Z, 0)

   ShiftedXPosition = Position(1) - TIME*URef      !this puts the first X grid point at the undeflected tower centerline


      ! The wind file is periodic so we'll translate this position to ( 0 <= ShiftedXPosition < LengthX )

   ShiftedXPosition = MODULO( ShiftedXPosition, LengthX )
   ! If ShiftedXPosition is a very small negative number, modulo returns the incorrect value due to internal rounding errors.
   ! See bug report #471
   IF (ShiftedXPosition == LengthX) ShiftedXPosition = 0.0_ReKi

   XGrid            = ShiftedXPosition*deltaXInv

   IXLO = INT( XGrid ) + 1             ! convert REAL to INTEGER, then add one since our grids start at 1, not 0

   IF ( IXLO == NX ) THEN
      IXHI = 1
   ELSE
      IXHI = IXLO + 1

! BJJ: assuming LengthX and NX have been correctly defined, this cannot happen:
!      IF ( IXLO > NX .OR. IXLO < 1 ) THEN
!            CALL WrScr( ' HAWCWind error: wind array was exhausted at '//TRIM( Num2LStr( REAL( TIME,   ReKi ) ) )//' seconds '//&
!                        '(trying to access X data at '//TRIM( Num2LStr( REAL( ShiftedXPosition, ReKi ) ) )//' m).'  )
!            ErrStat = 1
!            RETURN
!      ENDIF

   END IF

   X = XGrid - ( IXLO - 1 )         ! a value between 0 and 1 that indicates a relative location between IXLO and IXHI

   !-------------------------------------------------------------------------------------------------
   ! Find the bounding rows for the Z position. [The lower-left corner is (1,1) when looking upwind.]
   !-------------------------------------------------------------------------------------------------

   ZGRID = ( Position(3) - GridBase )*deltaZInv


   IZLO = INT( ZGRID ) + 1             ! convert REAL to INTEGER, then add one since our grids start at 1, not 0
   IZHI = IZLO + 1

   Z = ZGRID - ( IZLO - 1 )            ! a value between 0 and 1 that indicates a relative location between IZLO and IZHI

   IF ( IZLO < 1 ) THEN
      IF ( IZLO == 0 .AND. Z >= 1.0-TOL ) THEN
         Z    = 0.0
         IZLO = 1
      ELSE
         CALL WrScr( ' HAWCWind error: wind array boundaries violated. Grid too small in Z direction (Z='//&
                     TRIM(Num2LStr(Position(3)))//' m is below the grid).' )
         ErrStat = 1
         RETURN
      END IF
   ELSEIF ( IZLO >= NZ ) THEN
      IF ( IZLO == NZ .AND. Z <= TOL ) THEN
         Z    = 0.0
         IZHI = IZLO                   ! We're right on the last point, which is still okay
      ELSE
         CALL WrScr( ' HAWCWind error: wind array boundaries violated. Grid too small in Z direction (Z='//&
                     TRIM(Num2LStr(Position(3)))//' m is above the grid).' )
         ErrStat = 3
         RETURN
      END IF
   ENDIF


   !-------------------------------------------------------------------------------------------------
   ! Find the bounding columns for the Y position. [The lower-left corner is (1,1) when looking upwind.]
   !-------------------------------------------------------------------------------------------------

   YGRID = ( Position(2) + LengthYHalf )*deltaYInv    ! really, it's (Position(2) - -1.0*YHalfWid)

   IYLO = INT( YGRID ) + 1             ! convert REAL to INTEGER, then add one since our grids start at 1, not 0
   IYHI = IYLO + 1

   Y    = YGRID - ( IYLO - 1 )         ! a value between 0 and 1 that indicates a relative location between IYLO and IYHI

   IF ( IYLO >= NY .OR. IYLO < 1 ) THEN
      IF ( IYLO == 0 .AND. Y >= 1.0-TOL ) THEN
         Y    = 0.0
         IYLO = 1
      ELSE IF ( IYLO == NY .AND. Y <= TOL ) THEN
         Y    = 0.0
         IYHI = IYLO                   ! We're right on the last point, which is still okay
      ELSE
         CALL WrScr( ' HAWCWind error: wind array boundaries violated: Grid too small in Y direction. Y=' &
                        //TRIM(Num2LStr(Position(2)))//'; Y boundaries = ['//TRIM(Num2LStr(-1.0*LengthYHalf)) &
                        //', '//TRIM(Num2LStr(LengthYHalf))//']' )
         ErrStat = 2
         RETURN
      END IF
   ENDIF


   !-------------------------------------------------------------------------------------------------
   ! Interpolate on the Y-Z grid for each X (time) slice
   !-------------------------------------------------------------------------------------------------

   DO IC=1,NC            ! all the components

      IX = IXLO          ! start using the first time (X) slice

      DO IG = 1,2        ! repeat for 2 time slices (by changing the value of IX. note that we can't loop from IXLO to IXHI because they could be NX and 1 respectively)

         !-------------------------------------------------------------------------------------------
         ! Get the wind velocity values for the four corners of the grid for this time.
         !-------------------------------------------------------------------------------------------

         W_YL_ZL = WindData( IZLO, IYLO, IX, IC )
         W_YL_ZH = WindData( IZHI, IYLO, IX, IC )
         W_YH_ZL = WindData( IZLO, IYHI, IX, IC )
         W_YH_ZH = WindData( IZHI, IYHI, IX, IC )


         !-------------------------------------------------------------------------------------------
         ! Interpolate within the grid for this time.
         !-------------------------------------------------------------------------------------------

         W_YL_Z  = ( W_YL_ZH - W_YL_ZL )*Z + W_YL_ZL
         W_YH_Z  = ( W_YH_ZH - W_YH_ZL )*Z + W_YH_ZL
         Wnd(IG) = ( W_YH_Z  - W_YL_Z  )*Y + W_YL_Z

         IX = IXHI  ! repeat for the second time (X) slice

      END DO !IX

      !----------------------------------------------------------------------------------------------
      ! Interpolate between the two times.
      !----------------------------------------------------------------------------------------------

      HW_LinearInterp( IC ) = ( Wnd(2) - Wnd(1) ) * X + Wnd(1)    ! interpolated velocity

   END DO !IDIM


   RETURN

END FUNCTION HW_LinearInterp
!====================================================================================================
SUBROUTINE HW_Terminate( ErrStat )
!  This subroutine cleans up any data that is still allocated.  The (possibly) open files are
!  closed in InflowWindMod.
!----------------------------------------------------------------------------------------------------

   INTEGER,    INTENT(OUT)    :: ErrStat           ! return 0 if no errors; non-zero otherwise

   ErrStat = 0

   IF ( ALLOCATED( WindData  ) )   DEALLOCATE( WindData,  STAT=ErrStat )

   Initialized = .FALSE.


END SUBROUTINE HW_Terminate
!====================================================================================================
END MODULE HAWCWind
