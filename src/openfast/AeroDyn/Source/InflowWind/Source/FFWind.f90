MODULE FFWind
!
!  This module uses full-field binary wind files to determine the wind inflow.
!  This module assumes that the origin, (0,0,0), is located at the tower centerline at ground level,
!  and that all units are specified in the metric system (using meters and seconds).
!  Data is shifted by half the grid width to account for turbine yaw (so that data in the X 
!  direction actually starts at -1*FFYHWid meters).
!
!  Created 25-Sept-2009 by B. Jonkman, National Renewable Energy Laboratory
!     using subroutines and modules from AeroDyn v12.58
!
!----------------------------------------------------------------------------------------------------

   USE      NWTC_Library
   USE      SharedInflowDefns

   IMPLICIT NONE

   PRIVATE                                                        ! By default, everything in FFWind is private (methods, data, types, etc.)
   
      ! former FF_Wind module
      
   REAL(ReKi), ALLOCATABLE          :: FFData  (:,:,:,:)          ! Array of FF data
   REAL(ReKi), ALLOCATABLE          :: FFtower (:,:,:)            ! Array of data along the tower, below the FF array

   REAL(ReKi)                       :: FFDTime                    ! delta time
   REAL(ReKi)                       :: FFRate                     ! data rate in Hz (1/FFDTime)
   REAL(ReKi)                       :: FFYHWid                    ! half the grid width
   REAL(ReKi)                       :: FFZHWid                    ! half the grid height
   REAL(ReKi)                       :: RefHt                      ! the reference (hub) height of the grid in meters 
   REAL(ReKi)                       :: GridBase                   ! the height of the bottom of the grid in meters
   REAL(ReKi)                       :: InvFFYD                    ! reciprocal of delta y
   REAL(ReKi)                       :: InvFFZD                    ! reciprocal of delta z
   REAL(ReKi)                       :: InvMFFWS                   ! reciprocal of the mean wind speed (MeanFFWS)
   REAL(ReKi)                       :: MeanFFWS                   ! the mean wind speed (as defined in the FF file), not necessarially the mean of the portion of the wind used

   INTEGER                          :: NFFComp                    ! number of wind components
   INTEGER                          :: NFFSteps                   ! number of time steps in the FF array
   INTEGER                          :: NYGrids                    ! number of points in the lateral (y) direction of the grids
   INTEGER                          :: NZGrids                    ! number of points in the vertical (z) direction of the grids
   INTEGER                          :: NTGrids                    ! number of points in the vertical (z) direction on the tower (below the grids)
         
   LOGICAL, SAVE                    :: Initialized = .FALSE.      ! flag that determines if the module has been initialized


   INTERFACE FF_GetValue
      MODULE PROCEDURE FF_GetRValue                               ! routine to return scalar real values 
   END INTERFACE


   PUBLIC                           :: FF_Init                    ! initialization subroutine to read the FF grids
   PUBLIC                           :: FF_GetWindSpeed            ! interpolation function that returns velocities at specified time and space
   PUBLIC                           :: FF_GetValue                ! interface to return requested values
   PUBLIC                           :: FF_Terminate               ! subroutine that deallocates memory stored in the FFWind module

CONTAINS
!====================================================================================================
SUBROUTINE FF_Init ( UnWind, BinFile, ErrStat )
!  This routine is used read the full-field turbulence data.
!  09/25/97 - Created by M. Buhl from GETFILES in ViewWind.
!  09/23/09 - modified by B. Jonkman: this subroutine was split into several subroutines (was ReadFF)
!----------------------------------------------------------------------------------------------------

   IMPLICIT                       NONE


      ! Passed Variables:
      
   INTEGER,      INTENT(IN)    :: UnWind                       ! unit number for reading wind files
   INTEGER,      INTENT(OUT)   :: ErrStat                      ! determines if an error has been encountered
   
   CHARACTER(*), INTENT(IN)    :: BinFile                      ! Name of the binary FF wind file

      ! Local Variables:

   REAL(ReKi)                  :: TI      (3)                  ! turbulence intensities of the wind components as defined in the FF file, not necessarially the actual TI
   REAL(ReKi)                  :: BinTI   (3)                  ! turbulence intensities of the wind components as defined in the FF binary file, not necessarially the actual TI
   REAL(ReKi)                  :: UBar
   REAL(ReKi)                  :: ZCenter
   
   INTEGER(B2Ki)               :: Dum_Int2
   INTEGER                     :: DumInt
   INTEGER                     :: I
   LOGICAL                     :: CWise
   LOGICAL                     :: Exists
   CHARACTER( 1028 )           :: SumFile                      ! length is LEN(BinFile) + the 4-character extension.
   CHARACTER( 1028 )           :: TwrFile                      ! length is LEN(BinFile) + the 4-character extension.


      !----------------------------------------------------------------------------------------------
      ! Check that the module hasn't already been initialized.
      !----------------------------------------------------------------------------------------------   
      
   IF ( Initialized ) THEN  
      CALL WrScr( ' FFWind has already been initialized.' )
      ErrStat = 1
      RETURN
   ELSE
      ErrStat = 0
      CALL NWTC_Init()
   END IF   


      !----------------------------------------------------------------------------------------------
      ! Open the binary file, read its "header" (first 2-byte integer) to determine what format
      ! binary file it is, and close it.
      !----------------------------------------------------------------------------------------------

   CALL OpenBInpFile (UnWind, TRIM(BinFile), ErrStat)
   IF (ErrStat /= 0) RETURN
      
   READ (UnWind)  Dum_Int2

   CLOSE( UnWind )
   
   
      !----------------------------------------------------------------------------------------------
      ! Read the files to get the required FF data.
      !----------------------------------------------------------------------------------------------   
   DumInt = Dum_Int2  ! change to default INTEGER, instead of INT(2) to compare in SELECT below
    
   SELECT CASE (DumInt)  
   
      CASE ( 7 )                                                       !        TurbSim binary format
         
         CALL Read_TurbSim_FF(UnWind, TRIM(BinFile), ErrStat)
         
      CASE ( -1, -2, -3, -99 )                                         !   Bladed-style binary format
      
         !...........................................................................................
         ! Create full-field summary file name from binary file root name.  Also get tower file
         ! name.
         !...........................................................................................

            CALL GetRoot(BinFile, SumFile)
            
            TwrFile = TRIM(SumFile)//'.twr'
            SumFile = TRIM(SumFile)//'.sum'
      
         !...........................................................................................
         ! Read the summary file to get necessary scaling information
         !...........................................................................................
   
            CALL Read_Summary_FF (UnWind, TRIM(SumFile), CWise, ZCenter, TI, ErrStat ) 
            IF (ErrStat /= 0) RETURN
            
            UBar = MeanFFWS      ! temporary storage .... this is our only check to see if the summary and binary files "match"

         !...........................................................................................
         ! Open the binary file and read its header
         !...........................................................................................
         
            CALL OpenBInpFile (UnWind, TRIM(BinFile), ErrStat)

            IF (ErrStat /= 0) RETURN

            IF ( Dum_Int2 == -99 ) THEN                                       ! Newer-style BLADED format
               CALL Read_Bladed_FF_Header1 (UnWind, BinTI, ErrStat)    
               
                  ! If the TIs are also in the binary file (BinTI > 0), 
                  ! use those numbers instead of ones from the summary file
                  
               DO I =1,NFFComp                  
                  IF ( BinTI(I) > 0 ) TI(I) = BinTI(I)
               END DO
               
            ELSE
               CALL Read_Bladed_FF_Header0 (UnWind, ErrStat)                  ! Older-style BLADED format
            END IF

            IF (ErrStat /= 0) RETURN

         !...........................................................................................
         ! Let's see if the summary and binary FF wind files go together before continuing.
         !...........................................................................................
               
            IF ( ABS( UBar - MeanFFWS ) > 0.1 )  THEN
               CALL WrScr( ' Error: Incompatible mean hub-height wind speeds in FF wind files. '//&
                           '(Check that the .sum and .wnd files were generated together.)' )
               ErrStat = 1
               RETURN
            ENDIF

         !...........................................................................................
         ! Calculate the height of the bottom of the grid
         !...........................................................................................

            GridBase = ZCenter - FFZHWid         ! the location, in meters, of the bottom of the grid

         !...........................................................................................
         ! Read the binary grids (converted to m/s) and close the file
         !...........................................................................................

            CALL Read_Bladed_Grids( UnWind, CWise, TI, ErrStat) 
            CLOSE ( UnWind )
                           
            IF ( ErrStat /= 0 ) RETURN
   
         !...........................................................................................
         ! Read the tower points file
         !...........................................................................................
   
            INQUIRE ( FILE=TRIM(TwrFile) , EXIST=Exists )

            IF (  Exists )  THEN  
               CALL Read_FF_Tower( UnWind, TRIM(TwrFile), ErrStat  )
            ELSE
               NTgrids = 0            
            END IF                        

                               
      CASE DEFAULT
         
         CALL WrScr( ' Error: Unrecognized binary wind file type.' )
         ErrStat = 1
         RETURN
                  
   END SELECT


   Initialized = .TRUE.

   RETURN
   
END SUBROUTINE FF_Init
!====================================================================================================
SUBROUTINE Read_Bladed_FF_Header0 (UnWind, ErrStat)
!   Reads the binary headers from the turbulence files of the old Bladed variety.  Note that
!   because of the normalization, neither NZGrids or NYGrids are larger than 32 points.
!   21-Sep-2009 - B. Jonkman, NREL/NWTC.
!----------------------------------------------------------------------------------------------------


   IMPLICIT                      NONE


      ! Passed Variables:
      
   INTEGER,   INTENT(IN)      :: UnWind
   INTEGER,   INTENT(OUT)     :: ErrStat

      ! Local Variables:

   REAL(ReKi)                 :: FFXDelt
   REAL(ReKi)                 :: FFYDelt
   REAL(ReKi)                 :: FFZDelt

   INTEGER(B2Ki)              :: Dum_Int2

   INTEGER                    :: I
!rm not used:   INTEGER                    :: TurbType

   !-------------------------------------------------------------------------------------------------
   ! Read the header (file has just been opened)
   !-------------------------------------------------------------------------------------------------

   READ (UnWind, IOSTAT=ErrStat)  Dum_Int2                                                   ! -NFFC (file ID)

      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading number of wind components from binary FF file.' )
         RETURN
      END IF
      NFFComp = -1*Dum_Int2
         

   READ (UnWind, IOSTAT=ErrStat) Dum_Int2                                                    ! delta z (mm)
    
      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading dz from binary FF file.' )
         RETURN
      END IF
      FFZDelt = 0.001*Dum_Int2
      InvFFZD = 1.0/FFZDelt


   READ (UnWind, IOSTAT=ErrStat)  Dum_Int2                                                   ! delta y (mm)
   
      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading dy from binary FF file.' )
         RETURN
      END IF
      FFYDelt = 0.001*Dum_Int2
      InvFFYD = 1.0/FFYDelt


   READ (UnWind, IOSTAT=ErrStat)  Dum_Int2                                                   ! delta x (mm)
   
      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading dx from binary FF file.' )
         RETURN
      END IF
      FFXDelt = 0.001*Dum_Int2


   READ (UnWind, IOSTAT=ErrStat)  Dum_Int2                                                   ! half the number of time steps
   
      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading number of time steps from binary FF file.' )
         RETURN
      END IF
      NFFSteps = 2*Dum_Int2


   READ (UnWind, IOSTAT=ErrStat)  Dum_Int2                                                   ! 10 times the mean full-field wind speed
   
      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading mean full-field wind speed from binary FF file.' )
         RETURN
      END IF
      MeanFFWS = 0.1*Dum_Int2
      InvMFFWS = 1.0/MeanFFWS
      FFDTime  = FFXDelt/MeanFFWS
      FFRate   = 1.0/FFDTime

      
   DO I = 1,5   
   
      READ (UnWind, IOSTAT=ErrStat)  Dum_Int2                                                ! unused variables: zLu, yLu, xLu, dummy, random seed

         IF (ErrStat /= 0) THEN
            CALL WrScr( ' Error reading 2-byte integers from binary FF file.' )
            RETURN
         END IF
         
   END DO


   READ (UnWind, IOSTAT=ErrStat)  Dum_Int2                                                   ! 1000*nz

      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading nz from binary FF file.' )
         RETURN
      END IF
      NZGrids  = Dum_Int2/1000
      FFZHWid  = 0.5*FFZDelt*( NZGrids - 1 )    ! half the vertical size of the grid


   READ (UnWind, IOSTAT=ErrStat)  Dum_Int2                                                   ! 1000*ny
   
      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading ny from binary FF file.' )
         RETURN
      END IF   
      NYGrids  = Dum_Int2/1000
      FFYHWid  = 0.5*FFYDelt*( NYGrids - 1 )


   IF (NFFComp == 3) THEN
   
      DO I=1,6
      
         READ (UnWind, IOSTAT=ErrStat)  Dum_Int2                                             ! unused variables: zLv, yLv, xLv, zLw, yLw, xLw
         
            IF (ErrStat /= 0) THEN
               CALL WrScr( ' Error reading 2-byte length scales from binary FF file.' )
               RETURN
            END IF
         
      ENDDO !I
      
   END IF !NFFComp


   RETURN
   
END SUBROUTINE Read_Bladed_FF_Header0
!====================================================================================================
!bjj rm SIunit:SUBROUTINE Read_Bladed_FF (UnWind,ConvFact,StrMFFWS,UBar,TFFSteps,SNYGrids,SNZGrids)
SUBROUTINE Read_Bladed_FF_Header1 (UnWind, TI, ErrStat)
!   Reads the binary headers from the turbulence files of the new Bladed variety.
!   16-May-2002 - Windward Engineering.
!   21-Sep-2009 - B. Jonkman, NREL.  updated to trap errors and add extra parameters for MANN model
!----------------------------------------------------------------------------------------------------


   IMPLICIT                      NONE


      ! Passed Variables:
      
   INTEGER,   INTENT(IN)      :: UnWind
   REAL(ReKi), INTENT(OUT)    :: TI(3)
   INTEGER,   INTENT(OUT)     :: ErrStat

      ! Local Variables:

   REAL(ReKi)                 :: FFXDelt
   REAL(ReKi)                 :: FFYDelt
   REAL(ReKi)                 :: FFZDelt

   REAL(SiKi)                 :: Dum_Real4
   INTEGER(B2Ki)              :: Dum_Int2
   INTEGER(B4Ki)              :: Dum_Int4

   INTEGER                    :: I
   INTEGER                    :: TurbType


   TI(:) = -1                                                                                !Initialize to -1 (not all models contain TI)

   READ (UnWind, IOSTAT=ErrStat)  Dum_Int2                                                   ! -99 (file ID)

      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading integer from binary FF file.' )
         RETURN
      END IF

   
   READ (UnWind, IOSTAT=ErrStat)  Dum_Int2                                                   ! turbulence type

      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading turbulence type from binary FF file.' )
         RETURN
      END IF
      TurbType = Dum_Int2


   SELECT CASE (TurbType)
      CASE(1, 2) 
         !----------------------------------------
         !1-component Von Karman (1) or Kaimal (2)
         !----------------------------------------
            NFFComp = 1
                  
      CASE(3, 5) 
         !----------------------------------------
         !3-component Von Karman (3) or IEC-2 
         ! Kaimal (5)
         !----------------------------------------
            NFFComp = 3
         
      CASE(4) 
         !----------------------------------------
         !improved Von Karman
         !----------------------------------------
      
            READ (UnWind, IOSTAT=ErrStat) Dum_Int4                                           ! number of components (should be 3)
            
               IF (ErrStat /= 0) THEN           
                  CALL WrScr( ' Error reading number of components from binary FF file.' )
                  RETURN
               END IF
               NFFComp = Dum_Int4
            
            READ (UnWind, IOSTAT=ErrStat) Dum_Real4                                          ! Latitude (deg)
            
               IF (ErrStat /= 0) THEN
                  CALL WrScr( ' Error reading latitude from binary FF file.' )  
                  RETURN
               END IF

            READ (UnWind, IOSTAT=ErrStat) Dum_Real4                                          ! Roughness length (m)

               IF (ErrStat /= 0) THEN
                  CALL WrScr( ' Error reading roughness length from binary FF file.' )
                  RETURN
               END IF
            
            READ (UnWind, IOSTAT=ErrStat) Dum_Real4                                          ! Reference height (m) = Z(1) + GridHeight / 2.0

               IF (ErrStat /= 0) THEN
                  CALL WrScr( ' Error reading reference height from binary FF file.' )
                  RETURN
               END IF
            
            
            DO I = 1,3
               READ (UnWind, IOSTAT=ErrStat) Dum_Real4                                       ! TI(u, v, w) (%)
               
                  IF (ErrStat /= 0) THEN
                     CALL WrScr( ' Error reading TI('//'TRIM(Int2LStr(I))'//') from binary FF file.' )
                     RETURN
                  END IF
                  TI(I) = Dum_Real4                                                          ! This overwrites the TI read in the summary file
                  
            END DO !I            
                             
                  
      CASE (7, 8)
         !----------------------------------------
         ! General Kaimal (7) or  Mann model (8)
         !----------------------------------------
      
            READ (UnWind, IOSTAT=ErrStat) Dum_Int4                                           ! number of bytes in header
            
               IF (ErrStat /= 0) THEN           
                  CALL WrScr( ' Error reading number of header records from binary FF file.' )
                  RETURN
               END IF

            READ (UnWind, IOSTAT=ErrStat) Dum_Int4                                           ! number of components
            
               IF (ErrStat /= 0) THEN           
                  CALL WrScr( ' Error reading number of data from binary FF file.' )
                  RETURN
               END IF
               NFFComp = Dum_Int4

                                  
      CASE DEFAULT
      
         CALL ProgWarn( ' AeroDyn does not recognize the Full field turbulence file type ='//TRIM(Int2LStr(TurbType))//'.' )
                  
   END SELECT !TurbType


   READ (UnWind, IOSTAT=ErrStat) Dum_Real4                                                   ! delta z (m)
    
      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading dz from binary FF file.' )
         RETURN
      END IF
      FFZDelt = Dum_Real4
      InvFFZD = 1.0/FFZDelt


   READ (UnWind, IOSTAT=ErrStat)  Dum_Real4                                                  ! delta y (m)
   
      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading dy from binary FF file.' )
         RETURN
      END IF
      FFYDelt = Dum_Real4
      InvFFYD = 1.0/FFYDelt

   READ (UnWind, IOSTAT=ErrStat)  Dum_Real4                                                  ! delta x (m)
   
      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading dx from binary FF file.' )
         RETURN
      END IF
      FFXDelt = Dum_Real4


   READ (UnWind, IOSTAT=ErrStat)  Dum_Int4                                                   ! half the number of time steps
   
      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading number of time steps from binary FF file.' )
         RETURN
      END IF
      NFFSteps = 2*Dum_Int4


   READ (UnWind, IOSTAT=ErrStat)  Dum_Real4                                                  ! mean full-field wind speed
   
      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading mean full-field wind speed from binary FF file.' )
         RETURN
      END IF
      MeanFFWS = Dum_Real4
      InvMFFWS = 1.0/MeanFFWS
      FFDTime  = FFXDelt/MeanFFWS
      FFRate   = 1.0/FFDTime

      
   DO I = 1,3   
   
      READ (UnWind, IOSTAT=ErrStat)  Dum_Real4                                               ! unused variables: zLu, yLu, xLu

         IF (ErrStat /= 0) THEN
            CALL WrScr( ' Error reading 4-byte length scales from binary FF file.' )
            RETURN
         END IF
         
   END DO
   
   
   DO I = 1,2
   
      READ (UnWind, IOSTAT=ErrStat)  Dum_Int4                                                ! unused variables: dummy, random seed
      
         IF (ErrStat /= 0) THEN
            CALL WrScr( ' Error reading 4-byte integers from binary FF file.' )
            RETURN
         END IF
         
   END DO


   READ (UnWind, IOSTAT=ErrStat)  Dum_Int4                                                   ! nz

      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading nz from binary FF file.' )
         RETURN
      END IF
      NZGrids  = Dum_Int4
      FFZHWid  = 0.5*FFZDelt*( NZGrids - 1 )    ! half the vertical size of the grid


   READ (UnWind, IOSTAT=ErrStat)  Dum_Int4                                                   ! ny
   
      IF (ErrStat /= 0) THEN
         CALL WrScr( ' Error reading ny from binary FF file.' )
         RETURN
      END IF   
      NYGrids  = Dum_Int4
      FFYHWid  = 0.5*FFYDelt*( NYGrids - 1 )


   IF (NFFComp == 3) THEN
   
      DO I=1,6
      
         READ (UnWind, IOSTAT=ErrStat)  Dum_Real4                                            ! unused variables: zLv, yLv, xLv, zLw, yLw, xLw
         
            IF (ErrStat /= 0) THEN
               CALL WrScr( ' Error reading 4-byte length scales from binary FF file.' )
               RETURN
            END IF
         
      ENDDO !I
      
   END IF !NFFComp



   IF ( TurbType == 7 ) THEN     ! General Kaimal model
   
         READ (UnWind, IOSTAT=ErrStat)  Dum_Real4                                            ! unused variable: coherence decay constant
         
            IF (ErrStat /= 0) THEN
               CALL WrScr( ' Error reading coherence decay constant from binary FF file.' )
               RETURN
            END IF

         READ (UnWind, IOSTAT=ErrStat)  Dum_Real4                                            ! unused variables: coherence scale parameter in m
         
            IF (ErrStat /= 0) THEN
               CALL WrScr( ' Error reading coherence scale parameter from binary FF file.' )
               RETURN
            END IF
         
   ELSE IF ( TurbType == 8 ) THEN     ! Mann model
      
      DO I=1,2
      
         READ (UnWind, IOSTAT=ErrStat)  Dum_Real4                                            ! unused variables: shear parameter (gamma), scale length
         
            IF (ErrStat /= 0) THEN
               CALL WrScr( ' Error reading 4-byte parameters from binary FF file.' )
               RETURN
            END IF
         
      ENDDO !I

      DO I=1,4
      
         READ (UnWind, IOSTAT=ErrStat)  Dum_Real4                                            ! unused variables
         
            IF (ErrStat /= 0) THEN
               CALL WrScr( ' Error reading 4-byte parameters from binary FF file.' )
               RETURN
            END IF
         
      ENDDO !I

      DO I=1,3
      
         READ (UnWind, IOSTAT=ErrStat)  Dum_Int4                                            ! unused variables
         
            IF (ErrStat /= 0) THEN
               CALL WrScr( ' Error reading 4-byte parameters from binary FF file.' )
               RETURN
            END IF
         
      ENDDO !I

      DO I=1,2
      
         READ (UnWind, IOSTAT=ErrStat)  Dum_Real4                                            ! unused variables
         
            IF (ErrStat /= 0) THEN
               CALL WrScr( ' Error reading 4-byte parameters from binary FF file.' )
               RETURN
            END IF
         
      ENDDO !I

      DO I=1,3
      
         READ (UnWind, IOSTAT=ErrStat)  Dum_Int4                                            ! unused variables
         
            IF (ErrStat /= 0) THEN
               CALL WrScr( ' Error reading 4-byte parameters from binary FF file.' )
               RETURN
            END IF
         
      ENDDO !I

      DO I=1,2
      
         READ (UnWind, IOSTAT=ErrStat)  Dum_Real4                                            ! unused variables
         
            IF (ErrStat /= 0) THEN
               CALL WrScr( ' Error reading 4-byte parameters from binary FF file.' )
               RETURN
            END IF
         
      ENDDO !I


   END IF !TurbType


   RETURN
   
END SUBROUTINE Read_Bladed_FF_Header1
!====================================================================================================
SUBROUTINE Read_Bladed_Grids ( UnWind, CWise, TI, ErrStat )
! This subroutine continues reading UnWind, starting after the headers have been read.
! It reads the grids and converts the data to un-normalized wind speeds in m/s.
!----------------------------------------------------------------------------------------------------

   INTEGER,     INTENT(IN)    :: UnWind
   LOGICAL,     INTENT(IN)    :: CWise
   REAL(ReKi),  INTENT(IN)    :: TI      (3)                  ! turbulence intensities of the wind components as defined in the FF file, not necessarially the actual TI
   INTEGER,     INTENT(OUT)   :: ErrStat

   REAL(ReKi), PARAMETER      :: FF_Offset(3) = (/ 1.0, 0.0, 0.0 /)  ! used for "un-normalizing" the data

   INTEGER                    :: CFirst
   INTEGER                    :: CLast
   INTEGER                    :: CStep
   INTEGER(B2Ki)              :: Dum_Int2
   INTEGER                    :: I
   INTEGER                    :: IC
   INTEGER                    :: IR
   INTEGER                    :: IT
   
   INTEGER                    :: TmpNumSteps


   !-------------------------------------------------------------------------------------------------
   ! Generate an informative message.
   !-------------------------------------------------------------------------------------------------
      
   CALL WrScr1( ' Reading a '//TRIM( Int2LStr(NYGrids) )//'x'//TRIM( Int2LStr(NZGrids) )//  &
            ' grid ('//TRIM( Flt2Lstr(FFYHWid*2) )//' m wide, '// &
            TRIM( Flt2Lstr(GridBase) )//' m to '//TRIM( Flt2Lstr(GridBase+FFZHWid*2) )//&
            ' m above ground) with a characterstic wind speed of '//TRIM( Flt2LStr(MeanFFWS) )//' m/s. ' )

   !-------------------------------------------------------------------------------------------------
   ! Allocate space for the FF array 
   !-------------------------------------------------------------------------------------------------

   TmpNumSteps = NFFSteps + 1       ! add another step, just in case there is an odd number of steps.

!bjj: should we reorganize this FFData array so we access the data faster?
   
   IF ( .NOT. ALLOCATED( FFData ) ) THEN
      ALLOCATE ( FFData(NZGrids,NYGrids,NFFComp,TmpNumSteps),STAT=ErrStat )

      IF ( ErrStat /= 0 )  THEN

         CALL WrScr( ' Cannot allocate the full-field wind data array.' )
         RETURN

      ENDIF
      
   ELSE
      IF (SIZE(FFDATA,1) /= NZGrids .OR. SIZE(FFDATA,2) /= NYGrids .OR. &
          SIZE(FFDATA,3) /= NFFComp .OR. SIZE(FFDATA,3) /= TmpNumSteps ) THEN
          
            ! Let's make the array the correct size (we should never get here, but you never know)
            
         DEALLOCATE( FFData )
          
         ALLOCATE ( FFData(NZGrids,NYGrids,NFFComp,TmpNumSteps),STAT=ErrStat )

         IF ( ErrStat /= 0 )  THEN

            CALL WrScr( ' Cannot allocate the full-field wind data array.' )
            RETURN

         END IF ! Error
          
      END IF !Incorrect size
   END IF ! allocated

   !-------------------------------------------------------------------------------------------------
   ! Initialize the data and set column indexing to account for direction of turbine rotation (CWise)
   !-------------------------------------------------------------------------------------------------
   
   FFData(:,:,:,:) = 0.0                        ! we may have only one component
   
   IF ( CWise )  THEN
      CFirst    = NYGrids
      CLast     = 1
      CStep     = -1
   ELSE
      CFirst    = 1
      CLast     = NYGrids
      CStep     = 1
   ENDIF


   !-------------------------------------------------------------------------------------------------
   ! Loop through all the time steps, reading the data and converting to m/s
   !-------------------------------------------------------------------------------------------------
!bjj: should we reorganize this FFData array so we access the data faster?
  
   NFFSteps = TmpNumSteps
  
TIME_LOOP:  DO IT=1,TmpNumSteps     ! time (add 1 to see if there is an odd number of grids)
   
      DO IR=1,NZGrids               ! the rows (vertical)
      
         DO IC=CFirst,CLast,CStep   ! the columns (lateral)
         
            DO I=1,NFFComp          ! wind components (U, V, W)
            
               READ (UnWind,IOStat=ErrStat)  Dum_Int2
               IF (ErrStat /= 0) THEN
                  IF ( IT == TmpNumSteps ) THEN ! There really were an even number of steps
                     NFFSteps = TmpNumSteps - 1
                     ErrStat  = 0
                     EXIT TIME_LOOP       
                  ELSE               
                     CALL WrScr( ' Error reading binary data file. ic = '//TRIM(Int2LStr(ic))// &
                                    ', ir = '//TRIM(Int2LStr(ir))//', it = '//TRIM(Int2LStr(it))// &
                                    ', nffsteps = '//TRIM(Int2LStr(nffsteps)) )
                     ErrStat = 1
                     RETURN
                  END IF
               ELSE               
                  FFData(IR,IC,I,IT) = MeanFFWS*(FF_Offset(I)+0.00001*TI(I)*Dum_Int2) 
               END IF
               
            END DO !I

         END DO !IC

      END DO !IR

   END DO TIME_LOOP !IT
   
   
   CALL WrScr ( ' Processed '//TRIM( Int2LStr( NFFSteps ) )//' time steps of '//TRIM( Flt2LStr ( FFRate ) )// & 
                  '-Hz full-field data ('//TRIM( Flt2LStr( FFDTime*( NFFSteps - 1 ) ) )//' seconds).' )


END SUBROUTINE Read_Bladed_Grids
!====================================================================================================
SUBROUTINE Read_Summary_FF ( UnWind, FileName, CWise, ZCenter, TI, ErrStat )
! This subroutine reads the text summary file to get normalizing parameters, the location of the
! grid, and the direction the grid was written to the binary file
!----------------------------------------------------------------------------------------------------

   INTEGER,     INTENT(IN)    :: UnWind         ! unit number for the file to open
   CHARACTER(*),INTENT(IN)    :: FileName       ! name of the summary file
   LOGICAL,     INTENT(OUT)   :: CWise          ! rotation (for reading the order of the binary data)
   REAL(ReKi),  INTENT(OUT)   :: ZCenter        ! the height at the center of the grid
   REAL(ReKi),  INTENT(OUT)   :: TI      (3)    ! turbulence intensities of the wind components as defined in the FF file, not necessarially the actual TI
   INTEGER,     INTENT(OUT)   :: ErrStat        ! returns 0 if no error encountered in the subroutine
   
!   REAL(ReKi)                 :: ZHub           ! the reference wind point (typically hub-height)
   REAL(ReKi)                 :: ZGOffset       ! The vertical offset of the turbine on rectangular grid (allows turbulence not centered on turbine hub)

!   REAL(ReKi)                 :: GridHeight     ! The grid height, when it is different than the grid width, as found in TurbSim
!   REAL(ReKi)                 :: GridWidth      ! The grid width
!   REAL(ReKi)                 :: RDiam
      
   
   INTEGER, PARAMETER         :: NumStrings = 5 ! number of strings to be looking for in the file

   INTEGER                    :: FirstIndx      ! The first character of a line where data is located
   INTEGER                    :: I              ! A loop counter
   INTEGER                    :: LastIndx       ! The last  character of a line where data is located
   INTEGER                    :: LineCount      ! Number of lines that have been read in the file
   INTEGER                    :: Status         ! Status from I/O calls
   
   LOGICAL                    :: StrNeeded(NumStrings)   ! if the string has been found
   
   CHARACTER(1024)            :: LINE           ! temporary storage for reading a line from the file
   
      !----------------------------------------------------------------------------------------------
      ! Initialize some variables
      !----------------------------------------------------------------------------------------------

   ErrStat      = 0
   LineCount    = 0
   StrNeeded(:) = .TRUE.
!   GridHeight   = 0.0        ! Initialize the grid height
!   GridWidth    = 0.0
   ZGOffset     = 0.0
   RefHt        = 0.0

   
      !----------------------------------------------------------------------------------------------
      ! Open summary file.
      !----------------------------------------------------------------------------------------------
      
   CALL OpenFInpFile ( UnWind, TRIM( FileName ), ErrStat) 


      !----------------------------------------------------------------------------------------------
      ! Read the summary file.
      !----------------------------------------------------------------------------------------------

   DO WHILE ( ( ErrStat == 0 ) .AND. StrNeeded(NumStrings) )

      LineCount = LineCount + 1

      READ ( UnWind, '(A)', IOSTAT=ErrStat ) LINE
      IF ( ErrStat /= 0 ) THEN
      
         IF ( StrNeeded(NumStrings-1) ) THEN  ! the "HEIGHT OFFSET" StrNeeded(NumStrings) parameter is not necessary.  We'll assume it's zero if we didn't find it.
            CALL WrScr( ' Error reading line #'//TRIM(Int2LStr(LineCount))//' of the summary file, "'//TRIM(FileName)//'"' )
            CALL WrScr( 'Could not find all of the required parameters.' )
            ErrStat = NumStrings+1
            RETURN
         ELSE           
            EXIT
         ENDIF
         
      END IF
      
      CALL Conv2UC ( LINE )
            

      IF ( StrNeeded(1) ) THEN
      
         !-------------------------------------------------------------------------------------------
         ! #1: Get the rotation direction, using the string "CLOCKWISE" 
         !-------------------------------------------------------------------------------------------
            
         IF ( INDEX( LINE, 'CLOCKWISE' ) > 0 ) THEN
            
            READ (LINE, *, IOSTAT = Status)  CWise          ! Look for True/False values

            IF ( Status /= 0 ) THEN                         ! Look for Yes/No values instead

               LINE = ADJUSTL ( LINE )                      ! Remove leading spaces from input line

               SELECT CASE (LINE(1:1) )
                  CASE ('Y')
                     CWise = .TRUE.
                  CASE ('N')
                     CWise = .FALSE.
                  CASE DEFAULT                  
                     CALL WrScr( ' Error reading rotation direction (CLOCKWISE) from FF summary file.' )
                     ErrStat = 1
                     RETURN
               END SELECT
               
            END IF ! Status /= 0
            StrNeeded(1) = .FALSE.
            
         END IF   ! INDEX for "CLOCKWISE"
         
      ELSEIF ( StrNeeded(2) ) THEN
      
         !-------------------------------------------------------------------------------------------
         ! #2: Get the hub height, using the strings "HUB HEIGHT" or "ZHUB"
         !-------------------------------------------------------------------------------------------

         IF ( INDEX( LINE, 'HUB HEIGHT' ) > 0 .OR. INDEX( LINE, 'ZHUB' ) > 0 ) THEN
         
            READ (LINE, *, IOSTAT = Status) RefHt
         
            IF ( Status /= 0 ) THEN
               CALL WrScr( ' Error reading hub height from FF summary file.' )
               ErrStat = 2
               RETURN
            END IF ! Status /= 0
            StrNeeded(2) = .FALSE.
                  
         END IF !INDEX for "HUB HEIGHT" or "ZHUB"
         
         
!      ELSEIF ( StrNeeded(3) ) THEN
!
!         !-------------------------------------------------------------------------------------------
!         ! #3: Get the grid width (& height, if available), using the strings "GRID WIDTH" or "RDIAM"
!         !    If GRID HEIGHT is specified, use it, too. -- THIS IS UNNECESSARY AS IT'S STORED IN THE BINARY FILE
!         !-------------------------------------------------------------------------------------------         
!
!         IF ( INDEX( LINE, 'GRID HEIGHT') > 0 ) THEN    ! TurbSim also uses grid height, before grid width
!
!            READ (LINE, *, IOSTAT = Status) GridHeight
!            IF ( Status /= 0 ) THEN
!               CALL WrScr( ' Error reading grid height from FF summary file.' )
!               ErrStat = 3
!               RETURN
!            END IF ! Status /= 0
!
!         ELSEIF
!
!            IF ( INDEX( LINE, 'GRID WIDTH' ) > 0 .OR. INDEX( LINE, 'RDIAM' ) > 0 ) THEN
!            
!               READ (LINE, *, IOSTAT = Status) GridWidth
!            
!               IF ( Status /= 0 ) THEN
!                  CALL WrScr( ' Error reading grid width from FF summary file.' )
!                  ErrStat = 3
!                  RETURN
!               END IF ! Status /= 0
!                     
!               IF ( GridHeight <= 0.0 )  THEN
!                  GridHeight = GridWidth
!               ENDIF
!               RDiam = MIN( GridHeight, GridWidth )
!               StrNeeded(3) = .FALSE.
!               
!            END IF !INDEX for "GRID WIDTH" or "RDIAM"
!            
!         END IF
!

      ELSEIF ( StrNeeded(4) ) THEN
      
         !-------------------------------------------------------------------------------------------
         ! #4: Get the mean wind speed "UBAR" and turbulence intensities from following lines for 
         !     scaling Bladed-style FF binary files
         !-------------------------------------------------------------------------------------------         

         IF ( INDEX( LINE, 'UBAR') > 0 ) THEN

            FirstIndx = INDEX( LINE, '=' ) + 1        ! Look for the equal siqn to find the number we're looking for

            READ ( LINE( FirstIndx:LEN(LINE) ), *, IOSTAT=Status ) MeanFFWS

            IF ( Status /= 0 ) THEN
               CALL WrScr( ' Error reading UBar binary data normalizing parameter from FF summary file.' )
               ErrStat = 4
               RETURN
            END IF ! Status /= 0      

            DO I = 1,3

               LineCount = LineCount + 1

               READ ( UnWind, '(A)', IOSTAT=Status ) LINE
               IF ( Status /= 0 ) THEN
                  CALL WrScr( ' Error reading line #'//TRIM(Int2LStr(LineCount))//' of the summary file, "'//TRIM(FileName)//'"' )
                  CALL WrScr( 'Could not find all of the required parameters.' )
                  ErrStat = Status
                  RETURN
               END IF

               FirstIndx = INDEX( LINE, '=' ) + 1     ! Read the number between the = and % signs
               LastIndx  = INDEX( LINE, '%' ) - 1

               IF ( LastIndx <= FirstIndx ) LastIndx = LEN( LINE )   ! If there's no % sign, read to the end of the line

               READ ( LINE( FirstIndx:LastIndx ), *, IOSTAT=Status ) TI(I)
               IF ( Status /= 0 ) THEN
                  CALL WrScr( ' Error reading TI('//TRIM(Int2LStr(I))//') binary data normalizing parameter from FF summary file.' )
                  ErrStat = 4
                  RETURN
               END IF ! Status /= 0      

            END DO !I

            StrNeeded(4) = .FALSE.            

          END IF
      
      ELSEIF ( StrNeeded(5) ) THEN
         
         !-------------------------------------------------------------------------------------------
         ! #5: Get the grid "HEIGHT OFFSET", if it exists (in TurbSim). Otherwise, assume it's zero 
         !           ZGOffset = HH - GridBase - FFZHWid
         !-------------------------------------------------------------------------------------------         
         IF ( INDEX( LINE, 'HEIGHT OFFSET' ) > 0  ) THEN
         
            FirstIndx = INDEX ( LINE, '=' ) + 1

            READ ( LINE( FirstIndx:LEN(LINE) ), *, IOSTAT=Status ) ZGOffset            
                  
            IF ( Status /= 0 ) THEN
               CALL WrScr( ' Error reading height offset from FF summary file.' )
               ErrStat = 5
               RETURN
            END IF ! Status /= 0
                  
            StrNeeded(5) = .FALSE.
            
         END IF !INDEX for "HEIGHT OFFSET"
         
      END IF ! StrNeeded
      
      
   END DO !WHILE

   ErrStat = 0    ! We made it to the end of the file
   
   !-------------------------------------------------------------------------------------------------
   ! Close the summary file
   !-------------------------------------------------------------------------------------------------         
   
   CLOSE ( UnWind )


   !-------------------------------------------------------------------------------------------------
   ! Calculate the height of the grid center
   !-------------------------------------------------------------------------------------------------         

    ZCenter  = RefHt - ZGOffset


END SUBROUTINE Read_Summary_FF
!====================================================================================================
SUBROUTINE Read_TurbSim_FF(UnWind,WindFile, ErrStat)
! This subroutine reads the binary TurbSim-format FF file (.bts).  It fills the FFData array with
! velocity data for the grids and fills the FFtower array with velocities at points on the tower 
! (if data exists).
!----------------------------------------------------------------------------------------------------

      ! Passed Variables:
      
   INTEGER,      INTENT(IN)   :: UnWind            ! unit number for the wind file
   CHARACTER(*), INTENT(IN)   :: WindFile          ! name of the binary TurbSim file
   INTEGER,      INTENT(OUT)  :: ErrStat           ! error status return value (0=no error; non-zero is error)

      ! Local Variables:

   REAL(SiKi)                 :: Dum_Real4         ! dummy 4-byte real number
   INTEGER(B1Ki)              :: Dum_Int1          ! dummy 1-byte integer
   INTEGER(B2Ki)              :: Dum_Int2          ! dummy 2-byte integer
   INTEGER(B4Ki)              :: Dum_Int4          ! dummy 4-byte integer

   INTEGER                    :: IC                ! loop counter for wind components
   INTEGER                    :: IT                ! loop counter for time
   INTEGER                    :: IY                ! loop counter for y
   INTEGER                    :: IZ                ! loop counter for z
   INTEGER                    :: NChar             ! number of characters in the description string
   
   REAL(SiKi)                 :: Vslope(3)         ! slope  for "un-normalizing" data
   REAL(SiKi)                 :: Voffset(3)        ! offset for "un-normalizing" data
            
   CHARACTER(1024)            :: DescStr           ! description string contained in the file


   NFFComp = 3                                              ! this file contains 3 wind components
         
   !-------------------------------------------------------------------------------------------------
   ! Open the file
   !-------------------------------------------------------------------------------------------------

   CALL OpenBInpFile (UnWind, TRIM(WindFile), ErrStat)
   IF (ErrStat /= 0) RETURN
      
   !-------------------------------------------------------------------------------------------------
   ! Read the header information
   !-------------------------------------------------------------------------------------------------
      READ (UnWind, IOSTAT=ErrStat)  Dum_Int2               ! the number 7: (file identifier), INT(2)
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading the file identifier in the FF binary file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF


      READ (UnWind, IOSTAT=ErrStat)  Dum_Int4               ! the number of grid points vertically, INT(4)
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading the number of z grid points in the FF binary file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF
         NZgrids = Dum_Int4       

     
      READ (UnWind, IOSTAT=ErrStat) Dum_Int4                ! the number of grid points laterally, INT(4)
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading the number of y grid points in the FF binary file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF
         NYgrids = Dum_Int4


      READ (UnWind, IOSTAT=ErrStat)  Dum_Int4               ! the number of tower points, INT(4)
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading the number of tower points in the FF binary file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF
         NTgrids = Dum_Int4
         

      READ (UnWind, IOSTAT=ErrStat)  Dum_Int4               ! the number of time steps, INT(4)
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading the number of time steps in the FF binary file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF
         NFFSteps = Dum_Int4


      READ (UnWind, IOSTAT=ErrStat)  Dum_Real4              ! grid spacing in vertical direction (dz), REAL(4), in m
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading dz in the FF binary file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF
         InvFFZD = 1.0/Dum_Real4                            ! 1/dz
         FFZHWid = 0.5*(NZgrids-1)*Dum_Real4                ! half the grid height


      READ (UnWind, IOSTAT=ErrStat)  Dum_Real4              ! grid spacing in lateral direction (dy), REAL(4), in m
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading dy in the FF binary file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF
         InvFFYD = 1.0 / Dum_Real4                          ! 1/dy
         FFYHWid = 0.5*(NYgrids-1)*Dum_Real4                ! half grid grid width


      READ (UnWind, IOSTAT=ErrStat)  Dum_Real4              ! grid spacing in time (dt), REAL(4), in m/s
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading dt in the FF binary file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF
         FFDTime = Dum_Real4
         FFRate  = 1.0/FFDTime
                  
                  
      READ (UnWind, IOSTAT=ErrStat)  Dum_Real4              ! the mean wind speed at hub height, REAL(4), in m/s
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading mean wind speed in the FF binary file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF
         MeanFFWS = Dum_Real4
         InvMFFWS = 1.0 / MeanFFWS
         
         
      READ (UnWind, IOSTAT=ErrStat)  Dum_Real4              ! height of the hub, REAL(4), in m
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading zHub in the FF binary file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF
         RefHt = Dum_Real4
         
         
      READ (UnWind, IOSTAT=ErrStat)  Dum_Real4              ! height of the bottom of the grid, REAL(4), in m
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading GridBase in the FF binary file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF
         GridBase = Dum_Real4

 !        ZGOffset = RefHt - GridBase  - FFZHWid
         
         
      !----------------------------------------------------------------------------------------------
      ! Read the binary scaling factors
      !----------------------------------------------------------------------------------------------         
         
         DO IC = 1,NFFComp         
            READ (UnWind, IOSTAT=ErrStat)  Vslope(IC)       ! the IC-component slope for scaling, REAL(4)
               IF ( ErrStat /= 0 )  THEN
                  CALL WrScr ( ' Error reading Vslope('//Int2LStr(IC)//') in the FF binary file "'//TRIM( WindFile )//'."' )
                  RETURN
               ENDIF


            READ (UnWind, IOSTAT=ErrStat)  Voffset(IC)      ! the IC-component offset for scaling, REAL(4)
               IF ( ErrStat /= 0 )  THEN
                  CALL WrScr ( ' Error reading Voffset('//Int2LStr(IC)//') in the FF binary file "'//TRIM( WindFile )//'."' )
                  RETURN
               ENDIF
               
         END DO !IC


      !----------------------------------------------------------------------------------------------
      ! Read the description string: "Generated by TurbSim (vx.xx, dd-mmm-yyyy) on dd-mmm-yyyy at hh:mm:ss."
      !----------------------------------------------------------------------------------------------

         READ (UnWind, IOSTAT=ErrStat)  Dum_Int4                ! the number of characters in the description string, max 200, INT(4)
            IF ( ErrStat /= 0 )  THEN
               CALL WrScr ( ' Error reading NCHAR in the FF binary file "'//TRIM( WindFile )//'."' )
               RETURN
            ENDIF
            nchar = Dum_Int4
            
         DescStr = ''                                       ! Initialize the description string
         
         DO IC=1,nchar

            READ (UnWind, IOSTAT=ErrStat) Dum_Int1          ! the ASCII integer representation of the character, INT(1)
            IF ( ErrStat /= 0 )  THEN
               CALL WrScr ( ' Error reading description line in the FF binary file "'//TRIM( WindFile )//'."' )
               RETURN
            ENDIF
            
            IF ( LEN(DescStr) >= IC ) THEN
               DescStr(IC:IC) = ACHAR( Dum_Int1 )              ! converted ASCII characters
            ELSE
               CALL WrScr ( ' Description string too long.' )
               EXIT
            END IF

         ENDDO !IC


   !-------------------------------------------------------------------------------------------------
   ! Get the grid and tower velocities
   !-------------------------------------------------------------------------------------------------

   CALL WrScr1( ' Reading a '//TRIM( Int2LStr(NYGrids) )//'x'//TRIM( Int2LStr(NZGrids) )//  &
            ' grid ('//TRIM( Flt2Lstr(FFYHWid*2) )//' m wide, '// &
            TRIM( Flt2Lstr(GridBase) )//' m to '//TRIM( Flt2Lstr(GridBase+FFZHWid*2) )//&
            ' m above ground) with a characterstic wind speed of '//TRIM( Flt2LStr(MeanFFWS) )//' m/s. '//TRIM(DescStr) )
               
               
   !----------------------------------------------------------------------------------------------
   ! Allocate arrays for the FF grid as well as the tower points, if they exist
   !----------------------------------------------------------------------------------------------
            
      IF ( .NOT. ALLOCATED( FFData ) ) THEN
         ALLOCATE ( FFData(NZGrids,NYGrids,NFFComp,NFFSteps), STAT=ErrStat )
               
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr( ' Cannot allocate the full-field wind data array.' )
            RETURN
         ENDIF         
      ENDIF
         
     
      IF ( NTgrids > 0 ) THEN
      
         IF ( .NOT. ALLOCATED( FFtower ) ) THEN
            ALLOCATE( FFtower( NFFComp, NTgrids, NFFSteps ), STAT=ErrStat )
            
            IF ( ErrStat /= 0 )  THEN
               CALL WrScr ( ' Cannot allocate the tower wind data array.' )
               RETURN
            ENDIF            
         ENDIF
         
      ENDIF         
      
   !-------------------------------------------------------------------------------------------------
   ! Read the 16-bit data and scale it to 32-bit reals
   !-------------------------------------------------------------------------------------------------
               
      ! Loop through time.

      DO IT=1,NFFSteps
      
         !...........................................................................................
         ! Read grid data at this time step.
         !...........................................................................................

         DO IZ=1,NZgrids
            ! Zgrid(IZ) = Z1 + (IZ-1)*dz                 ! Vertical location of grid data point, in m relative to ground

            DO IY=1,NYgrids
               ! Ygrid(IY) = -0.5*(ny-1)*dy + (IY-1)*dy  ! Horizontal location of grid data point, in m relative to tower centerline
            
               DO IC=1,NFFComp                           ! number of wind components (U, V, W)

                  READ (UnWind, IOSTAT=ErrStat)  Dum_Int2      ! normalized wind-component, INT(2)
                  IF ( ErrStat /= 0 )  THEN
                     CALL WrScr ( ' Error reading grid wind components in the FF binary file "'//TRIM( WindFile )//'."' )
                     RETURN
                  ENDIF
                  
                  FFData(IZ,IY,IC,IT) = ( Dum_Int2 - Voffset(IC) ) / VSlope(IC)

               ENDDO !IC

            ENDDO !IY
            
         ENDDO ! IZ


         !...........................................................................................
         ! Read the tower data at this time step.
         !...........................................................................................
            
         DO IZ=1,NTgrids         ! If NTgrids<1, there are no tower points & FFtower is not allocated

            ! Ytower     = 0               ! Lateral location of the tower data point, in m relative to tower centerline
            ! Ztower(IZ) = Z1 - (IZ-1)*dz  ! Vertical location of tower data point, in m relative to ground

            DO IC=1,NFFComp   ! number of wind components

               READ (UnWind, IOSTAT=ErrStat)  Dum_Int2      ! normalized wind-component, INT(2)
               IF ( ErrStat /= 0 )  THEN
                  CALL WrScr ( ' Error reading tower wind components in the FF binary file "'//TRIM( WindFile )//'."' )
                  RETURN
               ENDIF
               
               FFtower(IC,IZ,IT) = ( Dum_Int2 - Voffset(IC) ) / VSlope(IC)  ! wind-component scaled to m/s

            ENDDO !IC

         ENDDO ! IZ


      ENDDO ! IT

   !-------------------------------------------------------------------------------------------------
   ! close the file and return
   !-------------------------------------------------------------------------------------------------
   
   CLOSE ( UnWind )


   CALL WrScr ( ' Processed '//TRIM( Int2LStr( NFFSteps ) )//' time steps of '//TRIM( Flt2LStr ( FFRate ) )// & 
                  '-Hz full-field data ('//TRIM( Flt2LStr( FFDTime*( NFFSteps - 1 ) ) )//' seconds).' )

   RETURN

END SUBROUTINE READ_TurbSim_FF
!====================================================================================================
SUBROUTINE Read_FF_Tower( UnWind, WindFile, ErrStat )
! This subroutine reads the binary tower file that corresponds with the Bladed-style FF binary file.
! The FF grid must be read before this subroutine is called! (many checks are made to ensure the
! files belong together)
!----------------------------------------------------------------------------------------------------

      ! Passed Variables:
      
   INTEGER,      INTENT(IN)   :: UnWind            ! unit number for the wind file
   CHARACTER(*), INTENT(IN)   :: WindFile          ! name of the binary TurbSim file
   INTEGER,      INTENT(OUT)  :: ErrStat           ! error status return value (0=no error; non-zero is error)

      ! Local Variables:

   REAL(SiKi)                 :: Dum_Real4         ! dummy 4-byte real number
   INTEGER(B2Ki)              :: Dum_Int2          ! dummy 2-byte integer
   INTEGER(B4Ki)              :: Dum_Int4          ! dummy 4-byte integer

   INTEGER                    :: IC                ! loop counter for wind components
   INTEGER                    :: IT                ! loop counter for time
   INTEGER                    :: IZ                ! loop counter for z
   
   REAL(ReKi), PARAMETER      :: TOL = 1E-4        ! tolerence for wind file comparisons

   REAL(ReKi), PARAMETER      :: FF_Offset(3) = (/ 1.0, 0.0, 0.0 /)  ! used for "un-normalizing" the data
   REAL(SiKi)                 :: TI       (3)      ! scaling values for "un-normalizing the data" [approx. turbulence intensities of the wind components]

   !-------------------------------------------------------------------------------------------------
   ! 
   !-------------------------------------------------------------------------------------------------

   NTgrids = 0

   IF ( NFFComp /= 3 ) THEN
      CALL WrScr( ' Error: Tower binary files require 3 wind components.' )
      ErrStat = 1
      RETURN
   END IF

   !-------------------------------------------------------------------------------------------------
   ! Open the file
   !-------------------------------------------------------------------------------------------------

   CALL OpenBInpFile (UnWind, TRIM(WindFile), ErrStat)
   IF (ErrStat /= 0) THEN
      ErrStat = -1
      RETURN
   END IF
      
   !-------------------------------------------------------------------------------------------------
   ! Read the header information and check that it's compatible with the FF Bladed-style binary
   ! parameters already read.
   !-------------------------------------------------------------------------------------------------
      READ (UnWind, IOSTAT=ErrStat)  Dum_Real4                 ! dz, in meters [4-byte REAL]
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading dz in the binary tower file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF

         IF ( ABS(Dum_Real4*InvFFZD-1) > TOL ) THEN
            CALL WrScr ( ' Resolution in the FF binary file does not match the tower file.' )
            ErrStat = 1
            RETURN
         END IF
         
         
      READ (UnWind, IOSTAT=ErrStat)  Dum_Real4                 ! dx, in meters [4-byte REAL]
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading dx in the binary tower file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF
         
         IF ( ABS(Dum_Real4*InvMFFWS/FFDTime-1) > TOL ) THEN
            CALL WrScr ( ' Time resolution in the FF binary file does not match the tower file.' )
            ErrStat = 1
            RETURN
         END IF
         

      READ (UnWind, IOSTAT=ErrStat)  Dum_Real4                 ! Zmax, in meters [4-byte REAL]
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading GridBase in the binary tower file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF

         IF ( ABS(Dum_Real4/GridBase-1) > TOL ) THEN
            CALL WrScr ( ' Height in the FF binary file does not match the tower file.' )
            ErrStat = 1
            RETURN
         END IF


      READ (UnWind, IOSTAT=ErrStat)  Dum_Int4                  ! NumOutSteps [4-byte INTEGER]
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading NumOutSteps in the binary tower file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF

         IF ( Dum_Int4 /= NFFSteps ) THEN
            CALL WrScr ( ' Number of time steps in the FF binary file does not match the tower file.' )
            ErrStat = 1
            RETURN
         END IF


      READ (UnWind, IOSTAT=ErrStat)  Dum_Int4                  ! NumZ      [4-byte INTEGER]
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading NumZ in the binary tower file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF
         NTgrids = Dum_Int4
         

      READ (UnWind, IOSTAT=ErrStat)  Dum_Real4                 ! UHub      [4-byte REAL]
         IF ( ErrStat /= 0 )  THEN
            CALL WrScr ( ' Error reading UHub in the binary tower file "'//TRIM( WindFile )//'."' )
            RETURN
         ENDIF

         IF ( ABS(Dum_Real4*InvMFFWS - 1) > TOL ) THEN
            CALL WrScr ( ' Mean wind speed in the FF binary file does not match the tower file.' )
            ErrStat = 1
            NTgrids = 0
            RETURN
         END IF


      DO IC=1,3
         READ (UnWind, IOSTAT=ErrStat)  TI(IC)               ! TI(u), TI(v), TI(w)  [4-byte REAL]
            IF ( ErrStat /= 0 )  THEN
               CALL WrScr ( ' Error reading TI('//TRIM(Int2Lstr(IC))//') in the binary tower file "' &
                               //TRIM( WindFile )//'."' )
               NTgrids = 0                               
               RETURN
            ENDIF
      END DO      

   !----------------------------------------------------------------------------------------------
   ! Allocate arrays for the tower points
   !----------------------------------------------------------------------------------------------        
     
      IF ( NTgrids > 0 ) THEN
      
         IF ( .NOT. ALLOCATED( FFtower ) ) THEN
!            CALL AllocAry( FFtower, NFFComp, NTgrids, NFFSteps, 'tower wind data', ErrStat )            
            ALLOCATE ( FFtower(NFFComp,NTgrids,NFFSteps), STAT=ErrStat )

            IF ( ErrStat /= 0 )  THEN
               CALL WrScr ( ' Error allocating memory for the tower wind data array.' )
               NTgrids = 0
               RETURN
            END IF

         ELSE
            ! Check sizes here!
         ENDIF
         
      ENDIF         
      
   !-------------------------------------------------------------------------------------------------
   ! Read the 16-bit time-series data and scale it to 32-bit reals
   !-------------------------------------------------------------------------------------------------
               
      ! Loop through time.

      DO IT=1,NFFSteps

         DO IZ=1,NTgrids         ! If NTgrids<1, there are no tower points & FFtower is not allocated

            ! Ytower     = 0               ! Lateral location of the tower data point, in m relative to tower centerline
            ! Ztower(IZ) = Z1 - (IZ-1)*dz  ! Vertical location of tower data point, in m relative to ground

            DO IC=1,NFFComp   ! number of wind components

               READ (UnWind, IOSTAT=ErrStat)  Dum_Int2      ! normalized wind-component, INT(2)
               IF ( ErrStat /= 0 )  THEN
                  CALL WrScr( ' Error reading binary tower data file. it = '//TRIM(Int2LStr(it))// &
                                 ', nffsteps = '//TRIM(Int2LStr(nffsteps)) )
                  ErrStat = 1  
                  NTgrids = 0            
                  RETURN
               ENDIF
               
               FFtower(IC,IZ,IT) = MeanFFWS*(FF_Offset(IC)+0.00001*TI(IC)*Dum_Int2)   ! wind-component scaled to m/s

            ENDDO !IC

         ENDDO ! IZ


      ENDDO ! IT   

   !-------------------------------------------------------------------------------------------------
   ! Close the file
   !-------------------------------------------------------------------------------------------------
   CLOSE ( UnWind )


   CALL WrScr ( ' Processed '//TRIM( Int2LStr(NFFSteps) )//' time steps of '//TRIM( Int2LStr(NTgrids) )//'x1 tower data grids.')


   RETURN

END SUBROUTINE Read_FF_Tower
!====================================================================================================
FUNCTION FF_GetRValue(RVarName, ErrStat)
!  This function returns a real scalar value whose name is listed in the RVarName input argument.
!  If the name is not recognized, an error is returned in ErrStat.
!----------------------------------------------------------------------------------------------------

   CHARACTER(*),   INTENT(IN)    :: RVarName
   INTEGER,        INTENT(OUT)   :: ErrStat
   REAL(ReKi)                    :: FF_GetRValue

   
   CHARACTER(20)                 :: VarNameUC
   

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
   ! Return the requested values.
   !-------------------------------------------------------------------------------------------------   

   VarNameUC = RVarName
   CALL Conv2UC( VarNameUC )

   SELECT CASE ( TRIM(VarNameUC) )
   
      CASE ('HUBHEIGHT', 'REFHEIGHT' )
         FF_GetRValue = RefHt
         
      CASE ('GRIDWIDTH', 'FFYWID' )
         FF_GetRValue = FFYHWid*2

      CASE ('GRIDHEIGHT', 'FFZWID' )
         FF_GetRValue = FFZHWid*2
         
      CASE ('MEANFFWS' )
         FF_GetRValue = MeanFFWS         
         
      CASE DEFAULT
         CALL WrScr( ' Invalid variable name in FF_GetRValue().' )
         ErrStat = 1
         
   END SELECT

END FUNCTION FF_GetRValue
!====================================================================================================
FUNCTION FF_GetWindSpeed(Time, InputPosition, ErrStat)
! This function receives time and position (in InputInfo) where (undisturbed) velocities are are 
! requested.  It determines if the point is on the FF grid or tower points and calls the
! corresponding interpolation routine, which returns the velocities at the specified time and space.
!----------------------------------------------------------------------------------------------------
   
   REAL(ReKi),        INTENT(IN) :: Time
   REAL(ReKi),        INTENT(IN) :: InputPosition(3)
   INTEGER,           INTENT(OUT):: ErrStat
   TYPE(InflIntrpOut)            :: FF_GetWindSpeed
   
   REAL(ReKi), PARAMETER         :: TOL = 1E-3
   
   
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
   ! Find out if the location is on the grid on on tower points; interpolate and return the value.
   !-------------------------------------------------------------------------------------------------   

    FF_GetWindSpeed%Velocity = FF_Interp(Time,InputPosition, ErrStat)


!   IF ( InputPosition(3) >= GridBase - TOL ) THEN  
!   
!         ! Get the velocities interpolated on the FF grid
!      
!      FF_GetWindSpeed%Velocity = FF_Interp(Time,InputPosition, ErrStat)
!      
!   ELSE
!   
!         ! Get the velocities interpolated below the FF grid, on the tower points
!
!      IF ( NTgrids < 1 ) THEN
!      
!         CALL WrScr( ' Error: FF interpolation height is below the grid and no tower points have been defined.' )
!         ErrStat = 1
!         RETURN
!         
!      ELSE
!      
!         FF_GetWindSpeed%Velocity = FF_TowerInterp(Time,InputInfo%Position, ErrStat)
!         
!      END IF   ! NTgrids < 1
!      
!   
!   END IF      ! InputInfo%Position(3)>= GridBase


END FUNCTION FF_GetWindSpeed
!====================================================================================================
!FUNCTION FF_Interp(Time, Position, ErrStat)
!!    This function is used to interpolate into the full-field wind array.  It receives X, Y, Z and
!!    TIME from the calling routine.  It then computes a time shift due to a nonzero X based upon 
!!    the average windspeed.  The modified time is used to decide which pair of time slices to interpolate
!!    within and between.  After finding the two time slices, it decides which four grid points bound the 
!!    (Y,Z) pair.  It does a bilinear interpolation for each time slice. Linear interpolation is then used 
!!    to interpolate between time slices.  This routine assumes that X is downwind, Y is to the left when  
!!    looking downwind and Z is up.  It also assumes that no extrapolation will be needed.
!!
!!    11/07/94 - Created by M. Buhl from the original TURBINT.
!!    09/25/97 - Modified by M. Buhl to use f90 constructs and new variable names.  Renamed to FF_Interp.
!!    09/23/09 - Modified by B. Jonkman to use arguments instead of modules to determine time and position.  
!!               Height is now relative to the ground
!!----------------------------------------------------------------------------------------------------
!
!   IMPLICIT                      NONE
!
!   REAL(ReKi),      INTENT(IN) :: Position(3)       ! takes the place of XGrnd, YGrnd, ZGrnd
!   REAL(ReKi),      INTENT(IN) :: Time
!   REAL(ReKi)                  :: FF_Interp(3)      ! Takes the place of FFWind
!
!   INTEGER,         INTENT(OUT):: ErrStat
!
!      ! Local Variables:
!
!   REAL(ReKi)                  :: SLOPE
!   REAL(ReKi)                  :: TimeShifted
!   REAL(ReKi),PARAMETER        :: Tol = 1.0E-3      ! a tolerance for determining if two reals are the same (for extrapolation)
!   REAL(ReKi)                  :: W_YH_Z
!   REAL(ReKi)                  :: W_YH_ZH
!   REAL(ReKi)                  :: W_YH_ZL
!   REAL(ReKi)                  :: W_YL_Z
!   REAL(ReKi)                  :: W_YL_ZH
!   REAL(ReKi)                  :: W_YL_ZL
!   REAL(ReKi)                  :: Wnd      (2)
!   REAL(ReKi)                  :: T
!   REAL(ReKi)                  :: TGRID
!   REAL(ReKi)                  :: Y
!   REAL(ReKi)                  :: YGRID
!   REAL(ReKi)                  :: Z
!   REAL(ReKi)                  :: ZGRID
!
!   INTEGER                    :: IDIM
!   INTEGER                    :: IG
!   INTEGER                    :: IT
!   INTEGER                    :: ITHI
!   INTEGER                    :: ITLO
!   INTEGER                    :: IYHI
!   INTEGER                    :: IYLO
!   INTEGER                    :: IZHI
!   INTEGER                    :: IZLO
!
!   !-------------------------------------------------------------------------------------------------
!   ! Initialize variables
!   !-------------------------------------------------------------------------------------------------
!
!   FF_Interp(:)          = 0.0                         ! the output velocities (in case NFFComp /= 3)
!   Wnd(:)                = 0.0                         ! just in case we're on an end point
!
!   !-------------------------------------------------------------------------------------------------
!   ! Find the bounding time slices.
!   !-------------------------------------------------------------------------------------------------
!
!   ! Perform the time shift.  At time=0, a point half the grid width downstream (FFYHWid) will index into the zero time slice.  
!   ! If we did not do this, any point downstream of the tower at the beginning of the run would index outside of the array.   
!   ! This all assumes the grid width is at least as large as the rotor.  If it isn't, then the interpolation will not work.
!
!! bjj: should we shift by MIN(FFYHWid,FFZHWid) instead of FFYHWid?
!
!   TimeShifted = TIME + ( FFYHWid - Position(1) )*InvMFFWS    ! in distance, X: (TIME*MeanFFWS + FFYHWid) - InputInfo%Position(1))
!   TGRID       = TimeShifted*FFRate
!
!   ITLO = INT( TGRID ) + 1             ! convert REAL to INTEGER, then add one since our grids start at 1, not 0
!   ITHI = ITLO + 1
!   
!   T    = TGRID - ( ITLO - 1 )         ! a value between 0 and 1 that indicates a relative location between ITLO and ITHI
!
!   IF ( ITLO >= NFFSteps .OR. ITLO < 1 ) THEN
!!      IF ( ITLO == NFFSteps .AND. T <= TOL ) THEN
!      IF ( ITLO == NFFSteps  ) THEN
!         ITHI = ITLO   
!         IF ( T <= TOL ) THEN ! we're on the last point
!            T = 0.0
!         ELSE  ! We'll extrapolate one dt past the last value in the file
!            ITLO = ITHI - 1
!         END IF         
!      ELSE                 
!         CALL WrScr( ' Error FF wind array was exhausted at '//TRIM( Flt2LStr( REAL( TIME,   ReKi ) ) )// & 
!                        ' seconds (trying to access data at '//TRIM( Flt2LStr( REAL( TimeShifted, ReKi ) ) )//' seconds).'  )
!         ErrStat = 1   
!         RETURN
!      END IF
!   ENDIF
!
!
!   !-------------------------------------------------------------------------------------------------
!   ! Find the bounding columns for the Y position. [The lower-left corner is (1,1) when looking upwind.]
!   !-------------------------------------------------------------------------------------------------
!
!   YGRID = ( Position(2) + FFYHWid )*InvFFYD    ! really, it's (Position(2) - -1.0*FFYHWid)
!
!   IYLO = INT( YGRID ) + 1             ! convert REAL to INTEGER, then add one since our grids start at 1, not 0
!   IYHI = IYLO + 1
!
!   Y    = YGRID - ( IYLO - 1 )         ! a value between 0 and 1 that indicates a relative location between IYLO and IYHI
!   
!   IF ( IYLO >= NYGrids .OR. IYLO < 1 ) THEN
!      IF ( IYLO == 0 .AND. Y >= 1.0-TOL ) THEN
!         Y    = 0.0 
!         IYLO = 1
!      ELSE IF ( IYLO == NYGrids .AND. Y <= TOL ) THEN
!         Y    = 0.0
!         IYHI = IYLO                   ! We're right on the last point, which is still okay      
!      ELSE
!         CALL WrScr( ' Error FF wind array boundaries violated: Grid too small in Y direction.' )
!         ErrStat = 2   
!         RETURN
!      END IF
!   ENDIF
!
!
!   !-------------------------------------------------------------------------------------------------
!   ! Find the bounding rows for the Z position. [The lower-left corner is (1,1) when looking upwind.]
!   !-------------------------------------------------------------------------------------------------
!
!   ZGRID = ( Position(3) - GridBase )*InvFFZD  !
!
!   IZLO = INT( ZGRID ) + 1             ! convert REAL to INTEGER, then add one since our grids start at 1, not 0
!   IZHI = IZLO + 1
!
!   Z = ZGRID - ( IZLO - 1 )            ! a value between 0 and 1 that indicates a relative location between IZLO and IZHI
!
!   IF ( IZLO < 1 ) THEN
!      IF ( IZLO == 0 .AND. Z >= 1.0-TOL ) THEN
!         Z    = 0.0 
!         IZLO = 1
!      ELSE
!         CALL WrScr( ' Error FF wind array boundaries violated: Grid too small in Z direction (height is below the grid).' )
!         ErrStat = 1   
!         RETURN
!      END IF
!   ELSEIF ( IZLO >= NZGrids ) THEN
!      IF ( IZLO == NZGrids .AND. Z <= TOL ) THEN
!         Z    = 0.0
!         IZHI = IZLO                   ! We're right on the last point, which is still okay
!      ELSE      
!         CALL WrScr( ' Error FF wind array boundaries violated: Grid too small in Z direction (height is above the grid).' )
!         ErrStat = 3   
!         RETURN
!      END IF         
!   ENDIF
!
!
!   !-------------------------------------------------------------------------------------------------
!   ! Loop through all the wind components.
!   !-------------------------------------------------------------------------------------------------
!
!   DO IDIM=1,NFFComp
!
!      IG = 1
!
!      DO IT=ITLO,ITHI
!
!         !-------------------------------------------------------------------------------------------
!         ! Get the wind velocity values for the four corners of the grid for this time.
!         !-------------------------------------------------------------------------------------------
!
!         W_YL_ZL = FFData( IZLO, IYLO, IDIM, IT )
!         W_YL_ZH = FFData( IZHI, IYLO, IDIM, IT )
!         W_YH_ZL = FFData( IZLO, IYHI, IDIM, IT )
!         W_YH_ZH = FFData( IZHI, IYHI, IDIM, IT )
!
!
!         !-------------------------------------------------------------------------------------------
!         ! Interpolate within the grid for this time.
!         !-------------------------------------------------------------------------------------------
!
!         W_YL_Z  = ( W_YL_ZH - W_YL_ZL )*Z + W_YL_ZL
!         W_YH_Z  = ( W_YH_ZH - W_YH_ZL )*Z + W_YH_ZL
!         Wnd(IG) = ( W_YH_Z  - W_YL_Z  )*Y + W_YL_Z
!
!         IG = IG + 1
!
!      END DO !IT
!
!      !----------------------------------------------------------------------------------------------
!      ! Interpolate between the two times.
!      !----------------------------------------------------------------------------------------------
!      
!      FF_Interp(IDIM) = ( Wnd(2) - Wnd(1) ) * T + Wnd(1)    ! interpolated velocity
!      
!   END DO !IDIM
!
!
!   RETURN
!   
!END FUNCTION FF_Interp
!====================================================================================================
FUNCTION FF_Interp(Time, Position, ErrStat)
!    This function is used to interpolate into the full-field wind array or tower array if it has   
!    been defined and is and necessary for the given inputs.  It receives X, Y, Z and
!    TIME from the calling routine.  It then computes a time shift due to a nonzero X based upon 
!    the average windspeed.  The modified time is used to decide which pair of time slices to interpolate
!    within and between.  After finding the two time slices, it decides which four grid points bound the 
!    (Y,Z) pair.  It does a bilinear interpolation for each time slice. Linear interpolation is then used 
!    to interpolate between time slices.  This routine assumes that X is downwind, Y is to the left when  
!    looking downwind and Z is up.  It also assumes that no extrapolation will be needed.
!    
!    If tower points are used, it assumes the velocity at the ground is 0.  It interpolates between
!    heights and between time slices, but ignores the Y input.
!
!    11/07/94 - Created by M. Buhl from the original TURBINT.
!    09/25/97 - Modified by M. Buhl to use f90 constructs and new variable names.  Renamed to FF_Interp.
!    09/23/09 - Modified by B. Jonkman to use arguments instead of modules to determine time and position.  
!               Height is now relative to the ground
!
!----------------------------------------------------------------------------------------------------

   IMPLICIT                      NONE

   REAL(ReKi),      INTENT(IN) :: Position(3)       ! takes the place of XGrnd, YGrnd, ZGrnd
   REAL(ReKi),      INTENT(IN) :: Time
   REAL(ReKi)                  :: FF_Interp(3)      ! The U, V, W velocities

   INTEGER,         INTENT(OUT):: ErrStat

      ! Local Variables:

!rm not used:   REAL(ReKi)                  :: SLOPE
   REAL(ReKi)                  :: TimeShifted
   REAL(ReKi),PARAMETER        :: Tol = 1.0E-3      ! a tolerance for determining if two reals are the same (for extrapolation)
   REAL(ReKi)                  :: W_YH_Z
   REAL(ReKi)                  :: W_YH_ZH
   REAL(ReKi)                  :: W_YH_ZL
   REAL(ReKi)                  :: W_YL_Z
   REAL(ReKi)                  :: W_YL_ZH
   REAL(ReKi)                  :: W_YL_ZL
   REAL(ReKi)                  :: Wnd      (2)
   REAL(ReKi)                  :: T
   REAL(ReKi)                  :: TGRID
   REAL(ReKi)                  :: Y
   REAL(ReKi)                  :: YGRID
   REAL(ReKi)                  :: Z
   REAL(ReKi)                  :: ZGRID

   INTEGER                    :: IDIM
   INTEGER                    :: IG
   INTEGER                    :: IT
   INTEGER                    :: ITHI
   INTEGER                    :: ITLO
   INTEGER                    :: IYHI
   INTEGER                    :: IYLO
   INTEGER                    :: IZHI
   INTEGER                    :: IZLO
   
   LOGICAL                    :: OnGrid
   
   !-------------------------------------------------------------------------------------------------
   ! Initialize variables
   !-------------------------------------------------------------------------------------------------

   FF_Interp(:)          = 0.0                         ! the output velocities (in case NFFComp /= 3)
   Wnd(:)                = 0.0                         ! just in case we're on an end point

   !-------------------------------------------------------------------------------------------------
   ! Find the bounding time slices.
   !-------------------------------------------------------------------------------------------------

   ! Perform the time shift.  At time=0, a point half the grid width downstream (FFYHWid) will index into the zero time slice.  
   ! If we did not do this, any point downstream of the tower at the beginning of the run would index outside of the array.   
   ! This all assumes the grid width is at least as large as the rotor.  If it isn't, then the interpolation will not work.

! bjj: should we shift by MIN(FFYHWid,FFZHWid) instead of FFYHWid?  Keep this the same as FF_Interp!

   TimeShifted = TIME + ( FFYHWid - Position(1) )*InvMFFWS    ! in distance, X: (TIME*MeanFFWS + FFYHWid) - InputInfo%Position(1))
   TGRID       = TimeShifted*FFRate

   ITLO = INT( TGRID ) + 1             ! convert REAL to INTEGER, then add one since our grids start at 1, not 0
   ITHI = ITLO + 1
   
   T    = TGRID - ( ITLO - 1 )         ! a value between 0 and 1 that indicates a relative location between ITLO and ITHI

   IF ( ITLO >= NFFSteps .OR. ITLO < 1 ) THEN
      IF ( ITLO == NFFSteps  ) THEN
         ITHI = ITLO   
         IF ( T <= TOL ) THEN ! we're on the last point
            T = 0.0
         ELSE  ! We'll extrapolate one dt past the last value in the file
            ITLO = ITHI - 1
         END IF         
      ELSE                 
         CALL WrScr( ' Error: FF wind array was exhausted at '//TRIM( Flt2LStr( REAL( TIME,   ReKi ) ) )// & 
                        ' seconds (trying to access data at '//TRIM( Flt2LStr( REAL( TimeShifted, ReKi ) ) )//' seconds).'  )
         ErrStat = 1   
         RETURN
      END IF
   ENDIF


   !-------------------------------------------------------------------------------------------------
   ! Find the bounding rows for the Z position. [The lower-left corner is (1,1) when looking upwind.]
   !-------------------------------------------------------------------------------------------------

   ZGRID = ( Position(3) - GridBase )*InvFFZD

   IF (ZGRID > -1*TOL) THEN
      OnGrid = .TRUE.
      
      IZLO = INT( ZGRID ) + 1             ! convert REAL to INTEGER, then add one since our grids start at 1, not 0
      IZHI = IZLO + 1

      Z = ZGRID - ( IZLO - 1 )            ! a value between 0 and 1 that indicates a relative location between IZLO and IZHI

      IF ( IZLO < 1 ) THEN
         IF ( IZLO == 0 .AND. Z >= 1.0-TOL ) THEN
            Z    = 0.0 
            IZLO = 1
         ELSE
            CALL WrScr( ' Error: FF wind array boundaries violated. Grid too small in Z direction (Z='//&
                        TRIM(Flt2LStr(Position(3)))//' m is below the grid).' )
            ErrStat = 1   
            RETURN
         END IF
      ELSEIF ( IZLO >= NZGrids ) THEN
         IF ( IZLO == NZGrids .AND. Z <= TOL ) THEN
            Z    = 0.0
            IZHI = IZLO                   ! We're right on the last point, which is still okay
         ELSE      
            CALL WrScr( ' Error: FF wind array boundaries violated. Grid too small in Z direction (Z='//&
                        TRIM(Flt2LStr(Position(3)))//' m is above the grid).' )
            ErrStat = 3   
            RETURN
         END IF         
      ENDIF

   ELSE
   
      OnGrid = .FALSE.  ! this is on the tower
      
      IF ( NTGrids < 1 ) THEN
         CALL WrScr ( ' Error: FF wind array boundaries violated. Grid too small in Z direction '// &
                       '(height (Z='//TRIM(Flt2LStr(Position(3)))//' m) is below the grid and no tower points are defined).' )
         ErrStat = 1

         RETURN
      END IF

      IZLO = INT( -1.0*ZGRID ) + 1            ! convert REAL to INTEGER, then add one since our grids start at 1, not 0      
      

      IF ( IZLO >= NTGrids ) THEN  !our dz is the difference between the bottom tower point and the ground
         IZLO = NTGrids
         
         Z    = 1.0 - Position(3) / (GridBase - (IZLO-1)/InvFFZD) !check that this isn't 0         
      ELSE
         Z    = ABS(ZGRID) - (IZLO - 1)
      END IF
      IZHI = IZLO + 1
            
   END IF


   IF ( OnGrid ) THEN      ! The tower points don't use this

      !-------------------------------------------------------------------------------------------------
      ! Find the bounding columns for the Y position. [The lower-left corner is (1,1) when looking upwind.]
      !-------------------------------------------------------------------------------------------------

         YGRID = ( Position(2) + FFYHWid )*InvFFYD    ! really, it's (Position(2) - -1.0*FFYHWid)

         IYLO = INT( YGRID ) + 1             ! convert REAL to INTEGER, then add one since our grids start at 1, not 0
         IYHI = IYLO + 1

         Y    = YGRID - ( IYLO - 1 )         ! a value between 0 and 1 that indicates a relative location between IYLO and IYHI
         
         IF ( IYLO >= NYGrids .OR. IYLO < 1 ) THEN
            IF ( IYLO == 0 .AND. Y >= 1.0-TOL ) THEN
               Y    = 0.0 
               IYLO = 1
            ELSE IF ( IYLO == NYGrids .AND. Y <= TOL ) THEN
               Y    = 0.0
               IYHI = IYLO                   ! We're right on the last point, which is still okay      
            ELSE
               CALL WrScr( ' Error FF wind array boundaries violated: Grid too small in Y direction. Y=' &
                             //TRIM(Flt2LStr(Position(2)))//'; Y boundaries = ['//TRIM(Flt2LStr(-1.0*FFYHWid)) &
                             //', '//TRIM(Flt2LStr(FFYHWid))//']' )
               ErrStat = 2   
               RETURN
            END IF
         ENDIF

      !-------------------------------------------------------------------------------------------------
      ! Interpolate on the grid 
      !-------------------------------------------------------------------------------------------------

      DO IDIM=1,NFFComp       ! all the components

         IG = 1

         DO IT=ITLO,ITHI      ! time slices

            !-------------------------------------------------------------------------------------------
            ! Get the wind velocity values for the four corners of the grid for this time.
            !-------------------------------------------------------------------------------------------

            W_YL_ZL = FFData( IZLO, IYLO, IDIM, IT )
            W_YL_ZH = FFData( IZHI, IYLO, IDIM, IT )
            W_YH_ZL = FFData( IZLO, IYHI, IDIM, IT )
            W_YH_ZH = FFData( IZHI, IYHI, IDIM, IT )


            !-------------------------------------------------------------------------------------------
            ! Interpolate within the grid for this time.
            !-------------------------------------------------------------------------------------------

            W_YL_Z  = ( W_YL_ZH - W_YL_ZL )*Z + W_YL_ZL
            W_YH_Z  = ( W_YH_ZH - W_YH_ZL )*Z + W_YH_ZL
            Wnd(IG) = ( W_YH_Z  - W_YL_Z  )*Y + W_YL_Z

            IG = IG + 1

         END DO !IT

         !----------------------------------------------------------------------------------------------
         ! Interpolate between the two times.
         !----------------------------------------------------------------------------------------------
         
         FF_Interp(IDIM) = ( Wnd(2) - Wnd(1) ) * T + Wnd(1)    ! interpolated velocity
         
      END DO !IDIM

   ELSE
   
   !-------------------------------------------------------------------------------------------------
   ! Interpolate on the tower array
   !-------------------------------------------------------------------------------------------------
      
      DO IDIM=1,NFFComp    ! all the components

         IG = 1

         DO IT=ITLO,ITHI

            !-------------------------------------------------------------------------------------------
            ! Get the wind velocity values for the two corners of the grid for this time.
            !-------------------------------------------------------------------------------------------

            W_YH_ZL = FFTower( IDIM, IZLO, IT )
            
            IF ( IZHI > NTGrids ) THEN
               W_YH_ZH = 0.0
            ELSE
               W_YH_ZH = FFTower( IDIM, IZHI, IT )
            END IF


            !-------------------------------------------------------------------------------------------
            ! Interpolate within the grid for this time.
            !-------------------------------------------------------------------------------------------

            Wnd(IG) = ( W_YH_ZH - W_YH_ZL )*Z + W_YH_ZL

            IG = IG + 1

         END DO !IT

         !----------------------------------------------------------------------------------------------
         ! Interpolate between the two times.
         !----------------------------------------------------------------------------------------------
         
         FF_Interp(IDIM) = ( Wnd(2) - Wnd(1) ) * T + Wnd(1)    ! interpolated velocity
         
      END DO !IDIM
   
   END IF ! OnGrid

   RETURN
   
END FUNCTION FF_Interp
!====================================================================================================
SUBROUTINE FF_Terminate( ErrStat )
!  This subroutine cleans up any data that is still allocated.  The (possibly) open files are 
!  closed in InflowWindMod.
!----------------------------------------------------------------------------------------------------

   INTEGER,    INTENT(OUT)    :: ErrStat           ! return 0 if no errors; non-zero otherwise

   ErrStat = 0

   IF ( ALLOCATED( FFData  ) )   DEALLOCATE( FFData,  STAT=ErrStat )   
   IF ( ALLOCATED( FFTower ) )   DEALLOCATE( FFTower, STAT=ErrStat )   
   
   Initialized = .FALSE.


END SUBROUTINE FF_Terminate
!====================================================================================================
END MODULE FFWind
