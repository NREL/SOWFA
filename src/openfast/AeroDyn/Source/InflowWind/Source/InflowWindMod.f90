MODULE InflowWind
! This module is used to read and process the (undisturbed) inflow winds.  It must be initialized
! using WindInf_Init() with the name of the file, the file type, and possibly reference height and
! width (depending on the type of wind file being used).  This module calls appropriate routines
! in the wind modules so that the type of wind becomes seamless to the user.  WindInf_Terminate()
! should be called when the program has finshed.
!
! Data are assumed to be in units of meters and seconds.  Z is measured from the ground (NOT the hub!).
!
!  7 Oct 2009    B. Jonkman, NREL/NWTC
!----------------------------------------------------------------------------------------------------  

   USE                           NWTC_Library
   USE                           SharedInflowDefns
   
   !-------------------------------------------------------------------------------------------------
   ! The included wind modules
   !-------------------------------------------------------------------------------------------------
   
   USE                           FFWind               ! full-field binary wind files
   USE                           HHWind               ! hub-height text wind files
   USE                           FDWind               ! 4-D binary wind files
   USE                           CTWind               ! coherent turbulence from KH billow - binary file superimposed on another wind type
   USE                           UserWind             ! user-defined wind module
   

   IMPLICIT                      NONE
   PRIVATE

   !-------------------------------------------------------------------------------------------------
   ! Private internal variables
   !-------------------------------------------------------------------------------------------------

   INTEGER, SAVE               :: WindType = 0           ! Wind Type Flag   
   INTEGER                     :: UnWind   = 91          ! The unit number used for wind inflow files
   
   LOGICAL, SAVE               :: CT_Flag  = .FALSE.     ! determines if coherent turbulence is used

   !-------------------------------------------------------------------------------------------------
   ! Definitions of public types and routines
   !-------------------------------------------------------------------------------------------------

   TYPE, PUBLIC :: InflInitInfo
      CHARACTER(1024)          :: WindFileName
      INTEGER                  :: WindFileType
      REAL(ReKi)               :: ReferenceHeight        ! reference height for HH and/or 4D winds (was hub height), in meters
      REAL(ReKi)               :: Width                  ! width of the HH file (was 2*R), in meters
   END TYPE InflInitInfo

   PUBLIC                      :: WindInf_Init           ! Initialization subroutine
   PUBLIC                      :: WindInf_GetVelocity    ! function to get wind speed at point in space and time
   PUBLIC                      :: WindInf_GetMean        ! function to get the mean wind speed at a point in space
   PUBLIC                      :: WindInf_GetStdDev      ! function to calculate standard deviation at a point in space
   PUBLIC                      :: WindInf_GetTI          ! function to get TI at a point in space
   PUBLIC                      :: WindInf_Terminate      ! subroutine to clean up
   
   PUBLIC                      :: WindInf_ADhack_diskVel ! used to keep old AeroDyn functionality--remove soon!
   PUBLIC                      :: WindInf_ADhack_DIcheck ! used to keep old AeroDyn functionality--remove soon!
   PUBLIC                      :: WindInf_LinearizePerturbation !used for linearization; should be modified


CONTAINS
!====================================================================================================
SUBROUTINE WindInf_Init( FileInfo, ErrStat )
!  Open and read the wind files, allocating space for necessary variables
!
!----------------------------------------------------------------------------------------------------  
   
      ! Passed variables
      
   TYPE(InflInitInfo), INTENT(IN)   :: FileInfo
   INTEGER,            INTENT(OUT)  :: ErrStat
   
      ! Local variables
      
   TYPE(HH_Info)                    :: HHInitInfo
   TYPE(CT_Backgr)                  :: BackGrndValues
   
   REAL(ReKi)                       :: Height
   REAL(ReKi)                       :: HalfWidth
   CHARACTER(1024)                  :: FileName
         
   
   
   IF ( WindType /= 0 ) THEN  
      CALL WrScr( ' Wind inflow has already been initialized.' )
      ErrStat = 1
      RETURN
   ELSE
      WindType = FileInfo%WindFileType         
      FileName = FileInfo%WindFileName 
      CALL NWTC_Init()
   END IF

   !-------------------------------------------------------------------------------------------------
   ! Get default wind type, based on file name, if requested
   !-------------------------------------------------------------------------------------------------
   IF ( FileInfo%WindFileType == DEFAULT_Wind ) THEN
      WindType = GetWindType( FileName, ErrStat )
   END IF

      
   !-------------------------------------------------------------------------------------------------
   ! Check for coherent turbulence file (KH superimposed on a background wind file) 
   ! Initialize the CTWind module and initialize the module of the other wind type.
   !-------------------------------------------------------------------------------------------------
      
   IF ( WindType == CTP_Wind ) THEN
      
      CALL CT_Init(UnWind, FileName, BackGrndValues, ErrStat)      
      IF (ErrStat /= 0) THEN
         CALL WindInf_Terminate( ErrStat )
         WindType = 0
         ErrStat  = 1
         RETURN
      END IF
      
      FileName = BackGrndValues%WindFile
      WindType = BackGrndValues%WindFileType
      CT_Flag  = BackGrndValues%CoherentStr                  
   
   ELSE
   
      CT_Flag  = .FALSE.
      
   END IF      
   
   
   !-------------------------------------------------------------------------------------------------
   ! Initialize based on the wind type
   !-------------------------------------------------------------------------------------------------

   SELECT CASE ( WindType )
   
      CASE (HH_Wind)

         HHInitInfo%ReferenceHeight = FileInfo%ReferenceHeight
         HHInitInfo%Width           = FileInfo%Width
         
         CALL HH_Init( UnWind, FileName, HHInitInfo, ErrStat )
      
!        IF (CT_Flag) CALL CT_SetRefVal(FileInfo%ReferenceHeight, 0.5*FileInfo%Width, ErrStat)
         IF (ErrStat == 0 .AND. CT_Flag) CALL CT_SetRefVal(FileInfo%ReferenceHeight, REAL(0.0, ReKi), ErrStat)
      
      
      CASE (FF_Wind)
      
         CALL FF_Init( UnWind, FileName, ErrStat )
                  
                  
            ! Set CT parameters                   
         
         IF ( ErrStat == 0 .AND. CT_Flag ) THEN
            Height     = FF_GetValue('HubHeight', ErrStat) 
            IF ( ErrStat /= 0 ) Height = FileInfo%ReferenceHeight        

            HalfWidth  = 0.5*FF_GetValue('GridWidth', ErrStat)
            IF ( ErrStat /= 0 ) HalfWidth = 0        

            CALL CT_SetRefVal(Height, HalfWidth, ErrStat)
         END IF
                 
                                                         
      CASE (UD_Wind)
      
         CALL UsrWnd_Init(ErrStat)
      
      
      CASE (FD_Wind)
      
         CALL FD_Init(UnWind, FileName, FileInfo%ReferenceHeight, ErrStat)
      
      
      CASE DEFAULT
      
         CALL WrScr(' Error: Undefined wind type in WindInflow_Init()' )
         ErrStat = 1  
         RETURN
         
   END SELECT

   IF ( ErrStat /= 0 ) THEN
      CALL WindInf_Terminate( ErrStat )  !Just in case we've allocated something
      WindType = 0
      ErrStat  = 1
   END IF
      
   RETURN

END SUBROUTINE WindInf_Init
!====================================================================================================
FUNCTION WindInf_GetVelocity(Time, InputPosition, ErrStat)
!FUNCTION WindInf_GetVelocity(Time, InputPosition, IElement, IBlade, ErrStat)
! Get the wind speed at a point in space and time
!----------------------------------------------------------------------------------------------------

      ! passed variables
   REAL(ReKi),       INTENT(IN)  :: Time
   REAL(ReKi),       INTENT(IN)  :: InputPosition(3)        ! X, Y, Z positions

!   INTEGER                    :: IBlade
!   INTEGER                    :: IElement

   INTEGER,          INTENT(OUT) :: ErrStat                 ! Return 0 if no error; non-zero otherwise
  
      ! local variables
   TYPE(InflIntrpOut)            :: WindInf_GetVelocity     ! U, V, W velocities
   TYPE(InflIntrpOut)            :: CTWindSpeed             ! U, V, W velocities to superimpose on background wind


   ErrStat = 0
   
   SELECT CASE ( WindType )
      CASE (HH_Wind)
         WindInf_GetVelocity = HH_GetWindSpeed(     Time, InputPosition, ErrStat )
         
      CASE (FF_Wind)     
         WindInf_GetVelocity = FF_GetWindSpeed(     Time, InputPosition, ErrStat )
         
      CASE (UD_Wind)         
         WindInf_GetVelocity = UsrWnd_GetWindSpeed( Time, InputPosition, ErrStat )
		 !WindInf_GetVelocity = UsrWnd_GetWindSpeed( Time, InputPosition, IElement, IBlade, ErrStat )
      
      CASE (FD_Wind)
         WindInf_GetVelocity = FD_GetWindSpeed(     Time, InputPosition, ErrStat )
      
      CASE DEFAULT
         CALL WrScr(' Error: Undefined wind type in WindInf_GetVelocity(). ' &
                   //'Call WindInflow_Init() before calling this function.' )
         ErrStat = 1  
         WindInf_GetVelocity%Velocity(:) = 0.0
         
   END SELECT


   IF (ErrStat /= 0) THEN
   
      WindInf_GetVelocity%Velocity(:) = 0.0
   
   ELSE
   
         ! Add coherent turbulence to background wind
         
      IF (CT_Flag) THEN      
         
         CTWindSpeed = CT_GetWindSpeed(Time, InputPosition, ErrStat)
         IF (ErrStat /=0 ) RETURN
         
         WindInf_GetVelocity%Velocity(:) = WindInf_GetVelocity%Velocity(:) + CTWindSpeed%Velocity(:)
         
      ENDIF
            
   ENDIF

END FUNCTION WindInf_GetVelocity
!====================================================================================================
FUNCTION WindInf_GetMean(StartTime, EndTime, delta_time, InputPosition,  ErrStat )
!  This function returns the mean wind speed
!----------------------------------------------------------------------------------------------------

      ! passed variables
   REAL(ReKi),       INTENT(IN)  :: StartTime
   REAL(ReKi),       INTENT(IN)  :: EndTime
   REAL(ReKi),       INTENT(IN)  :: delta_time
   REAL(ReKi),       INTENT(IN)  :: InputPosition(3)        ! X, Y, Z positions
   INTEGER,          INTENT(OUT) :: ErrStat                 ! Return 0 if no error; non-zero otherwise
  
      ! function definition
   REAL(ReKi)                    :: WindInf_GetMean(3)      ! MEAN U, V, W 

      ! local variables
   REAL(ReKi)                    :: Time
   REAL(DbKi)                    :: SumVel(3)
   INTEGER                       :: I
   INTEGER                       :: Nt
   
   TYPE(InflIntrpOut)            :: NewVelocity             ! U, V, W velocities
   
   
   Nt = (EndTime - StartTime) / delta_time

   SumVel(:) = 0.0
   ErrStat   = 0


   DO I=1,Nt
   
      Time = StartTime + (I-1)*delta_time
   
      !NewVelocity = WindInf_GetVelocity(Time, InputPosition, 1, 1, ErrStat)
	  NewVelocity = WindInf_GetVelocity(Time, InputPosition, ErrStat)
      IF ( ErrStat /= 0 ) THEN
         WindInf_GetMean(:) = SumVel(:) / REAL(I-1, ReKi)
         RETURN
      ELSE
         SumVel(:) = SumVel(:) + NewVelocity%Velocity(:)
      END IF
      
   END DO
   
   WindInf_GetMean(:) = SumVel(:) / REAL(Nt, ReKi)
   
   
END FUNCTION WindInf_GetMean
!====================================================================================================
FUNCTION WindInf_GetStdDev(StartTime, EndTime, delta_time, InputPosition,  ErrStat )
!  This function returns the mean wind speed (mean, std, TI, etc)
!----------------------------------------------------------------------------------------------------

      ! passed variables
   REAL(ReKi),       INTENT(IN)  :: StartTime
   REAL(ReKi),       INTENT(IN)  :: EndTime
   REAL(ReKi),       INTENT(IN)  :: delta_time
   REAL(ReKi),       INTENT(IN)  :: InputPosition(3)        ! X, Y, Z positions
   INTEGER,          INTENT(OUT) :: ErrStat                 ! Return 0 if no error; non-zero otherwise
  
      ! function definition
   REAL(ReKi)                    :: WindInf_GetStdDev(3)    ! STD U, V, W 

      ! local variables
   REAL(ReKi)                    :: Time
   REAL(ReKi), ALLOCATABLE       :: Velocity(:,:)
   REAL(DbKi)                    :: SumAry(3)
   REAL(DbKi)                    :: MeanVel(3)
   INTEGER                       :: I
   INTEGER                       :: Nt
   
   TYPE(InflIntrpOut)            :: NewVelocity             ! U, V, W velocities


   !-------------------------------------------------------------------------------------------------
   ! Initialize
   !-------------------------------------------------------------------------------------------------

   WindInf_GetStdDev(:) = 0.0 
   
   Nt = (EndTime - StartTime) / delta_time

   IF ( Nt < 2 ) RETURN    ! StdDev is 0
   

   IF (.NOT. ALLOCATED(Velocity)) THEN
!      CALL AllocAry( Velocity, 3, Nt, 'StdDev velocity', ErrStat)
      ALLOCATE ( Velocity(3, Nt), STAT=ErrStat )

      IF ( ErrStat /= 0 )  THEN
         CALL WrScr ( ' Error allocating memory for the StdDev velocity array.' )
         RETURN
      END IF
   END IF
   

   !-------------------------------------------------------------------------------------------------
   ! Calculate the mean, storing the velocity for later
   !-------------------------------------------------------------------------------------------------
   SumAry(:) = 0.0

   DO I=1,Nt
   
      Time = StartTime + (I-1)*delta_time
   
      !NewVelocity = WindInf_GetVelocity(Time, InputPosition,  1, 1, ErrStat)
	  NewVelocity = WindInf_GetVelocity(Time, InputPosition, ErrStat)
      IF ( ErrStat /= 0 ) RETURN
      Velocity(:,I) = NewVelocity%Velocity(:)
      SumAry(:)     = SumAry(:) + NewVelocity%Velocity(:)
      
   END DO

   MeanVel(:) = SumAry(:) / REAL(Nt, ReKi)
   
   
   !-------------------------------------------------------------------------------------------------
   ! Calculate the standard deviation
   !-------------------------------------------------------------------------------------------------
   SumAry(:) = 0.0  
   
   DO I=1,Nt
   
      SumAry(:) = SumAry(:) + ( Velocity(:,I) - MeanVel(:) )**2
      
   END DO ! I

   WindInf_GetStdDev(:) = SQRT( SumAry(:) / ( Nt - 1 ) )

   
   !-------------------------------------------------------------------------------------------------
   ! Deallocate
   !-------------------------------------------------------------------------------------------------
   IF ( ALLOCATED(Velocity) ) DEALLOCATE( Velocity )
   
   
END FUNCTION WindInf_GetStdDev
!====================================================================================================
FUNCTION WindInf_GetTI(StartTime, EndTime, delta_time, InputPosition,  ErrStat )
!  This function returns the TI of the wind speed.  It's basically a copy of WindInf_GetStdDev,
!  except the return value is divided by the mean U-component wind speed.
!----------------------------------------------------------------------------------------------------

      ! passed variables
   REAL(ReKi),       INTENT(IN)  :: StartTime
   REAL(ReKi),       INTENT(IN)  :: EndTime
   REAL(ReKi),       INTENT(IN)  :: delta_time
   REAL(ReKi),       INTENT(IN)  :: InputPosition(3)        ! X, Y, Z positions
   INTEGER,          INTENT(OUT) :: ErrStat                 ! Return 0 if no error; non-zero otherwise
  
      ! function definition
   REAL(ReKi)                    :: WindInf_GetTI(3)        ! TI U, V, W 

      ! local variables
   REAL(ReKi)                    :: Time
   REAL(ReKi), ALLOCATABLE       :: Velocity(:,:)
   REAL(DbKi)                    :: SumAry(3)
   REAL(DbKi)                    :: MeanVel(3)
   INTEGER                       :: I
   INTEGER                       :: Nt
   
   TYPE(InflIntrpOut)            :: NewVelocity             ! U, V, W velocities


   !-------------------------------------------------------------------------------------------------
   ! Initialize
   !-------------------------------------------------------------------------------------------------

   WindInf_GetTI(:) = 0.0 
   
   Nt = (EndTime - StartTime) / delta_time

   IF ( Nt < 2 ) RETURN    ! StdDev is 0
   

   IF (.NOT. ALLOCATED(Velocity)) THEN
!      CALL AllocAry( Velocity, 3, Nt, 'TI velocity', ErrStat)
      ALLOCATE ( Velocity(3, Nt), STAT=ErrStat )

      IF ( ErrStat /= 0 )  THEN
         CALL WrScr ( ' Error allocating memory for the TI velocity array.' )
         RETURN
      END IF
   END IF
   

   !-------------------------------------------------------------------------------------------------
   ! Calculate the mean, storing the velocity for later
   !-------------------------------------------------------------------------------------------------
   SumAry(:) = 0.0

   DO I=1,Nt
   
      Time = StartTime + (I-1)*delta_time

      !NewVelocity = WindInf_GetVelocity(Time, InputPosition,  1, 1, ErrStat)
	  NewVelocity = WindInf_GetVelocity(Time, InputPosition, ErrStat)
      IF ( ErrStat /= 0 ) RETURN
      Velocity(:,I) = NewVelocity%Velocity(:)
      SumAry(:)     = SumAry(:) + NewVelocity%Velocity(:)
      
   END DO

   MeanVel(:) = SumAry(:) / REAL(Nt, ReKi)
   
   IF ( ABS(MeanVel(1)) <= EPSILON(MeanVel(1)) ) THEN
      CALL WrScr( ' Wind speed is small in WindInf_GetTI(). TI is undefined.' )
      ErrStat = 1
      RETURN
   END IF
   
   !-------------------------------------------------------------------------------------------------
   ! Calculate the standard deviation
   !-------------------------------------------------------------------------------------------------
   SumAry(:) = 0.0  
   
   DO I=1,Nt
   
      SumAry(:) = SumAry(:) + ( Velocity(:,I) - MeanVel(:) )**2
      
   END DO ! I

   WindInf_GetTI(:) = SQRT( SumAry(:) / ( Nt - 1 ) ) / MeanVel(1)

   
   !-------------------------------------------------------------------------------------------------
   ! Deallocate
   !-------------------------------------------------------------------------------------------------
   IF ( ALLOCATED(Velocity) ) DEALLOCATE( Velocity )
   
   
END FUNCTION WindInf_GetTI
!====================================================================================================
FUNCTION GetWindType( FileName, ErrStat )
!  This subroutine checks the file FileName to see what kind of wind file we are using.  Used when
!  the wind file type is unknown. 
!----------------------------------------------------------------------------------------------------


   IMPLICIT             NONE


      ! Passed Variables:
      
   CHARACTER(*),INTENT(INOUT) :: FileName   
   INTEGER, INTENT(OUT)       :: ErrStat

      ! Function definition
   INTEGER                    :: GetWindType

      ! Local Variables:

   INTEGER                    :: IND
   LOGICAL                    :: Exists

   CHARACTER(  3)             :: FileNameEnd
   CHARACTER(  8)             :: WndFilNam



   ErrStat = 0
   
   !-------------------------------------------------------------------------------------------------
   ! Check for user-defined wind file first; file starts with "USERWIND"
   !-------------------------------------------------------------------------------------------------
   
   WndFilNam = FileName
   CALL Conv2UC( WndFilNam )

   IF ( WndFilNam == 'USERWIND' )  THEN
      
      CALL WrScr1( ' Detected user-defined wind file.' )
      GetWindType = UD_Wind

      RETURN
   END IF
   
   !-------------------------------------------------------------------------------------------------
   ! Get the file extension (or at least what we expect the extension to be)
   !-------------------------------------------------------------------------------------------------   
   IND         = INDEX( FileName, '.', BACK=.TRUE. )    ! Find the last period in the file name - to determine file extension   
   FileNameEnd = FileName(IND+1:)                       ! Get the extenstion (may not be the whole "extension")
   CALL Conv2UC (FileNameEnd)       


   !-------------------------------------------------------------------------------------------------
   ! If there was no '.' in the file name, assume FF, and add a .wnd extension
   !-------------------------------------------------------------------------------------------------
   IF ( IND == 0 ) THEN
      CALL WrScr1(' No file extension found. Assuming '//TRIM(FileName)// &
                  ' is a binary FF wind file with a ".wnd" extension.')
      GetWindType = FF_Wind
      FileName = TRIM(FileName)//'.wnd'   
      RETURN
   END IF
      

   !-------------------------------------------------------------------------------------------------
   ! Base the file type on the extension
   !-------------------------------------------------------------------------------------------------
   SELECT CASE ( TRIM(FileNameEnd) )
      CASE ('WND')
      
            ! If a summary file exists, assume FF; otherwise, assume HH file.
         
         INQUIRE ( FILE=FileName(1:IND)//'sum' , EXIST=Exists )
         IF (Exists) THEN
            CALL WrScr1(' Assuming '//TRIM(FileName)//' is a binary FF wind file.')
            GetWindType = FF_Wind
         ELSE
            CALL WrScr1(' Assuming '//TRIM(FileName)//' is a formatted HH wind file.')
            GetWindType = HH_Wind
         END IF
      
      CASE ('BTS')
         CALL WrScr1(' Assuming '//TRIM(FileName)//' is a binary FF wind file.')
         GetWindType = FF_Wind

      CASE ('CTP')
         CALL WrScr1(' Assuming '//TRIM(FileName)//' is a coherent turbulence wind file.')
         GetWindType = CTP_Wind
         
      CASE ('FDP')
         CALL WrScr1(' Assuming '//TRIM(FileName)//' is a binary 4-dimensional wind file.')
         GetWindType = FD_Wind
         
      CASE DEFAULT
         CALL WrScr1(' Assuming '//TRIM(FileName)//' is a formatted HH wind file.')
         GetWindType = HH_Wind
         
   END SELECT


RETURN
END FUNCTION GetWindType
!====================================================================================================
SUBROUTINE WindInf_LinearizePerturbation( LinPerturbations, ErrStat )
! This function is used in FAST's linearization scheme.  It should be fixed at some point.
!----------------------------------------------------------------------------------------------------

      ! Passed variables
      
   INTEGER,    INTENT(OUT)    :: ErrStat 
   
   REAL(ReKi), INTENT(IN)     :: LinPerturbations(7)
   
      ! Local variables
      
      
   ErrStat = 0

   SELECT CASE ( WindType )
      CASE (HH_Wind)
      
         CALL HH_SetLinearizeDels( LinPerturbations, ErrStat )   
     
      CASE ( FF_Wind, UD_Wind, FD_Wind )
      
         CALL WrScr( ' Error: Linearization is valid only with HH wind files.' )
         ErrStat = 1

      CASE DEFAULT
         CALL WrScr(' Error: Undefined wind type in WindInf_LinearizePerturbation(). '// &
                     'Call WindInflow_Init() before calling this function.' )
         ErrStat = 1  
         
   END SELECT
   

END SUBROUTINE WindInf_LinearizePerturbation
!====================================================================================================
FUNCTION WindInf_ADhack_diskVel( Time, InpPosition, ErrStat )
! This function should be deleted ASAP.  It's purpose is to reproduce results of AeroDyn 12.57;
! when a consensus on the definition of "average velocity" is determined, this function will be
! removed.  InpPosition(2) should be the rotor radius; InpPosition(3) should be hub height
!----------------------------------------------------------------------------------------------------

      ! Passed variables
      
   REAL(ReKi), INTENT(IN)     :: Time        
   REAL(ReKi), INTENT(IN)     :: InpPosition(3)        
   INTEGER, INTENT(OUT)       :: ErrStat     

      ! Function definition
   REAL(ReKi)                 :: WindInf_ADhack_diskVel(3)

      ! Local variables
   TYPE(InflIntrpOut)         :: NewVelocity             ! U, V, W velocities
   REAL(ReKi)                 :: Position(3)        
   INTEGER                    :: IY
   INTEGER                    :: IZ
   

   ErrStat = 0

   SELECT CASE ( WindType )
      CASE (HH_Wind)
         
!      VXGBAR =  V * COS( DELTA )
!      VYGBAR = -V * SIN( DELTA )
!      VZGBAR =  VZ

         Position    = (/ REAL(0.0, ReKi), REAL(0.0, ReKi), InpPosition(3) /)
         NewVelocity = HH_Get_ADHack_WindSpeed(Time, Position, ErrStat)
         
         WindInf_ADhack_diskVel(:) = NewVelocity%Velocity(:)

         
      CASE (FF_Wind)     
!      VXGBAR = MeanFFWS
!      VYGBAR = 0.0
!      VZGBAR = 0.0
         
         WindInf_ADhack_diskVel(1)   = FF_GetValue('MEANFFWS', ErrStat)
         WindInf_ADhack_diskVel(2:3) = 0.0

      CASE (UD_Wind)         
!      VXGBAR = UWmeanU
!      VYGBAR = UWmeanV
!      VZGBAR = UWmeanW

         WindInf_ADhack_diskVel(1)   = UsrWnd_GetValue('MEANU', ErrStat)
         IF (ErrStat /= 0) RETURN
         WindInf_ADhack_diskVel(2)   = UsrWnd_GetValue('MEANV', ErrStat)
         IF (ErrStat /= 0) RETURN
         WindInf_ADhack_diskVel(3)   = UsrWnd_GetValue('MEANW', ErrStat)

      CASE (FD_Wind)
!      XGrnd = 0.0
!      YGrnd = 0.5*RotDiam
!      ZGrnd = 0.5*RotDiam
!      CALL FD_Interp
!      VXGBAR = FDWind( 1 )
!      VYGBAR = FDWind( 2 )
!      VZGBAR = FDWind( 3 )
!
!      XGrnd =  0.0
!      YGrnd = -0.5*RotDiam
!      ZGrnd =  0.5*RotDiam
!      CALL FD_Interp
!      VXGBAR = VXGBAR + FDWind( 1 )
!      VYGBAR = VYGBAR + FDWind( 2 )
!      VZGBAR = VZGBAR + FDWind( 3 )
!
!      XGrnd =  0.0
!      YGrnd = -0.5*RotDiam
!      ZGrnd = -0.5*RotDiam
!      CALL FD_Interp
!      VXGBAR = VXGBAR + FDWind( 1 )
!      VYGBAR = VYGBAR + FDWind( 2 )
!      VZGBAR = VZGBAR + FDWind( 3 )
!
!      XGrnd =  0.0
!      YGrnd =  0.5*RotDiam
!      ZGrnd = -0.5*RotDiam
!      CALL FD_Interp
!      VXGBAR = 0.25*( VXGBAR + FDWind( 1 ) )
!      VYGBAR = 0.25*( VYGBAR + FDWind( 2 ) )
!      VZGBAR = 0.25*( VZGBAR + FDWind( 3 ) )      


         Position(1) = 0.0
         WindInf_ADhack_diskVel(:) = 0.0
         
         DO IY = -1,1,2
            Position(2)  =  IY*FD_GetValue('RotDiam',ErrStat)
            
            DO IZ = -1,1,2
               Position(3)  = IZ*InpPosition(2) + InpPosition(3)
               
               !NewVelocity = WindInf_GetVelocity(Time, Position, 1, 1, ErrStat)
			   NewVelocity = WindInf_GetVelocity(Time, Position, ErrStat)
               WindInf_ADhack_diskVel(:) = WindInf_ADhack_diskVel(:) + NewVelocity%Velocity(:)
            END DO
         END DO
         WindInf_ADhack_diskVel(:) = 0.25*WindInf_ADhack_diskVel(:)
      
      CASE DEFAULT
         CALL WrScr(' Error: Undefined wind type in WindInf_ADhack_diskVel(). '// &
                    'Call WindInflow_Init() before calling this function.' )
         ErrStat = 1  
         
   END SELECT

   RETURN

END FUNCTION WindInf_ADhack_diskVel
!====================================================================================================
FUNCTION WindInf_ADhack_DIcheck( ErrStat )
! This function should be deleted ASAP.  It's purpose is to reproduce results of AeroDyn 12.57;
! it performs a wind speed check for the dynamic inflow initialization
! it returns MFFWS for the FF wind files; for all others, a sufficiently large number is used ( > 8 m/s)
!----------------------------------------------------------------------------------------------------

      ! Passed variables
      
   INTEGER, INTENT(OUT)       :: ErrStat     

      ! Function definition
   REAL(ReKi)                 :: WindInf_ADhack_DIcheck
   

   ErrStat = 0

   SELECT CASE ( WindType )
      CASE (HH_Wind, UD_Wind, FD_Wind )
         
         WindInf_ADhack_DIcheck = 50  ! just return something greater than 8 m/s
         
      CASE (FF_Wind)     
         
         WindInf_ADhack_DIcheck = FF_GetValue('MEANFFWS', ErrStat)
      
      CASE DEFAULT
         CALL WrScr(' Error: Undefined wind type in WindInf_ADhack_DIcheck(). '// &
                    'Call WindInflow_Init() before calling this function.' )
         ErrStat = 1  
         
   END SELECT

   RETURN

END FUNCTION WindInf_ADhack_DIcheck
!====================================================================================================
SUBROUTINE WindInf_Terminate( ErrStat )
! Clean up the allocated variables and close all open files.  Reset the initialization flag so
! that we have to reinitialize before calling the routines again.
!----------------------------------------------------------------------------------------------------

   INTEGER, INTENT(OUT)       :: ErrStat     !bjj: do we care if there's an error on cleanup?

   
      ! Close the wind file, if it happens to be open
         
   CLOSE( UnWind )
   

      ! End the sub-modules (deallocates their arrays and closes their files):

   SELECT CASE ( WindType )
   
      CASE (HH_Wind)
         CALL HH_Terminate(     ErrStat )
         
      CASE (FF_Wind)
         CALL FF_Terminate(     ErrStat )
               
      CASE (UD_Wind)
         CALL UsrWnd_Terminate( ErrStat )      
      
      CASE (FD_Wind)
         CALL FD_Terminate(     ErrStat )
      
      CASE DEFAULT
         CALL WrScr(' Undefined wind type in WindInf_Terminate().' )
         ErrStat = 1  
         
   END SELECT
      
!   IF (CT_Flag) CALL CT_Terminate( ErrStat )
   CALL CT_Terminate( ErrStat )
   
   
      ! Reset the wind type so that the initialization routine must be called
   
   WindType = 0               
   CT_Flag  = .FALSE.


END SUBROUTINE WindInf_Terminate
!====================================================================================================
END MODULE InflowWind
