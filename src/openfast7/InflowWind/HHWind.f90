MODULE HHWind
! This module contains all the data and procedures that define hub-height wind files. This could 
! more accurately be called a point wind file since the wind speed at any point is calculated by 
! shear applied to the point where wind is defined.  It is basically uniform wind over the rotor disk.
! The entire file is read on initialization, then the columns that make up the wind file are
! interpolated to the time requested, and wind is calculated based on the location in space.
!
! the file contains header information (rows that contain "!"), followed by numeric data stored in
! 8 columns:   (1) Time                                  [s]
!              (2) Horizontal wind speed       (V)       [m/s]
!              (3) Wind direction              (Delta)   [deg]
!              (4) Vertical wind speed         (VZ)      [m/s]
!              (5) Horizontal linear shear     (HLinShr) [-]
!              (6) Vertical power-law shear    (VShr)    [-]
!              (7) Vertical linear shear       (VLinShr) [-]
!              (8) Gust (horizontal) velocity  (VGust)   [m/s]
!
! The horizontal wind speed at (X, Y, Z) is then calculated using the interpolated columns by
!   Vh = V * ( Z/RefHt ) ** VShr                                        ! power-law wind shear
!      + V * HLinShr/RefWid * ( Y * COS(Delta) + X * SIN(Delta) )       ! horizontal linear shear
!      + V * VLinShr/RefWid * ( Z-RefHt )                               ! vertical linear shear
!      + VGust                                                          ! gust speed
!----------------------------------------------------------------------------------------------------

   USE                     NWTC_Library
   USE                     SharedInflowDefns
   
   IMPLICIT                NONE
   PRIVATE

      
   REAL(ReKi), ALLOCATABLE      :: Tdata  (:)                              ! Time array from the HH wind file
   REAL(ReKi), ALLOCATABLE      :: DELTA  (:)                              ! HH Wind direction (angle)
   REAL(ReKi), ALLOCATABLE      :: V      (:)                              ! HH horizontal wind speed
   REAL(ReKi), ALLOCATABLE      :: VZ     (:)                              ! wind, including tower shadow, along the Z axis
   REAL(ReKi), ALLOCATABLE      :: HSHR   (:)                              ! HH Horizontal linear shear
   REAL(ReKi), ALLOCATABLE      :: VSHR   (:)                              ! HH vertical shear exponent
   REAL(ReKi), ALLOCATABLE      :: VLINSHR(:)                              ! HH vertical linear shear
   REAL(ReKi), ALLOCATABLE      :: VGUST  (:)                              ! HH wind gust

   REAL(ReKi)                   :: LinearizeDels(7)                        ! The delta values for linearization -- perhaps at some point, this could be T/F and we determine the deltas by sqrt(eps) or something similar
   REAL(ReKi)                   :: RefHt                                   ! reference height; was HH (hub height); used to center the wind
   REAL(ReKi)                   :: RefWid                                  ! reference width; was 2*R (=rotor diameter); used to scale the linear shear
   
   INTEGER                      :: NumDataLines
   INTEGER, SAVE                :: TimeIndx = 0                            ! An index into the Tdata array (to allow us faster searching, starting search from previous one)

   LOGICAL, SAVE                :: Linearize = .FALSE.                     ! If this is TRUE, we are linearizing
   
   TYPE, PUBLIC                 :: HH_Info
      REAL(ReKi)                :: ReferenceHeight
      REAL(ReKi)                :: Width
   END TYPE HH_Info
      
   PUBLIC                       :: HH_Init
   PUBLIC                       :: HH_GetWindSpeed
   PUBLIC                       :: HH_Terminate
   PUBLIC                       :: HH_SetLinearizeDels
   PUBLIC                       :: HH_Get_ADhack_WindSpeed                  ! REMOVE THIS!!!!

CONTAINS
!====================================================================================================
SUBROUTINE HH_Init(UnWind, WindFile, WindInfo, ErrStat)
! A subroutine to initialize the HHWind module.  It reads the HH file and stores the data in an
! array to use later.  It requires an initial reference height (hub height) and width (rotor diameter),
! both in meters, which are used to define the volume where wind velocities will be calculated.  This
! information is necessary because of the way the shears are defined.
!----------------------------------------------------------------------------------------------------

      ! Passed Variables:
      
   INTEGER,      INTENT(IN)    :: UnWind                       ! unit number for reading wind files
   INTEGER,      INTENT(OUT)   :: ErrStat                      ! determines if an error has been encountered
   TYPE(HH_Info),INTENT(IN)    :: WindInfo                     ! Additional information needed to initialize this wind type
   
   CHARACTER(*), INTENT(IN)    :: WindFile                     ! Name of the text HH wind file

      ! local variables
            
   INTEGER, PARAMETER          :: NumCols = 8                  ! Number of columns in the HH file
   REAL(ReKi)                  :: TmpData(NumCols)             ! Temp variable for reading all columns from a line 
   REAL(ReKi)                  :: DelDiff                      ! Temp variable for storing the direction difference

   INTEGER                     :: I
   INTEGER                     :: NumComments
   INTEGER                     :: ILine                        ! Counts the line number in the file
   INTEGER, PARAMETER          :: MaxTries = 100
   CHARACTER(1024)             :: Line                         ! Temp variable for reading whole line from file

    
   !-------------------------------------------------------------------------------------------------
   ! Check that it's not already initialized
   !-------------------------------------------------------------------------------------------------
      
   IF ( TimeIndx /= 0 ) THEN  
      CALL WrScr( ' HHWind has already been initialized.' )
      ErrStat = 1
      RETURN
   ELSE
      ErrStat = 0
      CALL NWTC_Init()
      
      LinearizeDels(:) = 0.0
      Linearize        = .FALSE.
   END IF   

  
   !-------------------------------------------------------------------------------------------------
   ! Open the file for reading
   !-------------------------------------------------------------------------------------------------
   CALL OpenFInpFile (UnWind, TRIM(WindFile), ErrStat)
   
   IF ( ErrStat /= 0 ) RETURN

   !-------------------------------------------------------------------------------------------------
   ! Find the number of comment lines
   !-------------------------------------------------------------------------------------------------
   LINE = '!'                          ! Initialize the line for the DO WHILE LOOP
   NumComments = -1
   
   DO WHILE (INDEX( LINE, '!' ) > 0 ) ! Lines containing "!" are treated as comment lines
      NumComments = NumComments + 1
      
      READ(UnWind,'( A )',IOSTAT=ErrStat) LINE
            
      IF ( ErrStat /=0 ) THEN
         CALL WrScr ( ' Error reading from HH wind file on line '//TRIM(Num2LStr(NumComments))//'.' )
         RETURN
      END IF
      
   END DO !WHILE
   
   !-------------------------------------------------------------------------------------------------
   ! Find the number of data lines
   !-------------------------------------------------------------------------------------------------
   NumDataLines = 0
   
   READ(LINE,*,IOSTAT=ErrStat) ( TmpData(I), I=1,NumCols )

   DO WHILE (ErrStat == 0)  ! read the rest of the file (until an error occurs)
      NumDataLines = NumDataLines + 1               
      
      READ(UnWind,*,IOSTAT=ErrStat) ( TmpData(I), I=1,NumCols )

   END DO !WHILE


   IF (NumDataLines < 1) THEN
      CALL WrScr ( ' Error reading data from HH wind file on line '//TRIM(Num2LStr(NumDataLines+NumComments))//'.' )
      RETURN
   ELSE
      CALL WrScr ( ' Reading '//TRIM(Num2LStr(NumDataLines))//' lines of data from the HH wind file.' )
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Allocate arrays for the HH data
   !-------------------------------------------------------------------------------------------------
   ! BJJ note: If the subroutine AllocAry() is called, the CVF compiler with A2AD does not work
   !   properly.  The arrays are not properly read even though they've been allocated.
   !-------------------------------------------------------------------------------------------------
   
   IF (.NOT. ALLOCATED(Tdata) ) THEN
      ALLOCATE ( Tdata(NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH time array.' )
         RETURN
      END IF   
   END IF

   IF (.NOT. ALLOCATED(V) ) THEN
      ALLOCATE ( V(NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH horizontal wind speed array.' )
         RETURN
      END IF   
   END IF

   IF (.NOT. ALLOCATED(Delta) ) THEN
      ALLOCATE ( Delta(NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH wind direction array.' )
         RETURN
      END IF   
   END IF

   IF (.NOT. ALLOCATED(VZ) ) THEN
      ALLOCATE ( VZ(NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH vertical wind speed array.' )
         RETURN
      END IF   
   END IF

   IF (.NOT. ALLOCATED(HShr) ) THEN
      ALLOCATE ( HShr(NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH horizontal linear shear array.' )
         RETURN
      END IF   
   END IF

   IF (.NOT. ALLOCATED(VShr) ) THEN
      ALLOCATE ( VShr(NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH vertical power-law shear exponent array.' )
         RETURN
      END IF   
   END IF

   IF (.NOT. ALLOCATED(VLinShr) ) THEN
      ALLOCATE ( VLinShr(NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH vertical linear shear array.' )
         RETURN
      END IF   
   END IF

   IF (.NOT. ALLOCATED(VGust) ) THEN
      ALLOCATE ( VGust(NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH gust velocity array.' )
         RETURN
      END IF   
   END IF
   

   !-------------------------------------------------------------------------------------------------
   ! Rewind the file (to the beginning) and skip the comment lines
   !-------------------------------------------------------------------------------------------------
   REWIND( UnWind )
   
   DO I=1,NumComments
      CALL ReadCom( UnWind, TRIM(WindFile), 'Header line #'//TRIM(Num2LStr(I)), ErrStat )
      IF ( ErrStat /= 0 ) RETURN
   END DO !I


   !-------------------------------------------------------------------------------------------------
   ! Read the data arrays
   !-------------------------------------------------------------------------------------------------

   DO I=1,NumDataLines
         
      CALL ReadAry( UnWind, TRIM(WindFile), TmpData(1:NumCols), NumCols, 'TmpData', & 
                'Data from HH line '//TRIM(Num2LStr(NumComments+I)), ErrStat )
      IF (ErrStat /= 0) RETURN
                 
      Tdata(  I) = TmpData(1)
      V(      I) = TmpData(2)
      Delta(  I) = TmpData(3)*D2R 
      VZ(     I) = TmpData(4)
      HShr(   I) = TmpData(5)
      VShr(   I) = TmpData(6)
      VLinSHR(I) = TmpData(7)
      VGust(  I) = TmpData(8)           
      
   END DO !I


   !-------------------------------------------------------------------------------------------------
   ! Make sure the wind direction isn't jumping more than 180 degrees between any 2 consecutive
   ! input times.  (Avoids interpolation errors with modular arithemetic.)
   !-------------------------------------------------------------------------------------------------

   DO I=2,NumDataLines
   
      ILine = 1
      
      DO WHILE ( ILine < MaxTries )
     
         DelDiff = ( Delta(I) - Delta(I-1) )

         IF ( ABS( DelDiff ) < Pi ) EXIT  ! exit inner loop

         Delta(I) = Delta(I) - SIGN( TwoPi, DelDiff )
         
         ILine = ILine + 1

      END DO
      
      IF ( ILine >= MaxTries ) THEN
         CALL WrScr( ' Error calculating wind direction from HH file. Delta(' &
               // TRIM(Num2LStr(I  )) // ') = ' // TRIM(Num2LStr(Delta(I))) // '; Delta(' & 
               // TRIM(Num2LStr(I+1)) // ') = ' // TRIM(Num2LStr(Delta(I+1))) )
         ErrStat = 1
      END IF
           

   END DO !I


   !-------------------------------------------------------------------------------------------------
   ! Close the file
   !-------------------------------------------------------------------------------------------------
   
   CLOSE( UnWind )
   

   !-------------------------------------------------------------------------------------------------
   ! Print warnings and messages
   !-------------------------------------------------------------------------------------------------
!   CALL WrScr ( ' Processed '//TRIM( Num2LStr( NumDataLines ) )//' records of HH data' )
   
   
   IF ( Tdata(1) > 0.0 ) THEN
      CALL ProgWarn( 'The hub-height wind file : "'//TRIM(ADJUSTL(WindFile))//'" starts at a time '// & 
                     'greater than zero. Interpolation errors may result.')
   ENDIF
   
   IF ( NumDataLines == 1 ) THEN
      CALL WrScr( ' Only 1 line in HH wind file. Steady, hub-height horizontal wind speed = '//TRIM(Num2LStr(V(1)))//' m/s.' )
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Set the initial index into the time array (it indicates that we've initialized the module, too)
   ! and initialize the spatial scaling for the wind calculations
   !-------------------------------------------------------------------------------------------------
   TimeIndx = 1            

   RefHt  = WindInfo%ReferenceHeight
   RefWid = WindInfo%Width   


   RETURN
     
END SUBROUTINE HH_Init
!====================================================================================================
FUNCTION HH_GetWindSpeed(Time, InputPosition, ErrStat)
! This subroutine linearly interpolates the columns in the HH input file to get the values for 
! the requested time, then uses the interpolated values to calclate the wind speed at a point
! in space represented by InputPosition.
!----------------------------------------------------------------------------------------------------

   REAL(ReKi),        INTENT(IN) :: Time                 ! time from the start of the simulation
   REAL(ReKi),        INTENT(IN) :: InputPosition(3)     ! input information: positions X,Y,Z
   INTEGER,           INTENT(OUT):: ErrStat              ! error status
   TYPE(InflIntrpOut)            :: HH_GetWindSpeed      ! return velocities (U,V,W)
   
   REAL(ReKi)                    :: CosDelta             ! cosine of Delta_tmp
   REAL(ReKi)                    :: Delta_tmp            ! interpolated Delta   at input TIME
   REAL(ReKi)                    :: HShr_tmp             ! interpolated HShr    at input TIME
   REAL(ReKi)                    :: P                    ! temporary storage for slope (in time) used in linear interpolation
   REAL(ReKi)                    :: SinDelta             ! sine of Delta_tmp
   REAL(ReKi)                    :: V_tmp                ! interpolated V       at input TIME
   REAL(ReKi)                    :: VGust_tmp            ! interpolated VGust   at input TIME
   REAL(ReKi)                    :: VLinShr_tmp          ! interpolated VLinShr at input TIME
   REAL(ReKi)                    :: VShr_tmp             ! interpolated VShr    at input TIME
   REAL(ReKi)                    :: VZ_tmp               ! interpolated VZ      at input TIME
   REAL(ReKi)                    :: V1                   ! temporary storage for horizontal velocity
   

   !-------------------------------------------------------------------------------------------------
   ! verify the module was initialized first
   !-------------------------------------------------------------------------------------------------

   IF ( TimeIndx == 0 ) THEN
      CALL WrScr( ' Error: Call HH_Init() before getting wind speed.')
      ErrStat = 1
      RETURN
   ELSE
      ErrStat = 0
   END IF
   
      
   !-------------------------------------------------------------------------------------------------
   ! Linearly interpolate in time (or used nearest-neighbor to extrapolate) 
   ! (compare with NWTC_Num.f90\InterpStpReal)
   !-------------------------------------------------------------------------------------------------

    IF ( Linearize ) THEN  !get the perturbed wind speed

      TimeIndx      = 1
      V_tmp         = V      (1) + LinearizeDels(1)
      Delta_tmp     = Delta  (1) + LinearizeDels(2)
      VZ_tmp        = VZ     (1) + LinearizeDels(3)
      HShr_tmp      = HShr   (1) + LinearizeDels(4)
      VShr_tmp      = VShr   (1) + LinearizeDels(5)
      VLinShr_tmp   = VLinShr(1) + LinearizeDels(6)
      VGust_tmp     = VGust  (1) + LinearizeDels(7)

      ! Let's check the limits.
   ELSE IF ( Time <= Tdata(1) .OR. NumDataLines == 1 )  THEN
   
      TimeIndx      = 1
      V_tmp         = V      (1)
      Delta_tmp     = Delta  (1)
      VZ_tmp        = VZ     (1)
      HShr_tmp      = HShr   (1)
      VShr_tmp      = VShr   (1)
      VLinShr_tmp   = VLinShr(1)
      VGust_tmp     = VGust  (1)   
         
   ELSE IF ( Time >= Tdata(NumDataLines) )  THEN
   
      TimeIndx      = NumDataLines - 1
      V_tmp         = V      (NumDataLines)
      Delta_tmp     = Delta  (NumDataLines)
      VZ_tmp        = VZ     (NumDataLines)
      HShr_tmp      = HShr   (NumDataLines)
      VShr_tmp      = VShr   (NumDataLines)
      VLinShr_tmp   = VLinShr(NumDataLines)
      VGust_tmp     = VGust  (NumDataLines)
      
   ELSE
   
         ! Let's interpolate!

      TimeIndx = MAX( MIN( TimeIndx, NumDataLines-1 ), 1 )

      DO

         IF ( Time < Tdata(TimeIndx) )  THEN

            TimeIndx = TimeIndx - 1

         ELSE IF ( Time >= Tdata(TimeIndx+1) )  THEN

            TimeIndx = TimeIndx + 1

         ELSE
            P           = ( Time - Tdata(TimeIndx) )/( Tdata(TimeIndx+1) - Tdata(TimeIndx) )
            V_tmp       = ( V(      TimeIndx+1) - V(      TimeIndx) )*P + V(      TimeIndx)
            Delta_tmp   = ( Delta(  TimeIndx+1) - Delta(  TimeIndx) )*P + Delta(  TimeIndx)
            VZ_tmp      = ( VZ(     TimeIndx+1) - VZ(     TimeIndx) )*P + VZ(     TimeIndx)
            HShr_tmp    = ( HShr(   TimeIndx+1) - HShr(   TimeIndx) )*P + HShr(   TimeIndx)
            VShr_tmp    = ( VShr(   TimeIndx+1) - VShr(   TimeIndx) )*P + VShr(   TimeIndx)
            VLinShr_tmp = ( VLinShr(TimeIndx+1) - VLinShr(TimeIndx) )*P + VLinShr(TimeIndx)
            VGust_tmp   = ( VGust(  TimeIndx+1) - VGust(  TimeIndx) )*P + VGust(  TimeIndx)
            EXIT

         END IF

      END DO
      
   END IF

   
   !-------------------------------------------------------------------------------------------------
   ! calculate the wind speed at this time
   !-------------------------------------------------------------------------------------------------
   
   CosDelta = COS( Delta_tmp )
   SinDelta = SIN( Delta_tmp )
   
   V1 = V_tmp * ( ( InputPosition(3)/RefHt ) ** VShr_tmp &                                  ! power-law wind shear
        + ( HShr_tmp   * ( InputPosition(2) * CosDelta + InputPosition(1) * SinDelta ) &    ! horizontal linear shear
        +  VLinShr_tmp * ( InputPosition(3)-RefHt ) )/RefWid  ) &                           ! vertical linear shear
        + VGUST_tmp                                                                         ! gust speed
   
   HH_GetWindSpeed%Velocity(1) =  V1 * CosDelta
   HH_GetWindSpeed%Velocity(2) = -V1 * SinDelta
   HH_GetWindSpeed%Velocity(3) =  VZ_tmp      


   RETURN

END FUNCTION HH_GetWindSpeed
!====================================================================================================
FUNCTION HH_Get_ADHack_WindSpeed(Time, InputPosition, ErrStat)
! This subroutine linearly interpolates the columns in the HH input file to get the values for 
! the requested time, then uses the interpolated values to calclate the wind speed at a point
! in space represented by InputPosition. THIS FUNCTION SHOULD BE REMOVED!!!!! (used for DISK VEL ONLY)
!----------------------------------------------------------------------------------------------------

   REAL(ReKi),        INTENT(IN) :: Time                 ! time from the start of the simulation
   REAL(ReKi),        INTENT(IN) :: InputPosition(3)     ! input information: positions X,Y,Z   -   NOT USED HERE!!!
   INTEGER,           INTENT(OUT):: ErrStat              ! error status
   TYPE(InflIntrpOut)            :: HH_Get_ADHack_WindSpeed      ! return velocities (U,V,W)
   
   REAL(ReKi)                    :: Delta_tmp            ! interpolated Delta   at input TIME
   REAL(ReKi)                    :: P                    ! temporary storage for slope (in time) used in linear interpolation
   REAL(ReKi)                    :: V_tmp                ! interpolated V       at input TIME
   REAL(ReKi)                    :: VZ_tmp               ! interpolated VZ      at input TIME
   

   !-------------------------------------------------------------------------------------------------
   ! verify the module was initialized first
   !-------------------------------------------------------------------------------------------------

   IF ( TimeIndx == 0 ) THEN
      CALL WrScr( ' Error: Call HH_Init() before getting wind speed.')
      ErrStat = 1
      RETURN
   ELSE
      ErrStat = 0
   END IF
   
      
   !-------------------------------------------------------------------------------------------------
   ! Linearly interpolate in time (or use nearest-neighbor to extrapolate) 
   ! (compare with NWTC_Num.f90\InterpStpReal)
   !-------------------------------------------------------------------------------------------------

     ! Let's check the limits.

   IF ( Time <= Tdata(1) .OR. NumDataLines == 1)  THEN
   
      TimeIndx      = 1
      V_tmp         = V      (1)
      Delta_tmp     = Delta  (1)
      VZ_tmp        = VZ     (1)
         
   ELSE IF ( Time >= Tdata(NumDataLines) )  THEN
   
      TimeIndx      = NumDataLines - 1
      V_tmp         = V      (NumDataLines)
      Delta_tmp     = Delta  (NumDataLines)
      VZ_tmp        = VZ     (NumDataLines)
      
   ELSE
   
         ! Let's interpolate!

      TimeIndx = MAX( MIN( TimeIndx, NumDataLines-1 ), 1 )

      DO

         IF ( Time < Tdata(TimeIndx) )  THEN

            TimeIndx = TimeIndx - 1

         ELSE IF ( Time >= Tdata(TimeIndx+1) )  THEN

            TimeIndx = TimeIndx + 1

         ELSE
            P           = ( Time - Tdata(TimeIndx) )/( Tdata(TimeIndx+1) - Tdata(TimeIndx) )
            V_tmp       = ( V(      TimeIndx+1) - V(      TimeIndx) )*P + V(      TimeIndx)
            Delta_tmp   = ( Delta(  TimeIndx+1) - Delta(  TimeIndx) )*P + Delta(  TimeIndx)
            VZ_tmp      = ( VZ(     TimeIndx+1) - VZ(     TimeIndx) )*P + VZ(     TimeIndx)
            EXIT

         END IF

      END DO
      
   END IF
   
   !-------------------------------------------------------------------------------------------------
   ! calculate the wind speed at this time
   !-------------------------------------------------------------------------------------------------
      
   HH_Get_ADHack_WindSpeed%Velocity(1) =  V_tmp * COS( Delta_tmp )
   HH_Get_ADHack_WindSpeed%Velocity(2) = -V_tmp * SIN( Delta_tmp )
   HH_Get_ADHack_WindSpeed%Velocity(3) =  VZ_tmp      


   RETURN

END FUNCTION HH_Get_ADHack_WindSpeed
!====================================================================================================
SUBROUTINE HH_SetLinearizeDels( Perturbations, ErrStat )
! This subroutine sets the perturbation values for the linearization scheme.
!----------------------------------------------------------------------------------------------------

   REAL(ReKi),       INTENT(IN)  :: Perturbations(7)     ! purturbations for each of the 7 input parameters
   INTEGER,          INTENT(OUT) :: ErrStat              ! time from the start of the simulation

   !-------------------------------------------------------------------------------------------------
   ! verify the module was initialized first
   !-------------------------------------------------------------------------------------------------

   IF ( TimeIndx == 0 ) THEN
      CALL WrScr( ' Error: Call HH_Init() before getting wind speed.')
      ErrStat = 1
      RETURN
   ELSE
      ErrStat = 0
   END IF

   Linearize = .TRUE.
   LinearizeDels(:) = Perturbations(:)

   RETURN

END SUBROUTINE HH_SetLinearizeDels
!====================================================================================================
SUBROUTINE HH_Terminate(ErrStat)

   INTEGER,      INTENT(OUT)   :: ErrStat                      ! determines if an error has been encountered

   INTEGER                     :: SumErrs
   
   SumErrs = 0

   IF ( ALLOCATED(Tdata  ) ) DEALLOCATE( Tdata,   STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)
   
   IF ( ALLOCATED(DELTA  ) ) DEALLOCATE( DELTA,   STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)
   
   IF ( ALLOCATED(V      ) ) DEALLOCATE( V,       STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)
   
   IF ( ALLOCATED(VZ     ) ) DEALLOCATE( VZ,      STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)
   
   IF ( ALLOCATED(HSHR   ) ) DEALLOCATE( HSHR,    STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)
   
   IF ( ALLOCATED(VSHR   ) ) DEALLOCATE( VSHR,    STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)
   
   IF ( ALLOCATED(VGUST  ) ) DEALLOCATE( VGUST,   STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)
   
   IF ( ALLOCATED(VLINSHR) ) DEALLOCATE( VLINSHR, STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)

   ErrStat  = SumErrs
   TimeIndx = 0            
   
END SUBROUTINE HH_Terminate   
!====================================================================================================
END MODULE HHWind
