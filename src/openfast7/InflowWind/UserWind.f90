MODULE UserWind
!  The purpose of this module is to allow user-defined wind.  
!----------------------------------------------------------------------------------------------------

   USE                           NWTC_Library
   USE                           SharedInflowDefns

   IMPLICIT                      NONE
   PRIVATE
    
    
      ! define variables for UserWind here
      
   LOGICAL, SAVE              :: Initialized = .FALSE.         ! This variable indicates if the initialization routine has been run
   
   REAL(ReKi)                 :: UWmeanU                       ! Possibly instantaneous, disk-averaged wind speeds.
   REAL(ReKi)                 :: UWmeanV                       !
   REAL(ReKi)                 :: UWmeanW                       !   
   
   REAL(ReKi)                 :: OpenFOAM_u(3,120)
   REAL(ReKi)                 :: OpenFOAM_v(3,120)
   REAL(ReKi)                 :: OpenFOAM_w(3,120)

      ! allow the initialization and termination routines to be public (called from outside)

   PUBLIC                     :: UsrWnd_Init
   PUBLIC                     :: UsrWnd_Terminate
   PUBLIC                     :: UsrWnd_GetValue
   PUBLIC                     :: UsrWnd_GetWindSpeed

   PUBLIC                     :: UsrWnd_GetWindVec
   PUBLIC                     :: UsrWnd_ReadFlow_OpenFOAM

   
CONTAINS
!====================================================================================================
SUBROUTINE UsrWnd_Init(ErrStat)
!  This subroutine is called at the beginning of
!----------------------------------------------------------------------------------------------------

   INTEGER,    INTENT(OUT)    :: ErrStat           ! return 0 if no errors; non-zero otherwise

   !-------------------------------------------------------------------------------------------------
   ! Check that the module hasn't already been initialized.
   !-------------------------------------------------------------------------------------------------
      
   IF ( Initialized ) THEN  
      CALL WrScr( ' UserWind has already been initialized.' )
      ErrStat = 1
      RETURN
   ELSE
      ErrStat = 0
      CALL NWTC_Init()
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Perform any initialization steps here (read input files, etc.)
   !-------------------------------------------------------------------------------------------------
   
   CALL WrScr( '***** NOTE: User-defined wind employed *****' )


      ! Set the disk-average wind vector.
   
   UWmeanU = 10.0
   UWmeanV =  0.0
   UWmeanW =  0.0

   
   !-------------------------------------------------------------------------------------------------
   ! Set the initialization flag
   !-------------------------------------------------------------------------------------------------
   
   Initialized = .TRUE.
   
   RETURN

END SUBROUTINE UsrWnd_Init
!====================================================================================================
FUNCTION UsrWnd_GetValue(VarName, ErrStat)
!  This function returns a real scalar value whose name is listed in the VarName input argument.
!  If the name is not recognized, an error is returned in ErrStat.
!----------------------------------------------------------------------------------------------------
   
   CHARACTER(*),   INTENT(IN)    :: VarName
   INTEGER,        INTENT(OUT)   :: ErrStat           ! return 0 if no errors; non-zero otherwise
   REAL(ReKi)                    :: UsrWnd_GetValue

   
   CHARACTER(20)                 :: VarNameUC         ! upper-case VarName
   
   
   !-------------------------------------------------------------------------------------------------
   ! Check that the module has been initialized.
   !-------------------------------------------------------------------------------------------------   

   IF ( .NOT. Initialized ) THEN   
      CALL WrScr( 'Initialize UserWind before calling its subroutines.' )
      ErrStat = 1
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
   
      CASE ('MEANU' )
         UsrWnd_GetValue = UWmeanU
         
      CASE ('MEANV' )
         UsrWnd_GetValue = UWmeanV

      CASE ('MEANW' )
         UsrWnd_GetValue = UWmeanW
         
      CASE DEFAULT
         CALL WrScr( ' Invalid variable name in UsrWnd_GetValue().' )
         ErrStat = 1
         
   END SELECT
      
   

END FUNCTION UsrWnd_GetValue
!====================================================================================================
FUNCTION UsrWnd_GetWindSpeed(Time, InputPosition, ErrStat)
! This function receives time and position (in InputInfo) where (undisturbed) velocities are 
! requested. It returns the velocities at the specified time and space.
!----------------------------------------------------------------------------------------------------
   
   IMPLICIT NONE

   REAL(ReKi),        INTENT(IN) :: Time
   REAL(ReKi),        INTENT(IN) :: InputPosition(3)        ! X,Y,Z (z is 0 at ground level)

   INTEGER,           INTENT(OUT):: ErrStat                 ! return 0 if no errors; non-zero otherwise
   TYPE(InflIntrpOut)            :: UsrWnd_GetWindSpeed
   
   !-------------------------------------------------------------------------------------------------
   ! Check that the module has been initialized.
   !-------------------------------------------------------------------------------------------------   
   IF ( .NOT. Initialized ) THEN   
      CALL WrScr( 'Initialize UserWind before calling its subroutines.' )
      ErrStat = 1
      RETURN
   ELSE
      ErrStat = 0
   END IF
   
   !-------------------------------------------------------------------------------------------------
   ! Calculate the wind speed at this time and position.
   !-------------------------------------------------------------------------------------------------   
   !     Time
   !     X = InputPosition(1)           ! relative to the undeflected tower centerline (positive downwind)
   !     Y = InputPosition(2)           ! relative to the undeflected tower centerline (positive left when looking downwind)
   !     Z = InputPosition(3)           ! relative to the ground (0 is ground level)
   !-------------------------------------------------------------------------------------------------

   !-------------------------------------------------------------------------------------------------
   ! identify cell
   !-------------------------------------------------------------------------------------------------
      
   UsrWnd_GetWindSpeed%Velocity(1) =  10.0   ! U velocity (along positive X)
   UsrWnd_GetWindSpeed%Velocity(2) =  0.0    ! V velocity (along positive Y)
   UsrWnd_GetWindSpeed%Velocity(3) =  0.0    ! W velocity (along positive Z)

END FUNCTION UsrWnd_GetWindSpeed
!====================================================================================================
FUNCTION UsrWnd_GetWindVec(IElement, IBlade)
! This function receives time and position (in InputInfo) where (undisturbed) velocities are 
! requested. It returns the velocities at the specified time and space.
!----------------------------------------------------------------------------------------------------
   
   IMPLICIT NONE
  
   INTEGER                    :: IBlade
   INTEGER                    :: IElement

   REAL(ReKi)                 :: UsrWnd_GetWindVec(3)
      
   !-------------------------------------------------------------------------------------------------
   ! Calculate the wind speed at this time and position.
   !-------------------------------------------------------------------------------------------------   
   !     Time
   !     X = InputPosition(1)           ! relative to the undeflected tower centerline (positive downwind)
   !     Y = InputPosition(2)           ! relative to the undeflected tower centerline (positive left when looking downwind)
   !     Z = InputPosition(3)           ! relative to the ground (0 is ground level)
   !-------------------------------------------------------------------------------------------------

   !-------------------------------------------------------------------------------------------------
   ! identify cell
   !-------------------------------------------------------------------------------------------------
   
   UsrWnd_GetWindVec(1) = OpenFOAM_u(IBlade, IElement)
   UsrWnd_GetWindVec(2) = OpenFOAM_v(IBlade, IElement)
   UsrWnd_GetWindVec(3) = OpenFOAM_w(IBlade, IElement)
      
!   UsrWnd_GetWindVec%Velocity(1) =  8.0   ! U velocity (along positive X)
!   UsrWnd_GetWindVec%Velocity(2) =  0.0    ! V velocity (along positive Y)
!   UsrWnd_GetWindVec%Velocity(3) =  0.0    ! W velocity (along positive Z)

END FUNCTION UsrWnd_GetWindVec
!====================================================================================================
SUBROUTINE UsrWnd_ReadFlow_OpenFOAM(u, v, w)
 
  USE                           Blade
  USE                           Blades

  IMPLICIT NONE
  
  REAL(ReKi),        INTENT(IN) :: u(NB*BldNodes)
  REAL(ReKi),        INTENT(IN) :: v(NB*BldNodes)
  REAL(ReKi),        INTENT(IN) :: w(NB*BldNodes)

  Integer(4) j, k
  
  do j=1, NB
   do k=1, BldNodes
     
     OpenFOAM_u(j,k) = u(k + BldNodes*(j-1)) 
     OpenFOAM_v(j,k) = v(k + BldNodes*(j-1)) 
     OpenFOAM_w(j,k) = w(k + BldNodes*(j-1)) 

   end do
  end do

!!=========================
! check data communication
!!=========================
!  do k=1,nz
!   do j=1,ny
!    do i=1,nx
!      write(80,*) OpenFOAM_x(i,j,k), OpenFOAM_y(i,j,k), OpenFOAM_z(i,j,k)
!    end do
!   end do
!  end do

!   do j=1, NB
!   do k=1, BldNodes
     
!      write(80,*)  OpenFOAM_u(j,k),  OpenFOAM_v(j,k),  OpenFOAM_w(j,k)  

!   end do
!  end do
 
 
!  write(*,*) '  '
!  write(*,*) '!!!!!!!!!!!!!!!!!!!'
!  write(*,*) '!!LES DATA LOADED!!'
!  write(*,*) '!!!!!!!!!!!!!!!!!!!'
!  write(*,*) '  '
!!=========================
! end check data communication
!!=========================

END SUBROUTINE UsrWnd_ReadFlow_OpenFOAM
!====================================================================================================
SUBROUTINE UsrWnd_Terminate(ErrStat)
!  This subroutine is called at the end of program execution (including after fatal errors occur).  
!  It should close any files that could be open and deallocate any arrays that have been allocated.
!----------------------------------------------------------------------------------------------------
      
   INTEGER,    INTENT(OUT)    :: ErrStat           ! return 0 if no errors; non-zero otherwise

   ErrStat = 0

   !-------------------------------------------------------------------------------------------------
   ! Close files
   !-------------------------------------------------------------------------------------------------
      
      
   !-------------------------------------------------------------------------------------------------
   ! Deallocate arrays
   !-------------------------------------------------------------------------------------------------

      
   !-------------------------------------------------------------------------------------------------
   ! Set the initialization flag
   !-------------------------------------------------------------------------------------------------
   
   Initialized = .FALSE.

END SUBROUTINE UsrWnd_Terminate
!====================================================================================================
END MODULE UserWind
