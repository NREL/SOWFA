MODULE SharedTypes
!
!----------------------------------------------------------------------------------------------------

   USE                              NWTC_Library

   IMPLICIT NONE

   !-------------------------------------------------------------------------------------------------
   ! Public types and subroutines
   !-------------------------------------------------------------------------------------------------
   TYPE, PUBLIC :: Marker
      REAL(ReKi)                 :: Position(3)
      REAL(ReKi)                 :: Orientation(3,3)     ! Direction Cosine Matrix (DCM)
      REAL(ReKi)                 :: TranslationVel(3)
      REAL(ReKi)                 :: RotationVel(3)
   END TYPE Marker

   TYPE, PUBLIC :: Load
      REAL(ReKi)                 :: Force(3)
      REAL(ReKi)                 :: Moment(3)
   END TYPE Load

   TYPE, PUBLIC :: AllAeroMarkers
      TYPE(Marker), ALLOCATABLE  :: Blade(:,:)
      TYPE(Marker), ALLOCATABLE  :: Hub(:)
      TYPE(Marker), ALLOCATABLE  :: RotorFurl(:)
      TYPE(Marker), ALLOCATABLE  :: Nacelle(:)
      TYPE(Marker), ALLOCATABLE  :: Tower(:)
      TYPE(Marker), ALLOCATABLE  :: Tail(:)
   END TYPE AllAeroMarkers

   TYPE, PUBLIC :: AllAeroLoads
      TYPE(Load),  ALLOCATABLE   :: Blade(:,:)
      TYPE(Load),  ALLOCATABLE   :: Hub(:)
      TYPE(Load),  ALLOCATABLE   :: RotorFurl(:)
      TYPE(Load),  ALLOCATABLE   :: Nacelle(:)
      TYPE(Load),  ALLOCATABLE   :: Tower(:)
      TYPE(Load),  ALLOCATABLE   :: Tail(:)
   END TYPE AllAeroLoads

!   TYPE, PUBLIC :: ProgDesc !do we want to add date as well?
!      CHARACTER(20)              :: Name
!      CHARACTER(99)              :: Ver
!   END TYPE ProgDesc


   TYPE, PUBLIC :: AeroConfig
      TYPE(Marker), ALLOCATABLE  :: Blade(:)
      REAL(ReKi)                 :: BladeLength
      TYPE(Marker)               :: Hub
      TYPE(Marker)               :: RotorFurl
      TYPE(Marker)               :: Nacelle
      TYPE(Marker)               :: TailFin
      TYPE(Marker)               :: Tower
      TYPE(Marker)               :: Substructure
      TYPE(Marker)               :: Foundation
   END TYPE AeroConfig

!====================================================================================================
!CONTAINS
!====================================================================================================
!SUBROUTINE AllocateLoads(LoadVar,NB,NBEle,NHub,NNacelle,NTwr,NTail,ErrorStatus)
!! Allocation subroutine for type AllAeroLoads
!!----------------------------------------------------------------------------------------------------
!
!      ! Passed variables
!
!   TYPE(AllAeroLoads), INTENT(INOUT)   :: LoadVar
!   INTEGER,            INTENT(IN)      :: NB                      ! Number of blades
!   INTEGER,            INTENT(IN)      :: NBEle                   ! Number of elements per blade
!   INTEGER,            INTENT(IN)      :: NHub                    ! Number of elements on the hub
!   INTEGER,            INTENT(IN)      :: NNacelle                ! Number of elements on the nacelle
!   INTEGER,            INTENT(IN)      :: NTwr                    ! Number of elements on the tower
!   INTEGER,            INTENT(IN)      :: NTail                   ! Number of elements on the tail
!   INTEGER,            INTENT(OUT)     :: ErrorStatus             ! Determines if an error was encountered
!
!
!      ! Allocate the states
!
!
!   IF (.NOT. ALLOCATED(LoadVar%Blade) .AND. NB*NBEle > 0 ) THEN
!      ALLOCATE( LoadVar%Blade(NB,NBEle), STAT=ErrorStatus )
!      IF (ErrorStatus /=0) RETURN
!   ENDIF
!
!   IF (.NOT. ALLOCATED(LoadVar%Hub) .AND. NHub > 0 ) THEN
!      ALLOCATE( LoadVar%Hub(NHub), STAT=ErrorStatus )
!      IF (ErrorStatus /=0) RETURN
!   ENDIF
!
!   IF (.NOT. ALLOCATED(LoadVar%Nacelle) .AND. NNacelle > 0 ) THEN
!      ALLOCATE( LoadVar%Nacelle(NNacelle), STAT=ErrorStatus )
!      IF (ErrorStatus /=0) RETURN
!   ENDIF
!
!   IF (.NOT. ALLOCATED(LoadVar%Tower) .AND. NTwr > 0 ) THEN
!      ALLOCATE( LoadVar%Tower(NTwr), STAT=ErrorStatus )
!      IF (ErrorStatus /=0) RETURN
!   ENDIF
!
!   IF (.NOT. ALLOCATED(LoadVar%Tail) .AND. NTail > 0 ) THEN
!      ALLOCATE( LoadVar%Tail(NTail), STAT=ErrorStatus )
!      IF (ErrorStatus /=0) RETURN
!   ENDIF
!
!   ErrorStatus = 0
!
!
!END SUBROUTINE AllocateLoads
!!====================================================================================================
!SUBROUTINE AllocateStates(StateVar,NB,NBEle,NHub,NNacelle,NTwr,NTail,ErrorStatus)
!! Allocation subroutine for type AllAeroStates
!!----------------------------------------------------------------------------------------------------
!
!      ! Passed variables
!
!   TYPE(AllAeroStates), INTENT(INOUT)  :: StateVar
!   INTEGER,             INTENT(IN)     :: NB                      ! Number of blades
!   INTEGER,             INTENT(IN)     :: NBEle                   ! Number of elements per blade
!   INTEGER,             INTENT(IN)     :: NHub                    ! Number of elements on the hub
!   INTEGER,             INTENT(IN)     :: NNacelle                ! Number of elements on the nacelle
!   INTEGER,             INTENT(IN)     :: NTwr                    ! Number of elements on the tower
!   INTEGER,             INTENT(IN)     :: NTail                   ! Number of elements on the tail
!   INTEGER,             INTENT(OUT)    :: ErrorStatus             ! Determines if an error was encountered
!
!
!      ! Allocate the states
!
!   IF (.NOT. ALLOCATED(StateVar%Blade) .AND. NB*NBEle > 0 ) THEN
!      ALLOCATE( StateVar%Blade(NB,NBEle), STAT=ErrorStatus )
!      IF (ErrorStatus /=0) RETURN
!   ENDIF
!
!   IF (.NOT. ALLOCATED(StateVar%Hub) .AND. NHub > 0 ) THEN
!      ALLOCATE( StateVar%Hub(NHub), STAT=ErrorStatus )
!      IF (ErrorStatus /=0) RETURN
!   ENDIF
!
!   IF (.NOT. ALLOCATED(StateVar%Nacelle) .AND. NNacelle > 0 ) THEN
!      ALLOCATE( StateVar%Nacelle(NNacelle), STAT=ErrorStatus )
!      IF (ErrorStatus /=0) RETURN
!   ENDIF
!
!   IF (.NOT. ALLOCATED(StateVar%Tower) .AND. NTwr > 0 ) THEN
!      ALLOCATE( StateVar%Tower(NTwr), STAT=ErrorStatus )
!      IF (ErrorStatus /=0) RETURN
!   ENDIF
!
!   IF (.NOT. ALLOCATED(StateVar%Tail) .AND. NTail > 0 ) THEN
!      ALLOCATE( StateVar%Tail(NTail), STAT=ErrorStatus )
!      IF (ErrorStatus /=0) RETURN
!   ENDIF
!
!   ErrorStatus = 0
!
!
!END SUBROUTINE AllocateStates
!====================================================================================================

END MODULE SharedTypes
