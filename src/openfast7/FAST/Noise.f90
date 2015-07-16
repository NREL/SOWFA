MODULE Noise

USE                             NWTC_Library

!bjj: several subroutines have default FORTRAN typing:
! IMPLICIT INTEGER (I-N), REAL (A-H, O-Z)
! so this module does not have IMPLICIT NONE right now.  
! The subroutines should be changed to avoid this problem!!!
! IMPLICIT                        NONE


REAL(ReKi)                   :: AlphaNoise                                      ! angle of attack for each element used in noise calculation
REAL(ReKi)                   :: ALPRAT       = 1.0                              ! TIP LIFT CURVE SLOPE and(Default = 1.0)
REAL(ReKi), ALLOCATABLE      :: AvePressure (:,:)                               ! average mean square pressure at each frequency relative to observer
REAL(ReKi), ALLOCATABLE      :: AveSPL      (:,:)                               ! average sound pressure level at observer location
REAL(ReKi)                   :: C0                                              ! Speed of sound (m/s)
REAL(ReKi), ALLOCATABLE      :: ChordAngleLE(:,:)                               ! Directivity angle between trailing edge coord sys and observer position relative to chord
REAL(ReKi), ALLOCATABLE      :: ChordAngleTE(:,:)                               ! Directivity angle between trailing edge coord sys and observer position relative to chord
REAL(ReKi)                   :: KinViscosity
REAL(ReKi)                   :: MeanVNoise   = 0.0                              ! mean wind speed (m/s)
REAL(ReKi), ALLOCATABLE      :: OASPL       (:,:)                               ! overall sound pressure level of each element
REAL(ReKi), ALLOCATABLE      :: OASPLBlunt  (:,:)                               ! Overall sound pressure level of each element associated with bluntness
REAL(ReKi), ALLOCATABLE      :: OASPLInflow (:,:)                               ! Overall sound pressure level of each element associated with tip noise
REAL(ReKi), ALLOCATABLE      :: OASPLLBL    (:,:)                               ! Overall sound pressure level of each element associated with LBL
REAL(ReKi), ALLOCATABLE      :: OASPLSep    (:,:)                               ! Overall sound pressure level of each element associated with separated TBL
REAL(ReKi), ALLOCATABLE      :: OASPLTBLAll (:,:)                               ! Overall sound pressure level of each element associated with all TBL mechanisms
REAL(ReKi), ALLOCATABLE      :: OASPLTBLP   (:,:)                               ! Overall sound pressure level of each element associated with pressure side TBL
REAL(ReKi), ALLOCATABLE      :: OASPLTBLS   (:,:)                               ! Overall sound pressure level of each element associated with suction side TBL
REAL(ReKi), ALLOCATABLE      :: OASPLTip    (:,:)                               ! Overall sound pressure level of each element associated with tip noise
REAL(ReKi), ALLOCATABLE      :: rLEtoObserve(:,:)                               ! Position vector from observer to current point S leading edge on the blade
REAL(ReKi)                   :: RObserve    (3)                                 ! (x,y,z) Observer location in tower-base coordinate system
REAL(ReKi), ALLOCATABLE      :: rTEtoObserve(:,:)                               ! Position vector from observer to current point S trailing edge on the blade
REAL(ReKi), ALLOCATABLE      :: SpanAngleLE (:,:)                               ! Directivity angle between leading edge coord sys and observer position relative to span
REAL(ReKi), ALLOCATABLE      :: SpanAngleTE (:,:)                               ! Directivity angle between trailing edge coord sys and observer position relative to span
REAL(ReKi), ALLOCATABLE      :: TEAngle     (:)                                 ! Trailing edge angle of each blade element
REAL(ReKi), ALLOCATABLE      :: TEThick     (:)                                 ! Trailing edge thickness of each blade element
REAL(ReKi)                   :: TINoise      = 0.0                              ! longitudinal turbulence intensity used for inflow noise
REAL(ReKi)                   :: UNoise = 0.0

LOGICAL                      :: ROUND                                           ! LOGICAL INDICATING ROUNDED TIP     ---
LOGICAL                      :: IBLUNT                                          ! FLAG TO COMPUTE BLUNTNESS NOISE    ---
LOGICAL                      :: ILAM                                            ! FLAG TO COMPUTE LBL NOISE          ---
LOGICAL                      :: ITIP                                            ! FLAG TO COMPUTE TIP NOISE          ---
LOGICAL                      :: ITURB                                           ! FLAG TO COMPUTE TBLTE NOISE        ---
LOGICAL                      :: IInflow                                         ! FLAG TO COMPUTE Turbulent Inflow NOISE        ---

INTEGER(4)                   :: ITRIP                                           ! FLAG TO TRIP BOUNDARY LAYER (= 0 untripped BL, = 1 tripped BL, = 2 lightly tripped BL)
INTEGER(4)                   :: NAverage     = 0                                ! Number of points to calculate the average pressure
INTEGER(4)                   :: NoiseOutSwitch                                  ! Switch to determine which time series of overall sound pressure level to output
                                                                                ! = 1 Total, = 2 Laminar Boundary Layer, = 3  Total Turbulent Boundary Layer
                                                                                ! = 4 Turbulent Inflow, = 5 Tip, = 6 Blunt Trailing Edge
                                                                                ! = 7 Turbulent Boundary Layer - Pressure Side, = 8 Turbulent Boundary Layer - Suction Side
                                                                                ! = 9 Separated Turbulent Boundary Layer

INTEGER(4), PARAMETER        :: NMech        = 8                                ! Number of noise mechanisms to calculate
INTEGER(4), PARAMETER        :: NFrequency   = 27                               ! Number of 1/3 octave bands

REAL(ReKi), DIMENSION(NFrequency) :: FrequencyCenter = &    ! Center frequency of each 1/3 octave band
                (/ 100.   ,  125.   ,  160.   ,  200.   ,  250.   , &
                   315.   ,  400.   ,  500.   ,  630.   ,  800.   , &
                  1000.   , 1250.   , 1600.   , 2000.   , 2500.   , &
                  3150.   , 4000.   , 5000.   , 6300.   , 8000.   , &
                 10000.   ,12500.   ,16000.   ,20000.   ,25000.   , &
                 31500.   ,40000.   /)
!bjj: should FrequencyCenter be a parameter?


CONTAINS
!====================================================================================================
SUBROUTINE PredictNoise

USE                             AeroElem
USE                             Blades
USE                             Output
USE                             RtHndSid
USE                             SimCont
USE                             TurbConf
USE                             AeroDyn


IMPLICIT                        NONE

REAL(ReKi)                   :: PBLNT                                           ! Instantaneous pressure level at given frequency for Bluntness prediction
REAL(ReKi)                   :: PLBL                                            ! Instantaneous pressure level at given frequency for LBL prediction
REAL(ReKi)                   :: PTBLALH                                         ! Instantaneous pressure level at given frequency for separated TBLTE prediction
REAL(ReKi)                   :: PTBLP                                           ! Instantaneous pressure level at given frequency for pressure side TBLTE prediction
REAL(ReKi)                   :: PTBLS                                           ! Instantaneous pressure level at given frequency for suction side TBLTE prediction
REAL(ReKi)                   :: PTip                                            ! Instantaneous pressure level at given frequency for Tip Noise Prediction
REAL(ReKi)                   :: PTI                                             ! Instantaneous pressure level at given frequency for turbulent inflow Prediction
REAL(ReKi)                   :: Ptotal                                          ! Overall pressure for each element for Total prediction
REAL(ReKi)                   :: PtotalBlunt                                     ! Overall pressure for each element for Bluntness prediction
REAL(ReKi)                   :: PtotalInflow                                    ! Overall pressure for each element for turbulent inflow Prediction
REAL(ReKi)                   :: PtotalLBL                                       ! Overall pressure for each element for LBL prediction
REAL(ReKi)                   :: PtotalSep                                       ! Overall pressure for each element for separated TBLTE prediction
REAL(ReKi)                   :: PtotalTBLAll                                    ! Overall pressure for each element for total TBLTE prediction
REAL(ReKi)                   :: PtotalTBLP                                      ! Overall pressure for each element for pressure side TBLTE prediction
REAL(ReKi)                   :: PtotalTBLS                                      ! Overall pressure for each element for suction side TBLTE prediction
REAL(ReKi)                   :: PtotalTip                                       ! Overall pressure for each element for Tip Noise Prediction
REAL(ReKi)                   :: SPLLBL   (NFrequency)                           ! Sound pressure level associated with LBL prediction (dB)
REAL(ReKi)                   :: SPLTBL   (NFrequency)                           ! Sound pressure level associated with TBLTE prediction (dB)
REAL(ReKi)                   :: SPLP     (NFrequency)                           ! Sound pressure level associated with pressure side TBLTE prediction (dB)
REAL(ReKi)                   :: SPLS     (NFrequency)                           ! Sound pressure level associated with suction side TBLTE prediction (dB)
REAL(ReKi)                   :: SPLALPH  (NFrequency)                           ! Sound pressure level associated with separated TBLTE prediction (dB)
REAL(ReKi)                   :: SPLti    (NFrequency)                           ! Sound pressure level associated with turbulent inflow prediction (dB)
REAL(ReKi)                   :: SPLBLNT  (NFrequency)                           ! Sound pressure level associated with Bluntness prediction (dB)
REAL(ReKi)                   :: SPLTIP   (NFrequency)                           ! Sound Pressure Level Associated with Tip Noise Prediction (dB)

INTEGER(4)                   :: I                                               ! A generic index for DO loops.
INTEGER(4)                   :: III                                             ! A generic index for DO loops.
INTEGER(4)                   :: K                                               ! A generic index for DO loops.

INTEGER                       :: ErrStat

! Calculate observer distance and directivity angles

CALL CalcObserve

DO K = 1, NumBl


!     FOR EACH BLADE SEGMENT, MAKE A NOISE PREDICTION ACCORDING
!     TO THE MECHANISMS SELECTED. TIP NOISE IS PREDICTED FOR
!     THE LAST SEGMENT ONLY.
!     ---------------------------------------------------------

   DO III=1,BldNodes

        UNoise = SQRT( AD_GetCurrentValue('W2',ErrStat, IBlade=K, IElement=III) )

        IF (UNoise .GE. C0) CYCLE

        AlphaNoise = ABS( R2D * AD_GetCurrentValue('ALPHA',ErrStat, IBlade=K, IElement=III) )

        IF ( ILAM .AND. ( ITRIP .EQ. 0 ) )                                     &
         CALL LBLVS(AlphaNoise,Chord(III),UNoise,SPLLBL, &
                 ChordAngleTE(K,III),SpanAngleTE(K,III),DRNodes(III),rTEtoObserve(K,III))

        IF ( ITURB )                                              &
         CALL TBLTE(AlphaNoise,Chord(III),UNoise,SPLP,       &
                SPLS,SPLALPH,SPLTBL,ChordAngleTE(K,III),SpanAngleTE(K,III),DRNodes(III),rTEtoObserve(K,III))

        IF ( IBLUNT )                                            &
         CALL BLUNT(AlphaNoise,Chord(III),UNoise,SPLBLNT,   &
                  ChordAngleTE(K,III),SpanAngleTE(K,III),DRNodes(III),rTEtoObserve(K,III),TEThick(III),TEAngle(III))

        IF ( ITIP .AND. ( III .EQ. BldNodes ) )                       &
          CALL TIPNOIS(AlphaNoise,ALPRAT,Chord(III),UNoise,SPLTIP,      &
                       ChordAngleTE(K,III),SpanAngleTE(K,III),rTEtoObserve(K,III))

        IF ( IInflow )  &
          CALL InflowNoise(UNoise,Chord(III),DRNodes(III),rLEtoObserve(K,III), &
                            ChordAngleLE(K,III),SpanAngleLE(K,III),SPLti)


!      ADD IN THIS SEGMENT'S CONTRIBUTION ON A MEAN-SQUARE
!      PRESSURE BASIS
!      ---------------------------------------------------
        Ptotal = 0.0
        PtotalLBL= 0.0
        PtotalTBLP= 0.0
        PtotalTBLS= 0.0
        PtotalSep= 0.0
        PtotalTBLAll = 0.0
        PtotalBlunt= 0.0
        PtotalTip= 0.0
        PtotalInflow= 0.0

    DO I=1,NFrequency

          IF ( ILAM .AND. ( ITRIP .EQ. 0 ) )  THEN
            PLBL = 10.**(SPLLBL(I)/10.)
            PtotalLBL = PtotalLBL + PLBL
            Ptotal = Ptotal + PLBL
            AvePressure (5,I) = AvePressure (5,I) + PLBL
            AvePressure (4,I) = AvePressure (4,I) + PLBL
          ENDIF

          IF ( ITURB )  THEN
            PTBLP = 10.**(SPLP(I)/10.)
            PTBLS = 10.**(SPLS(I)/10.)
            PTBLALH = 10.**(SPLALPH(I)/10.)
            PtotalTBLP = PtotalTBLP + PTBLP
            PtotalTBLS = PtotalTBLS + PTBLS
            PtotalSep  = PtotalSep  + PTBLALH
            Ptotal = Ptotal + PTBLP + PTBLS + PTBLALH
            PtotalTBLAll = PtotalTBLAll + 10.**(SPLTBL(I)/10.)
            AvePressure (1,I) = AvePressure (1,I) + PTBLP
            AvePressure (2,I) = AvePressure (2,I) + PTBLS
            AvePressure (3,I) = AvePressure (3,I) + PTBLALH
            AvePressure (4,I) = AvePressure (4,I) + PTBLP + PTBLS + PTBLALH
          ENDIF

          IF ( IBLUNT )  THEN
            PBLNT = 10.**(SPLBLNT(I)/10.)
            PtotalBlunt = PtotalBlunt + PBLNT
            Ptotal = Ptotal + PBLNT
            AvePressure (6,I) = AvePressure (6,I) + PBLNT
            AvePressure (4,I) = AvePressure (4,I) + PBLNT
          ENDIF

          IF ( ITIP .AND. ( III .EQ. BldNodes ) )  THEN
            PTip = 10.**(SPLTIP(I)/10.)
            PtotalTip = PtotalTip + PTip
            Ptotal = Ptotal + PTip
            AvePressure (7,I) = AvePressure (7,I) + PTip
            AvePressure (4,I) = AvePressure (4,I) + PTip
          ENDIF

          IF ( IInflow .AND. ( TINoise .NE. 0.0 ) )  THEN
            PTI = 10.**(SPLti(I)/10.)
            PtotalInflow = PtotalInflow + PTI
            Ptotal = Ptotal + PTI
            AvePressure (8,I) = AvePressure (8,I) + PTI
            AvePressure (4,I) = AvePressure (4,I) + PTI
          ENDIF

   ENDDO ! I = 1, NFrequency

   IF (PtotalLBL    .NE. 0.) OASPLLBL   (K,III) = 10.*LOG10(PtotalLBL)
   IF (PtotalTBLP   .NE. 0.) OASPLTBLP  (K,III) = 10.*LOG10(PtotalTBLP)
   IF (PtotalTBLS   .NE. 0.) OASPLTBLS  (K,III) = 10.*LOG10(PtotalTBLS)
   IF (PtotalSep    .NE. 0.) OASPLSep   (K,III) = 10.*LOG10(PtotalSep)
   IF (PtotalTBLAll .NE. 0.) OASPLTBLAll(K,III) = 10.*LOG10(PtotalTBLAll)
   IF (PtotalBlunt  .NE. 0.) OASPLBlunt (K,III) = 10.*LOG10(PtotalBlunt)
   IF (PtotalTip    .NE. 0.) OASPLTip   (K,III) = 10.*LOG10(PtotalTip)
   IF (PtotalInflow .NE. 0.) OASPLInflow(K,III) = 10.*LOG10(PtotalInflow)
   OASPL(K,III) = 10.*LOG10(Ptotal)
 ENDDO ! III = 1, BldNodes

ENDDO ! K = 1, NumBl

NAverage = NAverage + 1 ! The noise spectrum for each time step is averaged into the total

RETURN
END SUBROUTINE PredictNoise
!====================================================================================================

      SUBROUTINE LBLVS(ALPSTAR,C,U ,SPLLAM,THETA,PHI,L,R)

!                  --------------------------------
!                  ***** VARIABLE DEFINITIONS *****
!                  --------------------------------
!
!       VARIABLE NAME               DEFINITION                  UNITS
!       -------------               ----------                  -----
!
!
!       ALPSTAR             ANGLE OF ATTACK                   DEGREES
!       C                  CHORD LENGTH                       METERS
!       C0                 SPEED OF SOUND                     METERS/SEC
!       D                  REYNOLDS NUMBER RATIO              ---
!       DBARH              HIGH FREQUENCY DIRECTIVITY         ---
!       DELTAP             PRESSURE SIDE BOUNDARY LAYER
!                            THICKNESS                        METERS
!       DSTRP              PRESSURE SIDE BOUNDARY LAYER
!                            DISPLACEMENT THICKNESS           METERS
!       DSTRS              SUCTION SIDE BOUNDARY LAYER
!                            DISPLACEMENT THICKNESS           METERS
!       E                  STROUHAL NUMBER RATIO              ---
!       FRCEN              1/3 OCTAVE FREQUENCIES             HERTZ
!       G1                 SOUND PRESSURE LEVEL FUNCTION      DB
!       G2                 OVERALL SOUND PRESSURE LEVEL
!                            FUNCTION                         DB
!       G3                 OVERALL SOUND PRESSURE LEVEL
!                            FUNCTION                         DB
!       ITRIP              FLAG TO TRIP BOUNDARY LAYER        ---
!       L                  SPAN                               METERS
!       M                  MACH NUMBER                        ---
!       NFREQ              NUMBER OF FREQUENCIES              ---
!       OASPL              OVERALL SOUND PRESSURE LEVEL       DB
!       PHI                DIRECTIVITY ANGLE                  DEGREES
!       R                  OBSERVER DISTANCE FROM SEGMENT     METERS
!       RC                 REYNOLDS NUMBER BASED ON CHORD     ---
!       RC0                REFERENCE REYNOLDS NUMBER          ---
!       SCALE              GEOMETRIC SCALING TERM
!       SPLLAM             SOUND PRESSURE LEVEL DUE TO
!                            LAMINAR MECHANISM                DB
!       STPRIM             STROUHAL NUMBER BASED ON PRESSURE
!                            SIDE BOUNDARY LAYER THICKNESS    ---
!       ST1PRIM            REFERENCE STROUHAL NUMBER          ---
!       STPKPRM            PEAK STROUHAL NUMBER               ---
!       THETA              DIRECTIVITY ANGLE                  DEGREES
!       U                  FREESTREAM VELOCITY                METERS/SEC
!       KinViscosity       KINEMATIC VISCOSITY                M2/SEC



      REAL(ReKi)  :: L
      REAL(ReKi)  :: M
      REAL(ReKi)  :: SPLLAM   (NFrequency)
      REAL(ReKi)  :: STPRIM   (NFrequency)


!      COMPUTE REYNOLDS NUMBER AND MACH NUMBER
!      ---------------------------------------

      M        = U  / C0

      RC       = U  * C/KinViscosity

!      COMPUTE BOUNDARY LAYER THICKNESSES
!      ----------------------------------

      CALL THICK(C,U ,ALPSTAR,DELTAP,DSTRS,DSTRP)



!      COMPUTE DIRECTIVITY FUNCTION
!      ----------------------------

      CALL DIRECTH(M,THETA,PHI,DBARH)
      IF (DBARH <= 0) THEN
          SPLLAM = 0.
          RETURN
      ENDIF



!      COMPUTE REFERENCE STROUHAL NUMBER
!      ---------------------------------

      IF (RC .LE. 1.3E+05) ST1PRIM = .18
      IF((RC .GT. 1.3E+05).AND.(RC.LE.4.0E+05))ST1PRIM=.001756*RC**.3931
      IF (RC .GT. 4.0E+05) ST1PRIM = .28

      STPKPRM  = 10.**(-.04*ALPSTAR) * ST1PRIM



!      COMPUTE REFERENCE REYNOLDS NUMBER
!      ---------------------------------

      IF (ALPSTAR .LE. 3.0) RC0=10.**(.215*ALPSTAR+4.978)
      IF (ALPSTAR .GT. 3.0) RC0=10.**(.120*ALPSTAR+5.263)




!      COMPUTE PEAK SCALED SPECTRUM LEVEL
!      ----------------------------------

      D   = RC / RC0

      IF (D .LE. .3237) G2=77.852*LOG10(D)+15.328
      IF ((D .GT. .3237).AND.(D .LE. .5689)) &
        G2 = 65.188*LOG10(D) + 9.125
      IF ((D .GT. .5689).AND.(D .LE. 1.7579)) &
        G2 = -114.052 * LOG10(D)**2.
      IF ((D .GT. 1.7579).AND.(D .LE. 3.0889)) &
        G2 = -65.188*LOG10(D)+9.125
      IF (D .GT. 3.0889) G2 =-77.852*LOG10(D)+15.328


      G3      = 171.04 - 3.03 * ALPSTAR

      SCALE   = 10. * LOG10(DELTAP*M**5*DBARH*L/R**2)



!      COMPUTE SCALED SOUND PRESSURE LEVELS FOR EACH STROUHAL NUMBER
!      -------------------------------------------------------------

      DO 100 I=1,NFrequency

         STPRIM(I)  = FrequencyCenter(I) * DELTAP / U

         E          = STPRIM(I) / STPKPRM

         IF (E .LT. .5974) G1=39.8*LOG10(E)-11.12
         IF ((E .GE. .5974).AND.(E .LE. .8545)) &
           G1 = 98.409 * LOG10(E) + 2.0
         IF ((E .GE. .8545).AND.(E .LT. 1.17)) &
           G1 = -5.076+SQRT(2.484-506.25*(LOG10(E))**2.)
         IF ((E .GE. 1.17).AND.(E .LT. 1.674)) &
           G1 = -98.409 * LOG10(E) + 2.0
         IF (E .GE. 1.674) G1=-39.80*LOG10(E)-11.12

         SPLLAM(I) = G1 + G2 + G3 + SCALE

  100 CONTINUE

      RETURN

   END SUBROUTINE LBLVS
!====================================================================================================
      SUBROUTINE TBLTE(ALPSTAR,C,U ,SPLP,SPLS, &
                   SPLALPH,SPLTBL,THETA,PHI,L,R)

!                  --------------------------------
!                  ***** VARIABLE DEFINITIONS *****
!                  --------------------------------
!
!
!
!       VARIABLE NAME               DEFINITION                  UNITS
!       -------------               ----------                  -----
!
!       A                  STROUHAL NUMBER RATIO                 ---
!       A0                 FUNCTION USED IN 'A' CALCULATION      ---
!       A02                FUNCTION USED IN 'A' CALCULATION      ---
!       AA                 'A' SPECTRUM SHAPE EVALUATED AT
!                             STROUHAL NUMBER RATIO              DB
!       ALPSTAR            ANGLE OF ATTACK                     DEGREES
!       AMAXA              MAXIMUM 'A' CURVE EVALUATED AT
!                            STROUHAL NUMBER RATIO                DB
!       AMAXA0             MAXIMUM 'A' CURVE EVALUATED AT A0      DB
!       AMAXA02            MAXIMUM 'A' CURVE EVALUATED AT A02     DB
!       AMAXB              MAXIMUM 'A' CURVE EVALUATED AT B       DB
!       AMINA              MINIMUM 'A' CURVE EVALUATED AT
!                            STROUHAL NUMBER RATIO                DB
!       AMINA0             MINIMUM 'A' CURVE EVALUATED AT A0      DB
!       AMINA02            MINIMUM 'A' CURVE EVALUATED AT A02     DB
!       AMINB              MINIMUM 'A' CURVE EVALUATED AT B       DB
!       ARA0               INTERPOLATION FACTOR                  ---
!       ARA02              INTERPOLATION FACTOR                  ---
!       B                  STROUHAL NUMBER RATIO                 ---
!       B0                 FUNCTION USED IN 'B' CALCULATION      ---
!       BB                 'B' SPECTRUM SHAPE EVALUATED AT
!                            STROUHAL NUMBER RATIO                DB
!       BETA               USED IN 'B' COMPUTATION               ---
!       BETA0              USED IN 'B' COMPUTATION               ---
!       BMAXB              MAXIMUM 'B' EVALUATED AT B             DB
!       BMAXB0             MAXIMUM 'B' EVALUATED AT B0            DB
!       BMINB              MINIMUM 'B' EVALUATED AT B             DB
!       BMINB0             MINIMUM 'B' EVALUATED AT B0            DB
!       BRB0               INTERPOLATION FACTOR                   DB
!       C                  CHORD LENGTH                          METERS
!       C0                 SPEED OF SOUND                      METERS/SEC
!       DBARH              HIGH FREQUENCY DIRECTIVITY             ---
!       DBARL              LOW FREQUENCY DIRECTIVITY              ---
!       DELK1              CORRECTION TO AMPLITUDE FUNCTION       DB
!       DELTAP             PRESSURE SIDE BOUNDARY LAYER THICKNESS METERS
!       DSTRP              PRESSURE SIDE DISPLACEMENT THICKNESS  METERS
!       DSTRS              SUCTION SIDE DISPLACEMENT THICKNESS   METERS
!       FRCEN              ARRAY OF CENTERED FREQUENCIES         HERTZ
!       GAMMA              USED IN 'B' COMPUTATION                ---
!       GAMMA0             USED IN 'B' COMPUTATION                ---
!       ITRIP              TRIGGER TO TRIP BOUNDARY LAYER         ---
!       K1                 AMPLITUDE FUNCTION                     DB
!       K2                 AMPLITUDE FUNCTION                     DB
!       L                  SPAN                                  METERS
!       M                  MACH NUMBER                            ---
!       NFREQ              NUMBER OF CENTERED FREQUENCIES         ---
!       PHI                DIRECTIVITY ANGLE                    DEGREES
!       P1                 PRESSURE SIDE PRESSURE               NT/M2
!       P2                 SUCTION SIDE PRESSURE                NT/M2
!       P4                 PRESSURE FROM ANGLE OF ATTACK
!                            CONTRIBUTION                       NT/M2
!       R                  SOURCE TO OBSERVER DISTANCE           METERS
!       RC                 REYNOLDS NUMBER BASED ON  CHORD        ---
!       RDSTRP             REYNOLDS NUMBER BASED ON PRESSURE
!                            SIDE DISPLACEMENT THICKNESS          ---
!       RDSTRS             REYNOLDS NUMBER BASED ON SUCTION
!                            SIDE DISPLACEMENT THICKNESS          ---
!       SPLALPH            SOUND PRESSURE LEVEL DUE TO ANGLE OF
!                            ATTACK CONTRIBUTION                  DB
!       SPLP               SOUND PRESSURE LEVEL DUE TO PRESSURE
!                            SIDE OF AIRFOIL                      DB
!       SPLS               SOUND PRESSURE LEVEL DUE TO SUCTION
!                            SIDE OF AIRFOIL                      DB
!       SPLTBL             TOTAL SOUND PRESSURE LEVEL DUE TO
!                            TBLTE MECHANISM                      DB
!       STP                PRESSURE SIDE STROUHAL NUMBER          ---
!       STS                SUCTION SIDE STROUHAL NUMBER           ---
!       ST1                PEAK STROUHAL NUMBER                   ---
!       ST1PRIM            PEAK STROUHAL NUMBER                   ---
!       ST2                PEAK STROUHAL NUMBER                   ---
!       STPEAK             PEAK STROUHAL NUMBER                   ---
!       SWITCH             LOGICAL FOR COMPUTATION OF ANGLE
!                            OF ATTACK CONTRIBUTION               ---
!       THETA              DIRECTIVITY ANGLE                     DEGREES
!       U                  VELOCITY                             METERS/SEC
!       KinViscosity       KINEMATIC VISCOSITY                   M2/SEC
!       XCHECK             USED TO CHECK FOR ANGLE OF ATTACK
!                            CONTRIBUTION                         ---
!


      REAL(ReKi)  :: K1
      REAL(ReKi)  :: K2
      REAL(ReKi)  :: L
      REAL(ReKi)  :: M
      REAL(ReKi)  :: SPLALPH  (NFrequency)
      REAL(ReKi)  :: SPLP     (NFrequency)
      REAL(ReKi)  :: SPLS     (NFrequency)
      REAL(ReKi)  :: SPLTBL   (NFrequency)
      REAL(ReKi)  :: STP      (NFrequency)
      REAL(ReKi)  :: STS      (NFrequency)

      LOGICAL     :: SWITCH

      RC       = U  * C / KinViscosity
      M        = U  / C0


!      COMPUTE BOUNDARY LAYER THICKNESSES
!      ----------------------------------

      CALL THICK(C,U ,ALPSTAR,DELTAP,DSTRS,DSTRP)

!     COMPUTE DIRECTIVITY FUNCTION
!     ----------------------------

      CALL DIRECTL(M,THETA,PHI,DBARL)
      CALL DIRECTH(M,THETA,PHI,DBARH)
      IF (DBARH <= 0) THEN
          SPLP = 0.
          SPLS = 0.
          SPLALPH = 0.
          RETURN
      ENDIF


!     CALCULATE THE REYNOLDS NUMBERS BASED ON PRESSURE AND
!     SUCTION DISPLACEMENT THICKNESS
!     ---------------------------------------------------

      RDSTRS = DSTRS * U  / KinViscosity
      RDSTRP = DSTRP * U  / KinViscosity

!      DETERMINE PEAK STROUHAL NUMBERS TO BE USED FOR
!      'A' AND 'B' CURVE CALCULATIONS
!      ----------------------------------------------

      ST1    = .02 * M ** (-.6)

      IF (ALPSTAR .LE. 1.333) ST2 = ST1
      IF ((ALPSTAR .GT. 1.333).AND.(ALPSTAR .LE. 12.5)) &
        ST2 = ST1*10.**(.0054*(ALPSTAR-1.333)**2.)
      IF (ALPSTAR .GT. 12.5) ST2 = 4.72 * ST1


      ST1PRIM = (ST1+ST2)/2.


      CALL A0COMP(RC,A0)
      CALL A0COMP(3.*RC,A02)

!      EVALUATE MINIMUM AND MAXIMUM 'A' CURVES AT A0
!      ----------------------------------------------

      CALL AMIN(A0,AMINA0)
      CALL AMAX(A0,AMAXA0)

      CALL AMIN(A02,AMINA02)
      CALL AMAX(A02,AMAXA02)

!      COMPUTE 'A' MAX/MIN RATIO
!      -------------------------

      ARA0  = (20. + AMINA0) / (AMINA0 - AMAXA0)
      ARA02 = (20. + AMINA02)/ (AMINA02- AMAXA02)

!      COMPUTE B0 TO BE USED IN 'B' CURVE CALCULATIONS
!      -----------------------------------------------

      IF (RC .LT. 9.52E+04) B0 = .30
      IF ((RC .GE. 9.52E+04).AND.(RC .LT. 8.57E+05)) &
         B0 = (-4.48E-13)*(RC-8.57E+05)**2. + .56
      IF (RC .GE. 8.57E+05) B0 = .56

!      EVALUATE MINIMUM AND MAXIMUM 'B' CURVES AT B0
!      ----------------------------------------------

      CALL BMIN(B0,BMINB0)
      CALL BMAX(B0,BMAXB0)

!      COMPUTE 'B' MAX/MIN RATIO
!      -------------------------

      BRB0  = (20. + BMINB0) / (BMINB0 - BMAXB0)

!      FOR EACH CENTER FREQUENCY, COMPUTE AN
!      'A' PREDICTION FOR THE PRESSURE SIDE
!      -------------------------------------

      STPEAK = ST1

      DO 100 I=1,NFrequency
        STP(I) = FrequencyCenter(I) * DSTRP / U
        A      = LOG10( STP(I) / STPEAK )
        CALL AMIN(A,AMINA)
        CALL AMAX(A,AMAXA)
        AA     = AMINA + ARA0 * (AMAXA - AMINA)

        IF (RC .LT. 2.47E+05) K1 = -4.31 * LOG10(RC) + 156.3
        IF((RC .GE. 2.47E+05).AND.(RC .LT. 8.0E+05)) &
          K1 = -9.0 * LOG10(RC) + 181.6
        IF (RC .GT. 8.0E+05) K1 = 128.5

        IF (RDSTRP .LE. 5000.) DELK1 = -ALPSTAR*(5.29-1.43* &
          LOG10(RDSTRP))
        IF (RDSTRP .GT. 5000.) DELK1 = 0.0

        SPLP(I)=AA+K1-3.+10.*LOG10(DSTRP*M**5.*DBARH*L/R**2.)+DELK1




      GAMMA   = 27.094 * M +  3.31
      BETA    = 72.650 * M + 10.74
      GAMMA0  = 23.430 * M +  4.651
      BETA0   =-34.190 * M - 13.820

      IF (ALPSTAR .LE. (GAMMA0-GAMMA)) K2 = -1000.0
      IF ((ALPSTAR.GT.(GAMMA0-GAMMA)).AND.(ALPSTAR.LE.(GAMMA0+GAMMA))) &
       K2=SQRT(BETA**2.-(BETA/GAMMA)**2.*(ALPSTAR-GAMMA0)**2.)+BETA0
      IF (ALPSTAR .GT. (GAMMA0+GAMMA)) K2 = -12.0

      K2 = K2 + K1



      STS(I) = FrequencyCenter(I) * DSTRS / U

!      CHECK FOR 'A' COMPUTATION FOR SUCTION SIDE
!      ------------------------------------------

      XCHECK = GAMMA0
      SWITCH = .FALSE.
      IF ((ALPSTAR .GE. XCHECK).OR.(ALPSTAR .GT. 12.5))SWITCH=.TRUE.
      IF (.NOT. SWITCH) THEN
        A      = LOG10( STS(I) / ST1PRIM )
        CALL AMIN(A,AMINA)
        CALL AMAX(A,AMAXA)
        AA = AMINA + ARA0 * (AMAXA - AMINA)

        SPLS(I) = AA+K1-3.+10.*LOG10(DSTRS*M**5.*DBARH* &
                 L/R**2.)

!      'B' CURVE COMPUTATION
!       --------------------

        B = ABS(LOG10(STS(I) / ST2))
        CALL BMIN(B,BMINB)
        CALL BMAX(B,BMAXB)
        BB = BMINB + BRB0 * (BMAXB-BMINB)
        SPLALPH(I)=BB+K2+10.*LOG10(DSTRS*M**5.*DBARH*L/R**2.)

      ELSE

!       THE 'A' COMPUTATION IS DROPPED IF 'SWITCH' IS TRUE
!       --------------------------------------------------


        SPLS(I) = 0.0 + 10.*LOG10(DSTRS*M**5.*DBARL*L/R**2.)
        SPLP(I) = 0.0 + 10.*LOG10(DSTRS*M**5.*DBARL*L/R**2.)
        B = ABS(LOG10(STS(I) / ST2))
        CALL AMIN(B,AMINB)
        CALL AMAX(B,AMAXB)
        BB = AMINB + ARA02 * (AMAXB-AMINB)
        SPLALPH(I)=BB+K2+10.*LOG10(DSTRS*M**5.*DBARL*L/R**2.)
      ENDIF


!      SUM ALL CONTRIBUTIONS FROM 'A' AND 'B' ON BOTH
!      PRESSURE AND SUCTION SIDE ON A MEAN-SQUARE PRESSURE
!      BASIS
!      ---------------------------------------------------

      IF (SPLP(I)    .LT. -100.) SPLP(I)    = -100.
      IF (SPLS(I)    .LT. -100.) SPLS(I)    = -100.
      IF (SPLALPH(I) .LT. -100.) SPLALPH(I) = -100.

      P1  = 10.**(SPLP(I) / 10.)
      P2  = 10.**(SPLS(I) / 10.)
      P4  = 10.**(SPLALPH(I) / 10.)

      SPLTBL(I) = 10. * LOG10(P1 + P2 + P4)

  100 CONTINUE

      RETURN
      END SUBROUTINE TBLTE
!====================================================================================================

      SUBROUTINE AMIN(A,AMINA)

!     THIS SUBROUTINE DEFINES THE CURVE FIT CORRESPONDING
!     TO THE A-CURVE FOR THE MINIMUM ALLOWED REYNOLDS NUMBER.
!

      X1 = ABS(A)

      IF (X1 .LE. .204) AMINA=SQRT(67.552-886.788*X1**2.)-8.219
      IF((X1 .GT. .204).AND.(X1 .LE. .244))AMINA=-32.665*X1+3.981
      IF (X1 .GT. .244)AMINA=-142.795*X1**3.+103.656*X1**2.-57.757*X1+6.006

      RETURN

      END SUBROUTINE AMIN
!====================================================================================================
      SUBROUTINE AMAX(A,AMAXA)

!     THIS SUBROUTINE DEFINES THE CURVE FIT CORRESPONDING
!     TO THE A-CURVE FOR THE MAXIMUM ALLOWED REYNOLDS NUMBER.

      X1 = ABS(A)

      IF (X1 .LE. .13)AMAXA=SQRT(67.552-886.788*X1**2.)-8.219
      IF((X1 .GT. .13).AND.(X1 .LE. .321))AMAXA=-15.901*X1+1.098
      IF (X1 .GT. .321)AMAXA=-4.669*X1**3.+3.491*X1**2.-16.699*X1+1.149

      RETURN

      END SUBROUTINE AMAX
!====================================================================================================
      SUBROUTINE BMIN(B,BMINB)

!     THIS SUBROUTINE DEFINES THE CURVE FIT CORRESPONDING
!     TO THE B-CURVE FOR THE MINIMUM ALLOWED REYNOLDS NUMBER.

      X1 = ABS(B)

      IF (X1 .LE. .13)BMINB=SQRT(16.888-886.788*X1**2.)-4.109
      IF((X1 .GT. .13).AND.(X1 .LE. .145))BMINB=-83.607*X1+8.138
      IF (X1.GT..145)BMINB=-817.81*X1**3.+355.21*X1**2.-135.024*X1+10.619

      RETURN

      END SUBROUTINE BMin
!====================================================================================================
      SUBROUTINE BMAX(B,BMAXB)

!     THIS SUBROUTINE DEFINES THE CURVE FIT CORRESPONDING
!     TO THE B-CURVE FOR THE MAXIMUM ALLOWED REYNOLDS NUMBER.

      X1 = ABS(B)

      IF (X1 .LE. .1) BMAXB=SQRT(16.888-886.788*X1**2.)-4.109
      IF((X1 .GT. .1).AND.(X1 .LE. .187))BMAXB=-31.313*X1+1.854
      IF (X1.GT..187)BMAXB=-80.541*X1**3.+44.174*X1**2.-39.381*X1+2.344

      RETURN

      END SUBROUTINE BMax
!====================================================================================================
      SUBROUTINE A0COMP(RC,A0)

!     THIS SUBROUTINE DETERMINES WHERE THE A-CURVE
!     TAKES ON A VALUE OF -20 dB.

      IF (RC .LT. 9.52E+04) A0 = .57
      IF ((RC .GE. 9.52E+04).AND.(RC .LT. 8.57E+05)) &
         A0 = (-9.57E-13)*(RC-8.57E+05)**2. + 1.13
      IF (RC .GE. 8.57E+05) A0 = 1.13
      RETURN

      END SUBROUTINE A0COMP
!====================================================================================================
      SUBROUTINE DIRECTH(M,THETA,PHI,DBAR)

!     THIS SUBROUTINE COMPUTES THE HIGH FREQUENCY
!     DIRECTIVITY FUNCTION FOR THE INPUT OBSERVER LOCATION


      REAL(ReKi)  :: M
      REAL(ReKi)  :: MC

      DEGRAD  = .017453

      MC     = .8 * M
      THETAR = THETA * DEGRAD
      PHIR   = PHI * DEGRAD

      DBAR=2.*SIN(THETAR/2.)**2.*SIN(PHIR)**2./((1.+M*COS(THETAR))* &
           (1.+(M-MC)*COS(THETAR))**2.)
      RETURN

      END SUBROUTINE DirectH
!====================================================================================================
      SUBROUTINE DIRECTL(M,THETA,PHI,DBAR)

!     THIS SUBROUTINE COMPUTES THE LOW FREQUENCY
!     DIRECTIVITY FUNCTION FOR THE INPUT OBSERVER LOCATION



      REAL(ReKi)  :: M
      REAL(ReKi)  :: MC

      DEGRAD  = .017453

      MC     = .8 * M
      THETAR = THETA * DEGRAD
      PHIR   = PHI * DEGRAD

      DBAR = (SIN(THETAR)*SIN(PHIR))**2/(1.+M*COS(THETAR))**4

      RETURN

      END SUBROUTINE DirectL
!====================================================================================================
      SUBROUTINE BLUNT(ALPSTAR,C,U ,SPLBLNT,THETA,PHI, &
                      L,R,H,PSI)

!                  --------------------------------
!                  ***** VARIABLE DEFINITIONS *****
!                  --------------------------------
!
!       VARIABLE NAME               DEFINITION                  UNITS
!       -------------               ----------                  -----
!
!       ALPSTAR            ANGLE OF ATTACK                     DEGREES
!       ATERM              USED TO COMPUTE PEAK STROUHAL NO.    ---
!       C                  CHORD LENGTH                        METERS
!       C0                 SPEED OF SOUND                      METERS/SEC
!       DBARH              HIGH FREQUENCY DIRECTIVITY           ---
!       DELTAP             PRESSURE SIDE BOUNDARY LAYER
!                            THICKNESS                          METERS
!       DSTARH             AVERAGE DISPLACEMENT THICKNESS
!                            OVER TRAILING EDGE BLUNTNESS       ---
!       DSTRAVG            AVERAGE DISPLACEMENT THICKNESS       METERS
!       DSTRP              PRESSURE SIDE DISPLACEMENT THICKNESS METERS
!       DSTRS              SUCTION SIDE DISPLACEMENT THICKNESS  METERS
!       ETA                RATIO OF STROUHAL NUMBERS             ---
!       FRCEN              ARRAY OF 1/3 OCTAVE CENTERED FREQ.   HERTZ
!       F4TEMP             G5 EVALUATED AT MINIMUM HDSTARP       DB
!       G4                 SCALED SPECTRUM LEVEL                 DB
!       G5                 SPECTRUM SHAPE FUNCTION               DB
!       G50                G5 EVALUATED AT PSI=0.0               DB
!       G514               G5 EVALUATED AT PSI=14.0              DB
!       H                  TRAILING EDGE BLUNTNESS              METERS
!       HDSTAR             BLUNTNESS OVER AVERAGE DISPLACEMENT
!                            THICKNESS                           ---
!       HDSTARL            MINIMUM ALLOWED VALUE OF HDSTAR       ---
!       HDSTARP            MODIFIED VALUE OF HDSTAR              ---
!       ITRIP              TRIGGER FOR BOUNDARY LAYER TRIPPING    ---
!       L                  SPAN                                  METERS
!       M                  MACH NUMBER                           ---
!       NFREQ              NUMBER OF CENTERED FREQUENCIES        ---
!       PHI                DIRECTIVITY ANGLE                    DEGREES
!       PSI                TRAILING EDGE ANGLE                  DEGREES
!       R                  SOURCE TO OBSERVER DISTANCE           METERS
!       RC                 REYNOLDS NUMBER BASED ON CHORD        ---
!       SCALE              SCALING FACTOR                        ---
!       SPLBLNT            SOUND PRESSURE LEVELS DUE TO
!                            BLUNTNESS                            DB
!       STPEAK             PEAK STROUHAL NUMBER                  ---
!       STPPP              STROUHAL NUMBER                       ---
!       THETA              DIRECTIVITY ANGLE                     ---
!       U                  FREESTREAM VELOCITY                 METERS/SEC
!       KinViscosity       KINEMATIC VISCOSITY                 M2/SEC

      REAL(ReKi)  :: L
      REAL(ReKi)  :: M
      REAL(ReKi)  :: SPLBLNT  (NFrequency)
      REAL(ReKi)  :: STPPP    (NFrequency)

!      COMPUTE NECESSARY QUANTITIES
!      ----------------------------

      M  = U /C0
      RC = U  * C / KinViscosity


!      COMPUTE BOUNDARY LAYER THICKNESSES
!      ----------------------------------

      CALL THICK(C,U ,ALPSTAR,DELTAP,DSTRS,DSTRP)

!      COMPUTE AVERAGE DISPLACEMENT THICKNESS
!      --------------------------------------

      DSTRAVG = (DSTRS + DSTRP) / 2.
      HDSTAR  = H / DSTRAVG

      DSTARH = 1. /HDSTAR

!      COMPUTE DIRECTIVITY FUNCTION
!      ----------------------------

      CALL DIRECTH(M,THETA,PHI,DBARH)
      IF (DBARH <= 0) THEN
          SPLBLNT = 0.
          RETURN
      ENDIF


!      COMPUTE PEAK STROUHAL NUMBER
!      ----------------------------

      ATERM  = .212 - .0045 * PSI

      IF (HDSTAR .GE. .2) &
        STPEAK    = ATERM / (1.+.235*DSTARH-.0132*DSTARH**2.)
      IF (HDSTAR .LT. .2) &
        STPEAK    = .1 * HDSTAR + .095 - .00243 * PSI

!      COMPUTE SCALED SPECTRUM LEVEL
!      -----------------------------

      IF (HDSTAR .LE. 5.) G4=17.5*LOG10(HDSTAR)+157.5-1.114*PSI
      IF (HDSTAR .GT. 5.) G4=169.7 - 1.114 * PSI


!      FOR EACH FREQUENCY, COMPUTE SPECTRUM SHAPE REFERENCED TO 0 DB
!      -------------------------------------------------------------

      DO 1000 I=1,NFrequency

        STPPP(I) = FrequencyCenter(I) * H / U
        ETA      = LOG10(STPPP(I)/STPEAK)

        HDSTARL = HDSTAR

        CALL G5COMP(HDSTARL,ETA,G514)

        HDSTARP = 6.724 * HDSTAR **2.-4.019*HDSTAR+1.107

        CALL G5COMP(HDSTARP,ETA,G50)


        G5 = G50 + .0714 * PSI * (G514-G50)
        IF (G5 .GT. 0.) G5 = 0.
        CALL G5COMP(.25,ETA,F4TEMP)
        IF (G5 .GT. F4TEMP) G5 = F4TEMP


        SCALE = 10. * LOG10(M**5.5*H*DBARH*L/R**2.)

        SPLBLNT(I) = G4 + G5 + SCALE


 1000 CONTINUE

      RETURN

      END SUBROUTINE Blunt
!====================================================================================================
      SUBROUTINE G5COMP(HDSTAR,ETA,G5)


      REAL(ReKi)  :: K
      REAL(ReKi)  :: M
      REAL(ReKi)  :: MU


      IF (HDSTAR .LT. .25) MU = .1211
      IF ((HDSTAR .GT. .25).AND.(HDSTAR .LE. .62)) &
          MU=-.2175*HDSTAR + .1755
      IF ((HDSTAR .GT. .62).AND.(HDSTAR .LT. 1.15)) &
       MU = -.0308 * HDSTAR + .0596
      IF (HDSTAR .GE. 1.15)MU = .0242

      IF (HDSTAR .LE. .02) M = 0.0
      IF ((HDSTAR .GE. .02).AND.(HDSTAR .LT. .5)) &
          M=68.724*HDSTAR - 1.35
      IF ((HDSTAR .GT. .5).AND.(HDSTAR .LE. .62)) &
        M = 308.475 * HDSTAR - 121.23
      IF ((HDSTAR .GT. .62).AND.(HDSTAR .LE. 1.15)) &
        M = 224.811 * HDSTAR - 69.354
      IF ((HDSTAR .GT. 1.15) .AND. (HDSTAR .LT. 1.2)) &
        M = 1583.28 * HDSTAR - 1631.592
      IF (HDSTAR .GT. 1.2) M = 268.344
      IF (M .LT. 0.0) M = 0.0

      ETA0 = -SQRT((M*M*MU**4)/(6.25+M*M*MU*MU))

      K    = 2.5*SQRT(1.-(ETA0/MU)**2.)-2.5-M*ETA0

      ETALIMIT = 0.03615995

      IF (ETA .LE. ETA0) G5 = M * ETA + K
      IF ((ETA .GT. ETA0).AND.(ETA .LE. 0.))G5=2.5*SQRT(1.-(ETA/MU)**2.)-2.5
      IF((ETA.GT.0.).AND.(ETA.LE.ETALIMIT))G5=SQRT(1.5625-1194.99*ETA**2.)-1.25
      IF (ETA .GT. ETALIMIT) G5=-155.543 * ETA + 4.375

      RETURN

      END SUBROUTINE G5Comp
!====================================================================================================

      SUBROUTINE TIPNOIS(ALPHTIP,ALPRAT2,C,U ,SPLTIP,THETA,PHI, R)

!                  --------------------------------
!                  ***** VARIABLE DEFINITIONS *****
!                  --------------------------------
!
!       VARIABLE NAME               DEFINITION                  UNITS
!       -------------               ----------                  -----
!
!       ALPHTIP            TIP ANGLE OF ATTACK                DEGREES
!       ALPRAT             TIP LIFT CURVE SLOPE                 ---
!       ALPTIPP            CORRECTED TIP ANGLE OF ATTACK      DEGREES
!       C                  CHORD LENGTH                         METERS
!       C0                 SPEED OF SOUND                    METERS/SEC
!       DBARH              DIRECTIVITY                         ---
!       FRCEN              CENTERED FREQUENCIES              HERTZ
!       L                  CHARACTERISTIC LENGTH FOR TIP      METERS
!       M                  MACH NUMBER                         ---
!       MM                 MAXIMUM MACH NUMBER                 ---
!       NFREQ              NUMBER OF CENTERED FREQUENCIES      ---
!       PHI                DIRECTIVITY ANGLE                  DEGREES
!       R                  SOURCE TO OBSERVER DISTANCE        METERS
!       ROUND              LOGICAL SET TRUE IF TIP IS ROUNDED  ---
!       SCALE              SCALING TERM                        ---
!       SPLTIP             SOUND PRESSURE LEVEL DUE TO TIP
!                            MECHANISM                         DB
!       STPP               STROUHAL NUMBER                     ---
!       TERM               SCALING TERM                        ---
!       THETA              DIRECTIVITY ANGLE                  DEGREES
!       U                  FREESTREAM VELOCITY               METERS/SEC
!       UM                 MAXIMUM VELOCITY                  METERS/SEC
!       KinViscosity       KINEMATIC VISCOSITY               M2/SEC

      REAL(ReKi)  :: L
      REAL(ReKi)  :: M
      REAL(ReKi)  :: MM
      REAL(ReKi)  :: SPLTIP   (NFrequency)

      IF (alphtip.eq.0.) THEN
         SPLTIP= 0
         RETURN
      ELSEIF (alphtip.lt.0.) THEN
         alphtip = ABS (alphtip)
      ENDIF


      ALPTIPP = ALPHTIP * ALPRAT2
      M       = U  / C0

      CALL DIRECTH(M,THETA,PHI,DBARH)

      IF (ROUND) THEN
        L = .008 * ALPTIPP * C
      ELSE
        IF (ABS(ALPTIPP) .LE. 2.) THEN
          L = (.023 + .0169*ALPTIPP) * C
        ELSE
          L = (.0378 + .0095*ALPTIPP) * C
        ENDIF
      ENDIF


      MM     = (1. + .036*ALPTIPP) * M

      UM     = MM * C0

      TERM  = M*M*MM**3.*L**2.*DBARH/R**2.
      IF (TERM .NE. 0.0) THEN
        SCALE = 10.*LOG10(TERM)
      ELSE
        SCALE = 0.0
      ENDIF

      DO 100 I=1,NFrequency
        STPP      = FrequencyCenter(I) * L / UM
        SPLTIP(I) = 126.-30.5*(LOG10(STPP)+.3)**2. + SCALE
  100 CONTINUE
      RETURN

      END SUBROUTINE TipNois
!====================================================================================================
      SUBROUTINE THICK(C,U ,ALPSTAR,DELTAP,DSTRS,DSTRP)
!                  --------------------------------
!                  ***** VARIABLE DEFINITIONS *****
!                  --------------------------------
!
!       VARIABLE NAME               DEFINITION                  UNITS
!       -------------               ----------                  -----
!
!       ALPSTAR            ANGLE OF ATTACK                    DEGREES
!       C                  CHORD LENGTH                        METERS
!       C0                 SPEED OF SOUND                    METERS/SEC
!       DELTA0             BOUNDARY LAYER THICKNESS AT
!                            ZERO ANGLE OF ATTACK              METERS
!       DELTAP             PRESSURE SIDE BOUNDARY LAYER
!                            THICKNESS                         METERS
!       DSTR0              DISPLACEMENT THICKNESS AT ZERO
!                            ANGLE OF ATTACK                   METERS
!       DSTRP              PRESSURE SIDE DISPLACEMENT
!                            THICKNESS                         METERS
!       DSTRS              SUCTION SIDE DISPLACEMENT
!                            THICKNESS                         METERS
!       ITRIP              TRIGGER FOR BOUNDARY LAYER TRIPPING  ---
!       M                  MACH NUMBER                          ---
!       RC                 REYNOLDS NUMBER BASED ON CHORD       ---
!       U                  FREESTREAM VELOCITY                METERS/SEC
!       KinViscosity       KINEMATIC VISCOSITY                M2/SEC
!
!
!      COMPUTE ZERO ANGLE OF ATTACK BOUNDARY LAYER
!      THICKNESS (METERS) AND REYNOLDS NUMBER
!      -------------------------------------------

      M        = U  / C0

      RC       = U  * C/KinViscosity

      DELTA0   = 10.**(1.6569-.9045*LOG10(RC)+ &
                .0596*LOG10(RC)**2.)*C
      IF (ITRIP .EQ. 2) DELTA0 = .6 * DELTA0


!      COMPUTE PRESSURE SIDE BOUNDARY LAYER THICKNESS
!      ----------------------------------------------

      DELTAP   = 10.**(-.04175*ALPSTAR+.00106*ALPSTAR**2.)*DELTA0


!      COMPUTE ZERO ANGLE OF ATTACK DISPLACEMENT THICKNESS
!      ---------------------------------------------------

      IF ((ITRIP .EQ. 1) .OR. (ITRIP .EQ. 2)) THEN
        IF (RC .LE. .3E+06) DSTR0 = .0601 * RC **(-.114)*C
        IF (RC .GT. .3E+06) &
         DSTR0=10.**(3.411-1.5397*LOG10(RC)+.1059*LOG10(RC)**2.)*C
        IF (ITRIP .EQ. 2) DSTR0 = DSTR0 * .6
      ELSE
        DSTR0=10.**(3.0187-1.5397*LOG10(RC)+.1059*LOG10(RC)**2.)*C
      ENDIF

!      PRESSURE SIDE DISPLACEMENT THICKNESS
!      ------------------------------------

      DSTRP   = 10.**(-.0432*ALPSTAR+.00113*ALPSTAR**2.)*DSTR0
      IF (ITRIP .EQ. 3) DSTRP = DSTRP * 1.48

!      SUCTION SIDE DISPLACEMENT THICKNESS
!      -----------------------------------

      IF (ITRIP .EQ. 1) THEN
        IF (ALPSTAR .LE. 5.) DSTRS=10.**(.0679*ALPSTAR)*DSTR0
        IF((ALPSTAR .GT. 5.).AND.(ALPSTAR .LE. 12.5)) &
         DSTRS = .381*10.**(.1516*ALPSTAR)*DSTR0
        IF (ALPSTAR .GT. 12.5)DSTRS=14.296*10.**(.0258*ALPSTAR)*DSTR0
      ELSE
        IF (ALPSTAR .LE. 7.5)DSTRS =10.**(.0679*ALPSTAR)*DSTR0
        IF((ALPSTAR .GT. 7.5).AND.(ALPSTAR .LE. 12.5)) &
         DSTRS = .0162*10.**(.3066*ALPSTAR)*DSTR0
        IF (ALPSTAR .GT. 12.5) DSTRS = 52.42*10.**(.0258*ALPSTAR)*DSTR0
      ENDIF

      RETURN

      END SUBROUTINE Thick
!====================================================================================================
SUBROUTINE WrNoiseOutHdr


   ! This routine generates the header for the noise output files.


USE                             Blades
USE                             General
USE                             Output
USE                             TurbConf

    ! AeroDyn modules
USE                             AeroDyn

IMPLICIT                        NONE


   ! Local variables.

INTEGER(4)                   :: I                                               ! A generic index for DO loops.
INTEGER(4)                   :: J                                               ! A generic index for DO loops.

CHARACTER(200)               :: Frmt                                            ! Format for outputting headings.




   ! Open the output files.

CALL OpenFOutFile ( UnNoSpec, TRIM( RootName )//'.nos' )
CALL OpenFOutFile ( UnNoSPL, TRIM( RootName )//'.spl' )


! Debug
!   CALL OpenFOutFile ( 199, TRIM( RootName )//'.spa' )
!   CALL OpenFOutFile ( 299, TRIM( RootName )//'.cho' )


   ! Add some file information.
WRITE (UnNoSPL,'(/,A)')  'These predictions were generated by '//TRIM(ProgName)//' '//TRIM( ProgVer )// &
                         ' on '//CurDate()//' at '//CurTime()//'.'
WRITE (UnNoSPL,'(  A)')  'The aerodynamic calculations were made by '//TRIM(GetNVD(AD_Prog))//'.'
WRITE (UnNoSPL,'(/,1X,A,/)')  TRIM( FTitle )

SELECT CASE (NoiseOutSwitch)
CASE (2)
   WRITE(UnNoSPL,*) 'Overall Sound Pressure Level from Laminar Boundary Layer Noise(dB)'
CASE (3)
   WRITE(UnNoSPL,*) 'Overall Sound Pressure Level from Total Turbulent Boundary Layer Noise(dB)'
CASE (4)
   WRITE(UnNoSPL,*) 'Overall Sound Pressure Level from Turbulent Inflow Noise(dB)'
CASE (5)
   WRITE(UnNoSPL,*) 'Overall Sound Pressure Level from Tip Noise(dB)'
CASE (6)
   WRITE(UnNoSPL,*) 'Overall Sound Pressure Level from Blunt Trailing Edge Noise(dB)'
CASE (7)
   WRITE(UnNoSPL,*) 'Overall Sound Pressure Level from Turb. Bound. Layer Noise - Pressure Side(dB)'
CASE (8)
   WRITE(UnNoSPL,*) 'Overall Sound Pressure Level from Turb. Bound. Layer Noise - Suction Side(dB)'
CASE (9)
   WRITE(UnNoSPL,*) 'Overall Sound Pressure Level from Separated Turbulent Boundary Layer Noise(dB)'
CASE DEFAULT
   WRITE(UnNoSPL,*) 'Total Overall Sound Pressure Level (dB)'
END SELECT

WRITE(UnNoSPL,*)

IF ( TabDelim )  THEN
   Frmt = '(A,A,A,200(:, A,A,I1,A,I2))'
   WRITE(UnNoSPL,Frmt)  '    Time',  TAB, '  Azimuth',((TAB, 'Bl', J,'-Elem', I, I = 1,BldNodes), J = 1,NumBl)
ELSE
   Frmt = '(A,A,200(:, A,A,I1,A,I2))'
   WRITE(UnNoSPL,Frmt)  '    Time', '  Azimuth',(( 'Bl', J,'-Elem', I, I = 1,BldNodes), J = 1,NumBl)
ENDIF

WRITE (UnNoSpec,'(/,A)')  'These predictions were generated by '//TRIM(ProgName)//' '//TRIM( ProgVer )// &
                          ' on '//CurDate()//' at '//CurTime()//'.'
WRITE (UnNoSpec,'(  A)')  'The aerodynamic calculations were made by '//TRIM(GetNVD(AD_Prog))//'.'
WRITE (UnNoSpec,'(/,1X,A,/)')  TRIM( FTitle )


      WRITE(UnNoSpec,  '(/I7,5X,3X,A)') BldNodes     , ' NUMBER OF SEGMENTS                         ---     '
      WRITE(UnNoSpec,'(ES12.5e2,3X,A)') C0           , ' SPEED OF SOUND                          METERS/SEC '
      WRITE(UnNoSpec,'(ES12.5e2,3X,A)') KinViscosity , ' KINEMATIC VISCOSITY                       M2/SEC   '
      WRITE(UnNoSpec,'(ES12.5e2,3X,A)') ALPRAT       , ' TIP LIFT CURVE SLOPE                       ---     '
      WRITE(UnNoSpec,  '( L7,5X,3X,A)') ROUND        , ' LOGICAL INDICATING ROUNDED TIP             ---     '

      WRITE(UnNoSpec,  '(/L7,5X,3X,A)') IBLUNT       , ' FLAG TO COMPUTE BLUNTNESS NOISE            ---     '
      WRITE(UnNoSpec,  '( L7,5X,3X,A)') ILAM         , ' FLAG TO COMPUTE LBL NOISE                  ---     '
      WRITE(UnNoSpec,  '( L7,5X,3X,A)') ITIP         , ' FLAG TO COMPUTE TIP NOISE                  ---     '
      WRITE(UnNoSpec,  '( I7,5X,3X,A)') ITRIP        , ' FLAG TO TRIP BOUNDARY LAYER                ---     '
      WRITE(UnNoSpec,  '( L7,5X,3X,A)') ITURB        , ' FLAG TO COMPUTE TBLTE NOISE                ---     '
      WRITE(UnNoSpec,  '( L7,5X,3X,A)') IInflow      , ' FLAG TO COMPUTE Turbulent Inflow NOISE     ---     '

      WRITE(UnNoSpec,  '(/1X,A,F11.5,A)') 'Mean Wind Speed =      ', MeanVNoise,' m/s'
      WRITE(UnNoSpec,  '( 1X,A,F11.5,A)') 'Turbulence Intensity = ', TINoise,   ' %'
      WRITE(UnNoSpec,  '(/3(F10.4,1X),A/)') RObserve(1), RObserve(2), RObserve(3), & 
                       ' (x,y,z) Observer location relative to tower base centerline'

      WRITE (UnNoSpec,"(A10,4(1X,A9))") " Segment# ", "    C    ", "    L    ", "    H    ", "   PSI   "
      WRITE (UnNoSpec,"(A10,4(1X,A9))") "----------", "---------", "---------", "---------", "---------"
      DO I = 1, BldNodes
         WRITE(UnNoSpec,"(I10,4(1X,F9.3))") I, Chord(I), DRNodes(I), TEThick(I), TEAngle(I)
      ENDDO
      WRITE(UnNoSpec,*)

Frmt ='(52X,A,/,50X,A,////,5X,4(A)/,(5X,9(A)/))'
WRITE (UnNoSpec, Frmt)'ONE-THIRD OCTAVE','SOUND PRESSURE LEVELS','               ','   PRESSURE    ',   &
                     '    SUCTION    ','  SEPARATION   ',' FREQUENCY(HZ) ','   SIDE TBL    ',  &
                     '   SIDE TBL    ','   SIDE TBL    ',  &
                     '    LAMINAR    ','  BLUNTNESS    ',  &
                     '      TIP      ','    INFLOW     ',  &
                     '    TOTAL      ',  &
                     ('-------------- ', I = 1,9)

RETURN
END SUBROUTINE WrNoiseOutHdr
!====================================================================================================
SUBROUTINE NoiseInput(UnIn, NoiseFile)

   ! This routine reads the noise input files.

USE                             Blades

IMPLICIT                        NONE


   INTEGER, INTENT(IN)       :: UnIn
   CHARACTER(*),INTENT(IN)   :: NoiseFile


   ! Local variables.

INTEGER(4)                   :: I                                               ! A generic index for DO loops.
INTEGER(4)                   :: Sttus                                           ! Status returned by an attempted allocation.

   ! Allocates some of the input arrays

ALLOCATE ( TEThick(BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TEThick array.' )
ENDIF

ALLOCATE ( TEAngle(BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TEAngle array.' )
ENDIF

   ! Open and read input file

CALL OpenFInpFile (UnIn,NoiseFile)
      READ(UnIn,*)
      READ(UnIn,*)
      READ(UnIn,*) C0
      READ(UnIn,*) ALPRAT
      READ(UnIn,*) ROUND
      READ(UnIn,*)
      READ(UnIn,*) IBLUNT
      READ(UnIn,*) ILAM
      READ(UnIn,*) ITIP
      READ(UnIn,*) ITRIP
      READ(UnIn,*) ITURB
      READ(UnIn,*) IInflow
      READ(UnIn,*)
      READ(UnIn,*) NoiseOutSwitch
      READ(UnIn,*)
      READ(UnIn,*) RObserve(1), RObserve(2), RObserve(3)
      READ(UnIn,*)
      READ(UnIn,*)
      DO I = 1, BldNodes
         READ(UnIn,*)TEThick(I),TEAngle(I)
      ENDDO

CLOSE (UnIn)
RETURN
END SUBROUTINE NoiseInput
!====================================================================================================
SUBROUTINE WriteSPLOut


USE                             Blades
USE                             DOFs
USE                             General
USE                             Output
USE                             RtHndSid
USE                             SimCont
USE                             TurbConf

IMPLICIT                        NONE


   ! Local variables.

REAL(ReKi)                   :: AzimuthOut                                      ! Azimuth angle
REAL(ReKi)                   :: OASPLout (NumBl,BldNodes)                       ! Overall sound pressure level to output

INTEGER(4)                   :: I                                               ! A generic index for DO loops.
INTEGER(4)                   :: J                                               ! A generic index for DO loops.

CHARACTER(200)               :: Frmt                                            ! Format for outputting headings.


SELECT CASE (NoiseOutSwitch)
CASE (2)
   OASPLout = OASPLLBL
CASE (3)
   OASPLout = OASPLTBLAll
CASE (4)
   OASPLout = OASPLInflow
CASE (5)
   OASPLout = OASPLTip
CASE (6)
   OASPLout = OASPLBlunt
CASE (7)
   OASPLout = OASPLTBLP
CASE (8)
   OASPLout = OASPLTBLS
CASE (9)
   OASPLout = OASPLSep
CASE DEFAULT
   OASPLout = OASPL
END SELECT

AzimuthOut =MOD( ( QT(DOF_GeAz) + QT(DOF_DrTr) )*R2D + AzimB1Up + 90.0, 360.0 )

IF ( TabDelim )  THEN
   Frmt = '(F8.3,A,F8.3,200(:,A ,F8.3))'
   WRITE(UnNoSPL,Frmt)  ZTime, TAB, AzimuthOut,(( TAB, OASPLout(J,I), I = 1,BldNodes), J = 1, NumBl)
ELSE
   Frmt = '(F8.3,F8.3,200(:,1X,F8.3))'
   WRITE(UnNoSPL,Frmt)  ZTime, AzimuthOut,((  OASPLout(J,I), I = 1,BldNodes), J = 1, NumBl)
ENDIF

! Debug
!WRITE(199,Frmt)  ZTime, TAB, AzimuthOut,(( TAB, SpanAngleTE(J,I), I = 1,BldNodes), J = 1, NumBl)
!WRITE(299,Frmt)  ZTime, TAB, AzimuthOut,(( TAB, ChordAngleTE(J,I), I = 1,BldNodes), J = 1, NumBl)

RETURN
END SUBROUTINE WriteSPLOut

!====================================================================================================
SUBROUTINE CalcObserve


USE                             Blades
USE                             CoordSys
USE                             RtHndSid
USE                             SimCont
USE                             TurbConf


IMPLICIT                        NONE


   ! Local variables.

REAL(DbKi)                   :: RLEObserve (3)                                  ! position vector from leading edge to observer in trailing edge coordinate system
REAL(DbKi)                   :: RTEObserve (3)                                  ! position vector from trailing edge to observer in trailing edge coordinate system

REAL(ReKi)                   :: RObserveInt(3)                                  ! RObserve in the internal coordinate system
REAL(ReKi)                   :: rSLE       (3)                                  ! Distance from tower base to leading edge in trailing edge coordinate system
REAL(ReKi)                   :: rSTE       (3)                                  ! Distance from tower base to trailing edge in trailing edge coordinate system
REAL(ReKi)                   :: timeLE                                          ! Time of sound propagation from leading edge to observer
REAL(ReKi)                   :: timeTE                                          ! Time of sound propagation from trailing edge to observer
REAL(ReKi)                   :: tmpR       (3)                                  ! temporary distance vector
REAL(ReKi)                   :: UConvect   (3)                                  ! convection velocity of noise source in trailing edge coordinate system

INTEGER(4)                   :: I                                               ! A generic index for DO loops.
INTEGER(4)                   :: J                                               ! A generic index for DO loops.


! Transform RObserve to the internal a coordinate system
RObserveInt (1) = RObserve (1)
RObserveInt (2) = RObserve (3) + PtfmRef
RObserveInt (3) =-RObserve (2)

   DO J = 1, NumBl
      DO I = 1, BldNodes

          ! Calculate position vector of trailing edge from tower base in trailing edge coordinate system
          rSTE (1) = DOT_PRODUCT(te1(J,I,:),rS(J,I,:))
          rSTE (2) = DOT_PRODUCT(te2(J,I,:),rS(J,I,:)) + 0.75*Chord(I)
          rSTE (3) = DOT_PRODUCT(te3(J,I,:),rS(J,I,:))

          ! Calculate position vector of leading edge from tower base in trailing edge coordinate system
          rSLE (1) = rSTE (1)
          rSLE (2) = rSTE (2) - Chord(I)
          rSLE (3) = rSTE (3)

          ! Calculate position vector of observer from tower base in trailing edge coordinate system
          tmpR (1) = DOT_PRODUCT(te1(J,I,:),RObserveInt)
          tmpR (2) = DOT_PRODUCT(te2(J,I,:),RObserveInt)
          tmpR (3) = DOT_PRODUCT(te3(J,I,:),RObserveInt)

          ! Calculate position vector from leading and trailing edge to observer in trailing edge coordinate system
          RTEObserve = tmpR-rSTE
          RLEObserve = tmpR-rSLE

          ! Calculate convection velocity of noise source
          ! Assumes noise source convects at some constant times the mean wind speed, approximately accounts for
          ! induction velocity and change in convection velocity as noise propagates to observer (likely on the ground)
          UConvect (1) = te1(J,I,1)*0.8*MeanVNoise
          UConvect (2) = te2(J,I,1)*0.8*MeanVNoise
          UConvect (3) = te3(J,I,1)*0.8*MeanVNoise

          ! Calculate time of noise propagation to observer
          timeTE = SQRT (RTEObserve(1)**2+RTEObserve(2)**2+RTEObserve(3)**2)/C0
          timeLE = SQRT (RLEObserve(1)**2+RLEObserve(2)**2+RLEObserve(3)**2)/C0

          ! Calculate position vector from leading and trailing edge to observer in retarded trailing edge coordinate system
          RTEObserve = RTEObserve-UConvect*timeTE
          RLEObserve = RTEObserve-UConvect*timeLE

          ! Calculate inputs into noise subroutines
          rTEtoObserve(J,I) = SQRT (RTEObserve(1)**2+RTEObserve(2)**2+RTEObserve(3)**2)
          rLEtoObserve(J,I) = SQRT (RLEObserve(1)**2+RLEObserve(2)**2+RLEObserve(3)**2)

          ChordAngleTE(J,I) = ACOS (RTEObserve(2)/SQRT(RTEObserve(1)**2+RTEObserve(2)**2+RTEObserve(3)**2))*R2D
          SpanAngleTE(J,I) = ACOS (RTEObserve(3)/SQRT(RTEObserve(1)**2+RTEObserve(3)**2))*R2D
          IF (SpanAngleTE(J,I)< 0) SpanAngleTE(J,I)= 180+SpanAngleTE(J,I)
          IF (ChordAngleTE(J,I)< 0) ChordAngleTE(J,I)= 180+ChordAngleTE(J,I)

          ChordAngleLE(J,I) = ACOS (RLEObserve(2)/SQRT(RLEObserve(1)**2+RLEObserve(2)**2+RLEObserve(3)**2))*R2D
          SpanAngleLE(J,I) = ACOS (RLEObserve(3)/SQRT(RLEObserve(1)**2+RLEObserve(3)**2))*R2D
          IF (SpanAngleLE(J,I)< 0) SpanAngleLE(J,I)= 180+SpanAngleLE(J,I)
          IF (ChordAngleLE(J,I)< 0) ChordAngleLE(J,I)= 180+ChordAngleLE(J,I)

      ENDDO !I
   ENDDO  !J
RETURN
END SUBROUTINE CalcObserve

!====================================================================================================
SUBROUTINE WriteAveSpecOut

USE           General

IMPLICIT      NONE

INTEGER(4) :: I       ! A generic index for DO loops.
INTEGER(4) :: J       ! A generic index for DO loops.

WHERE (AvePressure > 0.)
   AveSPL = 10.*LOG10(AvePressure/NAverage)
ELSEWHERE
   AveSPL = 0.
END WHERE

!      WRITE OUTPUT FILE
!      -----------------


DO I=1,NFrequency
        WRITE(UnNoSpec,7100) FrequencyCenter(I),(AveSPL(J,I),J=1,3),(AveSPL(J,I),J=5,8),AveSPL(4,I)
        !IF (MOD(I,5) .EQ. 0) WRITE(UnNoSpec,7200)
ENDDO

 7100 FORMAT(9F15.3)
 7200 FORMAT(' ')

RETURN
END SUBROUTINE WriteAveSpecOut

!====================================================================================================
SUBROUTINE InflowNoise(U,Chord,d,RObs,THETA,PHI,SPLti)


USE                             EnvCond
USE                             TurbConf

IMPLICIT                        NONE


REAL(ReKi)                   :: Beta2                                           ! Prandtl-Glauert correction factor
REAL(ReKi)                   :: Chord                                           ! chord length
REAL(ReKi)                   :: d                                               ! element span
REAL(ReKi)                   :: DBARH                                           ! High-frequency directivity correction factor
REAL(ReKi)                   :: DBARL                                           ! Low-frequency directivity correction factor
REAL(ReKi)                   :: Directivity                                     ! Directivity correction factor
REAL(ReKi)                   :: Frequency_cutoff                                ! Cutoff frequency between
REAL(ReKi)                   :: LFC                                             ! low-frequency correction factor
REAL(ReKi)                   :: LTurb                                           ! turbulence length scale (isotropic integral scale parameter from IEC standard (Von Karman))
REAL(ReKi)                   :: Mach                                            ! local mach number
REAL(ReKi)                   :: PHI                                             ! Spanwise directivity angle
REAL(ReKi)                   :: RObs                                            ! distance to observer
REAL(ReKi)                   :: Sears                                           ! Sears function
REAL(ReKi)                   :: SPLhigh                                         ! predicted high frequency sound pressure level
REAL(ReKi)                   :: SPLti    (NFrequency)                           ! total predicted turbulence inflow noise
REAL(ReKi)                   :: THETA                                           ! Chordwise directivity angle
REAL(ReKi)                   :: U                                               ! local total velocity
REAL(ReKi)                   :: Ums                                             ! mean square turbulence level
REAL(ReKi)                   :: WaveNumber                                      ! wave number - non-dimensional frequency

INTEGER(4)                   :: I                                               ! A generic index for DO loops.


Mach = U/C0
IF (TINoise > 0) THEN
    Ums = (TINoise*MeanVNoise/100.)**2
ELSE
    SPLti = 0.
    RETURN
ENDIF

IF (FASTHH < 30.0) THEN
    LTurb = 3.5*0.7*FASTHH ! Prediction sensitive to this parameter!
ELSE
    LTurb = 3.5*21.
ENDIF
!LTurb = LTurb/100

! Calculate directivity...?
CALL DIRECTL(Mach,THETA,PHI,DBARL)  !yes, assume that noise is low-freq in nature because turbulence length scale is large
CALL DIRECTH(Mach,THETA,PHI,DBARH)
Frequency_cutoff = 10*U/PI/Chord

IF (DBARL <= 0.) THEN
    SPLti = 0.
    RETURN
ENDIF

DO I=1,NFrequency
   IF (FrequencyCenter(I) <= Frequency_cutoff) THEN
       Directivity = DBARL
   ELSE
       Directivity = DBARH
   ENDIF
   WaveNumber = PI*FrequencyCenter(I)*Chord/U
   Beta2 = 1-Mach*Mach
   SPLhigh = 10.*LOG10(AirDens*AirDens*C0*C0*LTurb*(d/2.)/(RObs*RObs)*(Mach**3)*Ums* &
             (WaveNumber**3)*(1+WaveNumber**2)**(-7./3.)*Directivity) + 58.4
   Sears = 1/(2*PI*WaveNumber/Beta2+1/(1+2.4*WaveNumber/Beta2))
   LFC = 10*Sears*Mach*WaveNumber*WaveNumber/Beta2
   SPLti(I) = SPLhigh + 10.*LOG10(LFC/(1+LFC))
ENDDO

RETURN
END SUBROUTINE InflowNoise
!====================================================================================================
SUBROUTINE Noise_CalcTI (Time_Start, Time_End, delta_time, InputPosition)

   USE                           InflowWind        ! bjj: there may be a better way to do this (use AeroDyn in the module?)!

   IMPLICIT NONE

      ! passed variables

   REAL(ReKi), INTENT(IN)     :: Time_Start
   REAL(ReKi), INTENT(IN)     :: Time_End
   REAL(ReKi), INTENT(IN)     :: delta_time
   REAL(ReKi), INTENT(IN)     :: InputPosition(3)

      ! local variables

   REAL(ReKi)                 :: Velocity(3)
   INTEGER                    :: ErrStat

      ! Calculate velocity

   Velocity(:) = WindInf_GetMean( Time_Start, Time_End, delta_time, InputPosition,  ErrStat )
   MeanVNoise  = Velocity(1)
   IF ( ErrStat /=0 ) CALL ProgAbort(' Error getting wind speed in Noise_CalcTI().')

      ! Calculate turbulence intensity

   Velocity(:) = WindInf_GetTI( Time_Start, Time_End, delta_time, InputPosition,  ErrStat )
   TINoise     = 100.0*Velocity(1)
   IF ( ErrStat /=0 ) CALL ProgAbort(' Error getting turbulence intensity in Noise_CalcTI().')

   RETURN
END SUBROUTINE Noise_CalcTI
!====================================================================================================
SUBROUTINE AllocNoise

USE                             TurbConf
USE                             Blades

IMPLICIT                        NONE


INTEGER(4)                   :: Sttus                                           ! Status returned by an attempted allocation.


ALLOCATE ( ChordAngleTE(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the ChordAngleTE array.' )
ENDIF
ChordAngleTE = 0.0

ALLOCATE ( SpanAngleTE(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the SpanAngleTE array.' )
ENDIF
SpanAngleTE = 0.0

ALLOCATE ( ChordAngleLE(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the ChordAngleLE array.' )
ENDIF
ChordAngleLE = 0.0

ALLOCATE ( SpanAngleLE(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the SpanAngleTE array.' )
ENDIF
SpanAngleLE = 0.0

ALLOCATE ( AvePressure(NMech,NFrequency) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the AvePressure array.' )
ENDIF
AvePressure = 0.0

ALLOCATE ( AveSPL(NMech,NFrequency) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the AveSPL array.' )
ENDIF
AveSPL = 0.0

ALLOCATE ( OASPLLBL(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the OASPLLBL array.' )
ENDIF
OASPLLBL = 0.0

ALLOCATE ( OASPLTBLP(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the OASPLTBLP array.' )
ENDIF
OASPLTBLP = 0.0

ALLOCATE ( OASPLTBLS(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the OASPLTBLS array.' )
ENDIF
OASPLTBLS = 0.0

ALLOCATE ( OASPLSep(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the OASPLSep array.' )
ENDIF
OASPLSep = 0.0

ALLOCATE ( OASPLTBLAll(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the OASPLAll array.' )
ENDIF
OASPLTBLAll = 0.0

ALLOCATE ( OASPLBlunt(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the OASPLBlunt array.' )
ENDIF
OASPLBlunt = 0.0

ALLOCATE ( OASPLTip(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the OASPLTip array.' )
ENDIF
OASPLTip = 0.0

ALLOCATE ( OASPLInflow(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the OASPLTip array.' )
ENDIF
OASPLInflow = 0.0

ALLOCATE ( OASPL(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the OASPL array.' )
ENDIF
OASPL = 0.0

ALLOCATE ( rTEtoObserve(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the rTEtoObserve array.' )
ENDIF
rTEtoObserve = 0.0

ALLOCATE ( rLEtoObserve(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the rLEtoObserve array.' )
ENDIF
rLEtoObserve = 0.0



RETURN
END SUBROUTINE AllocNoise
!====================================================================================================
SUBROUTINE Noise_Terminate( )

   IMPLICIT NONE

      ! deallocate arrays
      
   IF ( ALLOCATED( AvePressure  ) ) DEALLOCATE( AvePressure  ) 
   IF ( ALLOCATED( AveSPL       ) ) DEALLOCATE( AveSPL       ) 
   IF ( ALLOCATED( ChordAngleLE ) ) DEALLOCATE( ChordAngleLE ) 
   IF ( ALLOCATED( ChordAngleTE ) ) DEALLOCATE( ChordAngleTE ) 
   IF ( ALLOCATED( OASPL        ) ) DEALLOCATE( OASPL        ) 
   IF ( ALLOCATED( OASPLBlunt   ) ) DEALLOCATE( OASPLBlunt   ) 
   IF ( ALLOCATED( OASPLInflow  ) ) DEALLOCATE( OASPLInflow  ) 
   IF ( ALLOCATED( OASPLLBL     ) ) DEALLOCATE( OASPLLBL     ) 
   IF ( ALLOCATED( OASPLSep     ) ) DEALLOCATE( OASPLSep     ) 
   IF ( ALLOCATED( OASPLTBLAll  ) ) DEALLOCATE( OASPLTBLAll  ) 
   IF ( ALLOCATED( OASPLTBLP    ) ) DEALLOCATE( OASPLTBLP    ) 
   IF ( ALLOCATED( OASPLTBLS    ) ) DEALLOCATE( OASPLTBLS    ) 
   IF ( ALLOCATED( OASPLTip     ) ) DEALLOCATE( OASPLTip     ) 
   IF ( ALLOCATED( rLEtoObserve ) ) DEALLOCATE( rLEtoObserve ) 
   IF ( ALLOCATED( rTEtoObserve ) ) DEALLOCATE( rTEtoObserve ) 
   IF ( ALLOCATED( SpanAngleLE  ) ) DEALLOCATE( SpanAngleLE  ) 
   IF ( ALLOCATED( SpanAngleTE  ) ) DEALLOCATE( SpanAngleTE  ) 
   IF ( ALLOCATED( TEAngle      ) ) DEALLOCATE( TEAngle      ) 
   IF ( ALLOCATED( TEThick      ) ) DEALLOCATE( TEThick      ) 
   
      ! close files
   
END SUBROUTINE Noise_Terminate
!====================================================================================================

END MODULE Noise
