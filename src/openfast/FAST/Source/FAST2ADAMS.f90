!BJJ Start of proposed change vXX NWTC_Lib
MODULE FAST2ADAMSSubs

   USE   NWTC_Library

CONTAINS
!bjj end of proposed change
!=======================================================================
SUBROUTINE MakeACF


   ! This routine generates an ADAMS control file (.acf) for an ADAMS
   !   SIMULATE analysis using the properties specified in the FAST
   !   input files as model parameters.

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Simplify the SFORCE used to generate free surface GRAPHICS in the
!jmj   FAST-to-ADAMS preprocessor.  Also, eliminate the free surface DOFs
!jmj   during a linearization analysis:
USE                             ADAMSInput
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

USE                             Blades
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Simplify the SFORCE used to generate free surface GRAPHICS in the
!jmj   FAST-to-ADAMS preprocessor.  Also, eliminate the free surface DOFs
!jmj   during a linearization analysis:
USE                             EnvCond
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
USE                             Features
USE                             General
USE                             MassInert
USE                             Output
USE                             SimCont
!bjj rm NWTC_Library: USE                             SysSubs
USE                             Tower
USE                             TurbConf
USE                             TurbCont


IMPLICIT                        NONE


   ! Local variables.

INTEGER(4)                   :: K                                               ! Loops through blades.

CHARACTER( 3)                :: FmtText   = '(A)'                               ! Format for outputting pure text.
!bjj rm unused:CHARACTER(10)                :: FmtTR     = '(A,ES13.6)'                        ! Format for outputting text then a real value.
CHARACTER(12)                :: FmtTRT    = '(A,ES13.6,A)'                      ! Format for outputting text then a real value then text again.
CHARACTER(19)                :: FmtTRTR   = '(A,ES13.6,A,ES13.6)'               ! Format for outputting text, a real value, text, and a real value.


   ! Global functions.

!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.



   ! Open the ADAMS control file and give it a heading:

CALL OpenFOutFile ( UnAC, TRIM( RootName )//'_ADAMS.acf' )


   ! Read in the ADAMS datset file (.adm):

WRITE (UnAC,FmtText  )  TRIM( RootName )//'_ADAMS'


   ! Use the same name for the output files:

WRITE (UnAC,FmtText  )  TRIM( RootName )//'_ADAMS'


   ! Specify the INTEGRATOR properties:

WRITE (UnAC,FmtTRT   )  'INTEGRATOR/GSTIFF, ERROR = 0.001, HMAX = ', DT, ', INTERPOLATE = ON'
!JASON: Make ERROR an input!--Do this when you add variable-step size integration in FAST!


   ! Run the simulation for the first time step:

WRITE (UnAC,FmtTRTR  )  'SIMULATE/DYNAMICS, END = ', DT, ', DTOUT = ', DT*DecFact



   ! DEACTIVATE the MOTION statements for the translational platform DOFs if
   !   the corresponding DOFs are enabled:

IF ( PtfmSgDOF )  WRITE (UnAC,FmtText  )  'DEACTIVATE/MOTION, ID = 1001'
IF ( PtfmSwDOF )  WRITE (UnAC,FmtText  )  'DEACTIVATE/MOTION, ID = 1002'
IF ( PtfmHvDOF )  WRITE (UnAC,FmtText  )  'DEACTIVATE/MOTION, ID = 1003'


   ! DEACTIVATE the JPRIM statement for the rotational platform DOFs if the
   !   platform roll DOF is enabled [all of the platform rotational DOFs are
   !   controlled by the the roll DOF since PtfmRDOF, PtfmPDOF, and PtfmYDOF
   !   must all be set to the same value, a requirement enforced in routine
   !   MakeADM()]:

IF ( PtfmRDOF  )  WRITE (UnAC,FmtText  )  'DEACTIVATE/JPRIM, ID = 1000'


   ! DEACTIVATE the MOTION statements for the yaw, rotor-furl, tail-furl, HSS,
   !   and teeter bearings and the drivetrain LSS/HSS lock if the corresponding
   !   DOFs are enabled:

IF ( YawDOF    )  WRITE (UnAC,FmtText  )  'DEACTIVATE/MOTION, ID = 2010'
IF ( TFrlDOF   )  WRITE (UnAC,FmtText  )  'DEACTIVATE/MOTION, ID = 5040'
IF ( RFrlDOF   )  WRITE (UnAC,FmtText  )  'DEACTIVATE/MOTION, ID = 2130'
IF ( TeetDOF   )  WRITE (UnAC,FmtText  )  'DEACTIVATE/MOTION, ID = 4010'
IF ( DrTrDOF   )  WRITE (UnAC,FmtText  )  'DEACTIVATE/MOTION, ID = 3020'
IF ( GenDOF    )  WRITE (UnAC,FmtText  )  'DEACTIVATE/MOTION, ID = 3150'


   ! Turn off fixed pitch MOTION; use advanced, individual blade pitch control
   !   instead:
   ! Only do this if we will be modifying the blade pitch angles during
   !   our run.

DO K = 1,NumBl ! Loop through all blades

   IF ( ( ( PCMode /= 0 ) .AND. ( TPCOn < TMax ) ) .OR. ( TPitManS(K) < TMax ) )  &
      WRITE (UnAC,FmtText  )  'DEACTIVATE/MOTION, ID = '//TRIM(Int2LStr( 10000*K ))

ENDDO          ! K - Blades


   ! DEACTIVATE the FIXED JOINTS in the tower and blade if tower and blade
   !   flexibility are enabled:
   ! It is necessary to lock the flexible elements together at the start
   !   of the simulation since the initial rotor speed kicks the system
   !   to intensly during the initial condition solution.  Locking the
   !   elements together during the initial condition solution eliminates
   !   this problem.

IF ( TwFADOF1 )  THEN   ! Tower flexibility is enabled.

   WRITE    (UnAC,FmtText  )  'DEACTIVATE/JOINT, RANGE = '//TRIM(Int2LStr(           1300 + 1        ))//', '// &
                                                            TRIM(Int2LStr(           1300 + TwrNodes ))
   WRITE    (UnAC,FmtText  )  'DEACTIVATE/JOINT, ID = '//TRIM(   Int2LStr(           1500            ))

ENDIF

IF ( FlapDOF1 )  THEN   ! Blade flexibility is enabled.

   DO K = 1,NumBl       ! Loop through all the blades

      WRITE (   UnAC,FmtText  )  'DEACTIVATE/JOINT, RANGE = '//TRIM(Int2LStr( 10000*K + 3000 + 1        ))//', '// &
                                                               TRIM(Int2LStr( 10000*K + 3000 + BldNodes ))
      IF ( TipMass(K) /= 0.0 )  &
         WRITE (UnAC,FmtText  )  'DEACTIVATE/JOINT, ID = '//TRIM(   Int2LStr( 10000*K + 5000            ))

   ENDDO                ! K - blades

ENDIF

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Simplify the SFORCE used to generate free surface GRAPHICS in the
!jmj   FAST-to-ADAMS preprocessor.  Also, eliminate the free surface DOFs
!jmj   during a linearization analysis:
   ! DEACTIVATE the MOTION statements for the translational DOFs of the free
   !   surface if necessary:

IF ( CompHydro .AND. SaveGrphcs .AND. ( WaveMod /= 4 ) )  THEN ! .TRUE. if we are using the undocumented monopile or platform features .AND. SaveGrphcs is enabled, but not with GH Bladed wave data

   WRITE (UnAC,FmtText  )  'DEACTIVATE/MOTION, RANGE = '//TRIM(Int2LStr( 100900 + 0         ))//', '// &
                                                          TRIM(Int2LStr( 100900 + NFreeSrfc ))

ENDIF


!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
   ! Continue the simulation from where we left off before, but now
   !   with tower and blade flexibility and other DOFs if requested:

WRITE (UnAC,FmtTRTR  )  'SIMULATE/DYNAMICS, END = ', TMax, ', DTOUT = ', DT*DecFact



   ! We're done!

WRITE (UnAC,FmtText  )  'STOP'


   ! Inform the users of this great news!

CALL WrScr (' ADAMS command file '''//TRIM( RootName )//'_ADAMS.acf'' created.')


   ! Close the file.

CLOSE ( UnAC )



RETURN
END SUBROUTINE MakeACF
!=======================================================================
SUBROUTINE MakeACF_LIN


   ! This routine generates an ADAMS control file (.acf) for an ADAMS
   !   LINEAR analysis using the properties specified in the FAST input
   !   files as model parameters.


USE                             Blades
!bjj rm NWTC_Library: USE                             Constants
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Put in some logic to ensure that the hydrodynamic loads are time invariant
!jmj   when linearizing a model:
USE                             EnvCond
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
USE                             Features
USE                             General
USE                             InitCond
USE                             MassInert
USE                             Output
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Put in some logic to ensure that the hydrodynamic loads are time invariant
!jmj   when linearizing a model:
USE                             Platform
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
USE                             SimCont
!bjj rm NWTC_Library: USE                             SysSubs
USE                             TipBrakes
USE                             Tower
USE                             TurbConf


IMPLICIT                        NONE


   ! Local variables.

INTEGER(4)                   :: J                                               ! Loops through nodes / elements.
INTEGER(4)                   :: K                                               ! Loops through blades.


CHARACTER( 3)                :: FmtText   = '(A)'                               ! Format for outputting pure text.
CHARACTER(10)                :: FmtTR     = '(A,ES13.6)'                        ! Format for outputting text then a real value.
CHARACTER(12)                :: FmtTRT    = '(A,ES13.6,A)'                      ! Format for outputting text then a real value then text again.
CHARACTER(19)                :: FmtTRTR   = '(A,ES13.6,A,ES13.6)'               ! Format for outputting text, a real value, text, and a real value.


   ! Global functions.

!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Put in some logic to ensure that the hydrodynamic loads are time invariant
!jmj   when linearizing a model:
   ! The ADAMS LINEAR analysis will fail when the hydrodynamic loads are time
   !    variant.
   ! Make sure FAST aborts if any of the following conditions are met:

IF ( ( WaveMod  /= 0   ) .AND. CompHydro )  &
!jmj Start of proposed change.  v6.10a-jmj  21-Feb-2007.
!jmj Reword Abort message:
!remove6.10a   CALL ProgAbort ( ' An ADAMS control file for a LINEAR analysis can''t linearize be built when using incident wave '// &
!remove6.10a                     'kinematics. Set WaveMod to 0 or MakeLINacf to False.'                                              )
   CALL ProgAbort ( ' An ADAMS control file for a LINEAR analysis can''t be built when using incident wave kinematics.'// &
                    '  Set WaveMod to 0 or MakeLINacf to False.'                                                           )
!jmj End of proposed change.  v6.10a-jmj  21-Feb-2007.


IF ( ( RdtnTMax /= 0.0 ) .AND. CompHydro )  &
!jmj Start of proposed change.  v6.10a-jmj  21-Feb-2007.
!jmj Reword Abort message:
!remove6.10a   CALL ProgAbort ( ' An ADAMS control file for a LINEAR analysis can''t linearize be built when using wave radiation '// &
!remove6.10a                     'damping. Set RdtnTMax to 0.0 or MakeLINacf to False.'                                               )
   CALL ProgAbort ( ' An ADAMS control file for a LINEAR analysis can''t be built when using wave radiation damping.'// &
                    '  Set RdtnTMax to 0.0 or MakeLINacf to False.'                                                       )
!jmj End of proposed change.  v6.10a-jmj  21-Feb-2007.

!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

   ! Open the ADAMS control file and give it a heading:

CALL OpenFOutFile ( UnAL, TRIM( RootName )//'_ADAMS_LIN.acf' )


   ! Read in the ADAMS datset file (.adm):

WRITE (UnAL,FmtText  )  TRIM( RootName )//'_ADAMS'


   ! Use the same name appended with "_LIN" for the output files:

WRITE (UnAL,FmtText  )  TRIM( RootName )//'_ADAMS_LIN'


!jmj Start of proposed change.  v6.10d-jmj  13-Aug-2009.
!jmj Do not disable gravity when linearizing in ADAMS:
!remove6.10d   ! Turn off gravity:
!remove6.10d
!remove6.10dWRITE (UnAL,FmtText  )  'ACCGRAV/KGRAV = 0'
!remove6.10d
!remove6.10d
!jmj End of proposed change.  v6.10d-jmj  13-Aug-2009.


   ! Make sure generator and rotor are not spinning by switching
   !   MOTION/3150 from a VELOCITY statement to a DISPLACEMENT
   !   statement (since a VELOCITY statement will give an error
   !   during the STATICS simulation):

WRITE (UnAL,FmtText  )  'MOTION/3150, VELOCITY, ICDISP = 0'                                                    ! Make sure ICDISP = 0 or else ADAMS/SOLVER will generate an error when switching this MOTION statement from VELOCITY to DISPLACEMENT.
WRITE (UnAL,FmtTR    )  'MOTION/3150, DISPLACEMENT, FUNCTION = ', MOD( Azimuth - AzimB1Up + 180.0, 360.0 )*D2R ! Set the fixid DISPLACEMENT to what was previously the ICDISP value.


   ! Disable AeroDynamics if AeroDyn was called from the dataset by
   !   setting all the aerodynamic forces and moments to zero:

IF ( CompAero )  THEN

   DO K = 1,NumBl       ! Loop through all blades
      DO J = 1,BldNodes ! Loop through the blade nodes/elements

         WRITE (UnAL,FmtText  )  'GFORCE/'//TRIM(Int2LStr( 10000*K + 1000 + 10*J ))// &
                                 ', FX = 0\ FY = 0\ FZ = 0\ TX = 0\ TY = 0\ TZ = 0\'

      ENDDO             ! J - Blade nodes/elements
   ENDDO                ! K - Blades

ENDIF


   ! Similarly, let's remove tip brake drag effects if necessary:

IF ( ( TBDrConN /= 0.0 ) .OR. ( TBDrConD /= 0.0 ) )  THEN   ! Only removed when TBDrConN or TBDrConD is nonzero:

   DO K = 1,NumBl       ! Loop through all blades

      WRITE (UnAL,FmtText  )  'VFORCE/'//TRIM(Int2LStr( 10000*K + 5100 ))//', FX = 0\ FY = 0\ FZ = 0\'

   ENDDO                ! K - Blades

ENDIF


   ! Make sure no outputs are calculated and no "reqsub1.plt" file
   !   is created (indicated by zero as the first input; all other
   !   inputs are don't cares):

WRITE (UnAL,FmtText  )  'REQUEST/1, FUNCTION = USER( 0, 0 )'


   ! Specify the INTEGRATOR properties:

WRITE (UnAL,FmtTRT   )  'INTEGRATOR/GSTIFF, ERROR = 0.001, HMAX = ', DT, ', INTERPOLATE = ON'


   ! Run the simulation for a single time step:

WRITE (UnAL,FmtTRTR  )  'SIMULATE/DYNAMICS, END = ', DT, ', DTOUT = ', DT*DecFact



   ! DEACTIVATE the MOTION statements for the translational platform DOFs if
   !   the corresponding DOFs are enabled:

IF ( PtfmSgDOF )  WRITE (UnAL,FmtText  )  'DEACTIVATE/MOTION, ID = 1001'
IF ( PtfmSwDOF )  WRITE (UnAL,FmtText  )  'DEACTIVATE/MOTION, ID = 1002'
IF ( PtfmHvDOF )  WRITE (UnAL,FmtText  )  'DEACTIVATE/MOTION, ID = 1003'


   ! DEACTIVATE the JPRIM statement for the rotational platform DOFs if the
   !   platform roll DOF is enabled [all of the platform rotational DOFs are
   !   controlled by the the roll DOF since PtfmRDOF, PtfmPDOF, and PtfmYDOF
   !   must all be set to the same value, a requirement enforced in routine
   !   MakeADM()]:

IF ( PtfmRDOF  )  WRITE (UnAL,FmtText  )  'DEACTIVATE/JPRIM, ID = 1000'


   ! DEACTIVATE the MOTION statements for the yaw, rotor-furl, tail-furl, and
   !   teeter bearings and the drivetrain LSS/HSS lock if the corresponding
   !   DOFs are enabled.
   ! Do not DEACTIVATE the generator MOTION statement:

IF ( YawDOF    )  WRITE (UnAL,FmtText  )  'DEACTIVATE/MOTION, ID = 2010'
IF ( TFrlDOF   )  WRITE (UnAL,FmtText  )  'DEACTIVATE/MOTION, ID = 5040'
IF ( RFrlDOF   )  WRITE (UnAL,FmtText  )  'DEACTIVATE/MOTION, ID = 2130'
IF ( TeetDOF   )  WRITE (UnAL,FmtText  )  'DEACTIVATE/MOTION, ID = 4010'
IF ( DrTrDOF   )  WRITE (UnAL,FmtText  )  'DEACTIVATE/MOTION, ID = 3020'


   ! DEACTIVATE the FIXED JOINTS in the tower and blade if tower and blade
   !   flexibility are enabled:
   ! It is necessary to lock the flexible elements together at the start
   !   of the simulation since the initial rotor speed kicks the system
   !   to intensly during the initial condition solution.  Locking the
   !   elements together during the initial condition solution eliminates
   !   this problem.
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Simplify the SFORCE used to generate free surface GRAPHICS in the
!jmj   FAST-to-ADAMS preprocessor.  Also, eliminate the free surface DOFs
!jmj   during a linearization analysis:
   ! Do not DEACTIVATE the free surface MOTION statements here:
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

IF ( TwFADOF1 )  THEN   ! Tower flexibility is enabled.

   WRITE    (   UnAL,FmtText  )  'DEACTIVATE/JOINT, RANGE = '//TRIM(Int2LStr(           1300 + 1        ))//', '// &
                                                               TRIM(Int2LStr(           1300 + TwrNodes ))
   WRITE    (   UnAL,FmtText  )  'DEACTIVATE/JOINT, ID = '//TRIM(   Int2LStr(           1500            ))

ENDIF

IF ( FlapDOF1 )  THEN   ! Blade flexibility is enabled.

   DO K = 1,NumBl       ! Loop through all the blades

      WRITE (   UnAL,FmtText  )  'DEACTIVATE/JOINT, RANGE = '//TRIM(Int2LStr( 10000*K + 3000 + 1        ))//', '// &
                                                               TRIM(Int2LStr( 10000*K + 3000 + BldNodes ))
      IF ( TipMass(K) /= 0.0 )  &
         WRITE (UnAL,FmtText  )  'DEACTIVATE/JOINT, ID = '//TRIM(   Int2LStr( 10000*K + 5000            ))

   ENDDO                ! K - blades

ENDIF


   ! Run a STATICS analysis:

WRITE (UnAL,FmtText  )  'SIMULATE/STATICS'


   ! Run the ADAMS LINEAR analysis; send the first 25 mode shapes to the .out file:

!jmj Start of proposed change.  v6.10d-jmj  13-Aug-2009.
!jmj Do not eliminate damping when linearizing in ADAMS:
!remove6.10dWRITE (UnAL,FmtText  )  'LINEAR/EIGENSOL, NODAMPIN, COORDS = 1, 25'
WRITE (UnAL,FmtText  )  'LINEAR/EIGENSOL, COORDS = 1, 25'
!jmj End of proposed change.  v6.10d-jmj  13-Aug-2009.


   ! We're done!

WRITE (UnAL,FmtText  )  'STOP'


   ! Inform the users of this great news!

CALL WrScr (' ADAMS command file '''//TRIM( RootName )//'_ADAMS_LIN.acf'' created.')


   ! Close the file.

CLOSE ( UnAL )



RETURN
END SUBROUTINE MakeACF_LIN
!=======================================================================
SUBROUTINE MakeADM


   ! This routine generates an ADAMS dataset file (.adm) using the
   !   geometry and mechanical properties specified in the FAST input
   !   files as model parameters.


   ! AeroDyn MODULEs:

!bjj Start of proposed change, AD v12.70a-bjj
!rmUSE                             AD_IOParams
!USE                             AD_IOParams, ONLY: UnADin
USE                             AeroDyn  !to get the unit number for AeroDyn... can we fix this????
!bjj End of proposed change
!bjj rm NWTC_Library: USE                             Precision


   ! FAST MODULEs:

USE                             ADAMSInput
USE                             Blades
USE                             Constants
USE                             CoordSys
USE                             DOFs
USE                             DriveTrain
USE                             EnvCond
USE                             Features
USE                             General
USE                             InitCond
USE                             MassInert
USE                             Modes
USE                             NacelleYaw
USE                             Output
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Rename MODULE PlatformLd() to Platform():
!remove6.02aUSE                             PlatformLd
USE                             Platform
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
USE                             RotorFurling
USE                             RtHndSid
USE                             SimCont
!bjj rm NWTC_Library: USE                             SysSubs
USE                             TeeterVars
USE                             TailAero
USE                             TailFurling
USE                             TipBrakes
USE                             Tower
USE                             TurbConf
USE                             TurbCont
!bjj start of proposed change vXX
USE                             FASTSubs    !SetCoordSy
!bjj end of proposed change


IMPLICIT                        NONE


   ! Local variables:

REAL(ReKi)                   :: CMatrix   (6,6)                                 ! A temporary element damping CMatrix for FIELD statements.
REAL(ReKi)                   :: CRatioBEd                                       ! The ratio of CMatrix to KMatrix for the blade edge        deflection.
REAL(ReKi)                   :: CRatioBFl                                       ! The ratio of CMatrix to KMatrix for the blade flap        deflection.
REAL(ReKi)                   :: CRatioTFA                                       ! The ratio of CMatrix to KMatrix for the tower FA          deflection.
REAL(ReKi)                   :: CRatioTSS                                       ! The ratio of CMatrix to KMatrix for the tower SS          deflection.
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
REAL(ReKi)                   :: CWaveDir                                        ! COS( WaveDir )
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
REAL(ReKi), ALLOCATABLE      :: DRNodesGRA(:)                                   ! Length of variable-spaced blade elements used in blade graphics.
REAL(ReKi), ALLOCATABLE      :: EAVec     (:,:,:)                               ! Position vector directed from the structural axis of blade K, element J-1 to the structural axis of blade K, element J.
REAL(ReKi)                   :: KMatrix   (6,6)                                 ! A temporary element stiffness KMatrix for FIELD statements.
REAL(ReKi)                   :: Ref1      (3)                                   ! Vector / direction Ref1 of the reference blade axis (= j1 if precurve and presweep are zero).
REAL(ReKi)                   :: Ref2      (3)                                   ! Vector / direction Ref2 of the reference blade axis (= j2 if precurve and presweep are zero).
REAL(ReKi)                   :: Ref3      (3)                                   ! Vector / direction Ref3 of the reference blade axis (= j3 if precurve and presweep are zero).
REAL(ReKi)                   :: Slopexb                                         ! Slope of the reference axis about the xb-axis using central difference differentation.
REAL(ReKi)                   :: Slopeyb                                         ! Slope of the reference axis about the yb-axis using central difference differentation.
REAL(ReKi), PARAMETER        :: SmllNmbr  = 9.999E-4                            ! A small number used to define masses and inertias of PARTs in ADAMS to avoid singularities in ADAMS' equations of motion (EoM).
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
REAL(ReKi)                   :: SWaveDir                                        ! SIN( WaveDir )
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
REAL(ReKi)                   :: ThnBarI                                         ! The tranverse inertia of thin uniform bar about the bar's c.g. used to determine the transverse inertias of tower and blade elements
REAL(ReKi)                   :: TmpLength                                       ! A temporary distance
REAL(ReKi)                   :: TmpLength2                                      ! = TmpLength^2.
REAL(ReKi)                   :: TmpLength3                                      ! = TmpLength^3.
REAL(ReKi)                   :: TmpVec    (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TmpVec1   (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TmpVec2   (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TransMat  (3,3)                                 ! The resulting transformation matrix due to three orthogonal rotations, (-).

INTEGER(4)                   :: CompAeroI                                       ! An INTEGER representing what is in CompAero: = 0 if CompAero = .FALSE., 1 if CompAero = .TRUE.
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
!remove6.02aINTEGER(4)                   :: CompAeroNB                                      ! An INTEGER representing what is in CompAero, NumBl, and TabDelim: = NumBl if CompAero = .FALSE. (+ 10 if CompAero = .TRUE. ) (+ 100 if TabDelim = .TRUE.).
INTEGER(4)                   :: CompHydroI                                      ! An INTEGER representing what is in CompHydro: = 0 if CompHydro = .FALSE., 1 if CompHydro = .TRUE.
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
INTEGER(4)                   :: GenTiStrp                                       ! An INTEGER representing what is in GenTiStr and GenTiStp = (1 if GenTiStr = .TRUE.) (+ 10 if GenTiStp = .TRUE.).
INTEGER(4)                   :: I                                               ! Generic Index
INTEGER(4)                   :: J                                               ! Loops through nodes / elements.
INTEGER(4)                   :: K                                               ! Loops through blades.
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Simplify the SFORCE used to generate free surface GRAPHICS in the
!jmj   FAST-to-ADAMS preprocessor.  Also, eliminate the free surface DOFs
!jmj   during a linearization analysis:
!remove6.02bINTEGER(4)                   :: NFreeSrfc = -1                                  ! Number of points on free surface (not including the zero'th point) where the elevation of the incident waves will be computed (computed every FrSrfcSpc meters along the incident wave propogation heading direction for a length of the rotor diameter).
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
!bjj start of proposed change
!INTEGER(4)                   :: Sttus                                           ! Status of an attempted array allocation.
INTEGER                      :: Sttus                                           ! Status of an attempted array allocation.
!bjj end of proposed change
INTEGER(4)                   :: SubAxI                                          ! An INTEGER representing what is in SubAxInd = (1 if SubAxInd = .TRUE.; 0 otherwise).
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
INTEGER(4)                   :: TabDelimI                                       ! An INTEGER representing what is in TabDelim : = 0 if TabDelim  = .FALSE., 1 if TabDelim  = .TRUE.
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
INTEGER(4)                   :: TmpID                                           ! A temporary ID for a PART, MARKER, etc...
INTEGER(4)                   :: TmpID2                                          ! A temporary ID for a PART, MARKER, etc...

CHARACTER( 3)                :: FmtText   = '(A)'                               ! Format for outputting pure text.
CHARACTER(10)                :: FmtTR     = '(A,ES13.6)'                        ! Format for outputting text then a real value.
!bjj rm unused:CHARACTER(19)                :: FmtTRTR   = '(A,ES13.6,A,ES13.6)'               ! Format for outputting text, a real value, text, and a real value.
CHARACTER(28)                :: FmtTRTRTR = '(A,ES13.6,A,ES13.6,A,ES13.6)'      ! Format for outputting text, a real value, text, a real value, text, and (you guessed it!) a real value.
CHARACTER(55)                :: FmtTRTRTRTRTRTR = '(A,ES11.4,A,ES11.4,A,ES11.4,A,ES11.4,A,ES11.4,A,ES11.4)' ! Format for outputting text, a real value, text, a real value, text, a real value, text, a real value, text, a real value, text, and (you guessed it!) a real value.


   ! Global functions:

!bjj rm DOT_PRODUCT: REAL(ReKi), EXTERNAL         :: DotProd                                         ! A function returning the dot product of two vectors.

!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: CurDate                                         ! A function that returns the durrent date in the form "dd-mmm-ccyy".
!bjj rm AD 12.70b CHARACTER( 8), EXTERNAL      :: CurTime                                         ! A function that returns the durrent date in the form "hh:mm:ss".
!bjj rm AD 12.70b CHARACTER(15), EXTERNAL      :: Flt2LStr                                        ! A function to convert a real to a left-justified string.
!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.



   ! An equivalent ADAMS model can't be built for all possible FAST models.
   ! Make sure FAST aborts if any of the following conditions are met:

IF ( SCAN( FTitle, ',' ) /= 0 )  &
   CALL ProgAbort ( ' An ADAMS dataset can''t be built if character "," appears in the title of the primary FAST input file: "'// &
                TRIM( FTitle )//'".'                                                                                            )
IF ( SCAN( FTitle, ';' ) /= 0 )  &
   CALL ProgAbort ( ' An ADAMS dataset can''t be built if character ";" appears in the title of the primary FAST input file: "'// &
                TRIM( FTitle )//'".'                                                                                            )
IF ( SCAN( FTitle, '&' ) /= 0 )  &
   CALL ProgAbort ( ' An ADAMS dataset can''t be built if character "&" appears in the title of the primary FAST input file: "'// &
                TRIM( FTitle )//'".'                                                                                            )
IF ( SCAN( FTitle, '!' ) /= 0 )  &
   CALL ProgAbort ( ' An ADAMS dataset can''t be built if character "!" appears in the title of the primary FAST input file: "'// &
                TRIM( FTitle )//'".'                                                                                            )

IF ( ( PtfmRDOF .NEQV. PtfmPDOF ) .OR. ( PtfmRDOF .NEQV. PtfmYDOF ) )  &   ! .TRUE. if one platform rotation DOF is set differently than the other two
   CALL ProgAbort ( ' An ADAMS dataset can''t be built when one of the platform rotational DOFs is set differently than the'// &
                ' other two.  Set PtfmRDOF, PtfmPDOF, and PtfmYDOF to the same value (i.e., all .TRUE. or all .FALSE.).'     )

IF ( ( .NOT. YawDOF ) .AND. ( ( ( YCMode /= 0 ) .AND. ( TYCOn < TMax ) ) .OR. ( TYawManS < TMax ) ) )  &
   CALL ProgAbort ( ' An ADAMS dataset can''t be built with yaw control unless the yaw DOF is enabled.'//  &
                '  Set YawDOF to .TRUE., YCMode to 0, or TYawManS > TMax.'                               )

IF (     TwFADOF1 .AND. ( .NOT. TwSSDOF1 ) )  THEN ! FA flexibility is enabled, SS is rigid
    CALL ProgAbort ( ' An ADAMS dataset can''t be built with tower FA flexibility and SS rigidity.'// &
                 '  Force TwFADOF1 to equal TwSSDOF1 or vice-versa.'                                )
ELSEIF ( TwSSDOF1 .AND. ( .NOT. TwFADOF1 ) )  THEN ! SS flexibility is enabled, FA is rigid
   CALL ProgAbort ( ' An ADAMS dataset can''t be built with tower SS flexibility and FA rigidity.'// &
                '  Force TwFADOF1 to equal TwSSDOF1 or vice-versa.'                                )
ENDIF

! NOTE: Blade flexibility is determined by FlapDOF1
! NOTE: FlapDOF2 is ignored by MakeADM()
IF (     FlapDOF1 .AND. ( .NOT. EdgeDOF  ) )  THEN ! flap flexibility is enabled, edge is rigid
   CALL ProgAbort ( ' An ADAMS dataset can''t be built with blade flap flexibility and edge rigidity.'// &
                '  Force FlapDOF1 to equal EdgeDOF or vice-versa.'                                     )
ELSEIF ( EdgeDOF  .AND. ( .NOT. FlapDOF1 ) )  THEN ! edge flexibility is enabled, flap is rigid
   CALL ProgAbort ( ' An ADAMS dataset can''t be built with blade edge flexibility and flap rigidity.'// &
                '  Force FlapDOF1 to equal EdgeDOF or vice-versa.'                                     )
ENDIF

IF ( OoPDefl  /= 0.0 )  CALL ProgAbort ( ' An ADAMS dataset can''t be built with initial OoP blade-tip displacements.'// &
                                         '  Set OoPDefl to 0.0.'                                                           )
IF ( IPDefl   /= 0.0 )  CALL ProgAbort ( ' An ADAMS dataset can''t be built with initial IP blade-tip displacements.'// &
                                         '  Set IPDefl to 0.0.'                                                           )
IF ( TTDspFA  /= 0.0 )  CALL ProgAbort ( ' An ADAMS dataset can''t be built with an initial FA tower-top displacement.'// &
                                         '  Set TTDspFA to 0.0.'                                                            )
IF ( TTDspSS  /= 0.0 )  CALL ProgAbort ( ' An ADAMS dataset can''t be built with an initial SS tower-top displacement.'// &
                                         '  Set TTDspSS to 0.0.'                                                            )
If ( TeetDefl /= 0.0 )  CALL ProgAbort ( ' An ADAMS dataset can''t be built with an initial teeter angle.'// &
                                         '  Set TeetDefl to 0.0.'                                              )

IF ( GBoxEff  /= 1.0 )  CALL ProgAbort ( ' An ADAMS dataset can''t be built with a non-ideal gearbox.'// &
                                         '  Set GBoxEff to 100.0.'                                         )
IF ( GBRevers        )  CALL ProgAbort ( ' An ADAMS dataset can''t be built with a gearbox reversal.'// &
                                         '  Set GBRevers to False.'                                       )

IF ( TwrNodes  > 99  )  CALL ProgAbort ( ' An ADAMS dataset can''t be built with more than 99 tower elements.'// &
                                         '  Set TwrNodes <= 99.'                                                   )
IF ( BldNodes  > 99  )  CALL ProgAbort ( ' An ADAMS dataset can''t be built with more than 99 blade elements.'// &
                                         '  Set BldNodes <= 99.'                                                   )

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
IF ( NumLines  > 99  )  CALL ProgAbort ( ' An ADAMS dataset can''t be built with more than 99 mooring lines.'// &
                                         '  Set NumLines <= 99.'                                                  )
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.


   ! ALLOCATE some arrays:

ALLOCATE ( DRNodesGRA(BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the DRNodesGRA array.' )
ENDIF

ALLOCATE ( EAVec(NumBl,TipNode,3) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the EAVec array.' )
ENDIF


   ! Lets define the coordinate systems that will be used throughout this
   !   routine to orient PARTs and MARKERs:

QT  = Q (:,1) ! Transfer the initial conditions of the DOFs to the QT array, which is used throughout SetCoordSy().
CALL SetCoordSy



   ! Open the ADAMS dateset file and give it a heading:

CALL OpenFOutFile ( UnAD, TRIM( RootName )//'_ADAMS.adm' )

WRITE (UnAD,FmtText  )  '!ADAMS/View model name: '//TRIM( FTitle )
!bjj start of proposed change vxx
!rmWRITE (UnAD,FmtText  )  '!This ADAMS dataset file was generated by '//ProgName//TRIM( ProgVer )// &
!rm                        ' on '//CurDate()//' at '//CurTime()//'.'
WRITE (UnAD,FmtText  )  '!This ADAMS dataset file was generated by '//TRIM(ProgName)//' '//TRIM( ProgVer )// &
                        ' on '//CurDate()//' at '//CurTime()//'.'
!bjj end of proposed change
WRITE (UnAD,FmtText  )  '!Turbine input data from file "'//TRIM( PriFile )//'".'


   ! The VARIABLE statement for calling routine CalcOuts() at every time step:
   ! Find CompAeroI  = 0 if CompAero  = .FALSE., 1 if CompAero  = .TRUE.:
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
!remove6.02a   ! Find CompAeroNB = NumBl (+ 10 if CompAero = .TRUE.) (+ 100 if TabDelim = .TRUE.):
!remove6.02a
!remove6.02aIF ( CompAero )  THEN
!remove6.02a   CompAeroI  = 1
!remove6.02a   CompAeroNB = 10  + NumBl
!remove6.02aELSE
!remove6.02a   CompAeroI  = 0
!remove6.02a   CompAeroNB =       NumBl
!remove6.02aENDIF
!remove6.02aIF ( TabDelim )  THEN
!remove6.02a   CompAeroNB = 100 + CompAeroNB
!remove6.02aENDIF
   ! Find CompHydroI = 0 if CompHydro = .FALSE., 1 if CompHydro = .TRUE.:
   ! Find TabDelimI  = 0 if TabDelim  = .FALSE., 1 if TabDelim  = .TRUE.:

IF ( CompAero  )  THEN
   CompAeroI  = 1
ELSE
   CompAeroI  = 0
ENDIF

IF ( CompHydro )  THEN
   CompHydroI = 1
ELSE
   CompHydroI = 0
ENDIF

IF ( TabDelim  )  THEN
   TabDelimI  = 1
ELSE
   TabDelimI  = 0
ENDIF
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


WRITE (UnAD,FmtText  )  '!                             adams_view_name=''CalcOuts_V'''
WRITE (UnAD,FmtText  )  'VARIABLE/1'
WRITE (UnAD,FmtText  )  ', FUNCTION = USER( '//TRIM(Flt2LStr( AzimB1Up ))//', '//TRIM(Flt2LStr( GBRatio ))// &
                        ', '//TRIM(Flt2LStr( AvgNrmTpRd ))//', '//TRIM(Flt2LStr( ProjArea ))//               &
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
!remove6.02a                        ', '//TRIM(Int2LStr( CompAeroNB ))//', '//TRIM(Int2LStr( BldNodes ))//               &
!remove6.02a                        ', '//TRIM(Int2LStr( TwrNodes   ))//', '//TRIM(Flt2LStr( TipRad   ))//               &
!remove6.02a                        ', '//TRIM(Flt2LStr( GenIner    ))//' )'
                        ', '//TRIM(Int2LStr( CompAeroI  ))//', '//TRIM(Int2LStr( CompHydroI ))//             &
                        ', '//TRIM(Int2LStr( TabDelimI  ))//', '//TRIM(Int2LStr( NumBl      ))//','
WRITE (UnAD,FmtText  )  ', '//TRIM(Int2LStr( BldNodes   ))//', '//TRIM(Int2LStr( TwrNodes   ))//             &
                        ', '//TRIM(Flt2LStr( TipRad     ))//', '//TRIM(Flt2LStr( GenIner    ))//' )'
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.



   ! Begin defining PARTs, MARKERs, and GRAPHICS:

WRITE (UnAD,FmtText  )  '!=================================== PARTS ====================================='



   ! Those on the GROUND:

WRITE (UnAD,FmtText  )  '!----------------------------------- Ground ------------------------------------'


   ! GROUND:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''Ground_P'''
WRITE (UnAD,FmtText  )  'PART/1'
WRITE (UnAD,FmtText  )  ', GROUND'


   ! AeroDyn GROUND MARKER:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''AeroDynGround_M'''
WRITE (UnAD,FmtText  )  'MARKER/1'
WRITE (UnAD,FmtText  )  ', PART = 1'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', 0.0, ', ', 0.0, ', ', PtfmRef
WRITE (UnAD,FmtText  )  ', REULER = 0D, 0D, 0D'


   ! Inertial frame coordinate system:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''InertialFrameCS_M'''
WRITE (UnAD,FmtText  )  'MARKER/10'    ! MARKER/10 is equivalent to the inertial frame coordinate system: X = Xi, Y = Yi, Z = Zi
WRITE (UnAD,FmtText  )  ', PART = 1'


   ! Initial platform orientation MARKER:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''InitPtfmOrientation_M'''
WRITE (UnAD,FmtText  )  'MARKER/11'
WRITE (UnAD,FmtText  )  ', PART = 1'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', 0.0  , ', ', 0.0   , ', ', 0.0    ! Orient the initial platform
WRITE (UnAD,FmtTRTRTR)  ', ZP = ', a2(1), ', ', -a2(3), ', ', a2(2)  ! orientation MARKER
WRITE (UnAD,FmtTRTRTR)  ', XP = ', a1(1), ', ', -a1(3), ', ', a1(2)  ! using the 3-point method

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
IF ( ( PtfmModel == 3 ) .AND. CompHydro )  THEN ! .TRUE. if we have floating offshore turbine and we are using the undocumented platform features.
   IF ( LineMod == 1 )  THEN                    ! .TRUE if we have standard quasi-static mooring lines; store the mooring line data into the ARRAY

   ! Anchors:

      DO I = 1,NumLines ! Loop through all mooring lines
         WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''Anchor', I, '_M'''
         WRITE (UnAD,FmtText     )  'MARKER/'//TRIM(Int2LStr( 700 + I ))
         WRITE (UnAD,FmtText     )  ', PART = 1'
         WRITE (UnAD,FmtTRTRTR   )  ', QP = ', LAnchxi(I), ', ', LAnchyi(I), ', ', LAnchzi(I)   ! NOTE: PtfmRef = 0.0 in this equation
         WRITE (UnAD,FmtText     )  ', REULER = 0D, 0D, 0D'
      ENDDO             ! I - All mooring lines

   ENDIF
ENDIF


!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

   ! FLOATING Aerodynamic and Hydrodynamic MARKERs:

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailFinFloatingAero_M'''
WRITE (UnAD,FmtText  )  'MARKER/500'
WRITE (UnAD,FmtText  )  ', PART = 1'
WRITE (UnAD,FmtText  )  ', FLOATING'

!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''PlatformLoadingFloating_M'''
WRITE (UnAD,FmtText  )  'MARKER/800'
WRITE (UnAD,FmtText  )  ', PART = 1'
WRITE (UnAD,FmtText  )  ', FLOATING'

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:
!remove6.02a
!remove6.02aWRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailFinFloatingAero_M'''
!remove6.02aWRITE (UnAD,FmtText  )  'MARKER/500'
!remove6.02aWRITE (UnAD,FmtText  )  ', PART = 1'
!remove6.02aWRITE (UnAD,FmtText  )  ', FLOATING'
WRITE (UnAD,FmtText  )  '!------------------ Ground: Floating Aero and Hydro for Tower ------------------'

DO J = 1,TwrNodes ! Loop through the blade nodes / elements
   TmpID = 800 + J
   WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''TowerSec', J, 'FloatingAeroHydro_M'''
   WRITE (UnAD,FmtText     )  'MARKER/'//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtText     )  ', PART = 1'
   WRITE (UnAD,FmtText     )  ', FLOATING'
ENDDO             ! J - Blade nodes / elements
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

DO K = 1,NumBl       ! Loop through all blades

   WRITE (UnAD,'(A,I1,A)')  '!--------------------- Ground: Floating Aero for Blade ', K, ' -----------------------'

   DO J = 1,BldNodes ! Loop through the blade nodes / elements
      TmpID = K*100 + J
      WRITE (UnAD,'(A,I1,A,I2.2,A)')  '!                             adams_view_name=''Bld', K , 'Sec', J, 'FloatingAero_M'''
      WRITE (UnAD,FmtText          )  'MARKER/'//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtText          )  ', PART = 1'
      WRITE (UnAD,FmtText          )  ', FLOATING'
   ENDDO             ! J - Blade nodes / elements

   TmpID = 500 + K
   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''TipBrake', K, 'FloatingAero_M'''
   WRITE (UnAD,FmtText   )  'MARKER/'//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtText   )  ', PART = 1'
   WRITE (UnAD,FmtText   )  ', FLOATING'

ENDDO                ! K - All Blades

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
!jmj Also, add a GRAPHICS statement to depict the ground as a CIRCLE of radius
!jmj   TipRad:
WRITE (UnAD,FmtText  )  '!------------------------------- Ground GRAPHICS -------------------------------'


IF ( CompHydro )  THEN  ! .TRUE. if we are using the undocumented monopile or platform features


   ! Seabed MARKER:

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''Seabed_M'''
   WRITE (UnAD,FmtText  )  'MARKER/20'
   WRITE (UnAD,FmtText  )  ', PART = 1'
   WRITE (UnAD,FmtTRTRTR)  ', QP = ', 0.0, ', ', 0.0, ', ', PtfmRef - WtrDpth
   WRITE (UnAD,FmtText  )  ', REULER = 0D, 0D, 0D'


   ! Seabed graphics:

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''Seabed_G'''
   WRITE (UnAD,FmtText  )  'GRAPHICS/20'
   WRITE (UnAD,FmtText  )  ', CIRCLE'
   WRITE (UnAD,FmtText  )  ', CM = 20'
!JASON:THIS CHANGED FOR ITI BARGE:   WRITE (UnAD,FmtText  )  ', RADIUS = 425'  !JASON:THIS CHANGED FOR ITI BARGE:
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Start of proposed change.  v6.10a-jmj  21-Feb-2007.
!jmj Make sure the seabed GRAPHICS in FAST-to-ADAMS has a radius at least as
!jmj   large as the maximum mooring line anchor radius:
!remove6.10a   WRITE (UnAD,FmtTR    )  ', RADIUS = ', TipRad
   WRITE (UnAD,FmtTR    )  ', RADIUS = ', MAX( TipRad, MaxLRadAnch )
!jmj End of proposed change.  v6.10a-jmj  21-Feb-2007.
   WRITE (UnAD,FmtText  )  ', SEG = '//TRIM(Int2LStr( NSides ))


ENDIF


!JASON: MOVE THE ADAMS STATEMENTS TO THE PROPER LOCATIONS WITHIN THE ADM FILE ONCE WE DOCUMENT THIS FEATURE!!!!!
IF ( CompHydro .AND. SaveGrphcs .AND. ( WaveMod /= 4 ) )  THEN ! .TRUE. if we are using the undocumented monopile or platform features .AND. SaveGrphcs is enabled, but not with GH Bladed wave data


   ! Those on the free surface of the water:

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Simplify the SFORCE used to generate free surface GRAPHICS in the
!jmj   FAST-to-ADAMS preprocessor.  Also, eliminate the free surface DOFs
!jmj   during a linearization analysis:
!remove6.02b   WRITE (UnAD,FmtText  )  '!-------------------------- Free Surface of the Water --------------------------'
   WRITE (UnAD,FmtText  )  '!--------------------- Free Surface of the Water GRAPHICS ----------------------'
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.


   ! Compute the variables needed to place MARKERs on the incident wave
   !   propogation heading direction:

   NFreeSrfc = CEILING ( ( 2.0*TipRad )/FrSrfcSpc )
   IF ( NFreeSrfc > 99 )  CALL ProgAbort ( ' An ADAMS dataset can''t be built with more than 99 free'// &
                                          ' surface GRAPHICS statements.  Set FrSrfcSpc >= 2*TipRad/99.' )

   TmpLength = 0.5*NFreeSrfc*FrSrfcSpc ! The distance in the xi/yi plane between the inertial frame reference point and the line of MARKERs representing the incident wave propogation heading direction.
   CWaveDir  = COS( D2R*WaveDir )
   SWaveDir  = SIN( D2R*WaveDir )


   DO I = 0,NFreeSrfc   ! Loop through all points on free surface (including the zero'th point) where the elevation of the incident waves will be computed


   ! The free surface MARKER attached to the GROUND:

      TmpID  = 100900 + I
      TmpID2 = 100800 + I
      WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''FreeSrfc', I , 'OnGround_M'''
      WRITE (UnAD,FmtText     )  'MARKER/'//TRIM(Int2LStr( TmpID - 100000 ))
      WRITE (UnAD,FmtText     )  ', PART = 1'
      WRITE (UnAD,FmtTRTRTR   )  ', QP = ', ( I*FrSrfcSpc - TmpLength )*CWaveDir + TmpLength*SWaveDir, &
                                 ', '     , ( I*FrSrfcSpc - TmpLength )*SWaveDir - TmpLength*CWaveDir, ', ', PtfmRef
      WRITE (UnAD,FmtTRTRTR   )  ', REULER = ', WaveDir*D2R, ', ', 0.0, ', ', 0.0


   ! Free surface PART:

      WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''FreeSrfc', I , '_P'''
      WRITE (UnAD,FmtText     )  'PART/'//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtTRTRTR   )  ', QG = ', ( I*FrSrfcSpc - TmpLength )*CWaveDir + TmpLength*SWaveDir, &
                                 ', '     , ( I*FrSrfcSpc - TmpLength )*SWaveDir - TmpLength*CWaveDir, ', ', PtfmRef
      WRITE (UnAD,FmtTRTRTR   )  ', REULER = ', WaveDir*D2R, ', ', 0.0, ', ', 0.0
      WRITE (UnAD,FmtText     )  ', CM = '//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtTR       )  ', MASS = ', SmllNmbr
      WRITE (UnAD,FmtTRTRTR   )  ', IP = ', SmllNmbr, ', ', SmllNmbr, ', ', SmllNmbr


   ! First free surface MARKER attached to the PART:

      WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''FreeSrfc', I , 'Ref1_M'''
      WRITE (UnAD,FmtText     )  'MARKER/'//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtText     )  ', PART = '//TRIM(Int2LStr( TmpID ))


   ! Second free surface MARKER attached to the PART:

      WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''FreeSrfc', I , 'Ref2_M'''
      WRITE (UnAD,FmtText     )  'MARKER/'//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText     )  ', PART = '//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtTRTRTR)  ', QP = ', 0.0, ', ', 2.0*TmpLength, ', ', 0.0
      WRITE (UnAD,FmtText  )  ', REULER = 0D, 0D, 0D'


   ! Free surface elevation JOINT and MOTION:

      WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''FreeSrfc', I , '_J'''
      WRITE (UnAD,FmtText     )  'JOINT/'//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtText     )  ', TRANSLATIONAL'
      WRITE (UnAD,FmtText     )  ', I = '//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtText     )  ', J = '//TRIM(Int2LStr( TmpID - 100000 ))

      WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''FreeSrfc', I , 'Demand_V'''
      WRITE (UnAD,FmtText     )  'VARIABLE/'//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtText     )  ', FUNCTION = USER( 0 )'

      WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''FreeSrfc', I , 'Error_V'''
      WRITE (UnAD,FmtText     )  'VARIABLE/'//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText     )  ', FUNCTION = VARVAL('//TRIM(Int2LStr( TmpID ))//') '// &
                                 '- DZ('//TRIM(Int2LStr( TmpID ))//','//TRIM(Int2LStr( TmpID - 100000 ))//',10)'

      WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''FreeSrfc', I , 'Actuator_SF'''
      WRITE (UnAD,FmtText     )  'SFORCE/'//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtText     )  ', TRANSLATION'
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Simplify the SFORCE used to generate free surface GRAPHICS in the
!jmj   FAST-to-ADAMS preprocessor.  Also, eliminate the free surface DOFs
!jmj   during a linearization analysis:
!remove6.02b      WRITE (UnAD,FmtText     )  ', I = '//TRIM(Int2LStr( TmpID ))
!remove6.02b      WRITE (UnAD,FmtText     )  ', J = '//TRIM(Int2LStr( TmpID - 100000 ))
!remove6.02b      WRITE (UnAD,FmtText     )  ', FUNCTION = '//TRIM(Flt2LStr( 0.0002249775 ))//'*VARVAL('//TRIM(Int2LStr( TmpID2 ))// &          ! NOTE: 0.0002249775kN/m     = 0.001*  Mass*NaturalFrequency^2            = 0.001*  (9.999E-4kg)*(5*(3 rad/s))^2
!remove6.02b                                 ')*SIGN(1.0,DZ('//TRIM(Int2LStr( TmpID ))//','//TRIM(Int2LStr( TmpID - 100000 ))//',10))'
!remove6.02b      WRITE (UnAD,FmtText     )           ', - '//TRIM(Flt2LStr( 0.0000209979 ))//'*VZ('//TRIM(Int2LStr( TmpID ))// &               ! NOTE: 0.0000209979kN/(m/s) = 0.001*2*Mass*NaturalFrequency*DampingRatio = 0.001*2*(9.999E-4kg)*(5*(3 rad/s))*(0.7)
!remove6.02b                                 ',0,10,0)*SIGN(1.0,DZ('//TRIM(Int2LStr( TmpID ))//','//TRIM(Int2LStr( TmpID - 100000 ))//',10))'
      WRITE (UnAD,FmtText     )  ', ACTIONONLY'
      WRITE (UnAD,FmtText     )  ', I = '//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtText     )  ', J = '//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtText     )  ', FUNCTION = '//TRIM(Flt2LStr( 0.0009868617 ))//'*VARVAL('//TRIM(Int2LStr( TmpID2 ))//')'// &  ! NOTE: 0.0009868617kN/m     = 0.001*  Mass*NaturalFrequency^2            = 0.001*  (9.999E-4kg)*(5Hz)^2     = 0.001*  (9.999E-4kg)*(31.41593rad/s)^2
                                           ' - '//TRIM(Flt2LStr( 0.0000439779 ))//'*VZ('//TRIM(Int2LStr( TmpID ))//',0,10,0)'    ! NOTE: 0.0000439779kN/(m/s) = 0.001*2*Mass*NaturalFrequency*DampingRatio = 0.001*2*(9.999E-4kg)*(5Hz)*(0.7) = 0.001*2*(9.999E-4kg)*(31.41593rad/s)*(0.7)

      WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''FreeSrfc', I , 'Locked_MO'''
      WRITE (UnAD,FmtText     )  'MOTION/'//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtText     )  ', I = '//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtText     )  ', J = '//TRIM(Int2LStr( TmpID - 100000 ))
      WRITE (UnAD,FmtText     )  ', Z'
      WRITE (UnAD,FmtText     )  ', DISPLACEMENT'
      WRITE (UnAD,FmtText     )  ', FUNCTION = 0'  ! Lock free surface PART at the MSL
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.


   ! Free surface GRAPHICS:

      IF ( I > 0 )  THEN   ! All but the zero'th point
         WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''FreeSrfc', I , '_G'''
         WRITE (UnAD,FmtText     )  'GRAPHICS/'//TRIM(Int2LStr( TmpID ))
         WRITE (UnAD,FmtText     )  ', OUTLINE = '//TRIM(Int2LStr( TmpID      ))//', '//TRIM(Int2LStr( TmpID2    ))// &
                                    ', '          //TRIM(Int2LStr( TmpID2 - 1 ))//', '//TRIM(Int2LStr( TmpID - 1 ))// &
                                    ', '          //TRIM(Int2LStr( TmpID      ))
      ENDIF


   ENDDO                ! I - All points on free surface (including the zero'th point) where the elevation of the incident waves will be computed

ELSE


   ! GROUND graphics:

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''Ground_G'''
   WRITE (UnAD,FmtText  )  'GRAPHICS/1'
   WRITE (UnAD,FmtText  )  ', CIRCLE'
   WRITE (UnAD,FmtText  )  ', CM = 1'
   WRITE (UnAD,FmtTR    )  ', RADIUS = ', TipRad
   WRITE (UnAD,FmtText  )  ', SEG = '//TRIM(Int2LStr( NSides ))


ENDIF


!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
!JASON: MOVE THE ADAMS STATEMENTS TO THE PROPER LOCATIONS WITHIN THE ADM FILE ONCE WE DOCUMENT THIS FEATURE!!!!!
!JASON: WHAT DO WE DO HERE DURING LINEARIZATION????
IF ( ( PtfmModel == 3 ) .AND. CompHydro .AND. SaveGrphcs )  THEN  ! .TRUE. if we have floating offshore turbine and we are using the undocumented platform features .AND. SaveGrphcs is enabled.
   IF ( LineMod == 1 )  THEN                                      ! .TRUE if we have standard quasi-static mooring lines; store the mooring line data into the ARRAY

   ! Those on the mooring lines:

      WRITE (UnAD,FmtText  )  '!--------------------------- Mooring Lines GRAPHICS ----------------------------'

      IF ( NLnNodes  > 99  )  CALL ProgAbort ( ' An ADAMS dataset can''t be built with more than 99 GRAPHICS'// &
                                               ' nodes per mooring line.  Set NLnNodes <= 99.'                    )

      DO I = 1,NumLines ! Loop through all mooring lines

         TmpVec = PtfmSurge *z1 + PtfmHeave *z2 - PtfmSway  *z3 &
                + LFairxt(I)*a1 + LFairzt(I)*a2 - LFairyt(I)*a3 &
                - LAnchxi(I)*z1 - LAnchzi(I)*z2 + LAnchyi(I)*z3   ! = Position vector directed from the anchor to the fairlead of the current mooring line at simulation initialization

         DO J = 1,NLnNodes ! Loop through all the nodes per line for mooring line GRAPHICS

   ! Mooring line node POINT_MASS:
   ! NOTE: Since we don't yet know the position of each node (because routine
   !       Catenary() has not been called yet), instead, the mooring line nodes
   !       are placed at equally-spaced increments between the anchor and
   !       fairlead.

            TmpID  = 100000*I + 700 + J
            WRITE (UnAD,'(A,I2.2,A,I2.2,A)')  '!                             adams_view_name=''Line', I, 'Node', J, '_PM'''
            WRITE (UnAD,FmtText            )  'POINT_MASS/'//TRIM(Int2LStr( TmpID ))
            WRITE (UnAD,FmtTRTRTR          )  ', QG = ', LAnchxi(I) + TmpVec(1)*J/( NLnNodes + 1 ), &
                                              ', '     , LAnchyi(I) - TmpVec(3)*J/( NLnNodes + 1 ), &
                                              ', '     , LAnchzi(I) + TmpVec(2)*J/( NLnNodes + 1 )    ! NOTE: PtfmRef = 0.0 in this equation
            WRITE (UnAD,FmtText            )  ', REULER = 0D, 0D, 0D'
            WRITE (UnAD,FmtText            )  ', CM = '//TRIM(Int2LStr( TmpID ))
            WRITE (UnAD,FmtTR              )  ', MASS = ', SmllNmbr

   ! Mooring line node center of mass:

            WRITE (UnAD,'(A,I2.2,A,I2.2,A)')  '!                             adams_view_name=''Line', I, 'Node', J, 'CM_M'''
            WRITE (UnAD,FmtText            )  'MARKER/'//TRIM(Int2LStr( TmpID ))
            WRITE (UnAD,FmtText            )  ', POINT_MASS = '//TRIM(Int2LStr( TmpID ))


   ! Nodal position control:

            WRITE (UnAD,'(A,I2.2,A,I2.2,A)')  '!                             adams_view_name=''Line', I, 'Node', J, 'Floating_M'''
            WRITE (UnAD,FmtText            )  'MARKER/'//TRIM(Int2LStr( TmpID + 1000 ))
            WRITE (UnAD,FmtText            )  ', PART = 1'
            WRITE (UnAD,FmtText            )  ', FLOATING'

            WRITE (UnAD,'(A,I2.2,A,I2.2,A)')  '!                             adams_view_name=''Line', I, 'Node', J, 'Demandxi_V'''
            WRITE (UnAD,FmtText            )  'VARIABLE/'//TRIM(Int2LStr( TmpID + 1000 ))
            WRITE (UnAD,FmtText            )  ', FUNCTION = USER( 0 )'

            WRITE (UnAD,'(A,I2.2,A,I2.2,A)')  '!                             adams_view_name=''Line', I, 'Node', J, 'Demandyi_V'''
            WRITE (UnAD,FmtText            )  'VARIABLE/'//TRIM(Int2LStr( TmpID + 2000 ))
            WRITE (UnAD,FmtText            )  ', FUNCTION = USER( 0 )'

            WRITE (UnAD,'(A,I2.2,A,I2.2,A)')  '!                             adams_view_name=''Line', I, 'Node', J, 'Demandzi_V'''
            WRITE (UnAD,FmtText            )  'VARIABLE/'//TRIM(Int2LStr( TmpID + 3000 ))
            WRITE (UnAD,FmtText            )  ', FUNCTION = USER( 0 )'

            WRITE (UnAD,'(A,I2.2,A,I2.2,A)')  '!                             adams_view_name=''Line', I, 'Node', J, 'Errorxi_V'''
            WRITE (UnAD,FmtText            )  'VARIABLE/'//TRIM(Int2LStr( TmpID + 4000 ))
            WRITE (UnAD,FmtText            )  ', FUNCTION = VARVAL('//TRIM(Int2LStr( TmpID + 1000 ))//') '// &
                                              '- DX('//TRIM(Int2LStr( TmpID ))//',10,10)'

            WRITE (UnAD,'(A,I2.2,A,I2.2,A)')  '!                             adams_view_name=''Line', I, 'Node', J, 'Erroryi_V'''
            WRITE (UnAD,FmtText            )  'VARIABLE/'//TRIM(Int2LStr( TmpID + 5000 ))
            WRITE (UnAD,FmtText            )  ', FUNCTION = VARVAL('//TRIM(Int2LStr( TmpID + 2000 ))//') '// &
                                              '- DY('//TRIM(Int2LStr( TmpID ))//',10,10)'

            WRITE (UnAD,'(A,I2.2,A,I2.2,A)')  '!                             adams_view_name=''Line', I, 'Node', J, 'Errorzi_V'''
            WRITE (UnAD,FmtText            )  'VARIABLE/'//TRIM(Int2LStr( TmpID + 6000 ))
            WRITE (UnAD,FmtText            )  ', FUNCTION = VARVAL('//TRIM(Int2LStr( TmpID + 3000 ))//') '// &
                                              '- DZ('//TRIM(Int2LStr( TmpID ))//',10,10)'

            WRITE (UnAD,'(A,I2.2,A,I2.2,A)')  '!                             adams_view_name=''Line', I, 'Node', J, 'Actuator_VF'''
            WRITE (UnAD,FmtText            )  'VFORCE/'//TRIM(Int2LStr( TmpID ))
            WRITE (UnAD,FmtText            )  ', I = '//TRIM(Int2LStr( TmpID ))
            WRITE (UnAD,FmtText            )  ', JFLOAT = '//TRIM(Int2LStr( TmpID + 1000 ))
            WRITE (UnAD,FmtText            )  ', RM = '//TRIM(Int2LStr( TmpID ))
            WRITE (UnAD,FmtText            )  ', FX = '//TRIM(Flt2LStr( 0.0009868617 ))// &  ! NOTE: 0.0009868617kN/m     = 0.001*  Mass*NaturalFrequency^2            = 0.001*  (9.999E-4kg)*(5Hz)^2     = 0.001*  (9.999E-4kg)*(31.41593rad/s)^2
                                                      '*VARVAL('//TRIM(Int2LStr( TmpID + 4000 ))//')'// &
                                                  ' - '//TRIM(Flt2LStr( 0.0000439779 ))// &  ! NOTE: 0.0000439779kN/(m/s) = 0.001*2*Mass*NaturalFrequency*DampingRatio = 0.001*2*(9.999E-4kg)*(5Hz)*(0.7) = 0.001*2*(9.999E-4kg)*(31.41593rad/s)*(0.7)
                                                      '*VX('//TRIM(Int2LStr( TmpID ))//',0,10,0)\'
            WRITE (UnAD,FmtText            )  ', FY = '//TRIM(Flt2LStr( 0.0009868617 ))// &  ! NOTE: 0.0009868617kN/m     = 0.001*  Mass*NaturalFrequency^2            = 0.001*  (9.999E-4kg)*(5Hz)^2     = 0.001*  (9.999E-4kg)*(31.41593rad/s)^2
                                                      '*VARVAL('//TRIM(Int2LStr( TmpID + 5000 ))//')'// &
                                                  ' - '//TRIM(Flt2LStr( 0.0000439779 ))// &  ! NOTE: 0.0000439779kN/(m/s) = 0.001*2*Mass*NaturalFrequency*DampingRatio = 0.001*2*(9.999E-4kg)*(5Hz)*(0.7) = 0.001*2*(9.999E-4kg)*(31.41593rad/s)*(0.7)
                                                      '*VY('//TRIM(Int2LStr( TmpID ))//',0,10,0)\'
            WRITE (UnAD,FmtText            )  ', FZ = '//TRIM(Flt2LStr( 0.0009868617 ))// &  ! NOTE: 0.0009868617kN/m     = 0.001*  Mass*NaturalFrequency^2            = 0.001*  (9.999E-4kg)*(5Hz)^2     = 0.001*  (9.999E-4kg)*(31.41593rad/s)^2
                                                      '*VARVAL('//TRIM(Int2LStr( TmpID + 6000 ))//')'// &
                                                  ' - '//TRIM(Flt2LStr( 0.0000439779 ))// &  ! NOTE: 0.0000439779kN/(m/s) = 0.001*2*Mass*NaturalFrequency*DampingRatio = 0.001*2*(9.999E-4kg)*(5Hz)*(0.7) = 0.001*2*(9.999E-4kg)*(31.41593rad/s)*(0.7)
                                                      '*VZ('//TRIM(Int2LStr( TmpID ))//',0,10,0)'

         ENDDO             ! J - All the nodes per line for mooring line GRAPHICS


   ! Mooring line GRAPHICS:

         WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''Line', I, '_G'''
         WRITE (UnAD,FmtText     )  'GRAPHICS/'//TRIM(Int2LStr(    100000*I + 700     ))
         WRITE (UnAD,FmtText     )  ', OUTLINE = '//TRIM(Int2LStr(            700 + I ))//','
         DO J = 1,NLnNodes ! Loop through all the nodes per line for mooring line GRAPHICS
            WRITE (UnAD,FmtText  )  ', '          //TRIM(Int2LStr( 100000*I + 700 + J ))//','
         ENDDO             ! J - All the nodes per line for mooring line GRAPHICS
         WRITE (UnAD,FmtText     )  ', '          //TRIM(Int2LStr(           1700 + I ))


      ENDDO             ! I - All mooring lines

   ENDIF
ENDIF



!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

   ! Those on the support platform:

WRITE (UnAD,FmtText  )  '!------------------------------ Support Platform -------------------------------'


   ! Platform:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''Platform_P'''
WRITE (UnAD,FmtText  )  'PART/1000'
WRITE (UnAD,FmtTRTRTR)  ', QG = ', PtfmSurge        , ', ', PtfmSway        , ', ', PtfmHeave         ! Orient the
WRITE (UnAD,FmtTRTRTR)  ', ZG = ', PtfmSurge + a2(1), ', ', PtfmSway - a2(3), ', ', PtfmHeave + a2(2) ! platform using the
WRITE (UnAD,FmtTRTRTR)  ', XG = ', PtfmSurge + a1(1), ', ', PtfmSway - a1(3), ', ', PtfmHeave + a1(2) ! 3-point method
WRITE (UnAD,FmtText  )  ', CM = 1005'
WRITE (UnAD,FmtTR    )  ', MASS = ', PtfmMass + SmllNmbr
WRITE (UnAD,FmtTRTRTR)  ', IP = ', PtfmRIner + SmllNmbr, ', ', PtfmPIner + SmllNmbr, ', ', PtfmYIner + SmllNmbr


   ! Reference axis for platform:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''PlatformRef_M'''
WRITE (UnAD,FmtText  )  'MARKER/1000'
WRITE (UnAD,FmtText  )  ', PART = 1000'


   ! Platform center of mass:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''PlatformCM_M'''
WRITE (UnAD,FmtText  )  'MARKER/1005'
WRITE (UnAD,FmtText  )  ', PART = 1000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', 0.0, ', ', 0.0, ', ', rZYzt
WRITE (UnAD,FmtText  )  ', REULER = 0D, 0D, 0D'


   ! Undeflected tower top location:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''UndeflTwrTop_M'''
WRITE (UnAD,FmtText  )  'MARKER/1030'
WRITE (UnAD,FmtText  )  ', PART = 1000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', 0.0, ', ', 0.0, ', ', RefTwrHt
WRITE (UnAD,FmtText  )  ', REULER = 0D, 0D, 0D'

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
IF ( ( PtfmModel == 3 ) .AND. CompHydro )  THEN ! .TRUE. if we have floating offshore turbine and we are using the undocumented platform features.

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
   IF ( LineMod == 1 )  THEN  ! .TRUE if we have standard quasi-static mooring lines; store the mooring line data into the ARRAY

   ! Fairleads:

      DO I = 1,NumLines ! Loop through all mooring lines
         WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''Fairlead', I, '_M'''
         WRITE (UnAD,FmtText     )  'MARKER/'//TRIM(Int2LStr( 1700 + I ))
         WRITE (UnAD,FmtText     )  ', PART = 1000'
         WRITE (UnAD,FmtTRTRTR   )  ', QP = ', LFairxt(I), ', ', LFairyt(I), ', ', LFairzt(I)   ! NOTE: PtfmRef = 0.0 in this equation
         WRITE (UnAD,FmtText     )  ', REULER = 0D, 0D, 0D'
      ENDDO             ! I - All mooring lines


   ENDIF


!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
   ! Platform graphics:

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''Platform_G'''
   WRITE (UnAD,FmtText  )  'GRAPHICS/1000'
   WRITE (UnAD,FmtText  )  ', CYLINDER'
   WRITE (UnAD,FmtText  )  ', CM = 1100'
   WRITE (UnAD,FmtTR    )  ', LENGTH = ', TwrDraft - PtfmDraft ! NOTE: TwrDraft <= 0.0 in this equation
   WRITE (UnAD,FmtText  )  ', SIDES = '//TRIM(Int2LStr( NSides ))
   WRITE (UnAD,FmtTR    )  ', RADIUS = ', 0.5*PtfmDiam
   WRITE (UnAD,FmtText  )  ', SEG = '//TRIM(Int2LStr( NSides ))
!JASON:THIS CHANGED FOR ITI BARGE:   WRITE (UnAD,FmtText  )  ', BOX'           !JASON:THIS CHANGED FOR ITI BARGE:
!JASON:THIS CHANGED FOR ITI BARGE:   WRITE (UnAD,FmtText  )  ', CORNER = 1705' !JASON:THIS CHANGED FOR ITI BARGE:
!JASON:THIS CHANGED FOR ITI BARGE:   WRITE (UnAD,FmtText  )  ', X = 40'        !JASON:THIS CHANGED FOR ITI BARGE:
!JASON:THIS CHANGED FOR ITI BARGE:   WRITE (UnAD,FmtText  )  ', Y = 40'        !JASON:THIS CHANGED FOR ITI BARGE:
!JASON:THIS CHANGED FOR ITI BARGE:   WRITE (UnAD,FmtText  )  ', Z = 10'        !JASON:THIS CHANGED FOR ITI BARGE:


ENDIF


!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
   ! Those on the tower:

WRITE (UnAD,FmtText  )  '!------------------------------------ Tower ------------------------------------'


   ! Tower-base coordinate system:

WRITE (UnAD,FmtText  )  '!------------------------------ Tower: Tower Base ------------------------------'
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TowerBaseCS_M'''
WRITE (UnAD,FmtText  )  'MARKER/1100'  ! MARKER/1100 is equivalent to the tower-base coordinate system: X = Xt, Y = Yt, Z = Zt
WRITE (UnAD,FmtText  )  ', PART = 1000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', 0.0, ', ', 0.0, ', ', PtfmRef - TwrDraft
WRITE (UnAD,FmtText  )  ', REULER = 0D, 0D, 0D'


   ! Tower base (rigid portion of tower):

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TwrBaseToTwrSec01_M'''
WRITE (UnAD,FmtText  )  'MARKER/1400'
WRITE (UnAD,FmtText  )  ', PART = 1000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', 0.0, ', ', 0.0, ', ', rZT0zt
WRITE (UnAD,FmtText  )  ', REULER = 90D, 90D, 90D'


   ! Tower base graphics:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TowerBase_G'''
WRITE (UnAD,FmtText  )  'GRAPHICS/1400'
WRITE (UnAD,FmtText  )  ', CYLINDER'
WRITE (UnAD,FmtText  )  ', CM = 1100'
WRITE (UnAD,FmtTR    )  ', LENGTH = ', TwrRBHt
WRITE (UnAD,FmtText  )  ', SIDES = '//TRIM(Int2LStr( NSides ))
WRITE (UnAD,FmtTR    )  ', RADIUS = ', TwrBaseRad
WRITE (UnAD,FmtText  )  ', SEG = '//TRIM(Int2LStr( NSides ))


   ! Tower segments:

DO J = 1,TwrNodes ! Loop through the tower nodes/elements


   WRITE (UnAD,'(A,I2.2,A)')  '!--------------------------- Tower: Tower Section ', J, ' ---------------------------'


   ! PART and elastic axis:

   TmpID = 1100 + J
   ThnBarI = MassT(J)*( DHNodes(J)**3 )/12.0 ! Define the transverse inertias of the tower element (both identical) to be that of a thin uniform bar.
   TmpVec  = PtfmSurge*z1 + PtfmHeave*z2 - PtfmSway *z3 + ( rZT0zt + HNodes(J) )*a2 ! rT = Position vector from ground to current tower node (point T(J))
   TmpVec1 = TmpVec + t1(J,:)
   TmpVec2 = TmpVec + t2(J,:)
   WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''TowerSec', J, '_P'''
   WRITE (UnAD,FmtText     )  'PART/'//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtTRTRTR   )  ', QG = ', TmpVec (1), ', ', -TmpVec (3), ', ', TmpVec (2)  ! Orient the tower
   WRITE (UnAD,FmtTRTRTR   )  ', ZG = ', TmpVec1(1), ', ', -TmpVec1(3), ', ', TmpVec1(2)  ! section using the
   WRITE (UnAD,FmtTRTRTR   )  ', XG = ', TmpVec2(1), ', ', -TmpVec2(3), ', ', TmpVec2(2)  ! 3-point method
   WRITE (UnAD,FmtText     )  ', CM = '//TRIM(Int2LStr( 1500 + J ))
   WRITE (UnAD,FmtTR       )  ', MASS = ', MassT(J)*DHNodes(J) + SmllNmbr
   WRITE (UnAD,FmtTRTRTR   )  ', IP = ', ( ( InerTFA(J) + InerTSS(J) )*DHNodes(J) ) + SmllNmbr, ', ', &
                              ( InerTFA(J)*DHNodes(J) ) + ThnBarI + SmllNmbr, ', ', ( InerTSS(J)*DHNodes(J) ) + ThnBarI + SmllNmbr

   WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''TowerSec', J, 'Elastic_M'''
   WRITE (UnAD,FmtText     )  'MARKER/'//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtText     )  ', PART = '//TRIM(Int2LStr( TmpID ))


!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:
   ! Aerodynamic and hydrodynamic MARKERs:

   TmpID2 = 1800 + J
   WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''TowerSec', J, 'AeroHydro_M'''
   WRITE (UnAD,FmtText     )  'MARKER/'//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText     )  ', PART = '//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtText     )  ', QP = 0, 0, 0'
   WRITE (UnAD,FmtText     )  ', REULER = 90D, 90D, 90D'


!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
   ! Graphics:

   TmpID2 = 1200 + J
   WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''TowerSec', J, 'Graphics_M'''
   WRITE (UnAD,FmtText     )  'MARKER/'//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText     )  ', PART = '//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtTRTRTR   )  ', QP = ', -0.5*DHNodes(J), ', ', 0.0, ', ', 0.0
   WRITE (UnAD,FmtText     )  ', REULER = 90D, 90D, 90D'

   WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''TowerSec', J, '_G'''
   WRITE (UnAD,FmtText     )  'GRAPHICS/'//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText     )  ', CYLINDER'
   WRITE (UnAD,FmtText     )  ', CM = '//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtTR       )  ', LENGTH = ', DHNodes(J)
   WRITE (UnAD,FmtText     )  ', SIDES = '//TRIM(Int2LStr( NSides ))
   WRITE (UnAD,FmtTR       )  ', RADIUS = ', TwrBaseRad + ( TwrTopRad - TwrBaseRad )*HNodesNorm(J) ! Linearly interpolate the tower base and tower top radii.
   WRITE (UnAD,FmtText     )  ', SEG = '//TRIM(Int2LStr( NSides ))


   ! Bottom (for connection to the segment below if rigid):

   TmpID2 = 1300 + J
   WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''TwrSec', J, 'ToTwrSecBelow_M'''
   WRITE (UnAD,FmtText     )  'MARKER/'//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText     )  ', PART = '//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtTRTRTR   )  ', QP = ', -0.5*DHNodes(J), ', ', 0.0, ', ', 0.0
   WRITE (UnAD,FmtText     )  ', REULER = 0D, 0D, 0D'


   ! Top (for connection to the segment above if rigid):

   TmpID2 = 1400 + J
   WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''TwrSec', J, 'ToTwrSecAbove_M'''
   WRITE (UnAD,FmtText     )  'MARKER/'//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText     )  ', PART = '//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtTRTRTR   )  ', QP = ', 0.5*DHNodes(J), ', ', 0.0, ', ', 0.0
   WRITE (UnAD,FmtText     )  ', REULER = 0D, 0D, 0D'


   ! Center of Mass:

   TmpID2 = 1500 + J
   WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''TowerSec', J, 'CM_M'''
   WRITE (UnAD,FmtText     )  'MARKER/'//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText     )  ', PART = '//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtTRTRTR   )  ', QP = ', 0.0, ', ', -cgOffTSS(J), ', ', cgOffTFA(J)
   WRITE (UnAD,FmtText     )  ', REULER = 0D, 0D, 0D'


ENDDO             ! J - Tower nodes/elements


   ! Tower-top:

TmpVec  = PtfmSurge*z1 + PtfmHeave*z2 - PtfmSway *z3 + RefTwrHt*a2   ! rO = Position vector from ground to tower-top / base plate (point O)
TmpVec1 = TmpVec + b2
TmpVec2 = TmpVec + b1
WRITE (UnAD,FmtText  )  '!------------------------------ Tower: Tower-Top -------------------------------'
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TowerTop_P'''
WRITE (UnAD,FmtText  )  'PART/1600'
WRITE (UnAD,FmtTRTRTR)  ', QG = ', TmpVec (1), ', ', -TmpVec (3), ', ', TmpVec (2)  ! Orient the tower
WRITE (UnAD,FmtTRTRTR)  ', ZG = ', TmpVec1(1), ', ', -TmpVec1(3), ', ', TmpVec1(2)  ! top using the
WRITE (UnAD,FmtTRTRTR)  ', XG = ', TmpVec2(1), ', ', -TmpVec2(3), ', ', TmpVec2(2)  ! 3-point method
WRITE (UnAD,FmtText  )  ', CM = 1600'
WRITE (UnAD,FmtTR    )  ', MASS = ', YawBrMass + SmllNmbr
WRITE (UnAD,FmtTRTRTR)  ', IP = ', SmllNmbr, ', ', SmllNmbr, ', ', SmllNmbr


   ! Tower-top center of mass - tower-top/base plate coordinate system:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TowerTopBasePlateCS_M'''
WRITE (UnAD,FmtText  )  'MARKER/1600'  ! MARKER/1600 is equivalent to the tower-top/base-plate coordinate system: X = Xp, Y = Yp, Z = Zp
WRITE (UnAD,FmtText  )  ', PART = 1600'


   ! Connection to highest tower segment:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TowerTopToTwrSecBelow_M'''
WRITE (UnAD,FmtText  )  'MARKER/1500'
WRITE (UnAD,FmtText  )  ', PART = 1600'
WRITE (UnAD,FmtText  )  ', QP = 0, 0, 0'
WRITE (UnAD,FmtText  )  ', REULER = 90D, 90D, 90D'


   ! Yaw bearing attached to tower top:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''YawBrBottom_M'''
WRITE (UnAD,FmtText  )  'MARKER/1010'
WRITE (UnAD,FmtText  )  ', PART = 1600'
WRITE (UnAD,FmtText  )  ', QP = 0, 0, 0'
WRITE (UnAD,FmtText  )  ', REULER = 0D, 180D, 0D'



   ! Those on the Nacelle:

WRITE (UnAD,FmtText  )  '!----------------------------------- Nacelle -----------------------------------'


   ! Nacelle:

TmpVec  = PtfmSurge*z1 + PtfmHeave*z2 - PtfmSway *z3 + RefTwrHt*a2   ! rO = Position vector from ground to tower-top / base plate (point O)
TmpVec1 = TmpVec + d2
TmpVec2 = TmpVec + d1
!bjj start of proposed change v6.02d-bjj: fix spelling
!rmWRITE (UnAD,FmtText  )  '!                             adams_view_name=''Nacalle_P''' !bjj: spelling???
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''Nacelle_P'''
!bjj end of proposed change
WRITE (UnAD,FmtText  )  'PART/2000'
WRITE (UnAD,FmtTRTRTR)  ', QG = ', TmpVec (1), ', ', -TmpVec (3), ', ', TmpVec (2)  ! Orient the
WRITE (UnAD,FmtTRTRTR)  ', ZG = ', TmpVec1(1), ', ', -TmpVec1(3), ', ', TmpVec1(2)  ! nacelle using the
WRITE (UnAD,FmtTRTRTR)  ', XG = ', TmpVec2(1), ', ', -TmpVec2(3), ', ', TmpVec2(2)  ! 3-point method
WRITE (UnAD,FmtText  )  ', CM = 2005'
WRITE (UnAD,FmtTR    )  ', MASS = ', NacMass + SmllNmbr
WRITE (UnAD,FmtTRTRTR)  ', IP = ', SmllNmbr, ', ', SmllNmbr, ', ', Nacd2Iner + SmllNmbr


   ! Nacelle coordinate system (fixed in nacelle):

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''NacelleCS_M'''
WRITE (UnAD,FmtText  )  'MARKER/2000'  ! MARKER/2000 is equivalent to the nacelle coordinate system: X = Xn, Y = Yn, Z = Zn
WRITE (UnAD,FmtText  )  ', PART = 2000'


   ! Nacelle center of mass:

!bjj start of proposed change - spelling Nacelle correctly
!rmWRITE (UnAD,FmtText  )  '!                             adams_view_name=''NacalleCM_M'''
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''NacelleCM_M'''
!bjj end of proposed change
WRITE (UnAD,FmtText  )  'MARKER/2005'
WRITE (UnAD,FmtText  )  ', PART = 2000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', NacCMxn, ', ', NacCMyn, ', ', NacCMzn
WRITE (UnAD,FmtText  )  ', REULER = 0, 0, 0'


   ! Yaw bearing attached to bed plate / nacelle:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''YawBrTop_M'''
WRITE (UnAD,FmtText  )  'MARKER/2010'
WRITE (UnAD,FmtText  )  ', PART = 2000'
WRITE (UnAD,FmtText  )  ', QP = 0, 0, 0'
WRITE (UnAD,FmtText  )  ', REULER = 0D, 180D, 0D'


   ! Rotor-furl bearing attached to nacelle:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''RFrlBrBottom_M'''
WRITE (UnAD,FmtText  )  'MARKER/2030'
WRITE (UnAD,FmtText  )  ', PART = 2000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', RFrlPntxn                    , ', ', RFrlPntyn                    , ', ', RFrlPntzn           ! Orient the rotor-furl bearing sing the 3-point method
WRITE (UnAD,FmtTRTRTR)  ', ZP = ', RFrlPntxn+CRFrlSkew*CRFrlTilt, ', ', RFrlPntyn+SRFrlSkew*CRFrlTilt, ', ', RFrlPntzn+SRFrlTilt ! NOTE: the z-axis is the rotor-furl axis
WRITE (UnAD,FmtTRTRTR)  ', XP = ', RFrlPntxn-CRFrlSkew*SRFrlTilt, ', ', RFrlPntyn-SRFrlSkew*SRFrlTilt, ', ', RFrlPntzn+CRFrlTilt ! NOTE: the x-axis is oriented using the same technique the z-axis, but uses RFrlSkew and (RFrlTilt + 90 deg)


   ! Tail-furl bearing attached to nacelle:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TFrlBrBottom_M'''
WRITE (UnAD,FmtText  )  'MARKER/2040'
WRITE (UnAD,FmtText  )  ', PART = 2000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', TFrlPntxn                    , ', ', TFrlPntyn                    , ', ', TFrlPntzn           ! Orient the rotor-furl bearing sing the 3-point method
WRITE (UnAD,FmtTRTRTR)  ', ZP = ', TFrlPntxn+CTFrlSkew*CTFrlTilt, ', ', TFrlPntyn+STFrlSkew*CTFrlTilt, ', ', TFrlPntzn+STFrlTilt ! NOTE: the z-axis is the rotor-furl axis
WRITE (UnAD,FmtTRTRTR)  ', XP = ', TFrlPntxn-CTFrlSkew*STFrlTilt, ', ', TFrlPntyn-STFrlSkew*STFrlTilt, ', ', TFrlPntzn+CTFrlTilt ! NOTE: the x-axis is oriented using the same technique the z-axis, but uses RFrlSkew and (RFrlTilt + 90 deg)


   ! Nacelle graphics:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''NacelleGraphics_M'''
WRITE (UnAD,FmtText  )  'MARKER/2020'
WRITE (UnAD,FmtText  )  ', PART = 2000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', -0.5*NacLength, ', ', Yaw2Shft, ', ', Twr2Shft
WRITE (UnAD,FmtText  )  ', REULER = 90D, 90D, -90D'

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''Nacelle_G'''
WRITE (UnAD,FmtText  )  'GRAPHICS/2000'
WRITE (UnAD,FmtText  )  ', FRUSTUM'
WRITE (UnAD,FmtText  )  ', CM = 2020'
WRITE (UnAD,FmtTR    )  ', LENGTH = ', NacLength
WRITE (UnAD,FmtText  )  ', SIDES = '//TRIM(Int2LStr( NSides ))
WRITE (UnAD,FmtTR    )  ', TOP = ', NacRadTop
WRITE (UnAD,FmtTR    )  ', BOTTOM = ', NacRadBot
WRITE (UnAD,FmtText  )  ', SEG = '//TRIM(Int2LStr( NSides ))

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''Twr2Shft_G'''
WRITE (UnAD,FmtText  )  'GRAPHICS/2010'
WRITE (UnAD,FmtText  )  ', FRUSTUM'
WRITE (UnAD,FmtText  )  ', CM = 2010'
WRITE (UnAD,FmtTR    )  ', LENGTH = ', -Twr2Shft
WRITE (UnAD,FmtText  )  ', SIDES = '//TRIM(Int2LStr( NSides ))
WRITE (UnAD,FmtTR    )  ', TOP = ', 0.5*TwrTopRad
WRITE (UnAD,FmtTR    )  ', BOTTOM = ',  TwrTopRad
WRITE (UnAD,FmtText  )  ', SEG = '//TRIM(Int2LStr( NSides ))



   ! Those on the Tail:

WRITE (UnAD,FmtText  )  '!------------------------------------ Tail -------------------------------------'


   ! Tail boom:

TmpVec  = PtfmSurge*z1 + PtfmHeave*z2 - PtfmSway *z3 + RefTwrHt*a2 &
        + TFrlPntxn*d1 + TFrlPntzn*d2 - TFrlPntyn*d3                 ! rW = Position vector from ground to specified point on tail-furl axis (point W)
TmpVec1 = TmpVec + tf2
TmpVec2 = TmpVec + tf1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailBoom_P'''
WRITE (UnAD,FmtText  )  'PART/5000'
WRITE (UnAD,FmtTRTRTR)  ', QG = ', TmpVec (1), ', ', -TmpVec (3), ', ', TmpVec (2)  ! Orient the tail
WRITE (UnAD,FmtTRTRTR)  ', ZG = ', TmpVec1(1), ', ', -TmpVec1(3), ', ', TmpVec1(2)  ! boom using the
WRITE (UnAD,FmtTRTRTR)  ', XG = ', TmpVec2(1), ', ', -TmpVec2(3), ', ', TmpVec2(2)  ! 3-point method
WRITE (UnAD,FmtText  )  ', CM = 5005'
WRITE (UnAD,FmtTR    )  ', MASS = ', BoomMass + SmllNmbr
WRITE (UnAD,FmtTRTRTR)  ', IP = ', SmllNmbr, ', ', SmllNmbr, ', ', AtfaIner + SmllNmbr


   ! Reference axis for the tail boom:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailBoomRef_M'''
WRITE (UnAD,FmtText  )  'MARKER/5000'
WRITE (UnAD,FmtText  )  ', PART = 5000'


   ! Center of mass of the tail boom:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailBoomCM_M'''
WRITE (UnAD,FmtText  )  'MARKER/5005'
WRITE (UnAD,FmtText  )  ', PART = 5000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', rWIxn                      , ', ', rWIyn                      , ', ', rWIzn             ! Orient the tail-furl center-of-mass marker using the 3-point method
WRITE (UnAD,FmtTRTRTR)  ', ZP = ', rWIxn + CTFrlSkew*CTFrlTilt, ', ', rWIyn + STFrlSkew*CTFrlTilt, ', ', rWIzn + STFrlTilt ! NOTE: the z-axis is parallel to the tail-furl axis
WRITE (UnAD,FmtTRTRTR)  ', XP = ', rWIxn - CTFrlSkew*STFrlTilt, ', ', rWIyn - STFrlSkew*STFrlTilt, ', ', rWIzn + CTFrlTilt ! NOTE: the x-axis is oriented using the same technique the z-axis, but uses TFrlSkew and (TFrlTilt + 90 deg)


   ! Rotor-furl bearing attached to nacelle:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TFrlBrTop_M'''
WRITE (UnAD,FmtText  )  'MARKER/5040'
WRITE (UnAD,FmtText  )  ', PART = 5000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', 0.0                 , ', ', 0.0                 , ', ', 0.0        ! Orient the tail-furl bearing sing the 3-point method
WRITE (UnAD,FmtTRTRTR)  ', ZP = ',  CTFrlSkew*CTFrlTilt, ', ',  STFrlSkew*CTFrlTilt, ', ', STFrlTilt  ! NOTE: the z-axis is the tail-furl axis
WRITE (UnAD,FmtTRTRTR)  ', XP = ', -CTFrlSkew*STFrlTilt, ', ', -STFrlSkew*STFrlTilt, ', ', CTFrlTilt  ! NOTE: the x-axis is oriented using the same technique the z-axis, but uses TFrlSkew and (TFrlTilt + 90 deg)


   ! Tail boom graphics:

TmpVec1 = rWKxn*tf1 + rWKzn*tf2 - rWKyn*tf3 - 0.25*SQRTTFinA*p2                           ! z-axis along the boom - directed from the specified point on the tail-furl axis to a point just below the tail fin CM and just downwind (along p1) of TailFinOutline5_M
TmpVec2 = tfa                                                                             ! This is chosen somewhat arbitrarily (only the z-axis matters) - I chose tfa since it is very unlikely that the the boom-axis will lie along the tail-furl axis
TmpLength = SQRT( TmpVec1(1)*TmpVec1(1) + TmpVec1(2)*TmpVec1(2) + TmpVec1(3)*TmpVec1(3) ) ! Length of tail boom
IF ( TmpLength /= 0.0 )  THEN                                                             ! Only add the tail boom graphics if the specified point on the z-axis (via TmpVec1) is different from the origin.
   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailBoomGraphics_M'''
   WRITE (UnAD,FmtText  )  'MARKER/5020'
   WRITE (UnAD,FmtText  )  ', PART = 5000'
   WRITE (UnAD,FmtTRTRTR)  ', QP = ', 0.0                    , ', ',  0.0                    , ', ', 0.0
!bjj: I had to make continuation lines since DOT_PRODUCT is longer than DotProd:   
   WRITE (UnAD,FmtTRTRTR)  ', ZP = ', DOT_PRODUCT( TmpVec1, tf1 ), ', ', -DOT_PRODUCT( TmpVec1, tf3 ), ', ', & 
                                      DOT_PRODUCT( TmpVec1, tf2 )
   WRITE (UnAD,FmtTRTRTR)  ', XP = ', DOT_PRODUCT( TmpVec2, tf1 ), ', ', -DOT_PRODUCT( TmpVec2, tf3 ), ', ', &
                                      DOT_PRODUCT( TmpVec2, tf2 )

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailBoom_G'''
   WRITE (UnAD,FmtText  )  'GRAPHICS/5000'
   WRITE (UnAD,FmtText  )  ', CYLINDER'
   WRITE (UnAD,FmtText  )  ', CM = 5020'
   WRITE (UnAD,FmtTR    )  ', LENGTH = ', TmpLength
   WRITE (UnAD,FmtText  )  ', SIDES = '//TRIM(Int2LStr( NSides ))
   WRITE (UnAD,FmtTR    )  ', RADIUS = ', BoomRad
   WRITE (UnAD,FmtText  )  ', SEG = '//TRIM(Int2LStr( NSides ))
ENDIF


   ! Tail fin:

TmpVec  = PtfmSurge*z1 + PtfmHeave*z2 - PtfmSway *z3 + RefTwrHt*a2 &
        + TFrlPntxn*d1 + TFrlPntzn*d2 - TFrlPntyn*d3                 ! rW = Position vector from ground to specified point on tail-furl axis (point W)
TmpVec1 = TmpVec + tf2
TmpVec2 = TmpVec + tf1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailFin_P'''
WRITE (UnAD,FmtText  )  'PART/5100'
WRITE (UnAD,FmtTRTRTR)  ', QG = ', TmpVec (1), ', ', -TmpVec (3), ', ', TmpVec (2)  ! Orient the tail
WRITE (UnAD,FmtTRTRTR)  ', ZG = ', TmpVec1(1), ', ', -TmpVec1(3), ', ', TmpVec1(2)  ! fin using the
WRITE (UnAD,FmtTRTRTR)  ', XG = ', TmpVec2(1), ', ', -TmpVec2(3), ', ', TmpVec2(2)  ! 3-point method
WRITE (UnAD,FmtText  )  ', CM = 5105'
WRITE (UnAD,FmtTR    )  ', MASS = ', TFinMass + SmllNmbr
WRITE (UnAD,FmtTRTRTR)  ', IP = ', SmllNmbr, ', ', SmllNmbr, ', ', SmllNmbr


   ! Reference axis for the tail fin:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailFinRef_M'''
WRITE (UnAD,FmtText  )  'MARKER/5100'
WRITE (UnAD,FmtText  )  ', PART = 5100'


   ! Center of mass of the tail fin:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailFinCM_M'''
WRITE (UnAD,FmtText  )  'MARKER/5105'
WRITE (UnAD,FmtText  )  ', PART = 5100'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', rWJxn, ', ', rWJyn, ', ', rWJzn
WRITE (UnAD,FmtText  )  ', REULER = 0, 0, 0'


   ! Tail fin center of pressure / tail fin coordinate system:

TmpVec  = rWKxn*tf1 + rWKzn*tf2 - rWKyn*tf3  ! rWK = Position vector from specified point on tail-furl axis (point W) to tail fin center-of-pressure (point K)
TmpVec1 = TmpVec + p2
TmpVec2 = TmpVec + p1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailFinAero_M'''
WRITE (UnAD,FmtText  )  'MARKER/5110'  ! MARKER/5110 is equivalent to the tail fin coordinate system: X = tail fin x, Y = tail fin y, Z = tail fin z
WRITE (UnAD,FmtText  )  ', PART = 5100'
!bjj: I had to make continuation lines since DOT_PRODUCT is longer than DotProd:   
WRITE (UnAD,FmtTRTRTR)  ', QP = ', DOT_PRODUCT( TmpVec , tf1 ), ', ', -DOT_PRODUCT( TmpVec , tf3 ), ', ', &
                                   DOT_PRODUCT( TmpVec , tf2 )  ! Orient the tail
WRITE (UnAD,FmtTRTRTR)  ', ZP = ', DOT_PRODUCT( TmpVec1, tf1 ), ', ', -DOT_PRODUCT( TmpVec1, tf3 ), ', ', &
                                   DOT_PRODUCT( TmpVec1, tf2 )  ! fin CS using the
WRITE (UnAD,FmtTRTRTR)  ', XP = ', DOT_PRODUCT( TmpVec2, tf1 ), ', ', -DOT_PRODUCT( TmpVec2, tf3 ), ', ', &
                                   DOT_PRODUCT( TmpVec2, tf2 )  ! 3-point method


   ! Tail fin graphics:

TmpVec  = rWKxn*tf1 + rWKzn*tf2 - rWKyn*tf3 - 0.75*SQRTTFinA*p2
TmpVec1 = TmpVec + p2
TmpVec2 = TmpVec + p1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailFinOutline1_M'''
WRITE (UnAD,FmtText  )  'MARKER/5121'
WRITE (UnAD,FmtText  )  ', PART = 5100'
!bjj: I had to make continuation lines since DOT_PRODUCT is longer than DotProd:   
WRITE (UnAD,FmtTRTRTR)  ', QP = ', DOT_PRODUCT( TmpVec , tf1 ), ', ', -DOT_PRODUCT( TmpVec , tf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec , tf2 )
WRITE (UnAD,FmtTRTRTR)  ', ZP = ', DOT_PRODUCT( TmpVec1, tf1 ), ', ', -DOT_PRODUCT( TmpVec1, tf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec1, tf2 )
WRITE (UnAD,FmtTRTRTR)  ', XP = ', DOT_PRODUCT( TmpVec2, tf1 ), ', ', -DOT_PRODUCT( TmpVec2, tf3 ), ', ', &    
                                   DOT_PRODUCT( TmpVec2, tf2 )

TmpVec  = rWKxn*tf1 + rWKzn*tf2 - rWKyn*tf3 + 0.4166667*SQRTTFinA*p1 - 0.75*SQRTTFinA*p2
TmpVec1 = TmpVec + p2
TmpVec2 = TmpVec + p1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailFinOutline2_M'''
WRITE (UnAD,FmtText  )  'MARKER/5122'
WRITE (UnAD,FmtText  )  ', PART = 5100'
!bjj: I had to make continuation lines since DOT_PRODUCT is longer than DotProd:   
WRITE (UnAD,FmtTRTRTR)  ', QP = ', DOT_PRODUCT( TmpVec , tf1 ), ', ', -DOT_PRODUCT( TmpVec , tf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec , tf2 )
WRITE (UnAD,FmtTRTRTR)  ', ZP = ', DOT_PRODUCT( TmpVec1, tf1 ), ', ', -DOT_PRODUCT( TmpVec1, tf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec1, tf2 )
WRITE (UnAD,FmtTRTRTR)  ', XP = ', DOT_PRODUCT( TmpVec2, tf1 ), ', ', -DOT_PRODUCT( TmpVec2, tf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec2, tf2 )

TmpVec  = rWKxn*tf1 + rWKzn*tf2 - rWKyn*tf3 + 0.4166667*SQRTTFinA*p1 + 0.75*SQRTTFinA*p2
TmpVec1 = TmpVec + p2
TmpVec2 = TmpVec + p1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailFinOutline3_M'''
WRITE (UnAD,FmtText  )  'MARKER/5123'
WRITE (UnAD,FmtText  )  ', PART = 5100'
!bjj: I had to make continuation lines since DOT_PRODUCT is longer than DotProd:   
WRITE (UnAD,FmtTRTRTR)  ', QP = ', DOT_PRODUCT( TmpVec , tf1 ), ', ', -DOT_PRODUCT( TmpVec , tf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec , tf2 )
WRITE (UnAD,FmtTRTRTR)  ', ZP = ', DOT_PRODUCT( TmpVec1, tf1 ), ', ', -DOT_PRODUCT( TmpVec1, tf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec1, tf2 )
WRITE (UnAD,FmtTRTRTR)  ', XP = ', DOT_PRODUCT( TmpVec2, tf1 ), ', ', -DOT_PRODUCT( TmpVec2, tf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec2, tf2 )

TmpVec  = rWKxn*tf1 + rWKzn*tf2 - rWKyn*tf3 + 0.75*SQRTTFinA*p2
TmpVec1 = TmpVec + p2
TmpVec2 = TmpVec + p1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailFinOutline4_M'''
WRITE (UnAD,FmtText  )  'MARKER/5124'
WRITE (UnAD,FmtText  )  ', PART = 5100'
!bjj: I had to make continuation lines since DOT_PRODUCT is longer than DotProd:   
WRITE (UnAD,FmtTRTRTR)  ', QP = ', DOT_PRODUCT( TmpVec , tf1 ), ', ', -DOT_PRODUCT( TmpVec , tf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec , tf2 )
WRITE (UnAD,FmtTRTRTR)  ', ZP = ', DOT_PRODUCT( TmpVec1, tf1 ), ', ', -DOT_PRODUCT( TmpVec1, tf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec1, tf2 )
WRITE (UnAD,FmtTRTRTR)  ', XP = ', DOT_PRODUCT( TmpVec2, tf1 ), ', ', -DOT_PRODUCT( TmpVec2, tf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec2, tf2 )

TmpVec  = rWKxn*tf1 + rWKzn*tf2 - rWKyn*tf3 - 0.5*SQRTTFinA*p1 - 0.25*SQRTTFinA*p2
TmpVec1 = TmpVec + p2
TmpVec2 = TmpVec + p1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailFinOutline5_M'''
WRITE (UnAD,FmtText  )  'MARKER/5125'
WRITE (UnAD,FmtText  )  ', PART = 5100'
!bjj: I had to make continuation lines since DOT_PRODUCT is longer than DotProd:   
WRITE (UnAD,FmtTRTRTR)  ', QP = ', DOT_PRODUCT( TmpVec , tf1 ), ', ', -DOT_PRODUCT( TmpVec , tf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec , tf2 )
WRITE (UnAD,FmtTRTRTR)  ', ZP = ', DOT_PRODUCT( TmpVec1, tf1 ), ', ', -DOT_PRODUCT( TmpVec1, tf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec1, tf2 )
WRITE (UnAD,FmtTRTRTR)  ', XP = ', DOT_PRODUCT( TmpVec2, tf1 ), ', ', -DOT_PRODUCT( TmpVec2, tf3 ), ', ', &
                                   DOT_PRODUCT( TmpVec2, tf2 )

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailFin_G'''
WRITE (UnAD,FmtText  )  'GRAPHICS/5100'
WRITE (UnAD,FmtText  )  ', OUTLINE = 5121, 5122, 5123, 5124, 5125, 5121'



   ! Those on the Structure Furling with the Rotor:

WRITE (UnAD,FmtText  )  '!--------------------- Structure that Furls with the Rotor ---------------------'


   ! Structure that furls with the rotor (not including rotor):

TmpVec  = PtfmSurge*z1 + PtfmHeave*z2 - PtfmSway *z3 + RefTwrHt*a2 &
        + RFrlPntxn*d1 + RFrlPntzn*d2 - RFrlPntyn*d3                 ! rV = Position vector from ground to specified point on rotor-furl axis (point V)
TmpVec1 = TmpVec + rf2
TmpVec2 = TmpVec + rf1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''RotorFurl_P'''
WRITE (UnAD,FmtText  )  'PART/2100'
WRITE (UnAD,FmtTRTRTR)  ', QG = ', TmpVec (1), ', ', -TmpVec (3), ', ', TmpVec (2)  ! Orient the structure that
WRITE (UnAD,FmtTRTRTR)  ', ZG = ', TmpVec1(1), ', ', -TmpVec1(3), ', ', TmpVec1(2)  ! furls with the rotor using
WRITE (UnAD,FmtTRTRTR)  ', XG = ', TmpVec2(1), ', ', -TmpVec2(3), ', ', TmpVec2(2)  ! the 3-point method
WRITE (UnAD,FmtText  )  ', CM = 2105'
WRITE (UnAD,FmtTR    )  ', MASS = ', RFrlMass + SmllNmbr
WRITE (UnAD,FmtTRTRTR)  ', IP = ', SmllNmbr, ', ', SmllNmbr, ', ', RrfaIner + SmllNmbr


   ! Reference axis for the structure that furls with the rotor:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''RotorFurlRef_M'''
WRITE (UnAD,FmtText  )  'MARKER/2100'
WRITE (UnAD,FmtText  )  ', PART = 2100'


   ! Center of mass of the structure that furls with the rotor (not including rotor):

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''RotorFurlCM_M'''
WRITE (UnAD,FmtText  )  'MARKER/2105'
WRITE (UnAD,FmtText  )  ', PART = 2100'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', rVDxn                      , ', ', rVDyn                      , ', ', rVDzn             ! Orient the rotor-furl center-of-mass marker using the 3-point method
WRITE (UnAD,FmtTRTRTR)  ', ZP = ', rVDxn + CRFrlSkew*CRFrlTilt, ', ', rVDyn + SRFrlSkew*CRFrlTilt, ', ', rVDzn + SRFrlTilt ! NOTE: the z-axis is parallel to the rotor-furl axis
WRITE (UnAD,FmtTRTRTR)  ', XP = ', rVDxn - CRFrlSkew*SRFrlTilt, ', ', rVDyn - SRFrlSkew*SRFrlTilt, ', ', rVDzn + CRFrlTilt ! NOTE: the x-axis is oriented using the same technique the z-axis, but uses RFrlSkew and (RFrlTilt + 90 deg)


   ! Rotor-furl bearing attached to nacelle:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''RFrlBrTop_M'''
WRITE (UnAD,FmtText  )  'MARKER/2130'
WRITE (UnAD,FmtText  )  ', PART = 2100'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', 0.0                 , ', ', 0.0                 , ', ', 0.0        ! Orient the rotor-furl bearing sing the 3-point method
WRITE (UnAD,FmtTRTRTR)  ', ZP = ',  CRFrlSkew*CRFrlTilt, ', ',  SRFrlSkew*CRFrlTilt, ', ', SRFrlTilt  ! NOTE: the z-axis is the rotor-furl axis
WRITE (UnAD,FmtTRTRTR)  ', XP = ', -CRFrlSkew*SRFrlTilt, ', ', -SRFrlSkew*SRFrlTilt, ', ', CRFrlTilt  ! NOTE: the x-axis is oriented using the same technique the z-axis, but uses RFrlSkew and (RFrlTilt + 90 deg)


   ! Nacelle IMU location (fixed in structure that furls with the rotor):

TmpVec  = rVIMUxn*rf1 + rVIMUzn*rf2 - rVIMUyn*rf3  ! = Position vector from specified point on rotor-furl axis (point V) to nacelle IMU  (point IMU)
TmpVec1 = TmpVec + c2
TmpVec2 = TmpVec + c1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''NacelleIMU_M'''
WRITE (UnAD,FmtText  )  'MARKER/2140'
WRITE (UnAD,FmtText  )  ', PART = 2100'
!bjj: I had to make continuation lines since DOT_PRODUCT is longer than DotProd:   
WRITE (UnAD,FmtTRTRTR)  ', QP = ', DOT_PRODUCT( TmpVec , rf1 ), ', ', -DOT_PRODUCT( TmpVec , rf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec , rf2 )  ! Orient the nacelle
WRITE (UnAD,FmtTRTRTR)  ', ZP = ', DOT_PRODUCT( TmpVec1, rf1 ), ', ', -DOT_PRODUCT( TmpVec1, rf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec1, rf2 )  ! IMU MARKER using
WRITE (UnAD,FmtTRTRTR)  ', XP = ', DOT_PRODUCT( TmpVec2, rf1 ), ', ', -DOT_PRODUCT( TmpVec2, rf3 ), ', ', & 
                                   DOT_PRODUCT( TmpVec2, rf2 )  ! the 3-point method


   ! Shaft coordinate system (fixed in structure that furls with the rotor):

TmpVec  = rVPxn*rf1 + rVPzn*rf2 - rVPyn*rf3  ! = Position vector from specified point on rotor-furl axis (point V) to origin of shaft CS
TmpVec1 = TmpVec + c2
TmpVec2 = TmpVec + c1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''ShaftCS_M'''
WRITE (UnAD,FmtText  )  'MARKER/2150'  ! MARKER/2150 is equivalent to the shaft coordinate system: X = Xs, Y = Ys, Z = Zs
WRITE (UnAD,FmtText  )  ', PART = 2100'
!bjj: I had to make continuation lines since DOT_PRODUCT is longer than DotProd:   
WRITE (UnAD,FmtTRTRTR)  ', QP = ', DOT_PRODUCT( TmpVec , rf1 ), ', ', -DOT_PRODUCT( TmpVec , rf3 ), ', ', &
                                   DOT_PRODUCT( TmpVec , rf2 )  ! Orient the shaft
WRITE (UnAD,FmtTRTRTR)  ', ZP = ', DOT_PRODUCT( TmpVec1, rf1 ), ', ', -DOT_PRODUCT( TmpVec1, rf3 ), ', ', &
                                   DOT_PRODUCT( TmpVec1, rf2 )  ! CS using the
WRITE (UnAD,FmtTRTRTR)  ', XP = ', DOT_PRODUCT( TmpVec2, rf1 ), ', ', -DOT_PRODUCT( TmpVec2, rf3 ), ', ', &
                                   DOT_PRODUCT( TmpVec2, rf2 )  ! 3-point method


   ! Nacelle/hub reference:

TmpVec  = rVPxn*rf1 + rVPzn*rf2 - rVPyn*rf3  ! = Position vector from specified point on rotor-furl axis (point V) to origin of shaft CS
TmpVec1 = TmpVec + c1
TmpVec2 = TmpVec - c2
!bjj start of proposed change - spelling Nacelle correctly
!rmWRITE (UnAD,FmtText  )  '!                             adams_view_name=''NacalleHubRef_M'''
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''NacelleHubRef_M'''
!bjj end of proposed change - spelling Nacelle correctly
WRITE (UnAD,FmtText  )  'MARKER/2050'
WRITE (UnAD,FmtText  )  ', PART = 2100'
!bjj: I had to make continuation lines since DOT_PRODUCT is longer than DotProd:   
WRITE (UnAD,FmtTRTRTR)  ', QP = ', DOT_PRODUCT( TmpVec , rf1 ), ', ', -DOT_PRODUCT( TmpVec , rf3 ), ', ', &
                                   DOT_PRODUCT( TmpVec , rf2 )  ! Orient the nacelle/hub
WRITE (UnAD,FmtTRTRTR)  ', ZP = ', DOT_PRODUCT( TmpVec1, rf1 ), ', ', -DOT_PRODUCT( TmpVec1, rf3 ), ', ', &
                                   DOT_PRODUCT( TmpVec1, rf2 )  ! reference MARKER using
WRITE (UnAD,FmtTRTRTR)  ', XP = ', DOT_PRODUCT( TmpVec2, rf1 ), ', ', -DOT_PRODUCT( TmpVec2, rf3 ), ', ', &
                                   DOT_PRODUCT( TmpVec2, rf2 )  ! the 3-point method


   ! Gearbox graphics:

IF ( GBoxLength > 0.0 )  THEN ! Only include these statements in the dataset if GBoxLength > 0.0:
   TmpVec  = rVPxn*rf1 + rVPzn*rf2 - rVPyn*rf3 + (OverHang-LSSLength-0.5*GBoxLength)*c1 - 0.5*GBoxLength*c2 + 0.5*GBoxLength*c3  ! = Position vector from specified point on rotor-furl axis (point V) to origin gearbox graphics MARKER
   TmpVec1 = TmpVec + c2
   TmpVec2 = TmpVec + c1
   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''GBoxGraphics_M'''
   WRITE (UnAD,FmtText  )  'MARKER/2120'
   WRITE (UnAD,FmtText  )  ', PART = 2100'
!bjj: I had to make continuation lines since DOT_PRODUCT is longer than DotProd:   
   WRITE (UnAD,FmtTRTRTR)  ', QP = ', DOT_PRODUCT( TmpVec , rf1 ), ', ', -DOT_PRODUCT( TmpVec , rf3 ), ', ', &
                                      DOT_PRODUCT( TmpVec , rf2 )  ! Orient the gearbox
   WRITE (UnAD,FmtTRTRTR)  ', ZP = ', DOT_PRODUCT( TmpVec1, rf1 ), ', ', -DOT_PRODUCT( TmpVec1, rf3 ), ', ', &
                                      DOT_PRODUCT( TmpVec1, rf2 )  ! graphics MARKER using
   WRITE (UnAD,FmtTRTRTR)  ', XP = ', DOT_PRODUCT( TmpVec2, rf1 ), ', ', -DOT_PRODUCT( TmpVec2, rf3 ), ', ', &
                                      DOT_PRODUCT( TmpVec2, rf2 )  ! the 3-point method

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''GBox_G'''
   WRITE (UnAD,FmtText  )  'GRAPHICS/2120'
   WRITE (UnAD,FmtText  )  ', BOX'
   WRITE (UnAD,FmtText  )  ', CORNER = 2120'
   WRITE (UnAD,FmtTR    )  ', X = ', GBoxLength
   WRITE (UnAD,FmtTR    )  ', Y = ', GBoxLength
   WRITE (UnAD,FmtTR    )  ', Z = ', GBoxLength
ENDIF



   ! Those on the shaft:

WRITE (UnAD,FmtText  )  '!------------------------------------ Shaft ------------------------------------'


   ! Low-speed shaft:

TmpVec  = PtfmSurge*z1 + PtfmHeave*z2 - PtfmSway *z3 + RefTwrHt*a2 &
        + RFrlPntxn*d1 + RFrlPntzn*d2 - RFrlPntyn*d3 + rVPxn*rf1 + rVPzn*rf2 - rVPyn*rf3  ! Position vector from ground to origin of shaft CS
TmpVec1 = TmpVec + e3
TmpVec2 = TmpVec + e1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''LSS_P'''
WRITE (UnAD,FmtText  )  'PART/3000'
WRITE (UnAD,FmtTRTRTR)  ', QG = ', TmpVec (1), ', ', -TmpVec (3), ', ', TmpVec (2)  ! Orient the
WRITE (UnAD,FmtTRTRTR)  ', ZG = ', TmpVec1(1), ', ', -TmpVec1(3), ', ', TmpVec1(2)  ! LSS using the
WRITE (UnAD,FmtTRTRTR)  ', XG = ', TmpVec2(1), ', ', -TmpVec2(3), ', ', TmpVec2(2)  ! 3-point method
WRITE (UnAD,FmtText  )  ', CM = 3005'
WRITE (UnAD,FmtTR    )  ', MASS = ', SmllNmbr
WRITE (UnAD,FmtTRTRTR)  ', IP = ', SmllNmbr, ', ', SmllNmbr, ', ', SmllNmbr


   ! Azimuth coordinate system (fixed in LSS attached to rotor):

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''AzimuthCS_M'''
WRITE (UnAD,FmtText  )  'MARKER/3000'  ! MARKER/3000 is equivalent to the azimuth coordinate system: X = Xa, Y = Ya, Z = Za
WRITE (UnAD,FmtText  )  ', PART = 3000'


   ! Low-speed shaft center of mass:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''LSSCM_M'''
WRITE (UnAD,FmtText  )  'MARKER/3005'
WRITE (UnAD,FmtText  )  ', PART = 3000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', OverHang - 0.5*LSSLength, ', ', 0.0, ', ', 0.0  ! Locate the LSS CM half-way down the shaft.
WRITE (UnAD,FmtText  )  ', REULER = 0, 0, 0'


   ! Low-speed shaft connection to teeter pin:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''LSSTeetPin_M'''
WRITE (UnAD,FmtText  )  'MARKER/3030'
WRITE (UnAD,FmtText  )  ', PART = 3000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', OverHang, ', ', 0.0, ', ', 0.0
WRITE (UnAD,FmtText  )  ', REULER = 0, 0, 0'


   ! Low-speed shaft strain gage location:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''LSSGag_M'''
WRITE (UnAD,FmtText  )  'MARKER/3040'
WRITE (UnAD,FmtText  )  ', PART = 3000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', OverHang + ShftGagL, ', ', 0.0, ', ', 0.0
WRITE (UnAD,FmtText  )  ', REULER = 0, 0, 0'


   ! Rotor azimuth orientation:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''RotAzim_M'''
WRITE (UnAD,FmtText  )  'MARKER/3050'
WRITE (UnAD,FmtText  )  ', PART = 3000'
WRITE (UnAD,FmtText  )  ', QP = 0, 0, 0'
WRITE (UnAD,FmtText  )  ', REULER = 90D, 90D, 90D'
   ! Blade azimuth reference MARKERs:
DO K = 1,NumBl ! Loop through all blades
   TmpID = 3050 + K
   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''Blade', K, 'AzimRef_M'''
   WRITE (UnAD,FmtText   )  'MARKER/'//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtText   )  ', PART = 3000'
   WRITE (UnAD,FmtText   )  ', QP = 0, 0, 0'
   WRITE (UnAD,FmtTRTRTR )  ', REULER = ', 90.0*D2R, ', ', 90.0*D2R, ', ', 90.0*D2R + TwoPiNB*(K-1)
ENDDO          ! K - Blades


   ! Low-speed shaft graphics:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''LSSGraphics_M'''
WRITE (UnAD,FmtText  )  'MARKER/3020'
WRITE (UnAD,FmtText  )  ', PART = 3000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', OverHang - LSSLength, ', ', 0.0, ', ', 0.0
WRITE (UnAD,FmtText  )  ', REULER = 90D, 90D, 90D'

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''LSS_G'''
WRITE (UnAD,FmtText  )  'GRAPHICS/3000'
WRITE (UnAD,FmtText  )  ', CYLINDER'
WRITE (UnAD,FmtText  )  ', CM = 3020'
WRITE (UnAD,FmtTR    )  ', LENGTH = ', LSSLength
WRITE (UnAD,FmtText  )  ', SIDES = '//TRIM(Int2LStr( NSides ))
WRITE (UnAD,FmtTR    )  ', RADIUS = ', LSSRad
WRITE (UnAD,FmtText  )  ', SEG = '//TRIM(Int2LStr( NSides ))


   ! High-speed shaft:

TmpVec  = PtfmSurge*z1 + PtfmHeave*z2 - PtfmSway *z3 + RefTwrHt*a2 &
        + RFrlPntxn*d1 + RFrlPntzn*d2 - RFrlPntyn*d3 + rVPxn*rf1 + rVPzn*rf2 - rVPyn*rf3  ! Position vector from ground to origin of shaft CS
TmpVec1 = TmpVec + e3
TmpVec2 = TmpVec + e1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''HSS_P'''
WRITE (UnAD,FmtText  )  'PART/3100'
WRITE (UnAD,FmtTRTRTR)  ', QG = ', TmpVec (1), ', ', -TmpVec (3), ', ', TmpVec (2)  ! Orient the
WRITE (UnAD,FmtTRTRTR)  ', ZG = ', TmpVec1(1), ', ', -TmpVec1(3), ', ', TmpVec1(2)  ! HSS using the
WRITE (UnAD,FmtTRTRTR)  ', XG = ', TmpVec2(1), ', ', -TmpVec2(3), ', ', TmpVec2(2)  ! 3-point method
WRITE (UnAD,FmtText  )  ', CM = 3105'
WRITE (UnAD,FmtTR    )  ', MASS = ', SmllNmbr
WRITE (UnAD,FmtTRTRTR)  ', IP = ', SmllNmbr, ', ', SmllNmbr, ', ', SmllNmbr


   ! High-speed shaft reference axis:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''HSSRef_M'''
WRITE (UnAD,FmtText  )  'MARKER/3100'
WRITE (UnAD,FmtText  )  ', PART = 3100'


   ! High-speed shaft center of mass:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''HSSCM_M'''
WRITE (UnAD,FmtText  )  'MARKER/3105'
WRITE (UnAD,FmtText  )  ', PART = 3100'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', OverHang - LSSLength + 0.5*HSSLength, ', ', 0.0, ', ', 0.0   ! Locate the HSS CM half-way down the shaft.
WRITE (UnAD,FmtText  )  ', REULER = 0, 0, 0'


   ! Generator azimuth orientation:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''GenAzim_M'''
WRITE (UnAD,FmtText  )  'MARKER/3150'
WRITE (UnAD,FmtText  )  ', PART = 3100'
WRITE (UnAD,FmtText  )  ', QP = 0, 0, 0'
WRITE (UnAD,FmtText  )  ', REULER = 90D, 90D, 90D'


   ! High-speed shaft graphics:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''HSSGraphics_M'''
WRITE (UnAD,FmtText  )  'MARKER/3120'
WRITE (UnAD,FmtText  )  ', PART = 3100'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', OverHang - LSSLength, ', ', 0.0, ', ', 0.0
WRITE (UnAD,FmtText  )  ', REULER = 90D, 90D, 90D'

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''HSS_G'''
WRITE (UnAD,FmtText  )  'GRAPHICS/3100'
WRITE (UnAD,FmtText  )  ', CYLINDER'
WRITE (UnAD,FmtText  )  ', CM = 3120'
WRITE (UnAD,FmtTR    )  ', LENGTH = ', HSSLength
WRITE (UnAD,FmtText  )  ', SIDES = '//TRIM(Int2LStr( NSides ))
WRITE (UnAD,FmtTR    )  ', RADIUS = ', HSSRad
WRITE (UnAD,FmtText  )  ', SEG = '//TRIM(Int2LStr( NSides ))


   ! Generator:

TmpVec  = PtfmSurge*z1 + PtfmHeave*z2 - PtfmSway *z3 + RefTwrHt*a2 &
        + RFrlPntxn*d1 + RFrlPntzn*d2 - RFrlPntyn*d3 + rVPxn*rf1 + rVPzn*rf2 - rVPyn*rf3  ! Position vector from ground to origin of shaft CS
TmpVec1 = TmpVec + e3
TmpVec2 = TmpVec + e1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''Generator_P'''
WRITE (UnAD,FmtText  )  'PART/3200'
WRITE (UnAD,FmtTRTRTR)  ', QG = ', TmpVec (1), ', ', -TmpVec (3), ', ', TmpVec (2)  ! Orient the
WRITE (UnAD,FmtTRTRTR)  ', ZG = ', TmpVec1(1), ', ', -TmpVec1(3), ', ', TmpVec1(2)  ! gen. using the
WRITE (UnAD,FmtTRTRTR)  ', XG = ', TmpVec2(1), ', ', -TmpVec2(3), ', ', TmpVec2(2)  ! 3-point method
WRITE (UnAD,FmtText  )  ', CM = 3205'
WRITE (UnAD,FmtTR    )  ', MASS = ', SmllNmbr
WRITE (UnAD,FmtTRTRTR)  ', IP = ', GenIner*GBRatio*GBRatio + SmllNmbr, ', ', SmllNmbr, ', ', SmllNmbr


   ! Generator reference axis:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''GenRef_M'''
WRITE (UnAD,FmtText  )  'MARKER/3200'
WRITE (UnAD,FmtText  )  ', PART = 3200'


   ! Generator center of mass:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''GenCM_M'''
WRITE (UnAD,FmtText  )  'MARKER/3205'
WRITE (UnAD,FmtText  )  ', PART = 3200'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', OverHang - LSSLength + HSSLength + 0.5*GenLength, ', ', 0.0, ', ', 0.0   ! Locate the generator CM in the middle of the generator.
WRITE (UnAD,FmtText  )  ', REULER = 0, 0, 0'


   ! Generator graphics:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''GenGraphics_M'''
WRITE (UnAD,FmtText  )  'MARKER/3220'
WRITE (UnAD,FmtText  )  ', PART = 3200'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', OverHang - LSSLength + HSSLength, ', ', 0.0, ', ', 0.0
WRITE (UnAD,FmtText  )  ', REULER = 90D, 90D, 90D'

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''Generator_G'''
WRITE (UnAD,FmtText  )  'GRAPHICS/3200'
WRITE (UnAD,FmtText  )  ', CYLINDER'
WRITE (UnAD,FmtText  )  ', CM = 3220'
WRITE (UnAD,FmtTR    )  ', LENGTH = ', GenLength
WRITE (UnAD,FmtText  )  ', SIDES = '//TRIM(Int2LStr( NSides ))
WRITE (UnAD,FmtTR    )  ', RADIUS = ', GenRad
WRITE (UnAD,FmtText  )  ', SEG = '//TRIM(Int2LStr( NSides ))


   ! Teeter pin:

TmpVec  = PtfmSurge*z1 + PtfmHeave*z2 - PtfmSway *z3 + RefTwrHt*a2 &
        + RFrlPntxn*d1 + RFrlPntzn*d2 - RFrlPntyn*d3 + rVPxn*rf1 + rVPzn*rf2 - rVPyn*rf3 + OverHang*c1   ! rP = Position vector from ground to teeter pin (point P).
TmpVec1 = TmpVec + e3
TmpVec2 = TmpVec + e1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TeeterPin_P'''
WRITE (UnAD,FmtText  )  'PART/3300'
WRITE (UnAD,FmtTRTRTR)  ', QG = ', TmpVec (1), ', ', -TmpVec (3), ', ', TmpVec (2)  ! Orient the teeter
WRITE (UnAD,FmtTRTRTR)  ', ZG = ', TmpVec1(1), ', ', -TmpVec1(3), ', ', TmpVec1(2)  ! pin using the
WRITE (UnAD,FmtTRTRTR)  ', XG = ', TmpVec2(1), ', ', -TmpVec2(3), ', ', TmpVec2(2)  ! 3-point method
WRITE (UnAD,FmtText  )  ', CM = 3300'
WRITE (UnAD,FmtTR    )  ', MASS = ', SmllNmbr
WRITE (UnAD,FmtTRTRTR)  ', IP = ', SmllNmbr, ', ', SmllNmbr, ', ', SmllNmbr


   ! Teeter pin center of mass:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TeetPinCM_M'''
WRITE (UnAD,FmtText  )  'MARKER/3300'
WRITE (UnAD,FmtText  )  ', PART = 3300'


   ! Teeter bearing attached to low-speed shaft:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TeetBrBottom_M'''
WRITE (UnAD,FmtText  )  'MARKER/3310'
WRITE (UnAD,FmtText  )  ', PART = 3300'
WRITE (UnAD,FmtText  )  ', QP = 0, 0, 0'
WRITE (UnAD,FmtText  )  ', REULER = 0D, -90D, 0D'



   ! Those on the hub:

WRITE (UnAD,FmtText  )  '!------------------------------------- Hub -------------------------------------'


   ! Hub:

TmpVec  = PtfmSurge*z1 + PtfmHeave*z2 - PtfmSway *z3 + RefTwrHt*a2                       &
        + RFrlPntxn*d1 + RFrlPntzn*d2 - RFrlPntyn*d3 + rVPxn*rf1 + rVPzn*rf2 - rVPyn*rf3 &
        + OverHang*c1 - UndSling*g1                                                          ! rP = Position vector from ground to apex of rotation (point Q).
TmpVec1 = TmpVec + g3
TmpVec2 = TmpVec + g1
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''Hub_P'''
WRITE (UnAD,FmtText  )  'PART/4000'
WRITE (UnAD,FmtTRTRTR)  ', QG = ', TmpVec (1), ', ', -TmpVec (3), ', ', TmpVec (2)  ! Orient the apex of
WRITE (UnAD,FmtTRTRTR)  ', ZG = ', TmpVec1(1), ', ', -TmpVec1(3), ', ', TmpVec1(2)  ! rotation using the
WRITE (UnAD,FmtTRTRTR)  ', XG = ', TmpVec2(1), ', ', -TmpVec2(3), ', ', TmpVec2(2)  ! 3-point method
WRITE (UnAD,FmtText  )  ', CM = 4005'
WRITE (UnAD,FmtTR    )  ', MASS = ', HubMass + SmllNmbr
WRITE (UnAD,FmtTRTRTR)  ', IP = ', Hubg1Iner + SmllNmbr, ', ', Hubg2Iner + SmllNmbr, ', ', SmllNmbr


   ! Hub coordinate system (fixed in hub):

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''HubCS_M'''
WRITE (UnAD,FmtText  )  'MARKER/4000'  ! MARKER/4000 is equivalent to the hub coordinate system: X = Xh, Y = Yh, Z = Zh
WRITE (UnAD,FmtText  )  ', PART = 4000'


   ! Hub center of mass:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''HubCM_M'''
WRITE (UnAD,FmtText  )  'MARKER/4005'
WRITE (UnAD,FmtText  )  ', PART = 4000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', HubCM, ', ', 0.0, ', ', 0.0
WRITE (UnAD,FmtText  )  ', REULER = 0, 0, 0'


   ! Teeter bearing attached to hub:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TeetBrTop_M'''
WRITE (UnAD,FmtText  )  'MARKER/4010'
WRITE (UnAD,FmtText  )  ', PART = 4000'
WRITE (UnAD,FmtTRTRTR)  ', QP = ', UndSling, ', ', 0.0, ', ', 0.0

!bjj Start of proposed change v12.70
!rm      WRITE (UnAD,FmtTRTRTR)  ', REULER = ', 0.0, ', ', -Delta3 - PiOvr2, ', ', 0.0
WRITE (UnAD,FmtTRTRTR)  ', REULER = ', 0.0, ', ', -Delta3 - PiBy2, ', ', 0.0
!bjj End of proposed change


DO K = 1,NumBl ! Loop through all blades


   ! Coned coordinate system (fixed in hub):

   TmpID = 4000 + K
   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''Coned', K, 'CS_M'''
   WRITE (UnAD,FmtText   )  'MARKER/'//TRIM(Int2LStr( TmpID )) ! MARKER/400K is equivalent to the coned coordinate system for blade K: X = XcK, Y = YcK, Z = ZcK
   WRITE (UnAD,FmtText   )  ', PART = 4000'
   WRITE (UnAD,FmtText   )  ', QP = 0, 0, 0'                                                                               ! Orient the coned
   WRITE (UnAD,FmtTRTRTR )  ', ZP = ', DOT_PRODUCT( i3(K,:), g1 ), ', ', DOT_PRODUCT( i3(K,:), g2 ), ', ', &
                                       DOT_PRODUCT( i3(K,:), g3 )                                                          ! CS using the
   WRITE (UnAD,FmtTRTRTR )  ', XP = ', DOT_PRODUCT( i1(K,:), g1 ), ', ', DOT_PRODUCT( i1(K,:), g2 ), ', ', &
                                       DOT_PRODUCT( i1(K,:), g3 )                                                          ! 3-point method


   ! Pitch bearing (fixed in hub):

   TmpID = 4010 + K
   TmpVec  = HubRad*i3(K,:)   ! rQS0 = Position vector from apex of rotation (point Q) to the blade root (point S(0)).
   TmpVec1 = TmpVec + i3(K,:)
   TmpVec2 = TmpVec + i1(K,:)
   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''PitchBr', K, 'Bottom_M'''
   WRITE (UnAD,FmtText   )  'MARKER/'//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtText   )  ', PART = 4000'
   WRITE (UnAD,FmtTRTRTR )  ', QP = ', DOT_PRODUCT( TmpVec , g1 ), ', ', DOT_PRODUCT( TmpVec , g2 ), ', ', &
                                       DOT_PRODUCT( TmpVec , g3 )  ! Orient the pitch
   WRITE (UnAD,FmtTRTRTR )  ', ZP = ', DOT_PRODUCT( TmpVec1, g1 ), ', ', DOT_PRODUCT( TmpVec1, g2 ), ', ', &
                                       DOT_PRODUCT( TmpVec1, g3 )  ! bearing using the
   WRITE (UnAD,FmtTRTRTR )  ', XP = ', DOT_PRODUCT( TmpVec2, g1 ), ', ', DOT_PRODUCT( TmpVec2, g2 ), ', ', &
                                       DOT_PRODUCT( TmpVec2, g3 )  ! 3-point method


   ! Undeflected location of blade tip  (fixed in hub):

   TmpID = 4030 + K
   TmpVec  = TipRad*i3(K,:)   ! rQS = Position vector from apex of rotation (point Q) to the blade tip (point S(BldFlexL)).
   TmpVec1 = TmpVec + i3(K,:)
   TmpVec2 = TmpVec + i1(K,:)
   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''UndeflBld', K, 'Tip_M'''
   WRITE (UnAD,FmtText   )  'MARKER/'//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtText   )  ', PART = 4000'
   WRITE (UnAD,FmtTRTRTR )  ', QP = ', DOT_PRODUCT( TmpVec , g1 ), ', ', DOT_PRODUCT( TmpVec , g2 ), ', ', &
                                       DOT_PRODUCT( TmpVec , g3 )  ! Orient the undeflected
   WRITE (UnAD,FmtTRTRTR )  ', ZP = ', DOT_PRODUCT( TmpVec1, g1 ), ', ', DOT_PRODUCT( TmpVec1, g2 ), ', ', &
                                       DOT_PRODUCT( TmpVec1, g3 )  ! blade tip using the
   WRITE (UnAD,FmtTRTRTR )  ', XP = ', DOT_PRODUCT( TmpVec2, g1 ), ', ', DOT_PRODUCT( TmpVec2, g2 ), ', ', &
                                       DOT_PRODUCT( TmpVec2, g3 )  ! 3-point method


   ! Pitch reference MARKERS (fixed in hub):

   TmpID = 4091 + K*100
   TmpVec  = HubRad*i3(K,:)   ! rQS0 = Position vector from apex of rotation (point Q) to the blade root (point S(0)).
   TmpVec1 = TmpVec - i2(K,:)
   TmpVec2 = TmpVec + i3(K,:)
   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''PitchRef', K, '_M'''
   WRITE (UnAD,FmtText   )  'MARKER/'//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtText   )  ', PART = 4000'
   WRITE (UnAD,FmtTRTRTR )  ', QP = ', DOT_PRODUCT( TmpVec , g1 ), ', ', DOT_PRODUCT( TmpVec , g2 ), ', ', &
                                       DOT_PRODUCT( TmpVec , g3 )  ! Orient the pitch ref.
   WRITE (UnAD,FmtTRTRTR )  ', ZP = ', DOT_PRODUCT( TmpVec1, g1 ), ', ', DOT_PRODUCT( TmpVec1, g2 ), ', ', &
                                       DOT_PRODUCT( TmpVec1, g3 )  ! MARKERS using the
   WRITE (UnAD,FmtTRTRTR )  ', XP = ', DOT_PRODUCT( TmpVec2, g1 ), ', ', DOT_PRODUCT( TmpVec2, g2 ), ', ', &
                                       DOT_PRODUCT( TmpVec2, g3 )  ! 3-point method


   ! Hub graphics:

   TmpID = 4000 + K
   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''Hub', K, '_G'''
   WRITE (UnAD,FmtText   )  'GRAPHICS/'//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtText   )  ', CYLINDER'
   WRITE (UnAD,FmtText   )  ', CM = '//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtTR     )  ', LENGTH = ', HubRad
   WRITE (UnAD,FmtText   )  ', SIDES = '//TRIM(Int2LStr( NSides ))
   WRITE (UnAD,FmtTR     )  ', RADIUS = ', HubCylRad
   WRITE (UnAD,FmtText   )  ', SEG = '//TRIM(Int2LStr( NSides ))


ENDDO          ! K - Blades



   ! Those on the blades:

WRITE (UnAD,FmtText  )  '!------------------------------------ Blades -----------------------------------'


DO K = 1,NumBl       ! Loop through all blades


   ! Pitch plate (attached to blade):

   WRITE (UnAD,'(A,I1,A)')  '!---------------------------- Blade ', K, ': Pitch Plate -----------------------------'
   TmpID = 10000*K
   TmpVec  = PtfmSurge*z1 + PtfmHeave*z2 - PtfmSway *z3 + RefTwrHt*a2                       &
           + RFrlPntxn*d1 + RFrlPntzn*d2 - RFrlPntyn*d3 + rVPxn*rf1 + rVPzn*rf2 - rVPyn*rf3 &
           + OverHang*c1 - UndSling*g1 + HubRad*i3(K,:)                                         ! rS0 = Position vector from ground to the blade root (point S(0)).
   TmpVec1 = TmpVec + j3(K,:)
   TmpVec2 = TmpVec + j1(K,:)
   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''PitchPlate', K, '_P'''
   WRITE (UnAD,FmtText   )  'PART/'//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtTRTRTR )  ', QG = ', TmpVec (1), ', ', -TmpVec (3), ', ', TmpVec (2)  ! Orient the pitch
   WRITE (UnAD,FmtTRTRTR )  ', ZG = ', TmpVec1(1), ', ', -TmpVec1(3), ', ', TmpVec1(2)  ! plate using the
   WRITE (UnAD,FmtTRTRTR )  ', XG = ', TmpVec2(1), ', ', -TmpVec2(3), ', ', TmpVec2(2)  ! 3-point method
   WRITE (UnAD,FmtText   )  ', CM = '//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtTR     )  ', MASS = ', SmllNmbr
   WRITE (UnAD,FmtTRTRTR )  ', IP = ', SmllNmbr, ', ', SmllNmbr, ', ', SmllNmbr


   ! Pitch plate center of mass - blade coordinate system:

   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''Blade', K, 'CS_M'''
   WRITE (UnAD,FmtText   )  'MARKER/'//TRIM(Int2LStr( TmpID ))   ! MARKER/K0000 is equivalent to the blade K coordinate system: X = XbK, Y = YbK, Z = ZbK
   WRITE (UnAD,FmtText   )  ', PART = '//TRIM(Int2LStr( TmpID ))


   ! Connection to 1st blade element

   TmpID2 = 10000*K + 4000
   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''PitchPlate', K, 'ToBldSec01_M'''
   WRITE (UnAD,FmtText   )  'MARKER/'//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText   )  ', PART = '//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtText   )  ', QP = 0, 0, 0'
!bjj Start of proposed change v12.70
!rm      WRITE (UnAD,FmtTRTRTR )  ', REULER = ', -ThetaS(K,1), ', ', PiOvr2, ', ', PiOvr2
   WRITE (UnAD,FmtTRTRTR )  ', REULER = ', -ThetaS(K,1), ', ', PiBy2, ', ', PiBy2
!bjj End of proposed change




   DO J = 1,BldNodes ! Loop through the blade nodes/elements


   ! Let's define the reference axis coordinate system for the current blade
   !   element.  This coordinate system is coincident with the pitch axis
   !   coordinate system for unprecurved and unpreswept blades, but is
   !   oriented with the reference axis for precurved and preswept blades:

      IF ( J == 1 )  THEN              ! Innermost blade element
         Slopexb = -( RefAxisyb(K,J+1)                    )/( RNodes(J+1)               ) ! The reference axis is coincident with the
         Slopeyb =  ( RefAxisxb(K,J+1)                    )/( RNodes(J+1)               ) ! pitch axis at the blade root (RNodes = 0)
      ELSEIF ( J == BldNodes )  THEN   ! Outermost blade element
         Slopexb = -( RefAxisyb(K,J+1) - RefAxisyb(K,J-1) )/( BldFlexL    - RNodes(J-1) ) ! Slope of the reference axis about the xb-axis using central difference differentation
         Slopeyb =  ( RefAxisxb(K,J+1) - RefAxisxb(K,J-1) )/( BldFlexL    - RNodes(J-1) ) ! Slope of the reference axis about the yb-axis using central difference differentation
      ELSE                             ! All other blade elements
         Slopexb = -( RefAxisyb(K,J+1) - RefAxisyb(K,J-1) )/( RNodes(J+1) - RNodes(J-1) ) ! Slope of the reference axis about the xb-axis using central difference differentation
         Slopeyb =  ( RefAxisxb(K,J+1) - RefAxisxb(K,J-1) )/( RNodes(J+1) - RNodes(J-1) ) ! Slope of the reference axis about the yb-axis using central difference differentation
      ENDIF

      CALL SmllRotTrans( 'blade prebend', Slopexb, Slopeyb, 0.0, TransMat )   ! Get the transformation matrix, TransMat, from the pitch axis to the reference axis coordinate system.

      Ref1 = TransMat(1,1)*j1(K,:) + TransMat(1,2)*j2(K,:) + TransMat(1,3)*j3(K,:)  ! Vector / direction Ref1 for node J of blade K (= j1 if precurve and presweep are zero).
      Ref2 = TransMat(2,1)*j1(K,:) + TransMat(2,2)*j2(K,:) + TransMat(2,3)*j3(K,:)  ! Vector / direction Ref2 for node J of blade K (= j2 if precurve and presweep are zero).
      Ref3 = TransMat(3,1)*j1(K,:) + TransMat(3,2)*j2(K,:) + TransMat(3,3)*j3(K,:)  ! Vector / direction Ref3 for node J of blade K (= j3 if precurve and presweep are zero).


   ! Let's define the lengths of the blade elements used in blade graphics:

      DRNodesGRA(J) = DRNodes(J)*SQRT( 1.0 + Slopexb*Slopexb + Slopeyb*Slopeyb )


   ! Let's redefine what the trailing edge coordinate system is now that we
   !   have a precurved and pretwisted blade.
   ! This coordinate system is blade element-fixed and oriented with the local
   !   aerodynamic axes (te2 points toward trailing edge, te1 points toward
   !   suction surface):

      te1(K,J,:) = Ref1*CAeroTwst(J) - Ref2*SAeroTwst(J)
      te2(K,J,:) = Ref1*SAeroTwst(J) + Ref2*CAeroTwst(J)
      te3(K,J,:) = Ref3


   ! Let's store the location of the structural axes of blade K, element J
   !   relative to the structural axes of blade K, element J-1:

      IF ( J == 1 )  THEN  ! Innermost blade element
         EAVec(K,J,:) = RNodes   (  J  )*i3 (K,    :)                                 &
                      + RefAxisxb(K,J  )*j1 (K,    :) + RefAxisyb(K,J  )*j2 (K,    :) &
                      + EAOffBFlp(K,J  )*te1(K,J  ,:) + EAOffBEdg(K,J  )*te2(K,J  ,:)
      ELSE                 ! All other blade elements
         EAVec(K,J,:) = RNodes   (  J  )*i3 (K,    :) - RNodes   (  J-1)*i3 (K,    :) &
                      + RefAxisxb(K,J  )*j1 (K,    :) + RefAxisyb(K,J  )*j2 (K,    :) &
                      - RefAxisxb(K,J-1)*j1 (K,    :) - RefAxisyb(K,J-1)*j2 (K,    :) &
                      + EAOffBFlp(K,J  )*te1(K,J  ,:) + EAOffBEdg(K,J  )*te2(K,J  ,:) &
                      - EAOffBFlp(K,J-1)*te1(K,J-1,:) - EAOffBEdg(K,J-1)*te2(K,J-1,:)
      ENDIF


   ENDDO             ! J - Blade nodes/elements


   ! Let's store the location of the blade tip relative to the outermost blade
   !   element structural axes:

   EAVec(K,TipNode,:) = BldFlexL             *i3 (K,         :) - RNodes   (  BldNodes)*i3 (K,         :) &
                      + RefAxisxb(K,TipNode )*j1 (K,         :) + RefAxisyb(K,TipNode )*j2 (K,         :) &
                      - RefAxisxb(K,BldNodes)*j1 (K,         :) - RefAxisyb(K,BldNodes)*j2 (K,         :) &
                      - EAOffBFlp(K,BldNodes)*te1(K,BldNodes,:) - EAOffBEdg(K,BldNodes)*te2(K,BldNodes,:)



   ! Blade segments:

   DO J = 1,BldNodes ! Loop through the blade nodes/elements

      WRITE (UnAD,'(A,I1,A,I2.2,A)')  '!-------------------------- Blade ', K, ': Blade Section ', J, ' --------------------------'


   ! PART:

      TmpID  = 10000*K + J
      ThnBarI = MassB(K,J)*( DRNodes(J)**3 )/12.0  ! Define the transverse inertias of the blade element (both identical) to be that of a thin uniform bar.
      TmpVec  = PtfmSurge*z1 + PtfmHeave*z2 - PtfmSway *z3 + RefTwrHt*a2                       &
              + RFrlPntxn*d1 + RFrlPntzn*d2 - RFrlPntyn*d3 + rVPxn*rf1 + rVPzn*rf2 - rVPyn*rf3 &
              + OverHang*c1 - UndSling*g1 + ( HubRad + RNodes(J) )*i3(K,:) + RefAxisxb(K,J)*j1(K,:) + RefAxisyb(K,J)*j2(K,:)  ! rS = Position vector from ground to the current blade point (point S).
      TmpVec1 = TmpVec - te2(K,J,:)
      TmpVec2 = TmpVec + te3(K,J,:)
      WRITE (UnAD,'(A,I1,A,I2.2,A)')  '!                             adams_view_name=''Blade', K, 'Sec', J, '_P'''
      WRITE (UnAD,FmtText          )  'PART/'//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtTRTRTR        )  ', QG = ', TmpVec (1), ', ', -TmpVec (3), ', ', TmpVec (2)   ! Orient the blade
      WRITE (UnAD,FmtTRTRTR        )  ', ZG = ', TmpVec1(1), ', ', -TmpVec1(3), ', ', TmpVec1(2)   ! section using the
      WRITE (UnAD,FmtTRTRTR        )  ', XG = ', TmpVec2(1), ', ', -TmpVec2(3), ', ', TmpVec2(2)   ! 3-point method
      WRITE (UnAD,FmtText          )  ', CM = '//TRIM(Int2LStr( 10000*K + 5000 + J ))
!JASON: THIS MASS CALCULATION SHOULD USE DRNodesGRA SINCE THE MASS OF A SHEARED ELEMENT WOULD BE MORE THAN A STRAIGHT ELEMENT.  FOR EXAMPLE, IF THERE WAS A LINEAR PRESWEEP IN A BEAM, THEN THE MASS OF THE BEAM WOULD BE 1/COS(ANGLE) TIMES THE MASS OF AN EQUIVALENT STRAIGHT BEAM (SINCE THE LENGTH OF A THE SWEPT BEAM WOULD BE 1/COS(ANGLE) TIMES THE LENGTH OF THE STRAIGHT BEAM).  THE SAME LENGTH SHOULD BE USED IN THE CALCULATION OF THE STIFFNESS TERMS IN THE FIELD STATMENT.  THE MASS CALCULATION PERFORMED BELOW IS WHAT APPEARED IN KIRK PIERCE'S VERSION OF ADAMS/WT AND I DON'T LIKE IT (BUT I USED IT FOR CONSISTENCY WITH THE OLD CODE)!!!
      WRITE (UnAD,FmtTR            )  ', MASS = ', MassB(K,J)*DRNodes(J) + SmllNmbr
      WRITE (UnAD,FmtTRTRTR        )  ', IP = ', ( ( InerBFlp(K,J) + InerBEdg(K,J) )*DRNodes(J) ) + SmllNmbr, ', ', &
                                      ( InerBEdg(K,J)*DRNodes(J) ) + ThnBarI + SmllNmbr, ', ',                      &
                                      ( InerBFlp(K,J)*DRNodes(J) ) + ThnBarI + SmllNmbr

      WRITE (UnAD,'(A,I1,A,I2.2,A)')  '!                             adams_view_name=''Blade', K, 'Sec', J, 'Ref_M'''
      WRITE (UnAD,FmtText          )  'MARKER/'//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtText          )  ', PART = '//TRIM(Int2LStr( TmpID ))


   ! Aerodynamic MARKERS:

      TmpID2 = 10000*K + 1000 + J*10
      WRITE (UnAD,'(A,I1,A,I2.2,A)')  '!                             adams_view_name=''Blade', K, 'Sec', J, 'Aero_M'''
      WRITE (UnAD,FmtText          )  'MARKER/'//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText          )  ', PART = '//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtTRTRTR        )  ', QP = ', 0.0, ', ', 0.0, ', ', ( 0.25 - AeroCent(K,J) )*Chord(J)
      WRITE (UnAD,FmtText          )  ', REULER = 0D, 0D, 0D'


   ! Graphics:

      TmpID2 = 10000*K + 2000 + J
      WRITE (UnAD,'(A,I1,A,I2.2,A)')  '!                             adams_view_name=''Blade', K, 'Sec', J, 'Graphics_M'''
      WRITE (UnAD,FmtText          )  'MARKER/'//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText          )  ', PART = '//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtTRTRTR        )  ', QP = ', -0.5*DRNodesGRA(J), ', ', -0.5*ThkOvrChrd*Chord(J), ', ', -0.75*Chord(J)  ! The pitch axis (reference axis) is at 25% chord
      WRITE (UnAD,FmtText          )  ', REULER = 0D, 0D, 0D'

      WRITE (UnAD,'(A,I1,A,I2.2,A)')  '!                             adams_view_name=''Blade', K, 'Sec', J, '_G'''
      WRITE (UnAD,FmtText          )  'GRAPHICS/'//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText          )  ', BOX'
      WRITE (UnAD,FmtText          )  ', CORNER = '//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtTR            )  ', X = ', DRNodesGRA(J)
      WRITE (UnAD,FmtTR            )  ', Y = ', ThkOvrChrd*Chord(J)
      WRITE (UnAD,FmtTR            )  ', Z = ', Chord(J)


   ! Bottom (for connection to the segment below):

      TmpID2 = 10000*K + 3000 + J
      WRITE (UnAD,'(A,I1,A,I2.2,A)')  '!                             adams_view_name=''Bld', K, 'Sec', J, 'ToBldSecBelow_M'''
      WRITE (UnAD,FmtText          )  'MARKER/'//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText          )  ', PART = '//TRIM(Int2LStr( TmpID ))
      IF ( J == 1 )  THEN        ! Innermost blade element
         WRITE (UnAD,FmtTRTRTR     )  ', QP = ', DOT_PRODUCT( -    EAVec(K,J,:)            ,  te3(K,J,:) ), ', ', -EAOffBFlp(K,J) &
                                               + DOT_PRODUCT( -    EAVec(K,J,:)            , -te1(K,J,:) ), ', ', -EAOffBEdg(K,J) &
                                               + DOT_PRODUCT( -    EAVec(K,J,:)            , -te2(K,J,:) )                         ! Orient the MARKER
         WRITE (UnAD,FmtTRTRTR     )  ', ZP = ', DOT_PRODUCT( -    EAVec(K,J,:) - n2(K,J,:),  te3(K,J,:) ), ', ', -EAOffBFlp(K,J) &
                                               + DOT_PRODUCT( -    EAVec(K,J,:) - n2(K,J,:), -te1(K,J,:) ), ', ', -EAOffBEdg(K,J) &
                                               + DOT_PRODUCT( -    EAVec(K,J,:) - n2(K,J,:), -te2(K,J,:) )                         ! using the
         WRITE (UnAD,FmtTRTRTR     )  ', XP = ', DOT_PRODUCT( -    EAVec(K,J,:) + n3(K,J,:),  te3(K,J,:) ), ', ', -EAOffBFlp(K,J) &
                                               + DOT_PRODUCT( -    EAVec(K,J,:) + n3(K,J,:), -te1(K,J,:) ), ', ', -EAOffBEdg(K,J) &
                                               + DOT_PRODUCT( -    EAVec(K,J,:) + n3(K,J,:), -te2(K,J,:) )                         ! 3-point method
      ELSE                       ! All other blade elements
         WRITE (UnAD,FmtTRTRTR     )  ', QP = ', DOT_PRODUCT( -0.5*EAVec(K,J,:)            ,  te3(K,J,:) ), ', ', -EAOffBFlp(K,J) &
                                               + DOT_PRODUCT( -0.5*EAVec(K,J,:)            , -te1(K,J,:) ), ', ', -EAOffBEdg(K,J) &
                                               + DOT_PRODUCT( -0.5*EAVec(K,J,:)            , -te2(K,J,:) )                         ! Orient the MARKER
         WRITE (UnAD,FmtTRTRTR     )  ', ZP = ', DOT_PRODUCT( -0.5*EAVec(K,J,:) - n2(K,J,:),  te3(K,J,:) ), ', ', -EAOffBFlp(K,J) &
                                               + DOT_PRODUCT( -0.5*EAVec(K,J,:) - n2(K,J,:), -te1(K,J,:) ), ', ', -EAOffBEdg(K,J) &
                                               + DOT_PRODUCT( -0.5*EAVec(K,J,:) - n2(K,J,:), -te2(K,J,:) )                         ! using the
         WRITE (UnAD,FmtTRTRTR     )  ', XP = ', DOT_PRODUCT( -0.5*EAVec(K,J,:) + n3(K,J,:),  te3(K,J,:) ), ', ', -EAOffBFlp(K,J) &
                                               + DOT_PRODUCT( -0.5*EAVec(K,J,:) + n3(K,J,:), -te1(K,J,:) ), ', ', -EAOffBEdg(K,J) &
                                               + DOT_PRODUCT( -0.5*EAVec(K,J,:) + n3(K,J,:), -te2(K,J,:) )                         ! 3-point method
      ENDIF


   ! Top (for connection to the segment above):

      TmpID2 = 10000*K + 4000 + J
      WRITE (UnAD,'(A,I1,A,I2.2,A)')  '!                             adams_view_name=''Bld', K, 'Sec', J, 'ToBldSecAbove_M'''
      WRITE (UnAD,FmtText          )  'MARKER/'//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText          )  ', PART = '//TRIM(Int2LStr( TmpID ))
      IF ( J == BldNodes )  THEN ! Outermost blade element
!bjj: I removed 2 spaces so the line is not too long; now it doesn't quite match the spacing above... that's for JASON! ;-)         
         WRITE (UnAD,FmtTRTRTR   )  ', QP = ', DOT_PRODUCT(     EAVec(K,J+1,:)              ,  te3(K,J,:) ), ', ', -EAOffBFlp(K,J) &
                                             + DOT_PRODUCT(     EAVec(K,J+1,:)              , -te1(K,J,:) ), ', ', -EAOffBEdg(K,J) &
                                             + DOT_PRODUCT(     EAVec(K,J+1,:)              , -te2(K,J,:) )                        ! Orient the MARKER
         WRITE (UnAD,FmtTRTRTR   )  ', ZP = ', DOT_PRODUCT(     EAVec(K,J+1,:) - n2(K,J  ,:),  te3(K,J,:) ), ', ', -EAOffBFlp(K,J) &
                                             + DOT_PRODUCT(     EAVec(K,J+1,:) - n2(K,J  ,:), -te1(K,J,:) ), ', ', -EAOffBEdg(K,J) &
                                             + DOT_PRODUCT(     EAVec(K,J+1,:) - n2(K,J  ,:), -te2(K,J,:) )                        ! using the
         WRITE (UnAD,FmtTRTRTR   )  ', XP = ', DOT_PRODUCT(     EAVec(K,J+1,:) + n3(K,J  ,:),  te3(K,J,:) ), ', ', -EAOffBFlp(K,J) &
                                             + DOT_PRODUCT(     EAVec(K,J+1,:) + n3(K,J  ,:), -te1(K,J,:) ), ', ', -EAOffBEdg(K,J) &
                                             + DOT_PRODUCT(     EAVec(K,J+1,:) + n3(K,J  ,:), -te2(K,J,:) )                        ! 3-point method
      ELSE                     ! All other blade elements
         WRITE (UnAD,FmtTRTRTR   )  ', QP = ', DOT_PRODUCT( 0.5*EAVec(K,J+1,:)              ,  te3(K,J,:) ), ', ', -EAOffBFlp(K,J) &
                                             + DOT_PRODUCT( 0.5*EAVec(K,J+1,:)              , -te1(K,J,:) ), ', ', -EAOffBEdg(K,J) &
                                             + DOT_PRODUCT( 0.5*EAVec(K,J+1,:)              , -te2(K,J,:) )                        ! Orient the MARKER
         WRITE (UnAD,FmtTRTRTR   )  ', ZP = ', DOT_PRODUCT( 0.5*EAVec(K,J+1,:) - n2(K,J+1,:),  te3(K,J,:) ), ', ', -EAOffBFlp(K,J) &
                                             + DOT_PRODUCT( 0.5*EAVec(K,J+1,:) - n2(K,J+1,:), -te1(K,J,:) ), ', ', -EAOffBEdg(K,J) &
                                             + DOT_PRODUCT( 0.5*EAVec(K,J+1,:) - n2(K,J+1,:), -te2(K,J,:) )                        ! using the
         WRITE (UnAD,FmtTRTRTR   )  ', XP = ', DOT_PRODUCT( 0.5*EAVec(K,J+1,:) + n3(K,J+1,:),  te3(K,J,:) ), ', ', -EAOffBFlp(K,J) &
                                             + DOT_PRODUCT( 0.5*EAVec(K,J+1,:) + n3(K,J+1,:), -te1(K,J,:) ), ', ', -EAOffBEdg(K,J) &
                                             + DOT_PRODUCT( 0.5*EAVec(K,J+1,:) + n3(K,J+1,:), -te2(K,J,:) )                        ! 3-point method
      ENDIF


   ! Center of Mass:

      TmpID2 = 10000*K + 5000 + J
      WRITE (UnAD,'(A,I1,A,I2.2,A)')  '!                             adams_view_name=''Blade', K, 'Sec', J, 'CM_M'''
      WRITE (UnAD,FmtText          )  'MARKER/'//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText          )  ', PART = '//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtTRTRTR        )  ', QP = ', 0.0, ', ', -cgOffBFlp(K,J), ', ', -cgOffBEdg(K,J)
!bjj start of proposed change v6.02d-bjj
! parentheses around an I/O list is an extension to Standard F2003
!rm      WRITE (UnAD,FmtTRTRTR        )  ', REULER = ', 0.0, ', ', ( AeroTwst(J) - ThetaS(K,J) ), ', ', 0.0    ! Use the same orientation as the structural axis
      WRITE (UnAD,FmtTRTRTR        )  ', REULER = ', 0.0, ', ', AeroTwst(J) - ThetaS(K,J), ', ', 0.0    ! Use the same orientation as the structural axis
!bjj end of proposed change

   ! Structural Axis:
   ! This MARKER is positioned at the structural axis of the precurved and
   !   preswept blade element but oriented with the structural axes of the
   !   blade as if there was no precurve nor presweep.  This follows the
   !   method implemented by Kirk Pierce in Windward Engineering's version
   !   of ADAMS/WT.

      TmpID2 = 10000*K + 6000 + J
      WRITE (UnAD,'(A,I1,A,I2.2,A)')  '!                             adams_view_name=''Blade', K, 'Sec', J, 'EA_M'''
      WRITE (UnAD,FmtText          )  'MARKER/'//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText          )  ', PART = '//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtTRTRTR        )  ', QP = ', 0.0                                                 , ', ', &
                                                 -EAOffBFlp(K,J)                                     , ', ', &
                                                 -EAOffBEdg(K,J)                                                   ! Orient the EA
      WRITE (UnAD,FmtTRTRTR        )  ', ZP = ', 0.0             + DOT_PRODUCT( -n2(K,J,:),  te3(K,J,:) ), ', ', &
                                                 -EAOffBFlp(K,J) + DOT_PRODUCT( -n2(K,J,:), -te1(K,J,:) ), ', ', &
                                                 -EAOffBEdg(K,J) + DOT_PRODUCT( -n2(K,J,:), -te2(K,J,:) )          ! MARKER using the
      WRITE (UnAD,FmtTRTRTR        )  ', XP = ', 0.0             + DOT_PRODUCT(  n3(K,J,:),  te3(K,J,:) ), ', ', &
                                                 -EAOffBFlp(K,J) + DOT_PRODUCT(  n3(K,J,:), -te1(K,J,:) ), ', ', &
                                                 -EAOffBEdg(K,J) + DOT_PRODUCT(  n3(K,J,:), -te2(K,J,:) )          ! 3-point method


   ENDDO             ! J - Blade nodes/elements


   ! Tip brake (attached to blade tip):

   WRITE (UnAD,'(A,I1,A)')  '!----------------------------- Blade ', K, ': Tip Brake ------------------------------'
   TmpID = 10000*K + 5000
   TmpVec  = PtfmSurge*z1 + PtfmHeave*z2 - PtfmSway *z3 + RefTwrHt*a2                       &
           + RFrlPntxn*d1 + RFrlPntzn*d2 - RFrlPntyn*d3 + rVPxn*rf1 + rVPzn*rf2 - rVPyn*rf3 &
           + OverHang*c1 - UndSling*g1 + TipRad*i3(K,:) + RefAxisxb(K,TipNode)*j1(K,:) + RefAxisyb(K,TipNode)*j2(K,:)   ! rS(BldFlexL) = Position vector from ground to the blade tip (point S(BldFlexL)).
   TmpVec1 = TmpVec - n2(K,BldNodes,:)
   TmpVec2 = TmpVec + n3(K,BldNodes,:)
   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''TipBrake', K, '_P'''
   WRITE (UnAD,FmtText   )  'PART/'//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtTRTRTR )  ', QG = ', TmpVec (1), ', ', -TmpVec (3), ', ', TmpVec (2) ! Orient the tip brake using the
   WRITE (UnAD,FmtTRTRTR )  ', ZG = ', TmpVec1(1), ', ', -TmpVec1(3), ', ', TmpVec1(2) ! 3-point method.  Make sure it
   WRITE (UnAD,FmtTRTRTR )  ', XG = ', TmpVec2(1), ', ', -TmpVec2(3), ', ', TmpVec2(2) ! is aligned with the outermost blade element.
   WRITE (UnAD,FmtText   )  ', CM = '//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtTR     )  ', MASS = ', TipMass(K) + SmllNmbr
   WRITE (UnAD,FmtTRTRTR )  ', IP = ', SmllNmbr, ', ', SmllNmbr, ', ', SmllNmbr


   ! Tip brake center of mass:

   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''TipBrake', K, 'CM_M'''
   WRITE (UnAD,FmtText   )  'MARKER/'//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtText   )  ', PART = '//TRIM(Int2LStr( TmpID ))


   ! Tip brake, zero-twist MARKER (i.e., the FAST blade K coordinate system originating at the tip):

   TmpID2 = 10000*K + 5100
   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''TipBrake', K, 'ZeroTwist_M'''
   WRITE (UnAD,FmtText   )  'MARKER/'//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText   )  ', PART = '//TRIM(Int2LStr( TmpID ))
   WRITE (UnAD,FmtText   )  ', QP = 0, 0, 0'


!bjj Start of proposed change v12.70
!rm   WRITE (UnAD,FmtTRTRTR )  ', REULER = ', -PiOvr2, ', ', -PiOvr2, ', ', ThetaS(K,BldNodes)
   WRITE (UnAD,FmtTRTRTR )  ', REULER = ', -PiBy2, ', ', -PiBy2, ', ', ThetaS(K,BldNodes)
!bjj End of proposed change

ENDDO                ! K - Blades




   ! Begin defining constraints (i.e., JOINTs and MOTIONs):

WRITE (UnAD,FmtText  )  '!================================ CONSTRAINTS =================================='



   ! Support Platform:

   ! Lock the platform translation DOFs with MOTION statements at the start
   !   of the simulation.  These MOTIONs are DEACTIVATED after the first time
   !   step if the associated DOFs are enabled.

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''PtfmSurgeLocked_MO'''
WRITE (UnAD,FmtText  )  'MOTION/1001'
WRITE (UnAD,FmtText  )  ', I = 1000'
WRITE (UnAD,FmtText  )  ', J = 10'
WRITE (UnAD,FmtText  )  ', X'
WRITE (UnAD,FmtText  )  ', DISPLACEMENT'
WRITE (UnAD,FmtTR    )  ', FUNCTION = ', PtfmSurge ! Lock platform surge at initial position PtfmSurge

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''PtfmSwayLocked_MO'''
WRITE (UnAD,FmtText  )  'MOTION/1002'
WRITE (UnAD,FmtText  )  ', I = 1000'
WRITE (UnAD,FmtText  )  ', J = 10'
WRITE (UnAD,FmtText  )  ', Y'
WRITE (UnAD,FmtText  )  ', DISPLACEMENT'
WRITE (UnAD,FmtTR    )  ', FUNCTION = ', PtfmSway  ! Lock platform sway  at initial position PtfmSway

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''PtfmHeaveLocked_MO'''
WRITE (UnAD,FmtText  )  'MOTION/1003'
WRITE (UnAD,FmtText  )  ', I = 1000'
WRITE (UnAD,FmtText  )  ', J = 10'
WRITE (UnAD,FmtText  )  ', Z'
WRITE (UnAD,FmtText  )  ', DISPLACEMENT'
WRITE (UnAD,FmtTR    )  ', FUNCTION = ', PtfmHeave ! Lock platform heave at initial position PtfmHeave


   ! Lock the platform rotational DOFs with a JPRIM statement at the start of
   !   the simulation.  This JPRIM is DEACTIVATED after the first time step if
   !   the rotational DOFs are enabled (collectively).

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''PtfmOrientationLocked_JP'''
WRITE (UnAD,FmtText  )  'JPRIM/1000'
WRITE (UnAD,FmtText  )  ', I = 1000'
WRITE (UnAD,FmtText  )  ', J = 11'
WRITE (UnAD,FmtText  )  ', ORIENTATION'



   ! Always start out with all of the tower elements FIXED to one another:
   ! These FIXED JOINTs are DEACTIVATED after the first time step in the
   !   ADAMS control file (.acf) if tower flexibility is enabled.  See
   !   SUBROUTINE MakeACF for details on this.
   ! It is necessary to lock the flexible elements together at the start
   !   of the simulation since the initial rotor speed kicks the system
   !   to intensly during the initial condition solution.  Locking the
   !   elements together during the initial condition solution eliminates
   !   this problem.

DO J = 1,TwrNodes ! Loop through the tower nodes/elements

   TmpID  = 1400 + J - 1   ! ID of the MARKER on the top    of tower element J - 1.
   TmpID2 = 1300 + J       ! ID of the MARKER on the bottom of tower element J.
   WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''TwrSec', J, 'ToTwrSecBelow_J'''
   WRITE (UnAD,FmtText     )  'JOINT/'//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText     )  ', FIXED'
   WRITE (UnAD,FmtText     )  ', I = '//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText     )  ', J = '//TRIM(Int2LStr( TmpID  ))

ENDDO             ! J - Tower nodes/elements

TmpID  = 1400 + TwrNodes   ! ID of the MARKER on the top    of the uppermost tower element.
TmpID2 = 1500              ! ID of the MARKER on the bottom of the tower top.
WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TowerTopToTwrSecBelow_J'''
WRITE (UnAD,FmtText  )  'JOINT/'//TRIM(Int2LStr( TmpID2 ))
WRITE (UnAD,FmtText  )  ', FIXED'
WRITE (UnAD,FmtText  )  ', I = '//TRIM(Int2LStr( TmpID2 ))
WRITE (UnAD,FmtText  )  ', J = '//TRIM(Int2LStr( TmpID  ))


   ! Yaw Bearing:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''YawBr_J'''
WRITE (UnAD,FmtText  )  'JOINT/2010'   ! NOTE:the (+) rotation about this joint is in the (-) yaw direction!
WRITE (UnAD,FmtText  )  ', REVOLUTE'
WRITE (UnAD,FmtText  )  ', I = 2010'
WRITE (UnAD,FmtText  )  ', J = 1010'


   ! Lock the yaw position with a MOTION statement at the start
   !   of the simulation.  This MOTION is DEACTIVATED after the
   !   first time step if yaw motion is enabled.

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''YawBrLocked_MO'''
WRITE (UnAD,FmtText  )  'MOTION/2010'
WRITE (UnAD,FmtText  )  ', JOINT = 2010'
WRITE (UnAD,FmtText  )  ', ROTATION'
WRITE (UnAD,FmtText  )  ', DISPLACEMENT'
WRITE (UnAD,FmtTR    )  ', FUNCTION = ', -NacYaw   ! Lock yaw at initial position NacYaw


   ! Tail-Furl Bearing:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TFrlBr_J'''
WRITE (UnAD,FmtText  )  'JOINT/5040'
WRITE (UnAD,FmtText  )  ', REVOLUTE'
WRITE (UnAD,FmtText  )  ', I = 5040'
WRITE (UnAD,FmtText  )  ', J = 2040'


   ! Lock the tail-furl position with a MOTION statement at the start
   !   of the simulation.  This MOTION is DEACTIVATED after the
   !   first time step if tail-furl motion is enabled.

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TFrlBrLocked_MO'''
WRITE (UnAD,FmtText  )  'MOTION/5040'
WRITE (UnAD,FmtText  )  ', JOINT = 5040'
WRITE (UnAD,FmtText  )  ', ROTATION'
WRITE (UnAD,FmtText  )  ', DISPLACEMENT'
WRITE (UnAD,FmtTR    )  ', FUNCTION = ', TailFurl  ! Lock tail-furl angle at initial position TailFurl


   ! The tail boom is FIXED to the tail fin:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TailBoomFin_J'''
WRITE (UnAD,FmtText  )  'JOINT/5100'
WRITE (UnAD,FmtText  )  ', FIXED'
WRITE (UnAD,FmtText  )  ', I = 5100'
WRITE (UnAD,FmtText  )  ', J = 5000'


   ! Rotor-Furl Bearing:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''RFrlBr_J'''
WRITE (UnAD,FmtText  )  'JOINT/2130'
WRITE (UnAD,FmtText  )  ', REVOLUTE'
WRITE (UnAD,FmtText  )  ', I = 2130'
WRITE (UnAD,FmtText  )  ', J = 2030'


   ! Lock the rotor-furl position with a MOTION statement at the start
   !   of the simulation.  This MOTION is DEACTIVATED after the
   !   first time step if rotor-furl motion is enabled.

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''RFrlBrLocked_MO'''
WRITE (UnAD,FmtText  )  'MOTION/2130'
WRITE (UnAD,FmtText  )  ', JOINT = 2130'
WRITE (UnAD,FmtText  )  ', ROTATION'
WRITE (UnAD,FmtText  )  ', DISPLACEMENT'
WRITE (UnAD,FmtTR    )  ', FUNCTION = ', RotFurl   ! Lock rotor-furl angle at initial position RotFurl


   ! Those on the shaft:

   ! High-speed shaft bearing (w/o friction):

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''HSSBr_J'''
WRITE (UnAD,FmtText  )  'JOINT/3150'
WRITE (UnAD,FmtText  )  ', REVOLUTE'
WRITE (UnAD,FmtText  )  ', I = 3150'
WRITE (UnAD,FmtText  )  ', J = 2050'


   ! Force the generator to spin at a constant speed with a MOTION
   !   statement at the start of the simulation.  This MOTION is
   !   DEACTIVATED after the first time step if GenDOF is enabled.

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''HSSBrCnstntSpd_MO'''
WRITE (UnAD,FmtText  )  'MOTION/3150'
WRITE (UnAD,FmtText  )  ', JOINT = 3150'
WRITE (UnAD,FmtText  )  ', ROTATION'
WRITE (UnAD,FmtText  )  ', VELOCITY'
WRITE (UnAD,FmtTR    )  ', ICDISP = ', MOD( Azimuth - AzimB1Up + 180.0, 360.0 )*D2R
WRITE (UnAD,FmtTR    )  ', FUNCTION = ', RotSpeed


   ! Low-speed shaft to high-speed shaft connection:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''LSSHSS_J'''
WRITE (UnAD,FmtText  )  'JOINT/3020'
WRITE (UnAD,FmtText  )  ', REVOLUTE'
WRITE (UnAD,FmtText  )  ', I = 3020'
WRITE (UnAD,FmtText  )  ', J = 3120'


   ! Lock the LSS to the HSS with a MOTION statement at the start
   !   of the simulation.  This MOTION is DEACTIVATED after the
   !   first time step if DrTrDOF is enabled.

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''LSSHSSLocked_MO'''
WRITE (UnAD,FmtText  )  'MOTION/3020'
WRITE (UnAD,FmtText  )  ', JOINT = 3020'
WRITE (UnAD,FmtText  )  ', ROTATION'
WRITE (UnAD,FmtText  )  ', DISPLACEMENT'
WRITE (UnAD,FmtText  )  ', FUNCTION = 0'  ! Lock LSS to HSS.


   ! The high-speed shaft is FIXED to the generator:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''HSSGen_J'''
WRITE (UnAD,FmtText  )  'JOINT/3200'
WRITE (UnAD,FmtText  )  ', FIXED'
WRITE (UnAD,FmtText  )  ', I = 3200'
WRITE (UnAD,FmtText  )  ', J = 3100'


   ! The low-speed shaft is FIXED to the teeter pin:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''LSSTeetPin_J'''
WRITE (UnAD,FmtText  )  'JOINT/3300'
WRITE (UnAD,FmtText  )  ', FIXED'
WRITE (UnAD,FmtText  )  ', I = 3300'
WRITE (UnAD,FmtText  )  ', J = 3030'


   ! Teeter Bearing:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TeetBr_J'''
WRITE (UnAD,FmtText  )  'JOINT/4010'
WRITE (UnAD,FmtText  )  ', REVOLUTE'
WRITE (UnAD,FmtText  )  ', I = 4010'
WRITE (UnAD,FmtText  )  ', J = 3310'


   ! Lock the teeter position with a MOTION statement at the start
   !   of the simulation.  This MOTION is DEACTIVATED after the
   !   first time step if teeter motion is enabled.

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TeetBrLocked_MO'''
WRITE (UnAD,FmtText  )  'MOTION/4010'
WRITE (UnAD,FmtText  )  ', JOINT = 4010'
WRITE (UnAD,FmtText  )  ', ROTATION'
WRITE (UnAD,FmtText  )  ', DISPLACEMENT'
WRITE (UnAD,FmtTR    )  ', FUNCTION = ', TeetDefl  ! Lock teeter angle at initial position TeetDefl.


   ! Pitch bearings:

DO K = 1,NumBl ! Loop through all blades

   TmpID  = 10000*K
   TmpID2 = 4010 + K
   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''PitchBr', K, '_J'''
   WRITE (UnAD,FmtText   )  'JOINT/'//TRIM(Int2LStr( TmpID  )) ! NOTE:the (+) rotation about this joint is in the (-) pitch direction!
   WRITE (UnAD,FmtText   )  ', REVOLUTE'
   WRITE (UnAD,FmtText   )  ', I = '//TRIM(Int2LStr( TmpID  ))
   WRITE (UnAD,FmtText   )  ', J = '//TRIM(Int2LStr( TmpID2 ))

   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''PitchBr', K, '_MO'''
   WRITE (UnAD,FmtText   )  'MOTION/'//TRIM(Int2LStr( TmpID  ))
   WRITE (UnAD,FmtText   )  ', JOINT = '//TRIM(Int2LStr( TmpID  ))
   WRITE (UnAD,FmtText   )  ', ROTATION'
   WRITE (UnAD,FmtText   )  ', DISPLACEMENT'
   WRITE (UnAD,FmtTR     )  ', FUNCTION = ', -BlPitch(K)

ENDDO          ! K - Blades


   ! Always start out with all of the blade elements FIXED to one another:
   ! These FIXED JOINTs are DEACTIVATED after the first time step in the
   !   ADAMS control file (.acf) if blade flexibility is enabled.  See
   !   SUBROUTINE MakeACF for details on this.
   ! It is necessary to lock the flexible elements together at the start
   !   of the simulation since the initial rotor speed kicks the system
   !   to intensly during the initial condition solution.  Locking the
   !   elements together during the initial condition solution eliminates
   !   this problem.

DO K = 1,NumBl ! Loop through all blades

   DO J = 1,BldNodes ! Loop through the blade nodes/elements
      TmpID  = 10000*K + 4000 + J - 1  ! ID of the MARKER on the top    of blade element J - 1.
      TmpID2 = 10000*K + 3000 + J      ! ID of the MARKER on the bottom of blade element J.
      WRITE (UnAD,'(A,I1,A,I2.2,A)')  '!                             adams_view_name=''Bld', K, 'Sec', J, 'ToBldSecBelow_J'''
      WRITE (UnAD,FmtText          )  'JOINT/'//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText          )  ', FIXED'
      WRITE (UnAD,FmtText          )  ', I = '//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText          )  ', J = '//TRIM(Int2LStr( TmpID  ))
   ENDDO             ! J - Blade nodes/elements

   TmpID  = 10000*K + 4000 + BldNodes  ! ID of the MARKER on the top    of the outermost blade element.
   TmpID2 = 10000*K + 5000             ! ID of the MARKER on the bottom of the tip brake.
   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''TipBrake', K, 'TobldSecBelow_J'''
   WRITE (UnAD,FmtText   )  'JOINT/'//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText   )  ', FIXED'
   WRITE (UnAD,FmtText   )  ', I = '//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText   )  ', J = '//TRIM(Int2LStr( TmpID  ))

ENDDO          ! K - Blades




   ! Begin defining forces (i.e., SPRINGDAMPERs, FIELDS, GFORCEs, and SFORCEs):

WRITE (UnAD,FmtText  )  '!=================================== FORCES ===================================='



   ! Support Platform:

WRITE (UnAD,FmtText  )  '!------------------------------ Support Platform -------------------------------'

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!remove6.02aSELECT CASE ( PtfmLdMod )  ! Which platform loading model are we using?
!remove6.02a
!remove6.02aCASE ( 0 )                 ! None!
!remove6.02a
!remove6.02a   ! Do nothing here!
!remove6.02a
!remove6.02a
!remove6.02aCASE ( 1 )                 ! User-defined platform loading.
!remove6.02a
!remove6.02a   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''PlatformLoading_GF'''
!remove6.02a   WRITE (UnAD,FmtText  )  'GFORCE/1000'
!remove6.02a   WRITE (UnAD,FmtText  )  ', I = 1000'
!remove6.02a   WRITE (UnAD,FmtText  )  ', JFLOAT = 800'
!remove6.02a   WRITE (UnAD,FmtText  )  ', RM = 1000'
!remove6.02a   WRITE (UnAD,FmtText  )  ', FUNCTION = USER( '//TRIM(Int2LStr( PtfmLdMod ))//' )'
!remove6.02a
!remove6.02a
!remove6.02aENDSELECT
SELECT CASE ( PtfmModel )  ! Which platform model are we using?

CASE ( 0 )                 ! None!


   ! Do nothing here!


CASE ( 1 )                 ! Onshore.


   SELECT CASE ( PtfmLdMod )  ! Which platform loading model are we using?

   CASE ( 0 )                 ! None!

   ! Do nothing here!


   CASE ( 1 )                 ! User-defined platform loading.

      WRITE (UnAD,FmtText  )  '!                             adams_view_name=''PlatformLoading_GF'''
      WRITE (UnAD,FmtText  )  'GFORCE/1000'
      WRITE (UnAD,FmtText  )  ', I = 1000'
      WRITE (UnAD,FmtText  )  ', JFLOAT = 800'
      WRITE (UnAD,FmtText  )  ', RM = 1000'
      WRITE (UnAD,FmtText  )  ', FUNCTION = USER( '//TRIM(Int2LStr( PtfmModel ))//', '//TRIM(Int2LStr( PtfmLdMod ))//' )'


   ENDSELECT


CASE ( 2 )                 ! Fixed bottom offshore.


   SELECT CASE ( PtfmLdMod )  ! Which platform loading model are we using?

   CASE ( 0 )                 ! None!

   ! Do nothing here!


   CASE ( 1 )                 ! User-defined platform loading.

      WRITE (UnAD,FmtText  )  '!                             adams_view_name=''PlatformLoading_GF'''
      WRITE (UnAD,FmtText  )  'GFORCE/1000'
      WRITE (UnAD,FmtText  )  ', I = 1000'
      WRITE (UnAD,FmtText  )  ', JFLOAT = 800'
      WRITE (UnAD,FmtText  )  ', RM = 1000'
      WRITE (UnAD,FmtText  )  ', FUNCTION = USER( '//TRIM(Int2LStr( PtfmModel ))//', '//TRIM(Int2LStr( PtfmLdMod ))//' )'


   ENDSELECT


CASE ( 3 )                 ! Floating offshore.


   SELECT CASE ( PtfmLdMod )  ! Which platform loading model are we using?

   CASE ( 0 )                 ! None!

   ! Do nothing here!


   CASE ( 1 )                 ! User-defined platform loading.

      WRITE (UnAD,FmtText  )  '!                             adams_view_name=''PlatformLoading_GF'''
      WRITE (UnAD,FmtText  )  'GFORCE/1000'
      WRITE (UnAD,FmtText  )  ', I = 1000'
      WRITE (UnAD,FmtText  )  ', JFLOAT = 800'
      WRITE (UnAD,FmtText  )  ', RM = 1000'
      WRITE (UnAD,FmtText  )  ', FUNCTION = USER( '//TRIM(Int2LStr( PtfmModel ))//', '//TRIM(Int2LStr( PtfmLdMod ))//' )'


   CASE ( 9999 )              ! Undocumented loading for a floating platform.

      WRITE (UnAD,FmtText  )  '!                             adams_view_name=''PlatformLoading_GF'''
      WRITE (UnAD,FmtText  )  'GFORCE/1000'
      WRITE (UnAD,FmtText  )  ', I = 1000'
      WRITE (UnAD,FmtText  )  ', JFLOAT = 800'
      WRITE (UnAD,FmtText  )  ', RM = 1000'
      WRITE (UnAD,FmtText  )  ', FUNCTION = USER( '//TRIM(Int2LStr( PtfmModel ))//', '//TRIM(Int2LStr( PtfmLdMod ))// &
                              ', '//TRIM(Flt2LStr( PtfmVol0    ))//', '//TRIM(Int2LStr( PtfmNodes   ))//              &
                              ', '//TRIM(Flt2LStr( PtfmDraft   ))//', '//TRIM(Flt2LStr( PtfmDiam    ))//              &
                              ', '//TRIM(Flt2LStr( PtfmCD      ))//', '//TRIM(Flt2LStr( RdtnTMax    ))//              &
                              ', '//TRIM(Flt2LStr( RdtnDT      ))//', '//TRIM(Flt2LStr( WtrDens     ))//','
      WRITE (UnAD,FmtText  )  ', '//TRIM(Flt2LStr( WtrDpth     ))//', '//TRIM(Int2LStr( WaveMod     ))//              &
                              ', '//TRIM(Flt2LStr( WaveTMax    ))//', '//TRIM(Flt2LStr( WaveDT      ))//              &
                              ', '//TRIM(Flt2LStr( WaveHs      ))//', '//TRIM(Flt2LStr( WaveTp      ))//              &
                              ', '//TRIM(Flt2LStr( WavePkShp   ))//', '//TRIM(Flt2LStr( WaveDir     ))//              &
                              ', '//TRIM(Int2LStr( WaveSeed(1) ))//', '//TRIM(Int2LStr( WaveSeed(2) ))//','
      WRITE (UnAD,FmtText  )  ', '//TRIM(Int2LStr( CurrMod     ))//', '//TRIM(Flt2LStr( CurrSSV0    ))//              &
                              ', '//TRIM(Flt2LStr( CurrSSDir   ))//', '//TRIM(Flt2LStr( CurrNSRef   ))//              &
                              ', '//TRIM(Flt2LStr( CurrNSV0    ))//', '//TRIM(Flt2LStr( CurrNSDir   ))//              &
                              ', '//TRIM(Flt2LStr( CurrDIV     ))//', '//TRIM(Flt2LStr( CurrDIDir   ))//              &
                              ', '//TRIM(Flt2LStr( Gravity     ))//', '//TRIM(Int2LStr( NFreeSrfc   ))//' )'
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:


   ! Create an ARRAY statement to hold the mooring line data:

      WRITE (UnAD,FmtText           )  '!                             adams_view_name=''MooringLines_A'''
      WRITE (UnAD,FmtText           )  'ARRAY/1000'

      IF ( LineMod == 1 )  THEN  ! .TRUE if we have standard quasi-static mooring lines; store the mooring line data into the ARRAY

         WRITE (UnAD,FmtText        )  ', IC, SIZE = '//TRIM(Int2LStr( 12*NumLines + 1 )) ! Specify a list of constants.  The ARRAY contains all 12 columns of mooring line data for each of the NumLines mooring lines + 1 extra element to contain the value of NLnNodes
         DO I = 1,NumLines ! Loop through all mooring lines
            IF ( I == 1        )  THEN ! First line
               WRITE (UnAD,FmtText  )  ', NUMBERS = '//TRIM(Flt2LStr( LAnchxi  (I) ))//','
            ELSE                       ! All other lines
               WRITE (UnAD,FmtText  )  ', '          //TRIM(Flt2LStr( LAnchxi  (I) ))//','
            ENDIF
         ENDDO             ! I - All mooring lines
         DO I = 1,NumLines ! Loop through all mooring lines
            WRITE (UnAD,FmtText     )  ', '          //TRIM(Flt2LStr( LAnchyi  (I) ))//','
         ENDDO             ! I - All mooring lines
         DO I = 1,NumLines ! Loop through all mooring lines
            WRITE (UnAD,FmtText     )  ', '          //TRIM(Flt2LStr( LAnchzi  (I) ))//','
         ENDDO             ! I - All mooring lines
         DO I = 1,NumLines ! Loop through all mooring lines
            WRITE (UnAD,FmtText     )  ', '          //TRIM(Flt2LStr( LFairxt  (I) ))//','
         ENDDO             ! I - All mooring lines
         DO I = 1,NumLines ! Loop through all mooring lines
            WRITE (UnAD,FmtText     )  ', '          //TRIM(Flt2LStr( LFairyt  (I) ))//','
         ENDDO             ! I - All mooring lines
         DO I = 1,NumLines ! Loop through all mooring lines
            WRITE (UnAD,FmtText     )  ', '          //TRIM(Flt2LStr( LFairzt  (I) ))//','
         ENDDO             ! I - All mooring lines
         DO I = 1,NumLines ! Loop through all mooring lines
            WRITE (UnAD,FmtText     )  ', '          //TRIM(Flt2LStr( LUnstrLen(I) ))//','
         ENDDO             ! I - All mooring lines
         DO I = 1,NumLines ! Loop through all mooring lines
            WRITE (UnAD,FmtText     )  ', '          //TRIM(Flt2LStr( LDiam    (I) ))//','
         ENDDO             ! I - All mooring lines
         DO I = 1,NumLines ! Loop through all mooring lines
            WRITE (UnAD,FmtText     )  ', '          //TRIM(Flt2LStr( LMassDen (I) ))//','
         ENDDO             ! I - All mooring lines
         DO I = 1,NumLines ! Loop through all mooring lines
            WRITE (UnAD,FmtText     )  ', '          //TRIM(Flt2LStr( LEAStff  (I) ))//','
         ENDDO             ! I - All mooring lines
         DO I = 1,NumLines ! Loop through all mooring lines
            WRITE (UnAD,FmtText     )  ', '          //TRIM(Flt2LStr( LSeabedCD(I) ))//','
         ENDDO             ! I - All mooring lines
         DO I = 1,NumLines ! Loop through all mooring lines
            WRITE (UnAD,FmtText     )  ', '          //TRIM(Flt2LStr( LTenTol  (I) ))//','
         ENDDO             ! I - All mooring lines
         IF ( SaveGrphcs )  THEN ! .TRUE. if GRAPHICS output is saved in an ADAMS analysis.
            WRITE (UnAD,FmtText     )  ', '          //TRIM(Int2LStr( NLnNodes     ))
         ELSE                    ! No GRAPHICS output; thus, there are no line nodes to be viewed
            WRITE (UnAD,FmtText     )  ', 0'
         ENDIF

      ELSE                       ! We must have user-defined or no mooring lines; store NumLines as the sole element in the ARRAY

         WRITE (UnAD,FmtText        )  ', IC, SIZE = 1'
         WRITE (UnAD,FmtText        )  ', NUMBERS = '//TRIM(Int2LStr( NumLines ))

      ENDIF
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.


   ENDSELECT


ENDSELECT
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! Tower segment stiffness and damping:

WRITE (UnAD,FmtText  )  '!------------------------------------ Tower ------------------------------------'

CRatioTFA = 0.01*TwrFADmp(1)/( Pi*FreqTFA(1,1) )   ! Use the same damping ratio as used for the first FA mode in FAST.
CRatioTSS = 0.01*TwrSSDmp(1)/( Pi*FreqTSS(1,1) )   ! Use the same damping ratio as used for the first SS mode in FAST.
TmpID  = 1400                                      ! ID of the MARKER on the top    of the tower rigid base.
TmpID2 = 1100 + 1                                  ! ID of the MARKER in the middle of     tower element 1.
TmpLength  = 0.5*DHNodes(1)                        ! Distance between tower node 1 and the top of the tower rigid base.
TmpLength2 = TmpLength*TmpLength                   ! = TmpLength^2.
TmpLength3 = TmpLength*TmpLength2                  ! = TmpLength^3.
KMatrix      =  0.0                                ! Initialize KMatrix to zero.
KMatrix(1,1) =  0.001*     StiffTEA(1)/TmpLength   ! Use the same KMatrix specified in the ADAMS/WT user's guide.
KMatrix(2,2) =  0.001*12.0*StiffTSS(1)/TmpLength3
KMatrix(3,3) =  0.001*12.0*StiffTFA(1)/TmpLength3
KMatrix(4,4) =  0.001*     StiffTGJ(1)/TmpLength
KMatrix(5,5) =  0.001* 4.0*StiffTFA(1)/TmpLength
KMatrix(6,6) =  0.001* 4.0*StiffTSS(1)/TmpLength
KMatrix(2,6) = -0.001* 6.0*StiffTSS(1)/TmpLength2
KMatrix(3,5) =  0.001* 6.0*StiffTFA(1)/TmpLength2
KMatrix(6,2) = KMatrix(2,6)
KMatrix(5,3) = KMatrix(3,5)
CMatrix      =  0.0                                ! Initialize CMatrix to zero.
CMatrix(1,1) = KMatrix(1,1)*CRatioTEA              ! Scale the  KMatrix to form the CMatrix.
CMatrix(2,2) = KMatrix(2,2)*CRatioTSS
CMatrix(3,3) = KMatrix(3,3)*CRatioTFA
CMatrix(4,4) = KMatrix(4,4)*CRatioTGJ
CMatrix(5,5) = KMatrix(5,5)*CRatioTFA
CMatrix(6,6) = KMatrix(6,6)*CRatioTSS
CMatrix(2,6) = KMatrix(2,6)*CRatioTSS
CMatrix(3,5) = KMatrix(3,5)*CRatioTFA
CMatrix(6,2) = CMatrix(2,6)
CMatrix(5,3) = CMatrix(3,5)

WRITE (UnAD,'(A,I2.2,A)'   )  '!                             adams_view_name=''TwrSec', 1, 'ToTwrSecBelow_F'''
WRITE (UnAD,FmtText        )  'FIELD/'//TRIM(Int2LStr( TmpID2 ))
WRITE (UnAD,FmtText        )  ', I = '//TRIM(Int2LStr( TmpID2 ))
WRITE (UnAD,FmtText        )  ', J = '//TRIM(Int2LStr( TmpID  ))
WRITE (UnAD,FmtText        )  ', CMATRIX = '
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,1), ', ', CMatrix(2,1), ', ', CMatrix(3,1), &
                              ', ', CMatrix(4,1), ', ', CMatrix(5,1), ', ', CMatrix(6,1)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,2), ', ', CMatrix(2,2), ', ', CMatrix(3,2), &
                              ', ', CMatrix(4,2), ', ', CMatrix(5,2), ', ', CMatrix(6,2)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,3), ', ', CMatrix(2,3), ', ', CMatrix(3,3), &
                              ', ', CMatrix(4,3), ', ', CMatrix(5,3), ', ', CMatrix(6,3)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,4), ', ', CMatrix(2,4), ', ', CMatrix(3,4), &
                              ', ', CMatrix(4,4), ', ', CMatrix(5,4), ', ', CMatrix(6,4)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,5), ', ', CMatrix(2,5), ', ', CMatrix(3,5), &
                              ', ', CMatrix(4,5), ', ', CMatrix(5,5), ', ', CMatrix(6,5)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,6), ', ', CMatrix(2,6), ', ', CMatrix(3,6), &
                              ', ', CMatrix(4,6), ', ', CMatrix(5,6), ', ', CMatrix(6,6)
WRITE (UnAD,FmtText        )  ', KMATRIX = '
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,1), ', ', KMatrix(2,1), ', ', KMatrix(3,1), &
                              ', ', KMatrix(4,1), ', ', KMatrix(5,1), ', ', KMatrix(6,1)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,2), ', ', KMatrix(2,2), ', ', KMatrix(3,2), &
                              ', ', KMatrix(4,2), ', ', KMatrix(5,2), ', ', KMatrix(6,2)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,3), ', ', KMatrix(2,3), ', ', KMatrix(3,3), &
                              ', ', KMatrix(4,3), ', ', KMatrix(5,3), ', ', KMatrix(6,3)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,4), ', ', KMatrix(2,4), ', ', KMatrix(3,4), &
                              ', ', KMatrix(4,4), ', ', KMatrix(5,4), ', ', KMatrix(6,4)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,5), ', ', KMatrix(2,5), ', ', KMatrix(3,5), &
                              ', ', KMatrix(4,5), ', ', KMatrix(5,5), ', ', KMatrix(6,5)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,6), ', ', KMatrix(2,6), ', ', KMatrix(3,6), &
                              ', ', KMatrix(4,6), ', ', KMatrix(5,6), ', ', KMatrix(6,6)
WRITE (UnAD,FmtTRTRTR      )  ', LENGTH = ', TmpLength, ', 0.0, 0.0, ', 0.0, ', 0.0, ', 0.0


DO J = 2,TwrNodes ! Loop through all but the innermost tower nodes/elements


   TmpID  = 1100 + J - 1                           ! ID of the MARKER in the middle of tower element J - 1.
   TmpID2 = 1100 + J                               ! ID of the MARKER in the middle of tower element J.
   TmpLength  = 0.5*( DHNodes(J) + DHNodes(J-1) )  ! Distance between tower node J and node J-1.
   TmpLength2 = TmpLength*TmpLength                ! = TmpLength^2.
   TmpLength3 = TmpLength*TmpLength2               ! = TmpLength^3.
   KMatrix      =  0.0                             ! Initialize KMatrix to zero.
   KMatrix(1,1) =  0.001*0.5*(     StiffTEA(J) + StiffTEA(J-1) )/TmpLength    ! Use the same KMatrix specified in the ADAMS/WT user's guide.
   KMatrix(2,2) =  0.001*6.0*(     StiffTSS(J) + StiffTSS(J-1) )/TmpLength3
   KMatrix(3,3) =  0.001*6.0*(     StiffTFA(J) + StiffTFA(J-1) )/TmpLength3
   KMatrix(4,4) =  0.001*0.5*(     StiffTGJ(J) + StiffTGJ(J-1) )/TmpLength
   KMatrix(5,5) =  0.001*    ( 3.0*StiffTFA(J) + StiffTFA(J-1) )/TmpLength
   KMatrix(6,6) =  0.001*    ( 3.0*StiffTSS(J) + StiffTSS(J-1) )/TmpLength
   KMatrix(2,6) = -0.001*2.0*( 2.0*StiffTSS(J) + StiffTSS(J-1) )/TmpLength2
   KMatrix(3,5) =  0.001*2.0*( 2.0*StiffTFA(J) + StiffTFA(J-1) )/TmpLength2
   KMatrix(6,2) = KMatrix(2,6)
   KMatrix(5,3) = KMatrix(3,5)
   CMatrix      =  0.0                             ! Initialize CMatrix to zero.
   CMatrix(1,1) = KMatrix(1,1)*CRatioTEA           ! Scale the  KMatrix to form the CMatrix.
   CMatrix(2,2) = KMatrix(2,2)*CRatioTSS
   CMatrix(3,3) = KMatrix(3,3)*CRatioTFA
   CMatrix(4,4) = KMatrix(4,4)*CRatioTGJ
   CMatrix(5,5) = KMatrix(5,5)*CRatioTFA
   CMatrix(6,6) = KMatrix(6,6)*CRatioTSS
   CMatrix(2,6) = KMatrix(2,6)*CRatioTSS
   CMatrix(3,5) = KMatrix(3,5)*CRatioTFA
   CMatrix(6,2) = CMatrix(2,6)
   CMatrix(5,3) = CMatrix(3,5)

   WRITE (UnAD,'(A,I2.2,A)'   )  '!                             adams_view_name=''TwrSec', J, 'ToTwrSecBelow_F'''
   WRITE (UnAD,FmtText        )  'FIELD/'//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText        )  ', I = '//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText        )  ', J = '//TRIM(Int2LStr( TmpID  ))
   WRITE (UnAD,FmtText        )  ', CMATRIX = '
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,1), ', ', CMatrix(2,1), ', ', CMatrix(3,1), &
                                 ', ', CMatrix(4,1), ', ', CMatrix(5,1), ', ', CMatrix(6,1)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,2), ', ', CMatrix(2,2), ', ', CMatrix(3,2), &
                                 ', ', CMatrix(4,2), ', ', CMatrix(5,2), ', ', CMatrix(6,2)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,3), ', ', CMatrix(2,3), ', ', CMatrix(3,3), &
                                 ', ', CMatrix(4,3), ', ', CMatrix(5,3), ', ', CMatrix(6,3)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,4), ', ', CMatrix(2,4), ', ', CMatrix(3,4), &
                                 ', ', CMatrix(4,4), ', ', CMatrix(5,4), ', ', CMatrix(6,4)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,5), ', ', CMatrix(2,5), ', ', CMatrix(3,5), &
                                 ', ', CMatrix(4,5), ', ', CMatrix(5,5), ', ', CMatrix(6,5)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,6), ', ', CMatrix(2,6), ', ', CMatrix(3,6), &
                                 ', ', CMatrix(4,6), ', ', CMatrix(5,6), ', ', CMatrix(6,6)
   WRITE (UnAD,FmtText        )  ', KMATRIX = '
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,1), ', ', KMatrix(2,1), ', ', KMatrix(3,1), &
                                 ', ', KMatrix(4,1), ', ', KMatrix(5,1), ', ', KMatrix(6,1)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,2), ', ', KMatrix(2,2), ', ', KMatrix(3,2), &
                                 ', ', KMatrix(4,2), ', ', KMatrix(5,2), ', ', KMatrix(6,2)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,3), ', ', KMatrix(2,3), ', ', KMatrix(3,3), &
                                 ', ', KMatrix(4,3), ', ', KMatrix(5,3), ', ', KMatrix(6,3)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,4), ', ', KMatrix(2,4), ', ', KMatrix(3,4), &
                                 ', ', KMatrix(4,4), ', ', KMatrix(5,4), ', ', KMatrix(6,4)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,5), ', ', KMatrix(2,5), ', ', KMatrix(3,5), &
                                 ', ', KMatrix(4,5), ', ', KMatrix(5,5), ', ', KMatrix(6,5)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,6), ', ', KMatrix(2,6), ', ', KMatrix(3,6), &
                                 ', ', KMatrix(4,6), ', ', KMatrix(5,6), ', ', KMatrix(6,6)
   WRITE (UnAD,FmtTRTRTR      )  ', LENGTH = ', TmpLength, ', 0.0, 0.0, ', 0.0, ', 0.0, ', 0.0


ENDDO             ! J - Tower nodes/elements


TmpID  = 1100 + TwrNodes               ! ID of the MARKER in the middle of tower element TwrNodes.
TmpID2 = 1500                          ! ID of the MARKER on the bottom of the tower top.
TmpLength  = 0.5*DHNodes(TwrNodes)     ! Distance between tower node TwrNodes and the tower top.
TmpLength2 = TmpLength*TmpLength       ! = TmpLength^2.
TmpLength3 = TmpLength*TmpLength2      ! = TmpLength^3.
KMatrix      =  0.0                    ! Initialize KMatrix to zero.
KMatrix(1,1) =  0.001*     StiffTEA(TwrNodes)/TmpLength  ! Use the same KMatrix specified in the ADAMS/WT user's guide.
KMatrix(2,2) =  0.001*12.0*StiffTSS(TwrNodes)/TmpLength3
KMatrix(3,3) =  0.001*12.0*StiffTFA(TwrNodes)/TmpLength3
KMatrix(4,4) =  0.001*     StiffTGJ(TwrNodes)/TmpLength
KMatrix(5,5) =  0.001* 4.0*StiffTFA(TwrNodes)/TmpLength
KMatrix(6,6) =  0.001* 4.0*StiffTSS(TwrNodes)/TmpLength
KMatrix(2,6) = -0.001* 6.0*StiffTSS(TwrNodes)/TmpLength2
KMatrix(3,5) =  0.001* 6.0*StiffTFA(TwrNodes)/TmpLength2
KMatrix(6,2) = KMatrix(2,6)
KMatrix(5,3) = KMatrix(3,5)
CMatrix      =  0.0                    ! Initialize CMatrix to zero.
CMatrix(1,1) = KMatrix(1,1)*CRatioTEA  ! Scale the  KMatrix to form the CMatrix.
CMatrix(2,2) = KMatrix(2,2)*CRatioTSS
CMatrix(3,3) = KMatrix(3,3)*CRatioTFA
CMatrix(4,4) = KMatrix(4,4)*CRatioTGJ
CMatrix(5,5) = KMatrix(5,5)*CRatioTFA
CMatrix(6,6) = KMatrix(6,6)*CRatioTSS
CMatrix(2,6) = KMatrix(2,6)*CRatioTSS
CMatrix(3,5) = KMatrix(3,5)*CRatioTFA
CMatrix(6,2) = CMatrix(2,6)
CMatrix(5,3) = CMatrix(3,5)

WRITE (UnAD,FmtText        )  '!                             adams_view_name=''TowerTopToTwrSecBelow_F'''
WRITE (UnAD,FmtText        )  'FIELD/'//TRIM(Int2LStr( TmpID2 ))
WRITE (UnAD,FmtText        )  ', I = '//TRIM(Int2LStr( TmpID2 ))
WRITE (UnAD,FmtText        )  ', J = '//TRIM(Int2LStr( TmpID  ))
WRITE (UnAD,FmtText        )  ', CMATRIX = '
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,1), ', ', CMatrix(2,1), ', ', CMatrix(3,1), &
                              ', ', CMatrix(4,1), ', ', CMatrix(5,1), ', ', CMatrix(6,1)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,2), ', ', CMatrix(2,2), ', ', CMatrix(3,2), &
                              ', ', CMatrix(4,2), ', ', CMatrix(5,2), ', ', CMatrix(6,2)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,3), ', ', CMatrix(2,3), ', ', CMatrix(3,3), &
                              ', ', CMatrix(4,3), ', ', CMatrix(5,3), ', ', CMatrix(6,3)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,4), ', ', CMatrix(2,4), ', ', CMatrix(3,4), &
                              ', ', CMatrix(4,4), ', ', CMatrix(5,4), ', ', CMatrix(6,4)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,5), ', ', CMatrix(2,5), ', ', CMatrix(3,5), &
                              ', ', CMatrix(4,5), ', ', CMatrix(5,5), ', ', CMatrix(6,5)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,6), ', ', CMatrix(2,6), ', ', CMatrix(3,6), &
                              ', ', CMatrix(4,6), ', ', CMatrix(5,6), ', ', CMatrix(6,6)
WRITE (UnAD,FmtText        )  ', KMATRIX = '
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,1), ', ', KMatrix(2,1), ', ', KMatrix(3,1), &
                              ', ', KMatrix(4,1), ', ', KMatrix(5,1), ', ', KMatrix(6,1)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,2), ', ', KMatrix(2,2), ', ', KMatrix(3,2), &
                              ', ', KMatrix(4,2), ', ', KMatrix(5,2), ', ', KMatrix(6,2)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,3), ', ', KMatrix(2,3), ', ', KMatrix(3,3), &
                              ', ', KMatrix(4,3), ', ', KMatrix(5,3), ', ', KMatrix(6,3)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,4), ', ', KMatrix(2,4), ', ', KMatrix(3,4), &
                              ', ', KMatrix(4,4), ', ', KMatrix(5,4), ', ', KMatrix(6,4)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,5), ', ', KMatrix(2,5), ', ', KMatrix(3,5), &
                              ', ', KMatrix(4,5), ', ', KMatrix(5,5), ', ', KMatrix(6,5)
WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,6), ', ', KMatrix(2,6), ', ', KMatrix(3,6), &
                              ', ', KMatrix(4,6), ', ', KMatrix(5,6), ', ', KMatrix(6,6)
WRITE (UnAD,FmtTRTRTR      )  ', LENGTH = ', TmpLength, ', 0.0, 0.0, ', 0.0, ', 0.0, ', 0.0

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:
   ! Tower aerodynamic and hydrodynamic forces

WRITE (UnAD,FmtText  )  '!------------------------------------ Tower ------------------------------------'

SELECT CASE ( PtfmModel )  ! Which platform model are we using?

CASE ( 0 )                 ! None!


   ! Do nothing here!


CASE ( 1 )                 ! Onshore.


   ! Do nothing here!


CASE ( 2 )                 ! Fixed bottom offshore.


   SELECT CASE ( TwrLdMod )   ! Which tower loading model are we using?

   CASE ( 0 )                 ! None!

   ! Do nothing here!


   CASE ( 1 )                 ! Undocumented hydrodynamic loading using Morison's equation.

      DO J = 1,TwrNodes ! Loop through the tower nodes/elements
         TmpID  =  800 + J ! ID of the FLOATING MARKER of the current tower element.
         TmpID2 = 1800 + J ! ID of the associated aerodynamic and hydrodynamic MARKER fixed in the current tower element.
         WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''TowerSec', J, 'AeroHydro_GF'''
         WRITE (UnAD,FmtText     )  'GFORCE/'//TRIM(Int2LStr( TmpID2 ))
         WRITE (UnAD,FmtText     )  ', I = '//TRIM(Int2LStr( TmpID2 ))
         WRITE (UnAD,FmtText     )  ', JFLOAT = '//TRIM(Int2LStr( TmpID  ))
         WRITE (UnAD,FmtText     )  ', RM = '//TRIM(Int2LStr( TmpID2 ))
         WRITE (UnAD,FmtText     )  ', FUNCTION = USER( '//TRIM(Int2LStr( PtfmModel ))//', '//TRIM(Int2LStr( TwrLdMod ))// &
                                    ', '//TRIM(Int2LStr( TwrNodes    ))//', '//TRIM(Flt2LStr( TwrDraft    ))//             &
                                    ', '//TRIM(Flt2LStr( WtrDens     ))//', '//TRIM(Flt2LStr( WtrDpth     ))//','
         WRITE (UnAD,FmtText     )  ', '//TRIM(Int2LStr( WaveMod     ))//', '//TRIM(Int2LStr( WaveStMod   ))//             &
                                    ', '//TRIM(Flt2LStr( WaveTMax    ))//', '//TRIM(Flt2LStr( WaveDT      ))//             &
                                    ', '//TRIM(Flt2LStr( WaveHs      ))//', '//TRIM(Flt2LStr( WaveTp      ))//             &
                                    ', '//TRIM(Flt2LStr( WavePkShp   ))//', '//TRIM(Flt2LStr( WaveDir     ))//             &
                                    ', '//TRIM(Int2LStr( WaveSeed(1) ))//', '//TRIM(Int2LStr( WaveSeed(2) ))//','
         WRITE (UnAD,FmtText     )  ', '//TRIM(Int2LStr( CurrMod     ))//', '//TRIM(Flt2LStr( CurrSSV0    ))//             &
                                    ', '//TRIM(Flt2LStr( CurrSSDir   ))//', '//TRIM(Flt2LStr( CurrNSRef   ))//             &
                                    ', '//TRIM(Flt2LStr( CurrNSV0    ))//', '//TRIM(Flt2LStr( CurrNSDir   ))//             &
                                    ', '//TRIM(Flt2LStr( CurrDIV     ))//', '//TRIM(Flt2LStr( CurrDIDir   ))//             &
                                    ', '//TRIM(Flt2LStr( Gravity     ))//', '//TRIM(Int2LStr( NFreeSrfc   ))//','
         WRITE (UnAD,FmtText     )  ', '//TRIM(Flt2LStr( DiamT(J)    ))//', '//TRIM(Flt2LStr( CAT(J)      ))//             &
                                    ', '//TRIM(Flt2LStr( CDT(J)      ))                                     //' )'
      ENDDO             ! J - Tower nodes/elements


   CASE ( 2 )                 ! User-defined tower loading.

      DO J = 1,TwrNodes ! Loop through the tower nodes/elements
         TmpID  =  800 + J ! ID of the FLOATING MARKER of the current tower element.
         TmpID2 = 1800 + J ! ID of the associated aerodynamic and hydrodynamic MARKER fixed in the current tower element.
         WRITE (UnAD,'(A,I2.2,A)')  '!                             adams_view_name=''TowerSec', J, 'AeroHydro_GF'''
         WRITE (UnAD,FmtText     )  'GFORCE/'//TRIM(Int2LStr( TmpID2 ))
         WRITE (UnAD,FmtText     )  ', I = '//TRIM(Int2LStr( TmpID2 ))
         WRITE (UnAD,FmtText     )  ', JFLOAT = '//TRIM(Int2LStr( TmpID  ))
         WRITE (UnAD,FmtText     )  ', RM = '//TRIM(Int2LStr( TmpID2 ))
         WRITE (UnAD,FmtText     )  ', FUNCTION = USER( '//TRIM(Int2LStr( PtfmModel ))//', '//TRIM(Int2LStr( TwrLdMod ))// &
                                    ', '//TRIM(Int2LStr( TwrNodes    ))//', '//TRIM(Flt2LStr( TwrDraft    ))//             &
                                    ', '//TRIM(Flt2LStr( WtrDens     ))//', '//TRIM(Flt2LStr( WtrDpth     ))//','
         WRITE (UnAD,FmtText     )  ', '//TRIM(Int2LStr( WaveMod     ))//', '//TRIM(Int2LStr( WaveStMod   ))//             &
                                    ', '//TRIM(Flt2LStr( WaveTMax    ))//', '//TRIM(Flt2LStr( WaveDT      ))//             &
                                    ', '//TRIM(Flt2LStr( WaveHs      ))//', '//TRIM(Flt2LStr( WaveTp      ))//             &
                                    ', '//TRIM(Flt2LStr( WavePkShp   ))//', '//TRIM(Flt2LStr( WaveDir     ))//             &
                                    ', '//TRIM(Int2LStr( WaveSeed(1) ))//', '//TRIM(Int2LStr( WaveSeed(2) ))//','
         WRITE (UnAD,FmtText     )  ', '//TRIM(Int2LStr( CurrMod     ))//', '//TRIM(Flt2LStr( CurrSSV0    ))//             &
                                    ', '//TRIM(Flt2LStr( CurrSSDir   ))//', '//TRIM(Flt2LStr( CurrNSRef   ))//             &
                                    ', '//TRIM(Flt2LStr( CurrNSV0    ))//', '//TRIM(Flt2LStr( CurrNSDir   ))//             &
                                    ', '//TRIM(Flt2LStr( CurrDIV     ))//', '//TRIM(Flt2LStr( CurrDIDir   ))//             &
                                    ', '//TRIM(Flt2LStr( Gravity     ))//', '//TRIM(Int2LStr( NFreeSrfc   ))//' )'
      ENDDO             ! J - Tower nodes/elements


   ENDSELECT


CASE ( 3 )                 ! Floating offshore.


   ! Do nothing here!


ENDSELECT



!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! Nacelle yaw:

WRITE (UnAD,FmtText  )  '!---------------------------------- Bed Plate ----------------------------------'

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''YawPosDemand_V'''
WRITE (UnAD,FmtText  )  'VARIABLE/2011'
WRITE (UnAD,FmtText  )  ', FUNCTION = USER( '//TRIM(Int2LStr( YCMode ))//', '//TRIM(Flt2LStr( TYCOn ))// &
                        ', '//TRIM(Flt2LStr( DT        ))//', '//TRIM(Flt2LStr( TYawManS ))//            &
                        ', '//TRIM(Flt2LStr( TYawManE  ))//', '//TRIM(Flt2LStr( NacYawF  ))//            &
                        ', '//TRIM(Flt2LStr( YawNeut   ))//', '//TRIM(Int2LStr( NumBl    ))//            &
                        ', '//TRIM(Int2LStr( CompAeroI ))//' )'

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''YawRateDemand_V'''
WRITE (UnAD,FmtText  )  'VARIABLE/2012'
WRITE (UnAD,FmtText  )  ', FUNCTION = USER( '//TRIM(Int2LStr( YCMode ))//', '//TRIM(Flt2LStr( TYCOn ))// &
                        ', '//TRIM(Flt2LStr( DT        ))//', '//TRIM(Flt2LStr( TYawManS ))//            &
                        ', '//TRIM(Flt2LStr( TYawManE  ))//', '//TRIM(Flt2LStr( NacYawF  ))//            &
                        ', '//TRIM(Flt2LStr( YawNeut   ))//', '//TRIM(Int2LStr( NumBl    ))//            &
                        ', '//TRIM(Int2LStr( CompAeroI ))//' )'

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''YawPosError_V'''
WRITE (UnAD,FmtText  )  'VARIABLE/2013'
WRITE (UnAD,FmtText  )  ', FUNCTION = VARVAL(2011) - AZ(1010,2010)'

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''YawRateError_V'''
WRITE (UnAD,FmtText  )  'VARIABLE/2014'
WRITE (UnAD,FmtText  )  ', FUNCTION = VARVAL(2012) - WZ(1010,2010,2010)'

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''YawActuator_SF'''
WRITE (UnAD,FmtText  )  'SFORCE/2010'
WRITE (UnAD,FmtText  )  ', ROTATION'
WRITE (UnAD,FmtText  )  ', I = 2010'
WRITE (UnAD,FmtText  )  ', J = 1010'
WRITE (UnAD,FmtText  )  ', FUNCTION = -'//TRIM(Flt2LStr( YawSpr *0.001 ))//'*VARVAL(2013) - '// &
                                          TRIM(Flt2LStr( YawDamp*0.001 ))//'*VARVAL(2014)'



   ! Tail-furl:

WRITE (UnAD,FmtText     )  '!---------------------------------- Tail-Furl ----------------------------------'

SELECT CASE ( TFrlMod ) ! Which tail-furl model are we using?

CASE ( 0 )              ! None!

   ! Do nothing here!


CASE ( 1 )              ! Standard (using inputs from the FAST furling input file).

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TFrl_SD'''
   WRITE (UnAD,FmtText  )  'SPRINGDAMPER/5040'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 5040'
   WRITE (UnAD,FmtText  )  ', J = 2040'
   WRITE (UnAD,FmtTR    )  ', CT = ', 0.001*TFrlDmp
   WRITE (UnAD,FmtTR    )  ', KT = ', 0.001*TFrlSpr
   WRITE (UnAD,FmtText  )  ', TORQUE = 0'          ! The reference torque at ANGLE
   WRITE (UnAD,FmtText  )  ', ANGLE = 0'

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TFrlUSSpr_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/5041'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 5040'
   WRITE (UnAD,FmtText  )  ', J = 2040'
   WRITE (UnAD,FmtText  )  ', FUNCTION = IF( AZ(5040,2040) - '//TRIM(Flt2LStr( TFrlUSSP ))//': 0, 0,'
   WRITE (UnAD,FmtText  )  ', -'//TRIM(Flt2LStr( TFrlUSSpr*0.001 ))//'*( AZ(5040,2040) - '//TRIM(Flt2LStr( TFrlUSSP ))//' ) )'

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TFrlDSSpr_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/5042'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 5040'
   WRITE (UnAD,FmtText  )  ', J = 2040'
   WRITE (UnAD,FmtText  )  ', FUNCTION = IF( AZ(5040,2040) - '//TRIM(Flt2LStr( TFrlDSSP ))//':'
   WRITE (UnAD,FmtText  )  ', -'//TRIM(Flt2LStr( TFrlDSSpr*0.001 ))// &
                           '*( AZ(5040,2040) - '//TRIM(Flt2LStr( TFrlDSSP ))//' ), 0, 0 )'

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TFrlUSDmp_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/5043'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 5040'
   WRITE (UnAD,FmtText  )  ', J = 2040'
   WRITE (UnAD,FmtText  )  ', FUNCTION = IF( AZ(5040,2040) - '//TRIM(Flt2LStr( TFrlUSDP ))// &
                           ': 0, 0, -'//TRIM(Flt2LStr( TFrlUSDmp*0.001 ))//'*WZ(5040,2040,2040) )'

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TFrlDSDmp_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/5044'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 5040'
   WRITE (UnAD,FmtText  )  ', J = 2040'
   WRITE (UnAD,FmtText  )  ', FUNCTION = IF( AZ(5040,2040) - '//TRIM(Flt2LStr( TFrlDSDP ))// &
                           ': -'//TRIM(Flt2LStr( TFrlDSDmp*0.001 ))//'*WZ(5040,2040,2040), 0, 0 )'

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TFrlCoulombDamp_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/5045'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 5040'
   WRITE (UnAD,FmtText  )  ', J = 2040'
   WRITE (UnAD,FmtText  )  ', FUNCTION = IF( WZ(5040,2040,2040): '//TRIM(Flt2LStr( TFrlCDmp*0.001 ))// &
                           ', 0, -'//TRIM(Flt2LStr( TFrlCDmp*0.001 ))//' )'


CASE ( 2 )              ! User-defined tail-furl spring/damper model.

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TFrlMom_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/5040'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 5040'
   WRITE (UnAD,FmtText  )  ', J = 2040'
   WRITE (UnAD,FmtText  )  ', FUNCTION = USER( 0 )'


ENDSELECT


   ! Tail fin aerodynamics:

IF ( CompAero )  THEN   ! AeroDyn will be used; therefore, add GFORCE statements with the calls to GFOSUB().


   WRITE (UnAD,FmtText     )  '!---------------------------- Tail Fin Aerodynamics ----------------------------'

   SELECT CASE ( TFinMod ) ! Which tail fin aerodynamics model are we using?

   CASE ( 0 )              ! None!

   ! Do nothing here!


   CASE ( 1 )              ! Standard (using inputs from the FAST furling input file).

      IF ( SubAxInd )  THEN
         SubAxI = 1
      ELSE
         SubAxI = 0
      ENDIF

      WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TFinAero_VF'''
      WRITE (UnAD,FmtText  )  'VFORCE/5110'
      WRITE (UnAD,FmtText  )  ', I = 5110'
      WRITE (UnAD,FmtText  )  ', JFLOAT = 500'
      WRITE (UnAD,FmtText  )  ', RM = 5110'
      WRITE (UnAD,FmtText  )  ', FUNCTION = USER( '//TRIM(Int2LStr( TFinMod ))//', '//TRIM(Int2LStr( TFinNFoil ))//', '// &
                                                     TRIM(Flt2LStr( TFinArea ))//', '//TRIM(Int2LStr( SubAxI ))//' )'


   CASE ( 2 )              ! User-defined tail fin aerodynamics model.

      WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TFinAero_VF'''
      WRITE (UnAD,FmtText  )  'VFORCE/5110'
      WRITE (UnAD,FmtText  )  ', I = 5110'
      WRITE (UnAD,FmtText  )  ', JFLOAT = 500'
      WRITE (UnAD,FmtText  )  ', RM = 5110'
      WRITE (UnAD,FmtText  )  ', FUNCTION = USER( '//TRIM(Int2LStr( TFinMod ))//' )'


   ENDSELECT


ENDIF



   ! Rotor-furl:

WRITE (UnAD,FmtText     )  '!--------------------------------- Rotor-Furl ----------------------------------'

SELECT CASE ( RFrlMod ) ! Which rotor-furl model are we using?

CASE ( 0 )              ! None!

   ! Do nothing here!


CASE ( 1 )              ! Standard (using inputs from the FAST furling input file).

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''RFrl_SD'''
   WRITE (UnAD,FmtText  )  'SPRINGDAMPER/2130'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 2130'
   WRITE (UnAD,FmtText  )  ', J = 2030'
   WRITE (UnAD,FmtTR    )  ', CT = ', 0.001*RFrlDmp
   WRITE (UnAD,FmtTR    )  ', KT = ', 0.001*RFrlSpr
   WRITE (UnAD,FmtText  )  ', TORQUE = 0'          ! The reference torque at ANGLE
   WRITE (UnAD,FmtText  )  ', ANGLE = 0'

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''RFrlUSSpr_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/2131'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 2130'
   WRITE (UnAD,FmtText  )  ', J = 2030'
   WRITE (UnAD,FmtText  )  ', FUNCTION = IF( AZ(2130,2030) - '//TRIM(Flt2LStr( RFrlUSSP ))//': 0, 0,'
   WRITE (UnAD,FmtText  )  ', -'//TRIM(Flt2LStr( RFrlUSSpr*0.001 ))//'*( AZ(2130,2030) - '//TRIM(Flt2LStr( RFrlUSSP ))//' ) )'

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''RFrlDSSpr_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/2132'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 2130'
   WRITE (UnAD,FmtText  )  ', J = 2030'
   WRITE (UnAD,FmtText  )  ', FUNCTION = IF( AZ(2130,2030) - '//TRIM(Flt2LStr( RFrlDSSP ))//':'
   WRITE (UnAD,FmtText  )  ', -'//TRIM(Flt2LStr( RFrlDSSpr*0.001 ))// &
                           '*( AZ(2130,2030) - '//TRIM(Flt2LStr( RFrlDSSP ))//' ), 0, 0 )'

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''RFrlUSDmp_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/2133'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 2130'
   WRITE (UnAD,FmtText  )  ', J = 2030'
   WRITE (UnAD,FmtText  )  ', FUNCTION = IF( AZ(2130,2030) - '//TRIM(Flt2LStr( RFrlUSDP ))// &
                           ': 0, 0, -'//TRIM(Flt2LStr( RFrlUSDmp*0.001 ))//'*WZ(2130,2030,2030) )'

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''RFrlDSDmp_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/2134'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 2130'
   WRITE (UnAD,FmtText  )  ', J = 2030'
   WRITE (UnAD,FmtText  )  ', FUNCTION = IF( AZ(2130,2030) - '//TRIM(Flt2LStr( RFrlDSDP ))// &
                           ': -'//TRIM(Flt2LStr( RFrlDSDmp*0.001 ))//'*WZ(2130,2030,2030), 0, 0 )'

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''RFrlCoulombDamp_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/2135'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 2130'
   WRITE (UnAD,FmtText  )  ', J = 2030'
   WRITE (UnAD,FmtText  )  ', FUNCTION = IF( WZ(2130,2030,2030): '//TRIM(Flt2LStr( RFrlCDmp*0.001 ))// &
                           ', 0, -'//TRIM(Flt2LStr( RFrlCDmp*0.001 ))//' )'


CASE ( 2 )              ! User-defined rotor-furl spring/damper model.

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''RFrlMom_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/2130'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 2130'
   WRITE (UnAD,FmtText  )  ', J = 2030'
   WRITE (UnAD,FmtText  )  ', FUNCTION = USER( 0 )'


ENDSELECT



   ! Low-speed shaft (or equivalent drivetrain) compliance:

WRITE (UnAD,FmtText  )  '!------------------------------------ Shaft ------------------------------------'

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''DriveTrain_SD'''
WRITE (UnAD,FmtText  )  'SPRINGDAMPER/3020'
WRITE (UnAD,FmtText  )  ', ROTATION'
WRITE (UnAD,FmtText  )  ', I = 3020'
WRITE (UnAD,FmtText  )  ', J = 3120'
WRITE (UnAD,FmtTR    )  ', CT = ', 0.001*DTTorDmp
WRITE (UnAD,FmtTR    )  ', KT = ', 0.001*DTTorSpr
WRITE (UnAD,FmtText  )  ', TORQUE = 0'          ! The reference torque at ANGLE
WRITE (UnAD,FmtText  )  ', ANGLE = 0'


   ! Generator torque:
   ! The PARameters passed to routine SFOSUB() depend on which generator model
   !   we are using:

IF ( GenDOF )  THEN  ! Only include the generator models if the generator is variable speed.

   WRITE (UnAD,FmtText  )  '!---------------------------------- Generator ----------------------------------'

   ! Find GenTiStrp = (1 if GenTiStr = .TRUE.) (+ 10 if GenTiStp = .TRUE.)
   IF ( GenTiStr )  THEN
      GenTiStrp = 1
   ELSE
      GenTiStrp = 0
   ENDIF
   IF ( GenTiStp )  GenTiStrp = GenTiStrp + 10

   WRITE (UnAD,FmtText        )  '!                             adams_view_name=''Generator_SF'''
   WRITE (UnAD,FmtText        )  'SFORCE/3150'
   WRITE (UnAD,FmtText        )  ', ROTATION'
   WRITE (UnAD,FmtText        )  ', I = 3150'
   WRITE (UnAD,FmtText        )  ', J = 2050'
   WRITE (UnAD,FmtText        )  ', FUNCTION = USER( '//TRIM(Int2LStr( GenTiStrp ))//', '//TRIM(Flt2LStr( SpdGenOn ))// &
                                 ', '//TRIM(Flt2LStr( TimGenOn ))//', '//TRIM(Flt2LStr( TimGenOf ))//                   &
                                 ', '//TRIM(Int2LStr( VSContrl ))//', '//TRIM(Flt2LStr( GBRatio  ))//                   &
                                 ', '//TRIM(Int2LStr( NumBl    ))//','
   SELECT CASE ( VSContrl )               ! Are we using variable-speed control?
   CASE ( 0 )                             ! No variable-speed control.  Using a generator model.
      SELECT CASE ( GenModel )            ! Which generator model are we using?
      CASE ( 1 )                          ! Simple induction-generator model.
         WRITE (UnAD,FmtText  )  ', '//TRIM(Int2LStr( GenModel ))//', '//TRIM(Flt2LStr( GenEff   ))//                   &
                                 ', '//TRIM(Flt2LStr( SIG_SySp ))//', '//TRIM(Flt2LStr( SIG_POSl ))//                   &
                                 ', '//TRIM(Flt2LStr( SIG_POTq ))//', '//TRIM(Flt2LStr( SIG_Slop ))//' )'
      CASE ( 2 )                          ! Thevenin-equivalent generator model.
         WRITE (UnAD,FmtText  )  ', '//TRIM(Int2LStr( GenModel ))//', '//TRIM(Flt2LStr( TEC_Freq ))//                   &
                                 ', '//TRIM(Int2LStr( TEC_NPol ))//', '//TRIM(Flt2LStr( TEC_SRes ))//                   &
                                 ', '//TRIM(Flt2LStr( TEC_RRes ))//', '//TRIM(Flt2LStr( TEC_VLL  ))//                   &
                                 ', '//TRIM(Flt2LStr( TEC_SLR  ))//', '//TRIM(Flt2LStr( TEC_RLR  ))//                   &
                                 ', '//TRIM(Flt2LStr( TEC_MR ))//' )'
      CASE ( 3 )                          ! User-defined generator model.
         WRITE (UnAD,FmtText  )  ', '//TRIM(Int2LStr( GenModel ))//', '//TRIM(Flt2LStr( GenEff ))//                     &
                                 ', '//TRIM(Flt2LStr( DT       ))//' )'
      ENDSELECT
   CASE ( 1 )                             ! Simple variable-speed control.
      WRITE (UnAD,FmtText     )  ', '//TRIM(Flt2LStr( GenEff   ))//', '//TRIM(Flt2LStr( VS_RtGnSp ))//                  &
                                 ', '//TRIM(Flt2LStr( VS_RtTq  ))//', '//TRIM(Flt2LStr( VS_Rgn2K  ))//                  &
                                 ', '//TRIM(Flt2LStr( VS_Slope ))//', '//TRIM(Flt2LStr( VS_TrGnSp ))//                  &
                                 ', '//TRIM(Flt2LStr( VS_SySp  ))//' )'
   CASE ( 2 )                             ! User-defined variable-speed control.
      WRITE (UnAD,FmtText     )  ', '//TRIM(Flt2LStr( GenEff   ))//', '//TRIM(Flt2LStr( DT ))//' )'
   ENDSELECT

ENDIF


   ! HSS brake torque:

IF ( ( GenDOF ) .AND. ( TMax > THSSBrDp ) )  THEN  ! .TRUE. if the HSS brake deploys during the simulation and is capable of slowing down the rotor.

   WRITE (UnAD,FmtText  )  '!---------------------------------- HSS Brake ----------------------------------'

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''HSSBrake_FF'''
   WRITE (UnAD,FmtText  )  'FRICTION/3151'
   WRITE (UnAD,FmtText  )  ', JOINT = 3150'
   WRITE (UnAD,FmtText  )  ', MU_STATIC = 1'
   WRITE (UnAD,FmtText  )  ', MU_DYNAMIC = 1'
   WRITE (UnAD,FmtTR    )  ', STICTION_TRANSITION_VELOCITY = ', SmllNmbr
   WRITE (UnAD,FmtText  )  ', EFFECT = ALL'
   WRITE (UnAD,FmtText  )  ', INPUTS = PRELOAD'
   WRITE (UnAD,FmtTR    )  ', FRICTION_TORQUE_PRELOAD = ', 0.001*GBRatio*HSSBrTqF   ! Apply the full HSS braking friction torque, transferred to the LSS and converted to kNm.

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''HSSBrake_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/3151'        ! This SFORCE is used to cancel out part or all of the brake torque from the FRICTION/3151 statement when the HSS brake is not fully applied.
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 3150'
   WRITE (UnAD,FmtText  )  ', J = 2050'
   WRITE (UnAD,FmtText  )  ', FUNCTION = USER( '//TRIM(Int2LStr( HSSBrMode ))//', '//TRIM(Flt2LStr( THSSBrDp ))// &
                           ', '//TRIM(Flt2LStr( HSSBrTqF ))//', '//TRIM(Flt2LStr( HSSBrDT ))//                    &
                           ', '//TRIM(Flt2LStr( DT       ))//', '//TRIM(Flt2LStr( GBRatio ))//                    &
                           ', '//TRIM(Int2LStr( NumBl    ))//' )'

ENDIF



   ! Rotor teeter:

WRITE (UnAD,FmtText     )  '!------------------------------------- Hub -------------------------------------'

SELECT CASE ( TeetMod ) ! Which teeter model are we using?

CASE ( 0 )              ! None!

   ! Do nothing here!


CASE ( 1 )              ! Standard (using inputs from the primary FAST input file).

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TeetSftStp_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/4011'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 4010'
   WRITE (UnAD,FmtText  )  ', J = 3310'
   WRITE (UnAD,FmtText  )  ', FUNCTION = IF( ABS(AZ(4010,3310)) - '//TRIM(Flt2LStr( TeetSStp ))//': 0, 0,'
   WRITE (UnAD,FmtText  )  ', -SIGN( '//TRIM(Flt2LStr( TeetSSSp*0.001 ))//'*( ABS(AZ(4010,3310)) - '// &
                           TRIM(Flt2LStr( TeetSStp ))//' ), AZ(4010,3310) ) )'

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TeetHrdStp_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/4012'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 4010'
   WRITE (UnAD,FmtText  )  ', J = 3310'
   WRITE (UnAD,FmtText  )  ', FUNCTION = IF( ABS(AZ(4010,3310)) - '//TRIM(Flt2LStr( TeetHStp ))//': 0, 0,'
   WRITE (UnAD,FmtText  )  ', -SIGN( '//TRIM(Flt2LStr( TeetHSSp*0.001 ))//'*( ABS(AZ(4010,3310)) - '// &
                           TRIM(Flt2LStr( TeetHStp ))//' ), AZ(4010,3310) ) )'

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TeetDamp_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/4013'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 4010'
   WRITE (UnAD,FmtText  )  ', J = 3310'
   WRITE (UnAD,FmtText  )  ', FUNCTION = IF( ABS(AZ(4010,3310)) - '//TRIM(Flt2LStr( TeetDmpP ))//': 0, 0, -'// &
                           TRIM(Flt2LStr( TeetDmp*0.001 ))//'*WZ(4010,3310,3310) )'

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TeetCoulombDamp_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/4014'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 4010'
   WRITE (UnAD,FmtText  )  ', J = 3310'
   WRITE (UnAD,FmtText  )  ', FUNCTION = IF( WZ(4010,3310,3310): '//TRIM(Flt2LStr( TeetCDmp*0.001 ))//', 0, -'// &
                           TRIM(Flt2LStr( TeetCDmp*0.001 ))//' )'


CASE ( 2 )              ! User-defined teeter spring/damper model.

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TeetMom_SF'''
   WRITE (UnAD,FmtText  )  'SFORCE/4010'
   WRITE (UnAD,FmtText  )  ', ROTATION'
   WRITE (UnAD,FmtText  )  ', I = 4010'
   WRITE (UnAD,FmtText  )  ', J = 3310'
   WRITE (UnAD,FmtText  )  ', FUNCTION = USER( 0 )'


ENDSELECT



   ! Blade pitch control:

DO K = 1,NumBl ! Loop through all blades

   WRITE (UnAD,'(A,I1,A)')  '!------------------------------------ Blade ', K, ' ----------------------------------'

   TmpID  = 10000*K
   TmpID2 = 4010 + K

   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''Pitch', K, 'Demand_V'''
   WRITE (UnAD,FmtText   )  'VARIABLE/'//TRIM(Int2LStr( TmpID     ))
   WRITE (UnAD,FmtText   )  ', FUNCTION = USER( '//TRIM(Int2LStr( PCMode ))//', '//TRIM(Flt2LStr( TPCOn ))// &
                            ', '//TRIM(Flt2LStr( DT          ))//', '//TRIM(Flt2LStr( GBRatio     ))//       &
                            ', '//TRIM(Int2LStr( NumBl       ))//','
   WRITE (UnAD,FmtText   )  ', '//TRIM(Flt2LStr( TPitManS(K) ))//                                            &
                            ', '//TRIM(Flt2LStr( TPitManE(K) ))//', '//TRIM(Flt2LStr( BlPitchF(K) ))//' )'

   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''Pitch', K, 'Error_V'''
   WRITE (UnAD,FmtText   )  'VARIABLE/'//TRIM(Int2LStr( TmpID + 1 ))
   WRITE (UnAD,FmtText   )  ', FUNCTION = VARVAL('//TRIM(Int2LStr( TmpID ))//') - AZ('//TRIM(Int2LStr( TmpID2 ))// &
                            ','//TRIM(Int2LStr( TmpID ))//')'

   WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''Pitch', K, 'Actuator_SF'''
   WRITE (UnAD,FmtText   )  'SFORCE/'  //TRIM(Int2LStr( TmpID     ))
   WRITE (UnAD,FmtText   )  ', ROTATION'
   WRITE (UnAD,FmtText   )  ', I = '   //TRIM(Int2LStr( TmpID     ))
   WRITE (UnAD,FmtText   )  ', J = '   //TRIM(Int2LStr( TmpID2    ))
   WRITE (UnAD,FmtText   )  ', FUNCTION = -'//TRIM(Flt2LStr( BPActrSpr*0.001 ))//                &
                            '*VARVAL('//TRIM(Int2LStr( TmpID + 1 ))//') - '//                    &
                            TRIM(Flt2LStr( BPActrDmp*0.001 ))//'*WZ('//TRIM(Int2LStr( TmpID ))// &
                            ','//TRIM(Int2LStr( TmpID2 ))//','//TRIM(Int2LStr( TmpID2 ))//')'

ENDDO          ! K - Blades



   ! Blade segment stiffness and damping:

WRITE (UnAD,FmtText      )  '!------------------------------------ Blades -----------------------------------'

DO K = 1,NumBl       ! Loop through all blades


   WRITE (UnAD,'(A,I1,A)')  '!------------------------------------ Blade ', K, ' ----------------------------------'

   CRatioBFl = 0.01*BldFDamp(K,1)/( Pi*FreqBF(K,1,1) )   ! Use the same damping ratio as used for the first blade flap mode in FAST.
   CRatioBEd = 0.01*BldEDamp(K,1)/( Pi*FreqBE(K,1,1) )   ! Use the same damping ratio as used for the first blade edge mode in FAST.
   TmpID  = 10000*K + 4000                               ! ID of the MARKER on the root of blade.
   TmpID2 = 10000*K + 6000 + 1                           ! ID of the structural axis MARKER in the middle of blade element 1.
   TmpLength  = 0.5*DRNodes(1)                           ! Distance between blade node 1 and the blade root.
   TmpLength2 = TmpLength*TmpLength                      ! = TmpLength^2.
   TmpLength3 = TmpLength*TmpLength2                     ! = TmpLength^3.
   KMatrix      =  0.0                                   ! Initialize KMatrix to zero.
   KMatrix(1,1) =  0.001*     StiffBEA(K,1)/TmpLength    ! Use the same KMatrix specified in the ADAMS/WT user's guide.
   KMatrix(2,2) =  0.001*12.0*StiffBF (K,1)/TmpLength3
   KMatrix(3,3) =  0.001*12.0*StiffBE (K,1)/TmpLength3
   KMatrix(4,4) =  0.001*     StiffBGJ(K,1)/TmpLength
   KMatrix(5,5) =  0.001* 4.0*StiffBE (K,1)/TmpLength
   KMatrix(6,6) =  0.001* 4.0*StiffBF (K,1)/TmpLength
   KMatrix(2,6) = -0.001* 6.0*StiffBF (K,1)/TmpLength2
   KMatrix(3,5) =  0.001* 6.0*StiffBE (K,1)/TmpLength2
   KMatrix(6,2) = KMatrix(2,6)
   KMatrix(5,3) = KMatrix(3,5)
   KMatrix(4,6) = -0.001*     BAlpha  (K,1)*SQRT( StiffBF(K,1)*StiffBGJ(K,1) )/TmpLength  ! Flap/twist coupling terms.  These are the terms described in the following reference:
   KMatrix(6,4) = KMatrix(4,6)                                                            ! Lobitz, D.W., Laino, D.J., "Load Mitigation with Twist-Coupled HAWT Blades." Proceedings, 1999 ASME Wind Energy Symposium/37th AIAA Aerospace Sciences Meeting and Exhibit, Reno, Nevada.  AIAA-1999-0033, January 1999, pp. 124-134.
   CMatrix      =  0.0                                   ! Initialize CMatrix to zero.
   CMatrix(1,1) = KMatrix(1,1)*CRatioBEA                 ! Scale the  KMatrix to form the CMatrix.
   CMatrix(2,2) = KMatrix(2,2)*CRatioBFl
   CMatrix(3,3) = KMatrix(3,3)*CRatioBEd
   CMatrix(4,4) = KMatrix(4,4)*CRatioBGJ
   CMatrix(5,5) = KMatrix(5,5)*CRatioBEd
   CMatrix(6,6) = KMatrix(6,6)*CRatioBFl
   CMatrix(2,6) = KMatrix(2,6)*CRatioBFl
   CMatrix(3,5) = KMatrix(3,5)*CRatioBEd
   CMatrix(6,2) = CMatrix(2,6)
   CMatrix(5,3) = CMatrix(3,5)
   CMatrix(4,6) = KMatrix(4,6)*SQRT( CRatioBFl*CRatioBGJ )  ! Flap/twist coupling terms.
   CMatrix(6,4) = CMatrix(4,6)                              !

   WRITE (UnAD,'(A,I1,A,I2.2,A)')  '!                             adams_view_name=''Bld', K, 'Sec', 1, 'ToBldSecBelow_F'''
   WRITE (UnAD,FmtText          )  'FIELD/'//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText          )  ', I = '//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText          )  ', J = '//TRIM(Int2LStr( TmpID  ))
   WRITE (UnAD,FmtText          )  ', CMATRIX = '
   WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', CMatrix(1,1), ', ', CMatrix(2,1), ', ', CMatrix(3,1), &
                                   ', ', CMatrix(4,1), ', ', CMatrix(5,1), ', ', CMatrix(6,1)
   WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', CMatrix(1,2), ', ', CMatrix(2,2), ', ', CMatrix(3,2), &
                                   ', ', CMatrix(4,2), ', ', CMatrix(5,2), ', ', CMatrix(6,2)
   WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', CMatrix(1,3), ', ', CMatrix(2,3), ', ', CMatrix(3,3), &
                                   ', ', CMatrix(4,3), ', ', CMatrix(5,3), ', ', CMatrix(6,3)
   WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', CMatrix(1,4), ', ', CMatrix(2,4), ', ', CMatrix(3,4), &
                                   ', ', CMatrix(4,4), ', ', CMatrix(5,4), ', ', CMatrix(6,4)
   WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', CMatrix(1,5), ', ', CMatrix(2,5), ', ', CMatrix(3,5), &
                                   ', ', CMatrix(4,5), ', ', CMatrix(5,5), ', ', CMatrix(6,5)
   WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', CMatrix(1,6), ', ', CMatrix(2,6), ', ', CMatrix(3,6), &
                                   ', ', CMatrix(4,6), ', ', CMatrix(5,6), ', ', CMatrix(6,6)
   WRITE (UnAD,FmtText          )  ', KMATRIX = '
   WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', KMatrix(1,1), ', ', KMatrix(2,1), ', ', KMatrix(3,1), &
                                   ', ', KMatrix(4,1), ', ', KMatrix(5,1), ', ', KMatrix(6,1)
   WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', KMatrix(1,2), ', ', KMatrix(2,2), ', ', KMatrix(3,2), &
                                   ', ', KMatrix(4,2), ', ', KMatrix(5,2), ', ', KMatrix(6,2)
   WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', KMatrix(1,3), ', ', KMatrix(2,3), ', ', KMatrix(3,3), &
                                   ', ', KMatrix(4,3), ', ', KMatrix(5,3), ', ', KMatrix(6,3)
   WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', KMatrix(1,4), ', ', KMatrix(2,4), ', ', KMatrix(3,4), &
                                   ', ', KMatrix(4,4), ', ', KMatrix(5,4), ', ', KMatrix(6,4)
   WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', KMatrix(1,5), ', ', KMatrix(2,5), ', ', KMatrix(3,5), &
                                   ', ', KMatrix(4,5), ', ', KMatrix(5,5), ', ', KMatrix(6,5)
   WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', KMatrix(1,6), ', ', KMatrix(2,6), ', ', KMatrix(3,6), &
                                   ', ', KMatrix(4,6), ', ', KMatrix(5,6), ', ', KMatrix(6,6)
   WRITE (UnAD,FmtText          )  ', LENGTH = '
   WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', TmpLength, ', ', DOT_PRODUCT( EAVec(K,1,:), -n1(K,1,:) ), &
                                   ', ', DOT_PRODUCT( EAVec(K,1,:), -n2(K,1,:) ), ', ', 0.0, ', ', 0.0, ', ', 0.0


   DO J = 2,BldNodes ! Loop through all but the innermost blade nodes/elements


      TmpID  = 10000*K + 6000 + J - 1                       ! ID of the structural axis MARKER in the middle of blade element J - 1.
      TmpID2 = 10000*K + 6000 + J                           ! ID of the structural axis MARKER in the middle of blade element J.
      TmpLength  = 0.5*( DRNodes(J) + DRNodes(J-1) )        ! Distance between blade node J and blade node J - 1.
      TmpLength2 = TmpLength*TmpLength                      ! = TmpLength^2.
      TmpLength3 = TmpLength*TmpLength2                     ! = TmpLength^3.
      KMatrix      =  0.0                                   ! Initialize KMatrix to zero.
      KMatrix(1,1) =  0.001*0.5* (     StiffBEA(K,J) + StiffBEA(K,J-1) )/TmpLength  ! Use the same KMatrix specified in the ADAMS/WT user's guide.
      KMatrix(2,2) =  0.001*6.0* (     StiffBF (K,J) + StiffBF (K,J-1) )/TmpLength3
      KMatrix(3,3) =  0.001*6.0* (     StiffBE (K,J) + StiffBE (K,J-1) )/TmpLength3
      KMatrix(4,4) =  0.001*0.5* (     StiffBGJ(K,J) + StiffBGJ(K,J-1) )/TmpLength
      KMatrix(5,5) =  0.001*     ( 3.0*StiffBE (K,J) + StiffBE (K,J-1) )/TmpLength
      KMatrix(6,6) =  0.001*     ( 3.0*StiffBF (K,J) + StiffBF (K,J-1) )/TmpLength
      KMatrix(2,6) = -0.001*2.0* ( 2.0*StiffBF (K,J) + StiffBF (K,J-1) )/TmpLength2
      KMatrix(3,5) =  0.001*2.0* ( 2.0*StiffBE (K,J) + StiffBE (K,J-1) )/TmpLength2
      KMatrix(6,2) = KMatrix(2,6)
      KMatrix(5,3) = KMatrix(3,5)
      KMatrix(4,6) = -0.001*0.25*(     BAlpha  (K,J) + BAlpha  (K,J-1) )* &
                     SQRT(( StiffBF(K,J) + StiffBF(K,J-1) )*( StiffBGJ(K,J) + StiffBGJ(K,J-1) ))/TmpLength  ! Flap/twist coupling terms.  These are the terms described in the following reference:
      KMatrix(6,4) = KMatrix(4,6)                                                                           ! Lobitz, D.W., Laino, D.J., "Load Mitigation with Twist-Coupled HAWT Blades." Proceedings, 1999 ASME Wind Energy Symposium/37th AIAA Aerospace Sciences Meeting and Exhibit, Reno, Nevada.  AIAA-1999-0033, January 1999, pp. 124-134.
      CMatrix      =  0.0                                   ! Initialize CMatrix to zero.
      CMatrix(1,1) = KMatrix(1,1)*CRatioBEA                 ! Scale the  KMatrix to form the CMatrix.
      CMatrix(2,2) = KMatrix(2,2)*CRatioBFl
      CMatrix(3,3) = KMatrix(3,3)*CRatioBEd
      CMatrix(4,4) = KMatrix(4,4)*CRatioBGJ
      CMatrix(5,5) = KMatrix(5,5)*CRatioBEd
      CMatrix(6,6) = KMatrix(6,6)*CRatioBFl
      CMatrix(2,6) = KMatrix(2,6)*CRatioBFl
      CMatrix(3,5) = KMatrix(3,5)*CRatioBEd
      CMatrix(6,2) = CMatrix(2,6)
      CMatrix(5,3) = CMatrix(3,5)
      CMatrix(4,6) = KMatrix(4,6)*SQRT( CRatioBFl*CRatioBGJ )  ! Flap/twist coupling terms.
      CMatrix(6,4) = CMatrix(4,6)                              !

      WRITE (UnAD,'(A,I1,A,I2.2,A)')  '!                             adams_view_name=''Bld', K, 'Sec', J, 'ToBldSecBelow_F'''
      WRITE (UnAD,FmtText          )  'FIELD/'//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText          )  ', I = '//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText          )  ', J = '//TRIM(Int2LStr( TmpID  ))
      WRITE (UnAD,FmtText          )  ', CMATRIX = '
      WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', CMatrix(1,1), ', ', CMatrix(2,1), ', ', CMatrix(3,1), &
                                      ', ', CMatrix(4,1), ', ', CMatrix(5,1), ', ', CMatrix(6,1)
      WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', CMatrix(1,2), ', ', CMatrix(2,2), ', ', CMatrix(3,2), &
                                      ', ', CMatrix(4,2), ', ', CMatrix(5,2), ', ', CMatrix(6,2)
      WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', CMatrix(1,3), ', ', CMatrix(2,3), ', ', CMatrix(3,3), &
                                      ', ', CMatrix(4,3), ', ', CMatrix(5,3), ', ', CMatrix(6,3)
      WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', CMatrix(1,4), ', ', CMatrix(2,4), ', ', CMatrix(3,4), &
                                      ', ', CMatrix(4,4), ', ', CMatrix(5,4), ', ', CMatrix(6,4)
      WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', CMatrix(1,5), ', ', CMatrix(2,5), ', ', CMatrix(3,5), &
                                      ', ', CMatrix(4,5), ', ', CMatrix(5,5), ', ', CMatrix(6,5)
      WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', CMatrix(1,6), ', ', CMatrix(2,6), ', ', CMatrix(3,6), &
                                      ', ', CMatrix(4,6), ', ', CMatrix(5,6), ', ', CMatrix(6,6)
      WRITE (UnAD,FmtText          )  ', KMATRIX = '
      WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', KMatrix(1,1), ', ', KMatrix(2,1), ', ', KMatrix(3,1), &
                                      ', ', KMatrix(4,1), ', ', KMatrix(5,1), ', ', KMatrix(6,1)
      WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', KMatrix(1,2), ', ', KMatrix(2,2), ', ', KMatrix(3,2), &
                                      ', ', KMatrix(4,2), ', ', KMatrix(5,2), ', ', KMatrix(6,2)
      WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', KMatrix(1,3), ', ', KMatrix(2,3), ', ', KMatrix(3,3), &
                                      ', ', KMatrix(4,3), ', ', KMatrix(5,3), ', ', KMatrix(6,3)
      WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', KMatrix(1,4), ', ', KMatrix(2,4), ', ', KMatrix(3,4), &
                                      ', ', KMatrix(4,4), ', ', KMatrix(5,4), ', ', KMatrix(6,4)
      WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', KMatrix(1,5), ', ', KMatrix(2,5), ', ', KMatrix(3,5), &
                                      ', ', KMatrix(4,5), ', ', KMatrix(5,5), ', ', KMatrix(6,5)
      WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', KMatrix(1,6), ', ', KMatrix(2,6), ', ', KMatrix(3,6), &
                                      ', ', KMatrix(4,6), ', ', KMatrix(5,6), ', ', KMatrix(6,6)
      WRITE (UnAD,FmtText          )  ', LENGTH = '
!bjj start of proposed change v6.02d-bjj
! parentheses around an I/O list is an extension to Standard F2003
!rm      WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', TmpLength, ', ', DOT_PRODUCT( EAVec(K,J,:), -n1(K,J-1,:) ), &
!rm                                      ', ', DOT_PRODUCT( EAVec(K,J,:), -n2(K,J-1,:) ),                  &
!rm                                      ', ', ( ThetaS(K,J-1) - ThetaS(K,J) ), ', ', 0.0, ', ', 0.0

      WRITE (UnAD,FmtTRTRTRTRTRTR  )  ', ', TmpLength, ', ', DOT_PRODUCT( EAVec(K,J,:), -n1(K,J-1,:) ), &
                                      ', ', DOT_PRODUCT( EAVec(K,J,:), -n2(K,J-1,:) ),                  &
                                      ', ', ThetaS(K,J-1) - ThetaS(K,J), ', ', 0.0, ', ', 0.0
!bjj end of proposed change

   ENDDO             ! J - Blade nodes/elements


   TmpID  = 10000*K + 6000 + BldNodes                    ! ID of the structural axis MARKER in the middle of blade element BldNodes.
   TmpID2 = 10000*K + 5000                               ! ID of the MARKER at the blade tip.
   TmpLength  = 0.5*DRNodes(BldNodes)                    ! Distance between blade node BldNodes and the blade tip.
   TmpLength2 = TmpLength*TmpLength                      ! = TmpLength^2.
   TmpLength3 = TmpLength*TmpLength2                     ! = TmpLength^3.
   KMatrix      =  0.0                                   ! Initialize KMatrix to zero.
   KMatrix(1,1) =  0.001*     StiffBEA(K,BldNodes)/TmpLength   ! Use the same KMatrix specified in the ADAMS/WT user's guide.
   KMatrix(2,2) =  0.001*12.0*StiffBF (K,BldNodes)/TmpLength3
   KMatrix(3,3) =  0.001*12.0*StiffBE (K,BldNodes)/TmpLength3
   KMatrix(4,4) =  0.001*     StiffBGJ(K,BldNodes)/TmpLength
   KMatrix(5,5) =  0.001* 4.0*StiffBE (K,BldNodes)/TmpLength
   KMatrix(6,6) =  0.001* 4.0*StiffBF (K,BldNodes)/TmpLength
   KMatrix(2,6) = -0.001* 6.0*StiffBF (K,BldNodes)/TmpLength2
   KMatrix(3,5) =  0.001* 6.0*StiffBE (K,BldNodes)/TmpLength2
   KMatrix(6,2) = KMatrix(2,6)
   KMatrix(5,3) = KMatrix(3,5)
   KMatrix(4,6) = -0.001*     BAlpha  (K,BldNodes)*SQRT( StiffBF(K,BldNodes)*StiffBGJ(K,BldNodes) )/TmpLength  ! Flap/twist coupling terms.  These are the terms described in the following reference:
   KMatrix(6,4) = KMatrix(4,6)                                                                                 ! Lobitz, D.W., Laino, D.J., "Load Mitigation with Twist-Coupled HAWT Blades." Proceedings, 1999 ASME Wind Energy Symposium/37th AIAA Aerospace Sciences Meeting and Exhibit, Reno, Nevada.  AIAA-1999-0033, January 1999, pp. 124-134.
   CMatrix      =  0.0                                   ! Initialize CMatrix to zero.
   CMatrix(1,1) = KMatrix(1,1)*CRatioBEA                 ! Scale the  KMatrix to form the CMatrix.
   CMatrix(2,2) = KMatrix(2,2)*CRatioBFl
   CMatrix(3,3) = KMatrix(3,3)*CRatioBEd
   CMatrix(4,4) = KMatrix(4,4)*CRatioBGJ
   CMatrix(5,5) = KMatrix(5,5)*CRatioBEd
   CMatrix(6,6) = KMatrix(6,6)*CRatioBFl
   CMatrix(2,6) = KMatrix(2,6)*CRatioBFl
   CMatrix(3,5) = KMatrix(3,5)*CRatioBEd
   CMatrix(6,2) = CMatrix(2,6)
   CMatrix(5,3) = CMatrix(3,5)
   CMatrix(4,6) = KMatrix(4,6)*SQRT( CRatioBFl*CRatioBGJ )  ! Flap/twist coupling terms.
   CMatrix(6,4) = CMatrix(4,6)                              !

   WRITE (UnAD,'(A,I1,A)'     )  '!                             adams_view_name=''TipBrake', K, 'ToBldSecBelow_F'''
   WRITE (UnAD,FmtText        )  'FIELD/'//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText        )  ', I = '//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText        )  ', J = '//TRIM(Int2LStr( TmpID  ))
   WRITE (UnAD,FmtText        )  ', CMATRIX = '
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,1), ', ', CMatrix(2,1), ', ', CMatrix(3,1), &
                                 ', ', CMatrix(4,1), ', ', CMatrix(5,1), ', ', CMatrix(6,1)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,2), ', ', CMatrix(2,2), ', ', CMatrix(3,2), &
                                 ', ', CMatrix(4,2), ', ', CMatrix(5,2), ', ', CMatrix(6,2)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,3), ', ', CMatrix(2,3), ', ', CMatrix(3,3), &
                                 ', ', CMatrix(4,3), ', ', CMatrix(5,3), ', ', CMatrix(6,3)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,4), ', ', CMatrix(2,4), ', ', CMatrix(3,4), &
                                 ', ', CMatrix(4,4), ', ', CMatrix(5,4), ', ', CMatrix(6,4)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,5), ', ', CMatrix(2,5), ', ', CMatrix(3,5), &
                                 ', ', CMatrix(4,5), ', ', CMatrix(5,5), ', ', CMatrix(6,5)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', CMatrix(1,6), ', ', CMatrix(2,6), ', ', CMatrix(3,6), &
                                 ', ', CMatrix(4,6), ', ', CMatrix(5,6), ', ', CMatrix(6,6)
   WRITE (UnAD,FmtText        )  ', KMATRIX = '
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,1), ', ', KMatrix(2,1), ', ', KMatrix(3,1), &
                                 ', ', KMatrix(4,1), ', ', KMatrix(5,1), ', ', KMatrix(6,1)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,2), ', ', KMatrix(2,2), ', ', KMatrix(3,2), &
                                 ', ', KMatrix(4,2), ', ', KMatrix(5,2), ', ', KMatrix(6,2)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,3), ', ', KMatrix(2,3), ', ', KMatrix(3,3), &
                                 ', ', KMatrix(4,3), ', ', KMatrix(5,3), ', ', KMatrix(6,3)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,4), ', ', KMatrix(2,4), ', ', KMatrix(3,4), &
                                 ', ', KMatrix(4,4), ', ', KMatrix(5,4), ', ', KMatrix(6,4)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,5), ', ', KMatrix(2,5), ', ', KMatrix(3,5), &
                                 ', ', KMatrix(4,5), ', ', KMatrix(5,5), ', ', KMatrix(6,5)
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', KMatrix(1,6), ', ', KMatrix(2,6), ', ', KMatrix(3,6), &
                                 ', ', KMatrix(4,6), ', ', KMatrix(5,6), ', ', KMatrix(6,6)
   WRITE (UnAD,FmtText        )  ', LENGTH = '
   WRITE (UnAD,FmtTRTRTRTRTRTR)  ', ', TmpLength, ', ', DOT_PRODUCT( EAVec(K,TipNode,:), -n1(K,BldNodes,:) ), &
                                 ', ', DOT_PRODUCT( EAVec(K,TipNode,:), -n2(K,BldNodes,:) ), ', ', 0.0, ', ', 0.0, ', ', 0.0


ENDDO                ! K - Blades



   ! Aerodynamic forces:

IF ( CompAero )  THEN   ! AeroDyn will be used; therefore, add GFORCE statements with the calls to GFOSUB().


   DO K = 1,NumBl       ! Loop through all blades


      WRITE (UnAD,'(A,I1,A)')  '!------------------------------------ Blade ', K, ' ----------------------------------'

      DO J = 1,BldNodes ! Loop through the blade nodes/elements
         TmpID  = K*100 + J               ! ID of the FLOATING MARKER of the current blade element.
         TmpID2 = 10000*K + 1000 + 10*J   ! ID of the associated aerodynamic MARKER fixed in the current blade element.
         WRITE (UnAD,'(A,I1,A,I2.2,A)')  '!                             adams_view_name=''Bld', K, 'Sec', J, 'Aero_GF'''
         WRITE (UnAD,FmtText          )  'GFORCE/'//TRIM(Int2LStr( TmpID2 ))
         WRITE (UnAD,FmtText          )  ', I = '//TRIM(Int2LStr( TmpID2 ))
         WRITE (UnAD,FmtText          )  ', JFLOAT = '//TRIM(Int2LStr( TmpID  ))
         WRITE (UnAD,FmtText          )  ', RM = '//TRIM(Int2LStr( TmpID2 ))
   ! NOTE: The third PARameter in this argument list should be the ID of the
   !       VARIABLE containing MulTabLoc for USER-defined multiple airfoil
   !       tables.  A user will have to add a VARIABLE statement and change
   !       this value to the VARIABLE ID if they want to incorporate USER-
   !       defined multiple airfoil table interpolations.  If the user wants
   !       to use Reynolds Number interpolation (RENUM), then this value
   !       should remain at zero!
         WRITE (UnAD,'(A,I1,A,I2,A)'  )  ', FUNCTION = USER( ', K, ', ', J, ', 0, '//TRIM(Int2LStr( TmpID2 ))//' )'

      ENDDO             ! J - Blade nodes/elements


   ENDDO                ! K - Blades


ENDIF


   ! Tip brake drag:

IF ( ( TBDrConN /= 0.0 ) .OR. ( TBDrConD /= 0.0 ) )  THEN   ! Only added when TBDrConN or TBDrConD is nonzero:


   DO K = 1,NumBl       ! Loop through all blades

      WRITE (UnAD,'(A,I1,A)')  '!------------------------------------ Blade ', K, ' ----------------------------------'

      TmpID  = 500 + K           ! ID of the FLOATING  MARKER of the current blade tip brake.
      TmpID2 = 10000*K + 5100    ! ID of the untwisted MARKER of the current blade tip brake.

      WRITE (UnAD,'(A,I1,A)')  '!                             adams_view_name=''TipBrake', K, 'Drag_VF'''
      WRITE (UnAD,FmtText   )  'VFORCE/'//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText   )  ', I = '//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText   )  ', JFLOAT = '//TRIM(Int2LStr( TmpID ))
      WRITE (UnAD,FmtText   )  ', RM = '//TRIM(Int2LStr( TmpID2 ))
      WRITE (UnAD,FmtText   )  ', FUNCTION = USER( '//TRIM(Int2LStr( NumBl ))//', '//TRIM(Flt2LStr( TBDrConN ))// &
                               ', '//TRIM(Flt2LStr( TBDrConD ))//', '//TRIM(Flt2LStr( TpBrDT ))//                 &
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Pass CompAero to the tip brake drag VFORCE to ensure that CompAero is
!jmj   known within VFOSUB():
!remove6.02a                               ', '//TRIM(Flt2LStr( TTpBrDp(K) ))//', '//TRIM(Flt2LStr( TBDepISp(K) ))//' )'
                               ', '//TRIM(Int2LStr( CompAeroI   ))//', '//TRIM(Flt2LStr( TTpBrDp(K) ))//          &
                               ', '//TRIM(Flt2LStr( TBDepISp(K) ))//' )'
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

   ENDDO                ! K - Blades


ENDIF




   ! Specify the gravitational acceleration:

WRITE (UnAD,FmtText  )  '!========================= GRAVITATIONAL ACCELERATION =========================='

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''Gravity'''
WRITE (UnAD,FmtText  )  'ACCGRAV/'
WRITE (UnAD,FmtTR    )  ', KGRAV = ', -Gravity



   ! Specify the system units:

WRITE (UnAD,FmtText  )  '!================================ SYSTEM UNITS ================================='

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''Units'''
WRITE (UnAD,FmtText  )  'UNITS/'
WRITE (UnAD,FmtText  )  ', FORCE = KNEWTON'
WRITE (UnAD,FmtText  )  ', MASS = KILOGRAM'
WRITE (UnAD,FmtText  )  ', LENGTH = METER'
WRITE (UnAD,FmtText  )  ', TIME = SECOND'



   ! Specify the execution control SENSORs:

WRITE (UnAD,FmtText  )  '!============================== EXECUTION CONTROL =============================='


   ! Tell AeroDyn the successful completion of a time step:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''AeroDyn_S'''
WRITE (UnAD,FmtText  )  'SENSOR/1'
WRITE (UnAD,FmtText  )  ', VALUE = 1.0'
WRITE (UnAD,FmtText  )  ', EQ'
WRITE (UnAD,FmtText  )  ', ERROR = 0.001'
WRITE (UnAD,FmtText  )  ', HALT'
WRITE (UnAD,FmtText  )  ', PRINT'
WRITE (UnAD,FmtText  )  ', FUNCTION = USER( 1.0 )'



   ! Specify the analysis settings (i.e., what files should be created) ond output parameters:

WRITE (UnAD,FmtText  )  '!========================= ANALYSIS SETTINGS / OUTPUT =========================='


   ! The ARRAY statement for constants:

IF ( NumOuts /= 0 )  THEN

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''OutIndSign_A'''
   WRITE (UnAD,FmtText  )  'ARRAY/1'
   WRITE (UnAD,FmtText  )  ', IC, SIZE = '//TRIM(Int2LStr( NumOuts ))   ! Specify a list of constants.  The number of elements in the ARRAY is NumOuts
   IF ( NumOuts == 1 )  THEN  ! Only one output channel  selected
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Add blade strain gage output parameters for the local loads and motions of
!jmj   blades 2 and 3:
!jmj Also, replace the hard-coded mooring line restoring calculation with a
!jmj   general purpose, quasi-static solution based on the analytical catenary
!jmj   cable equations with seabed interaction:
!remove6.02b      WRITE (      UnAD,FmtText  )  ', NUMBERS = '//TRIM(Int2LStr( OutInd(1) + 250*( 1 - OutSign(1) ) ))
      WRITE (      UnAD,FmtText  )  ', NUMBERS = '//TRIM(Int2LStr( OutInd(1) + 500*( 1 - OutSign(1) ) ))
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
   ELSE                       ! Multiple output channels selected
      DO I = 1,NumOuts  ! Loop through all selected output channels
         IF ( I == NumOuts )  THEN  ! Last output channel
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Add blade strain gage output parameters for the local loads and motions of
!jmj   blades 2 and 3:
!jmj Also, replace the hard-coded mooring line restoring calculation with a
!jmj   general purpose, quasi-static solution based on the analytical catenary
!jmj   cable equations with seabed interaction:
!remove6.02b            WRITE (UnAD,FmtText  )  ', '          //TRIM(Int2LStr( OutInd(I) + 250*( 1 - OutSign(I) ) ))
            WRITE (UnAD,FmtText  )  ', '          //TRIM(Int2LStr( OutInd(I) + 500*( 1 - OutSign(I) ) ))
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
         ELSEIF ( I == 1   )  THEN  ! First output channel
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Add blade strain gage output parameters for the local loads and motions of
!jmj   blades 2 and 3:
!jmj Also, replace the hard-coded mooring line restoring calculation with a
!jmj   general purpose, quasi-static solution based on the analytical catenary
!jmj   cable equations with seabed interaction:
!remove6.02b            WRITE (UnAD,FmtText  )  ', NUMBERS = '//TRIM(Int2LStr( OutInd(I) + 250*( 1 - OutSign(I) ) ))//','
            WRITE (UnAD,FmtText  )  ', NUMBERS = '//TRIM(Int2LStr( OutInd(I) + 500*( 1 - OutSign(I) ) ))//','
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
         ELSE                       ! All other output channels
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Add blade strain gage output parameters for the local loads and motions of
!jmj   blades 2 and 3:
!jmj Also, replace the hard-coded mooring line restoring calculation with a
!jmj   general purpose, quasi-static solution based on the analytical catenary
!jmj   cable equations with seabed interaction:
!remove6.02b            WRITE (UnAD,FmtText  )  ', '          //TRIM(Int2LStr( OutInd(I) + 250*( 1 - OutSign(I) ) ))//','  ! Each element of the list is OutInd() ( + 500 if the corresponding OutSign() = -1 ).
            WRITE (UnAD,FmtText  )  ', '          //TRIM(Int2LStr( OutInd(I) + 500*( 1 - OutSign(I) ) ))//','  ! Each element of the list is OutInd() ( + 1000 if the corresponding OutSign() = -1 ).
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
         ENDIF
      ENDDO             ! I - all selected output channels
   ENDIF

ENDIF

IF ( NBlGages /= 0 )  THEN

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''BldGagNd_A'''
   WRITE (UnAD,FmtText  )  'ARRAY/2'
   WRITE (UnAD,FmtText  )  ', IC, SIZE = '//TRIM(Int2LStr( NBlGages ))   ! Specify a list of constants.  The number of elements in the ARRAY is NBlGages
   IF ( NBlGages == 1 )  THEN ! Only one blade gage  selected
      WRITE (      UnAD,FmtText  )  ', NUMBERS = '//TRIM(Int2LStr( BldGagNd(1) ))
   ELSE                       ! Multiple blade gages selected
      DO I = 1,NBlGages ! Loop through all blade nodes with strain gages for output.
         IF ( I == NBlGages )  THEN ! Last blade gage node
            WRITE (UnAD,FmtText  )  ', '//TRIM(Int2LStr( BldGagNd(I) ))
         ELSEIF ( I == 1   )  THEN  ! First blade gage node
            WRITE (UnAD,FmtText  )  ', NUMBERS = '//TRIM(Int2LStr( BldGagNd(I) ))//','
         ELSE                       ! All other blade gage nodes
            WRITE (UnAD,FmtText  )  ', '//TRIM(Int2LStr( BldGagNd(I) ))//','
         ENDIF
      ENDDO             ! All blade nodes with strain gages for output.
   ENDIF

ENDIF

IF ( NTwGages /= 0 )  THEN

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''TwrGagNd_A'''
   WRITE (UnAD,FmtText  )  'ARRAY/3'
   WRITE (UnAD,FmtText  )  ', IC, SIZE = '//TRIM(Int2LStr( NTwGages ))   ! Specify a list of constants.  The number of elements in the ARRAY is NTwGages
   IF ( NTwGages == 1 )  THEN ! Only one tower gage  selected
      WRITE (      UnAD,FmtText  )  ', NUMBERS = '//TRIM(Int2LStr( TwrGagNd(1) ))
   ELSE                       ! Multiple tower gages selected
      DO I = 1,NTwGages ! Loop through all tower nodes with strain gages for output.
         IF ( I == NTwGages )  THEN ! Last tower gage node
            WRITE (UnAD,FmtText  )  ', '//TRIM(Int2LStr( TwrGagNd(I) ))
         ELSEIF ( I == 1   )  THEN  ! First tower gage node
            WRITE (UnAD,FmtText  )  ', NUMBERS = '//TRIM(Int2LStr( TwrGagNd(I) ))//','
         ELSE                       ! All other tower gage nodes
            WRITE (UnAD,FmtText  )  ', '//TRIM(Int2LStr( TwrGagNd(I) ))//','
         ENDIF
      ENDDO             ! All tower nodes with strain gages for output.
   ENDIF

ENDIF

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
IF ( ( NWaveKin /= 0 ) .AND. CompHydro )  THEN  ! .TRUE. if we are using the undocumented monopile or platform features

   WRITE (UnAD,FmtText  )  '!                             adams_view_name=''WaveKinNd_A'''
   WRITE (UnAD,FmtText  )  'ARRAY/4'
   WRITE (UnAD,FmtText  )  ', IC, SIZE = '//TRIM(Int2LStr( NWaveKin ))  ! Specify a list of constants.  The number of elements in the ARRAY is NWaveKin
   IF ( NWaveKin == 1 )  THEN ! Only one wave kinematics sensor  selected
      WRITE (      UnAD,FmtText  )  ', NUMBERS = '//TRIM(Int2LStr( WaveKinNd(1) ))
   ELSE                       ! Multiple wave kinematics sensors selected
      DO I = 1,NWaveKin ! Loop through all nodes with wave kinematics sensors for output.
         IF ( I == NWaveKin )  THEN ! Last node
            WRITE (UnAD,FmtText  )  ', '//TRIM(Int2LStr( WaveKinNd(I) ))
         ELSEIF ( I == 1   )  THEN  ! First node
            WRITE (UnAD,FmtText  )  ', NUMBERS = '//TRIM(Int2LStr( WaveKinNd(I) ))//','
         ELSE                       ! All nodes
            WRITE (UnAD,FmtText  )  ', '//TRIM(Int2LStr( WaveKinNd(I) ))//','
         ENDIF
      ENDDO             ! All nodes with wave kinematics sensors for output.
   ENDIF

ENDIF


!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

   ! The STRING statements for passing constant STRING expressions to the
   !   user-written subroutines:
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add a STRING constant in the ADAMS dataset that FAST2ADAMS creates, which
!jmj   represents the program version number of A2AD that is required to run
!jmj   the ADAMS model:
WRITE (UnAD,FmtText        )  '!                             adams_view_name=''ProgVerOfA2AD_SG'''
WRITE (UnAD,FmtText        )  'STRING/1'
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Update STRING/1 to the latest program version of A2AD:
!remove6.02bWRITE (UnAD,FmtText        )  ', STRING = v12.20a-jmj'
!jmj Start of proposed change.  v6.02c-jmj  02-Feb-2007.
!jmj Update STRING/1 to the latest program version of A2AD:
!remove6.02cWRITE (UnAD,FmtText        )  ', STRING = v12.20b-jmj'
!bjj start of proposed change
!rmWRITE (UnAD,FmtText        )  ', STRING = v12.20c-jmj'
WRITE (UnAD,FmtText        )  ', STRING = v13.00.00a-bjj'
!bjj end of proposed change
!jmj End of proposed change.  v6.02c-jmj  02-Feb-2007.
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

WRITE (UnAD,FmtText        )  '!                             adams_view_name=''FTitle_SG'''
WRITE (UnAD,FmtText        )  'STRING/21'
WRITE (UnAD,FmtText        )  ', STRING = '//TRIM( FTitle )             ! The title line from the primary input file.

WRITE (UnAD,FmtText        )  '!                             adams_view_name=''OutFmt_SG'''
WRITE (UnAD,FmtText        )  'STRING/22'
WRITE (UnAD,FmtText        )  ', STRING = '//TRIM( OutFmt )             ! Output format for tabular data.
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
IF ( ( WaveMod   == 4 ) .AND. CompHydro )  THEN ! .TRUE if we are to use GH Bladed wave data.
   WRITE (UnAD,FmtText     )  '!                             adams_view_name=''GHWvFile_SG'''
   WRITE (UnAD,FmtText     )  'STRING/31'
   WRITE (UnAD,FmtText     )  ', STRING = '//TRIM( GHWvFile )           ! The root name of GH Bladed files containing wave data.
ENDIF

IF ( ( PtfmModel == 3 ) .AND. CompHydro )  THEN ! .TRUE. if we have floating offshore turbine and we are using the undocumented platform features.
   WRITE (UnAD,FmtText     )  '!                             adams_view_name=''WAMITFile_SG'''
   WRITE (UnAD,FmtText     )  'STRING/32'
   WRITE (UnAD,FmtText     )  ', STRING = '//TRIM( WAMITFile )          ! Root name of WAMIT output files.
ENDIF

!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
WRITE (UnAD,FmtText        )  '!                             adams_view_name=''ADIptFile_SG'''
!BJJ START OF PROPOSED CHANGE
!RMWRITE (UnAD,FmtText        )  'STRING/'//TRIM(Int2LStr( UnADin ))
WRITE (UnAD,FmtText        )  'STRING/'//TRIM(Flt2LStr( AD_GetConstant('ADunit', Sttus) ))
!BJJ END OF PROPOSED CHANGE
WRITE (UnAD,FmtText        )  ', STRING = '//TRIM( ADFile )             ! The name of the AeroDyn input file.

WRITE (UnAD,FmtText        )  '!                             adams_view_name=''RootName_SG'''
WRITE (UnAD,FmtText        )  'STRING/95'
WRITE (UnAD,FmtText        )  ', STRING = '//TRIM( RootName )//'_ADAMS' ! The root name of the output file containing time-series output.

DO I = 0,NumOuts  ! Loop through all selected output channels (plus time)

   TmpID  = 1000 + I
   TmpID2 = 2000 + I

   WRITE (UnAD,'(A,I3.3,A)')  '!                             adams_view_name=''OutNames', I, '_SG'''
   WRITE (UnAD,FmtText     )  'STRING/'//TRIM(Int2LStr( TmpID  ))
   WRITE (UnAD,FmtText     )  ', STRING = '//TRIM( OutParam(I)%Name  )  ! The names of the output channels.

   WRITE (UnAD,'(A,I3.3,A)')  '!                             adams_view_name=''OutUnits', I, '_SG'''
   WRITE (UnAD,FmtText     )  'STRING/'//TRIM(Int2LStr( TmpID2 ))
   WRITE (UnAD,FmtText     )  ', STRING = '//TRIM( OutParam(I)%Units )  ! The units of the output channels.

ENDDO             ! I - All selected output channels (plus time)



   ! The REQUEST statement:

WRITE (UnAD,FmtText  )  '!                             adams_view_name=''UserRequest_R'''
WRITE (UnAD,FmtText  )  'REQUEST/1'
WRITE (UnAD,FmtText  )  ', FUNCTION = USER( 1, '//TRIM(Flt2LStr( TStart ))//' )'



   ! The OUTPUT statement:

WRITE (UnAD,FmtText                   )  'OUTPUT/'
WRITE (UnAD,FmtText                   )  ', NOPRINT'
WRITE (UnAD,FmtText                   )  ', NOSEPARATOR'
IF ( SaveGrphcs )  WRITE (UnAD,FmtText)  ', GRSAVE'   ! Generate the Graphics output file.



   ! The RESULTS statement:

IF ( MakeLINacf )  THEN

!jmj Start of proposed change.  v6.10d-jmj  13-Aug-2009.
!jmj Modify the RESULTS statement so that it only contains DISPLACEMENTS and
!jmj   results from a LINEAR analysis, and format the results into
!jmj   human-readable format:
!remove6.10d   WRITE (UnAD,FmtText  )  'RESULTS/'
!remove6.10d   WRITE (UnAD,FmtText  )  ', NOACCELERATIONS'        ! Only contain results for displacements and from LINEAR ANALYSIS
   WRITE (UnAD,FmtText  )  'RESULTS/FORMATTED'  ! Only contain results from a LINEAR analysis
   WRITE (UnAD,FmtText  )  ', NOACCELERATIONS'
!jmj End of proposed change.  v6.10d-jmj  13-Aug-2009.
   WRITE (UnAD,FmtText  )  ', NOAPPLIEDFORCES'
   WRITE (UnAD,FmtText  )  ', NODATASTRUCTURES'
   WRITE (UnAD,FmtText  )  ', NOFLOATINGMARKERS'
   WRITE (UnAD,FmtText  )  ', NOREACTIONFORCES'
!jmj Start of proposed change.  v6.10d-jmj  13-Aug-2009.
!jmj Modify the RESULTS statement so that it only contains DISPLACEMENTS and
!jmj   results from a LINEAR analysis, and format the results into
!jmj   human-readable format:
   WRITE (UnAD,FmtText  )  ', NOREQUESTS'
!jmj End of proposed change.  v6.10d-jmj  13-Aug-2009.
   WRITE (UnAD,FmtText  )  ', NOSYSTEMELEMENTS'
!jmj Start of proposed change.  v6.10d-jmj  13-Aug-2009.
!jmj Modify the RESULTS statement so that it only contains DISPLACEMENTS and
!jmj   results from a LINEAR analysis, and format the results into
!jmj   human-readable format:
   WRITE (UnAD,FmtText  )  ', NOTIRES'
!jmj End of proposed change.  v6.10d-jmj  13-Aug-2009.
   WRITE (UnAD,FmtText  )  ', NOVELOCITIES'

ENDIF




   ! We're done!

WRITE (UnAD,FmtText  )  'END'


   ! Inform the users of this great news!

CALL WrScr1(' ADAMS dataset file '''//TRIM( RootName )//'_ADAMS.adm'' created.')


   ! Close the file.

CLOSE ( UnAD )

!bjj start of proposed change V6.02D-BJJ
   ! deallocate arrays

IF ( ALLOCATED( DRNodesGRA ) ) DEALLOCATE ( DRNodesGRA )
IF ( ALLOCATED( EAVec      ) ) DEALLOCATE ( EAVec      )
!bjj END of proposed change V6.02D-BJJ



RETURN
END SUBROUTINE MakeADM
!=======================================================================
!BJJ Start of proposed change vXX NWTC_Lib
END MODULE FAST2ADAMSSubs
!bjj end of proposed change