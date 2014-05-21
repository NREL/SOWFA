   ! NOTE: This source file contains example PitchCntrl(), UserHSSBr(),
   !       UserVSCont(), and UserYawCont() routines that are used to interface
   !       FAST with a master controller implemented as a dynamic-link-library
   !       (DLL) in the style of Garrad Hassan's Bladed wind turbine software
   !       package.  All four routines call routine BladedDLLInterface(), which
   !       contains a call to the Bladed-style DLL that evaluates as DISCON().
   !       Routine BladedDLLInterface() USEs a MODULE named
   !       BladedDLLParameters(), which stores values of PARAMETER constants
   !       used in the interface.

   !       This source file is useful if you have a DLL controller created for
   !       a Bladed model and you want to use the same controller for your FAST
   !       model.  This source file is also a useful template if you prefer to
   !       control pitch, HSS brake torque, generator torque, and yaw with a
   !       single master controller, regardless of whether or not you use the
   !       Bladed code and regardless of whether or not you want to work with
   !       DLLs.

   !       In order to use these routines, you must first set the values of the
   !       PARAMETERs contained in MODULE BladedDLLParameters() as required by
   !       your model.  These PARAMETERs are model-specific inputs available in
   !       the Bladed code, which are not available inputs in FAST, and are
   !       passed to the Bladed DLL in this interface.  You must then
   !       comment-out the dummy placeholder versions of routines PitchCntrl(),
   !       UserHSSBr(), UserVSCont(), and UserYawCont() contained in source
   !       file UserSubs.f90 and recompile FAST with this additional source
   !       file.  The executable version of FAST that is distributed with the
   !       FAST archive is NOT linked with the routines contained within this
   !       source file.  Once FAST has been compiled with these routines, in
   !       order to use them in your simulation, the following inputs from the
   !       primary input file should be set accordingly (these conditions are
   !       NOT tested by these example routines):
   !          YCMode    = 1    - tells FAST to use routine UserYawCont() for active yaw control
   !          TYCOn     = 0.0  - tells FAST to start active yaw control at the beginning of the simulation
   !          PCMode    = 1    - tells FAST to use routine PitchCntrl() for active pitch control
   !          TPCOn     = 0.0  - tells FAST to start active pitch control at the beginning of the simulation
   !          VSContrl  = 2    - tells FAST to use routine UserVSCont() for active variable-speed torque control
   !          GenTiStr  = True - tells FAST to start torque control based on time TimGenOn
   !          GenTiStp  = True - tells FAST to stop  torque control based on time TimGenOf
   !          TimGenOn  = 0.0  - tells FAST to start torque control at the beginning of the simulation
   !          TimGenOf  > TMax - tells FAST not to stop controlling torque throughout the simulation
   !          HSSBrMode = 2    - tells FAST to use routine UserHSSBr() for control of the HSS brake
   !          THSSBrDp  = 0.0  - tells FAST to start HSS brake torque control at the beginning of the simulation

   !       This interface is valid for DLLs of the style specified in
   !       Appendices A and B of the Bladed User Manual of Bladed version 3.6.
   !       If you are running FAST using a master controller DLL developed in
   !       Bladed, please be aware of the following differences between this
   !       interface and Bladed's interface:
   !          Record  1, the status flag, is NOT set to -1 for the final call at the end of the simulation
   !          Record 10, the pitch actuator type, is always set to 0 by FAST indicating pitch position actuator; as such, the returned value of Record 46, demanded pitch rate (Collective pitch), is always ignored
   !          Record 29, the yaw control type, is always set to 0 by FAST indicating yaw rate control; as such, the returned value of Record 41, demanded yaw actuator torque, is always ignored
   !          Record 35, the generator contactor status, is initialized to 1 by FAST indicating main (high speed) or variable speed generator; the generator can be turned off in the DLL by setting Record 35 to 0 or by setting Record 47 to 0.0; if the DLL redefines Record 35 to something other than 0 or 1 (such as 2 = low speed generator), the program will Abort
   !          Record 41, the demanded yaw actuator torque, is always ignored in accordance with the specification of Record 29
   !          Record 46, the demanded pitch rate (Collective pitch), is always ignored in accordance with the specification of Record 10
   !          Record 55, the pitch  override returned by the DLL, must be set to 0 by the DLL indicating no override (i.e., pitch  demands come for the DLL); the program will Abort otherwise
   !          Record 56, the torque override returned by the DLL, must be set to 0 by the DLL indicating no override (i.e., torque demands come for the DLL); the program will Abort otherwise
   !          Record 62, the max. number of values which can be returned for logging, is always set to 0 by FAST indicating none
   !          Record 63, the record number for start of logging output, is always set to 0 (a don't care) by FAST in accordance with the specification of Record 62
   !          Record 64, the max. number of characters which can be returned in "OUTNAME", is always set to 0 in accordance with the specification of Record 62
   !          Record 65, the number of variables returned for logging returned by the DLL, must be set to 0 by the DLL indicating none in accordance with the specification of Record 62; the program will Abort otherwise
   !          Record 72, the generator start-up resistance, is always ignored
   !          Record 79, the request for loads, is ignored; instead, the blade, hub, and yaw bearing loads are always passed to the DLL as if Record 79 was set to 4
   !          Record 80, the variable-slip current demand toggle switch, is always ignored; instead, the generator torque demand from Record 47 is always used
   !          Record 81, the variable-slip current demand, is always ignored in accordance with the handling of Record 80

!=======================================================================
MODULE DLL_Interface


USE                             kernel32


IMPLICIT                        NONE


   ! Defined TYPEs:

TYPE DLL_Type

   INTEGER(4)                :: FileAddr                                        ! The address of file FileName.
   INTEGER(4)                :: ProcAddr                                        ! The address of procedure ProcName.

   CHARACTER(1024)           :: FileName                                        ! The name of the DLL file including the full path to the current working directory.
   CHARACTER(1024)           :: ProcName                                        ! The name of the procedure in the DLL that will be called.

END TYPE DLL_Type


CONTAINS
!=======================================================================
   SUBROUTINE LoadDLL ( DLL )


      ! This SUBROUTINE is used to load the DLL.

!bjj start of proposed change
   USE NWTC_LIBRARY, ONLY: ProgAbort
!bjj end of proposed change


   IMPLICIT                        NONE


      ! Passed Variables:

   TYPE (DLL_Type), POINTER     :: DLL                                             ! The DLL to be loaded.



      ! Load the DLL and get the file address:

   DLL%FileAddr = LoadLibrary( TRIM(DLL%FileName)//CHAR(0) )

   IF ( DLL%FileAddr == 0 )  CALL ProgAbort ( ' The DLL '//TRIM(DLL%FileName)//' could not be loaded.' )


      ! Get the procedure address:

   DLL%ProcAddr = GetProcAddress( DLL%FileAddr, TRIM(DLL%ProcName)//CHAR(0) )

   IF ( DLL%ProcAddr == 0 )  CALL ProgAbort ( ' The procedure '//TRIM(DLL%ProcName)//' could not be loaded.' )



   RETURN
   END SUBROUTINE LoadDLL
!=======================================================================
   SUBROUTINE CallDLL ( DLL, avrSWAP, aviFAIL, accINFILE, avcOUTNAME, avcMSG )


      ! This SUBROUTINE is used to call the DLL.


   IMPLICIT                        NONE


      ! Passed Variables:

   REAL(4),    INTENT(INOUT)    :: avrSWAP   (*)                                   ! The swap array, used to pass data to, and receive data from, the DLL controller.

   INTEGER(4), INTENT(  OUT)    :: aviFAIL                                         ! A flag used to indicate the success of this DLL call set as follows: 0 if the DLL call was successful, >0 if the DLL call was successful but cMessage should be issued as a warning messsage, <0 if the DLL call was unsuccessful or for any other reason the simulation is to be stopped at this point with cMessage as the error message.

   INTEGER(1), INTENT(IN   )    :: accINFILE (*)                                   ! The address of the first record of an array of 1-byte CHARACTERs giving the name of the parameter input file, 'DISCON.IN'.
   INTEGER(1), INTENT(  OUT)    :: avcMSG    (*)                                   ! The address of the first record of an array of 1-byte CHARACTERS giving the message contained in cMessage, which will be displayed by the calling program if aviFAIL <> 0.
   INTEGER(1), INTENT(IN   )    :: avcOUTNAME(*)                                   ! The address of the first record of an array of 1-byte CHARACTERS giving the simulation run name without extension.

   TYPE (DLL_Type), INTENT(IN ) :: DLL                                             ! The DLL to be called.


      ! Local Variables:

   POINTER ( DLL_ProcAddr, DLL_Procedure )


      ! DLL Interface:

   INTERFACE
      SUBROUTINE DLL_Procedure ( avrSWAP, aviFAIL, accINFILE, avcOUTNAME, avcMSG )
         REAL(4)                :: avrSWAP   (*)
         INTEGER(4)             :: aviFAIL
         INTEGER(1)             :: accINFILE (*)
         INTEGER(1)             :: avcMSG    (*)
         INTEGER(1)             :: avcOUTNAME(*)
      END SUBROUTINE DLL_Procedure
   END INTERFACE



      ! Call the DLL through a POINTER:

   DLL_ProcAddr = DLL%ProcAddr
   CALL DLL_Procedure ( avrSWAP, aviFAIL, accINFILE, avcOUTNAME, avcMSG )



   RETURN
   END SUBROUTINE CallDLL
!=======================================================================
END MODULE DLL_Interface
!=======================================================================
MODULE BladedDLLParameters


   ! This MODULE stores various PARAMETER constants used in the
   !   Bladed-style master controller DLL interface.  Users will need
   !   to set these values as required by their models:


USE                             Precision


INTEGER(4), PARAMETER        :: N                 = 0                           ! No. of points in torque-speed look-up table: 0 = none and use the optimal mode PARAMETERs instead, nonzero = ignore the optimal mode PARAMETERs by setting Record 16 to 0.0 (-)
INTEGER(4), PARAMETER        :: Ptch_Cntrl        = 0                           ! Pitch control: 0 = collective, 1 = individual (-)

REAL(ReKi), PARAMETER        :: DTCntrl           = 0.0001                      ! Communication interval for controller (sec) (this works in conjunction with FAST's time step (DT) the same way DTAero in AeroDyn works with FAST--see the FAST User's Guide description of DTAero for more information)
REAL(ReKi), PARAMETER        :: Gain_OM           = 0.0                         ! Optimal mode gain (Nm/(rad/s)^2)
REAL(ReKi), PARAMETER        :: GenPwr_Dem        = 0.0                         ! Demanded power (W)
REAL(ReKi), PARAMETER        :: GenSpd_Dem        = 0.0                         ! Demanded generator speed above rated (rad/s)
REAL(ReKi), PARAMETER        :: GenSpd_MaxOM      = 0.0                         ! Optimal mode maximum speed (rad/s)
REAL(ReKi), PARAMETER        :: GenSpd_MinOM      = 0.0                         ! Minimum generator speed (rad/s)
REAL(ReKi), PARAMETER        :: GenSpd_TLU(N)     = 0.0                         ! Table (array) containing N generator speeds  for the torque-speed table look-up (TLU) (rad/s) -- this should be defined using an array constructor; for example, if N = 3: GenSpd_TLU(N)    = (/ 0.0, 99.9, 999.9 /)
REAL(ReKi), PARAMETER        :: GenTrq_Dem        = 0.0                         ! Demanded generator torque (Nm)
REAL(ReKi), PARAMETER        :: GenTrq_TLU(N)     = 0.0                         ! Table (array) containing N generator torques for the torque-speed table look-up (TLU) (Nm   ) -- this should be defined using an array constructor, for example, if N = 3: GenTrq_TLU(N)    = (/ 0.0, 10.0, 200.0 /)
REAL(ReKi), PARAMETER        :: Ptch_Max          = 0.0                         ! Maximum pitch angle (rad)
REAL(ReKi), PARAMETER        :: Ptch_Min          = 0.0                         ! Minimum pitch angle (rad)
REAL(ReKi), PARAMETER        :: Ptch_SetPnt       = 0.0                         ! Below-rated pitch angle set-point (rad)
REAL(ReKi), PARAMETER        :: PtchRate_Max      = 0.0                         ! Maximum pitch rate                               (rad/s)
REAL(ReKi), PARAMETER        :: PtchRate_Min      = 0.0                         ! Minimum pitch rate (most negative value allowed) (rad/s)
REAL(ReKi), PARAMETER        :: NacYaw_North      = 0.0                         ! Reference yaw angle of the nacelle when the upwind end points due North (rad)

CHARACTER(1024), PARAMETER   :: DLL_FileName      = 'DISCON.dll'                ! The name of the DLL file including the full path to the current working directory.
CHARACTER(1024), PARAMETER   :: DLL_ProcName      = 'DISCON'                    ! The name of the procedure in the DLL that will be called.


END MODULE BladedDLLParameters
!=======================================================================
SUBROUTINE BladedDLLInterface ( DirRoot, NumBl, BlPitchCom, HSSBrFrac, GenTrq, YawRateCom )


   ! This SUBROUTINE is used to call a master controller implemented as
   !   a dynamic-link-library (DLL) in the style of Garrad Hassan's
   !   Bladed wind turbine software package.  This interface is valid
   !   for DLLs of the style specified in Appendices A and B of the
   !   Bladed User Manual of Bladed version 3.6.  This SUBROUTINE is
   !   called separately by routines PitchCntrl(), UserHSSBr(),
   !   UserVSCont(), and UserYawCont() in order to pass the pitch,
   !   fraction of full braking torque, generator torque, and yaw
   !   commands (demands) back to the FAST or ADAMS dynamics codes.


USE                             BladedDLLParameters
USE                             DLL_Interface
!bjj Start of proposed change AD_v12.70-bjj
!rmUSE                             Identify
!USE                             Identify,   ONLY: DynProg       ! Why not use the FAST ProgName variable instead?
USE                             NWTC_Library    !ProgName
!bjj End of proposed change
USE                             Output
!bjj rm NWTC_Library: USE                             Precision


IMPLICIT                        NONE


   ! Passed Variables:

INTEGER(4), INTENT(IN )      :: NumBl                                           ! Number of blades, (-).

REAL(ReKi), INTENT(OUT)      :: BlPitchCom(NumBl)                               ! Commanded blade pitch angles (demand pitch angles) (rad).
REAL(ReKi), INTENT(OUT)      :: GenTrq                                          ! Electrical generator torque (N-m).
REAL(ReKi), INTENT(OUT)      :: HSSBrFrac                                       ! Fraction of full braking torque: 0 (off) <= HSSBrFrac <= 1 (full), (-).
REAL(ReKi), INTENT(OUT)      :: YawRateCom                                      ! Commanded nacelle-yaw angular rate (demand yaw rate) (rad/s).

CHARACTER(1024), INTENT(IN ) :: DirRoot                                         ! The name of the root file including the full path to the current working directory.  This may be useful if you want this routine to write a permanent record of what it does to be stored with the simulation results: the results should be stored in a file whose name (including path) is generated by appending any suitable extension to DirRoot.


   ! Local Variables:

INTEGER(4), PARAMETER        :: R                 = 85                          ! Start of below-rated torque-speed look-up table (record no.)

REAL(4)                      :: avrSWAP   (R+(2*N)-1)                           ! The swap array, used to pass data to, and receive data from, the DLL controller.
REAL(ReKi), SAVE,ALLOCATABLE :: BlPitchCom_SAVE(:)                              ! Commanded blade pitch angles (demand pitch angles) (rad).
!bjj NWTC_LIBRARY:REAL(ReKi), PARAMETER        :: D2R               = 0.017453293                 ! Factor to convert degrees to radians.
REAL(ReKi), SAVE             :: GenTrq_SAVE       = 0.0                         ! Electrical generator torque (N-m).
REAL(ReKi), SAVE             :: LastTime          = 0.0                         ! The simulation time at the end of the last DLL call (sec).
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Scale the value of AllOuts(Time) by OnePlusEps to ensure that the
!jmj   contoller is called at every time step when DTCntrl = DT, even in the
!jmj    presence of numerical precision errors:
REAL(ReKi), PARAMETER        :: OnePlusEps        = 1.0 + EPSILON(OnePlusEps)   ! The number slighty greater than unity in the precision of ReKi.
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
!bjj NWTC_LIBRARY:REAL(ReKi), PARAMETER        :: RPM2RPS           = 0.10471976                  ! Factor to convert revolutions per minute to radians per second.
REAL(ReKi), SAVE             :: YawRateCom_SAVE   = 0.0                         ! Commanded nacelle-yaw angular rate (demand yaw rate) (rad/s).

INTEGER(4)                   :: aviFAIL                                         ! A flag used to indicate the success of the DLL call as follows: 0 if the DLL call was successful, >0 if the DLL call was successful but cMessage should be issued as a warning messsage, <0 if the DLL call was unsuccessful or for any other reason the simulation is to be stopped at this point with cMessage as the error message
INTEGER(4), SAVE             :: BrkState_SAVE     = 0                           ! Shaft brake status: 0 = off, 1 = on (full) (-).
INTEGER(4), SAVE             :: GenState_SAVE     = 1                           ! Generator contactor: 0 = off, 1 = main (high speed) or variable speed generator (-).
INTEGER(4)                   :: I                                               ! Generic index.
INTEGER(4)                   :: K                                               ! Loops through blades.
INTEGER(4)                   :: Sttus                                           ! Status returned by an attempted allocation.

INTEGER(1)                   :: accINFILE (  256)                               ! The address of the first record of an array of 1-byte CHARACTERs giving the name of the parameter input file, 'DISCON.IN'.
INTEGER(1)                   :: avcMSG    (  256)                               ! The address of the first record of an array of 1-byte CHARACTERS returning a message that will be displayed if aviFAIL <> 0.
INTEGER(1)                   :: avcOUTNAME( 1024)                               ! The address of the first record of an array of 1-byte CHARACTERS giving the simulation run name without extension.
INTEGER(1)                   :: iInFile   (  256)                               ! CHARACTER string cInFile  stored as a 1-byte array.
INTEGER(1)                   :: iMessage  (  256)                               ! CHARACTER string cMessage stored as a 1-byte array.
INTEGER(1)                   :: iOutName  ( 1024)                               ! CHARACTER string cOutName stored as a 1-byte array.

CHARACTER( 256)              :: cInFile                                         ! CHARACTER string giving the name of the parameter input file, 'DISCON.IN'
CHARACTER( 256)              :: cMessage                                        ! CHARACTER string giving a message that will be displayed by the calling program if aviFAIL <> 0.
CHARACTER(1024)              :: cOutName                                        ! CHARACTER string giving the simulation run name without extension.

!bjj chg: LOGICAL(1), SAVE             :: FirstPas          = .TRUE.                      ! When .TRUE., indicates we're on the first pass.
LOGICAL,    SAVE             :: FirstPas          = .TRUE.                      ! When .TRUE., indicates we're on the first pass.

TYPE (DLL_Type),POINTER,SAVE :: DLL_Pntr                                        ! The DLL pointer.
TYPE (DLL_Type),TARGET ,SAVE :: DLL_Trgt                                        ! The DLL target.


   ! Set EQUIVALENCE relationships between INTEGER(1) byte arrays and CHARACTER strings:

EQUIVALENCE (iInFile , cInFile )
EQUIVALENCE (iMessage, cMessage)
EQUIVALENCE (iOutName, cOutName)




   ! ALLOCATE and initilize the BlPitchCom_SAVE first pass only:

IF ( FirstPas .AND. ( .NOT. ALLOCATED(BlPitchCom_SAVE) ) )  THEN

   ALLOCATE ( BlPitchCom_SAVE(NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the BlPitchCom_SAVE array.' )
   ENDIF

   BlPitchCom_SAVE = 0.0

   ! Don't set FirstPas = .FALSE. here yet, since FirstPas is also needed in
   !   the next block of code.

ENDIF




   ! Since multiple control routines (one each for pitch, yaw, and torque
   !   control) are calling this routine, let's add a test condition to make
   !   sure we don't call the DLL master controller more than once per time
   !   step (all subsequent calls to this routine after the first one will
   !   use the SAVEd values from the first call of the time step):
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Scale the value of AllOuts(Time) by OnePlusEps to ensure that the
!jmj   contoller is called at every time step when DTCntrl = DT, even in the
!jmj    presence of numerical precision errors:
!remove6.02a
!remove6.02aIF ( ( AllOuts(Time) - LastTime ) >= DTCntrl )  THEN ! Make sure time has incremented a time step.
   ! NOTE: AllOuts(Time) is scaled by OnePlusEps to ensure that the controller
   !       is called at every time step when DTCntrl = DT, even in the presence
   !       of numerical precision errors.

IF ( ( AllOuts(Time)*OnePlusEps  - LastTime ) >= DTCntrl )  THEN  ! Make sure time has incremented a controller time step.
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! Initialize the avrSWAP array to zero:

   avrSWAP     = 0.0



   ! Let's do things differently, depending on whether or not this is the first
   !   pass through this routine:

   IF ( FirstPas )  THEN

   ! Define and load the DLL:

      DLL_Trgt%FileName = DLL_FileName
      DLL_Trgt%ProcName = DLL_ProcName

      DLL_Pntr => DLL_Trgt

      CALL LoadDLL ( DLL_Pntr )


   ! Set status flag:

      avrSWAP( 1) = 0.0                                  ! Status flag set as follows: 0 if this is the first call, 1 for all subsequent time steps, -1 if this is the final call at the end of the simulation (-)

      FirstPas = .FALSE.                                 ! Don't enter here again!

   ELSE

      avrSWAP( 1) = 1.0                                  ! Status flag set as follows: 0 if this is the first call, 1 for all subsequent time steps, -1 if this is the final call at the end of the simulation (-)

   ENDIF




   ! Load control measurements and other information into the avrSWAP array
   !   according to Appendix A of the Bladed User Manual:

   avrSWAP( 2) = AllOuts(     Time)                      ! Current time (sec)
   avrSWAP( 3) = AllOuts(     Time) - LastTime           ! Communication interval (sec)
   avrSWAP( 4) = AllOuts(PtchPMzc1)*D2R                  ! Blade 1 pitch angle (rad)
   avrSWAP( 5) = Ptch_SetPnt                             ! Below-rated pitch angle set-point (rad)
   avrSWAP( 6) = Ptch_Min                                ! Minimum pitch angle (rad)
   avrSWAP( 7) = Ptch_Max                                ! Maximum pitch angle (rad)
   avrSWAP( 8) = PtchRate_Min                            ! Minimum pitch rate (most negative value allowed) (rad/s)
   avrSWAP( 9) = PtchRate_Max                            ! Maximum pitch rate                               (rad/s)
   avrSWAP(10) = 0.0                                     ! 0 = pitch position actuator, 1 = pitch rate actuator (-) -- must be 0 for FAST
   avrSWAP(11) = BlPitchCom_SAVE(1)                      ! Current demanded pitch angle (rad  ) -- I am sending the value for blade 1, in the absence of any more information provided in Bladed documentation
   avrSWAP(12) = 0.0                                     ! Current demanded pitch rate  (rad/s) -- always zero for FAST
   avrSWAP(13) = GenPwr_Dem                              ! Demanded power (W)
!bjj start of proposed change - FIX CONVERSION ERROR!
!rm   avrSWAP(14) = AllOuts(   RotPwr)*0.001                ! Measured shaft power (W)
!rm   avrSWAP(15) = AllOuts(   GenPwr)*0.001                ! Measured electrical power output (W)
   avrSWAP(14) = AllOuts(   RotPwr)*1000.                ! Measured shaft power (W)
   avrSWAP(15) = AllOuts(   GenPwr)*1000.                ! Measured electrical power output (W)
!bjj end of proposed change
   IF ( N == 0 )  THEN  ! Torque-speed table look-up not selected
      avrSWAP(16) = Gain_OM                              ! Optimal mode gain (Nm/(rad/s)^2)
   ELSE                 ! Torque-speed table look-up selected
      avrSWAP(16) = 0.0                                  ! Optimal mode gain (Nm/(rad/s)^2) -- 0.0 indicates that torque-speed table look-up is selected
   ENDIF
   avrSWAP(17) = GenSpd_MinOM                            ! Minimum generator speed (rad/s)
   avrSWAP(18) = GenSpd_MaxOM                            ! Optimal mode maximum speed (rad/s)
   avrSWAP(19) = GenSpd_Dem                              ! Demanded generator speed above rated (rad/s)
   avrSWAP(20) = AllOuts(  HSShftV)*RPM2RPS              ! Measured generator speed (rad/s)
   avrSWAP(21) = AllOuts(LSSTipVxa)*RPM2RPS              ! Measured rotor speed (rad/s)
   avrSWAP(22) = GenTrq_Dem                              ! Demanded generator torque (Nm)
!bjj start of proposed change - FIX CONVERSION ERROR!
!rm   avrSWAP(23) = AllOuts(    GenTq)*0.001                ! Measured generator torque (Nm)
   avrSWAP(23) = AllOuts(    GenTq)*1000.                ! Measured generator torque (Nm)
!bjj end of proposed change
   avrSWAP(24) = AllOuts(NacYawErr)*D2R                  ! Measured yaw error (rad)
   IF ( N == 0 )  THEN  ! Torque-speed table look-up not selected
      avrSWAP(25) = 0.0                                  ! Start of below-rated torque-speed look-up table (record no.) -- 0.0 indicates that torque-speed table look-up is not selected
      avrSWAP(26) = 0.0                                  ! No. of points in torque-speed look-up table (-)              -- 0.0 indicates that torque-speed table look-up is not selected
   ELSE                 ! Torque-speed table look-up selected
      avrSWAP(25) = R                                    ! Start of below-rated torque-speed look-up table (record no.)
      avrSWAP(26) = N                                    ! No. of points in torque-speed look-up table (-)
   ENDIF
   avrSWAP(27) = AllOuts( HorWindV)                      ! Hub wind speed (m/s)
   avrSWAP(28) = Ptch_Cntrl                              ! Pitch control: 0 = collective, 1 = individual (-)
   avrSWAP(29) = 0.0                                     ! Yaw control: 0 = yaw rate control, 1 = yaw torque control (-) -- must be 0 for FAST
!bjj start of proposed change - FIX CONVERSION ERROR!
!rm   avrSWAP(30) = AllOuts( RootMyc1)*0.001                ! Blade 1 root out-of-plane bending moment (Nm)
!rm   avrSWAP(31) = AllOuts( RootMyc2)*0.001                ! Blade 2 root out-of-plane bending moment (Nm)
!rm   avrSWAP(32) = AllOuts( RootMyc3)*0.001                ! Blade 3 root out-of-plane bending moment (Nm)
   avrSWAP(30) = AllOuts( RootMyc1)*1000.                ! Blade 1 root out-of-plane bending moment (Nm)
   avrSWAP(31) = AllOuts( RootMyc2)*1000.                ! Blade 2 root out-of-plane bending moment (Nm)
   avrSWAP(32) = AllOuts( RootMyc3)*1000.                ! Blade 3 root out-of-plane bending moment (Nm)
!bjj end of proposed change
   avrSWAP(33) = AllOuts(PtchPMzc2)*D2R                  ! Blade 2 pitch angle (rad)
   avrSWAP(34) = AllOuts(PtchPMzc3)*D2R                  ! Blade 3 pitch angle (rad)
   avrSWAP(35) = GenState_SAVE                           ! Generator contactor (-)
   avrSWAP(36) = BrkState_SAVE                           ! Shaft brake status: 0 = off, 1 = on (full) (-)
   avrSWAP(37) = AllOuts(   YawPzn)*D2R &
               + AllOuts(YawBrRDzt)*D2R &
               + AllOuts( PtfmRDzi)*D2R - NacYaw_North   ! Nacelle yaw angle from North (rad)
! Records 38-40 are reserved
! Records 41-48 are demands returned by the DLL [see the lines of code after the CALL to DISCON()]
   avrSWAP(49) = 256.0                                   ! Maximum no. of characters allowed in the returned message (-)
   avrSWAP(50) = 256.0                                   ! No. of characters in the "INFILE"  argument (-)
   avrSWAP(51) = 1024.0                                  ! No. of characters in the "OUTNAME" argument (-)
! Record 52 is reserved for future use                   ! DLL interface version number (-)
   avrSWAP(53) = AllOuts(YawBrTAxp)                      ! Tower top fore-aft     acceleration (m/s^2)
   avrSWAP(54) = AllOuts(YawBrTAyp)                      ! Tower top side-to-side acceleration (m/s^2)
! Record 55 is the pitch  override returned by the DLL [see the lines of code after the CALL to DISCON()]
! Record 56 is the torque override returned by the DLL [see the lines of code after the CALL to DISCON()]
! Records 57-59 are reserved
   avrSWAP(60) = AllOuts(LSSTipPxa)*D2R                  ! Rotor azimuth angle (rad)
   avrSWAP(61) = NumBl                                   ! No. of blades (-)
   avrSWAP(62) = 0.0                                     ! Max. number of values which can be returned for logging (-) -- must be 0 for FAST
   avrSWAP(63) = 0.0                                     ! Record number for start of logging output (-)
   avrSWAP(64) = 0.0                                     ! Max. number of characters which can be returned in "OUTNAME" (-) -- must be 0 for FAST
! Record 65 is the number of variables returned for logging by the DLL [see the lines of code after the CALL to DISCON()]
! Records 66-68 are reserved
!bjj start of proposed change - FIX CONVERSION ERROR!
!rm   avrSWAP(69) = AllOuts( RootMxc1)*0.001                ! Blade 1 root in-plane bending moment (Nm)
!rm   avrSWAP(70) = AllOuts( RootMxc2)*0.001                ! Blade 2 root in-plane bending moment (Nm)
!rm   avrSWAP(71) = AllOuts( RootMxc3)*0.001                ! Blade 3 root in-plane bending moment (Nm)
!rm! Record 72 is the generator start-up resistance returned by the DLL [see the lines of code after the CALL to DISCON()]
!rm   avrSWAP(73) = AllOuts(LSSTipMya)*0.001                ! Rotating hub My (GL co-ords) (Nm)
!rm   avrSWAP(74) = AllOuts(LSSTipMza)*0.001                ! Rotating hub Mz (GL co-ords) (Nm)
!rm   avrSWAP(75) = AllOuts(LSSTipMys)*0.001                ! Fixed hub My (GL co-ords) (Nm)
!rm   avrSWAP(76) = AllOuts(LSSTipMzs)*0.001                ! Fixed hub Mz (GL co-ords) (Nm)
!rm   avrSWAP(77) = AllOuts( YawBrMyn)*0.001                ! Yaw bearing My (GL co-ords) (Nm)
!rm   avrSWAP(78) = AllOuts( YawBrMzn)*0.001                ! Yaw bearing Mz (GL co-ords) (Nm)
   avrSWAP(69) = AllOuts( RootMxc1)*1000.                ! Blade 1 root in-plane bending moment (Nm)
   avrSWAP(70) = AllOuts( RootMxc2)*1000.                ! Blade 2 root in-plane bending moment (Nm)
   avrSWAP(71) = AllOuts( RootMxc3)*1000.                ! Blade 3 root in-plane bending moment (Nm)
! Record 72 is the generator start-up resistance returned by the DLL [see the lines of code after the CALL to DISCON()]
   avrSWAP(73) = AllOuts(LSSTipMya)*1000.                ! Rotating hub My (GL co-ords) (Nm)
   avrSWAP(74) = AllOuts(LSSTipMza)*1000.                ! Rotating hub Mz (GL co-ords) (Nm)
   avrSWAP(75) = AllOuts(LSSTipMys)*1000.                ! Fixed hub My (GL co-ords) (Nm)
   avrSWAP(76) = AllOuts(LSSTipMzs)*1000.                ! Fixed hub Mz (GL co-ords) (Nm)
   avrSWAP(77) = AllOuts( YawBrMyn)*1000.                ! Yaw bearing My (GL co-ords) (Nm)
   avrSWAP(78) = AllOuts( YawBrMzn)*1000.                ! Yaw bearing Mz (GL co-ords) (Nm)
!bjj end of proposed change
! Record 79 is the request for loads returned by the DLL [see the lines of code after the CALL to DISCON()]
! Records 80-81 are demands returned by the DLL [see the lines of code after the CALL to DISCON()]
   avrSWAP(82) = AllOuts(NcIMURAxs)*D2R                  ! Nacelle roll    acceleration (rad/s^2) -- this is in the shaft (tilted) coordinate system, instead of the nacelle (nontilted) coordinate system
   avrSWAP(83) = AllOuts(NcIMURAys)*D2R                  ! Nacelle nodding acceleration (rad/s^2)
   avrSWAP(84) = AllOuts(NcIMURAzs)*D2R                  ! Nacelle yaw     acceleration (rad/s^2) -- this is in the shaft (tilted) coordinate system, instead of the nacelle (nontilted) coordinate system
   DO I = 1,N  ! Loop through all torque-speed look-up table elements
      avrSWAP( R + (2*I) - 2 ) = GenSpd_TLU(I)           ! Generator speed  look-up table elements (rad/s)
      avrSWAP( R + (2*I) - 1 ) = GenTrq_TLU(I)           ! Generator torque look-up table elements (Nm   )
   ENDDO



   ! Create the input file and outname file arguments to the DLL (this requires
   !   the CHARACTER strings to be converted to byte arrays):

   cInFile = 'DISCON.IN'
   cOutName = TRIM( DirRoot )

   DO I = 1,MIN(  256, NINT( avrSWAP(50) ) )
      accINFILE (I) = iInFile (I)   ! Same as cInFile  by EQUIVALENCE
   ENDDO
   DO I = 1,MIN( 1024, NINT( avrSWAP(51) ) )
      avcOUTNAME(I) = iOutName(I)   ! Same as cOutName by EQUIVALENCE
   ENDDO



   ! Call the Bladed-style DLL controller:

   CALL CallDLL ( DLL_Pntr, avrSWAP, aviFAIL, accINFILE, avcOUTNAME, avcMSG )



   ! Load control demands (commands) out of the avrSWAP array according to
   !   Appendix A of the Bladed User Manual:

   GenState_SAVE = NINT( avrSWAP(35) )                   ! Generator contactor (-)
   BrkState_SAVE = NINT( avrSWAP(36) )                   ! Shaft brake status (-)
! Records 38-40 are reserved
! Record 41, demanded yaw actuator torque, is ignored since record 29 is set to 0 by FAST indicating yaw rate control

   IF (     Ptch_Cntrl == 1 )  THEN ! Individual pitch control

      DO K = 1,NumBl ! Loop through all blades avrSWAP(42), avrSWAP(43), and, if NumBl = 3, avrSWAP(44)
         BlPitchCom_SAVE(K) = avrSWAP( 41 + K )          ! Demanded individual pitch position of blade K (rad)
      ENDDO ! K - blades

   ELSEIF ( Ptch_Cntrl == 0 )  THEN ! Collective pitch control

      BlPitchCom_SAVE       = avrSWAP(45)                ! Demanded pitch angle (Collective pitch) (rad)
! Record 46, demanded pitch rate (Collective pitch), is ingored since record 10 is set to 0 by FAST indicating pitch position actuator

   ELSE                             ! Ptch_Cntrl incorrectly specified

      CALL ProgAbort ( ' Ptch_Cntrl must be set to 0 or 1 in MODULE BladedDLLParameters(). ' )

   ENDIF

   GenTrq_SAVE     = avrSWAP(47)                         ! Demanded generator torque (Nm)
   YawRateCom_SAVE = avrSWAP(48)                         ! Demanded nacelle yaw rate (rad/s)
! Records 57-59 are reserved
! Records 66-68 are reserved
! Record 72, the generator start-up resistance, is ignored
! Record 79, the request for loads, is ignored; instead, the blade, hub, and yaw bearing loads are always passed to the DLL as if Record 79 was set to 4
! Records 80-81, the variable-slip current demand inputs, are ignored; instead, the generator torque demand from Record 47 is used



   ! Display a warning message or ProgAbort if necessary:

   DO I = 1,MIN( 256, NINT( avrSWAP(49) ) )
      iMessage(I) = avcMSG(I)       ! Sets cMessage by EQUIVALENCE
   ENDDO

   IF (     NINT( avrSWAP( 1) ) == -1 )  THEN                           ! DLL-requested termination of the simulation
!bjj rm/replace NWTC_Library:      CALL WrScr ( ' STOP requested by '//TRIM(DLL_FileName)//': '   , '(A)' )
      CALL WrScr ( 'STOP requested by '//TRIM(DLL_FileName)//': ' )
      CALL ProgAbort       ( cMessage                                                  )
   ELSEIF ( aviFAIL < 0 )  THEN                                         ! Error in DLL; ProgAbort program
!bjj rm/replace NWTC_Library:      CALL WrScr ( ' ERROR message from '//TRIM(DLL_FileName)//': '  , '(A)' )
      CALL WrScr ( 'ERROR message from '//TRIM(DLL_FileName)//': ' )
      CALL ProgAbort       ( cMessage                                                  )
   ELSEIF ( aviFAIL > 0 )  THEN                                         ! Write warning message to screen
      CALL WrScr ( 'WARNING message from '//TRIM(DLL_FileName)//': ' )
      CALL WrScr ( cMessage                                           )
!bjj start of proposed change
!I'm writing a blank line here so that WrOver() in SimStatus doesn't make it look so strange
   CALL WrScr( ' ' )
!bjj end of proposed change
   ELSEIF ( ( GenState_SAVE /= 0 ) .AND. ( GenState_SAVE /= 1 ) )  THEN ! Generator contactor indicates something other than off or main; abort program
!bjj start of proposed change  (removed DynProg and replace with ProgName)
!rm      CALL ProgAbort ( ' Only off and main generators supported in '//TRIM( DynProg )//'.  Set avrSWAP(35) to 0 or 1 in '//    &
!rm                                                                                                   TRIM(DLL_FileName)//'.'      )
      CALL ProgAbort ( ' Only off and main generators supported in '//TRIM( ProgName )//'.  Set avrSWAP(35) to 0 or 1 in '//    &
                                                                                                   TRIM(DLL_FileName)//'.'      )
!bjj end of proposed change
   ELSEIF ( ( BrkState_SAVE /= 0 ) .AND. ( BrkState_SAVE /= 1 ) )  THEN ! Shaft brake status specified incorrectly; abort program
!bjj start of proposed change  (removed DynProg and replace with ProgName)
!rm      CALL ProgAbort ( ' Shaft brake status improperly set in '//TRIM( DynProg )//'.  Set avrSWAP(36) to 0 or 1 in '//    &
!rm                                                                                              TRIM(DLL_FileName)//'.'           )
      CALL ProgAbort ( ' Shaft brake status improperly set in '//TRIM( ProgName )//'.  Set avrSWAP(36) to 0 or 1 in '//    &
                                                                                              TRIM(DLL_FileName)//'.'           )
!bjj end of proposed change
   ELSEIF ( NINT( avrSWAP(55) ) /=  0 )  THEN                           ! Pitch  override requested by DLL; abort program
!bjj start of proposed change  (removed DynProg and replace with ProgName)
!jmj Correct this error message:
!rm      CALL ProgAbort ( ' Pitch override unsupported in '//TRIM( DynProg )//'.  Set avrSWAP(55) to 0 in '//TRIM(DLL_FileName)//'.'   )
      CALL ProgAbort ( ' Built-in pitch unsupported in '//TRIM( ProgName )//'.  Set avrSWAP(55) to 0 in '//TRIM(DLL_FileName)//'.' )
!bjj end of proposed change
   ELSEIF ( NINT( avrSWAP(56) ) /=  0 )  THEN                           ! Torque override requested by DLL; abort program
!bjj start of proposed change  (removed DynProg and replace with ProgName)
!jmj Correct this error message:
!rm      CALL ProgAbort ( ' Torque override unsupported in '//TRIM( DynProg )//'.  Set avrSWAP(56) to 0 in '//TRIM(DLL_FileName)//'.'  )
      CALL ProgAbort ( ' Built-in torque unsupported in '//TRIM( ProgName )//'.  Set avrSWAP(56) to 0 in '//TRIM(DLL_FileName)//'.')
!bjj end of proposed change
   ELSEIF ( NINT( avrSWAP(65) ) /=  0 )  THEN                           ! Return variables for logging requested by DLL; abort program
!bjj start of proposed change  (removed DynProg and replace with ProgName)
!rm      CALL ProgAbort ( ' Return variables unsupported in '//TRIM( DynProg )//'.  Set avrSWAP(65) to 0 in '//TRIM(DLL_FileName)//'.' )
      CALL ProgAbort( ' Return variables unsupported in '//TRIM( ProgName )//'.  Set avrSWAP(65) to 0 in '//TRIM(DLL_FileName)//'.')
!bjj end of proposed change
   ENDIF



   ! Reset the value of LastTime to the current time:

   LastTime = AllOuts(Time)



ENDIF




   ! Return the SAVEd demand values of pitch, HSS brake fraction, generator
   !   torque, and yaw at every call:

BlPitchCom = BlPitchCom_SAVE
HSSBrFrac  = BrkState_SAVE
GenTrq     = GenState_SAVE*GenTrq_SAVE ! Set GenTrq to 0.0 if GenState = 0 = off, or GenTrq_SAVE if GenState = 1 = main (high speed) or variable speed generator
YawRateCom = YawRateCom_SAVE



RETURN
END SUBROUTINE BladedDLLInterface
!=======================================================================
SUBROUTINE PitchCntrl ( BlPitch, ElecPwr, HSS_Spd, GBRatio, TwrAccel, NumBl, ZTime, DT, DirRoot, BlPitchCom )


   ! This example pitch control SUBROUTINE is used to call an interface
   !   to a master controller implemented as a dynamic-link-library
   !   (DLL) in the style of Garrad Hassan's Bladed wind turbine
   !   software package.  The interface routine BladedDLLInterface(), in
   !   turn, contains a call to the Bladed-style DLL.


!bjj rm NWTC_Library: USE                             Precision
USE                           NWTC_Library


IMPLICIT                        NONE


   ! Passed variables:

INTEGER(4), INTENT(IN )      :: NumBl                                           ! Number of blades, (-).

REAL(ReKi), INTENT(IN )      :: BlPitch   (NumBl)                               ! Current values of the blade pitch angles, rad.
REAL(ReKi), INTENT(IN )      :: DT                                              ! Integration time step, sec.
REAL(ReKi), INTENT(IN )      :: ElecPwr                                         ! Electrical power, watts.
REAL(ReKi), INTENT(IN )      :: GBRatio                                         ! Gearbox ratio, (-).
REAL(ReKi), INTENT(IN )      :: HSS_Spd                                         ! HSS speed, rad/s.
REAL(ReKi), INTENT(OUT)      :: BlPitchCom(NumBl)                               ! Commanded blade pitch angles (demand pitch angles), rad.
REAL(ReKi), INTENT(IN )      :: TwrAccel                                        ! Tower Acceleration, m/s^2.
REAL(ReKi), INTENT(IN )      :: ZTime                                           ! Current simulation time, sec.

CHARACTER(1024), INTENT(IN ) :: DirRoot                                         ! The name of the root file including the full path to the current working directory.  This may be useful if you want this routine to write a permanent record of what it does to be stored with the simulation results: the results should be stored in a file whose name (including path) is generated by appending any suitable extension to DirRoot.


   ! Local variables:

REAL(ReKi)                   :: GenTrq                                          ! Electrical generator torque, N-m. -- NOT used by this routine
REAL(ReKi)                   :: HSSBrFrac                                       ! Fraction of full braking torque: 0 (off) <= HSSBrFrac <= 1 (full), (-). -- NOT used by this routine
REAL(ReKi)                   :: YawRateCom                                      ! Commanded nacelle-yaw angular rate (demand yaw rate), rad/s. -- NOT used by this routine



   ! Call the DLL interface routine to get BlPitchCom:

CALL BladedDLLInterface ( DirRoot, NumBl, BlPitchCom, HSSBrFrac, GenTrq, YawRateCom )



RETURN
END SUBROUTINE PitchCntrl
!=======================================================================
SUBROUTINE UserHSSBr ( GenTrq, ElecPwr, HSS_Spd, GBRatio, NumBl, ZTime, DT, DirRoot, HSSBrFrac )


   ! This example HSS brake SUBROUTINE is used to call an interface
   !   to a master controller implemented as a dynamic-link-library
   !   (DLL) in the style of Garrad Hassan's Bladed wind turbine
   !   software package.  The interface routine BladedDLLInterface(), in
   !   turn, contains a call to the Bladed-style DLL.


!bjj rm NWTC_Library: USE                             Precision
USE                           NWTC_Library


IMPLICIT                        NONE


   ! Passed Variables:

INTEGER(4), INTENT(IN )      :: NumBl                                           ! Number of blades, (-).

REAL(ReKi), INTENT(IN )      :: DT                                              ! Integration time step, sec.
REAL(ReKi), INTENT(IN )      :: ElecPwr                                         ! Electrical power (account for losses), watts.
REAL(ReKi), INTENT(IN )      :: GBRatio                                         ! Gearbox ratio, (-).
REAL(ReKi), INTENT(IN )      :: GenTrq                                          ! Electrical generator torque, N-m.
REAL(ReKi), INTENT(IN )      :: HSS_Spd                                         ! HSS speed, rad/s.
REAL(ReKi), INTENT(OUT)      :: HSSBrFrac                                       ! Fraction of full braking torque: 0 (off) <= HSSBrFrac <= 1 (full), (-).
REAL(ReKi), INTENT(IN )      :: ZTime                                           ! Current simulation time, sec.

CHARACTER(1024), INTENT(IN ) :: DirRoot                                         ! The name of the root file including the full path to the current working directory.  This may be useful if you want this routine to write a permanent record of what it does to be stored with the simulation results: the results should be stored in a file whose name (including path) is generated by appending any suitable extension to DirRoot.


   ! Local variables:

REAL(ReKi)                   :: BlPitchCom(NumBl)                               ! Commanded blade pitch angles (demand pitch angles), rad. -- NOT used by this routine
REAL(ReKi)                   :: GenTorq                                         ! Electrical generator torque, N-m. -- NOT used by this routine
REAL(ReKi)                   :: YawRateCom                                      ! Commanded nacelle-yaw angular rate (demand yaw rate), rad/s. -- NOT used by this routine



   ! Call the DLL interface routine to get HSSBrFrac:

!jmj Start of proposed change.  v6.10d-jmj  13-Aug-2009.
!jmj Bug fix: We should be using the dummy GenTorq variable here instead of
!jmj   GenTrq:
!remove6.10dCALL BladedDLLInterface ( DirRoot, NumBl, BlPitchCom, HSSBrFrac, GenTrq, YawRateCom )
CALL BladedDLLInterface ( DirRoot, NumBl, BlPitchCom, HSSBrFrac, GenTorq, YawRateCom )
!jmj End of proposed change.  v6.10d-jmj  13-Aug-2009.



RETURN
END SUBROUTINE UserHSSBr
!=======================================================================
SUBROUTINE UserVSCont ( HSS_Spd, GBRatio, NumBl, ZTime, DT, GenEff, DelGenTrq, DirRoot, GenTrq, ElecPwr )


   ! This example variable-speed torque control SUBROUTINE is used to
   !   call an interface to a master controller implemented as a
   !   dynamic-link-library (DLL) in the style of Garrad Hassan's Bladed
   !   wind turbine software package.  The interface routine
   !   BladedDLLInterface(), in turn, contains a call to the
   !   Bladed-style DLL.


!bjj rm NWTC_Library: USE                             Precision
USE                           NWTC_Library


IMPLICIT                        NONE


   ! Passed Variables:

INTEGER(4), INTENT(IN )      :: NumBl                                           ! Number of blades, (-).

REAL(ReKi), INTENT(IN )      :: DelGenTrq                                       ! Pertubation in generator torque used during FAST linearization (zero otherwise), N-m.
REAL(ReKi), INTENT(IN )      :: DT                                              ! Integration time step, sec.
REAL(ReKi), INTENT(OUT)      :: ElecPwr                                         ! Electrical power (account for losses), watts.
REAL(ReKi), INTENT(IN )      :: GBRatio                                         ! Gearbox ratio, (-).
REAL(ReKi), INTENT(IN )      :: GenEff                                          ! Generator efficiency, (-).
REAL(ReKi), INTENT(OUT)      :: GenTrq                                          ! Electrical generator torque, N-m.
REAL(ReKi), INTENT(IN )      :: HSS_Spd                                         ! HSS speed, rad/s.
REAL(ReKi), INTENT(IN )      :: ZTime                                           ! Current simulation time, sec.

CHARACTER(1024), INTENT(IN ) :: DirRoot                                         ! The name of the root file including the full path to the current working directory.  This may be useful if you want this routine to write a permanent record of what it does to be stored with the simulation results: the results should be stored in a file whose name (including path) is generated by appending any suitable extension to DirRoot.


   ! Local variables:

REAL(ReKi)                   :: BlPitchCom(NumBl)                               ! Commanded blade pitch angles (demand pitch angles), rad. -- NOT used by this routine
REAL(ReKi)                   :: HSSBrFrac                                       ! Fraction of full braking torque: 0 (off) <= HSSBrFrac <= 1 (full), (-). -- NOT used by this routine
REAL(ReKi)                   :: YawRateCom                                      ! Commanded nacelle-yaw angular rate (demand yaw rate), rad/s. -- NOT used by this routine



   ! Call the DLL interface routine to get GenTrq:

CALL BladedDLLInterface ( DirRoot, NumBl, BlPitchCom, HSSBrFrac, GenTrq, YawRateCom )


   ! Add-in the pertubation of generator torque:

GenTrq  = GenTrq + DelGenTrq  ! Make sure to add the pertubation on generator torque, DelGenTrq.  This is used only for FAST linearization (it is zero otherwise).


   ! Compute the electric generator power:
   ! The generator efficiency is either additive for motoring,
   !   or subtractive for generating power.

IF ( GenTrq > 0.0 )  THEN
   ElecPwr = GenTrq*HSS_Spd*GenEff
ELSE
   ElecPwr = GenTrq*HSS_Spd/GenEff
ENDIF



RETURN
END SUBROUTINE UserVSCont
!=======================================================================
SUBROUTINE UserYawCont ( YawPos, YawRate, WindDir, YawError, NumBl, ZTime, DT, DirRoot, YawPosCom, YawRateCom )


   ! This example yaw control SUBROUTINE is used to call an interface to
   !   a master controller implemented as a dynamic-link-library (DLL)
   !   in the style of Garrad Hassan's Bladed wind turbine software
   !   package.  The interface routine BladedDLLInterface(), in turn,
   !   contains a call to the Bladed-style DLL.


USE                             Output
!bjj rm NWTC_Library: USE                             Precision


IMPLICIT                        NONE


   ! Passed Variables:

INTEGER(4), INTENT(IN )      :: NumBl                                           ! Number of blades, (-).

REAL(ReKi), INTENT(IN )      :: DT                                              ! Integration time step, sec.
REAL(ReKi), INTENT(IN )      :: WindDir                                         ! Current horizontal hub-height wind direction (positive about the zi-axis), rad.
REAL(ReKi), INTENT(IN )      :: YawError                                        ! Current nacelle-yaw error estimate (positve about the zi-axis), rad.
REAL(ReKi), INTENT(IN )      :: YawPos                                          ! Current nacelle-yaw angular position, rad.
REAL(ReKi), INTENT(OUT)      :: YawPosCom                                       ! Commanded nacelle-yaw angular position (demand yaw angle), rad.
REAL(ReKi), INTENT(IN )      :: YawRate                                         ! Current nacelle-yaw angular rate, rad/s.
REAL(ReKi), INTENT(OUT)      :: YawRateCom                                      ! Commanded nacelle-yaw angular rate (demand yaw rate), rad/s.
REAL(ReKi), INTENT(IN )      :: ZTime                                           ! Current simulation time, sec.

CHARACTER(1024), INTENT(IN ) :: DirRoot                                         ! The name of the root file including the full path to the current working directory.  This may be useful if you want this routine to write a permanent record of what it does to be stored with the simulation results: the results should be stored in a file whose name (including path) is generated by appending any suitable extension to DirRoot.


   ! Local variables:

REAL(ReKi)                   :: BlPitchCom(NumBl)                               ! Commanded blade pitch angles (demand pitch angles), rad. -- NOT used by this routine
REAL(ReKi)                   :: GenTrq                                          ! Electrical generator torque, N-m. -- NOT used by this routine
REAL(ReKi)                   :: HSSBrFrac                                       ! Fraction of full braking torque: 0 (off) <= HSSBrFrac <= 1 (full), (-). -- NOT used by this routine
REAL(ReKi), SAVE             :: LastTime                                        ! The simulation time at the end of the last call to this routine, sec.
REAL(ReKi), SAVE             :: LastYawPosCom                                   ! Commanded nacelle-yaw angular position (demand yaw angle) computed during the lass call to this routine, rad.

!bjj chg: LOGICAL(1), SAVE             :: FirstPas          = .TRUE.                      ! When .TRUE., indicates we're on the first pass.
LOGICAL,    SAVE             :: FirstPas          = .TRUE.                      ! When .TRUE., indicates we're on the first pass.



   ! Let's initialize the values of LastTime and LastYawPosCom on the first
   !   pass only:

IF ( FirstPas )  THEN

   LastTime      = AllOuts(Time) - DT
   LastYawPosCom = YawPos

   FirstPas      = .FALSE. ! Don't enter here again!

ENDIF



   ! Call the DLL interface routine to get YawRateCom:

CALL BladedDLLInterface ( DirRoot, NumBl, BlPitchCom, HSSBrFrac, GenTrq, YawRateCom )


   ! Integrate to find the command nacelle-yaw angular position, YawPosCom:

YawPosCom     = LastYawPosCom + YawRateCom*( AllOuts(Time) - LastTime )



   ! Reset the value of LastTime and LastYawPosCom to the current values:

LastTime      = AllOuts(Time)
LastYawPosCom = YawPosCom



RETURN
END SUBROUTINE UserYawCont
!=======================================================================
