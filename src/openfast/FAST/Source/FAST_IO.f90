!BJJ Start of proposed change vXX NWTC_Lib
MODULE FAST_IO_Subs

   USE   NWTC_Library

CONTAINS
!====================================================================================================
SUBROUTINE AeroInput()
! This subroutine sets up the information needed to initialize AeroDyn, then initializes AeroDyn
!----------------------------------------------------------------------------------------------------

   USE                     AeroElem !,   ONLY: ADAeroMarkers, NumADBldNodes, ADCurrentOutputs, ADIntrfaceOptions, ADFirstLoop, Prev_Aero_t
   USE                     General,    ONLY: RootName, ADFile, SumPrint
   USE                     TurbConf,   ONLY: NumBl, TipRad, HubRad, SinPreC, CosPreC
   USE                     Output,     ONLY: WrEcho


   USE                     AeroDyn

   IMPLICIT NONE

      ! Local variables

   TYPE(AD_InitOptions)       :: ADOptions               ! Options for AeroDyn

   INTEGER                    :: ErrStat


      ! Set up the AeroDyn parameters
   ADOptions%ADInputFile      = ADFile
   ADOptions%OutRootName      = RootName
   ADOptions%WrSumFile        = SumPrint
   
   
      ! Hub position and orientation (relative here, but does not need to be)

   ADInterfaceComponents%Hub%Position(:)      = 0.0
   
   ADInterfaceComponents%Hub%Orientation(1,1) = 0.0
   ADInterfaceComponents%Hub%Orientation(1,1) = 1.0
   ADInterfaceComponents%Hub%Orientation(2,2) = 1.0
   ADInterfaceComponents%Hub%Orientation(3,3) = 1.0
   
   
      ! Blade root position and orientation (relative here, but does not need to be)

   IF (.NOT. ALLOCATED( ADInterfaceComponents%Blade ) ) THEN
      ALLOCATE( ADInterfaceComponents%Blade( NumBl ), STAT = ErrStat )
      IF ( ErrStat /= 0 ) THEN
         CALL ProgAbort( ' Error allocating space for ADInterfaceComponents%Blade.' )
      END IF
   END IF

  ADInterfaceComponents%Blade(:)%Position(1)      = HubRad*SinPreC(:)
  ADInterfaceComponents%Blade(:)%Position(2)      =  0.0
  ADInterfaceComponents%Blade(:)%Position(3)      = HubRad*CosPreC(:)
  
  ADInterfaceComponents%Blade(:)%Orientation(1,1) =        CosPreC(:)
  ADInterfaceComponents%Blade(:)%Orientation(2,1) =  0.0
  ADInterfaceComponents%Blade(:)%Orientation(3,1) =  1.0 * SinPreC(:)
  
  ADInterfaceComponents%Blade(:)%Orientation(1,2) =  0.0
  ADInterfaceComponents%Blade(:)%Orientation(2,2) =  1.0
  ADInterfaceComponents%Blade(:)%Orientation(3,2) =  0.0

  ADInterfaceComponents%Blade(:)%Orientation(1,3) = -1.0 * SinPreC(:)
  ADInterfaceComponents%Blade(:)%Orientation(2,3) =  0.0
  ADInterfaceComponents%Blade(:)%Orientation(3,3) =        CosPreC(:)


      ! Blade length
   
   ADInterfaceComponents%BladeLength = TipRad - HubRad
   
!print *, HubRad, TipRad, ACOS(CosPreC(1))   
     
      ! Initialize AeroDyn

   ADAeroMarkers = AD_Init(ADOptions, ADInterfaceComponents, ErrStat)    ! relative markers are returned



      ! get the number of blade nodes from the returned data structure'
   IF (.NOT. ALLOCATED( ADAeroMarkers%Blade ) ) THEN
      CALL ProgAbort( 'AeroDyn blade nodes are required to calculate aerodynamic loads.' )
   ELSE
      NumADBldNodes = SIZE( ADAeroMarkers%Blade, 1 )
   END IF

      ! allocate variables for aerodyn forces

   IF (.NOT. ALLOCATED(ADIntrfaceOptions%SetMulTabLoc)) THEN
      ALLOCATE( ADIntrfaceOptions%SetMulTabLoc(NumADBldNodes, NumBl), STAT = ErrStat )
      IF ( ErrStat /= 0 ) CALL ProgAbort ( ' Error allocating memory for ADIntrfaceOptions%SetMulTabLoc array.' )
   END IF

   ADIntrfaceOptions%SetMulTabLoc(:,:) = .FALSE.
   ADIntrfaceOptions%LinearizeFlag     = .FALSE.

!   bjj: we don't use this, so don't allocate it
!
!   IF (.NOT. ALLOCATED(ADIntrfaceOptions%MulTabLoc)) THEN
!      ALLOCATE(ADIntrfaceOptions%MulTabLoc(NumADBldNodes, NumBl), STAT = Sttus )
!      IF ( Sttus /= 0 ) CALL ProgAbort ( ' Error allocating memory for ADIntrfaceOptions%MulTabLoc array.' )
!   END IF


   CALL Set_FAST_Params()

   RETURN
END SUBROUTINE AeroInput
!====================================================================================================
!bjj end of proposed change
SUBROUTINE Begin


   ! This subroutine Prints out the name and version of the program, checks for
   !  command-line arguments, and creates the names of the output files.


USE                             General
!bjj rm NWTC_Library: USE                             SysSubs
USE                             TurbConf

!bjj start of proposed change vXX
!USE                             FAST_SysSubs !Get_CWD
!bjj end of proposed change

IMPLICIT                        NONE


   ! Local variables.

!bjj rm unused:INTEGER(4)                   :: I                                               ! A generic index for DO loops.
!bjj rm unused:INTEGER(4)                   :: LastChar                                        ! The location of the last non-blank character in the primary file name.
!bjj start of proposed change vXX
INTEGER                      :: Stat                                            ! The status of the call to GET_CWD
!bjj end of proposed change vXX


CHARACTER(1024)              :: DirName                                         ! A CHARACTER string containing the path of the current working directory.


   ! User-defined functions.

!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.



   ! Check for command line arguments.  Maybe the user specified an input file name.
   ! Don't perform this check when interfaced with Simulink:

!bjj Start of proposed change vXX
!rmIF ( .NOT. Cmpl4SFun )  CALL CheckArgs
IF ( .NOT. Cmpl4SFun )  CALL CheckArgs( PriFile )
!bjj End of proposed change

!bjj start of proposed change
!rm   ! Let's parse the root file name from the name of the primary input file.
!rm   ! We'll count everything after the last period as the extension.
!rm
!rmLastChar = LEN_TRIM( PriFile )
!rm
!rmDO I=LastChar,1,-1
!rm   IF ( PriFile(I:I) == '.' )  THEN
!rm      RootName = PriFile(:I-1)
!rm   ENDIF
!rmENDDO ! I
!rm
!rmIF ( LEN_TRIM( RootName ) == 0 )  RootName = PriFile

CALL GetRoot( PriFile, RootName )
!bjj end of proposed change


   ! Let's create a root file name variable, DirRoot, which includes the full
   !   path to the current working directory.

!bjj start of proposed change vXX
!rm CALL Get_CWD  ( DirName )
!rmDirRoot = TRIM( DirName )//'\'//TRIM( RootName )
CALL Get_CWD  ( DirName, Stat )
IF (Stat /= 0) CALL ProgAbort('Error retreiving current working directory.')

DirRoot = TRIM( DirName )//PathSep//TRIM( RootName )
!bjj end of proposed change vXX

   ! Let's create the name of the output file.
!bjj start of proposed change
!rmIF ( Cmpl4SFun )  THEN     ! FAST has been compiled as an S-Function for Simulink
!rm   OutFile = TRIM( RootName )//'_SFunc.out'
!rmELSE                       ! FAST has been compiled normally
!rm   OutFile = TRIM( RootName )//'.out'
!rmENDIF

IF ( Cmpl4SFun )  RootName = TRIM( RootName )//'_SFunc'
!bjj end of proposed change

RETURN
END SUBROUTINE Begin
!=======================================================================
SUBROUTINE ChckOutLst


   ! This routine checks to see if any inputted output channels (stored
   !    in the OutList(:)) are ill-conditioned (and if so, FAST Aborts)
   !    and assigns the settings for OutInd(:) and OutParam(:) (i.e, the
   !    index, name, and units of the output channels, OutData(:)).


USE                             Features
USE                             General
USE                             Output
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
USE                             Platform
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
USE                             TurbConf

IMPLICIT                        NONE


   ! Local variables.

INTEGER(4)                   :: I                                               ! Generic index.
INTEGER(4)                   :: Sttus                                           ! Status returned from an allocation request.

CHARACTER(10)                :: OutListTmp                                      ! A string to temporarily hold OutList(I).
CHARACTER(28)                :: OutPFmt    = "( I4, 3X,A 10,1 X, A10 )"         ! Output format parameter output list.



   ! ALLOCATE some arrays:

ALLOCATE ( OutData(0:NumOuts) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the OutData array.' )
ENDIF

ALLOCATE ( OutInd(0:NumOuts) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the OutInd array.' )
ENDIF

ALLOCATE ( OutParam(0:NumOuts) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the OutParam array.' )
ENDIF

ALLOCATE ( OutSign(0:NumOuts) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the OutSign array.' )
ENDIF



   ! Initialize OutSign(:) to 1:

OutSign           = 1


   ! Set index, name, and units for the time output channel:

OutInd  (0)       = 0         !
OutParam(0)%Name  = 'Time'    ! OutData(0) is the time channel by default.
OutParam(0)%Units = '(sec)'   !

IF ( Echo )  THEN
   WRITE (UnEc,"(/, '  Col  Parameter  Units', /, '  ---  ---------  -----')")
   WRITE (UnEc,OutPFmt)  0, OutParam(0)%Name, OutParam(0)%Units
ENDIF


   ! Set index, name, and units for all of the output channels.
   ! If a selected output channel is not available by FAST, ProgAbort.

DO I = 1,NumOuts

   OutParam(I)%Name  = OutList(I)

   OutListTmp = OutList(I)


   ! Reverse the sign (+/-) of the output channel if the user prefixed the
   !   channel name with a '-', '_', 'm', or 'M' character indicating "minus".
   ! NOTE: If, in the future, any output channel is named with a character
   !   beginning with 'm' (or 'M'), this simple code will have to be made
   !   a lot more complex!

   ! SEE NOTE ABOVE ON THIS IF...THEN STATEMENT BELOW:
   IF ( ( OutListTmp(1:1) == '-' ) .OR. ( OutListTmp(1:1) == '_' ) .OR. &
        ( OutListTmp(1:1) == 'm' ) .OR. ( OutListTmp(1:1) == 'M' )        )  THEN
   ! SEE NOTE ABOVE ON THIS IF...THEN STATEMENT ABOVE:
      OutSign (I)       = -1     ! ex, '-TipDxc1' causes the sign of TipDxc1 to be switched.
      OutListTmp        = OutListTmp(2:)
   END IF
   CALL Conv2UC( OutListTmp )    ! Convert OutListTmp to upper case


   SELECT CASE ( OutListTmp )


   ! Blade 1 Tip Motions:

   CASE ( 'TIPDXC1', 'OOPDEFL1' )
      OutInd  (I)       = TipDxc1
      OutParam(I)%Units = '(m)'
   CASE ( 'TIPDYC1', 'IPDEFL1' )
      OutInd  (I)       = TipDyc1
      OutParam(I)%Units = '(m)'
   CASE ( 'TIPDZC1', 'TIPDZB1' )
      OutInd  (I)       = TipDzc1
      OutParam(I)%Units = '(m)'
   CASE ( 'TIPDXB1' )
      OutInd  (I)       = TipDxb1
      OutParam(I)%Units = '(m)'
   CASE ( 'TIPDYB1' )
      OutInd  (I)       = TipDyb1
      OutParam(I)%Units = '(m)'
   CASE ( 'TIPALXB1' )
      OutInd  (I)       = TipALxb1
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'TIPALYB1' )
      OutInd  (I)       = TipALyb1
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'TIPALZB1' )
      OutInd  (I)       = TipALzb1
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'TIPRDXB1', 'ROLLDEFL1' )
      OutInd  (I)       = TipRDxb1
      OutParam(I)%Units = '(deg)'
   CASE ( 'TIPRDYB1', 'PTCHDEFL1' )
      OutInd  (I)       = TipRDyb1
      OutParam(I)%Units = '(deg)'
   CASE ( 'TIPRDZC1', 'TIPRDZB1', 'TWSTDEFL1' )
      OutInd  (I)       = TipRDzc1
      OutParam(I)%Units = '(deg)'
   CASE ( 'TIPCLRNC1', 'TWRCLRNC1', 'TIP2TWR1' )
      OutInd  (I)       = TipClrnc1
      OutParam(I)%Units = '(m)'


   ! Blade 1 Local Span Motions:

   CASE ( 'SPN1ALXB1' )
      OutInd  (I)       = Spn1ALxb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 1 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN1ALYB1' )
      OutInd  (I)       = Spn1ALyb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 1 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN1ALZB1' )
      OutInd  (I)       = Spn1ALzb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 1 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN2ALXB1' )
      OutInd  (I)       = Spn2ALxb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN2ALYB1' )
      OutInd  (I)       = Spn2ALyb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN2ALZB1' )
      OutInd  (I)       = Spn2ALzb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN3ALXB1' )
      OutInd  (I)       = Spn3ALxb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 3 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN3ALYB1' )
      OutInd  (I)       = Spn3ALyb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 3 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN3ALZB1' )
      OutInd  (I)       = Spn3ALzb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 3 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN4ALXB1' )
      OutInd  (I)       = Spn4ALxb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 4 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN4ALYB1' )
      OutInd  (I)       = Spn4ALyb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 4 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN4ALZB1' )
      OutInd  (I)       = Spn4ALzb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 4 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN5ALXB1' )
      OutInd  (I)       = Spn5ALxb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 5 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN5ALYB1' )
      OutInd  (I)       = Spn5ALyb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 5 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN5ALZB1' )
      OutInd  (I)       = Spn5ALzb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 5 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:
   CASE ( 'SPN6ALXB1' )
      OutInd  (I)       = Spn6ALxb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6ALYB1' )
      OutInd  (I)       = Spn6ALyb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6ALZB1' )
      OutInd  (I)       = Spn6ALzb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7ALXB1' )
      OutInd  (I)       = Spn7ALxb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7ALYB1' )
      OutInd  (I)       = Spn7ALyb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7ALZB1' )
      OutInd  (I)       = Spn7ALzb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8ALXB1' )
      OutInd  (I)       = Spn8ALxb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8ALYB1' )
      OutInd  (I)       = Spn8ALyb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8ALZB1' )
      OutInd  (I)       = Spn8ALzb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9ALXB1' )
      OutInd  (I)       = Spn9ALxb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9ALYB1' )
      OutInd  (I)       = Spn9ALyb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9ALZB1' )
      OutInd  (I)       = Spn9ALzb1
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! Blade 2 Tip Motions:

   CASE ( 'TIPDXC2', 'OOPDEFL2' )
      OutInd  (I)       = TipDxc2
      OutParam(I)%Units = '(m)'
   CASE ( 'TIPDYC2', 'IPDEFL2' )
      OutInd  (I)       = TipDyc2
      OutParam(I)%Units = '(m)'
   CASE ( 'TIPDZC2', 'TIPDZB2' )
      OutInd  (I)       = TipDzc2
      OutParam(I)%Units = '(m)'
   CASE ( 'TIPDXB2' )
      OutInd  (I)       = TipDxb2
      OutParam(I)%Units = '(m)'
   CASE ( 'TIPDYB2' )
      OutInd  (I)       = TipDyb2
      OutParam(I)%Units = '(m)'
   CASE ( 'TIPALXB2' )
      OutInd  (I)       = TipALxb2
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'TIPALYB2' )
      OutInd  (I)       = TipALyb2
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'TIPALZB2' )
      OutInd  (I)       = TipALzb2
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'TIPRDXB2', 'ROLLDEFL2' )
      OutInd  (I)       = TipRDxb2
      OutParam(I)%Units = '(deg)'
   CASE ( 'TIPRDYB2', 'PTCHDEFL2' )
      OutInd  (I)       = TipRDyb2
      OutParam(I)%Units = '(deg)'
   CASE ( 'TIPRDZC2', 'TIPRDZB2', 'TWSTDEFL2' )
      OutInd  (I)       = TipRDzc2
      OutParam(I)%Units = '(deg)'
   CASE ( 'TIPCLRNC2', 'TWRCLRNC2', 'TIP2TWR2' )
      OutInd  (I)       = TipClrnc2
      OutParam(I)%Units = '(m)'

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Add blade strain gage output parameters for the local loads and motions of
!jmj   blades 2 and 3:
   ! Blade 2 Local Span Motions:

   CASE ( 'SPN1ALXB2' )
      OutInd  (I)       = Spn1ALxb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 1 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN1ALYB2' )
      OutInd  (I)       = Spn1ALyb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 1 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN1ALZB2' )
      OutInd  (I)       = Spn1ALzb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 1 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN2ALXB2' )
      OutInd  (I)       = Spn2ALxb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 2 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN2ALYB2' )
      OutInd  (I)       = Spn2ALyb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 2 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN2ALZB2' )
      OutInd  (I)       = Spn2ALzb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 2 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN3ALXB2' )
      OutInd  (I)       = Spn3ALxb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 3 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN3ALYB2' )
      OutInd  (I)       = Spn3ALyb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 3 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN3ALZB2' )
      OutInd  (I)       = Spn3ALzb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 3 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN4ALXB2' )
      OutInd  (I)       = Spn4ALxb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 4 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN4ALYB2' )
      OutInd  (I)       = Spn4ALyb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 4 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN4ALZB2' )
      OutInd  (I)       = Spn4ALzb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 4 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN5ALXB2' )
      OutInd  (I)       = Spn5ALxb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 5 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN5ALYB2' )
      OutInd  (I)       = Spn5ALyb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 5 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN5ALZB2' )
      OutInd  (I)       = Spn5ALzb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 5 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6ALXB2' )
      OutInd  (I)       = Spn6ALxb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6ALYB2' )
      OutInd  (I)       = Spn6ALyb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6ALZB2' )
      OutInd  (I)       = Spn6ALzb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7ALXB2' )
      OutInd  (I)       = Spn7ALxb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7ALYB2' )
      OutInd  (I)       = Spn7ALyb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7ALZB2' )
      OutInd  (I)       = Spn7ALzb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8ALXB2' )
      OutInd  (I)       = Spn8ALxb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8ALYB2' )
      OutInd  (I)       = Spn8ALyb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8ALZB2' )
      OutInd  (I)       = Spn8ALzb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9ALXB2' )
      OutInd  (I)       = Spn9ALxb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9ALYB2' )
      OutInd  (I)       = Spn9ALyb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9ALZB2' )
      OutInd  (I)       = Spn9ALzb2
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NBlGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF


!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

   ! Blade 3 Tip Motions:

   CASE ( 'TIPDXC3', 'OOPDEFL3' )
      OutInd  (I)       = TipDxc3
      OutParam(I)%Units = '(m)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TIPDYC3', 'IPDEFL3' )
      OutInd  (I)       = TipDyc3
      OutParam(I)%Units = '(m)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TIPDZC3', 'TIPDZB3' )
      OutInd  (I)       = TipDzc3
      OutParam(I)%Units = '(m)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TIPDXB3' )
      OutInd  (I)       = TipDxb3
      OutParam(I)%Units = '(m)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TIPDYB3' )
      OutInd  (I)       = TipDyb3
      OutParam(I)%Units = '(m)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TIPALXB3' )
      OutInd  (I)       = TipALxb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
     ENDIF
   CASE ( 'TIPALYB3' )
      OutInd  (I)       = TipALyb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
     ENDIF
   CASE ( 'TIPALZB3' )
      OutInd  (I)       = TipALzb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
     ENDIF
   CASE ( 'TIPRDXB3', 'ROLLDEFL3' )
      OutInd  (I)       = TipRDxb3
      OutParam(I)%Units = '(deg)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TIPRDYB3', 'PTCHDEFL3' )
      OutInd  (I)       = TipRDyb3
      OutParam(I)%Units = '(deg)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TIPRDZC3', 'TIPRDZB3', 'TWSTDEFL3' )
      OutInd  (I)       = TipRDzc3
      OutParam(I)%Units = '(deg)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TIPCLRNC3', 'TWRCLRNC3', 'TIP2TWR3' )
      OutInd  (I)       = TipClrnc3
      OutParam(I)%Units = '(m)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Add blade strain gage output parameters for the local loads and motions of
!jmj   blades 2 and 3:
   ! Blade 3 Local Span Motions:

   CASE ( 'SPN1ALXB3' )
      OutInd  (I)       = Spn1ALxb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN1ALYB3' )
      OutInd  (I)       = Spn1ALyb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN1ALZB3' )
      OutInd  (I)       = Spn1ALzb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN2ALXB3' )
      OutInd  (I)       = Spn2ALxb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 2 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN2ALYB3' )
      OutInd  (I)       = Spn2ALyb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 2 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN2ALZB3' )
      OutInd  (I)       = Spn2ALzb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 2 ) )  THEN
         OutParam(I)%Name  = 'INVALID'
         OutParam(I)%Units = 'CHANNEL'
      ENDIF
   CASE ( 'SPN3ALXB3' )
      OutInd  (I)       = Spn3ALxb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN3ALYB3' )
      OutInd  (I)       = Spn3ALyb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN3ALZB3' )
      OutInd  (I)       = Spn3ALzb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN4ALXB3' )
      OutInd  (I)       = Spn4ALxb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN4ALYB3' )
      OutInd  (I)       = Spn4ALyb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN4ALZB3' )
      OutInd  (I)       = Spn4ALzb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN5ALXB3' )
      OutInd  (I)       = Spn5ALxb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 5 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN5ALYB3' )
      OutInd  (I)       = Spn5ALyb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 5 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN5ALZB3' )
      OutInd  (I)       = Spn5ALzb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 5 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6ALXB3' )
      OutInd  (I)       = Spn6ALxb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 6 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6ALYB3' )
      OutInd  (I)       = Spn6ALyb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 6 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6ALZB3' )
      OutInd  (I)       = Spn6ALzb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 6 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7ALXB3' )
      OutInd  (I)       = Spn7ALxb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 7 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7ALYB3' )
      OutInd  (I)       = Spn7ALyb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 7 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7ALZB3' )
      OutInd  (I)       = Spn7ALzb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 7 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8ALXB3' )
      OutInd  (I)       = Spn8ALxb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 8 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8ALYB3' )
      OutInd  (I)       = Spn8ALyb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 8 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8ALZB3' )
      OutInd  (I)       = Spn8ALzb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 8 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9ALXB3' )
      OutInd  (I)       = Spn9ALxb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 9 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9ALYB3' )
      OutInd  (I)       = Spn9ALyb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 9 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9ALZB3' )
      OutInd  (I)       = Spn9ALzb3
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 9 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF


!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
   ! Blade Pitch Motions:

   CASE ( 'PTCHPMZC1', 'PTCHPMZB1', 'BLDPITCH1', 'BLPITCH1' )
      OutInd  (I)       = PtchPMzc1
      OutParam(I)%Units = '(deg)'
   CASE ( 'PTCHPMZC2', 'PTCHPMZB2', 'BLDPITCH2', 'BLPITCH2' )
      OutInd  (I)       = PtchPMzc2
      OutParam(I)%Units = '(deg)'
   CASE ( 'PTCHPMZC3', 'PTCHPMZB3', 'BLDPITCH3', 'BLPITCH3' )
      OutInd  (I)       = PtchPMzc3
      OutParam(I)%Units = '(deg)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF


   ! Teeter Motions:

   CASE ( 'TEETPYA', 'ROTTEETP', 'TEETDEFL' )
      OutInd  (I)       = TeetPya
      OutParam(I)%Units = '(deg)'
      IF ( NumBl == 3 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TEETVYA', 'ROTTEETV' )
      OutInd  (I)       = TeetVya
      OutParam(I)%Units = '(deg/sec)'
      IF ( NumBl == 3 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TEETAYA', 'ROTTEETA' )
      OutInd  (I)       = TeetAya
      OutParam(I)%Units = '(deg/sec2)'
      IF ( NumBl == 3 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF


   ! Shaft Motions:

   CASE ( 'LSSTIPPXA', 'LSSTIPPXS', 'LSSTIPP', 'AZIMUTH' )
      OutInd  (I)       = LSSTipPxa
      OutParam(I)%Units = '(deg)'
   CASE ( 'LSSTIPVXA', 'LSSTIPVXS', 'LSSTIPV', 'ROTSPEED' )
      OutInd  (I)       = LSSTipVxa
      OutParam(I)%Units = '(rpm)'
   CASE ( 'LSSTIPAXA', 'LSSTIPAXS', 'LSSTIPA', 'ROTACCEL' )
      OutInd  (I)       = LSSTipAxa
      OutParam(I)%Units = '(deg/sec2)'
   CASE ( 'LSSGAGPXA', 'LSSGAGPXS', 'LSSGAGP' )
      OutInd  (I)       = LSSGagPxa
      OutParam(I)%Units = '(deg)'
   CASE ( 'LSSGAGVXA', 'LSSGAGVXS', 'LSSGAGV' )
      OutInd  (I)       = LSSGagVxa
      OutParam(I)%Units = '(rpm)'
   CASE ( 'LSSGAGAXA', 'LSSGAGAXS', 'LSSGAGA' )
      OutInd  (I)       = LSSGagAxa
      OutParam(I)%Units = '(deg/sec2)'
   CASE ( 'HSSHFTV', 'GENSPEED' )
      OutInd  (I)       = HSShftV
      OutParam(I)%Units = '(rpm)'
   CASE ( 'HSSHFTA', 'GENACCEL' )
      OutInd  (I)       = HSShftA
      OutParam(I)%Units = '(deg/sec2)'
   CASE ( 'TIPSPDRAT', 'TSR' )
      OutInd  (I)       = TipSpdRat
      OutParam(I)%Units = '(-)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF


   ! Nacelle IMU Motions:

   CASE ( 'NCIMUTVXS' )
      OutInd  (I)       = NcIMUTVxs
      OutParam(I)%Units = '(m/sec)'
   CASE ( 'NCIMUTVYS' )
      OutInd  (I)       = NcIMUTVys
      OutParam(I)%Units = '(m/sec)'
   CASE ( 'NCIMUTVZS' )
      OutInd  (I)       = NcIMUTVzs
      OutParam(I)%Units = '(m/sec)'
   CASE ( 'NCIMUTAXS' )
      OutInd  (I)       = NcIMUTAxs
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'NCIMUTAYS' )
      OutInd  (I)       = NcIMUTAys
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'NCIMUTAZS' )
      OutInd  (I)       = NcIMUTAzs
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'NCIMURVXS' )
      OutInd  (I)       = NcIMURVxs
      OutParam(I)%Units = '(deg/sec)'
   CASE ( 'NCIMURVYS' )
      OutInd  (I)       = NcIMURVys
      OutParam(I)%Units = '(deg/sec)'
   CASE ( 'NCIMURVZS' )
      OutInd  (I)       = NcIMURVzs
      OutParam(I)%Units = '(deg/sec)'
   CASE ( 'NCIMURAXS' )
      OutInd  (I)       = NcIMURAxs
      OutParam(I)%Units = '(deg/sec2)'
   CASE ( 'NCIMURAYS' )
      OutInd  (I)       = NcIMURAys
      OutParam(I)%Units = '(deg/sec2)'
   CASE ( 'NCIMURAZS' )
      OutInd  (I)       = NcIMURAzs
      OutParam(I)%Units = '(deg/sec2)'


   ! Rotor-Furl Motions:

   CASE ( 'ROTFURLP', 'ROTFURL' )
      OutInd  (I)       = RotFurlP
      OutParam(I)%Units = '(deg)'
   CASE ( 'ROTFURLV' )
      OutInd  (I)       = RotFurlV
      OutParam(I)%Units = '(deg/sec)'
   CASE ( 'ROTFURLA' )
      OutInd  (I)       = RotFurlA
      OutParam(I)%Units = '(deg/sec2)'


   ! Yaw Motions:

   CASE ( 'YAWPZN', 'YAWPZP', 'NACYAWP', 'NACYAW', 'YAWPOS' )
      OutInd  (I)       = YawPzn
      OutParam(I)%Units = '(deg)'
   CASE ( 'YAWVZN', 'YAWVZP', 'NACYAWV', 'YAWRATE' )
      OutInd  (I)       = YawVzn
      OutParam(I)%Units = '(deg/sec)'
   CASE ( 'YAWAZN', 'YAWAZP', 'NACYAWA', 'YAWACCEL' )
      OutInd  (I)       = YawAzn
      OutParam(I)%Units = '(deg/sec2)'
   CASE ( 'NACYAWERR' )
      OutInd  (I)       = NacYawErr
      OutParam(I)%Units = '(deg)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF


   ! Tower-Top / Yaw Bearing Motions

   CASE ( 'YAWBRTDXP' )
      OutInd  (I)       = YawBrTDxp
      OutParam(I)%Units = '(m)'
   CASE ( 'YAWBRTDYP' )
      OutInd  (I)       = YawBrTDyp
      OutParam(I)%Units = '(m)'
   CASE ( 'YAWBRTDZP' )
      OutInd  (I)       = YawBrTDzp
      OutParam(I)%Units = '(m)'
   CASE ( 'YAWBRTDXT', 'TTDSPFA' )
      OutInd  (I)       = YawBrTDxt
      OutParam(I)%Units = '(m)'
   CASE ( 'YAWBRTDYT', 'TTDSPSS' )
      OutInd  (I)       = YawBrTDyt
      OutParam(I)%Units = '(m)'
   CASE ( 'YAWBRTDZT', 'TTDSPAX' )
      OutInd  (I)       = YawBrTDzt
      OutParam(I)%Units = '(m)'
   CASE ( 'YAWBRTAXP' )
      OutInd  (I)       = YawBrTAxp
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'YAWBRTAYP' )
      OutInd  (I)       = YawBrTAyp
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'YAWBRTAZP' )
      OutInd  (I)       = YawBrTAzp
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'YAWBRRDXT', 'TTDSPROLL' )
      OutInd  (I)       = YawBrRDxt
      OutParam(I)%Units = '(deg)'
   CASE ( 'YAWBRRDYT', 'TTDSPPTCH' )
      OutInd  (I)       = YawBrRDyt
      OutParam(I)%Units = '(deg)'
   CASE ( 'YAWBRRDZT', 'TTDSPTWST' )
      OutInd  (I)       = YawBrRDzt
      OutParam(I)%Units = '(deg)'
   CASE ( 'YAWBRRVXP' )
      OutInd  (I)       = YawBrRVxp
      OutParam(I)%Units = '(deg/sec)'
   CASE ( 'YAWBRRVYP' )
      OutInd  (I)       = YawBrRVyp
      OutParam(I)%Units = '(deg/sec)'
   CASE ( 'YAWBRRVZP' )
      OutInd  (I)       = YawBrRVzp
      OutParam(I)%Units = '(deg/sec)'
   CASE ( 'YAWBRRAXP' )
      OutInd  (I)       = YawBrRAxp
      OutParam(I)%Units = '(deg/sec2)'
   CASE ( 'YAWBRRAYP' )
      OutInd  (I)       = YawBrRAyp
      OutParam(I)%Units = '(deg/sec2)'
   CASE ( 'YAWBRRAZP' )
      OutInd  (I)       = YawBrRAzp
      OutParam(I)%Units = '(deg/sec2)'


   ! Tail-Furl Motions:

   CASE ( 'TAILFURLP', 'TAILFURL' )
      OutInd  (I)       = TailFurlP
      OutParam(I)%Units = '(deg)'
   CASE ( 'TAILFURLV' )
      OutInd  (I)       = TailFurlV
      OutParam(I)%Units = '(deg/sec)'
   CASE ( 'TAILFURLA' )
      OutInd  (I)       = TailFurlA
      OutParam(I)%Units = '(deg/sec2)'


   ! Local Tower Motions:

   CASE ( 'TWHT1ALXT' )
      OutInd  (I)       = TwHt1ALxt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 1 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT1ALYT' )
      OutInd  (I)       = TwHt1ALyt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 1 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT1ALZT' )
      OutInd  (I)       = TwHt1ALzt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 1 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT2ALXT' )
      OutInd  (I)       = TwHt2ALxt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT2ALYT' )
      OutInd  (I)       = TwHt2ALyt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT2ALZT' )
      OutInd  (I)       = TwHt2ALzt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT3ALXT' )
      OutInd  (I)       = TwHt3ALxt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 3 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT3ALYT' )
      OutInd  (I)       = TwHt3ALyt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 3 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT3ALZT' )
      OutInd  (I)       = TwHt3ALzt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 3 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT4ALXT' )
      OutInd  (I)       = TwHt4ALxt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 4 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT4ALYT' )
      OutInd  (I)       = TwHt4ALyt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 4 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT4ALZT' )
      OutInd  (I)       = TwHt4ALzt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 4 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT5ALXT' )
      OutInd  (I)       = TwHt5ALxt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 5 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT5ALYT' )
      OutInd  (I)       = TwHt5ALyt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 5 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT5ALZT' )
      OutInd  (I)       = TwHt5ALzt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 5 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:
   CASE ( 'TWHT6ALXT' )
      OutInd  (I)       = TwHt6ALxt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT6ALYT' )
      OutInd  (I)       = TwHt6ALyt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT6ALZT' )
      OutInd  (I)       = TwHt6ALzt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT7ALXT' )
      OutInd  (I)       = TwHt7ALxt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT7ALYT' )
      OutInd  (I)       = TwHt7ALyt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT7ALZT' )
      OutInd  (I)       = TwHt7ALzt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT8ALXT' )
      OutInd  (I)       = TwHt8ALxt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT8ALYT' )
      OutInd  (I)       = TwHt8ALyt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT8ALZT' )
      OutInd  (I)       = TwHt8ALzt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT9ALXT' )
      OutInd  (I)       = TwHt9ALxt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT9ALYT' )
      OutInd  (I)       = TwHt9ALyt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT9ALZT' )
      OutInd  (I)       = TwHt9ALzt
      OutParam(I)%Units = '(m/sec^2)'
      IF ( NTwGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! Platform Motions:

   CASE ( 'PTFMTDXT' )
      OutInd  (I)       = PtfmTDxt
      OutParam(I)%Units = '(m)'
   CASE ( 'PTFMTDYT' )
      OutInd  (I)       = PtfmTDyt
      OutParam(I)%Units = '(m)'
   CASE ( 'PTFMTDZT' )
      OutInd  (I)       = PtfmTDzt
      OutParam(I)%Units = '(m)'
   CASE ( 'PTFMTDXI', 'PTFMSURGE' )
      OutInd  (I)       = PtfmTDxi
      OutParam(I)%Units = '(m)'
   CASE ( 'PTFMTDYI', 'PTFMSWAY' )
      OutInd  (I)       = PtfmTDyi
      OutParam(I)%Units = '(m)'
   CASE ( 'PTFMTDZI', 'PTFMHEAVE' )
      OutInd  (I)       = PtfmTDzi
      OutParam(I)%Units = '(m)'
   CASE ( 'PTFMTVXT' )
      OutInd  (I)       = PtfmTVxt
      OutParam(I)%Units = '(m/sec)'
   CASE ( 'PTFMTVYT' )
      OutInd  (I)       = PtfmTVyt
      OutParam(I)%Units = '(m/sec)'
   CASE ( 'PTFMTVZT' )
      OutInd  (I)       = PtfmTVzt
      OutParam(I)%Units = '(m/sec)'
   CASE ( 'PTFMTVXI' )
      OutInd  (I)       = PtfmTVxi
      OutParam(I)%Units = '(m/sec)'
   CASE ( 'PTFMTVYI' )
      OutInd  (I)       = PtfmTVyi
      OutParam(I)%Units = '(m/sec)'
   CASE ( 'PTFMTVZI' )
      OutInd  (I)       = PtfmTVzi
      OutParam(I)%Units = '(m/sec)'
   CASE ( 'PTFMTAXT' )
      OutInd  (I)       = PtfmTAxt
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'PTFMTAYT' )
      OutInd  (I)       = PtfmTAyt
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'PTFMTAZT' )
      OutInd  (I)       = PtfmTAzt
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'PTFMTAXI' )
      OutInd  (I)       = PtfmTAxi
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'PTFMTAYI' )
      OutInd  (I)       = PtfmTAyi
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'PTFMTAZI' )
      OutInd  (I)       = PtfmTAzi
      OutParam(I)%Units = '(m/sec^2)'
   CASE ( 'PTFMRDXI', 'PTFMROLL' )
      OutInd  (I)       = PtfmRDxi
      OutParam(I)%Units = '(deg)'
   CASE ( 'PTFMRDYI', 'PTFMPITCH' )
      OutInd  (I)       = PtfmRDyi
      OutParam(I)%Units = '(deg)'
   CASE ( 'PTFMRDZI', 'PTFMYAW' )
      OutInd  (I)       = PtfmRDzi
      OutParam(I)%Units = '(deg)'
   CASE ( 'PTFMRVXT' )
      OutInd  (I)       = PtfmRVxt
      OutParam(I)%Units = '(deg/sec)'
   CASE ( 'PTFMRVYT' )
      OutInd  (I)       = PtfmRVyt
      OutParam(I)%Units = '(deg/sec)'
   CASE ( 'PTFMRVZT' )
      OutInd  (I)       = PtfmRVzt
      OutParam(I)%Units = '(deg/sec)'
   CASE ( 'PTFMRVXI' )
      OutInd  (I)       = PtfmRVxi
      OutParam(I)%Units = '(deg/sec)'
   CASE ( 'PTFMRVYI' )
      OutInd  (I)       = PtfmRVyi
      OutParam(I)%Units = '(deg/sec)'
   CASE ( 'PTFMRVZI' )
      OutInd  (I)       = PtfmRVzi
      OutParam(I)%Units = '(deg/sec)'
   CASE ( 'PTFMRAXT' )
      OutInd  (I)       = PtfmRAxt
      OutParam(I)%Units = '(deg/sec2)'
   CASE ( 'PTFMRAYT' )
      OutInd  (I)       = PtfmRAyt
      OutParam(I)%Units = '(deg/sec2)'
   CASE ( 'PTFMRAZT' )
      OutInd  (I)       = PtfmRAzt
      OutParam(I)%Units = '(deg/sec2)'
   CASE ( 'PTFMRAXI' )
      OutInd  (I)       = PtfmRAxi
      OutParam(I)%Units = '(deg/sec2)'
   CASE ( 'PTFMRAYI' )
      OutInd  (I)       = PtfmRAyi
      OutParam(I)%Units = '(deg/sec2)'
   CASE ( 'PTFMRAZI' )
      OutInd  (I)       = PtfmRAzi
      OutParam(I)%Units = '(deg/sec2)'



   ! Blade 1 Root Loads

   CASE ( 'ROOTFXC1' )
      OutInd  (I)       = RootFxc1
      OutParam(I)%Units = '(kN)'
   CASE ( 'ROOTFYC1' )
      OutInd  (I)       = RootFyc1
      OutParam(I)%Units = '(kN)'
   CASE ( 'ROOTFZC1', 'ROOTFZB1' )
      OutInd  (I)       = RootFzc1
      OutParam(I)%Units = '(kN)'
   CASE ( 'ROOTFXB1' )
      OutInd  (I)       = RootFxb1
      OutParam(I)%Units = '(kN)'
   CASE ( 'ROOTFYB1' )
      OutInd  (I)       = RootFyb1
      OutParam(I)%Units = '(kN)'
   CASE ( 'ROOTMXC1', 'ROOTMIP1' )
      OutInd  (I)       = RootMxc1
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'ROOTMYC1', 'ROOTMOOP1' )
      OutInd  (I)       = RootMyc1
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'ROOTMZC1', 'ROOTMZB1' )
      OutInd  (I)       = RootMzc1
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'ROOTMXB1', 'ROOTMEDG1' )
      OutInd  (I)       = RootMxb1
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'ROOTMYB1', 'ROOTMFLP1' )
      OutInd  (I)       = RootMyb1
      OutParam(I)%Units = '(kN·m)'


   ! Blade 1 Local Span Loads:

   CASE ( 'SPN1MLXB1' )
      OutInd  (I)       = Spn1MLxb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 1 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN1MLYB1' )
      OutInd  (I)       = Spn1MLyb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 1 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN1MLZB1' )
      OutInd  (I)       = Spn1MLzb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 1 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN2MLXB1' )
      OutInd  (I)       = Spn2MLxb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN2MLYB1' )
      OutInd  (I)       = Spn2MLyb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN2MLZB1' )
      OutInd  (I)       = Spn2MLzb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN3MLXB1' )
      OutInd  (I)       = Spn3MLxb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 3 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN3MLYB1' )
      OutInd  (I)       = Spn3MLyb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 3 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN3MLZB1' )
      OutInd  (I)       = Spn3MLzb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 3 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN4MLXB1' )
      OutInd  (I)       = Spn4MLxb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 4 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN4MLYB1' )
      OutInd  (I)       = Spn4MLyb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 4 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN4MLZB1' )
      OutInd  (I)       = Spn4MLzb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 4 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN5MLXB1' )
      OutInd  (I)       = Spn5MLxb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 5 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN5MLYB1' )
      OutInd  (I)       = Spn5MLyb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 5 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'SPN5MLZB1' )
      OutInd  (I)       = Spn5MLzb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 5 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:
   CASE ( 'SPN6MLXB1' )
      OutInd  (I)       = Spn6MLxb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6MLYB1' )
      OutInd  (I)       = Spn6MLyb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6MLZB1' )
      OutInd  (I)       = Spn6MLzb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7MLXB1' )
      OutInd  (I)       = Spn7MLxb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7MLYB1' )
      OutInd  (I)       = Spn7MLyb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7MLZB1' )
      OutInd  (I)       = Spn7MLzb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8MLXB1' )
      OutInd  (I)       = Spn8MLxb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8MLYB1' )
      OutInd  (I)       = Spn8MLyb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8MLZB1' )
      OutInd  (I)       = Spn8MLzb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9MLXB1' )
      OutInd  (I)       = Spn9MLxb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9MLYB1' )
      OutInd  (I)       = Spn9MLyb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9MLZB1' )
      OutInd  (I)       = Spn9MLzb1
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! Blade 2 Root Loads

   CASE ( 'ROOTFXC2' )
      OutInd  (I)       = RootFxc2
      OutParam(I)%Units = '(kN)'
   CASE ( 'ROOTFYC2' )
      OutInd  (I)       = RootFyc2
      OutParam(I)%Units = '(kN)'
   CASE ( 'ROOTFZC2', 'ROOTFZB2' )
      OutInd  (I)       = RootFzc2
      OutParam(I)%Units = '(kN)'
   CASE ( 'ROOTFXB2' )
      OutInd  (I)       = RootFxb2
      OutParam(I)%Units = '(kN)'
   CASE ( 'ROOTFYB2' )
      OutInd  (I)       = RootFyb2
      OutParam(I)%Units = '(kN)'
   CASE ( 'ROOTMXC2', 'ROOTMIP2' )
      OutInd  (I)       = RootMxc2
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'ROOTMYC2', 'ROOTMOOP2' )
      OutInd  (I)       = RootMyc2
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'ROOTMZC2', 'ROOTMZB2' )
      OutInd  (I)       = RootMzc2
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'ROOTMXB2', 'ROOTMEDG2' )
      OutInd  (I)       = RootMxb2
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'ROOTMYB2', 'ROOTMFLP2' )
      OutInd  (I)       = RootMyb2
      OutParam(I)%Units = '(kN·m)'

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Add blade strain gage output parameters for the local loads and motions of
!jmj   blades 2 and 3:
   ! Blade 2 Local Span Loads:

   CASE ( 'SPN1MLXB2' )
      OutInd  (I)       = Spn1MLxb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 1 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN1MLYB2' )
      OutInd  (I)       = Spn1MLyb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 1 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN1MLZB2' )
      OutInd  (I)       = Spn1MLzb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 1 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN2MLXB2' )
      OutInd  (I)       = Spn2MLxb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 2 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN2MLYB2' )
      OutInd  (I)       = Spn2MLyb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 2 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN2MLZB2' )
      OutInd  (I)       = Spn2MLzb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 2 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN3MLXB2' )
      OutInd  (I)       = Spn3MLxb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 3 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN3MLYB2' )
      OutInd  (I)       = Spn3MLyb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 3 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN3MLZB2' )
      OutInd  (I)       = Spn3MLzb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 3 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN4MLXB2' )
      OutInd  (I)       = Spn4MLxb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 4 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN4MLYB2' )
      OutInd  (I)       = Spn4MLyb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 4 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN4MLZB2' )
      OutInd  (I)       = Spn4MLzb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 4 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN5MLXB2' )
      OutInd  (I)       = Spn5MLxb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 5 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN5MLYB2' )
      OutInd  (I)       = Spn5MLyb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 5 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN5MLZB2' )
      OutInd  (I)       = Spn5MLzb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 5 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6MLXB2' )
      OutInd  (I)       = Spn6MLxb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6MLYB2' )
      OutInd  (I)       = Spn6MLyb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6MLZB2' )
      OutInd  (I)       = Spn6MLzb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7MLXB2' )
      OutInd  (I)       = Spn7MLxb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7MLYB2' )
      OutInd  (I)       = Spn7MLyb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7MLZB2' )
      OutInd  (I)       = Spn7MLzb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8MLXB2' )
      OutInd  (I)       = Spn8MLxb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8MLYB2' )
      OutInd  (I)       = Spn8MLyb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8MLZB2' )
      OutInd  (I)       = Spn8MLzb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9MLXB2' )
      OutInd  (I)       = Spn9MLxb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9MLYB2' )
      OutInd  (I)       = Spn9MLyb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9MLZB2' )
      OutInd  (I)       = Spn9MLzb2
      OutParam(I)%Units = '(kN·m)'
      IF ( NBlGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF


!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
   ! Blade 3 Root Loads

   CASE ( 'ROOTFXC3' )
      OutInd  (I)       = RootFxc3
      OutParam(I)%Units = '(kN)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'ROOTFYC3' )
      OutInd  (I)       = RootFyc3
      OutParam(I)%Units = '(kN)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'ROOTFZC3', 'ROOTFZB3' )
      OutInd  (I)       = RootFzc3
      OutParam(I)%Units = '(kN)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'ROOTFXB3' )
      OutInd  (I)       = RootFxb3
      OutParam(I)%Units = '(kN)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'ROOTFYB3' )
      OutInd  (I)       = RootFyb3
      OutParam(I)%Units = '(kN)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'ROOTMXC3', 'ROOTMIP3' )
      OutInd  (I)       = RootMxc3
      OutParam(I)%Units = '(kN·m)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'ROOTMYC3', 'ROOTMOOP3' )
      OutInd  (I)       = RootMyc3
      OutParam(I)%Units = '(kN·m)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'ROOTMZC3', 'ROOTMZB3' )
      OutInd  (I)       = RootMzc3
      OutParam(I)%Units = '(kN·m)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'ROOTMXB3', 'ROOTMEDG3' )
      OutInd  (I)       = RootMxb3
      OutParam(I)%Units = '(kN·m)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'ROOTMYB3', 'ROOTMFLP3' )
      OutInd  (I)       = RootMyb3
      OutParam(I)%Units = '(kN·m)'
      IF ( NumBl == 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Add blade strain gage output parameters for the local loads and motions of
!jmj   blades 2 and 3:
   ! Blade 3 Local Span Loads:

   CASE ( 'SPN1MLXB3' )
      OutInd  (I)       = Spn1MLxb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN1MLYB3' )
      OutInd  (I)       = Spn1MLyb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN1MLZB3' )
      OutInd  (I)       = Spn1MLzb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN2MLXB3' )
      OutInd  (I)       = Spn2MLxb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 2 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN2MLYB3' )
      OutInd  (I)       = Spn2MLyb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 2 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN2MLZB3' )
      OutInd  (I)       = Spn2MLzb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 2 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN3MLXB3' )
      OutInd  (I)       = Spn3MLxb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN3MLYB3' )
      OutInd  (I)       = Spn3MLyb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN3MLZB3' )
      OutInd  (I)       = Spn3MLzb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN4MLXB3' )
      OutInd  (I)       = Spn4MLxb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN4MLYB3' )
      OutInd  (I)       = Spn4MLyb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN4MLZB3' )
      OutInd  (I)       = Spn4MLzb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN5MLXB3' )
      OutInd  (I)       = Spn5MLxb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 5 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN5MLYB3' )
      OutInd  (I)       = Spn5MLyb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 5 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN5MLZB3' )
      OutInd  (I)       = Spn5MLzb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 5 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6MLXB3' )
      OutInd  (I)       = Spn6MLxb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 6 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6MLYB3' )
      OutInd  (I)       = Spn6MLyb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 6 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN6MLZB3' )
      OutInd  (I)       = Spn6MLzb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 6 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7MLXB3' )
      OutInd  (I)       = Spn7MLxb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 7 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7MLYB3' )
      OutInd  (I)       = Spn7MLyb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 7 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN7MLZB3' )
      OutInd  (I)       = Spn7MLzb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 7 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8MLXB3' )
      OutInd  (I)       = Spn8MLxb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 8 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8MLYB3' )
      OutInd  (I)       = Spn8MLyb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 8 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN8MLZB3' )
      OutInd  (I)       = Spn8MLzb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 8 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9MLXB3' )
      OutInd  (I)       = Spn9MLxb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 9 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9MLYB3' )
      OutInd  (I)       = Spn9MLyb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 9 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'SPN9MLZB3' )
      OutInd  (I)       = Spn9MLzb3
      OutParam(I)%Units = '(kN·m)'
      IF ( ( NumBl == 2 ) .OR. ( NBlGages < 9 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF


!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

   ! Hub and Rotor Loads:

   CASE ( 'LSSHFTFXA', 'LSSHFTFXS', 'LSSGAGFXA', 'LSSGAGFXS', 'ROTTHRUST' )
      OutInd  (I)       = LSShftFxa
      OutParam(I)%Units = '(kN)'
   CASE ( 'LSSHFTFYA', 'LSSGAGFYA' )
      OutInd  (I)       = LSShftFya
      OutParam(I)%Units = '(kN)'
   CASE ( 'LSSHFTFZA', 'LSSGAGFZA' )
      OutInd  (I)       = LSShftFza
      OutParam(I)%Units = '(kN)'
   CASE ( 'LSSHFTFYS', 'LSSGAGFYS' )
      OutInd  (I)       = LSShftFys
      OutParam(I)%Units = '(kN)'
   CASE ( 'LSSHFTFZS', 'LSSGAGFZS' )
      OutInd  (I)       = LSShftFzs
      OutParam(I)%Units = '(kN)'
   CASE ( 'LSSHFTMXA', 'LSSHFTMXS', 'LSSGAGMXA', 'LSSGAGMXS', 'ROTTORQ', 'LSSHFTTQ' )
      OutInd  (I)       = LSShftMxa
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'LSSTIPMYA' )
      OutInd  (I)       = LSSTipMya
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'LSSTIPMZA' )
      OutInd  (I)       = LSSTipMza
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'LSSTIPMYS' )
      OutInd  (I)       = LSSTipMys
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'LSSTIPMZS' )
      OutInd  (I)       = LSSTipMzs
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'CTHRSTAZM' )
      OutInd  (I)       = CThrstAzm
      OutParam(I)%Units = '(deg)'
   CASE ( 'CTHRSTRAD', 'CTHRSTARM' )
      OutInd  (I)       = CThrstRad
      OutParam(I)%Units = '(-)'
   CASE ( 'ROTPWR', 'LSSHFTPWR' )
      OutInd  (I)       = RotPwr
      OutParam(I)%Units = '(kW)'
   CASE ( 'ROTCQ', 'LSSHFTCQ' )
      OutInd  (I)       = RotCq
      OutParam(I)%Units = '(-)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'ROTCP', 'LSSHFTCP' )
      OutInd  (I)       = RotCp
      OutParam(I)%Units = '(-)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'ROTCT', 'LSSHFTCT' )
      OutInd  (I)       = RotCt
      OutParam(I)%Units = '(-)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF


   ! Shaft Strain Gage Loads:

   CASE ( 'LSSGAGMYA' )
      OutInd  (I)       = LSSGagMya
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'LSSGAGMZA' )
      OutInd  (I)       = LSSGagMza
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'LSSGAGMYS' )
      OutInd  (I)       = LSSGagMys
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'LSSGAGMZS' )
      OutInd  (I)       = LSSGagMzs
      OutParam(I)%Units = '(kN·m)'


   ! Generator and High-Speed Shaft Loads:

   CASE ( 'HSSHFTTQ' )
      OutInd  (I)       = HSShftTq
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'HSSHFTPWR' )
      OutInd  (I)       = HSShftPwr
      OutParam(I)%Units = '(kW)'
   CASE ( 'HSSHFTCQ' )
      OutInd  (I)       = HSShftCq
      OutParam(I)%Units = '(-)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'HSSHFTCP' )
      OutInd  (I)       = HSShftCp
      OutParam(I)%Units = '(-)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'HSSBRTQ' )
      OutInd  (I)       = HSSBrTq
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'GENTQ' )
      OutInd  (I)       = GenTq
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'GENPWR' )
      OutInd  (I)       = GenPwr
      OutParam(I)%Units = '(kW)'
   CASE ( 'GENCQ' )
      OutInd  (I)       = GenCq
      OutParam(I)%Units = '(-)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'GENCP' )
      OutInd  (I)       = GenCp
      OutParam(I)%Units = '(-)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF


   ! Rotor-Furl Bearing Loads:

   CASE ( 'RFRLBRM' )
      OutInd  (I)       = RFrlBrM
      OutParam(I)%Units = '(kN·m)'


   ! Tower-Top / Yaw Bearing Loads:

   CASE ( 'YAWBRFXN' )
      OutInd  (I)       = YawBrFxn
      OutParam(I)%Units = '(kN)'
   CASE ( 'YAWBRFYN' )
      OutInd  (I)       = YawBrFyn
      OutParam(I)%Units = '(kN)'
   CASE ( 'YAWBRFZN', 'YAWBRFZP' )
      OutInd  (I)       = YawBrFzn
      OutParam(I)%Units = '(kN)'
   CASE ( 'YAWBRFXP' )
      OutInd  (I)       = YawBrFxp
      OutParam(I)%Units = '(kN)'
   CASE ( 'YAWBRFYP' )
      OutInd  (I)       = YawBrFyp
      OutParam(I)%Units = '(kN)'
   CASE ( 'YAWBRMXN' )
      OutInd  (I)       = YawBrMxn
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'YAWBRMYN' )
      OutInd  (I)       = YawBrMyn
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'YAWBRMZN', 'YAWBRMZP', 'YAWMOM' )
      OutInd  (I)       = YawBrMzn
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'YAWBRMXP' )
      OutInd  (I)       = YawBrMxp
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'YAWBRMYP' )
      OutInd  (I)       = YawBrMyp
      OutParam(I)%Units = '(kN·m)'


   ! Tower Base Loads:

   CASE ( 'TWRBSFXT' )
      OutInd  (I)       = TwrBsFxt
      OutParam(I)%Units = '(kN)'
   CASE ( 'TWRBSFYT' )
      OutInd  (I)       = TwrBsFyt
      OutParam(I)%Units = '(kN)'
   CASE ( 'TWRBSFZT' )
      OutInd  (I)       = TwrBsFzt
      OutParam(I)%Units = '(kN)'
   CASE ( 'TWRBSMXT' )
      OutInd  (I)       = TwrBsMxt
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'TWRBSMYT' )
      OutInd  (I)       = TwrBsMyt
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'TWRBSMZT' )
      OutInd  (I)       = TwrBsMzt
      OutParam(I)%Units = '(kN·m)'


   ! Tail-Furl Bearing Loads:

   CASE ( 'TFRLBRM' )
      OutInd  (I)       = TFrlBrM
      OutParam(I)%Units = '(kN·m)'


   ! Local Tower Loads:

   CASE ( 'TWHT1MLXT' )
      OutInd  (I)       = TwHt1MLxt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 1 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT1MLYT' )
      OutInd  (I)       = TwHt1MLyt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 1 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT1MLZT' )
      OutInd  (I)       = TwHt1MLzt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 1 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT2MLXT' )
      OutInd  (I)       = TwHt2MLxt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT2MLYT' )
      OutInd  (I)       = TwHt2MLyt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT2MLZT' )
      OutInd  (I)       = TwHt2MLzt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 2 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT3MLXT' )
      OutInd  (I)       = TwHt3MLxt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 3 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT3MLYT' )
      OutInd  (I)       = TwHt3MLyt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 3 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT3MLZT' )
      OutInd  (I)       = TwHt3MLzt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 3 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT4MLXT' )
      OutInd  (I)       = TwHt4MLxt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 4 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT4MLYT' )
      OutInd  (I)       = TwHt4MLyt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 4 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT4MLZT' )
      OutInd  (I)       = TwHt4MLzt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 4 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT5MLXT' )
      OutInd  (I)       = TwHt5MLxt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 5 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT5MLYT' )
      OutInd  (I)       = TwHt5MLyt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 5 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TWHT5MLZT' )
      OutInd  (I)       = TwHt5MLzt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 5 )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:
   CASE ( 'TWHT6MLXT' )
      OutInd  (I)       = TwHt6MLxt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT6MLYT' )
      OutInd  (I)       = TwHt6MLyt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT6MLZT' )
      OutInd  (I)       = TwHt6MLzt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 6 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT7MLXT' )
      OutInd  (I)       = TwHt7MLxt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT7MLYT' )
      OutInd  (I)       = TwHt7MLyt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT7MLZT' )
      OutInd  (I)       = TwHt7MLzt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 7 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT8MLXT' )
      OutInd  (I)       = TwHt8MLxt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT8MLYT' )
      OutInd  (I)       = TwHt8MLyt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT8MLZT' )
      OutInd  (I)       = TwHt8MLzt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 8 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT9MLXT' )
      OutInd  (I)       = TwHt9MLxt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT9MLYT' )
      OutInd  (I)       = TwHt9MLyt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'TWHT9MLZT' )
      OutInd  (I)       = TwHt9MLzt
      OutParam(I)%Units = '(kN·m)'
      IF ( NTwGages < 9 )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

   ! Platform Loads:

   CASE ( 'PTFMFXT' )
      OutInd  (I)       = PtfmFxt
      OutParam(I)%Units = '(kN)'
   CASE ( 'PTFMFYT' )
      OutInd  (I)       = PtfmFyt
      OutParam(I)%Units = '(kN)'
   CASE ( 'PTFMFZT' )
      OutInd  (I)       = PtfmFzt
      OutParam(I)%Units = '(kN)'
   CASE ( 'PTFMFXI' )
      OutInd  (I)       = PtfmFxi
      OutParam(I)%Units = '(kN)'
   CASE ( 'PTFMFYI' )
      OutInd  (I)       = PtfmFyi
      OutParam(I)%Units = '(kN)'
   CASE ( 'PTFMFZI' )
      OutInd  (I)       = PtfmFzi
      OutParam(I)%Units = '(kN)'
   CASE ( 'PTFMMXT' )
      OutInd  (I)       = PtfmMxt
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'PTFMMYT' )
      OutInd  (I)       = PtfmMyt
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'PTFMMZT' )
      OutInd  (I)       = PtfmMzt
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'PTFMMXI' )
      OutInd  (I)       = PtfmMxi
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'PTFMMYI' )
      OutInd  (I)       = PtfmMyi
      OutParam(I)%Units = '(kN·m)'
   CASE ( 'PTFMMZI' )
      OutInd  (I)       = PtfmMzi
      OutParam(I)%Units = '(kN·m)'
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
   ! Mooring Line Loads:

   CASE ( 'FAIR1TEN' )
      OutInd  (I)       = Fair1Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR1ANG' )
      OutInd  (I)       = Fair1Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH1TEN' )
      OutInd  (I)       = Anch1Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH1ANG' )
      OutInd  (I)       = Anch1Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR2TEN' )
      OutInd  (I)       = Fair2Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 2 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR2ANG' )
      OutInd  (I)       = Fair2Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 2 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH2TEN' )
      OutInd  (I)       = Anch2Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 2 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH2ANG' )
      OutInd  (I)       = Anch2Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 2 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR3TEN' )
      OutInd  (I)       = Fair3Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR3ANG' )
      OutInd  (I)       = Fair3Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH3TEN' )
      OutInd  (I)       = Anch3Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH3ANG' )
      OutInd  (I)       = Anch3Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR4TEN' )
      OutInd  (I)       = Fair4Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR4ANG' )
      OutInd  (I)       = Fair4Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH4TEN' )
      OutInd  (I)       = Anch4Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH4ANG' )
      OutInd  (I)       = Anch4Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR5TEN' )
      OutInd  (I)       = Fair5Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 5 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR5ANG' )
      OutInd  (I)       = Fair5Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 5 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH5TEN' )
      OutInd  (I)       = Anch5Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 5 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH5ANG' )
      OutInd  (I)       = Anch5Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 5 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR6TEN' )
      OutInd  (I)       = Fair6Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 6 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR6ANG' )
      OutInd  (I)       = Fair6Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 6 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH6TEN' )
      OutInd  (I)       = Anch6Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 6 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH6ANG' )
      OutInd  (I)       = Anch6Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 6 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR7TEN' )
      OutInd  (I)       = Fair7Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 7 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR7ANG' )
      OutInd  (I)       = Fair7Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 7 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH7TEN' )
      OutInd  (I)       = Anch7Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 7 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH7ANG' )
      OutInd  (I)       = Anch7Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 7 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR8TEN' )
      OutInd  (I)       = Fair8Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 8 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR8ANG' )
      OutInd  (I)       = Fair8Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 8 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH8TEN' )
      OutInd  (I)       = Anch8Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 8 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH8ANG' )
      OutInd  (I)       = Anch8Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 8 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR9TEN' )
      OutInd  (I)       = Fair9Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 9 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'FAIR9ANG' )
      OutInd  (I)       = Fair9Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 9 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH9TEN' )
      OutInd  (I)       = Anch9Ten
      OutParam(I)%Units = '(kN)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 9 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'ANCH9ANG' )
      OutInd  (I)       = Anch9Ang
      OutParam(I)%Units = '(deg)'
      IF ( ( .NOT. CompHydro ) .OR. ( NumLines < 9 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF


!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.



   ! Wind Motions:

   CASE ( 'WINDVXI', 'UWIND' )
      OutInd  (I)       = WindVxi
      OutParam(I)%Units = '(m/sec)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'WINDVYI', 'VWIND' )
      OutInd  (I)       = WindVyi
      OutParam(I)%Units = '(m/sec)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'WINDVZI', 'WWIND' )
      OutInd  (I)       = WindVzi
      OutParam(I)%Units = '(m/sec)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TOTWINDV' )
      OutInd  (I)       = TotWindV
      OutParam(I)%Units = '(m/sec)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'HORWINDV' )
      OutInd  (I)       = HorWindV
      OutParam(I)%Units = '(m/sec)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'HORWNDDIR' )
      OutInd  (I)       = HorWndDir
      OutParam(I)%Units = '(deg)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'VERWNDDIR' )
      OutInd  (I)       = VerWndDir
      OutParam(I)%Units = '(deg)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF


   ! Tail Fin Element Aerodynamics:

   CASE ( 'TFINALPHA' )
      OutInd  (I)       = TFinAlpha
      OutParam(I)%Units = '(deg)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TFINCLIFT' )
      OutInd  (I)       = TFinCLift
      OutParam(I)%Units = '(-)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TFINCDRAG' )
      OutInd  (I)       = TFinCDrag
      OutParam(I)%Units = '(-)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TFINDNPRS' )
      OutInd  (I)       = TFinDnPrs
      OutParam(I)%Units = '(Pa)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TFINCPFX' )
      OutInd  (I)       = TFinCPFx
      OutParam(I)%Units = '(kN)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF
   CASE ( 'TFINCPFY' )
      OutInd  (I)       = TFinCPFy
      OutParam(I)%Units = '(kN)'
      IF ( .NOT. CompAero )  THEN
!bjj start of proposed change: CRUNCH CHANNELS
!rm         OutParam(I)%Name  = 'INVALID'
!rm         OutParam(I)%Units = 'CHANNEL'
         OutParam(I)%Units = 'INVALID'
!bjj end of proposed change
      ENDIF

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for outputting the incident wave elevation at
!jmj   the platform reference point and the incident wave kinematics at up to 9
!jmj   nodes along the undeflected tower [not floating] or undisplaced platform
!jmj   [floating]:
   ! Wave Motions:

   CASE ( 'WAVEELEV' )
      OutInd  (I)       = WaveElev
      OutParam(I)%Units = '(m)'
      IF ( .NOT. CompHydro )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF


   CASE ( 'WAVE1VXI' )
      OutInd  (I)       = Wave1Vxi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE1VYI' )
      OutInd  (I)       = Wave1Vyi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE1VZI' )
      OutInd  (I)       = Wave1Vzi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE1AXI' )
      OutInd  (I)       = Wave1Axi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE1AYI' )
      OutInd  (I)       = Wave1Ayi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE1AZI' )
      OutInd  (I)       = Wave1Azi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 1 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE2VXI' )
      OutInd  (I)       = Wave2Vxi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 2 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE2VYI' )
      OutInd  (I)       = Wave2Vyi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 2 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE2VZI' )
      OutInd  (I)       = Wave2Vzi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 2 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE2AXI' )
      OutInd  (I)       = Wave2Axi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 2 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE2AYI' )
      OutInd  (I)       = Wave2Ayi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 2 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE2AZI' )
      OutInd  (I)       = Wave2Azi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 2 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE3VXI' )
      OutInd  (I)       = Wave3Vxi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE3VYI' )
      OutInd  (I)       = Wave3Vyi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE3VZI' )
      OutInd  (I)       = Wave3Vzi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE3AXI' )
      OutInd  (I)       = Wave3Axi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE3AYI' )
      OutInd  (I)       = Wave3Ayi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE3AZI' )
      OutInd  (I)       = Wave3Azi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 3 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE4VXI' )
      OutInd  (I)       = Wave4Vxi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE4VYI' )
      OutInd  (I)       = Wave4Vyi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE4VZI' )
      OutInd  (I)       = Wave4Vzi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE4AXI' )
      OutInd  (I)       = Wave4Axi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE4AYI' )
      OutInd  (I)       = Wave4Ayi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE4AZI' )
      OutInd  (I)       = Wave4Azi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 4 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE5VXI' )
      OutInd  (I)       = Wave5Vxi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 5 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE5VYI' )
      OutInd  (I)       = Wave5Vyi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 5 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE5VZI' )
      OutInd  (I)       = Wave5Vzi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 5) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE5AXI' )
      OutInd  (I)       = Wave5Axi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 5 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE5AYI' )
      OutInd  (I)       = Wave5Ayi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 5 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE5AZI' )
      OutInd  (I)       = Wave5Azi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 5 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE6VXI' )
      OutInd  (I)       = Wave6Vxi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 6 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE6VYI' )
      OutInd  (I)       = Wave6Vyi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 6 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE6VZI' )
      OutInd  (I)       = Wave6Vzi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 6) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE6AXI' )
      OutInd  (I)       = Wave6Axi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 6 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE6AYI' )
      OutInd  (I)       = Wave6Ayi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 6 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE6AZI' )
      OutInd  (I)       = Wave6Azi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 6 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE7VXI' )
      OutInd  (I)       = Wave7Vxi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 7 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE7VYI' )
      OutInd  (I)       = Wave7Vyi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 7 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE7VZI' )
      OutInd  (I)       = Wave7Vzi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 7) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE7AXI' )
      OutInd  (I)       = Wave7Axi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 7 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE7AYI' )
      OutInd  (I)       = Wave7Ayi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 7 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE7AZI' )
      OutInd  (I)       = Wave7Azi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 7 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE8VXI' )
      OutInd  (I)       = Wave8Vxi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 8 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE8VYI' )
      OutInd  (I)       = Wave8Vyi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 8 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE8VZI' )
      OutInd  (I)       = Wave8Vzi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 8) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE8AXI' )
      OutInd  (I)       = Wave8Axi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 8 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE8AYI' )
      OutInd  (I)       = Wave8Ayi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 8 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE8AZI' )
      OutInd  (I)       = Wave8Azi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 8 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE9VXI' )
      OutInd  (I)       = Wave9Vxi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 9 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE9VYI' )
      OutInd  (I)       = Wave9Vyi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 9 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE9VZI' )
      OutInd  (I)       = Wave9Vzi
      OutParam(I)%Units = '(m/sec)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 9) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE9AXI' )
      OutInd  (I)       = Wave9Axi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 9 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE9AYI' )
      OutInd  (I)       = Wave9Ayi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 9 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF
   CASE ( 'WAVE9AZI' )
      OutInd  (I)       = Wave9Azi
      OutParam(I)%Units = '(m/sec^2)'
      IF ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 9 ) )  THEN
         OutParam(I)%Units = 'INVALID'
      ENDIF



!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

   ! The selected channel does not match any of of the available output channels.

   CASE DEFAULT
      CALL ProgAbort( OutList(I)//' is not an available output channel.' )


   END SELECT

   IF ( Echo )  WRITE (UnEc,OutPFmt)  I, OutParam(I)%Name, OutParam(I)%Units

ENDDO



RETURN
END SUBROUTINE ChckOutLst
!=======================================================================
!bjj Start of proposed change vXX NWTC_Lib
!rmSUBROUTINE CheckArgs
!rm
!rm
!rm   ! This subroutine is used to check for command-line arguments.
!rm
!rm
!rmUSE                             General
!rm!bjj rm NWTC_Library: USE                             SysSubs
!rmUSE                             System
!rm
!rmIMPLICIT                        NONE
!rm
!rm
!rm   ! Local variables.
!rm
!rmINTEGER(4)                   :: IArg                                            ! The argument number.
!rmINTEGER(4)                   :: NumArg                                          ! The number of arguments on the command line.
!rm
!rmLOGICAL(1)                   :: Error                                           ! Flag indicating if there was an error getting an argument.
!rm
!rmCHARACTER(99)                :: Arg                                             ! A command-line argument.
!rm
!rm
!rm   ! User-defined functions.
!rm
!rm!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.
!rm
!rm
!rm
!rm
!rm   ! Find out how many arguments were entered on the command line.
!rm
!rmCALL Get_Arg_Num ( NumArg )
!rm
!rm
!rm   ! Parse them.
!rm
!rmIF ( NumArg .GT. 0 )  THEN
!rm
!rm   DO IArg=1,NumArg
!rm
!rm      CALL Get_Arg ( IArg , Arg , Error )
!rm
!rm      IF ( Error )  THEN
!rm         CALL ProgAbort ( ' Error getting command-line argument #'//TRIM( Int2LStr( IArg ) )//'.' )
!rm      ENDIF
!rm
!rm      IF (Arg(1:1) == SwChar )  THEN
!rm
!rm         IF ( ( Arg(2:2) .EQ. 'H' ) .OR. ( Arg(2:2) .EQ. 'h' ) .OR. ( Arg(2:2) .EQ. '?' ) )  THEN
!rm            CALL WrScr1 ( ' Syntax is:' )
!rm            CALL WrScr1 ( '    '//ProgName//' ['//SwChar//'h] [<PriFile>]' )
!rm            CALL WrScr1 ( ' where:' )
!rm            CALL WrScr1 ( '    '//SwChar//'h generates this help message.' )
!rm            CALL WrScr  ( '    <PriFile> is the name of the primary input file ['//TRIM( PriFile )//'].' )
!rm!JASON: Link NWTC_Subs.f90 and NWTC_Mods.f90 after AeroDyn is fully interfaced to these routines.  When you do this, replace all CALLs to EXIT() with CALLs to ProgExit() in order to get rid of the /stand Warnings.
!rm            CALL EXIT ( 1 )
!rm         ELSE
!rm            CALL ProgAbort ( ' Invalid command-line switch "'//SwChar//TRIM( Arg(2:) )//'".' )
!rm         ENDIF
!rm
!rm      ELSE
!rm         PriFile = Arg
!rm      ENDIF
!rm
!rm   ENDDO
!rm
!rmENDIF
!rm
!rm
!rmRETURN
!rmEND SUBROUTINE CheckArgs
!bjj End of proposed change
!=======================================================================
!bjj start of proposed change
!this should be checked in AeroDyn
!SUBROUTINE CheckRComp
!
!
!   ! This routine checks to see if RNodes(:) and DRNodes(:) are compatible w/n
!   ! a millimeter; if not, FAST aborts.
!
!
!USE                             Blades
!USE                             General
!!bjj rm NWTC_Library: USE                             Precision
!!bjj rm NWTC_Library: USE                             SysSubs
!USE                             TurbConf
!
!
!IMPLICIT                        NONE
!
!
!   ! Local variables.
!
!REAL(ReKi)                   :: DRNodesNew(BldNodes)  ! Length of variable-spaced blade elements--calculated from input RNodes(:).
!REAL(ReKi)                   :: DRSum                 ! Sum of DRs--should be close to BldFlexL
!
!INTEGER(4)                   :: I                     ! Generic index.
!
!!bjj chg: LOGICAL(1)                   :: DRMatch               ! Flag indicating whether or not DRNodes(:) and DRNodesNew(:) match.
!LOGICAL                   :: DRMatch               ! Flag indicating whether or not DRNodes(:) and DRNodesNew(:) match.
!
!CHARACTER(33)                :: DRChange              ! A string showing how to change DRNodes to get campatibility
!
!
!   ! User-defined functions.
!
!!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr              ! A function to convert an interger to a left-justified string.
!
!
!
!   ! Initialize DRMatch to .TRUE.:
!
!DRMatch = .TRUE.
!
!
!   ! Calculate DRNodesNew(:) based on inputted RNodes(:) and compare to inputted DRNodes(:):
!
!DRNodesNew(1) = 2.0*( RNodes(1) - HubRad )
!DRSum         = DRNodesNew(1)
!
!IF ( DRNodesNew(1) <= 0.0 )  THEN                              ! Check to see if RNodes(1) > HubRad; if not, ProgAbort program
!   CALL ProgAbort(' RNodes(1) must be > HubRad. ')
!ELSEIF ( ABS( DRNodesNew(1) - DRNodes(1) ) > 0.001 )  THEN     ! Check to see if the calculated DRNodes(1) is close to the inputted DRNodes(1); if not, set flag--this will cause the program to ProgAbort later.
!   DRMatch = .FALSE.
!ENDIF
!
!DO I = 2,BldNodes ! Loop through all but the innermost blade element
!
!   DRNodesNew(I) = 2.0*( RNodes(I) - RNodes(I-1) ) - DRNodesNew(I-1)
!   DRSum         = DRSum + DRNodesNew(I)
!
!   IF ( DRNodesNew(I) <= 0.0 )  THEN                           ! Check to see if it is possible to have compatible DRNodes(:) with the given, inputted RNodes(:); if not, ProgAbort program
!      CALL ProgAbort( 'RNodes('//TRIM( Int2LStr(I) )//') produces ill-conditioned DRNodes(:)' )
!   ELSEIF ( ABS( DRNodesNew(I) - DRNodes(I) ) > 0.001 )  THEN  ! Check to see if the calculated DRNodes(I) is close to the inputted DRNodes(I); if not, set flag--this will cause the program to Abort later.
!      DRMatch = .FALSE.
!   ENDIF
!
!ENDDO             ! I - all but the innermost blade element
!
!
!   ! Abort program if necessary
!
!IF ( .NOT. DRMatch )  THEN
!
!   ! Abort program since the inputted DRNodes(:) are not close to the calculated DRNodes(:)
!
!   CALL WrScr1(' Inputted DRNodes(:) are not compatible with inputted RNodes(:). ')
!   CALL WrScr (' To make them compatible, please modify DRNodes in '// TRIM( ADFile ) //' as follows:')
!   CALL WrScr1(' DRNodes (Old) --> DRNodes (New) ')
!
!   DO I = 1,BldNodes
!      WRITE( DRChange, "(' ', F13.4, ' --> ', F13.4, ' ')" ) DRNodes(I), DRNodesNew(I)
!      CALL WrScr( DRChange )
!   ENDDO !I
!
!   CALL ProgAbort(' ')
!
!
!ELSEIF ( ABS( DRSum - BldFlexL ) > 0.001 )  THEN
!
!   ! Abort program since SUM( DRNodes(:) ) /= BldFlexL
!
!   CALL ProgAbort(' HubRad + SUM( DRNodes(:) ) is not equal to TipRad. ')
!
!
!ENDIF
!
!
!   ! Inputted RNodes(:) and DRNodes(:) are compatible.
!   ! Use the more precise (calculated values) throughout the rest of the code:
!
!DRNodes = DRNodesNew
!
!
!
!RETURN
!END SUBROUTINE CheckRComp
!bjj end of proposed change
!=======================================================================
SUBROUTINE GetADAMS


   ! This routine reads in the ADAMS-specific parameters from ADAMSFile
   !   and validates the input.


USE                             ADAMSInput
USE                             General
USE                             Output
USE                             TurbConf


IMPLICIT                        NONE


   ! Local variables:

INTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.



   ! Open the ADAMS input file:

!BJJ:
CALL OpenFInpFile ( UnIn, ADAMSFile )


   ! Add a separator to the echo file if appropriate:

IF ( Echo )  WRITE (UnEc,'(//,A,/)')  'ADAMS input data from file "'//TRIM( ADAMSFile )//'":'



!  -------------- HEADER -------------------------------------------------------


   ! Skip the header.

READ (UnIn,'(//)',IOSTAT=IOS)

IF ( IOS < 0 )  THEN
   CALL PremEOF ( ADAMSFile , 'unused ADAMS-file header' )
ENDIF



!  -------------- FEATURE SWITCHES ---------------------------------------------


   ! Skip the comment line.
!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, ADAMSFile, 'feature switches', Echo  )
CALL ReadCom ( UnIn, ADAMSFile, 'feature switches'  )
!bjj End of proposed change

   ! SaveGrphcs - Save GRAPHICS output.

CALL ReadLVar ( UnIn, ADAMSFile, SaveGrphcs, 'SaveGrphcs', 'Save GRAPHICS output' )


   ! MakeLINacf - Make ADAMS/LINEAR control command file.

CALL ReadLVar ( UnIn, ADAMSFile, MakeLINacf, 'MakeLINacf', 'Make ADAMS/LINEAR control command file' )

IF ( MakeLINacf .AND. ( .NOT. SaveGrphcs ) )  &
   CALL ProgAbort ( ' SaveGrphcs must be True if MakeLINacf is True.' )



!  -------------- DAMPING PARAMETERS -------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL CALL SkipComment ( UnIn, ADAMSFile, 'damping parameters', Echo  )
CALL ReadCom ( UnIn, ADAMSFile, 'damping parameters'  )
!bjj End of proposed change


   ! CRatioTGJ - Tower torsional damping ratio.

CALL ReadRVar ( UnIn, ADAMSFile, CRatioTGJ, 'CRatioTGJ', 'Tower torsional damping ratio' )

IF ( CRatioTGJ < 0.0 )  CALL ProgAbort ( ' CRatioTGJ must not be less than zero.' )


   ! CRatioTEA - Tower extensional damping ratio.

CALL ReadRVar ( UnIn, ADAMSFile, CRatioTEA, 'CRatioTEA', 'Tower extensional damping ratio' )

IF ( CRatioTEA < 0.0 )  CALL ProgAbort ( ' CRatioTEA must not be less than zero.' )


   ! CRatioBGJ - Blade torsional damping ratio.

CALL ReadRVar ( UnIn, ADAMSFile, CRatioBGJ, 'CRatioBGJ', 'Blade torsional damping ratio' )

IF ( CRatioBGJ < 0.0 )  CALL ProgAbort ( ' CRatioBGJ must not be less than zero.' )


   ! CRatioBEA - Blade extensional damping ratio.

CALL ReadRVar ( UnIn, ADAMSFile, CRatioBEA, 'CRatioBEA', 'Blade extensional damping ratio' )

IF ( CRatioBEA < 0.0 )  CALL ProgAbort ( ' CRatioBEA must not be less than zero.' )



!  -------------- BLADE PITCH ACTUATOR PARAMETERS ------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, ADAMSFile, 'blade pitch actuator parameters', Echo  )
CALL ReadCom ( UnIn, ADAMSFile, 'blade pitch actuator parameters'  )
!bjj End of proposed change

   ! BPActrSpr - Blade pitch actuator spring constant.

CALL ReadRVar ( UnIn, ADAMSFile, BPActrSpr, 'BPActrSpr', 'Blade pitch actuator spring constant' )

IF ( BPActrSpr < 0.0 )  CALL ProgAbort ( ' BPActrSpr must not be less than zero.' )


   ! BPActrDmp - Blade pitch actuator damping constant.

CALL ReadRVar ( UnIn, ADAMSFile, BPActrDmp, 'BPActrDmp', 'Blade pitch actuator damping constant' )

IF ( BPActrDmp < 0.0 )  CALL ProgAbort ( ' BPActrSpr must not be less than zero.' )



!  -------------- GRAPHICS PARAMETERS ------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, ADAMSFile, 'GRAPHICS parameters', Echo  )
CALL ReadCom ( UnIn, ADAMSFile, 'GRAPHICS parameters'  )
!bjj End of proposed change

   ! NSides - Number of sides.

CALL ReadIVar ( UnIn, ADAMSFile, NSides, 'NSides', 'Number of sides' )

IF ( ( NSides < 0 ) .OR. ( NSides > 99999 ) )  &
   CALL ProgAbort ( ' NSides must be between 0 and 99,999 (inclusive).' )


   ! TwrBaseRad - Tower base radius.

CALL ReadRVar ( UnIn, ADAMSFile, TwrBaseRad, 'TwrBaseRad', 'Tower base radius' )

IF ( TwrBaseRad < 0.0 )  CALL ProgAbort ( ' TwrBaseRad must not be less than zero.' )


   ! TwrTopRad - Tower top radius.

CALL ReadRVar ( UnIn, ADAMSFile, TwrTopRad, 'TwrTopRad', 'Tower top radius' )

IF ( TwrTopRad < 0.0 )  CALL ProgAbort ( ' TwrTopRad must not be less than zero.' )


!bjj start of proposed change - spelling Nacelle correctly
!rm   ! NacLength - Nacalle length.
!rm
!rmCALL ReadRVar ( UnIn, ADAMSFile, NacLength, 'NacLength', 'Nacalle length' )
   ! NacLength - Nacelle length.

CALL ReadRVar ( UnIn, ADAMSFile, NacLength, 'NacLength', 'Nacelle length' )
!bjj end of proposed change - spelling Nacelle correctly

IF ( ( NacLength < 0.0 ) .OR. ( NacLength > 2.0*ABS(OverHang) ) )  &
   CALL ProgAbort ( ' NacLength must be between zero and 2*ABS(OverHang) (inclusive).' )


!bjj start of proposed change - spelling Nacelle correctly
!rm   ! NacRadBot - Bottom radius of nacalle.
!rm
!rmCALL ReadRVar ( UnIn, ADAMSFile, NacRadBot, 'NacRadBot', 'Bottom radius of nacalle' )
   ! NacRadBot - Bottom radius of nacelle.

CALL ReadRVar ( UnIn, ADAMSFile, NacRadBot, 'NacRadBot', 'Bottom radius of nacelle' )
!bjj end of proposed change - spelling Nacelle correctly

IF ( NacRadBot < 0.0 )  CALL ProgAbort ( ' NacRadBot must not be less than zero.' )


!bjj start of proposed change - spelling Nacelle correctly
!rm   ! NacRadTop - Top radius of nacalle.
!rm
!rmCALL ReadRVar ( UnIn, ADAMSFile, NacRadTop, 'NacRadTop', 'Top radius of nacalle' )
   ! NacRadTop - Top radius of nacelle.

CALL ReadRVar ( UnIn, ADAMSFile, NacRadTop, 'NacRadTop', 'Top radius of nacelle' )
!bjj end of proposed change - spelling Nacelle correctly

IF ( NacRadTop < 0.0 )  CALL ProgAbort ( ' NacRadTop must not be less than zero.' )


   ! GBoxLength - Gearbox length.

CALL ReadRVar ( UnIn, ADAMSFile, GBoxLength, 'GBoxLength', 'Gearbox length' )

IF ( GBoxLength < 0.0 )  CALL ProgAbort ( ' GBoxLength must not be less than zero.' )


   ! GenLength - Generator length.

CALL ReadRVar ( UnIn, ADAMSFile, GenLength, 'GenLength', 'Generator length' )

IF ( GenLength < 0.0 )  CALL ProgAbort ( ' GenLength must not be less than zero.' )


   ! HSSLength - High-speed shaft length.

CALL ReadRVar ( UnIn, ADAMSFile, HSSLength, 'HSSLength', 'High-speed shaft length' )

IF ( HSSLength < 0.0 )  CALL ProgAbort ( ' HSSLength must not be less than zero.' )


   ! LSSLength - Low-speed shaft length.

CALL ReadRVar ( UnIn, ADAMSFile, LSSLength, 'LSSLength', 'Low-speed shaft length' )

IF ( LSSLength < 0.0 )  CALL ProgAbort ( ' LSSLength must not be less than zero.' )


   ! GenRad - Generator radius.

CALL ReadRVar ( UnIn, ADAMSFile, GenRad, 'GenRad', 'Generator radius' )

IF ( GenRad < 0.0 )  CALL ProgAbort ( ' GenRad must not be less than zero.' )


   ! HSSRad - High-speed shaft radius.

CALL ReadRVar ( UnIn, ADAMSFile, HSSRad, 'HSSRad', 'High-speed shaft radius' )

IF ( HSSRad < 0.0 )  CALL ProgAbort ( ' HSSRad must not be less than zero.' )


   ! LSSRad - Low-speed shaft radius.

CALL ReadRVar ( UnIn, ADAMSFile, LSSRad, 'LSSRad', 'Low-speed shaft radius' )

IF ( LSSRad < 0.0 )  CALL ProgAbort ( ' LSSRad must not be less than zero.' )


   ! HubCylRad - Hub cylinder radius.

CALL ReadRVar ( UnIn, ADAMSFile, HubCylRad, 'HubCylRad', 'Hub cylinder radius' )

IF ( HubCylRad < 0.0 )  CALL ProgAbort ( ' HubCylRad must not be less than zero.' )


   ! ThkOvrChrd - Thickness over chord ratio.

CALL ReadRVar ( UnIn, ADAMSFile, ThkOvrChrd, 'ThkOvrChrd', 'Thickness over chord ratio' )

IF ( ThkOvrChrd < 0.0 )  CALL ProgAbort ( ' ThkOvrChrd must not be less than zero.' )


   ! BoomRad - Tail boom radius.

CALL ReadRVar ( UnIn, ADAMSFile, BoomRad, 'Boom', 'Tail boom radius' )

IF ( BoomRad < 0.0 )  CALL ProgAbort ( ' BoomRad must not be less than zero.' )



   ! Let's convert the sign of several variables as appropriate:

NacLength = SIGN( NacLength,  OverHang )
LSSLength = SIGN( LSSLength,  OverHang )
HSSLength = SIGN( HSSLength, -OverHang )
GenLength = SIGN( GenLength, -OverHang )



   ! Close the ADAMS file.

CLOSE ( UnIn )



RETURN
END SUBROUTINE GetADAMS
!=======================================================================
SUBROUTINE GetBlade ( K )


   ! This routine reads the Kth blade file and validates the input.


USE                             Blades
!bjj rm NWTC_Library: USE                             Constants
USE                             General
USE                             Modes
USE                             Output
!bjj rm NWTC_Library: USE                             Precision


IMPLICIT                        NONE


   ! Passed variables:

INTEGER(4), INTENT(IN )      :: K                                               ! Blade number.


   ! Local variables:

REAL(ReKi)                   :: TipDispl                                        ! Blade tip displacement for a mode shape.

INTEGER(4)                   :: I                                               ! A generic DO index.
INTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
INTEGER(4)                   :: Sttus                                           ! Status of allocation attempts.

CHARACTER(198)               :: Frmt                                            ! Format for element data.


   ! Global functions.

!bjj rm AD 12.70b CHARACTER(15), EXTERNAL      :: Flt2LStr                                        ! A function to convert a floating-point number to a left-justified string.
!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.




   ! Add a separator to the echo file if appropriate.

IF ( Echo )  WRITE (UnEc,'(//,A,/)')  'Blade '//TRIM( Int2LStr( K ) )//' input data from file "'//TRIM( BldFile(K) )//'":'


   ! Open the input file for blade K.

!BJJ:
CALL OpenFInpFile ( UnIn, BldFile(K) )



!  -------------- HEADER -------------------------------------------------------


   ! Ship the header.

READ (UnIn,'(//)',IOSTAT=IOS)

IF ( IOS < 0 )  THEN
   CALL PremEOF ( BldFile(K) , 'unused blade '//TRIM( Int2LStr( K ) )//' file header' )
ENDIF


!  -------------- BLADE PARAMETERS ---------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, BldFile(K), 'blade parameters', Echo  )
CALL ReadCom ( UnIn, BldFile(K), 'blade parameters'  )
!bjj End of proposed change

   ! NBlInpSt - Number of blade input stations.

CALL ReadIVar ( UnIn, BldFile(K), NBlInpSt, 'NBlInpSt', 'Number of blade input stations' )

IF ( NBlInpSt < 1 )  CALL ProgAbort ( ' NBlInpSt must be at least 1.' )


   ! CalcBMode - Calculate blade mode shapes (switch).

!JASON: ADD LOGIC FOR THIS NEW VARIABLE:
!JASON:CALL ReadLVar ( UnIn, BldFile(K), CalcBMode, 'CalcBMode', 'Calculate blade mode shapes' )
!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, BldFile(K), 'currently ignored CalcBMode', Echo  )   !JASON:
CALL ReadCom ( UnIn, BldFile(K), 'currently ignored CalcBMode'  )
!bjj End of proposed change


   ! BldFlDmp - Blade structural damping ratios in flapwise direction.

CALL ReadAryLines( UnIn, BldFile(K), BldFlDmp, 2, 'BldFlDmp', 'Blade structural damping ratios in flapwise direction' )

IF ( BldFlDmp(1) < 0.0 )    CALL ProgAbort ( ' BldFlDmp(1) for blade '//TRIM( Int2LStr( K ) )//' must not be negative.' )
IF ( BldFlDmp(2) < 0.0 )    CALL ProgAbort ( ' BldFlDmp(2) for blade '//TRIM( Int2LStr( K ) )//' must not be negative.' )


   ! BldEdDmp - Blade structural damping ratios in edgewise direction.

CALL ReadAryLines( UnIn, BldFile(K), BldEdDmp, 1, 'BldEdDmp', 'Blade structural damping ratios in edgewise direction' )

IF ( BldEdDmp(1) < 0.0 )    CALL ProgAbort ( ' BldEdDmp(1) for blade '//TRIM( Int2LStr( K ) )//' must not be negative.' )



!  -------------- BLADE ADJUSTMENT FACTORS -------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, BldFile(K), 'blade adjustment factors', Echo )
CALL ReadCom ( UnIn, BldFile(K), 'blade adjustment factors'  )
!bjj End of proposed change

   ! FlStTunr(1) - Blade flapwise modal stiffness tuners.

CALL ReadAryLines ( UnIn, BldFile(K), FlStTunr, 2, 'FlStTunr', 'Blade flapwise modal stiffness tuners' )

IF ( FlStTunr(1) <= 0.0 )  CALL ProgAbort ( ' FlStTunr(1) must be greater than zero.' )
IF ( FlStTunr(2) <= 0.0 )  CALL ProgAbort ( ' FlStTunr(2) must be greater than zero.' )


   ! AdjBlMs - Factor to adjust blade mass density.

CALL ReadRVar ( UnIn, BldFile(K), AdjBlMs, 'AdjBlMs', 'Factor to adjust blade mass density' )

IF ( AdjBlMs <= 0.0 )  CALL ProgAbort ( ' AdjBlMs must be greater than zero.' )


   ! AdjFlSt - Factor to adjust blade flap stiffness.

CALL ReadRVar ( UnIn, BldFile(K), AdjFlSt, 'AdjFlSt', 'Factor to adjust blade flap stiffness' )

IF ( AdjFlSt <= 0.0 )  CALL ProgAbort ( ' AdjFlSt must be greater than zero.' )


   ! AdjEdSt - Factor to adjust blade edge stiffness.

CALL ReadRVar ( UnIn, BldFile(K), AdjEdSt, 'AdjEdSt', 'Factor to adjust blade edge stiffness' )

IF ( AdjEdSt <= 0.0 )  CALL ProgAbort ( ' AdjEdSt must be greater than zero.' )



!  -------------- DISTRIBUTED BLADE PROPERTIES ---------------------------------


   ! Skip the comment lines.

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, BldFile(K), 'distributed blade parameters'     , Echo )
!rmCALL SkipComment ( UnIn, BldFile(K), 'distributed-blade-parameter names', .FALSE. )
!rmCALL SkipComment ( UnIn, BldFile(K), 'distributed-blade-parameter units', .FALSE. )
CALL ReadCom ( UnIn, BldFile(K), 'distributed blade parameters'  )
CALL ReadCom ( UnIn, BldFile(K), 'distributed-blade-parameter names' )
CALL ReadCom ( UnIn, BldFile(K), 'distributed-blade-parameter units'  )
!bjj End of proposed change


   ! Allocate the arrays.

ALLOCATE ( BlFract(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BlFract array.' )
ENDIF

ALLOCATE ( AerCen(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the AerCen array.' )
ENDIF

ALLOCATE ( StrcTwst(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the StrcTwst array.' )
ENDIF

ALLOCATE ( BMassDen(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BMassDen array.' )
ENDIF

ALLOCATE ( FlpStff(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the FlpStff array.' )
ENDIF

ALLOCATE ( EdgStff(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the EdgStff array.' )
ENDIF

ALLOCATE ( GJStff(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the GJStff array.' )
ENDIF

ALLOCATE ( EAStff(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the EAStff array.' )
ENDIF

ALLOCATE ( Alpha(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the Alpha array.' )
ENDIF

ALLOCATE ( FlpIner(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the FlpIner array.' )
ENDIF

ALLOCATE ( EdgIner(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the EdgIner array.' )
ENDIF

ALLOCATE ( PrecrvRef(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the PrecrvRef array.' )
ENDIF

ALLOCATE ( PreswpRef(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the PreswpRef array.' )
ENDIF

ALLOCATE ( FlpcgOf(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the FlpcgOf array.' )
ENDIF

ALLOCATE ( EdgcgOf(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the EdgcgOf array.' )
ENDIF

ALLOCATE ( FlpEAOf(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the FlpEAOf array.' )
ENDIF

ALLOCATE ( EdgEAOf(NBlInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the EdgEAOf array.' )
ENDIF


   ! Read the table.

IF ( Echo )  THEN

   IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, read in all the cols.

      Frmt = "( '  Node   BlFract   AeroCent   StrcTwst   BMassDen    FlpStff    EdgStff     GJStff     EAStff"// &
             "      Alpha    FlpIner    EdgIner  PrecrvRef  PreswpRef    FlpcgOf    EdgcgOf    FlpEAOf    EdgEAOf' )"
      WRITE (UnEc,Frmt)
      Frmt = "( '  ----   -------   --------   --------   --------    -------    -------     ------     ------"// &
             "      -----    -------    -------  ---------  ---------    -------    -------    -------    -------' )"
      WRITE (UnEc,Frmt)
      Frmt = '(I5,17(1X,'//TRIM( OutFmt )//'))'

   ELSE                                                     ! Only FAST will be run; thus, read in only the first 6 cols.

      Frmt = "( '  Node   BlFract   AeroCent   StrcTwst   BMassDen    FlpStff    EdgStff' )"
      WRITE (UnEc,Frmt)
      Frmt = "( '  ----   -------   --------   --------   --------    -------    -------' )"
      WRITE (UnEc,Frmt)
      Frmt = '(I5,6(1X,'//TRIM( OutFmt )//'))'

   ENDIF

ENDIF

DO I=1,NBlInpSt

   IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, read in all the cols.
      READ (UnIn,*,IOSTAT=IOS)  BlFract(I), AerCen(I), StrcTwst(I), BMassDen(I), FlpStff(I), EdgStff(I),            &
                                GJStff(I), EAStff(I), Alpha(I), FlpIner(I), EdgIner(I), PrecrvRef(I), PreswpRef(I), &
                                FlpcgOf(I), EdgcgOf(I), FlpEAOf(I), EdgEAOf(I)
   ELSE                                                     ! Only FAST will be run; thus, read in only the first 6 cols.
      READ (UnIn,*,IOSTAT=IOS)  BlFract(I), AerCen(I), StrcTwst(I), BMassDen(I), FlpStff(I), EdgStff(I)
   ENDIF

   CALL CheckIOS ( IOS, BldFile(K), 'line '//TRIM( Int2LStr( I ) )//' of the blade distributed properties for blade ' &
                                           //TRIM( Int2LStr( K ) ), NumType )
!bjj replace with above:                   //TRIM( Int2LStr( K ) ), Numeric )

   IF ( Echo )  THEN
      IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, read in all the cols.
         WRITE (UnEc,Frmt)  I, BlFract(I), AerCen(I), StrcTwst(I), BMassDen(I), FlpStff(I), EdgStff(I),            &
                               GJStff(I), EAStff(I), Alpha(I), FlpIner(I), EdgIner(I), PrecrvRef(I), PreswpRef(I), &
                               FlpcgOf(I), EdgcgOf(I), FlpEAOf(I), EdgEAOf(I)
      ELSE                                                     ! Only FAST will be run; thus, read in only the first 6 cols.
         WRITE (UnEc,Frmt)  I, BlFract(I), AerCen(I), StrcTwst(I), BMassDen(I), FlpStff(I), EdgStff(I)
      ENDIF
   ENDIF

   IF ( I == 1 )  THEN
      IF ( BlFract(I) /= 0.0 )  CALL ProgAbort ( ' BlFract(1) must be 0.0.' )
   ELSEIF ( ( I == NBlInpSt ) .AND. ( I /= 1 ) )  THEN
      IF ( BlFract(I) /= 1.0 )  CALL ProgAbort ( ' BlFract('//TRIM( Int2LStr( I ) )//') must be 1.0.' )
   ELSE
      IF ( BlFract(I) <= BlFract(I-1) )  CALL ProgAbort ( ' BlFract('//TRIM( Int2LStr( I ) )//') greater than '// &
                                                      TRIM( Flt2LStr( BlFract(I-1) ) )//'.'                     )
   ENDIF

   IF ( ( AerCen(I) ) < 0.0 .OR. ( AerCen(I) > 1.0 ) )  CALL ProgAbort ( ' AerCen('//TRIM( Int2LStr( I ) )//      &
                                                                     ') must be between 0 and 1 (inclusive).'   )
   IF ( ( StrcTwst(I) <= -180.0 ) .OR. ( StrcTwst(I) > 180.0 ) )  THEN
      CALL ProgAbort ( ' StrcTwst('//TRIM( Int2LStr( I ) )//') must be greater than -180 and less than or equal to 180.' )
   ENDIF
   IF ( BMassDen(I) <= 0.0 )  CALL ProgAbort ( ' BMassDen('//TRIM( Int2LStr( I ) )//') must be greater than zero.' )
   IF ( FlpStff (I) <= 0.0 )  CALL ProgAbort ( ' FlpStff('//TRIM( Int2LStr( I ) )//') must be greater than zero.' )
   IF ( EdgStff (I) <= 0.0 )  CALL ProgAbort ( ' EdgStff('//TRIM( Int2LStr( I ) )//') must be greater than zero.' )

   IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, read in all the cols.

      IF ( GJStff(I) <= 0.0 )  &
         CALL ProgAbort ( ' GJStff('//TRIM( Int2LStr( I ) )//') must be greater than zero.' )

      IF ( EAStff(I) <= 0.0 )  &
         CALL ProgAbort ( ' EAStff('//TRIM( Int2LStr( I ) )//') must be greater than zero.' )

      IF ( ( Alpha(I) <= -1.0 ) .OR. ( Alpha(I) >= 1.0 ) )  &
         CALL ProgAbort ( ' Alpha('//TRIM( Int2LStr( I ) )//') (the blade flap/twist'// &
                      ' coupling coefficient) must be between -1 and 1 (exclusive).'  )

      IF ( FlpIner(I) <  0.0 )  &
         CALL ProgAbort ( ' FlpIner('//TRIM( Int2LStr( I ) )//') must not be less than zero.' )

      IF ( EdgIner(I) <  0.0 )  &
         CALL ProgAbort ( ' EdgIner('//TRIM( Int2LStr( I ) )//') must not be less than zero.' )

   ! The reference axis must be coincident with the pitch axis at the blade
   !   root (I == 1):

      IF ( PrecrvRef(1) /= 0.0 )  &
         CALL ProgAbort ( ' Both PrecrvRef(1) and PreswpRef(1) must be zero.' )

      IF ( PreswpRef(1) /= 0.0 )  &
         CALL ProgAbort ( ' Both PrecrvRef(1) and PreswpRef(1) must be zero.' )

   ENDIF


      ! Apply the correction factors to the elemental data.

   BMassDen(I)  = BMassDen(I)*AdjBlMs
   FlpStff (I)  = FlpStff (I)*AdjFlSt
   EdgStff (I)  = EdgStff (I)*AdjEdSt

ENDDO ! I



!  -------------- BLADE MODE SHAPES --------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, BldFile(K), 'blade mode shapes', Echo  )
CALL ReadCom ( UnIn, BldFile(K), 'blade mode shapes'  )
!bjj End of proposed change


   ! BldFl1Sh - Blade-flap mode-1 shape coefficients.
!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Allow the mode shape to be described by a polynomial greater than sixth
!eab   order. Do this by setting the length of the mode shape coefficient array
!eab   to one less than the order of the polynomial.
!remove6.10bCALL ReadAryLines ( UnIn, BldFile(K), BldFl1Sh(:,K), 5, 'BldFl1Sh', 'Blade-flap mode-1 shape coefficients' )
CALL ReadAryLines ( UnIn, BldFile(K), BldFl1Sh(:,K), PolyOrd-1, 'BldFl1Sh', 'Blade-flap mode-1 shape coefficients' )
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.



TipDispl = 0.0

!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Loop through all mode shape coefficients.
!remove6.10bDO I=2,6
DO I=2,PolyOrd
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.
   TipDispl = TipDispl + BldFl1Sh(I,K)
ENDDO ! I

IF ( ABS( TipDispl - 1.0 ) > 0.001 )  CALL ProgAbort ( ' Blade-flap mode-1 shape coefficients must add to 1.0.' )


   ! BldFl2Sh - Blade-flap mode-2 shape coefficients.

!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Allow the mode shape to be described by a polynomial greater than sixth
!eab   order. Do this by setting the length of the mode shape coefficient array
!eab   to one less than the order of the polynomial.
!remove6.10bCALL ReadAryLines ( UnIn, BldFile(K), BldFl2Sh(:,K), 5, 'BldFl2Sh', 'Blade-flap mode-2 shape coefficients' )
CALL ReadAryLines ( UnIn, BldFile(K), BldFl2Sh(:,K), PolyOrd-1, 'BldFl2Sh', 'Blade-flap mode-2 shape coefficients' )
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.

TipDispl = 0.0

!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Loop through all mode shape coefficients.
!remove6.10bDO I=2,6
DO I=2,PolyOrd
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.
   TipDispl = TipDispl + BldFl2Sh(I,K)
ENDDO ! I

IF ( ABS( TipDispl - 1.0 ) > 0.001 )  CALL ProgAbort ( ' Blade-flap mode-2 shape coefficients must add to 1.0.' )


   ! BldEdgSh - Blade-edge mode shape coefficients.
   
!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Allow the mode shape to be described by a polynomial greater than sixth
!eab   order. Do this by setting the length of the mode shape coefficient array
!eab   to one less than the order of the polynomial.
!remove6.10bCALL ReadAryLines ( UnIn, BldFile(K), BldEdgSh(:,K), 5, 'BldEdgSh', 'Blade-edge mode shape coefficients' )
CALL ReadAryLines ( UnIn, BldFile(K), BldEdgSh(:,K), PolyOrd-1, 'BldEdgSh', 'Blade-edge mode shape coefficients' )
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.


TipDispl = 0.0

!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Loop through all mode shape coefficients.
!remove6.10bDO I=2,6
DO I=2,PolyOrd
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.
   TipDispl = TipDispl + BldEdgSh(I,K)
ENDDO ! I

IF ( ABS( TipDispl - 1.0 ) > 0.001 )  CALL ProgAbort ( ' Blade-edge mode shape coefficients must add to 1.0.' )



   ! Close the blade file.

CLOSE ( UnIn )



RETURN
END SUBROUTINE GetBlade
!=======================================================================
SUBROUTINE GetFurl


   ! This routine reads in the FAST furling input parameters from
   !   FurlFile and validates the input.


USE                             Features
USE                             General
USE                             InitCond
USE                             MassInert
USE                             Output
USE                             RotorFurling
!bjj rm NWTC_Library: USE                             SysSubs
USE                             TailAero
USE                             TailFurling
USE                             TurbConf


IMPLICIT                        NONE


   ! Local variables:

INTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.



   ! Open the FAST furling input file:

!BJJ:
CALL OpenFInpFile ( UnIn, FurlFile )


   ! Add a separator to the echo file if appropriate.

IF ( Echo )  WRITE (UnEc,'(//,A,/)')  'Furling input data from file "'//TRIM( FurlFile )//'":'



!  -------------- HEADER -------------------------------------------------------


   ! Skip the header.

READ (UnIn,'(//)',IOSTAT=IOS)

IF ( IOS < 0 )  THEN
   CALL PremEOF ( FurlFile , 'unused FAST furling-file header' )
ENDIF



!  -------------- FEATURE SWITCHES (CONT) --------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, FurlFile, 'degree of freedom switches (cont)', Echo  )
CALL ReadCom ( UnIn, FurlFile, 'degree of freedom switches (cont)'  )
!bjj End of proposed change


   ! RFrlDOF - Rotor-furl DOF.

CALL ReadLVar ( UnIn, FurlFile, RFrlDOF, 'RFrlDOF', 'Rotor-furl DOF' )


   ! TFrlDOF - Tail-furl DOF.

CALL ReadLVar ( UnIn, FurlFile, TFrlDOF, 'TFrlDOF', 'Tail-furl DOF' )



!  -------------- INITIAL CONDITIONS (CONT) ------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, FurlFile, 'initial conditions (cont)', Echo  )
CALL ReadCom ( UnIn, FurlFile, 'initial conditions (cont)'  )
!bjj End of proposed change


   ! RotFurl - Initial or fixed rotor-furl angle.

CALL ReadRVar ( UnIn, FurlFile, RotFurl, 'RotFurl', 'Initial or fixed rotor-furl angle' )

IF ( ( RotFurl <= -180.0 ) .OR. ( RotFurl > 180.0 ) )  &
   CALL ProgAbort ( ' RotFurl must be greater than -180 and less than or equal to 180.' )


   ! TailFurl - Initial or fixed tail-furl angle.

CALL ReadRVar ( UnIn, FurlFile, TailFurl, 'TailFurl', 'Initial or fixed tail-furl angle' )

IF ( ( TailFurl <= -180.0 ) .OR. ( TailFurl > 180.0 ) )  &
   CALL ProgAbort ( ' TailFurl must be greater than -180 and less than or equal to 180.' )



!  -------------- TURBINE CONFIGURATION (CONT) ---------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, FurlFile, 'turbine configuration (cont)', Echo  )
CALL ReadCom ( UnIn, FurlFile, 'turbine configuration (cont)'  )
!bjj End of proposed change


   ! Yaw2Shft - Lateral distance from yaw axis to rotor shaft.

CALL ReadRVar ( UnIn, FurlFile, Yaw2Shft, 'Yaw2Shft', 'Lateral distance from yaw axis to rotor shaft' )


   ! ShftSkew - Rotor shaft skew angle.

CALL ReadRVar ( UnIn, FurlFile, ShftSkew, 'ShftSkew', 'Rotor shaft skew angle' )

IF ( ( ShftSkew < -15.0 ) .OR. ( ShftSkew > 15.0 ) )  &
   CALL ProgAbort ( ' ShftSkew should only be used to skew the shaft a few degrees away from the zero-yaw position'//             &
                ' and must not be used as a replacement for the yaw angle.  ShftSkew must be between -15 and 15 (inclusive).'   )


   ! RFrlCMxn - Downwind distance from tower-top to CM of structure that furls with the rotor (not including rotor).

CALL ReadRVar ( UnIn, FurlFile, RFrlCMxn, 'RFrlCMxn', 'Downwind distance from tower-top to rotor-furl CM' )


   ! RFrlCMyn - Lateral  distance from tower-top to CM of structure that furls with the rotor (not including rotor).

CALL ReadRVar ( UnIn, FurlFile, RFrlCMyn, 'RFrlCMyn', 'Lateral  distance from tower-top to rotor-furl CM' )


   ! RFrlCMzn - Vertical distance from tower-top to CM of structure that furls with the rotor (not including rotor).

CALL ReadRVar ( UnIn, FurlFile, RFrlCMzn, 'RFrlCMzn', 'Vertical distance from tower-top to rotor-furl CM' )


   ! BoomCMxn - Downwind distance from tower-top to tail boom CM.

CALL ReadRVar ( UnIn, FurlFile, BoomCMxn, 'BoomCMxn', 'Downwind distance from tower-top to tail boom CM' )

IF ( BoomCMxn < 0.0 )  THEN   ! Print out warning when tail boom CM defined upwind of the tower.
   CALL UsrAlarm

   CALL WrScr1(' WARNING: ')
   CALL WrScr ('  Tail boom CM defined upwind of the tower (BoomCMxn < 0). ')
   CALL WrScr ('  Check for possible errors in file "'//TRIM( FurlFile )//'". ')
ENDIF


   ! BoomCMyn - Lateral  distance from tower-top to tail boom CM.

CALL ReadRVar ( UnIn, FurlFile, BoomCMyn, 'BoomCMyn', 'Lateral  distance from tower-top to tail boom CM' )


   ! BoomCMzn - Vertical distance from tower-top to tail boom CM.

CALL ReadRVar ( UnIn, FurlFile, BoomCMzn, 'BoomCMzn', 'Vertical distance from tower-top to tail boom CM' )


   ! TFinCMxn - Downwind distance from tower-top to tail fin CM.

CALL ReadRVar ( UnIn, FurlFile, TFinCMxn, 'TFinCMxn', 'Downwind distance from tower-top to tail fin CM' )

IF ( TFinCMxn < 0.0 )  THEN   ! Print out warning when tail fin CM defined upwind of the tower.
   CALL UsrAlarm

   CALL WrScr1(' WARNING: ')
   CALL WrScr ('  Tail fin CM defined upwind of the tower (TFinCMxn < 0). ')
   CALL WrScr ('  Check for possible errors in file "'//TRIM( FurlFile )//'". ')
ENDIF


   ! TFinCMyn - Lateral  distance from tower-top to tail fin CM.

CALL ReadRVar ( UnIn, FurlFile, TFinCMyn, 'TFinCMyn', 'Lateral  distance from tower-top to tail fin CM' )


   ! TFinCMzn - Vertical distance from tower-top to tail fin CM.

CALL ReadRVar ( UnIn, FurlFile, TFinCMzn, 'TFinCMzn', 'Vertical distance from tower-top to tail fin CM' )


   ! TFinCPxn - Downwind distance from tower-top to tail fin CP.

CALL ReadRVar ( UnIn, FurlFile, TFinCPxn, 'TFinCPxn', 'Downwind distance from tower-top to tail fin CP' )

IF ( TFinCPxn < 0.0 )  THEN   ! Print out warning when tail fin CP defined upwind of the tower.
   CALL UsrAlarm

   CALL WrScr1(' WARNING: ')
   CALL WrScr ('  Tail fin CP defined upwind of the tower (TFinCPxn < 0). ')
   CALL WrScr ('  Check for possible errors in file "'//TRIM( FurlFile )//'". ')
ENDIF


   ! TFinCPyn - Lateral  distance from tower-top to tail fin CP.

CALL ReadRVar ( UnIn, FurlFile, TFinCPyn, 'TFinCPyn', 'Lateral  distance from tower-top to tail fin CP' )


   ! TFinCPzn - Vertical distance from tower-top to tail fin CP.

CALL ReadRVar ( UnIn, FurlFile, TFinCPzn, 'TFinCPzn', 'Vertical distance from tower-top to tail fin CP' )


   ! TFinSkew - Tail fin chordline skew angle.

CALL ReadRVar ( UnIn, FurlFile, TFinSkew, 'TFinSkew', 'Tail fin chordline skew angle' )

IF ( ( TFinSkew <= -180.0 ) .OR. ( TFinSkew > 180.0 ) )  &
   CALL ProgAbort ( ' TFinSkew must be greater than -180 and less than or equal to 180.' )


   ! TFinTilt - Tail fin chordline tilt angle.

CALL ReadRVar ( UnIn, FurlFile, TFinTilt, 'TFinTilt', 'Tail fin chordline tilt angle' )

IF ( ( TFinTilt < -90.0 ) .OR. ( TFinTilt > 90.0 ) )  CALL ProgAbort ( ' TFinTilt must be between -90 and 90 (inclusive).' )


   ! TFinBank - Tail fin planform  bank angle.

CALL ReadRVar ( UnIn, FurlFile, TFinBank, 'TFinBank', 'Tail fin planform  bank angle' )

IF ( ( TFinBank <= -180.0 ) .OR. ( TFinBank > 180.0 ) )  &
   CALL ProgAbort ( ' TFinBank must be greater than -180 and less than or equal to 180.' )


   ! RFrlPntxn - Downwind distance from tower-top to arbitrary point on rotor-furl axis.

CALL ReadRVar ( UnIn, FurlFile, RFrlPntxn, 'RFrlPntxn', 'Downwind distance from tower-top to arbitrary point on rotor-furl axis' )


   ! RFrlPntyn - Lateral  distance from tower-top to arbitrary point on rotor-furl axis.

CALL ReadRVar ( UnIn, FurlFile, RFrlPntyn, 'RFrlPntyn', 'Lateral  distance from tower-top to arbitrary point on rotor-furl axis' )


   ! RFrlPntzn - Vertical distance from tower-top to arbitrary point on rotor-furl axis.

CALL ReadRVar ( UnIn, FurlFile, RFrlPntzn, 'RFrlPntzn', 'Vertical distance from tower-top to arbitrary point on rotor-furl axis' )


   ! RFrlSkew - Rotor-furl axis skew angle.

CALL ReadRVar ( UnIn, FurlFile, RFrlSkew, 'RFrlSkew', 'Rotor-furl axis skew angle' )

IF ( ( RFrlSkew <= -180.0 ) .OR. ( RFrlSkew > 180.0 ) )  &
   CALL ProgAbort ( ' RFrlSkew must be greater than -180 and less than or equal to 180.' )


   ! RFrlTilt - Rotor-furl axis tilt angle.

CALL ReadRVar ( UnIn, FurlFile, RFrlTilt, 'RFrlTilt', 'Rotor-furl axis tilt angle' )

IF ( ( RFrlTilt < -90.0 ) .OR. ( RFrlTilt > 90.0 ) )  CALL ProgAbort ( ' RFrlTilt must be between -90 and 90 (inclusive).' )


   ! TFrlPntxn - Downwind distance from tower-top to arbitrary point on tail-furl axis.

CALL ReadRVar ( UnIn, FurlFile, TFrlPntxn, 'TFrlPntxn', 'Downwind distance from tower-top to arbitrary point on tail-furl axis' )


   ! TFrlPntyn - Lateral  distance from tower-top to arbitrary point on tail-furl axis.

CALL ReadRVar ( UnIn, FurlFile, TFrlPntyn, 'TFrlPntyn', 'Lateral  distance from tower-top to arbitrary point on tail-furl axis' )


   ! TFrlPntzn - Vertical distance from tower-top to arbitrary point on tail-furl axis.

CALL ReadRVar ( UnIn, FurlFile, TFrlPntzn, 'TFrlPntzn', 'Vertical distance from tower-top to arbitrary point on tail-furl axis' )


   ! TFrlSkew - Tail-furl axis skew angle.

CALL ReadRVar ( UnIn, FurlFile, TFrlSkew, 'TFrlSkew', 'Tail-furl axis skew angle' )

IF ( ( TFrlSkew <= -180.0 ) .OR. ( TFrlSkew > 180.0 ) )  &
   CALL ProgAbort ( ' TFrlSkew must be greater than -180 and less than or equal to 180.' )


   ! TFrlTilt - Tail-furl axis tilt angle.

CALL ReadRVar ( UnIn, FurlFile, TFrlTilt, 'TFrlTilt', 'Tail-furl axis tilt angle' )

IF ( ( TFrlTilt < -90.0 ) .OR. ( TFrlTilt > 90.0 ) )  CALL ProgAbort ( ' TFrlTilt must be between -90 and 90 (inclusive).' )



!  -------------- MASS AND INERTIA (CONT) --------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, FurlFile, 'mass and inertia (cont)', Echo  )
CALL ReadCom ( UnIn, FurlFile, 'mass and inertia (cont)'  )
!bjj End of proposed change

   ! RFrlMass - Mass of structure that furls with the rotor (not including rotor).

CALL ReadRVar ( UnIn, FurlFile, RFrlMass, 'RFrlMass', 'Rotor-furl mass' )

IF ( RFrlMass < 0.0 )  CALL ProgAbort ( ' RFrlMass must not be negative.' )


   ! BoomMass - Tail boom mass.

CALL ReadRVar ( UnIn, FurlFile, BoomMass, 'BoomMass', 'Tail boom mass' )

IF ( BoomMass < 0.0 )  CALL ProgAbort ( ' BoomMass must not be negative.' )


   ! TFinMass - Tail fin mass.

CALL ReadRVar ( UnIn, FurlFile, TFinMass, 'TFinMass', 'Tail fin mass' )

IF ( TFinMass < 0.0 )  CALL ProgAbort ( ' TFinMass must not be negative.' )


   ! RFrlIner - Inertia of structure that furls with the rotor about the rotor-furl axis (not including rotor).

CALL ReadRVar ( UnIn, FurlFile, RFrlIner, 'RFrlIner', 'Rotor-furl inertia about rotor-furl axis' )

IF ( RFrlIner < 0.0 )  CALL ProgAbort ( ' RFrlIner must not be negative.' )


   ! TFrlIner - Tail boom inertia about tail-furl axis.

CALL ReadRVar ( UnIn, FurlFile, TFrlIner, 'TFrlIner', 'Tail boom inertia about tail-furl axis' )

IF ( TFrlIner < 0.0 )  CALL ProgAbort ( ' TFrlIner must not be negative.' )



!  -------------- ROTOR-FURL ---------------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, FurlFile, 'rotor-furl', Echo  )
CALL ReadCom ( UnIn, FurlFile, 'rotor-furl' )
!bjj End of proposed change


   ! RFrlMod - Rotor-furl spring/damper model switch.

CALL ReadIVar ( UnIn, FurlFile, RFrlMod, 'RFrlMod', 'Rotor-furl spring/damper model switch' )

IF ( ( RFrlMod /= 0 ) .AND. ( RFrlMod /= 1 ) .AND. ( RFrlMod /= 2 ) )  CALL ProgAbort ( ' RFrlMod must be 0, 1, or 2.' )


   ! RFrlSpr - Rotor-furl spring constant.

CALL ReadRVar ( UnIn, FurlFile, RFrlSpr, 'RFrlSpr', 'Rotor-furl spring constant' )

IF ( RFrlSpr < 0.0 )  CALL ProgAbort ( ' RFrlSpr must not be negative.' )


   ! RFrlDmp - Rotor-furl damping constant.

CALL ReadRVar ( UnIn, FurlFile, RFrlDmp, 'RFrlDmp', 'Rotor-furl damping constant' )

IF ( RFrlDmp < 0.0 )  CALL ProgAbort ( ' RFrlDmp must not be negative.' )


   ! RFrlCDmp - Rotor-furl rate-independent Coulomb-damping moment.

CALL ReadRVar ( UnIn, FurlFile, RFrlCDmp, 'RFrlCDmp', 'Rotor-furl rate-independent Coulomb-damping moment' )

IF ( RFrlCDmp < 0.0 )  CALL ProgAbort ( ' RFrlCDmp must not be negative.' )


   ! RFrlUSSP - Rotor-furl up-stop spring position.

CALL ReadRVar ( UnIn, FurlFile, RFrlUSSP, 'RFrlUSSP', 'Rotor-furl up-stop spring position' )

IF ( ( RFrlUSSP <= -180.0 ) .OR. ( RFrlUSSP > 180.0 ) )  &
   CALL ProgAbort ( ' RFrlUSSP must be greater than -180 and less than or equal to 180.' )


   ! RFrlDSSP - Rotor-furl down-stop spring position.

CALL ReadRVar ( UnIn, FurlFile, RFrlDSSP, 'RFrlDSSP', 'Rotor-furl down-stop spring position' )

IF ( ( RFrlDSSP <= -180.0 ) .OR. ( RFrlDSSP > RFrlUSSP ) )  &
   CALL ProgAbort ( ' RFrlDSSP must be greater than -180 and less than or equal to RFrlUSSP.' )


   ! RFrlUSSpr - Rotor-furl up-stop spring constant.

CALL ReadRVar ( UnIn, FurlFile, RFrlUSSpr, 'RFrlUSSpr', 'Rotor-furl up-stop spring constant' )

IF ( RFrlUSSpr < 0.0 )  CALL ProgAbort ( ' RFrlUSSpr must not be negative.' )


   ! RFrlDSSpr - Rotor-furl down-stop spring constant.

CALL ReadRVar ( UnIn, FurlFile, RFrlDSSpr, 'RFrlDSSpr', 'Rotor-furl down-stop spring constant' )

IF ( RFrlDSSpr < 0.0 )  CALL ProgAbort ( ' RFrlDSSpr must not be negative.' )


   ! RFrlUSDP - Rotor-furl up-stop damper position.

CALL ReadRVar ( UnIn, FurlFile, RFrlUSDP, 'RFrlUSDP', 'Rotor-furl up-stop damper position' )

IF ( ( RFrlUSDP <= -180.0 ) .OR. ( RFrlUSDP > 180.0 ) )  &
   CALL ProgAbort ( ' RFrlUSDP must be greater than -180 and less than or equal to 180.' )


   ! RFrlDSDP - Rotor-furl down-stop damper position.

CALL ReadRVar ( UnIn, FurlFile, RFrlDSDP, 'RFrlDSDP', 'Rotor-furl down-stop damper position' )

IF ( ( RFrlDSDP <= -180.0 ) .OR. ( RFrlDSDP > RFrlUSDP ) )  &
   CALL ProgAbort ( ' RFrlDSDP must be greater than -180 and less than or equal to RFrlUSDP.' )


   ! RFrlUSDmp - Rotor-furl up-stop damping constant.

CALL ReadRVar ( UnIn, FurlFile, RFrlUSDmp, 'RFrlUSDmp', 'Rotor-furl up-stop damping constant' )

IF ( RFrlUSDmp < 0.0 )  CALL ProgAbort ( ' RFrlUSDmp must not be negative.' )


   ! RFrlDSDmp - Rotor-furl down-stop damping constant.

CALL ReadRVar ( UnIn, FurlFile, RFrlDSDmp, 'RFrlDSDmp', 'Rotor-furl down-stop damping constant' )

IF ( RFrlDSDmp < 0.0 )  CALL ProgAbort ( ' RFrlDSDmp must not be negative.' )



!  -------------- TAIL-FURL ----------------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, FurlFile, 'tail-furl', Echo  )
CALL ReadCom ( UnIn, FurlFile, 'tail-furl' )
!bjj End of proposed change


   ! TFrlMod - Tail-furl spring/damper model switch.

CALL ReadIVar ( UnIn, FurlFile, TFrlMod, 'TFrlMod', 'Tail-furl spring/damper model switch' )

IF ( ( TFrlMod /= 0 ) .AND. ( TFrlMod /= 1 ) .AND. ( TFrlMod /= 2 ) )  CALL ProgAbort ( ' TFrlMod must be 0, 1, or 2.' )


   ! TFrlSpr - Tail-furl spring constant.

CALL ReadRVar ( UnIn, FurlFile, TFrlSpr, 'TFrlSpr', 'Tail-furl spring constant' )

IF ( TFrlSpr < 0.0 )  CALL ProgAbort ( ' TFrlSpr must not be negative.' )


   ! TFrlDmp - Tail-furl damping constant.

CALL ReadRVar ( UnIn, FurlFile, TFrlDmp, 'TFrlDmp', 'Tail-furl damping constant' )

IF ( TFrlDmp < 0.0 )  CALL ProgAbort ( ' TFrlDmp must not be negative.' )


   ! TFrlCDmp - Tail-furl rate-independent Coulomb-damping moment.

CALL ReadRVar ( UnIn, FurlFile, TFrlCDmp, 'TFrlCDmp', 'Tail-furl rate-independent Coulomb-damping moment' )

IF ( TFrlCDmp < 0.0 )  CALL ProgAbort ( ' TFrlCDmp must not be negative.' )


   ! TFrlUSSP - Tail-furl up-stop spring position.

CALL ReadRVar ( UnIn, FurlFile, TFrlUSSP, 'TFrlUSSP', 'Tail-furl up-stop spring position' )

IF ( ( TFrlUSSP <= -180.0 ) .OR. ( TFrlUSSP > 180.0 ) )  &
   CALL ProgAbort ( ' TFrlUSSP must be greater than -180 and less than or equal to 180.' )


   ! TFrlDSSP - Tail-furl down-stop spring position.

CALL ReadRVar ( UnIn, FurlFile, TFrlDSSP, 'TFrlDSSP', 'Tail-furl down-stop spring position' )

IF ( ( TFrlDSSP <= -180.0 ) .OR. ( TFrlDSSP > TFrlUSSP ) )  &
   CALL ProgAbort ( ' TFrlDSSP must be greater than -180 and less than or equal to TFrlUSSP.' )


   ! TFrlUSSpr - Tail-furl up-stop spring constant.

CALL ReadRVar ( UnIn, FurlFile, TFrlUSSpr, 'TFrlUSSpr', 'Tail-furl up-stop spring constant' )

IF ( TFrlUSSpr < 0.0 )  CALL ProgAbort ( ' TFrlUSSpr must not be negative.' )


   ! TFrlDSSpr - Tail-furl down-stop spring constant.

CALL ReadRVar ( UnIn, FurlFile, TFrlDSSpr, 'TFrlDSSpr', 'Tail-furl down-stop spring constant' )

IF ( TFrlDSSpr < 0.0 )  CALL ProgAbort ( ' TFrlDSSpr must not be negative.' )


   ! TFrlUSDP - Tail-furl up-stop damper position.

CALL ReadRVar ( UnIn, FurlFile, TFrlUSDP, 'TFrlUSDP', 'Tail-furl up-stop damper position' )

IF ( ( TFrlUSDP <= -180.0 ) .OR. ( TFrlUSDP > 180.0 ) )  &
   CALL ProgAbort ( ' TFrlUSDP must be greater than -180 and less than or equal to 180.' )


   ! TFrlDSDP - Tail-furl down-stop damper position.

CALL ReadRVar ( UnIn, FurlFile, TFrlDSDP, 'TFrlDSDP', 'Tail-furl down-stop damper position' )

IF ( ( TFrlDSDP <= -180.0 ) .OR. ( TFrlDSDP > TFrlUSDP ) )  &
   CALL ProgAbort ( ' TFrlDSDP must be greater than -180 and less than or equal to TFrlUSDP.' )


   ! TFrlUSDmp - Tail-furl up-stop damping constant.

CALL ReadRVar ( UnIn, FurlFile, TFrlUSDmp, 'TFrlUSDmp', 'Tail-furl up-stop damping constant' )

IF ( TFrlUSDmp < 0.0 )  CALL ProgAbort ( ' TFrlUSDmp must not be negative.' )


   ! TFrlDSDmp - Tail-furl down-stop damping constant.

CALL ReadRVar ( UnIn, FurlFile, TFrlDSDmp, 'TFrlDSDmp', 'Tail-furl down-stop damping constant' )

IF ( TFrlDSDmp < 0.0 )  CALL ProgAbort ( ' TFrlDSDmp must not be negative.' )



!  -------------- TAIL FIN AERODYNAMICS ----------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, FurlFile, 'tail fin aerodynamics', Echo  )
CALL ReadCom ( UnIn, FurlFile, 'tail fin aerodynamics' )
!bjj End of proposed change


   ! TFinMod - Tail fin aerodynamics model switch.

CALL ReadIVar ( UnIn, FurlFile, TFinMod, 'TFinMod', 'Tail fin aerodynamics model switch' )

IF ( ( TFinMod /= 0 ) .AND. ( TFinMod /= 1 ) .AND. ( TFinMod /= 2 ) )  CALL ProgAbort ( ' TFinMod must be 0, 1, or 2.' )


   ! TFinNFoil - Tail fin airfoil number.

CALL ReadIVar ( UnIn, FurlFile, TFinNFoil, 'TFinNFoil', 'Tail fin airfoil number' )


   ! TFinArea - Tail fin planform area.

CALL ReadRVar ( UnIn, FurlFile, TFinArea, 'TFinArea', 'Tail fin planform area' )

IF ( TFinArea < 0.0 )  CALL ProgAbort ( ' TFinArea must not be negative.' )


   ! SubAxInd - Subtract rotor axial induction?

CALL ReadLVar ( UnIn, FurlFile, SubAxInd, 'SubAxInd', 'Subtract rotor axial induction?' )



   ! Close the FAST furling file:

CLOSE ( UnIn )



RETURN
END SUBROUTINE GetFurl
!=======================================================================
SUBROUTINE GetLin


   ! This routine reads in the FAST linearization input parameters from
   !   LinFile and validates the input.


USE                             Features
USE                             General
USE                             Linear
USE                             Output
USE                             TurbConf
USE                             TurbCont


IMPLICIT                        NONE


   ! Local variables:

INTEGER(4)                   :: I                                               ! A generic index.
INTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
INTEGER(4)                   :: L                                               ! A generic index.


   ! Global functions:

!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.



   ! Open the FAST linearization input file:

!BJJ:
CALL OpenFInpFile ( UnIn, LinFile )


   ! Add a separator to the echo file if appropriate:

IF ( Echo )  WRITE (UnEc,'(//,A,/)')  'FAST linearization input data from file "'//TRIM( LinFile )//'":'



!  -------------- HEADER -------------------------------------------------------


   ! Skip the header.

READ (UnIn,'(//)',IOSTAT=IOS)

IF ( IOS < 0 )  THEN
   CALL PremEOF ( LinFile , 'unused FAST linearization-file header' )
ENDIF



!  -------------- PERIODIC STEADY STATE SOLUTION -------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, LinFile, 'Periodic steady state solution', Echo  )
CALL ReadCom ( UnIn, LinFile, 'Periodic steady state solution' )
!bjj End of proposed change


   ! CalcStdy - Calculate periodic steady state condition.

CALL ReadLVar ( UnIn, LinFile, CalcStdy, 'CalcStdy', 'Calculate periodic steady state condition' )


IF ( CalcStdy )  THEN   ! Only read in these variables if we will be computing a steady state solution


   ! TrimCase - Trim case.

   IF ( GenDOF )  THEN  ! Only read in TrimCase if we will be computing a steady state solution with a variable speed generator

      CALL ReadIVar ( UnIn, LinFile, TrimCase, 'TrimCase', 'Trim Case' )

      IF ( ( TrimCase < 1 ) .OR. ( TrimCase > 3 ) )  CALL ProgAbort ( ' TrimCase must be 1, 2, or 3.' )
      IF ( ( TrimCase == 3 ) )  THEN   ! We will be trimming rotor collective blade pitch

         IF (    BlPitch(1) /= BlPitch(2) )  &
               CALL ProgAbort ( ' All blade pitch angles must be identical when trimming with collective blade pitch.' )
         IF ( NumBl == 3 )  THEN ! 3-blader
            IF ( BlPitch(1) /= BlPitch(3) )  &
               CALL ProgAbort ( ' All blade pitch angles must be identical when trimming with collective blade pitch.' )
         ENDIF

      ENDIF

   ELSE                 ! Don't read in TrimCase since we wont be computing a steady state solution with a variable speed generator

!bjj Start of proposed change vXX NWTC_Lib
!rmCALL SkipComment ( UnIn, LinFile, 'unused TrimCase', .FALSE. )
         CALL ReadCom ( UnIn, LinFile, 'unused TrimCase' )
!bjj End of proposed change

   ENDIF


   ! DispTol - Convergence tolerance for the 2-norm of displacements in the periodic steady state calculation.

   CALL ReadRVar ( UnIn, LinFile, DispTol, 'DispTol', 'Convergence tolerance for displacements' )

   IF ( DispTol <= 0.0 )  CALL ProgAbort ( ' DispTol must be greater than 0.' )


   ! VelTol  - Convergence tolerance for the 2-norm of velocities    in the periodic steady state calculation.

   CALL ReadRVar ( UnIn, LinFile, VelTol, 'VelTol', 'Convergence tolerance for velocities' )

   IF ( VelTol <= 0.0 )  CALL ProgAbort ( ' VelTol must be greater than 0.' )


ELSE                    ! Don't read in these variables since we wont be computing a steady state solution


!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, LinFile, 'unused TrimCase', .FALSE. )
!rm   CALL SkipComment ( UnIn, LinFile, 'unused DispTol', .FALSE. )
!rm   CALL SkipComment ( UnIn, LinFile, 'unused VelTol', .FALSE. )
   CALL ReadCom ( UnIn, LinFile, 'unused TrimCase' )
   CALL ReadCom ( UnIn, LinFile, 'unused DispTol' )
   CALL ReadCom ( UnIn, LinFile, 'unused VelTol' )
!bjj End of proposed change


ENDIF



!  -------------- MODEL LINEARIZATION ------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, LinFile, 'Model linearization', Echo  )
   CALL ReadCom ( UnIn, LinFile, 'Model linearization' )
!bjj End of proposed change


   ! NAzimStep - Number of azimuth steps in periodic linearized model.

CALL ReadIVar ( UnIn, LinFile, NAzimStep, 'NAzimStep', 'Number of azimuth steps in periodic linearized model' )

IF ( NAzimStep <= 0 )  CALL ProgAbort ( ' NAzimStep must be greater than 0.' )


   ! MdlOrder - Order of output linearized model.

CALL ReadIVar ( UnIn, LinFile, MdlOrder, 'MdlOrder', 'Order of output linearized model' )

IF ( ( MdlOrder < 1 ) .OR. ( MdlOrder > 2 ) )  CALL ProgAbort ( ' MdlOrder must be 1 or 2.' )



!  -------------- INPUTS AND DISTURBANCES --------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, LinFile, 'Inputs and disturbances', Echo  )
   CALL ReadCom ( UnIn, LinFile, 'Inputs and disturbances' )
!bjj End of proposed change


   ! NInputs - Number of control inputs.

CALL ReadIVar ( UnIn, LinFile, NInputs, 'NInputs', 'Number of control inputs' )

IF ( NumBl == 3 )  THEN ! 3-blader
   IF ( ( NInputs < 0 ) .OR. ( NInputs  > 7 ) )  CALL ProgAbort ( ' NInputs must be between 0 and 7 (inclusive) for 3-blader.' )
ELSE                    ! 2-blader
   IF ( ( NInputs < 0 ) .OR. ( NInputs  > 6 ) )  CALL ProgAbort ( ' NInputs must be between 0 and 6 (inclusive) for 2-blader.' )
ENDIF


   ! CntrlInpt - List of control inputs.

READ (UnIn,*,IOSTAT=IOS)  ( CntrlInpt(I), I=1,NInputs )

IF ( IOS < 0 )  THEN
   CALL PremEOF ( LinFile , 'CntrlInpt' )
ELSEIF ( IOS > 0 )  THEN
   CALL WrScr1 ( ' Invalid numerical input for file "'//TRIM( LinFile )//'.' )
   CALL ProgAbort  ( ' The error occurred while trying to read the CntrlInpt array.' )
ENDIF

IF ( Echo )  THEN
   WRITE (UnEc,"(15X,A,T27,' - ',A)")  'CntrlInpt', 'List of control inputs'
   WRITE (UnEc,'(7(I4,:))')  ( CntrlInpt(I), I=1,NInputs )
ENDIF

IF ( NumBl == 3 )  THEN ! 3-blader


   DO I=1,NInputs ! Loop through all control inputs

      IF ( ( CntrlInpt(I) <  1 ) .OR. ( CntrlInpt(I) > 7 ) )  &
         CALL ProgAbort  ( ' All CntrlInpt values must be between 1 and 7 (inclusive) for 3-blader.' )

      IF (   CntrlInpt(I) == 4 )  THEN ! Rotor collective blade pitch is a control input
         IF ( ( BlPitch(1) /= BlPitch(2) ) .OR. ( BlPitch(1) /= BlPitch(3) ) )  &
            CALL ProgAbort ( ' All blade pitch angles must be identical when collective blade pitch is a control input.' )
      ENDIF

      DO L=1,I-1  ! Loop through all previous control inputs
         IF ( CntrlInpt(I) == CntrlInpt(L) ) &  ! .TRUE. if CntrlInpt(I) is listed twice in array CntrlInpt; therefore Abort.
            CALL ProgAbort  ( ' CntrlInpt value '//TRIM(Int2LStr( CntrlInpt(I) ))//                                             &
                          ' is listed twice in array CntrlInpt.  Change the value of CntrlInpt('//TRIM(Int2LStr( I ))//').'   )
      ENDDO       ! L - All previous control inputs

   ENDDO          ! I - All control inputs


ELSE                    ! 2-blader


   DO I=1,NInputs ! Loop through all control inputs

      IF ( ( CntrlInpt(I) <  1 ) .OR. ( CntrlInpt(I) > 6 ) )  &
         CALL ProgAbort  ( ' All CntrlInpt values must be between 1 and 6 (inclusive) for 2-blader.' )

      IF (   CntrlInpt(I) == 4 )  THEN ! Rotor collective blade pitch is a control input
         IF ( BlPitch(1) /= BlPitch(2) )  &
            CALL ProgAbort ( ' All blade pitch angles must be identical when collective blade pitch is a control input.' )
      ENDIF

      DO L=1,I-1  ! Loop through all previous control inputs
         IF ( CntrlInpt(I) == CntrlInpt(L) ) &  ! .TRUE. if CntrlInpt(I) is listed twice in array CntrlInpt; therefore Abort.
            CALL ProgAbort  ( ' CntrlInpt value '//TRIM(Int2LStr( CntrlInpt(I) ))//                                             &
                          ' is listed twice in array CntrlInpt.  Change the value of CntrlInpt('//TRIM(Int2LStr( I ))//').'   )
      ENDDO       ! L - All previous control inputs

   ENDDO          ! I - All control inputs


ENDIF


   ! NDisturbs - Number of wind disturbances.

CALL ReadIVar ( UnIn, LinFile, NDisturbs, 'NDisturbs', 'Number of wind disturbances' )

IF ( ( NDisturbs < 0 ) .OR.  ( NDisturbs  > 7 ) )  CALL ProgAbort ( ' NDisturbs must be between 0 and 7 (inclusive).' )
IF ( ( NDisturbs > 0 ) .AND. ( .NOT. CompAero ) )  &
   CALL ProgAbort ( ' There can be no wind disturbances if CompAero is False.  Set NDisturbs to 0 or CompAero to True.' )


   ! Disturbnc - List   of wind disturbances.

READ (UnIn,*,IOSTAT=IOS)  ( Disturbnc(I), I=1,NDisturbs )

IF ( IOS < 0 )  THEN
   CALL PremEOF ( LinFile , 'Disturbnc' )
ELSEIF ( IOS > 0 )  THEN
   CALL WrScr1 ( ' Invalid numerical input for file "'//TRIM( LinFile )//'.' )
   CALL ProgAbort  ( ' The error occurred while trying to read the Disturbnc array.' )
ENDIF

IF ( Echo )  THEN
   WRITE (UnEc,"(15X,A,T27,' - ',A)")  'Disturbnc', 'List of wind disturbances'
   WRITE (UnEc,'(7(I4,:))')  ( Disturbnc(I), I=1,NDisturbs )
ENDIF

DO I=1,NDisturbs  ! Loop through all wind disturbances

   IF ( ( Disturbnc(I) < 1 ) .OR. ( Disturbnc(I) > 7 ) )  &
      CALL ProgAbort  ( ' All Disturbnc values must be between 1 and 7 (inclusive).' )

   DO L=1,I-1  ! Loop through all previous wind disturbances
      IF ( Disturbnc(I) == Disturbnc(L) ) &  ! .TRUE. if Disturbnc(I) is listed twice in array Disturbnc; therefore Abort.
         CALL ProgAbort  ( ' Disturbnc value '//TRIM(Int2LStr( Disturbnc(I) ))//                                             &
                       ' is listed twice in array Disturbnc.  Change the value of Disturbnc('//TRIM(Int2LStr( I ))//').'   )
   ENDDO       ! L - All previous wind disturbances

ENDDO             ! I - All wind disturbances



   ! Close the FAST linearization file.

CLOSE ( UnIn )



RETURN
END SUBROUTINE GetLin
!=======================================================================
SUBROUTINE GetPrimary


   ! This routine reads the primary parameter file and validates the input.


USE                             Blades
!bjj rm NWTC_Library: USE                             Constants
USE                             Drivetrain
USE                             EnvCond
USE                             Features
USE                             General
USE                             InitCond
USE                             Linear
USE                             MassInert
USE                             NacelleYaw
USE                             Output
USE                             SimCont
!bjj rm NWTC_Library: USE                             SysSubs
USE                             TeeterVars
USE                             TipBrakes
USE                             Tower
USE                             TurbConf
USE                             TurbCont

IMPLICIT                        NONE


   ! Local variables.

INTEGER(4)                   :: I                                               ! A generic index.
INTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
INTEGER(4)                   :: K                                               ! Blade number.
INTEGER(4)                   :: NumWords                                        ! The number of words w/n a read in line.
INTEGER(4)                   :: Sttus                                           ! Status returned from an allocation request.

!bjj start of proposed change
!rmCHARACTER( 100)              :: Comment                                         ! String to temporarily hold the comment line.
CHARACTER( 1024)             :: Comment                                         ! String to temporarily hold the comment line.
!bjj end of prposed change
CHARACTER(   3)              :: EndOfFile                                       ! String read in at the end of the input file.
!bjj start of proposed change
!change to match NWTC_Library formats
!bjj rmCHARACTER(  31)              :: Frmt      = "( L13, 2X, A, T27, ' - ', A )"     ! Output format for logical parameters.
CHARACTER(  35)              :: Frmt      = "( 2X, L11, 2X, A, T30, ' - ', A )" ! Output format for logical parameters.
!bjj end of proposed change
CHARACTER(1000)              :: OutLine                                         ! String to temporarily hold the output parameter list.


   ! Global functions.

!bjj rm NWTC_LIB: INTEGER(4), EXTERNAL         :: CountWords                                      ! A function that returns the number of words within a string of text.
!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: CurDate                                         ! A function that returns the current date in the form "dd-mmm-ccyy".
!bjj rm AD 12.70b CHARACTER( 8), EXTERNAL      :: CurTime                                         ! A function that returns the current date in the form "hh:mm:ss".
!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.



   ! Open the primary input file.

!BJJ:
CALL OpenFInpFile ( UnIn, PriFile )



!-------------------------- HEADER ---------------------------------------------

READ (UnIn,'(//,A,/)',IOSTAT=IOS)  FTitle

!bjj start of proposed change
CALL CheckIOS( IOS, PriFile, 'file title', StrType )
!bjj end of proposed change


!bjj start of proposed change 6.02d-bjj
!rmCALL WrScr1( ' '//TRIM( FTitle ) )
!CALL WrScr1( ' Heading of the FAST input file: '//TRIM( FTitle ) )
!bjj end of proposed change

!bjj start of proposed change
!rmIF ( IOS < 0 )  THEN
!rm   CALL PremEOF ( PriFile , 'FTitle' )
!rmENDIF
!bjj end of proposed change



!-------------------------- SIMULATION CONTROL PARAMETERS ----------------------


   ! Skip the comment line.

READ (UnIn,'(A)',IOSTAT=IOS)  Comment

!bjj start of proposed change
!rmIF ( IOS < 0 )  THEN
!rm   CALL WrScr1 ( ' Premature EOF for file "'//TRIM( PriFile )//'.' )
!rm   CALL ProgAbort  ( ' The error occurred while trying to skip the simulation control parameters comment.' )
!rmENDIF
CALL CheckIOS( IOS, PriFile, 'simulation control parameters comment', StrType )
!bjj end of proposed change


   ! Echo - Echo input to "echo.out".

!bjj start of proposed change
!rmREAD (UnIn,*,IOSTAT=IOS)  Echo
!rmCALL CheckIOS ( IOS, PriFile, 'Echo', FlagType )
READ (UnIn,*,IOSTAT=IOS)  WrEcho
CALL CheckIOS ( IOS, PriFile, 'Echo', FlgType )
Echo = WrEcho
!bjj end of proposed change

IF ( Echo )  THEN
!bjj start of proposed change
!rm   CALL OpenFOutFile ( UnEc, 'echo.out' ) !bjj: is there a conflict with NWTC Echo file here?
   CALL OpenEcho ( UnEc, TRIM(RootName)//'.ech' )
!bjj end of proposed change
!bjj start of proposed change vXX
!rm   WRITE (UnEc,'(/,A)'   )  'This file of echoed input was generated by '//ProgName//ProgVer// &
!rm                            ' on '//CurDate()//' at '//CurTime()//'.'
   WRITE (UnEc,'(/,A)'   )  'This file of echoed input was generated by '//TRIM(ProgName)//' '//TRIM(ProgVer)// &
                            ' on '//CurDate()//' at '//CurTime()//'.'
   WRITE (UnEc,'(/,A,/)' )  'Turbine input data from file "'//TRIM( PriFile )//'":'
   WRITE (UnEc,'(2X,A,/)')  FTitle
   WRITE (UnEc,'(2X,A)'  )  Comment
   WRITE (UnEc,Frmt      )  Echo, 'Echo', 'Echo input to "echo.out"'
ENDIF


   ! ADAMSPrep - ADAMS preprocossor mode.

CALL ReadIVar ( UnIn, PriFile, ADAMSPrep, 'ADAMSPrep', 'ADAMS preprocessor mode' )

IF ( Cmpl4SFun .AND. ( ADAMSPrep /= 1 ) )  THEN
!bjj start of proposed change for Simulink
!rm   CALL ProgAbort ( ' An ADAMS dataset can''t be built when FAST is interfaced with Simulink.'// &
!rm                '  Set ADAMSPrep to 1 or use the standard version of FAST.'                    )
   CALL ProgWarn ( ' An ADAMS dataset can''t be built when FAST is interfaced with Simulink. ADAMSPrep is being set to 1.')
   ADAMSPrep = 1
!bjj end of proposed change
ELSEIF ( ( ADAMSPrep < 1 ) .OR. ( ADAMSPrep > 3 ) )  THEN
   CALL ProgAbort ( ' ADAMSPrep must be 1, 2, or 3.' )
ENDIF


   ! AnalMode - FAST analysis mode.

CALL ReadIVar ( UnIn, PriFile, AnalMode, 'AnalMode', 'Analysis mode' )

IF ( Cmpl4SFun .AND. ( AnalMode /= 1 ) )  THEN
   CALL ProgAbort ( ' FAST can''t linearize the model when interfaced with Simulink.'// &
                '  Set AnalMode to 1 or use the standard version of FAST.'            )
ELSEIF ( ( AnalMode < 1 ) .OR. ( AnalMode > 2 ) )  THEN
   CALL ProgAbort ( ' AnalMode must be 1 or 2.' )
ENDIF


   ! NumBl - Number of blades.

CALL ReadIVar ( UnIn, PriFile, NumBl, 'NumBl', 'Number of blades' )

IF ( ( NumBl < 2 ) .OR. ( NumBl > 3 ) )  CALL ProgAbort ( ' NumBl must be either 2 or 3.' )


   ! TMax - Total run time.

CALL ReadRVar ( UnIn, PriFile, TMax, 'TMax', 'Total run time' )

!jmj Start of proposed change.  v6.02c-jmj  02-Feb-2007.
!jmj Add an upper limit of 9999.999 to the input parameter TMax in order to
!jmj   avoid an overflow problem in the output file:
!remove6.02cIF ( TMax < 0.0    )  CALL ProgAbort ( ' TMax must not be less than 0.' )
IF ( ( TMax < 0.0 ) .OR. ( TMax > 9999.999 ) )  CALL ProgAbort ( ' TMax must be between 0.0 and 9999.999 (inclusive).' )
!jmj End of proposed change.  v6.02c-jmj  02-Feb-2007.

   ! DT - Integration time step.

CALL ReadRVar ( UnIn, PriFile, DT, 'DT', 'Integration time step' )

IF ( DT <= 0.0 )  CALL ProgAbort ( ' DT must be greater than 0.' )
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Test DT and TMax to ensure numerical stability:
IF ( DT <= TMax*EPSILON(DT) )  CALL ProgAbort ( ' DT must be greater than '//TRIM ( Flt2Lstr( TMax*EPSILON(DT) ) )//' seconds.' ) ! Test DT and TMax to ensure numerical stability -- HINT: see the use of OnePlusEps.
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


!-------------------------- TURBINE CONTROL PARAMETERS -------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'Turbine control parameters', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'Turbine control parameters' )
!bjj End of proposed change


   ! YCMode - Yaw control mode.

CALL ReadIVar ( UnIn, PriFile, YCMode, 'YCMode', 'Yaw control mode' )

IF ( ( .NOT. Cmpl4SFun ) .AND. ( YCMode == 2 ) )  THEN
   CALL ProgAbort ( ' YCMode can only equal 2 when FAST is interfaced with Simulink.'// &
                '  Set YCMode to 0 or 1 or interface FAST with Simulink.'             )
ELSEIF ( ( YCMode < 0 ) .OR. ( YCMode > 2 ) )  THEN
   CALL ProgAbort ( ' YCMode must be 0, 1, or 2.' )
ENDIF


   ! TYCOn - Time to enable yaw control.

CALL ReadRVar ( UnIn, PriFile, TYCOn, 'TYCOn', 'Time to enable yaw control' )

IF ( Cmpl4SFun .AND. ( YCMode == 2 ) .AND. ( TYCOn /= 0.0 ) )  THEN
   CALL ProgAbort ( ' Yaw control must be enabled at time zero when implemented in Simulink.'//      &
                '  Set TYCon to 0.0, set YCMode to 0 or 1, or use the standard version of FAST.'   )
ELSEIF ( TYCOn < 0.0 )  THEN
   CALL ProgAbort ( ' TYCOn must not be negative.' )
ENDIF


   ! PCMode - Pitch control mode.

CALL ReadIVar ( UnIn, PriFile, PCMode, 'PCMode', 'Pitch control mode' )

IF ( ( .NOT. Cmpl4SFun ) .AND. ( PCMode == 2 ) )  THEN
   CALL ProgAbort ( ' PCMode can only equal 2 when FAST is interfaced with Simulink.'// &
                '  Set PCMode to 0 or 1 or interface FAST with Simulink.'             )
ELSEIF ( ( PCMode < 0 ) .OR. ( PCMode > 2 ) )  THEN
   CALL ProgAbort ( ' PCMode must be 0, 1, or 2.' )
ENDIF


   ! TPCOn - Time to enable pitch control.

CALL ReadRVar ( UnIn, PriFile, TPCOn, 'TPCOn', 'Time to enable pitch control' )

IF ( Cmpl4SFun .AND. ( PCMode == 2 ) .AND. ( TPCOn /= 0.0 ) )  THEN
   CALL ProgAbort ( ' Pitch control must be enabled at time zero when implemented in Simulink.'//    &
                '  Set TPCon to 0.0, set PCMode to 0 or 1, or use the standard version of FAST.'   )
ELSEIF ( TPCOn < 0.0 )  THEN
   CALL ProgAbort ( ' TPCOn must not be negative.' )
ENDIF


   ! VSContrl - Variable-speed-generator control switch.

CALL ReadIVar ( UnIn, PriFile, VSContrl, 'VSContrl', 'Variable-speed-generator control switch' )

IF ( ( .NOT. Cmpl4SFun ) .AND. ( VSContrl == 3 ) )  THEN
   CALL ProgAbort ( ' VSContrl can only equal 3 when FAST is interfaced with Simulink.'// &
                '  Set VSContrl to 0, 1, or 2 or interface FAST with Simulink.'         )
ELSEIF ( ( VSContrl < 0 ) .OR. ( VSContrl > 3 ) )  THEN
   CALL ProgAbort ( ' VSContrl must be either 0, 1, 2, or 3.' )
ENDIF


   ! VS_RtGnSp - Rated generator speed for simple variable-speed generator control.

CALL ReadRVar ( UnIn, PriFile, VS_RtGnSp, 'VS_RtGnSp', 'Rated generator speed for simple variable-speed generator control' )

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Modify the simple variable speed control parameters to ensure that:
!jmj    (1) VS_RtGnSp cannot equal zero and
!jmj    (2) VS_TrGnSp equals VS_SySp when VS_Rgn2K equals zero.
!remove6.02aIF ( ( VSContrl == 1 ) .AND. ( VS_RtGnSp < 0.0 ) )  CALL Abort ( ' VS_RtGnSp must not be negative.' )
IF ( ( VSContrl == 1 ) .AND. ( VS_RtGnSp <= 0.0 ) )  CALL ProgAbort ( ' VS_RtGnSp must be greater than zero.' )
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! VS_RtTq - Rated generator torque/constant generator torque in Region 3 for simple variable-speed generator control.

CALL ReadRVar ( UnIn, PriFile, VS_RtTq, 'VS_RtTq', &
   'Rated generator torque/constant generator torque in Region 3 for simple variable-speed generator control' )

IF ( ( VSContrl == 1 ) .AND. ( VS_RtTq < 0.0 ) )  CALL ProgAbort ( ' VS_RtTq must not be negative.' )


   ! VS_Rgn2K - Generator torque constant in Region 2 for simple variable-speed generator control.

CALL ReadRVar ( UnIn, PriFile, VS_Rgn2K, 'VS_Rgn2K', &
   'Generator torque constant in Region 2 for simple variable-speed generator control' )

IF ( ( VSContrl == 1 ) .AND. ( VS_Rgn2K < 0.0 ) )  CALL ProgAbort ( ' VS_Rgn2K must not be negative.' )
IF ( ( VSContrl == 1 ) .AND. ( VS_Rgn2K*VS_RtGnSp*VS_RtGnSp >  VS_RtTq ) )  &
   CALL ProgAbort ( ' VS_Rgn2K*VS_RtGnSp^2 must not be greater than VS_RtTq.' )


   ! VS_SlPc - Rated generator slip percentage in Region 2 1/2 for simple variable-speed generator control.

CALL ReadRVar ( UnIn, PriFile, VS_SlPc, 'VS_SlPc', &
   'Rated generator slip percentage in Region 2 1/2 for simple variable-speed generator control' )

IF ( ( VSContrl == 1 ) .AND. ( VS_SlPc <= 0.0 ) )  CALL ProgAbort ( ' VS_SlPc must be greater than zero.' )

   ! GenModel - Generator model.

CALL ReadIVar ( UnIn, PriFile, GenModel, 'GenModel', 'Generator model' )

IF ( ( GenModel < 1 ) .OR. ( GenModel > 3 ) )  CALL ProgAbort ( ' GenModel must be either 1, 2, or 3.' )


   ! GenTiStr - Start generator based upon T: time or F: generator speed.

CALL ReadLVar ( UnIn, PriFile, GenTiStr, 'GenTiStr', 'Start generator based upon T: time or F: generator speed' )

IF ( Cmpl4SFun .AND. ( VSContrl == 3 ) .AND. ( .NOT. GenTiStr ) )  &
   CALL ProgAbort ( ' Variable-speed, generator torque control must be enabled at time zero when implemented in Simulink.'//       &
                '  Set GenTiStr to True and TimGenOn to 0.0, set VSContrl to 0, 1, or 2, or use the standard version of FAST.'   )


   ! GenTiStp - Stop generator based upon T: time or F: generator power = 0.

CALL ReadLVar ( UnIn, PriFile, GenTiStp, 'GenTiStp', 'Stop generator based upon T: time or F: generator power = 0' )

IF ( Cmpl4SFun .AND. ( VSContrl == 3 ) .AND. ( .NOT. GenTiStp ) )  &
   CALL ProgAbort ( ' Variable-speed, generator torque control must not be disabled during simulation when'//                      &
                ' implemented in Simulink.'//                                                                                  &
                '  Set GenTiStp to True and TimGenOf > TMax, set VSContrl to 0, 1, or 2, or use the standard version of FAST.'   )


   ! SpdGenOn - Generator speed to turn on the generator for a startup.

CALL ReadRVar ( UnIn, PriFile, SpdGenOn, 'SpdGenOn', 'Generator speed to turn on the generator' )

IF ( SpdGenOn < 0.0 )  CALL ProgAbort ( ' SpdGenOn must not be negative.' )


   ! TimGenOn - Time to turn on generator for startup.

CALL ReadRVar ( UnIn, PriFile, TimGenOn, 'TimGenOn', 'Time to turn on generator' )

IF ( Cmpl4SFun .AND. ( VSContrl == 3 ) .AND. ( TimGenOn /= 0.0 ) )  THEN
   CALL ProgAbort ( ' Variable-speed, generator torque control must be enabled at time zero when implemented in Simulink.'//       &
                '  Set GenTiStr to True and TimGenOn to 0.0, set VSContrl to 0, 1, or 2, or use the standard version of FAST.'   )
ELSEIF ( TimGenOn < 0.0 )  THEN
   CALL ProgAbort ( ' TimGenOn must not be negative.' )
ENDIF


   ! TimGenOf - Time to turn off generator for braking or modeling a run-away.

CALL ReadRVar ( UnIn, PriFile, TimGenOf, 'TimGenOf', 'Time to turn off generator' )

IF ( Cmpl4SFun .AND. ( VSContrl == 3 ) .AND. ( TimGenOf <= TMax ) )  THEN
   CALL ProgAbort ( ' Variable-speed, generator torque control must not be disabled during simulation when'//                      &
                ' implemented in Simulink.'//                                                                                  &
                '  Set GenTiStp to True and TimGenOf > TMax, set VSContrl to 0, 1, or 2, or use the standard version of FAST.'   )
ELSEIF ( TimGenOf < 0.0 )  THEN
   CALL ProgAbort ( ' TimGenOf must not be negative.' )
ENDIF


   ! HSSBrMode - HSS brake model.

CALL ReadIVar ( UnIn, PriFile, HSSBrMode, 'HSSBrMode', 'HSS brake model' )

IF ( ( HSSBrMode < 1 ) .OR. ( HSSBrMode > 2 ) )  CALL ProgAbort ( ' HSSBrMode must be 1 or 2.' )


   ! THSSBrDp - Time to initiate deployment of the HSS brake.

CALL ReadRVar ( UnIn, PriFile, THSSBrDp, 'THSSBrDp', 'Time to initiate deployment of the HSS brake' )

IF ( Cmpl4SFun .AND. ( THSSBrDp <= TMax ) )  THEN
   CALL ProgAbort ( ' A high-speed shaft brake shutdown event can''t be initiated when FAST is interfaced with Simulink.'// &
                '  Set THSSBrDp > TMax or use the standard version of FAST.'                                              )
ELSEIF ( THSSBrDp < 0.0 )  THEN
   CALL ProgAbort ( ' THSSBrDp must not be negative.' )
ENDIF


   ! TiDynBrk - Time to initiate deployment of the dynamic generator brake.

!JASON: ADD LOGIC FOR THIS NEW VARIABLE:
!JASON:CALL ReadRVar ( UnIn, PriFile, TiDynBrk, 'TiDynBrk', 'Time to initiate deployment of the dynamic generator brake' )
!JASON:
!JASON:IF ( TiDynBrk < 0.0 )  CALL ProgAbort ( ' TiDynBrk must not be negative.' )
!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'currently ignored TiDynBrk', Echo  ) !JASON:
   CALL ReadCom ( UnIn, PriFile, 'currently ignored TiDynBrk' )
!bjj End of proposed change


   ! TTpBrDp - Time to initiate deployment of tip brakes.

ALLOCATE ( TTpBrDp(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TTpBrDp array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, TTpBrDp, NumBl, 'TTpBrDp', 'Time to initiate deployment of tip brakes' )

DO K=1,NumBl
   IF ( TTpBrDp(K) < 0.0   )  THEN
      CALL ProgAbort ( ' TTpBrDp('//TRIM( Int2LStr( K ) )//') must not be negative.' )
   ENDIF
ENDDO ! K

IF ( NumBl == 2 )  THEN
!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'unused TTpBrDp(3)', .FALSE. )
   CALL ReadCom ( UnIn, PriFile, 'unused TTpBrDp(3)' )
!bjj End of proposed change
ENDIF


   ! TBDepISp - Deployment-initiation speed for the tip brakes.

ALLOCATE ( TBDepISp(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TBDepISp array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, TBDepISp, NumBl, 'TBDepISp', 'Deployment-initiation speed for the tip brakes' )

DO K=1,NumBl
   IF ( TBDepISp(K) < 0.0 )  THEN
      CALL ProgAbort ( ' TBDepISp('//TRIM( Int2LStr( K ) )//') must not be negative.' )
   ENDIF
ENDDO ! K

IF ( NumBl == 2 )  THEN
!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'unused TBDepISp(3)', .FALSE. )
   CALL ReadCom ( UnIn, PriFile, 'unused TBDepISp(3)' )
!bjj End of proposed change
ENDIF


   ! TYawManS - Time to start yaw maneuver.

CALL ReadRVar ( UnIn, PriFile, TYawManS, 'TYawManS', 'Time to start yaw maneuver' )

IF ( TYawManS < 0.0 )  CALL ProgAbort ( ' TYawManS must not be negative.' )


   ! TYawManE - Time to end yaw maneuver.

CALL ReadRVar ( UnIn, PriFile, TYawManE, 'TYawManE', 'Time to end yaw maneuver' )

IF ( TYawManE < TYawManS )  CALL ProgAbort ( ' TYawManE must not be less than TYawManS.' )


   ! NacYawF - Final nacelle-yaw angle for maneuvers.

CALL ReadRVar ( UnIn, PriFile, NacYawF, 'NacYawF', 'Final nacelle-yaw angle for maneuvers' )


   ! TPitManS - Time to start pitch maneuvers.

ALLOCATE ( TPitManS(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TPitManS array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, TPitManS, NumBl, 'TPitManS', 'Time to start pitch maneuvers' )

DO K=1,NumBl
   IF ( TPitManS(K) < 0.0 )  CALL ProgAbort ( ' TPitManS('//TRIM( Int2LStr( K ) )//') must not be negative.' )
ENDDO ! K

IF ( NumBl == 2 )  THEN
!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'unused TPitManS(3)', .FALSE. )
   CALL ReadCom ( UnIn, PriFile, 'unused TPitManS(3)' )
!bjj End of proposed change
ENDIF


   ! TPitManE - Time to end pitch maneuvers.

ALLOCATE ( TPitManE(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TPitManE array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, TPitManE, NumBl, 'TPitManE', 'Time to end pitch maneuvers' )

DO K=1,NumBl
   IF ( TPitManE(K) <TPitManS(K) )  &
      CALL ProgAbort ( ' TPitManE('//TRIM( Int2LStr( K ) )//') must not be less than TPitManS('//TRIM( Int2LStr( K ) )//').' )
ENDDO ! K

IF ( NumBl == 2 )  THEN
!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'unused TPitManE(3)', .FALSE. )
   CALL ReadCom ( UnIn, PriFile, 'unused TPitManE(3)' )
!bjj End of proposed change
ENDIF


   ! BlPitch - Initial pitch angle.

ALLOCATE ( BlPitch(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BlPitch array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, BlPitch, NumBl, 'BlPitch', 'Initial pitch angle' )

DO K=1,NumBl
   IF ( ( BlPitch(K) <= -180.0 ) .OR. ( BlPitch(K) > 180.0 ) )  THEN
      CALL ProgAbort ( ' BlPitch('//TRIM( Int2LStr( K ) )//') must be greater than -180 and less than or equal to 180.' )
   ENDIF
ENDDO ! K

IF ( NumBl == 2 )  THEN
!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'unused BlPitch(3)', .FALSE. )
   CALL ReadCom ( UnIn, PriFile, 'unused BlPitch(3)' )
!bjj End of proposed change
ENDIF


   ! BlPitchF - Final pitch angle for maneuvers.

ALLOCATE ( BlPitchF(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BlPitchF array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, BlPitchF, NumBl, 'BlPitchF', 'Final pitch angle for maneuvers' )

IF ( NumBl == 2 )  THEN
!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'unused BlPitchF(3)', .FALSE. )
   CALL ReadCom ( UnIn, PriFile, 'unused BlPitchF(3)' )
!bjj End of proposed change
ENDIF



!  ------- ENVIRONMENTAL CONDITIONS --------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'environmental conditions', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'environmental conditions' )
!bjj End of proposed change


   ! Gravity - Gravitational acceleration.

CALL ReadRVar ( UnIn, PriFile, Gravity, 'Gravity', 'Gravitational acceleration' )

IF ( Gravity < 0.0 )  CALL ProgAbort ( ' Gravity must not be negative.' )



!  -------------- FEATURE SWITCHES ---------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm  CALL SkipComment ( UnIn, PriFile, 'degree of freedom switches', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'degree of freedom switches' )
!bjj End of proposed change


   ! FlapDOF1 - First flapwise blade mode DOF.

CALL ReadLVar ( UnIn, PriFile, FlapDOF1, 'FlapDOF1', 'First flapwise blade mode DOF' )


   ! FlapDOF2 - Second flapwise blade mode DOF.

CALL ReadLVar ( UnIn, PriFile, FlapDOF2, 'FlapDOF2', 'Second flapwise blade mode DOF' )

IF ( FlapDOF2 .AND. ( .NOT. FlapDOF1 ) )  THEN  ! Print out warning when flap mode 1 is not enabled and flap mode 2 is enabled
   CALL UsrAlarm

   CALL WrScr1(' WARNING: ')
   CALL WrScr ('  2nd blade flap mode is enabled without the 1st. ')
   CALL WrScr ('  This designation is only recommended for debugging purposes. ')
ENDIF


   ! EdgeDOF - First edgewise blade mode DOF.

CALL ReadLVar ( UnIn, PriFile, EdgeDOF, 'EdgeDOF', 'First edgewise blade mode DOF' )


   ! TeetDOF - Teeter DOF.

IF ( NumBl == 2 )  THEN
   CALL ReadLVar ( UnIn, PriFile, TeetDOF, 'TeetDOF', 'Teeter DOF' )
ELSE
!bjj Start of proposed change vXX NWTC_Lib
!rm  CALL SkipComment ( UnIn, PriFile, 'unused TeetDOF', .FALSE. )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetDOF' )
!bjj End of proposed change
ENDIF


   ! DrTrDOF - Drivetrain rotational-flexibility DOF.

CALL ReadLVar ( UnIn, PriFile, DrTrDOF, 'DrTrDOF', 'Drivetrain rotational-flexibility DOF' )


   ! GenDOF - Generator DOF.

CALL ReadLVar ( UnIn, PriFile, GenDOF, 'GenDOF', 'Generator DOF' )


   ! YawDOF - Yaw DOF.

CALL ReadLVar ( UnIn, PriFile, YawDOF, 'YawDOF', 'Yaw DOF' )


   ! TwFADOF1 - First tower fore-aft bending-mode DOF.

CALL ReadLVar ( UnIn, PriFile, TwFADOF1, 'TwFADOF1', 'First tower fore-aft bending-mode DOF' )


   ! TwFADOF2 - Second tower fore-aft bending-mode DOF.

CALL ReadLVar ( UnIn, PriFile, TwFADOF2, 'TwFADOF2', 'Second tower fore-aft bending-mode DOF' )

IF ( TwFADOF2 .AND. ( .NOT. TwFADOF1 ) )  THEN  ! Print out warning when tower fore-aft mode 1 is not enabled and fore-aft mode 2 is enabled
   CALL UsrAlarm

   CALL WrScr1(' WARNING: ')
   CALL WrScr ('  2nd tower fore-aft mode is enabled without the 1st. ')
   CALL WrScr ('  This designation is only recommended for debugging purposes. ')
ENDIF


   ! TwSSDOF1 - First tower side-to-side bending-mode DOF.

CALL ReadLVar ( UnIn, PriFile, TwSSDOF1, 'TwSSDOF1', 'First tower side-to-side bending-mode DOF' )


   ! TwSSDOF2 - Second tower side-to-side bending-mode DOF.

CALL ReadLVar ( UnIn, PriFile, TwSSDOF2, 'TwSSDOF2', 'Second tower side-to-side bending-mode DOF' )

IF ( TwSSDOF2 .AND. ( .NOT. TwSSDOF1 ) )  THEN  ! Print out warning when tower side-to-side mode 1 is not enabled and side-to-side mode 2 is enabled
   CALL UsrAlarm

   CALL WrScr1(' WARNING: ')
   CALL WrScr ('  2nd tower side-to-side mode is enabled without the 1st. ')
   CALL WrScr ('  This designation is only recommended for debugging purposes. ')
ENDIF


   ! CompAero - Compute aerodynamic forces.

CALL ReadLVar ( UnIn, PriFile, CompAero, 'CompAero', 'Compute aerodynamic forces' )


   ! CompNoise - Compute aerodynamic noise.

CALL ReadLVar ( UnIn, PriFile, CompNoise, 'CompNoise', 'Compute aerodynamic noise' )

IF ( CompNoise .AND. ( .NOT. CompAero ) )  &
   CALL ProgAbort ( ' CompAero must be True if CompNoise is True.' )

IF ( CompNoise .AND. ( AnalMode == 2 ) )  THEN  ! Print out warning that noise will not be computed when linearizing FAST
   CALL WrScr1(' NOTE: Noise will not be computed during the FAST linearization process, even though CompNoise is enabled.')
ENDIF



!  -------------- INITIAL CONDITIONS -------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm  CALL SkipComment ( UnIn, PriFile, 'initial conditions', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'initial conditions' )
!bjj End of proposed change


   ! OoPDefl - Initial out-of-plane blade-tip deflection.

CALL ReadRVar ( UnIn, PriFile, OoPDefl, 'OoPDefl', 'Initial out-of-plane blade-tip deflection' )

IF ( Cmpl4SFun .AND. ( OoPDefl /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial out-of-plane blade-tip displacements must be zero when FAST is interfaced with Simulink.'// &
                '  Set OoPDefl to 0.0 or use the standard version of FAST.'                                             )


   ! IPDefl - Initial in-plane blade-tip deflection.

CALL ReadRVar ( UnIn, PriFile, IPDefl, 'IPDefl', 'Initial in-plane blade-tip deflection' )

IF ( Cmpl4SFun .AND. ( IPDefl  /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial in-plane blade-tip displacements must be zero when FAST is interfaced with Simulink.'// &
                '  Set IPDefl to 0.0 or use the standard version of FAST.'                                          )


   ! TeetDefl - Initial or fixed teeter angle.

IF ( NumBl == 2 )  THEN
   CALL ReadRVar ( UnIn, PriFile, TeetDefl, 'TeetDefl', 'Initial or fixed teeter angle' )
   IF ( ( TeetDefl <= -180.0 ) .OR. ( TeetDefl > 180.0 ) )  &
      CALL ProgAbort ( ' TeetDefl must be greater than -180 and less than or equal to 180.' )
ELSE
!bjj Start of proposed change vXX NWTC_Lib
!rm  CALL SkipComment ( UnIn, PriFile, 'unused Teeter', .FALSE. )
   CALL ReadCom ( UnIn, PriFile, 'unused Teeter' )
!bjj End of proposed change
ENDIF


   ! Azimuth - Initial azimuth position for blade 1.

CALL ReadRVar ( UnIn, PriFile, Azimuth, 'Azimuth', 'Initial azimuth position for blade 1' )

IF ( ( Azimuth < 0.0 ) .OR. ( Azimuth >= 360.0 ) )  &
   CALL ProgAbort ( ' Azimuth must be greater or equal to 0 and less than 360.' )


   ! RotSpeed - Initial or fixed rotor speed.

CALL ReadRVar ( UnIn, PriFile, RotSpeed, 'RotSpeed', 'Initial or fixed rotor speed' )

IF ( RotSpeed < 0.0 )  CALL ProgAbort ( ' RotSpeed must not be negative.' )


   ! NacYaw - Initial or fixed nacelle-yaw angle.

CALL ReadRVar ( UnIn, PriFile, NacYaw, 'NacYaw', 'Initial or fixed nacelle-yaw angle' )

IF ( ( NacYaw <= -180.0 ) .OR. ( NacYaw > 180.0 ) )  &
   CALL ProgAbort ( ' NacYaw must be greater than -180 and less than or equal to 180.' )


   ! TTDspFA - Initial fore-aft tower-top displacement.

CALL ReadRVar ( UnIn, PriFile, TTDspFA, 'TTDspFA', 'Initial fore-aft tower-top displacement' )

IF ( Cmpl4SFun .AND. ( TTDspFA /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial fore-aft tower-top displacements must be zero when FAST is interfaced with Simulink.'// &
                '  Set TTDspFA to 0.0 or use the standard version of FAST.'                                         )


   ! TTDspSS - Initial side-to-side tower-top displacement.

CALL ReadRVar ( UnIn, PriFile, TTDspSS, 'TTDspSS', 'Initial side-to-side tower-top displacement' )

IF ( Cmpl4SFun .AND. ( TTDspSS /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial side-to-side tower-top displacements must be zero when FAST is interfaced with Simulink.'// &
                '  Set TTDspSS to 0.0 or use the standard version of FAST.'                                             )



!  -------------- TURBINE CONFIGURATION ----------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm  CALL SkipComment ( UnIn, PriFile, 'turbine configuration', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'turbine configuration' )
!bjj End of proposed change


   ! TipRad - Preconed blade-tip radius.

CALL ReadRVar ( UnIn, PriFile, TipRad, 'TipRad', 'Preconed blade-tip radius' )

IF ( TipRad < 0.0 )  CALL ProgAbort ( ' TipRad must be greater than 0.' )


   ! HubRad - Preconed hub radius.

CALL ReadRVar ( UnIn, PriFile, HubRad, 'HubRad', 'Preconed hub radius' )

!BJJ start of proposed change v7.00.01a-bjj
!bjj this line is too long:
!rmIF ( ( HubRad < 0.0 ) .OR. ( HubRad >= TipRad ) )  CALL ProgAbort ( ' HubRad must be between 0 (inclusive) and TipRad (exclusive).' )
IF ( ( HubRad < 0.0 ) .OR. ( HubRad >= TipRad ) ) THEN
   CALL ProgAbort ( ' HubRad must be between 0 (inclusive) and TipRad (exclusive).' )
END IF
!bjj end of proposed change


   ! PSpnElN - Number of the innermost blade element which is still part of the pitchable portion of the blade for partial-span pitch control.

!JASON: ADD LOGIC FOR THIS NEW VARIABLE:
!JASON:CALL ReadIVar ( UnIn, PriFile, PSpnElN, 'PSpnElN', 'Partial-span pitch control element number' )
!bjj Start of proposed change vXX NWTC_Lib
!rm  CALL SkipComment ( UnIn, PriFile, 'currently ignored PSpnElN', Echo  )  !JASON:
   CALL ReadCom ( UnIn, PriFile, 'currently ignored PSpnElN' )
!bjj End of proposed change

   ! UndSling - Undersling length.

IF ( NumBl == 2 )  THEN
   CALL ReadRVar ( UnIn, PriFile, UndSling, 'UndSling', 'Undersling length' )
ELSE
!bjj Start of proposed change vXX NWTC_Lib
!rm  CALL SkipComment ( UnIn, PriFile, 'unused UndSling', .FALSE. )
   CALL ReadCom ( UnIn, PriFile, 'unused UndSling' )
!bjj End of proposed change
   UndSling = 0.0
ENDIF


   ! HubCM - Distance from rotor apex to hub mass.

CALL ReadRVar ( UnIn, PriFile, HubCM, 'HubCM', 'Distance from rotor apex to hub mass' )


   ! OverHang - Distance from yaw axis to rotor apex or teeter pin.

CALL ReadRVar ( UnIn, PriFile, OverHang, 'OverHang', 'Distance from yaw axis to rotor apex or teeter pin' )


   ! NacCMxn - Downwind distance from tower-top to nacelle CM.

CALL ReadRVar ( UnIn, PriFile, NacCMxn, 'NacCMxn', 'Downwind distance from tower-top to nacelle CM' )


   ! NacCMyn - Lateral  distance from tower-top to nacelle CM.

CALL ReadRVar ( UnIn, PriFile, NacCMyn, 'NacCMyn', 'Lateral  distance from tower-top to nacelle CM' )


   ! NacCMzn - Vertical distance from tower-top to nacelle CM.

CALL ReadRVar ( UnIn, PriFile, NacCMzn, 'NacCMzn', 'Vertical distance from tower-top to nacelle CM' )


   ! TowerHt - Tower height.

CALL ReadRVar ( UnIn, PriFile, TowerHt, 'TowerHt', 'Tower height' )

IF ( TowerHt <= 0.0 )  CALL ProgAbort ( ' TowerHt must be greater than zero.' )


   ! Twr2Shft - Vertical distance from tower-top rotor shaft.

CALL ReadRVar ( UnIn, PriFile, Twr2Shft, 'Twr2Shft', 'Vertical distance from tower-top to rotor shaft' )

IF ( Twr2Shft < 0.0 )  CALL ProgAbort ( ' Twr2Shft cannot be negative.' )


   ! TwrRBHt - Tower rigid base height.

CALL ReadRVar ( UnIn, PriFile, TwrRBHt, 'TwrRBHt', 'Tower rigid base height' )
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Check the value of TwrRBHt in routines GetPtfm() and GetPrimary() instead
!jmj   of in routine Input():

IF ( TwrRBHt < 0.0 )  CALL ProgAbort ( ' TwrRBHt must be greater or equal to 0 and less than TowerHt + TwrDraft.' )
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! ShftTilt - Rotor shaft tilt angle.

CALL ReadRVar ( UnIn, PriFile, ShftTilt, 'ShftTilt', 'Rotor shaft tilt angle' )

IF ( ( ShftTilt < -90.0 ) .OR. ( ShftTilt > 90.0 ) )              &
   CALL ProgAbort ( ' ShftTilt must be between -90 and 90 (inclusive).' )
IF ( TowerHt + Twr2Shft + OverHang*SIN(ShftTilt*D2R) <= TipRad )  &
   CALL ProgAbort ( ' TowerHt + Twr2Shft + OverHang*SIN(ShftTilt) must be greater than TipRad.' )


   ! Delta3 - Delta-3 angle for teetering rotors.

IF ( NumBl == 2 )  THEN
   CALL ReadRVar ( UnIn, PriFile, Delta3, 'Delta3', 'Delta-3 angle for teetering rotors' )
   IF ( ( Delta3 <= -90.0 ) .OR. ( Delta3 >= 90.0 ) )  CALL ProgAbort ( ' Delta3 must be between -90 and 90 (exclusive).' )
ELSE
!bjj Start of proposed change vXX NWTC_Lib
!rm  CALL SkipComment ( UnIn, PriFile, 'unused Delta3', .FALSE. )
   CALL ReadCom ( UnIn, PriFile, 'unused Delta3' )
!bjj End of proposed change
ENDIF


   ! PreCone - Blade coning angle.

ALLOCATE ( PreCone(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the PreCone array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, PreCone, NumBl, 'PreCone', 'Blade coning angle' )

DO K=1,NumBl
!bjj start of proposed change v6.02d-bjj
!rm   IF ( ( PreCone(K) <= -180.0 ) .OR. ( PreCone(K) > 180.0 ) )  THEN
!rm      CALL ProgAbort ( ' PreCone('//TRIM( Int2LStr( K ) )//') must be greater than -180 and less than or equal to 180.' )
!rm   ENDIF
   IF ( ( PreCone(K) <= -90.0 ) .OR. ( PreCone(K) >= 90.0 ) )  THEN
      CALL ProgAbort ( ' PreCone('//TRIM( Int2LStr( K ) )//') must be between -90 and 90 degrees (exclusive).' )
   ENDIF
!bjj end of proposed change
ENDDO ! K

IF ( NumBl == 2 )  THEN
!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'unused Beta(3)', .FALSE. )
   CALL ReadCom ( UnIn, PriFile, 'unused Beta(3)' )
!bjj End of proposed change
ENDIF


   ! AzimB1Up - Blade-tip radius.

CALL ReadRVar ( UnIn, PriFile, AzimB1Up, 'AzimB1Up', 'Blade-tip radius' )

IF ( ( AzimB1Up < 0.0 ) .OR. ( AzimB1Up > 360.0 ) )  CALL ProgAbort ( ' AzimB1Up must be between 0 and 360 (inclusive).' )



!  -------------- MASS AND INERTIA ---------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'mass and inertia', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'mass and inertia' )
!bjj End of proposed change


   ! YawBrMass - Yaw bearing mass.

CALL ReadRVar ( UnIn, PriFile, YawBrMass, 'YawBrMass', 'Yaw bearing mass' )

IF ( YawBrMass < 0.0 )  CALL ProgAbort ( ' YawBrMass must not be negative.' )


   ! NacMass - Nacelle mass.

CALL ReadRVar ( UnIn, PriFile, NacMass, 'NacMass', 'Nacelle mass' )

IF ( NacMass < 0.0 )  CALL ProgAbort ( ' NacMass must not be negative.' )


   ! HubMass - Hub mass.

CALL ReadRVar ( UnIn, PriFile, HubMass, 'HubMass', 'Hub mass' )

IF ( HubMass < 0.0 )  CALL ProgAbort ( ' HubMass must not be negative.' )


   ! TipMass - Tip-brake mass.

ALLOCATE ( TipMass(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TipMass array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, TipMass, NumBl, 'TipMass', 'Tip-brake mass' )

DO K=1,NumBl
   IF ( TipMass(K) < 0.0 )  THEN
      CALL ProgAbort ( ' TipMass('//TRIM( Int2LStr( K ) )//') must not be negative.' )
   ENDIF
ENDDO ! K

IF ( NumBl == 2 )  THEN
!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'unused TipMass(3)', .FALSE. )
   CALL ReadCom ( UnIn, PriFile, 'unused TipMass(3)' )
!bjj End of proposed change
ENDIF


   ! NacYIner - Nacelle yaw inertia.

CALL ReadRVar ( UnIn, PriFile, NacYIner, 'NacYIner', 'Nacelle yaw inertia' )

IF ( NacYIner < 0.0 )  CALL ProgAbort ( ' NacYIner must not be negative.' )


   ! GenIner - Generator inertia about HSS.

CALL ReadRVar ( UnIn, PriFile, GenIner, 'GenIner', 'Generator inertia about HSS' )

IF ( GenIner < 0.0 )  CALL ProgAbort ( ' GenIner must not be negative.' )


   ! HubIner - Hub inertia about teeter axis (2-blader) or rotor axis (3-blader).

IF ( NumBl == 2 )  THEN
   CALL ReadRVar ( UnIn, PriFile, HubIner, 'HubIner', 'Hub inertia about teeter axis' )
ELSE
   CALL ReadRVar ( UnIn, PriFile, HubIner, 'HubIner', 'Hub inertia about rotor axis' )
ENDIF

IF ( HubIner < 0.0 )  CALL ProgAbort ( ' HubIner must not be negative.' )



!  -------------- DRIVETRAIN PARAMETERS ----------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'drivetrain parameters', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'drivetrain parameters' )
!bjj End of proposed change


   ! GBoxEff - Gearbox efficiency.

CALL ReadRVar ( UnIn, PriFile, GBoxEff, 'GBoxEff', 'Gearbox efficiency' )

!jmj Start of proposed change.  v6.10d-jmj  13-Aug-2009.
!jmj Abort() when GBoxEff also equals 0.0 because GBoxEff can appear in the
!jmj   denominator:
!remove6.10dIF ( ( GBoxEff < 0.0 ) .OR. ( GBoxEff > 100.0 ) )  CALL Abort ( ' GBoxEff must be between 0 and 100 (inclusive).' )
!bjj start of proposed change v7.00.01a-bjj
!bjj this line is too long
!rmIF ( ( GBoxEff <= 0.0 ) .OR. ( GBoxEff > 100.0 ) )  CALL Abort ( ' GBoxEff must be greater than 0 and less than or equal to 100.' )
!rmIF ( ( GBoxEff <= 0.0 ) .OR. ( GBoxEff > 100.0 ) )  CALL ProgAbort ( ' GBoxEff must be greater than 0 and less than or equal to 100.' )
IF ( ( GBoxEff <= 0.0 ) .OR. ( GBoxEff > 100.0 ) ) THEN
   CALL ProgAbort ( ' GBoxEff must be greater than 0 and less than or equal to 100.' )
END IF   
!bjj end of proposed change v7.00.01a-bjj
!jmj End of proposed change.  v6.10d-jmj  13-Aug-2009.




   ! GenEff - Generator efficiency.

CALL ReadRVar ( UnIn, PriFile, GenEff, 'GenEff', 'Generator efficiency' )

IF ( ( GenEff < 0.0 ) .OR. ( GenEff > 100.0 ) )  CALL ProgAbort ( ' GenEff must be between 0 and 100 (inclusive).' )


   ! GBRatio - Gearbox ratio.

CALL ReadRVar ( UnIn, PriFile, GBRatio, 'GBRatio', 'Gearbox ratio' )

IF ( GBRatio <= 0.0 )  CALL ProgAbort ( ' GBRatio must be greater than 0.' )


!JASON: ELIMINATE THIS INPUT BY ALLOWING GBRatio TO BE NEGATIVE!!!!!<--ACTUALLY, DON'T DO THIS SINCE WE ALWAYS WANT THE HSS SPEED TO REMAIN POSITIVE AND THE TORQUE TO DEPEND ON WHETHER WE ARE PRODUCING POWER OR MOTORING UP.
   ! GBRevers - Gearbox reversal flag.

CALL ReadLVar ( UnIn, PriFile, GBRevers, 'GBRevers', 'Gearbox reversal flag' )


   ! HSSBrTqF - Fully deployed HSS brake torque.

CALL ReadRVar ( UnIn, PriFile, HSSBrTqF, 'HSSBrTqF', 'Fully deployed HSS brake torque' )

IF ( HSSBrTqF < 0.0 )  CALL ProgAbort ( ' HSSBrTqF must not be negative.' )


   ! HSSBrDT - Time for HSS-brake to reach full deployment once initiated.

CALL ReadRVar ( UnIn, PriFile, HSSBrDT, 'HSSBrDT', 'Time for HSS-brake to reach full deployment once initiated' )

IF ( HSSBrDT < 0.0 )  CALL ProgAbort ( ' HSSBrDT must not be negative.' )


   ! DynBrkFi - Name of file containing dynamic generator brake properties.

!JASON: ADD LOGIC FOR THIS NEW VARIABLE:
!JASON:CALL ReadCVar ( UnIn, PriFile, DynBrkFi, 'DynBrkFi', 'Name of file containing dynamic generator brake properties' )
!JASON:
!JASON:IF ( LEN_TRIM( DynBrkFi ) == 0 )  CALL ProgAbort ( ' DynBrkFi must not be an empty string.' )

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'currently ignored DynBrkFi', Echo  ) !JASON:
   CALL ReadCom ( UnIn, PriFile, 'currently ignored DynBrkFi' )
!bjj End of proposed change


   ! DTTorSpr - Drivetrain torsional spring.

CALL ReadRVar ( UnIn, PriFile, DTTorSpr, 'DTTorSpr', 'Drivetrain torsional spring' )

IF ( DTTorSpr < 0.0 )  CALL ProgAbort ( ' DTTorSpr must not be negative.' )


   ! DTTorDmp - Drivetrain torsional damper.

CALL ReadRVar ( UnIn, PriFile, DTTorDmp, 'DTTorDmp', 'Drivetrain torsional damper' )

IF ( DTTorDmp < 0.0 )  CALL ProgAbort ( ' DTTorDmp must not be negative.' )



!  -------------- SIMPLE-INDUCTION-GENERATOR PARAMETERS ------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'simple-induction-generator parameters', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'simple-induction-generator parameters' )
!bjj End of proposed change


   ! SIG_SlPc - Rated generator slip percentage.

CALL ReadRVar ( UnIn, PriFile, SIG_SlPc, 'SIG_SlPc', 'Rated generator slip percentage' )

IF ( ( GenModel == 1 )  .AND. ( SIG_SlPc <= 0.0 ) )  CALL ProgAbort ( ' SIG_SlPc must be greater than zero.' )


   ! SIG_SySp - Synchronous (zero-torque) generator speed.

CALL ReadRVar ( UnIn, PriFile, SIG_SySp, 'SIG_SySp', 'Synchronous (zero-torque) generator speed' )

IF ( ( GenModel == 1 )  .AND. ( SIG_SySp <= 0 ) )  CALL ProgAbort ( ' SIG_SySp must be greater than zero.' )


   ! SIG_RtTq - Rated torque.

CALL ReadRVar ( UnIn, PriFile, SIG_RtTq, 'SIG_RtTq', 'Rated torque' )

IF ( ( GenModel == 1 )  .AND. ( SIG_RtTq <= 0.0 ) )  CALL ProgAbort ( ' SIG_RtTq must be greater than zero.' )


   ! SIG_PORt - Pull-out ratio.

CALL ReadRVar ( UnIn, PriFile, SIG_PORt, 'SIG_PORt', 'Pull-out ratio' )

IF ( ( GenModel == 1 )  .AND. ( SIG_PORt < 1.0 ) )  CALL ProgAbort ( ' SIG_PORt must not be less than 1.' )



!  -------------- THEVENIN-EQUIVALENT INDUCTION-GENERATOR PARAMETERS -----------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'Thevenin-equivalent induction-generator parameters', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'Thevenin-equivalent induction-generator parameters' )
!bjj End of proposed change


   ! TEC_Freq - Line frequency.

CALL ReadRVar ( UnIn, PriFile, TEC_Freq, 'TEC_Freq', 'Line frequency' )

IF ( ( GenModel == 2 )  .AND. ( TEC_Freq <= 0.0 ) )  CALL ProgAbort ( ' TEC_Freq must be greater than zero.' )


   ! TEC_NPol - Number of poles.

CALL ReadIVar ( UnIn, PriFile, TEC_NPol, 'TEC_NPol', 'Number of poles' )

IF ( ( GenModel == 2 )  .AND. ( ( TEC_NPol <= 0 ) .OR. ( MOD( TEC_NPol, 2 ) /= 0 ) ) )  &
     CALL ProgAbort ( ' TEC_NPol must be an even number greater than zero.' )


   ! TEC_SRes - Stator resistance.

CALL ReadRVar ( UnIn, PriFile, TEC_SRes, 'TEC_SRes', 'Stator resistance' )

IF ( ( GenModel == 2 )  .AND. ( TEC_SRes <= 0.0 ) )  CALL ProgAbort ( ' TEC_SRes must be greater than zero.' )


   ! TEC_RRes - Rotor resistance.

CALL ReadRVar ( UnIn, PriFile, TEC_RRes, 'TEC_RRes', 'Rotor resistance' )

IF ( ( GenModel == 2 )  .AND. ( TEC_RRes <= 0.0 ) )  CALL ProgAbort ( ' TEC_RRes must be greater than zero.' )


   ! TEC_VLL - Line-to-line RMS voltage.

CALL ReadRVar ( UnIn, PriFile, TEC_VLL, 'TEC_VLL', 'Line-to-line RMS voltage' )

IF ( ( GenModel == 2 )  .AND. ( TEC_VLL <= 0.0 ) )  CALL ProgAbort ( ' TEC_VLL must be greater than zero.' )


   ! TEC_SLR - Stator leakage reactance.

CALL ReadRVar ( UnIn, PriFile, TEC_SLR, 'TEC_SLR', 'Stator leakage reactance' )

IF ( ( GenModel == 2 )  .AND. ( TEC_SLR <= 0.0 ) )  CALL ProgAbort ( ' TEC_SLR must be greater than zero.' )


   ! TEC_RLR  - Rotor leakage reactance.

CALL ReadRVar ( UnIn, PriFile, TEC_RLR, 'TEC_RLR', 'Rotor leakage reactance' )

IF ( ( GenModel == 2 )  .AND. ( TEC_RLR <= 0.0 ) )  CALL ProgAbort ( ' TEC_RLR must be greater than zero.' )


   ! TEC_MR - Magnetizing reactance.

CALL ReadRVar ( UnIn, PriFile, TEC_MR, 'TEC_MR', 'Magnetizing reactance' )

IF ( ( GenModel == 2 )  .AND. ( TEC_MR <= 0.0 ) )  CALL ProgAbort ( ' TEC_MR must be greater than zero.' )


!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Rename both the "PLATFORM MODEL" and "PLATFORM LOADING" sections of the
!jmj   input files to "PLATFORM":
!remove6.02a!  -------------- PLATFORM MODEL PARAMETERS ------------------------------------
!  -------------- PLATFORM PARAMETERS ------------------------------------------
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! Skip the comment line.

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Rename both the "PLATFORM MODEL" and "PLATFORM LOADING" sections of the
!jmj   input files to "PLATFORM":
!remove6.02aCALL SkipComment ( UnIn, PriFile, 'platform model parameters', Echo  )
!rm bjj:        CALL ReadCom ( UnIn, PriFile, 'platform model parameters' )
CALL ReadCom ( UnIn, PriFile, 'platform parameters'  )
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

   ! PtfmModel - Platform model switch.

CALL ReadIVar ( UnIn, PriFile, PtfmModel, 'PtfmModel', 'Platform model switch' )

IF ( ( PtfmModel < 0 ) .OR. ( PtfmModel > 3 ) )  CALL ProgAbort ( ' PtfmModel must be either 0, 1, 2, or 3.' )


   ! PtfmFile - Name of file containing platform properties.

CALL ReadCVar ( UnIn, PriFile, PtfmFile, 'PtfmFile', 'Name of file containing platform properties' )

!bjj start of proposed change v6.02d-bjj
!rmIF ( LEN_TRIM( PtfmFile ) == 0 )  CALL ProgAbort ( ' PtfmFile must not be an empty string.' ) 
IF ( LEN_TRIM( PtfmFile ) == 0 .AND. PtfmModel /= 0 )  CALL ProgAbort ( ' PtfmFile must not be an empty string.' ) 
!bjj end of proposed change



!  -------------- TOWER PARAMETERS ---------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'tower parameters', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'tower parameters' )
!bjj End of proposed change

   ! TwrNodes - Number of tower nodes used for analysis.

CALL ReadIVar ( UnIn, PriFile, TwrNodes, 'TwrNodes', 'Number of tower nodes used for analysis' )

IF ( TwrNodes < 1 )  CALL ProgAbort ( ' TwrNodes must not be less than 1.' )


   ! TwrFile - Name of file containing tower properties.

CALL ReadCVar ( UnIn, PriFile, TwrFile, 'TwrFile', 'Name of file containing tower properties' )

IF ( LEN_TRIM( TwrFile ) == 0 )  CALL ProgAbort ( ' TwrFile must not be an empty string.' )



!  -------------- NACELLE-YAW PARAMETERS ---------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'nacelle-yaw parameters', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'nacelle-yaw parameters' )
!bjj End of proposed change


   ! YawSpr - Yaw spring constant.

CALL ReadRVar ( UnIn, PriFile, YawSpr, 'YawSpr', 'Nacelle-yaw spring constant' )

IF ( YawSpr < 0.0 )  CALL ProgAbort ( ' YawSpr must not be negative.' )


   ! YawDamp - Yaw damping constant.

CALL ReadRVar ( UnIn, PriFile, YawDamp, 'YawDamp', 'Nacelle-yaw damping constant' )

IF ( YawDamp < 0.0 )  CALL ProgAbort ( ' YawDamp must not be negative.' )


   ! YawNeut - Neutral yaw position.

CALL ReadRVar ( UnIn, PriFile, YawNeut, 'YawNeut', 'Neutral yaw position' )

IF ( ( YawNeut <= -180.0 ) .OR. ( YawNeut > 180.0 ) )  &
   CALL ProgAbort ( ' YawNeut must be greater than -180 and less than or equal to 180.' )



!  -------------- FURLING PARAMETERS -------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'furling parameters', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'furling parameters' )
!bjj End of proposed change

   ! Furling - Read in additional model properties for furling turbine.

CALL ReadLVar ( UnIn, PriFile, Furling, 'Furling', 'Read in additional model properties for furling turbine' )

IF ( Furling .AND. ( OverHang > 0.0 ) )  THEN   ! Print out warning when downwind turbine is modeled with furling.
   CALL UsrAlarm

   CALL WrScr1(' WARNING: ')
   CALL WrScr ('  Furling designation (Furling = True) specified for downwind rotor configuration (OverHang > 0). ')
   CALL WrScr ('  Check for possible errors in the input file(s). ')
ENDIF


   ! FurlFile - Name of file containing furling properties.

CALL ReadCVar ( UnIn, PriFile, FurlFile, 'FurlFile', 'Name of file containing furling properties' )

!BJJ start of proposed change v6.02d-bjj
!rmIF ( LEN_TRIM( FurlFile ) == 0 )  CALL ProgAbort ( ' FurlFile must not be an empty string.' )
IF ( LEN_TRIM( FurlFile ) == 0 .AND. Furling )  CALL ProgAbort ( ' FurlFile must not be an empty string.' )
!bjj end of proposed change


!  -------------- ROTOR-TEETER PARAMETERS --------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'rotor-teeter parameters', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'rotor-teeter parameters' )
!bjj End of proposed change


IF ( NumBl == 2 )  THEN

      ! TeetMod - Rotor-teeter spring/damper model switch.

   CALL ReadIVar ( UnIn, PriFile, TeetMod, 'TeetMod', 'Rotor-teeter spring/damper model switch' )

   IF ( ( TeetMod /= 0 ) .AND. ( TeetMod /= 1 ) .AND. ( TeetMod /= 2 ) )  CALL ProgAbort ( ' TeetMod must be 0, 1, or 2.' )


      ! TeetDmpP - Rotor-teeter damper position.

   CALL ReadRVar ( UnIn, PriFile, TeetDmpP, 'TeetDmpP', 'Rotor-teeter damper position' )

   IF ( ( TeetDmpP < 0.0 ) .OR. ( TeetDmpP > 180.0 ) )  CALL ProgAbort ( ' TeetDmpP must be between 0 and 180 (inclusive).' )


      ! TeetDmp - Rotor-teeter damping constant.

   CALL ReadRVar ( UnIn, PriFile, TeetDmp, 'TeetDmp', 'Rotor-teeter damping constant' )

   IF ( TeetDmp < 0.0 )  CALL ProgAbort ( ' TeetDmp must not be negative.' )


      ! TeetCDmp - Rotor-teeter rate-independent Coulomb-damping moment.

   CALL ReadRVar ( UnIn, PriFile, TeetCDmp, 'TeetCDmp', 'Rotor-teeter rate-independent Coulomb-damping moment' )

   IF ( TeetCDmp < 0.0 )  CALL ProgAbort ( ' TeetCDmp must not be negative.' )


      ! TeetSStP - Rotor-teeter soft-stop position.

   CALL ReadRVar ( UnIn, PriFile, TeetSStP, 'TeetSStP', 'Rotor-teeter soft-stop position' )

   IF ( ( TeetSStP < 0.0 ) .OR. ( TeetSStP > 180.0 ) )  CALL ProgAbort ( ' TeetSStP must be between 0 and 180 (inclusive).' )


      ! TeetHStP - Rotor-teeter hard-stop position.

   CALL ReadRVar ( UnIn, PriFile, TeetHStP, 'TeetHStP', 'Rotor-teeter hard-stop position' )

   IF ( ( TeetHStP < TeetSStP ) .OR. ( TeetHStP > 180.0 ) )  &
      CALL ProgAbort ( ' TeetHStP must be between TeetSStP  and 180 (inclusive).' )


      ! TeetSSSp - Rotor-teeter soft-stop linear-spring constant.

   CALL ReadRVar ( UnIn, PriFile, TeetSSSp, 'TeetSSSp', 'Rotor-teeter soft-stop linear-spring constant' )

   IF ( TeetSSSp < 0.0 )  CALL ProgAbort ( ' TeetSSSp must not be negative.' )


      ! TeetHSSp - Rotor-teeter hard-stop linear-spring constant.

   CALL ReadRVar ( UnIn, PriFile, TeetHSSp, 'TeetHSSp', 'Rotor-teeter hard-stop linear-spring constant' )

   IF ( TeetHSSp < 0.0 )  CALL ProgAbort ( ' TeetHSSp must not be negative.' )

ELSE


      ! Three-bladed turbines don't use these parameters, so skip them.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'unused TeetMod' , .FALSE. )
!rm   CALL SkipComment ( UnIn, PriFile, 'unused TeetDmpP', .FALSE. )
!rm   CALL SkipComment ( UnIn, PriFile, 'unused TeetDmp' , .FALSE. )
!rm   CALL SkipComment ( UnIn, PriFile, 'unused TeetCDmp', .FALSE. )
!rm   CALL SkipComment ( UnIn, PriFile, 'unused TeetSStP', .FALSE. )
!rm   CALL SkipComment ( UnIn, PriFile, 'unused TeetHStP', .FALSE. )
!rm   CALL SkipComment ( UnIn, PriFile, 'unused TeetSSSp', .FALSE. )
!rm   CALL SkipComment ( UnIn, PriFile, 'unused TeetHSSp', .FALSE. )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetMod'  )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetDmpP' )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetDmp'  )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetCDmp' )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetSStP' )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetHStP' )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetSSSp' )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetHSSp' )
!bjj End of proposed change

ENDIF



!  -------------- TIP-BRAKE PARAMETERS -----------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'tower parameters', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'tip-brake parameters' )
!bjj End of proposed change


   ! TBDrConN - Tip-brake drag constant during normal operation.

CALL ReadRVar ( UnIn, PriFile, TBDrConN, 'TBDrConN', 'Tip-brake drag constant during normal operation' )

IF ( TBDrConN < 0.0 )  CALL ProgAbort ( ' TBDrConN must not be negative.' )


   ! TBDrConD - Tip-brake drag constant during fully-deployed operation.

CALL ReadRVar ( UnIn, PriFile, TBDrConD, 'TBDrConD', 'Tip-brake drag constant during fully-deployed operation' )

IF ( TBDrConD < TBDrConN )  CALL ProgAbort( ' TBDrConD must not be less than TBDrConN.' )


   ! TpBrDT - Time for tip-brake to reach full deployment once released.

CALL ReadRVar ( UnIn, PriFile, TpBrDT, 'TpBrDT', 'Time for tip-brake to reach full deployment once released' )

IF ( TpBrDT < 0.0 )  CALL ProgAbort ( ' TpBrDT must not be negative.' )


!  -------------- BLADE PARAMETERS ---------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'blade parameters', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'blade parameters' )
!bjj End of proposed change


   ! BldFile - Names of files containing blade properties.

ALLOCATE ( BldFile(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BldFile array.' )
ENDIF

!bjj start of proposed change vXX NWTC_Library
!rmCALL ReadCAry( UnIn, PriFile, BldFile, NumBl, 'BldFile', 'Names of files containing blade properties' )
CALL ReadAryLines( UnIn, PriFile, BldFile, NumBl, 'BldFile', 'Names of files containing blade properties' )
!bjj end of proposed change

DO K=1,NumBl
   IF ( LEN_TRIM( BldFile(K) ) == 0 )  THEN
      CALL ProgAbort ( 'BldFile('//TRIM( Int2LStr( K ) )//') must not be an empty string.' )
   ENDIF
ENDDO ! K

IF ( NumBl == 2 )  THEN
!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'unused BldFile(3)', .FALSE. )
   CALL ReadCom ( UnIn, PriFile, 'unused BldFile(3)' )
!bjj End of proposed change
ENDIF



!  -------------- AERODYN INPUT FILE PARAMETERS -------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'AeroDyn parameters', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'AeroDyn parameters' )
!bjj End of proposed change


   ! ADFile - Name of file containing AeroDyn parameters.

CALL ReadCVar( UnIn, PriFile, ADFile, 'ADFile', 'Name of file containing AeroDyn parameters' )

IF ( LEN_TRIM( ADFile ) == 0 )  CALL ProgAbort ( 'ADFile must not be an empty string.' )



!  -------------- NOISE INPUT FILE PARAMETERS --------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'Noise parameters', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'Noise parameters' )
!bjj End of proposed change


   ! NoiseFile - Name of file containing aerodynamic noise parameters.

CALL ReadCVar ( UnIn, PriFile, NoiseFile, 'NoiseFile', 'Name of file containing aerodynamic noise parameters' )

!BJJ start of proposed change v6.02d-bjj
!rmIF ( LEN_TRIM( NoiseFile ) == 0 )  CALL ProgAbort ( ' NoiseFile must not be an empty string.' )
IF ( LEN_TRIM( NoiseFile ) == 0 .AND. CompNoise)  CALL ProgAbort ( ' NoiseFile must not be an empty string.' )
!bjj end of proposed change


!  -------------- ADAMS INPUT FILE PARAMETERS ----------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'ADAMS parameters', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'ADAMS parameters' )
!bjj End of proposed change

   ! ADAMSFile - Name of file containing ADAMS-specific parameters.

CALL ReadCVar ( UnIn, PriFile, ADAMSFile, 'ADAMSFile', 'Name of file containing ADAMS-specific properties' )

!BJJ start of proposed change v6.02d-bjj
!RMIF ( LEN_TRIM( ADAMSFile ) == 0 )  CALL ProgAbort ( ' ADAMSFile must not be an empty string.' )
IF ( LEN_TRIM( ADAMSFile ) == 0 .AND. ADAMSPrep /= 1)  CALL ProgAbort ( ' ADAMSFile must not be an empty string.' )
!END start of proposed change v6.02d-bjj



!  -------------- FAST LINEARIZATION CONTROL PARAMETERS ------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'Linearization parameters', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'Linearization parameters' )
!bjj End of proposed change


   ! LinFile - Name of file containing FAST linearization parameters.

CALL ReadCVar ( UnIn, PriFile, LinFile, 'LinFile', 'Name of file containing FAST linearization parameters' )

!BJJ start of proposed change v6.02d-bjj
!rmIF ( LEN_TRIM( LinFile ) == 0 )  CALL ProgAbort ( ' LinFile must not be an empty string.' )
IF ( LEN_TRIM( LinFile ) == 0 .AND. AnalMode /= 1)  CALL ProgAbort ( ' LinFile must not be an empty string.' )
!end of proposed change v6.02d-bjj



!  -------------- OUTPUT PARAMETERS --------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile,  'output-parameters', Echo  )
   CALL ReadCom ( UnIn, PriFile, 'output parameters' )
!bjj End of proposed change


   ! SumPrint - Print summary data to "*.fsm".

CALL ReadLVar ( UnIn, PriFile, SumPrint, 'SumPrint', 'Print summary data to "*.fsm"' )


   ! TabDelim - Generate a tab-delimited output file.

CALL ReadLVar ( UnIn, PriFile, TabDelim, 'TabDelim', 'Generate a tab-delimited output file' )


   ! OutFmt - Output format for tabular data.

CALL ReadCVar ( UnIn, PriFile, OutFmt, 'OutFmt', 'Output format for tabular data' )

IF ( LEN_TRIM( OutFmt ) == 0 )  CALL ProgAbort ( ' OutFmt must not be an empty string.' )


   ! TStart - Time to start tabular output.

CALL ReadRVar ( UnIn, PriFile, TStart, 'TStart', 'Time to begin tabular output' )

IF ( TStart < 0.0  )  CALL ProgAbort ( ' TStart must not be less than 0.' )
IF ( TMax < TStart )  CALL ProgAbort ( ' TMax must not be less than TStart.' )


   ! DecFact - Decimation factor for tabular output.

CALL ReadIVar ( UnIn, PriFile, DecFact, 'DecFact', 'Decimation factor for tabular output' )

IF ( DecFact < 1 )  CALL ProgAbort ( ' DecFact must be greater than 0.' )


   ! SttsTime - Amount of time between screen status messages.

CALL ReadRVar ( UnIn, PriFile, SttsTime, 'SttsTime', 'Amount of time between screen status messages' )

IF ( SttsTime <= 0.0 )  CALL ProgAbort ( ' SttsTime must be greater than 0.' )


   ! NcIMUxn - Downwind distance from the tower-top to the nacelle IMU.

CALL ReadRVar ( UnIn, PriFile, NcIMUxn, 'NcIMUxn', 'Downwind distance from the tower-top to the nacelle IMU' )


   ! NcIMUyn - Lateral distance from the tower-top to the nacelle IMU.

CALL ReadRVar ( UnIn, PriFile, NcIMUyn, 'NcIMUyn', 'Lateral distance from the tower-top to the nacelle IMU' )


   ! NcIMUzn - Vertical distance from the tower-top to the nacelle IMU.

CALL ReadRVar ( UnIn, PriFile, NcIMUzn, 'NcIMUzn', 'Vertical distance from the tower-top to the nacelle IMU' )


   ! ShftGagL - Distance from hub or teeter pin to shaft strain gages.

CALL ReadRVar ( UnIn, PriFile, ShftGagL, 'ShftGagL', 'Distance from hub or teeter pin to shaft strain gages' )


   ! NTwGages - Number of tower "strain-gage" output stations.

CALL ReadIVar ( UnIn, PriFile, NTwGages, 'NTwGages', 'Number of tower "strain-gage" output stations' )

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:
!remove6.02aIF ( ( NTwGages < 0 ) .OR. ( NTwGages > 5 ) )  CALL ProgAbort ( ' NTwGages must be between 0 and 5 (inclusive).' )
IF ( ( NTwGages < 0 ) .OR. ( NTwGages > 9 ) )  CALL ProgAbort ( ' NTwGages must be between 0 and 9 (inclusive).' )
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! TwrGagNd - List of tower nodes that have strain gages.

READ (UnIn,*,IOSTAT=IOS)  ( TwrGagNd(I), I=1,NTwGages )

IF ( IOS < 0 )  THEN
   CALL PremEOF ( PriFile , 'TwrGagNd' )
ELSEIF ( IOS > 0 )  THEN
   CALL WrScr1 ( ' Invalid numerical input for file "'//TRIM( PriFile )//'.' )
   CALL ProgAbort  ( ' The error occurred while trying to read the TwrGagNd array.' )
ENDIF

IF ( Echo )  THEN
   WRITE (UnEc,"(15X,A,T27,' - ',A)")  'TwrGagNd', 'List of tower nodes that have strain gages'
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:
!remove6.02a   WRITE (UnEc,'(5(I4,:))')  ( TwrGagNd(I), I=1,NTwGages )
   WRITE (UnEc,'(9(I4,:))')  ( TwrGagNd(I), I=1,NTwGages )
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
ENDIF


   ! NBlGages - Number of blade "strain-gage" output stations.

CALL ReadIVar ( UnIn, PriFile, NBlGages, 'NBlGages', 'Number of blade "strain-gage" output stations' )

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:
!remove6.02aIF ( ( NBlGages < 0 ) .OR. ( NBlGages > 5 ) )  CALL ProgAbort ( ' NBlGages must be between 0 and 5 (inclusive).' )
IF ( ( NBlGages < 0 ) .OR. ( NBlGages > 9 ) )  CALL ProgAbort ( ' NBlGages must be between 0 and 9 (inclusive).' )
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! BldGagNd - List of blade nodes that have strain gages.

READ (UnIn,*,IOSTAT=IOS)  ( BldGagNd(I), I=1,NBlGages )

IF ( IOS < 0 )  THEN
   CALL PremEOF ( PriFile , 'BldGagNd' )
ELSEIF ( IOS > 0 )  THEN
   CALL WrScr1 ( ' Invalid numerical input for file "'//TRIM( PriFile )//'.' )
   CALL ProgAbort  ( ' The error occurred while trying to read the BldGagNd array.' )
ENDIF

IF ( Echo )  THEN
   WRITE (UnEc,"(15X,A,T27,' - ',A)")  'BldGagNd', 'List of blade nodes that have strain gages'
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:
!remove6.02a   WRITE (UnEc,'(5(I4,:))')  ( BldGagNd(I), I=1,NBlGages )
   WRITE (UnEc,'(9(I4,:))')  ( BldGagNd(I), I=1,NBlGages )
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
ENDIF


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PriFile, 'output-parameters-list', .FALSE.  )
   CALL ReadCom ( UnIn, PriFile, 'output-parameters list' )
!bjj End of proposed change


   ! OutList - Output parameter list.

OutList = ''   ! Initialize OutList(:) to ''.
NumOuts = 0    ! Initialize NumOuts to zero.


   ! Lets read in all of the lines containing output parameters and store them in OutList(:).
   ! The end of this list (and the end of the output file) is specified with the line
   !    beginning with END.

DO

   CALL ReadCVar ( UnIn, PriFile, OutLine, 'OutList', 'Output list' )

   EndOfFile = OutLine(1:3)            ! EndOfFile is the 1st 3 characters of OutLine
   CALL Conv2UC( EndOfFile )           ! Convert EndOfFile to upper case
   IF ( EndOfFile == 'END' )  EXIT     ! End of OutList has been reached; therefore, exit this DO

   NumWords = CountWords( OutLine )    ! The number of words in OutLine.

   NumOuts = NumOuts + NumWords        ! The total number of output channels read in so far.
   IF ( NumOuts > MaxOutPts )  &       ! Check to see if the maximum # of allowable outputs has been reached.
      CALL ProgAbort ( ' The maximum number of output channels allowed is ' &
                   //TRIM( Int2LStr(MaxOutPts) )//'.'                     )

   CALL GetWords ( OutLine, OutList(NumOuts - NumWords + 1), NumWords )

ENDDO


   ! Check to make sure some outputs have been entered when time-marhcing;
   !   if not, ProgAbort:

IF ( ( NumOuts == 0 ) .AND. ( AnalMode == 1 ) )  THEN
   CALL ProgAbort ( ' No output channels specified!' )
ENDIF

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Move the CALL to SUBROUTINE FAST_IO.f90/ChckOutLst() from
!jmj   FAST_IO.f90/GetPrimary() to FAST_IO.f90/Input():
!remove6.02a
!remove6.02a   ! Check to see if any inputted output channels are ill-conditioned (and if so, Abort)
!remove6.02a   !    and set values for OutInd(:) and OutParam(:):
!remove6.02a
!remove6.02aCALL ChckOutLst
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! Close primary input file.

CLOSE ( UnIn )


RETURN
END SUBROUTINE GetPrimary
!=======================================================================
SUBROUTINE GetPtfm

   ! This routine reads in the FAST platform input parameters from
   !   PtfmFile and validates the input.

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
USE                             Constants
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
USE                             EnvCond
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
USE                             Features
USE                             General
USE                             InitCond
USE                             MassInert
USE                             Output
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
!jmj Also, rename MODULE PlatformLd() to Platform():
!remove6.02aUSE                             PlatformLd
USE                             Platform
USE                             SimCont
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
!bjj rm NWTC_Library: USE                             SysSubs
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Check the value of TwrRBHt in routines GetPtfm() and GetPrimary() instead
!jmj   of in routine Input():
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
USE                             Tower
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
USE                             TurbConf
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
USE                             Waves, ONLY:WavePkShpDefault
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


IMPLICIT                        NONE


   ! Local variables:
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
REAL(ReKi)                   :: LAngAnch                                        ! Azimuth angle   of the current anchor   relative to the positive xi-axis of the inertial frame.
REAL(ReKi)                   :: LAngFair                                        ! Azimuth angle   of the current fairlead relative to the positive xt-axis of the platform.
REAL(ReKi)                   :: LDpthAnch                                       ! Depth           of the current anchor   relative to the origin           of the inertial frame.
REAL(ReKi)                   :: LDrftFair                                       ! Draft           of the current fairlead relative to the platform reference point.
REAL(ReKi)                   :: LRadAnch                                        ! Radial distance of the current anchor   relative to the origin           of the inertial frame.
REAL(ReKi)                   :: LRadFair                                        ! Radial distance of the current fairlead relative to the platform reference point.

!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
INTEGER(4)                   :: I                                               ! A generic index.
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

INTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
INTEGER(4)                   :: Sttus                                           ! Status returned by an attempted allocation.
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
CHARACTER(80)                :: Line                                            ! String to temporarily hold the value of PtfmLdMod.
CHARACTER(80)                :: LineUC                                          ! String to temporarily hold the value of PtfmLdMod in upper case.
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
CHARACTER(156)               :: Frmt                                            ! Format for element data.
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.


   ! Open the FAST platform input file:

!BJJ:
CALL OpenFInpFile ( UnIn, PtfmFile )


   ! Add a separator to the echo file if appropriate.

IF ( Echo )  WRITE (UnEc,'(//,A,/)')  'Platform input data from file "'//TRIM( PtfmFile )//'":'



!  -------------- HEADER -------------------------------------------------------


   ! Skip the header.

READ (UnIn,'(//)',IOSTAT=IOS)

IF ( IOS < 0 )  THEN
   CALL PremEOF ( PtfmFile , 'unused FAST platform-file header' )
ENDIF



!  -------------- FEATURE SWITCHES (CONT) --------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PtfmFile, 'degree of freedom switches (cont)', Echo  )
   CALL ReadCom ( UnIn, PtfmFile, 'degree of freedom switches (cont)' )
!bjj End of proposed change



   ! PtfmSgDOF - Platform horizontal surge translation DOF.

CALL ReadLVar ( UnIn, PtfmFile, PtfmSgDOF, 'PtfmSgDOF', 'Platform surge DOF' )


   ! PtfmSwDOF - Platform horizontal sway translation DOF.

CALL ReadLVar ( UnIn, PtfmFile, PtfmSwDOF, 'PtfmSwDOF', 'Platform sway DOF' )


   ! PtfmHvDOF - Platform vertical heave translation DOF.

CALL ReadLVar ( UnIn, PtfmFile, PtfmHvDOF, 'PtfmHvDOF', 'Platform heave DOF' )


   ! PtfmRDOF - Platform roll tilt rotation DOF.

CALL ReadLVar ( UnIn, PtfmFile, PtfmRDOF, 'PtfmRDOF', 'Platform roll DOF' )


   ! PtfmPDOF - Platform pitch tilt rotation DOF.

CALL ReadLVar ( UnIn, PtfmFile, PtfmPDOF, 'PtfmPDOF', 'Platform pitch DOF' )


   ! PtfmYDOF - Platform yaw rotation DOF.

CALL ReadLVar ( UnIn, PtfmFile, PtfmYDOF, 'PtfmYDOF', 'Platform yaw DOF' )



!  -------------- INITIAL CONDITIONS (CONT) ------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PtfmFile, 'initial conditions (cont)', Echo  )
   CALL ReadCom ( UnIn, PtfmFile, 'initial conditions (cont)' )
!bjj End of proposed change


   ! PtfmSurge - Initial or fixed horizontal surge translational displacement of platform.

CALL ReadRVar ( UnIn, PtfmFile, PtfmSurge, 'PtfmSurge', 'Initial or fixed platform surge' )


   ! PtfmSway - Initial or fixed horizontal sway translational displacement of platform.

CALL ReadRVar ( UnIn, PtfmFile, PtfmSway, 'PtfmSway', 'Initial or fixed platform sway' )


   ! PtfmHeave - Initial or fixed vertical heave translational displacement of platform.

CALL ReadRVar ( UnIn, PtfmFile, PtfmHeave, 'PtfmHeave', 'Initial or fixed platform heave' )


   ! PtfmRoll - Initial or fixed roll tilt rotational displacement of platform.

CALL ReadRVar ( UnIn, PtfmFile, PtfmRoll, 'PtfmRoll', 'Initial or fixed platform roll' )

IF ( ( PtfmRoll < -15.0 ) .OR. ( PtfmRoll > 15.0 ) )  &
   CALL ProgAbort ( ' PtfmRoll must be between -15 and 15 (inclusive).' )


   ! PtfmPitch - Initial or fixed pitch tilt rotational displacement of platform.

CALL ReadRVar ( UnIn, PtfmFile, PtfmPitch, 'PtfmPitch', 'Initial or fixed platform pitch' )

IF ( ( PtfmPitch < -15.0 ) .OR. ( PtfmPitch > 15.0 ) )  &
   CALL ProgAbort ( ' PtfmPitch must be between -15 and 15 (inclusive).' )


   ! PtfmYaw - Initial or fixed yaw rotational displacement of platform.

CALL ReadRVar ( UnIn, PtfmFile, PtfmYaw, 'PtfmYaw', 'Initial or fixed platform yaw' )

IF ( ( PtfmYaw < -15.0 ) .OR. ( PtfmYaw > 15.0 ) )  &
   CALL ProgAbort ( ' PtfmYaw must be between -15 and 15 (inclusive).' )



!  -------------- TURBINE CONFIGURATION (CONT) ---------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PtfmFile, 'turbine configuration (cont)', Echo  )
   CALL ReadCom ( UnIn, PtfmFile, 'turbine configuration (cont)' )
!bjj End of proposed change


   ! TwrDraft - Downward distance from the ground [onshore] or MSL [offshore] to the tower base platform connection.

CALL ReadRVar ( UnIn, PtfmFile, TwrDraft, 'TwrDraft', &
   'Downward distance from ground [onshore] or MSL [offshore] to tower base platform connection' )

IF ( TwrDraft <= -TowerHt )  CALL ProgAbort ( ' TwrDraft must be greater than -TowerHt.' )
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Check the value of TwrRBHt in routines GetPtfm() and GetPrimary() instead
!jmj   of in routine Input():

IF ( TwrRBHt >= ( TowerHt + TwrDraft ) )  &
         CALL ProgAbort ( ' TwrRBHt must be greater or equal to 0 and less than TowerHt + TwrDraft.' )
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! PtfmCM - Downward distance from the ground [onshore] or MSL [offshore] to the platform CM.

CALL ReadRVar ( UnIn, PtfmFile, PtfmCM, 'PtfmCM', &
   'Downward distance from ground [onshore] or MSL [offshore] to platform CM' )

IF ( PtfmCM < TwrDraft )  CALL ProgAbort ( ' PtfmCM must not be less than TwrDraft.' )


   ! PtfmRef - Downward distance from the ground [onshore] or MSL [offshore] to the platform reference point.

CALL ReadRVar ( UnIn, PtfmFile, PtfmRef, 'PtfmRef', &
   'Downward distance from ground [onshore] or MSL [offshore] to platform reference point' )

IF ( PtfmRef < TwrDraft )  CALL ProgAbort ( ' PtfmRef must not be less than TwrDraft.' )



!  -------------- MASS AND INERTIA (CONT) --------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, PtfmFile, 'mass and inertia (cont)', Echo  )
   CALL ReadCom ( UnIn, PtfmFile, 'mass and inertia (cont)' )
!bjj End of proposed change



   ! PtfmMass - Platform mass.

CALL ReadRVar ( UnIn, PtfmFile, PtfmMass, 'PtfmMass', 'Platform mass' )

IF ( PtfmMass < 0.0 )  CALL ProgAbort ( ' PtfmMass must not be negative.' )


   ! PtfmRIner - Platform inertia for roll tilt rotation about the platform CM.

CALL ReadRVar ( UnIn, PtfmFile, PtfmRIner, 'PtfmRIner', 'Platform inertia for roll tilt rotation about the platform CM' )

IF ( PtfmRIner < 0.0 )  CALL ProgAbort ( ' PtfmRIner must not be negative.' )


   ! PtfmPIner - Platform inertia for pitch tilt rotation about the platform CM.

CALL ReadRVar ( UnIn, PtfmFile, PtfmPIner, 'PtfmPIner', 'Platform inertia for pitch tilt rotation about the platform CM' )

IF ( PtfmPIner < 0.0 )  CALL ProgAbort ( ' PtfmPIner must not be negative.' )


   ! PtfmYIner - Platform inertia for yaw rotation about the platform CM.

CALL ReadRVar ( UnIn, PtfmFile, PtfmYIner, 'PtfmYIner', 'Platform inertia for yaw rotation about the platform CM' )

IF ( PtfmYIner < 0.0 )  CALL ProgAbort ( ' PtfmYIner must not be negative.' )


!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Rename both the "PLATFORM MODEL" and "PLATFORM LOADING" sections of the
!jmj   input files to "PLATFORM":
!remove6.02a!  -------------- PLATFORM LOADING ---------------------------------------------
!  -------------- PLATFORM (CONT) ----------------------------------------------
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! Skip the comment line.

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Rename both the "PLATFORM MODEL" and "PLATFORM LOADING" sections of the
!jmj   input files to "PLATFORM":
!remove6.02aCALL SkipComment ( UnIn, PtfmFile, 'platform loading', Echo  )
!rm bjj:   CALL ReadCom ( UnIn, PtfmFile, 'platform loading' )
CALL ReadCom ( UnIn, PtfmFile, 'platform (cont)'  )
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


!JASON: MARSHALL WANTS PLATFORM/TOWER HYDRODYNAMICS TO WORK LIKE AERODYNAMICS IN AeroDyn.  THIS MEANS HAVING A COMPLETELY SEPARATE CODE FOR DOING THE HYDRODYNAMICS CALCULATIONS (CALL IT HydroDyn).  THIS MEANS THAT THE REST OF THESE PARAMETERS (EXCLUDING THE KINEMATICS/KINETICS PARAMERERS NEEDED BY FAST / ADAMS) SHOULD BE IN THEIR OWN FILE AND SOURCE CODE.  MAKE THIS CHANGE BEFORE YOU DOCUMENT THESE ROUTINES!!!!  DO THE SAME THING WITH THE MOORING SYSTEM (CALL IT LineDyn!)
!JASON: ONCE CompHydro BECOMES AN INPUT TO THE PROGRAM, USE CompHydro THROUGHOUT RtHS() LIKE CompAero!!!!
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
!remove6.02a   ! PtfmLdMod - Platform loading model switch.
!remove6.02a
!remove6.02aCALL ReadIVar ( UnIn, PtfmFile, PtfmLdMod, 'PtfmLdMod', 'Platform loading model switch' )
!remove6.02a
!remove6.02aIF ( ( PtfmLdMod /= 0 ) .AND. ( PtfmLdMod /= 1 ) )  CALL ProgAbort ( ' PtfmLdMod must be 0 or 1.' )
SELECT CASE ( PtfmModel )  ! Which platform model are we using?

CASE ( 1 )                 ! Onshore.




   ! PtfmLdMod - Platform loading model switch.

   CALL ReadIVar ( UnIn, PtfmFile, PtfmLdMod, 'PtfmLdMod', 'Platform loading model switch' )

   IF ( ( PtfmLdMod /= 0 ) .AND. ( PtfmLdMod /= 1 ) )  CALL ProgAbort ( ' PtfmLdMod must be 0 or 1.' )




CASE ( 2 )                 ! Fixed bottom offshore.




   ! PtfmLdMod - Platform loading model switch.

   CALL ReadIVar ( UnIn, PtfmFile, PtfmLdMod, 'PtfmLdMod', 'Platform loading model switch' )

   IF ( ( PtfmLdMod /= 0 ) .AND. ( PtfmLdMod /= 1 ) )  CALL ProgAbort ( ' PtfmLdMod must be 0 or 1.' )


   ! Read in additional inputs only if we are using one of the undocumented
   !   features:

   READ (UnIn,'(A80)',IOSTAT=IOS)  Line

   IF ( IOS == 0 )  THEN   ! .TRUE. if there was no error reading the Line.

      IF ( Line(1:80) == '---------------------- TOWER (CONT) --------------------------------------------' )  THEN  ! .TRUE. if we are using the undocumented monopile feature


   ! Enable the undocumented hydrodynamic loading routines:

         CompHydro = .TRUE.



!  -------------- TOWER (CONT) -------------------------------------------------

         IF ( Echo )  WRITE (UnEc,'(A)')  &
            '  ---------------------- TOWER (CONT) --------------------------------------------                    '


   ! TwrLdMod - Tower loading model switch.

         CALL ReadIVar ( UnIn, PtfmFile, TwrLdMod, 'TwrLdMod', 'Tower loading model switch' )

         IF ( ( TwrLdMod /= 0 ) .AND. ( TwrLdMod /= 1 ) .AND. ( TwrLdMod /= 2 ) )  THEN
            CALL ProgAbort ( ' TwrLdMod must be 0, 1, or 2.' )
         END IF


         IF ( TwrLdMod == 1 )  THEN ! .TRUE if we will be using the built-in Morison's equation.


   ! TwrDiam - Tower diameter in Morison's equation.

            CALL ReadRVar ( UnIn, PtfmFile, TwrDiam, 'TwrDiam', 'Tower diameter in Morison''s equation' )

            IF ( TwrDiam < 0.0 )  CALL ProgAbort ( ' TwrDiam must not be negative.' )


   ! TwrCA - Normalized hydrodynamic added mass coefficient in Morison's equation.

            CALL ReadRVar ( UnIn, PtfmFile, TwrCA, 'TwrCA', &
               'Normalized hydrodynamic added mass coefficient in Morison''s equation' )

            IF ( TwrCA < 0.0 )  CALL ProgAbort ( ' TwrCA must not be negative.' )


   ! TwrCD - Normalized hydrodynamic viscous drag coefficient in Morison's equation.

            CALL ReadRVar ( UnIn, PtfmFile, TwrCD, 'TwrCD', &
               'Normalized hydrodynamic viscous drag coefficient in Morison''s equation' )

            IF ( TwrCD < 0.0 )  CALL ProgAbort ( ' TwrCD must not be negative.' )


         ELSE                       ! We must not be using the built-in Morison's equation, so skip these inputs.


            CALL ReadCom ( UnIn, PtfmFile, 'unused TwrDiam')
            CALL ReadCom ( UnIn, PtfmFile, 'unused TwrCA'  )
            CALL ReadCom ( UnIn, PtfmFile, 'unused TwrCD'  )


         ENDIF



!  -------------- WAVES --------------------------------------------------------


   ! Skip the comment line.

         CALL ReadCom ( UnIn, PtfmFile, 'waves' )


!JASON: MOVE THIS INPUT, WtrDens, TO AN ENVIRONMENTAL CONDITIONS SECTION OF THE INPUT FILE WHEN YOU DOCUMENT THIS NEW FEATURE!!!!
   ! WtrDens - Water density.

         CALL ReadRVar ( UnIn, PtfmFile, WtrDens, 'WtrDens', 'Water density' )

         IF ( WtrDens < 0.0 )  CALL ProgAbort ( ' WtrDens must not be negative.' )


   ! WtrDpth - Water depth.

         CALL ReadRVar ( UnIn, PtfmFile, WtrDpth, 'WtrDpth', 'Water depth' )

         IF ( WtrDpth <= 0.0 )  CALL ProgAbort ( ' WtrDpth must be greater than zero.' )

!JASON: WHAT LOADING DO WE APPLY TO THE FLEXIBLE PORTION OF THE TOWER EXTENDING BELOW THE SEABED?
         IF ( ( TwrDraft - TwrRBHt ) < WtrDpth )  THEN   ! Print out a warning when the flexible portion of the support structure does not extend to the seabed.
            CALL ProgWarn( ' Hydrodynamic loading will only be applied to the flexible portion of the support structure.'// &
                           ' Make sure that ( TwrDraft - TwrRBHt ) >= WtrDpth if you want hydrodynamic loading applied'// &
                           ' along the entire submerged portion of the support structure. ')
         ENDIF


   ! WaveMod - Wave kinematics model switch.

         CALL ReadIVar ( UnIn, PtfmFile, WaveMod, 'WaveMod', 'Wave kinematics model switch' )

         IF ( ( WaveMod /= 0 ) .AND. ( WaveMod /= 1 ) .AND. ( WaveMod /= 2 ) .AND. ( WaveMod /= 3 ) .AND. ( WaveMod /= 4 ) )  &
            CALL ProgAbort ( ' WaveMod must be 0, 1, 2, 3, or 4.' )


         IF ( WaveMod > 0 )  THEN   ! .TRUE if we have incident waves.


   ! WaveStMod - Model switch for stretching incident wave kinematics to instantaneous free surface.

            CALL ReadIVar ( UnIn, PtfmFile, WaveStMod, 'WaveStMod', &
               'Model switch for stretching incident wave kinematics to instantaneous free surface' )

            IF ( ( WaveStMod /= 0 ) .AND. ( WaveStMod /= 1 ) .AND. ( WaveStMod /= 2 ) .AND. ( WaveStMod /= 3 ) )  &
               CALL ProgAbort ( ' WaveStMod must be 0, 1, 2, or 3.' )

            IF ( ( WaveStMod /= 3 ) .AND. ( WaveMod == 4 ) )  &
               CALL ProgAbort ( ' WaveStMod must be set to 3 when WaveMod is set to 4.' )


   ! WaveTMax - Analysis time for incident wave calculations.

            CALL ReadRVar ( UnIn, PtfmFile, WaveTMax, 'WaveTMax', 'Analysis time for incident wave calculations' )

            IF ( WaveTMax < TMax )  CALL ProgAbort ( ' WaveTMax must not be less than TMax.' )


   ! WaveDT - Time step for incident wave calculations.

            CALL ReadRVar ( UnIn, PtfmFile, WaveDT, 'WaveDT', 'Time step for incident wave calculations' )

            IF ( WaveDT <= 0.0 )  CALL ProgAbort ( ' WaveDT must be greater than zero.' )


         ELSE                       ! We must not have incident waves, so skip these inputs.


            CALL ReadCom ( UnIn, PtfmFile, 'unused WaveStMod' )
            CALL ReadCom ( UnIn, PtfmFile, 'unused WaveTMax'  )
            CALL ReadCom ( UnIn, PtfmFile, 'unused WaveDT'    )


         ENDIF


         IF ( ( WaveMod > 0 ) .AND. ( WaveMod /= 4 ) )  THEN   ! .TRUE if we have incident waves, but not GH Bladed wave data.


   ! WaveHs - Significant wave height.

            IF ( ( WaveMod == 1 ) .OR. ( WaveMod == 2 ) )  THEN   ! .TRUE. for plane progressive (regular) or JONSWAP/Pierson-Moskowitz spectrum (irregular) wave

               CALL ReadRVar ( UnIn, PtfmFile, WaveHs, 'WaveHs', 'Significant wave height' )

               IF ( WaveHs <= 0.0 )  CALL ProgAbort ( ' WaveHs must be greater than zero.' )

            ELSE                                                  ! User-defined spectrum (irregular) wave

               CALL ReadCom ( UnIn, PtfmFile, 'unused WaveHs' )

            ENDIF


   ! WaveTp - Peak spectral period.

            IF ( ( WaveMod == 1 ) .OR. ( WaveMod == 2 ) )  THEN   ! .TRUE. for plane progressive (regular) or JONSWAP/Pierson-Moskowitz spectrum (irregular) wave

               CALL ReadRVar ( UnIn, PtfmFile, WaveTp, 'WaveTp', 'Peak spectral period' )

               IF ( WaveTp <= 0.0 )  CALL ProgAbort ( ' WaveTp must be greater than zero.' )

            ELSE                                                  ! User-defined spectrum (irregular) wave

               CALL ReadCom ( UnIn, PtfmFile, 'unused WaveTp' )

            ENDIF


   ! WavePkShp - Peak shape parameter.

            IF ( WaveMod == 2 )  THEN                             ! .TRUE. for JONSWAP/Pierson-Moskowitz spectrum (irregular) wave

               CALL ReadCVar ( UnIn, PtfmFile, Line, 'WavePkShp', 'Peak shape parameter' )
               CALL Conv2UC( Line )    ! Convert Line to upper case.

               IF ( TRIM(Line) == 'DEFAULT' )  THEN   ! .TRUE. when one wants to use the default value of the peak shape parameter, conditioned on significant wave height and peak spectral period.

                  WavePkShp = WavePkShpDefault ( WaveHs, WaveTp )

               ELSE                                   ! The input must have been specified numerically.

                  READ (Line,*,IOSTAT=IOS)  WavePkShp
                  CALL CheckIOS ( IOS, PtfmFile, 'WavePkShp', NumType )

                  IF ( ( WavePkShp < 1.0 ) .OR. ( WavePkShp > 7.0 ) )  &
                     CALL ProgAbort ( ' WavePkShp must be greater than or equal to 1 and less than or equal to 7.' )

               ENDIF

            ELSE                                                  ! Plane progressive (regular) or user-defined spectrum (irregular) wave

               CALL ReadCom ( UnIn, PtfmFile, 'unused WavePkShp' )

            ENDIF


   ! WaveDir - Wave heading direction.

            CALL ReadRVar ( UnIn, PtfmFile, WaveDir, 'WaveDir', 'Wave heading direction' )

            IF ( ( WaveDir <= -180.0 ) .OR. ( WaveDir > 180.0 ) )  &
                CALL ProgAbort ( ' WaveDir must be greater than -180 and less than or equal to 180.' )


   ! WaveSeed(1) - First random seed.

            CALL ReadIVar ( UnIn, PtfmFile, WaveSeed(1), 'WaveSeed(1)', 'First random seed' )


   ! WaveSeed(2) - Second random seed.

            CALL ReadIVar ( UnIn, PtfmFile, WaveSeed(2), 'WaveSeed(2)', 'Second random seed' )


         ELSE                                                  ! We must not have incident waves or we must have GH Bladed wave data, so skip these inputs.


            CALL ReadCom ( UnIn, PtfmFile, 'unused WaveHs'      )
            CALL ReadCom ( UnIn, PtfmFile, 'unused WaveTp'      )
            CALL ReadCom ( UnIn, PtfmFile, 'unused WavePkShp'   )
            CALL ReadCom ( UnIn, PtfmFile, 'unused WaveDir'     )
            CALL ReadCom ( UnIn, PtfmFile, 'unused WaveSeed(1)' )
            CALL ReadCom ( UnIn, PtfmFile, 'unused WaveSeed(2)' )


         ENDIF


   ! GHWvFile - Root name of GH Bladed files containing wave data.

         IF ( WaveMod == 4 )  THEN  ! .TRUE if we are to use GH Bladed wave data.

            CALL ReadCVar ( UnIn, PtfmFile, GHWvFile, 'GHWvFile', 'Root name of GH Bladed files containing wave data' )

            IF ( LEN_TRIM( GHWvFile ) == 0 )  CALL ProgAbort ( ' GHWvFile must not be an empty string.' )

         ELSE                       ! We must not have GH Bladed wave data.

            CALL ReadCom ( UnIn, PtfmFile, 'unused GHWvFile' )

         ENDIF



!  -------------- CURRENT ------------------------------------------------------


   ! Skip the comment line.

         CALL ReadCom ( UnIn, PtfmFile, 'current'  )


   ! CurrMod - Current profile model switch.

         CALL ReadIVar ( UnIn, PtfmFile, CurrMod, 'CurrMod', 'Current profile model switch' )

         IF ( ( CurrMod /= 0 ) .AND. ( CurrMod /= 1 ) .AND. ( CurrMod /= 2 ) )  THEN
            CALL ProgAbort ( ' CurrMod must be 0, 1, or 2.' )
         END IF

         IF ( ( CurrMod /= 0 ) .AND. ( WaveMod == 4 ) )  THEN
            CALL ProgAbort ( ' CurrMod must be set to 0 when WaveMod is set to 4.' )
         END IF


         IF ( CurrMod == 1 )  THEN  ! .TRUE if we have standard current.


   ! CurrSSV0 - Sub-surface current velocity at still water level.

            CALL ReadRVar ( UnIn, PtfmFile, CurrSSV0, 'CurrSSV0', 'Sub-surface current velocity at still water level' )

            IF ( CurrSSV0 < 0.0 )  CALL ProgAbort ( ' CurrSSV0 must not be less than zero.' )


   ! CurrSSDir - Sub-surface current heading direction.

            CALL ReadCVar ( UnIn, PtfmFile, Line, 'CurrSSDir', 'Sub-surface current heading direction' )
            CALL Conv2UC( Line )    ! Convert Line to upper case.

            IF ( TRIM(Line) == 'DEFAULT' )  THEN   ! .TRUE. when one wants to use the default value of codirectionality between sub-surface current and incident wave propogation heading directions.

               IF ( WaveMod == 0 )  CALL ProgAbort ( ' CurrSSDir must not be set to ''DEFAULT'' when WaveMod is set to 0.' )   ! ProgAbort() since WaveDir is not defined when WaveMod = 0.

               CurrSSDir = WaveDir

            ELSE                                   ! The input must have been specified numerically.

               READ (Line,*,IOSTAT=IOS)  CurrSSDir
               CALL CheckIOS ( IOS, PtfmFile, 'CurrSSDir', NumType )

               IF ( ( CurrSSDir <= -180.0 ) .OR. ( CurrSSDir > 180.0 ) )  &
                  CALL ProgAbort ( ' CurrSSDir must be greater than -180 and less than or equal to 180.' )

            ENDIF


   ! CurrNSRef - Near-surface current reference depth.

            CALL ReadRVar ( UnIn, PtfmFile, CurrNSRef, 'CurrNSRef', 'Near-surface current reference depth' )

            IF ( CurrNSRef <= 0.0 )  CALL ProgAbort ( ' CurrNSRef must be greater than zero.' )


   ! CurrNSV0 - Near-surface current velocity at still water level.

            CALL ReadRVar ( UnIn, PtfmFile, CurrNSV0, 'CurrNSV0', 'Near-surface current velocity at still water level' )

            IF ( CurrNSV0 < 0.0 )  CALL ProgAbort ( ' CurrNSV0 must not be less than zero.' )


   ! CurrNSDir - Near-surface current heading direction.

            CALL ReadRVar ( UnIn, PtfmFile, CurrNSDir, 'CurrNSDir', 'Near-surface current heading direction' )

            IF ( ( CurrNSDir <= -180.0 ) .OR. ( CurrNSDir > 180.0 ) )  &
               CALL ProgAbort ( ' CurrNSDir must be greater than -180 and less than or equal to 180.' )


   ! CurrDIV - Depth-independent current velocity.

            CALL ReadRVar ( UnIn, PtfmFile, CurrDIV, 'CurrDIV', 'Depth-independent current velocity' )

            IF ( CurrDIV < 0.0 )  CALL ProgAbort ( ' CurrDIV must not be less than zero.' )


   ! CurrDIDir - Depth-independent current heading direction.

            CALL ReadRVar ( UnIn, PtfmFile, CurrDIDir, 'CurrDIDir', 'Depth-independent current heading direction' )

            IF ( ( CurrDIDir <= -180.0 ) .OR. ( CurrDIDir > 180.0 ) )  &
               CALL ProgAbort ( ' CurrDIDir must be greater than -180 and less than or equal to 180.' )


         ELSE                       ! We must not have standard current, so skip these inputs.


            CALL ReadCom ( UnIn, PtfmFile, 'unused CurrSSV0'  )
            CALL ReadCom ( UnIn, PtfmFile, 'unused CurrSSDir' )
            CALL ReadCom ( UnIn, PtfmFile, 'unused CurrNSRef' )
            CALL ReadCom ( UnIn, PtfmFile, 'unused CurrNSV0'  )
            CALL ReadCom ( UnIn, PtfmFile, 'unused CurrNSDir' )
            CALL ReadCom ( UnIn, PtfmFile, 'unused CurrDIV'   )
            CALL ReadCom ( UnIn, PtfmFile, 'unused CurrDIDir' )


         ENDIF



!  -------------- OUTPUT (CONT) ------------------------------------------------


   ! Skip the comment line.

         CALL ReadCom ( UnIn, PtfmFile, 'output (cont)'  )


   ! NWaveKin - Number of points where the incident wave kinematics can be output.

         CALL ReadIVar ( UnIn, PtfmFile, NWaveKin, 'NWaveKin', &
            'Number of points where the incident wave kinematics can be output' )

         IF ( ( NWaveKin < 0 ) .OR. ( NWaveKin > 9 ) )  CALL ProgAbort ( ' NWaveKin must be between 0 and 9 (inclusive).' )


   ! WaveKinNd - List of tower nodes that have wave kinematics sensors.

         READ (UnIn,*,IOSTAT=IOS)  ( WaveKinNd(I), I=1,NWaveKin )

         CALL CheckIOS ( IOS, PtfmFile, 'WaveKinNd', NumType)
!         IF ( IOS < 0 )  THEN
!            CALL PremEOF ( PtfmFile , 'WaveKinNd' )
!         ELSEIF ( IOS > 0 )  THEN
!            CALL WrScr1 ( ' Invalid numerical input for file "'//TRIM( PtfmFile )//'.' )
!            CALL ProgAbort  ( ' The error occurred while trying to read the WaveKinNd array.' )
!         ENDIF

         IF ( Echo )  THEN
!bjj chg:    WRITE (UnEc,"(15X,A,T27,' - ',A)")  'WaveKinNd', 'List of tower nodes that have wave kinematics sensors'
            WRITE (UnEc,"( 2X, ES11.4e2, 2X, A, T30, ' - ', A )")  'WaveKinNd', &
                            'List of tower nodes that have wave kinematics sensors'

            WRITE (UnEc,'(9(I4,:))')  ( WaveKinNd(I), I=1,NWaveKin )
         ENDIF


   ! Check to see if all WaveKinNd(:) analysis points are existing analysis points:

         DO I=1,NWaveKin
            IF ( ( WaveKinNd(I) < 1 ) .OR. ( WaveKinNd(I) > TwrNodes ) )  &
               CALL ProgAbort  ( ' All WaveKinNd values must be between 1 and '//TRIM( Int2LStr( TwrNodes ) )//' (inclusive).' )
         ENDDO ! I


      ENDIF

   ENDIF




CASE ( 3 )                 ! Floating offshore.




   ! PtfmLdMod - Platform loading model switch.

   CALL ReadCVar ( UnIn, PtfmFile, Line, 'PtfmLdMod', 'Platform loading model switch' )

   LineUC = Line
   CALL Conv2UC( LineUC )    ! Convert LineUC to upper case.

   IF ( TRIM(LineUC) == 'FLTNGPTFMLD' )  THEN   ! .TRUE. for undocumented loading for a floating platform.

      PtfmLdMod = 9999

      IF ( Gravity <= 0.0 )  &
         CALL ProgAbort ( ' Gravity must be greater than zero when PtfmLdMod is set to "'//TRIM(Line)//'".' )

      IF ( TwrDraft > 0.0 )  &
         CALL ProgAbort ( ' TwrDraft must be less than or equal to zero when PtfmLdMod is set to "'//TRIM(Line)//'".' )  ! Do not allow the combination of tower hydrodynamics using Morison's equation and platform hydrodynamics using the true form of the using the true form of the hydrodynamics equations since the true equations require that the shape of the platform does not change above the MSL (platform reference point)--Consider the linear hydrostatic restoring matrix, for example.

      IF ( PtfmRef /= 0.0 )  &
         CALL ProgAbort ( ' PtfmRef must be zero when PtfmLdMod is set to "'//TRIM(Line)//'".' )

   ELSE                                         ! The input is specified as documented: ProgAbort the program if the input is not the Numeric value of 0 or 1.

      READ (Line,*,IOSTAT=IOS)  PtfmLdMod

      CALL CheckIOS ( IOS, PtfmFile, 'PtfmLdMod', NumType )

      IF ( ( PtfmLdMod /= 0 ) .AND. ( PtfmLdMod /= 1 ) )  CALL ProgAbort ( ' PtfmLdMod must be 0 or 1.' )

   ENDIF


   ! Read in additional inputs only if we are using one of the undocumented
   !   features:

   IF ( PtfmLdMod == 9999 )  THEN   ! .TRUE. if we are using the undocumented platform features


   ! Enable the undocumented hydrodynamic loading routines:

      CompHydro = .TRUE.


   ! WAMITFile - Root name of WAMIT output files.

      CALL ReadCVar ( UnIn, PtfmFile, WAMITFile, 'WAMITFile', 'Root name of WAMIT output files' )

      IF ( LEN_TRIM( WAMITFile ) == 0 )  CALL ProgAbort ( ' WAMITFile must not be an empty string.' )


   ! PtfmVol0 - Displaced volume of water when the platform is in its undisplaced position.

      CALL ReadRVar ( UnIn, PtfmFile, PtfmVol0, 'PtfmVol0', &
         'Displaced volume of water when the platform is in its undisplaced position' )

      IF ( PtfmVol0 < 0.0 )  CALL ProgAbort ( ' PtfmVol0 must not be negative.' )


   ! PtfmNodes - Number of platform nodes used in calculation of viscous drag term from Morison's equation.

      CALL ReadIVar ( UnIn, PtfmFile, PtfmNodes, 'PtfmNodes', &
         'Number of platform nodes used in calculation of viscous drag term from Morison''s equation' )

      IF ( PtfmNodes < 0 )  CALL ProgAbort ( ' PtfmNodes must not be less than 0.' )


   ! PtfmDraft - Effective platform draft in calculation of viscous drag term from Morison's equation.

      CALL ReadRVar ( UnIn, PtfmFile, PtfmDraft, 'PtfmDraft', &
         'Effective platform draft in calculation of viscous drag term from Morison''s equation' )

      IF ( PtfmDraft < 0.0 )  CALL ProgAbort ( ' PtfmDraft must not be negative.' )


   ! PtfmDiam - Effective platform diameter in calculation of viscous drag term from Morison's equation.

      CALL ReadRVar ( UnIn, PtfmFile, PtfmDiam, 'PtfmDiam', &
         'Effective platform diameter in calculation of viscous drag term from Morison''s equation' )

      IF ( PtfmDiam < 0.0 )  CALL ProgAbort ( ' PtfmDiam must not be negative.' )


   ! PtfmCD - Effective platform normalized hydrodynamic viscous drag coefficient in calculation of viscous drag term from Morison's equation.

      CALL ReadRVar ( UnIn, PtfmFile, PtfmCD, 'PtfmCD', &
         'Effective platform normalized hydrodynamic viscous drag coefficient in Morison''s equation' )

      IF ( PtfmCD < 0.0 )  CALL ProgAbort ( ' PtfmCD must not be negative.' )


   ! RdtnTMax - Analysis time for wave radiation kernel calculations.
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Put in some logic to ensure that the hydrodynamic loads are time invariant
!jmj   when linearizing a model:
   ! NOTE: Use RdtnTMax = 0.0 to eliminate wave radiation damping.
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

      CALL ReadRVar ( UnIn, PtfmFile, RdtnTMax, 'RdtnTMax', 'Analysis time for wave radiation kernel calculations' )

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Put in some logic to ensure that the hydrodynamic loads are time invariant
!jmj   when linearizing a model:
!remove6.02b      IF ( RdtnTMax <= 0.0 )  CALL ProgAbort ( ' RdtnTMax must be greater than zero.' )
      IF ( RdtnTMax < 0.0 )  CALL ProgAbort ( ' RdtnTMax must not be negative.' )
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.


   ! RdtnDT - Time step for wave radiation kernel calculations.

      CALL ReadRVar ( UnIn, PtfmFile, RdtnDT, 'RdtnDT', 'Time step for wave radiation kernel calculations' )

      IF ( RdtnDT <= 0.0 )  CALL ProgAbort ( ' RdtnDT must be greater than zero.' )



!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
!  -------------- MOORING LINES ------------------------------------------------


   ! Skip the comment line.

      CALL ReadCom ( UnIn, PtfmFile, 'mooring lines'  )


   ! NumLines - Number of mooring lines.

      CALL ReadIVar ( UnIn, PtfmFile, NumLines, 'NumLines', 'Number of mooring lines' )

      IF ( NumLines < 0 )  CALL ProgAbort ( ' NumLines must not be less than zero.' )


   ! Allocate the input arrays.
   ! NOTE: We must ALLOCATE these arrays even when LineMod <> 1 because the
   !       arrays are passed into the InitFltngPtfm() routine.

      IF (.NOT. ALLOCATED(LAnchxi) ) THEN
         ALLOCATE ( LAnchxi  (NumLines          ) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the LAnchxi array.' )
         ENDIF
      ENDIF

      IF (.NOT. ALLOCATED(LAnchyi) ) THEN
         ALLOCATE ( LAnchyi  (NumLines          ) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the LAnchyi array.' )
         ENDIF
      ENDIF

      IF (.NOT. ALLOCATED(LAnchzi) ) THEN
         ALLOCATE ( LAnchzi  (NumLines          ) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the LAnchzi array.' )
         ENDIF
      ENDIF

      IF (.NOT. ALLOCATED(LFairxt) ) THEN
         ALLOCATE ( LFairxt  (NumLines          ) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the LFairxt array.' )
         ENDIF
      ENDIF

      IF (.NOT. ALLOCATED(LFairyt) ) THEN
         ALLOCATE ( LFairyt  (NumLines          ) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the LFairyt array.' )
         ENDIF
      ENDIF

      IF (.NOT. ALLOCATED(LFairzt) ) THEN
         ALLOCATE ( LFairzt  (NumLines          ) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the LFairzt array.' )
         ENDIF
      ENDIF

      IF (.NOT. ALLOCATED(LUnstrLen) ) THEN
         ALLOCATE ( LUnstrLen(NumLines          ) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the LUnstrLen array.' )
         ENDIF
      ENDIF

      IF (.NOT. ALLOCATED(LDiam) ) THEN
         ALLOCATE ( LDiam    (NumLines          ) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the LDiam array.' )
         ENDIF
      ENDIF

      IF (.NOT. ALLOCATED(LMassDen) ) THEN
         ALLOCATE ( LMassDen (NumLines          ) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the LMassDen array.' )
         ENDIF
      ENDIF

      IF (.NOT. ALLOCATED(LEAStff) ) THEN
         ALLOCATE ( LEAStff  (NumLines          ) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the LEAStff array.' )
         ENDIF
      ENDIF

      IF (.NOT. ALLOCATED(LSeabedCD) ) THEN
         ALLOCATE ( LSeabedCD(NumLines          ) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the LSeabedCD array.' )
         ENDIF
      ENDIF

      IF (.NOT. ALLOCATED(LTenTol) ) THEN
         ALLOCATE ( LTenTol  (NumLines          ) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the LTenTol array.' )
         ENDIF
      ENDIF

      IF (.NOT. ALLOCATED(LSNodes) ) THEN
         ALLOCATE ( LSNodes  (NumLines,LineNodes) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the LSNodes array.' )
         ENDIF
      ENDIF


      IF ( NumLines > 0 )  THEN  ! .TRUE. if we have at least one mooring line.


   ! LineMod - Mooring line model switch.

         CALL ReadIVar ( UnIn, PtfmFile, LineMod, 'LineMod', 'Mooring line model switch' )

         IF ( ( LineMod /= 1 ) .AND. ( LineMod /= 2 ) )  CALL ProgAbort ( ' LineMod must be 1 or 2.' )


   ! Skip the comment lines.

         CALL ReadCom ( UnIn, PtfmFile, 'mooring line parameter names' )
         CALL ReadCom ( UnIn, PtfmFile, 'mooring line parameter units' )


         IF ( LineMod == 1 )  THEN  ! .TRUE if we have standard quasi-static mooring lines.


            IF ( Echo )  THEN

               Frmt = "( '  Line    LRadAnch    LAngAnch   LDpthAnch    LRadFair    LAngFair   LDrftFair"// &
                      "   LUnstrLen       LDiam    LMassDen     LEAStff   LSeabedCD     LTenTol' )"
               WRITE (UnEc,Frmt)
               Frmt = "( '  ----    --------    --------   ---------    --------    --------   ---------"// &
                      "   ---------       -----    --------     -------   ---------     -------' )"
               WRITE (UnEc,Frmt)
               Frmt = '( I5, 1X, 12( 2X, '//TRIM( OutFmt )//') )'

            ENDIF


   ! Read in the mooring line data.
   ! NOTE: Store the x-, y-, and z-coordinates of each anchor and fairlead,
   !       instead of the radius, angle, and depth/draft.

            DO I = 1,NumLines ! Loop through all mooring lines

               READ (UnIn,*,IOSTAT=IOS)  LRadAnch    , LAngAnch   , LDpthAnch   , LRadFair   , LAngFair    , LDrftFair   , &
                                         LUnstrLen(I), LDiam   (I), LMassDen (I), LEAStff (I), LSeabedCD(I), LTenTol  (I)

               CALL CheckIOS ( IOS, PtfmFile, 'mooring line '//TRIM( Int2LStr( I ) )//' properties', NumType )

               IF ( Echo )  THEN
                     WRITE (UnEc,Frmt)  I, LRadAnch    , LAngAnch   , LDpthAnch   , LRadFair   , LAngFair    , LDrftFair   , &
                                           LUnstrLen(I), LDiam   (I), LMassDen (I), LEAStff (I), LSeabedCD(I), LTenTol  (I)
               ENDIF

               IF ( LRadAnch     <  0.0       )  &
                  CALL ProgAbort ( ' LRadAnch('//TRIM( Int2LStr( I ) )//') must not be less than zero.' )

               IF ( LRadFair     <  0.0       )  &
                  CALL ProgAbort ( ' LRadFair('//TRIM( Int2LStr( I ) )//') must not be less than zero.' )

               IF ( LDrftFair    <  TwrDraft  ) &
                  CALL ProgAbort ( ' LDrftFair('//TRIM( Int2LStr( I ) )//') must not be less than TwrDraft.' )

               IF ( LDpthAnch    <  LDrftFair ) &
                  CALL ProgAbort ( ' LDpthAnch('//TRIM( Int2LStr( I ) )//') must not be less than'// &
                               ' LDrftFair('//TRIM( Int2LStr( I ) )//').'                          )

               IF ( LUnstrLen(I) <= 0.0       )  &
                  CALL ProgAbort ( ' LUnstrLen('//TRIM( Int2LStr( I ) )//') must be greater than zero.' )

               IF ( LDiam    (I) <  0.0       )  &
                  CALL ProgAbort ( ' LDiam('//TRIM( Int2LStr( I ) )//') must not be less than zero.' )

               IF ( LMassDen (I) <  0.0       )  &
                  CALL ProgAbort ( ' LMassDen('//TRIM( Int2LStr( I ) )//') must not be less than zero.' )

               IF ( LEAStff  (I) <= 0.0       )  &
                  CALL ProgAbort ( ' LEAStff('//TRIM( Int2LStr( I ) )//') must be greater than zero.' )

               ! NOTE: Values of LSeabedCD less than zero indicate no seabed interaction (i.e., the mooring line may fall below the anchor)

               IF ( LTenTol  (I) <= 0.0       )  &
                  CALL ProgAbort ( ' LTenTol('//TRIM( Int2LStr( I ) )//') must be greater than zero.' )


!jmj Start of proposed change.  v6.10a-jmj  21-Feb-2007.
!jmj Make sure the seabed GRAPHICS in FAST-to-ADAMS has a radius at least as
!jmj   large as the maximum mooring line anchor radius:
               MaxLRadAnch = MAX( MaxLRadAnch, LRadAnch )   ! Find the maximum value of the input array LRadAnch

!jmj End of proposed change.  v6.10a-jmj  21-Feb-2007.
               LAngAnch   =  LAngAnch*D2R             ! Convert the azimuth angle of the current
               LAngFair   =  LAngFair*D2R             ! anchor and fairlead from degrees to radians.

               LAnchxi(I) =  LRadAnch *COS(LAngAnch)  !
               LAnchyi(I) =  LRadAnch *SIN(LAngAnch)  ! Convert the radius, azimuth angle, and depth of the current anchor   to x-, y-, and z-coordinates into the inertial frame        coordinate system
               LAnchzi(I) = -LDpthAnch                !
               LFairxt(I) =  LRadFair *COS(LAngFair)  !
               LFairyt(I) =  LRadFair *SIN(LAngFair)  ! Convert the radius, azimuth angle, and draft of the current fairlead to x-, y-, and z-coordinates into the tower base / platform coordinate system
               LFairzt(I) = -LDrftFair                !

            ENDDO             ! I - All mooring lines


         ENDIF


      ELSE                       ! We must not have any mooring lines, so skip these inputs.


         LineMod = 0 ! Set LineMod to zero for "none".

         CALL ReadCom ( UnIn, PtfmFile, 'unused LineMod'                      )
         CALL ReadCom ( UnIn, PtfmFile, 'unused mooring line parameter names' )
         CALL ReadCom ( UnIn, PtfmFile, 'unused mooring line parameter units' )


      ENDIF



!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
!  -------------- WAVES --------------------------------------------------------


   ! Skip the comment line.

      CALL ReadCom ( UnIn, PtfmFile, 'waves'  )


!JASON: MOVE THIS INPUT, WtrDens, TO AN ENVIRONMENTAL CONDITIONS SECTION OF THE INPUT FILE WHEN YOU DOCUMENT THIS NEW FEATURE!!!!
   ! WtrDens - Water density.

      CALL ReadRVar ( UnIn, PtfmFile, WtrDens, 'WtrDens', 'Water density' )

      IF ( WtrDens < 0.0 )  CALL ProgAbort ( ' WtrDens must not be negative.' )


   ! WtrDpth - Water depth.

      CALL ReadRVar ( UnIn, PtfmFile, WtrDpth, 'WtrDpth', 'Water depth' )

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
!remove6.02b      IF ( WtrDpth <= PtfmDraft )  CALL ProgAbort ( ' WtrDpth must be greater than PtfmDraft.' )
      IF (       WtrDpth <= PtfmDraft  )  CALL ProgAbort ( ' WtrDpth must be greater than PtfmDraft.' )
      IF ( LineMod == 1 )  THEN  ! .TRUE if we have standard quasi-static mooring lines.
         DO I = 1,NumLines ! Loop through all mooring lines
            IF ( WtrDpth < -LAnchzi(I) )  & 
                  CALL ProgAbort ( ' WtrDpth must not be less than LDpthAnch('//TRIM( Int2LStr( I ) )//').' )
         ENDDO             ! I - All mooring lines
      ENDIF
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.


   ! WaveMod - Wave kinematics model switch.

      CALL ReadIVar ( UnIn, PtfmFile, WaveMod, 'WaveMod', 'Wave kinematics model switch' )

      IF ( ( WaveMod /= 0 ) .AND. ( WaveMod /= 1 ) .AND. ( WaveMod /= 2 ) .AND. ( WaveMod /= 3 ) )  &
         CALL ProgAbort ( ' WaveMod must be 0, 1, 2, or 3.' )


      IF ( WaveMod > 0 )  THEN   ! .TRUE if we have incident waves.


   ! NOTE: Do not read in WaveStMod for floating platforms since it is
   !       inconsistent to use stretching (which is a nonlinear correction) for
   !       the viscous drag term in Morison's equation while not accounting for
   !       stretching in the diffraction and radiation problems (according to
   !       Paul Sclavounos, there are such corrections).  Instead, the viscous
   !       drag term from Morison's equation is computed by integrating up to
   !       the MSL, regardless of the instantaneous free surface elevation.


   ! WaveTMax - Analysis time for incident wave calculations.

         CALL ReadRVar ( UnIn, PtfmFile, WaveTMax, 'WaveTMax', 'Analysis time for incident wave calculations' )

         IF ( WaveTMax < TMax )  CALL ProgAbort ( ' WaveTMax must not be less than TMax.' )


   ! WaveDT - Time step for incident wave calculations.

         CALL ReadRVar ( UnIn, PtfmFile, WaveDT, 'WaveDT', 'Time step for incident wave calculations' )

         IF ( WaveDT <= 0.0 )  CALL ProgAbort ( ' WaveDT must be greater than zero.' )


   ! WaveHs - Significant wave height.

         IF ( ( WaveMod == 1 ) .OR. ( WaveMod == 2 ) )  THEN   ! .TRUE. for plane progressive (regular) or JONSWAP/Pierson-Moskowitz spectrum (irregular) wave

            CALL ReadRVar ( UnIn, PtfmFile, WaveHs, 'WaveHs', 'Significant wave height' )

            IF ( WaveHs <= 0.0 )  CALL ProgAbort ( ' WaveHs must be greater than zero.' )

         ELSE                                                  ! User-defined spectrum (irregular) wave

            CALL ReadCom ( UnIn, PtfmFile, 'unused WaveHs' )

         ENDIF


   ! WaveTp - Peak spectral period.

         IF ( ( WaveMod == 1 ) .OR. ( WaveMod == 2 ) )  THEN   ! .TRUE. for plane progressive (regular) or JONSWAP/Pierson-Moskowitz spectrum (irregular) wave

            CALL ReadRVar ( UnIn, PtfmFile, WaveTp, 'WaveTp', 'Peak spectral period' )

            IF ( WaveTp <= 0.0 )  CALL ProgAbort ( ' WaveTp must be greater than zero.' )

         ELSE                                                  ! User-defined spectrum (irregular) wave

            CALL ReadCom ( UnIn, PtfmFile, 'unused WaveTp' )

         ENDIF


   ! WavePkShp - Peak shape parameter.

         IF ( WaveMod == 2 )  THEN                             ! .TRUE. for JONSWAP/Pierson-Moskowitz spectrum (irregular) wave

            CALL ReadCVar ( UnIn, PtfmFile, Line, 'WavePkShp', 'Peak shape parameter' )
            CALL Conv2UC( Line )    ! Convert Line to upper case.

            IF ( TRIM(Line) == 'DEFAULT' )  THEN   ! .TRUE. when one wants to use the default value of the peak shape parameter, conditioned on significant wave height and peak spectral period.

               WavePkShp = WavePkShpDefault ( WaveHs, WaveTp )

            ELSE                                   ! The input must have been specified numerically.

               READ (Line,*,IOSTAT=IOS)  WavePkShp
               CALL CheckIOS ( IOS, PtfmFile, 'WavePkShp', NumType )

               IF ( ( WavePkShp < 1.0 ) .OR. ( WavePkShp > 7.0 ) )  &
                  CALL ProgAbort ( ' WavePkShp must be greater than or equal to 1 and less than or equal to 7.' )

            ENDIF

         ELSE                                                  ! Plane progressive (regular) or user-defined spectrum (irregular) wave

            CALL ReadCom ( UnIn, PtfmFile, 'unused WavePkShp' )

         ENDIF


   ! WaveDir - Wave heading direction.

         CALL ReadRVar ( UnIn, PtfmFile, WaveDir, 'WaveDir', 'Wave heading direction' )

         IF ( ( WaveDir <= -180.0 ) .OR. ( WaveDir > 180.0 ) )  &
            CALL ProgAbort ( ' WaveDir must be greater than -180 and less than or equal to 180.' )


   ! WaveSeed(1) - First random seed.

         CALL ReadIVar ( UnIn, PtfmFile, WaveSeed(1), 'WaveSeed(1)', 'First random seed' )


   ! WaveSeed(2) - Second random seed.

         CALL ReadIVar ( UnIn, PtfmFile, WaveSeed(2), 'WaveSeed(2)', 'Second random seed' )


      ELSE                       ! We must not have incident waves, so skip these inputs.


         CALL ReadCom ( UnIn, PtfmFile, 'unused WaveTMax'    )
         CALL ReadCom ( UnIn, PtfmFile, 'unused WaveDT'      )
         CALL ReadCom ( UnIn, PtfmFile, 'unused WaveHs'      )
         CALL ReadCom ( UnIn, PtfmFile, 'unused WaveTp'      )
         CALL ReadCom ( UnIn, PtfmFile, 'unused WavePkShp'   )
         CALL ReadCom ( UnIn, PtfmFile, 'unused WaveDir'     )
         CALL ReadCom ( UnIn, PtfmFile, 'unused WaveSeed(1)' )
         CALL ReadCom ( UnIn, PtfmFile, 'unused WaveSeed(2)' )


      ENDIF



!  -------------- CURRENT ------------------------------------------------------


   ! Skip the comment line.

      CALL ReadCom ( UnIn, PtfmFile, 'current'  )


   ! CurrMod - Current profile model switch.

      CALL ReadIVar ( UnIn, PtfmFile, CurrMod, 'CurrMod', 'Current profile model switch' )

      IF ( ( CurrMod /= 0 ) .AND. ( CurrMod /= 1 ) .AND. ( CurrMod /= 2 ) )  CALL ProgAbort ( ' CurrMod must be 0, 1, or 2.' )


      IF ( CurrMod == 1 )  THEN  ! .TRUE if we have standard current.


   ! CurrSSV0 - Sub-surface current velocity at still water level.

         CALL ReadRVar ( UnIn, PtfmFile, CurrSSV0, 'CurrSSV0', 'Sub-surface current velocity at still water level' )

         IF ( CurrSSV0 < 0.0 )  CALL ProgAbort ( ' CurrSSV0 must not be less than zero.' )


   ! CurrSSDir - Sub-surface current heading direction.

         CALL ReadCVar ( UnIn, PtfmFile, Line, 'CurrSSDir', 'Sub-surface current heading direction' )
         CALL Conv2UC( Line )    ! Convert Line to upper case.

         IF ( TRIM(Line) == 'DEFAULT' )  THEN   ! .TRUE. when one wants to use the default value of codirectionality between sub-surface current and incident wave propogation heading directions.

            IF ( WaveMod == 0 )  CALL ProgAbort ( ' CurrSSDir must not be set to ''DEFAULT'' when WaveMod is set to 0.' )   ! ProgAbort() since WaveDir is not defined when WaveMod = 0.

            CurrSSDir = WaveDir

         ELSE                                   ! The input must have been specified numerically.

            READ (Line,*,IOSTAT=IOS)  CurrSSDir
            CALL CheckIOS ( IOS, PtfmFile, 'CurrSSDir', NumType )

            IF ( ( CurrSSDir <= -180.0 ) .OR. ( CurrSSDir > 180.0 ) )  &
               CALL ProgAbort ( ' CurrSSDir must be greater than -180 and less than or equal to 180.' )

         ENDIF


   ! CurrNSRef - Near-surface current reference depth.

         CALL ReadRVar ( UnIn, PtfmFile, CurrNSRef, 'CurrNSRef', 'Near-surface current reference depth' )

         IF ( CurrNSRef <= 0.0 )  CALL ProgAbort ( ' CurrNSRef must be greater than zero.' )


   ! CurrNSV0 - Near-surface current velocity at still water level.

         CALL ReadRVar ( UnIn, PtfmFile, CurrNSV0, 'CurrNSV0', 'Near-surface current velocity at still water level' )

         IF ( CurrNSV0 < 0.0 )  CALL ProgAbort ( ' CurrNSV0 must not be less than zero.' )


   ! CurrNSDir - Near-surface current heading direction.

         CALL ReadRVar ( UnIn, PtfmFile, CurrNSDir, 'CurrNSDir', 'Near-surface current heading direction' )

         IF ( ( CurrNSDir <= -180.0 ) .OR. ( CurrNSDir > 180.0 ) )  &
            CALL ProgAbort ( ' CurrNSDir must be greater than -180 and less than or equal to 180.' )


   ! CurrDIV - Depth-independent current velocity.

         CALL ReadRVar ( UnIn, PtfmFile, CurrDIV, 'CurrDIV', 'Depth-independent current velocity' )

         IF ( CurrDIV < 0.0 )  CALL ProgAbort ( ' CurrDIV must not be less than zero.' )


   ! CurrDIDir - Depth-independent current heading direction.

         CALL ReadRVar ( UnIn, PtfmFile, CurrDIDir, 'CurrDIDir', 'Depth-independent current heading direction' )

         IF ( ( CurrDIDir <= -180.0 ) .OR. ( CurrDIDir > 180.0 ) )  &
            CALL ProgAbort ( ' CurrDIDir must be greater than -180 and less than or equal to 180.' )


      ELSE                       ! We must not have standard current, so skip these inputs.


         CALL ReadCom ( UnIn, PtfmFile, 'unused CurrSSV0'  )
         CALL ReadCom ( UnIn, PtfmFile, 'unused CurrSSDir' )
         CALL ReadCom ( UnIn, PtfmFile, 'unused CurrNSRef' )
         CALL ReadCom ( UnIn, PtfmFile, 'unused CurrNSV0'  )
         CALL ReadCom ( UnIn, PtfmFile, 'unused CurrNSDir' )
         CALL ReadCom ( UnIn, PtfmFile, 'unused CurrDIV'   )
         CALL ReadCom ( UnIn, PtfmFile, 'unused CurrDIDir' )


      ENDIF



!  -------------- OUTPUT (CONT) ------------------------------------------------


   ! Skip the comment line.

      CALL ReadCom ( UnIn, PtfmFile, 'output (cont)' )


   ! NWaveKin - Number of points where the incident wave kinematics can be output.

      CALL ReadIVar ( UnIn, PtfmFile, NWaveKin, 'NWaveKin', &
         'Number of points where the incident wave kinematics can be output' )

      IF ( ( NWaveKin < 0 ) .OR. ( NWaveKin > 9 ) )  CALL ProgAbort ( ' NWaveKin must be between 0 and 9 (inclusive).' )


   ! WaveKinNd - List of platform nodes that have wave kinematics sensors.

      READ (UnIn,*,IOSTAT=IOS)  ( WaveKinNd(I), I=1,NWaveKin )

      CALL CheckIOS( IOS, PtfmFile, 'WaveKinNd', NumType )
!      IF ( IOS < 0 )  THEN
!         CALL PremEOF ( PtfmFile , 'WaveKinNd' )
!      ELSEIF ( IOS > 0 )  THEN
!         CALL WrScr1 ( ' Invalid numerical input for file "'//TRIM( PtfmFile )//'.' )
!         CALL ProgAbort  ( ' The error occurred while trying to read the WaveKinNd array.' )
!      ENDIF

      IF ( Echo )  THEN
!bjj rm         WRITE (UnEc,"(15X,A,T27,' - ',A)")  'WaveKinNd', 'List of platform nodes that have wave kinematics sensors'
!BJJ START OF PROPOSED CHANGE V7.00.01a-bjj
!bjj this line is too long:
!rm         WRITE (UnEc,"( 2X, ES11.4e2, 2X, A, T30, ' - ', A )")  'WaveKinNd', 'List of platform nodes that have wave kinematics sensors'
         WRITE (UnEc,"( 2X, ES11.4e2, 2X, A, T30, ' - ', A )")  'WaveKinNd', &
                        'List of platform nodes that have wave kinematics sensors'
!bjj end of proposed change
         WRITE (UnEc,'(9(I4,:))')  ( WaveKinNd(I), I=1,NWaveKin )
      ENDIF


   ! Check to see if all WaveKinNd(:) analysis points are existing analysis points:

      DO I=1,NWaveKin
         IF ( ( WaveKinNd(I) < 1 ) .OR. ( WaveKinNd(I) > PtfmNodes ) )  &
            CALL ProgAbort  ( ' All WaveKinNd values must be between 1 and '//TRIM( Int2LStr( PtfmNodes ) )//' (inclusive).' )
      ENDDO ! I


   ENDIF




ENDSELECT
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! Close the FAST platform file:

CLOSE ( UnIn )



RETURN
END SUBROUTINE GetPtfm
!=======================================================================
SUBROUTINE GetTower


   ! This routine reads the tower file and validates the input.


USE                             General
USE                             Modes
USE                             Output
!bjj rm NWTC_Library: USE                             Precision
USE                             Tower


IMPLICIT                        NONE


   ! Local variables.

REAL(ReKi)                   :: TopDispl                                        ! Tower-top displacement for a mode shape.


INTEGER(4)                   :: I                                               ! A generic index.
INTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
!bjj rm unused:INTEGER(4)                   :: K                                               ! Blade number.
INTEGER(4)                   :: Sttus                                           ! Status returned from an allocation request.

CHARACTER(132)               :: Frmt                                            ! Format for element data.


   ! Global functions.

!bjj rm AD 12.70b CHARACTER(15), EXTERNAL      :: Flt2LStr                                        ! A function to convert a floating-point number to a left-justified string.
!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.



   ! Open the tower input file.

!BJJ:
CALL OpenFInpFile ( UnIn, TwrFile )


   ! Add a separator to the echo file if appropriate.

IF ( Echo )  WRITE (UnEc,'(//,A,/)')  'Tower input data from file "'//TRIM( TwrFile )//'":'



!  -------------- HEADER -------------------------------------------------------


   ! Skip the header.

READ (UnIn,'(//)',IOSTAT=IOS)

IF ( IOS < 0 )  THEN
   CALL PremEOF ( TwrFile , 'unused tower-file header' )
ENDIF



!  -------------- TOWER PARAMETERS ---------------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, TwrFile, 'tower parameters', Echo  )
   CALL ReadCom ( UnIn, PtfmFile, 'tower parameters' )
!bjj End of proposed change


   ! NTwInpSt - Number of tower input stations.

CALL ReadIVar ( UnIn, TwrFile, NTwInpSt, 'NTwInpSt', 'Number of tower input stations' )

IF ( NTwInpSt < 1 )  CALL ProgAbort ( ' NTwInpSt must be at least 1.' )


   ! CalcTMode - Calculate tower mode shapes (switch).

!JASON: ADD LOGIC FOR THIS NEW VARIABLE:
!JASON:CALL ReadLVar ( UnIn, TwrFile, CalcTMode, 'CalcTMode', 'Calculate tower mode shapes' )
!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, TwrFile, 'currently ignored CalcTMode', Echo  )   !JASON:
   CALL ReadCom ( UnIn, TwrFile, 'currently ignored CalcTMode' )
!bjj End of proposed change


   ! TwrFADmp - Tower fore-aft structural damping ratios.

CALL ReadAryLines ( UnIn, TwrFile, TwrFADmp, 2, 'TwrFADmp', 'Tower fore-aft structural damping ratios' )

IF ( ( TwrFADmp(1) < 0.0 ) .OR. ( TwrFADmp(1) > 100.0 ) )  CALL ProgAbort ( ' TwrFADmp(1) must be between 0 and 100 (inclusive).' )
IF ( ( TwrFADmp(2) < 0.0 ) .OR. ( TwrFADmp(2) > 100.0 ) )  CALL ProgAbort ( ' TwrFADmp(2) must be between 0 and 100 (inclusive).' )


   ! TwrSSDmp - Tower side-to-side structural damping ratios.

CALL ReadAryLines ( UnIn, TwrFile, TwrSSDmp, 2, 'TwrSSDmp', 'Tower side-to-side structural damping ratios' )

IF ( ( TwrSSDmp(1) < 0.0 ) .OR. ( TwrSSDmp(1) > 100.0 ) )  CALL ProgAbort ( ' TwrSSDmp(1) must be between 0 and 100 (inclusive).' )
IF ( ( TwrSSDmp(2) < 0.0 ) .OR. ( TwrSSDmp(2) > 100.0 ) )  CALL ProgAbort ( ' TwrSSDmp(2) must be between 0 and 100 (inclusive).' )



!  -------------- TOWER ADJUSTMENT FACTORS -------------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, TwrFile, 'tower adjustment factors', Echo  )
   CALL ReadCom ( UnIn, TwrFile, 'tower adjustment factors' )
!bjj End of proposed change



   ! FAStTunr(1) - Tower fore-aft modal stiffness tuners.

CALL ReadAryLines ( UnIn, TwrFile, FAStTunr, 2, 'FAStTunr', 'Tower fore-aft modal stiffness tuners' )

IF ( FAStTunr(1) <= 0.0 )  CALL ProgAbort ( ' FAStTunr(1) must be greater than zero.' )
IF ( FAStTunr(2) <= 0.0 )  CALL ProgAbort ( ' FAStTunr(2) must be greater than zero.' )


   ! SSStTunr(1) - Tower side-to-side modal stiffness tuners.

CALL ReadAryLines ( UnIn, TwrFile, SSStTunr, 2, 'SSStTunr', 'Tower side-to-side modal stiffness tuners' )

IF ( SSStTunr(1) <= 0.0 )  CALL ProgAbort ( ' SSStTunr(1) must be greater than zero.' )
IF ( SSStTunr(2) <= 0.0 )  CALL ProgAbort ( ' SSStTunr(2) must be greater than zero.' )


   ! AdjTwMa - Factor to adjust tower mass density.

CALL ReadRVar ( UnIn, TwrFile, AdjTwMa, 'AdjTwMa', 'Factor to adjust tower mass density' )

IF ( AdjTwMa <= 0.0 )  CALL ProgAbort ( ' AdjTwMa must be greater than zero.' )


   ! AdjFASt - Factor to adjust tower fore-aft stiffness.

CALL ReadRVar ( UnIn, TwrFile, AdjFASt, 'AdjFASt', 'Factor to adjust tower fore-aft stiffness' )

IF ( AdjFASt <= 0.0 )  CALL ProgAbort ( ' AdjFASt must be greater than zero.' )


   ! AdjSSSt - Factor to adjust tower side-to-side stiffness.

CALL ReadRVar ( UnIn, TwrFile, AdjSSSt, 'AdjSSSt', 'Factor to adjust tower side-to-side stiffness' )

IF ( AdjSSSt <= 0.0 )  CALL ProgAbort ( ' AdjSSSt must be greater than zero.' )



!  -------------- DISTRIBUTED TOWER PROPERTIES ---------------------------------


   ! Skip the comment lines.

!bjj Start of proposed change vXX NWTC_Lib
!rm   CALL SkipComment ( UnIn, TwrFile, 'distributed tower parameters', Echo  )
!rm CALL SkipComment ( UnIn, TwrFile, 'distributed-tower-parameter names', .FALSE. )
!rm CALL SkipComment ( UnIn, TwrFile, 'distributed-tower-parameter units', .FALSE. )
   CALL ReadCom ( UnIn, TwrFile, 'distributed tower parameters' )
   CALL ReadCom ( UnIn, TwrFile, 'distributed-tower-parameter names' )
   CALL ReadCom ( UnIn, TwrFile, 'distributed-tower-parameter units' )
!bjj End of proposed change


   ! Allocate the input arrays.

ALLOCATE ( HtFract(NTwInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the HtFract array.' )
ENDIF

ALLOCATE ( TMassDen(NTwInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TMassDen array.' )
ENDIF

ALLOCATE ( TwFAStif(NTwInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TwFAStif array.' )
ENDIF

ALLOCATE ( TwSSStif(NTwInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TwSSStif array.' )
ENDIF

ALLOCATE ( TwGJStif(NTwInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TwGJStif array.' )
ENDIF

ALLOCATE ( TwEAStif(NTwInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TwEAStif array.' )
ENDIF

ALLOCATE ( TwFAIner(NTwInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TwFAIner array.' )
ENDIF

ALLOCATE ( TwSSIner(NTwInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TwSSIner array.' )
ENDIF

ALLOCATE ( TwFAcgOf(NTwInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TwFAcgOf array.' )
ENDIF

ALLOCATE ( TwSScgOf(NTwInpSt) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TwSScgOf array.' )
ENDIF


IF ( Echo )  THEN

   IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, read in all the cols.

      Frmt = "( '  Stat     HtFract    TMassDen    TwFAStif    TwSSStif"// &
             "    TwGJStif    TwEAStif    TwFAIner    TwSSIner    TwFAcgOf    TwSScgOf' )"
      WRITE (UnEc,Frmt)
      Frmt = "( '  ----     -------    --------    --------    --------"// &
             "    --------    --------    --------    --------    --------    --------' )"
      WRITE (UnEc,Frmt)
      Frmt = '( I5, 1X, 10( 2X, '//TRIM( OutFmt )//') )'

   ELSE                                                     ! Only FAST will be run; thus, read in only the first 4 cols.

      Frmt = "( '  Stat     HtFract    TMassDen    TwFAStif    TwSSStif' )"
      WRITE (UnEc,Frmt)
      Frmt = "( '  ----     -------    --------    --------    --------' )"
      WRITE (UnEc,Frmt)
      Frmt = '( I5, 1X,  4( 2X, '//TRIM( OutFmt )//') )'

   ENDIF

ENDIF


DO I=1,NTwInpSt

   IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, read in all the cols.
      READ (UnIn,*,IOSTAT=IOS)  HtFract(I), TMassDen(I), TwFAStif(I), TwSSStif(I), &
                                TwGJStif(I), TwEAStif(I), TwFAIner(I), TwSSIner(I), TwFAcgOf(I), TwSScgOf(I)
   ELSE                                                     ! Only FAST will be run; thus, read in only the first 4 cols.
      READ (UnIn,*,IOSTAT=IOS)  HtFract(I), TMassDen(I), TwFAStif(I), TwSSStif(I)
   ENDIF

!bjj start of proposed change
!rm   CALL CheckIOS ( IOS, TwrFile, 'line '//TRIM( Int2LStr( I ) )//' of the tower distributed properties', Numeric )
   CALL CheckIOS ( IOS, TwrFile, 'line '//TRIM( Int2LStr( I ) )//' of the tower distributed properties', NumType )
!bjj end of proposed change

   IF ( Echo )  THEN
      IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, read in all the cols.
         WRITE (UnEc,Frmt)  I, HtFract(I), TMassDen(I), TwFAStif(I), TwSSStif(I), &
                               TwGJStif(I), TwEAStif(I), TwFAIner(I), TwSSIner(I), TwFAcgOf(I), TwSScgOf(I)
      ELSE                                                     ! Only FAST will be run; thus, read in only the first 4 cols.
         WRITE (UnEc,Frmt)  I, HtFract(I), TMassDen(I), TwFAStif(I), TwSSStif(I)
      ENDIF
   ENDIF

   IF ( I == 1 )  THEN
      IF ( HtFract(I) /= 0.0 )  CALL ProgAbort ( ' HtFract(1) must be 0.0.' )
   ELSEIF ( ( I == NTwInpSt ) .AND. ( I /= 1 ) )  THEN
      IF ( HtFract(I) /= 1.0 )  CALL ProgAbort ( ' HtFract('//TRIM( Int2LStr( I ) )//') must be 1.0.' )
   ELSE
      IF ( HtFract(I) <= HtFract(I-1) )  &
         CALL ProgAbort ( ' HtFract('//TRIM( Int2LStr( I ) )//') greater than '//TRIM( Flt2LStr( HtFract(I-1) ) )//'.' )
   ENDIF

   IF ( TMassDen(I) <= 0.0 )  &
      CALL ProgAbort ( ' TMassDen('//TRIM( Int2LStr( I ) )//') must be greater than zero.' )

   IF ( TwFAStif(I) <= 0.0 )  &
      CALL ProgAbort ( ' TwFAStif('//TRIM( Int2LStr( I ) )//') must be greater than zero.' )

   IF ( TwSSStif(I) <= 0.0 )  &
      CALL ProgAbort ( ' TwSSStif('//TRIM( Int2LStr( I ) )//') must be greater than zero.' )

   IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, read in all the cols.

      IF ( TwGJStif(I) <= 0.0 )  &
         CALL ProgAbort ( ' TwGJStif('//TRIM( Int2LStr( I ) )//') must be greater than zero.' )

      IF ( TwEAStif(I) <= 0.0 )  &
         CALL ProgAbort ( ' TwEAStif('//TRIM( Int2LStr( I ) )//') must be greater than zero.' )

      IF ( TwFAIner(I) <  0.0 )  &
         CALL ProgAbort ( ' TwFAIner('//TRIM( Int2LStr( I ) )//') must not be less than zero.' )

      IF ( TwSSIner(I) <  0.0 )  &
         CALL ProgAbort ( ' TwSSIner('//TRIM( Int2LStr( I ) )//') must not be less than zero.' )

   ENDIF


   ! Apply the adjustment factors to the elemental data.

   TMassDen(I) = TMassDen(I)*AdjTwMa
   TwFAStif(I) = TwFAStif(I)*AdjFASt
   TwSSStif(I) = TwSSStif(I)*AdjSSSt

ENDDO ! I



!  -------------- TOWER FORE-AFT MODE SHAPES -----------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm CALL SkipComment ( UnIn, TwrFile, '', Echo  )
   CALL ReadCom ( UnIn, TwrFile, 'tower fore-aft mode shapes' )
!bjj End of proposed change


   ! TwFAM1Sh - Tower fore-aft mode-1 shape coefficients.

!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Allow the mode shape to be described by a polynomial greater than sixth
!eab   order. Do this by setting the length of the mode shape coefficient array
!eab   to one less than the order of the polynomial.
!remove6.10bCALL ReadAryLines ( UnIn, TwrFile, TwFAM1Sh, 5, 'TwFAM1Sh', 'Tower fore-aft mode-1 shape coefficients' )
CALL ReadAryLines ( UnIn, TwrFile, TwFAM1Sh, PolyOrd-1, 'TwFAM1Sh', 'Tower fore-aft mode-1 shape coefficients' )
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.


TopDispl = 0.0

!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Loop through all mode shape coefficients.
!remove6.10bDO I=2,6
DO I=2,PolyOrd
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.
   TopDispl = TopDispl + TwFAM1Sh(I)
ENDDO ! I

IF ( ABS( TopDispl - 1.0 ) > 0.001 )  CALL ProgAbort ( ' Tower fore-aft mode-1 shape coefficients must add to 1.0.' )


   ! TwFAM2Sh - Tower fore-aft mode-2 shape coefficients.

!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Allow the mode shape to be described by a polynomial greater than sixth
!eab   order. Do this by setting the length of the mode shape coefficient array
!eab   to one less than the order of the polynomial.
!remove6.10bCALL ReadAryLines ( UnIn, TwrFile, TwFAM2Sh, 5, 'TwFAM2Sh', 'Tower fore-aft mode-2 shape coefficients' )
CALL ReadAryLines ( UnIn, TwrFile, TwFAM2Sh, PolyOrd-1, 'TwFAM2Sh', 'Tower fore-aft mode-2 shape coefficients' )
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.

TopDispl = 0.0

!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Loop through all mode shape coefficients.
!remove6.10bDO I=2,6
DO I=2,PolyOrd
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.
   TopDispl = TopDispl + TwFAM2Sh(I)
ENDDO ! I

IF ( ABS( TopDispl - 1.0 ) > 0.001 )  CALL ProgAbort ( ' Tower fore-aft mode-2 shape coefficients must add to 1.0.' )



!  -------------- TOWER SIDE-TO-SIDE MODE SHAPES -------------------------------


   ! Skip the comment line.

!bjj Start of proposed change vXX NWTC_Lib
!rm CALL SkipComment ( UnIn, TwrFile, 'tower side-to-side mode shapes', Echo  )
   CALL ReadCom ( UnIn, TwrFile, 'tower side-to-side mode shapes' )
!bjj End of proposed change



   ! TwSSM1Sh - Tower side-to-side mode-1 shape coefficients.


!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Allow the mode shape to be described by a polynomial greater than sixth
!eab   order. Do this by setting the length of the mode shape coefficient array
!eab   to one less than the order of the polynomial.
!remove6.10bCALL ReadAryLines ( UnIn, TwrFile, TwSSM1Sh, 5, 'TwSSM1Sh', 'Tower side-to-side mode-1 shape coefficients' )
CALL ReadAryLines ( UnIn, TwrFile, TwSSM1Sh, PolyOrd-1, 'TwSSM1Sh', 'Tower side-to-side mode-1 shape coefficients' )
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.

TopDispl = 0.0

!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Loop through all mode shape coefficients.
!remove6.10bDO I=2,6
DO I=2,PolyOrd
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.
   TopDispl = TopDispl + TwSSM1Sh(I)
ENDDO ! I

IF ( ABS( TopDispl - 1.0 ) > 0.001 )  CALL ProgAbort ( ' Tower side-to-side mode-1 shape coefficients must add to 1.0.' )


   ! TwSSM2Sh - Tower side-to-side mode-2 shape coefficients.

!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Allow the mode shape to be described by a polynomial greater than sixth
!eab   order. Do this by setting the length of the mode shape coefficient array
!eab   to one less than the order of the polynomial.
!remove6.10bCALL ReadAryLines ( UnIn, TwrFile, TwSSM2Sh, 5, 'TwSSM2Sh', 'Tower side-to-side mode-2 shape coefficients' )
CALL ReadAryLines ( UnIn, TwrFile, TwSSM2Sh, PolyOrd-1, 'TwSSM2Sh', 'Tower side-to-side mode-2 shape coefficients' )
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.

TopDispl = 0.0

!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Loop through all mode shape coefficients.
!remove6.10bDO I=2,6
DO I=2,PolyOrd
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.
   TopDispl = TopDispl + TwSSM2Sh(I)
ENDDO ! I

IF ( ABS( TopDispl - 1.0 ) > 0.001 )  CALL ProgAbort ( ' Tower side-to-side mode-2 shape coefficients must add to 1.0.' )


   ! Close the tower file.

CLOSE ( UnIn )


RETURN
END SUBROUTINE GetTower
!=======================================================================
SUBROUTINE Input


   ! This routine reads the input files and does some preliminary processing.


   ! FAST Modules:

USE                             Blades
USE                             Constants
USE                             DOFs
USE                             DriveTrain
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Put in some logic to ensure that the hydrodynamic loads are time invariant
!jmj   when linearizing a model:
USE                             EnvCond
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
USE                             Features
USE                             General
USE                             InitCond
USE                             Linear
USE                             Modes
USE                             NacelleYaw
USE                             Output
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Put in some logic to ensure that the hydrodynamic loads are time invariant
!jmj   when linearizing a model:
USE                             Platform
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
!bjj rm NWTC_Library: USE                             Precision
USE                             RotorFurling
USE                             SimCont
USE                             TailAero
USE                             TailFurling
USE                             TeeterVars
USE                             TipBrakes
USE                             Tower
USE                             TurbConf
USE                             TurbCont


   ! AeroDyn Modules:
!bjj STart of proposed change AD_v12.70a-bjj
!rmUSE                             Airfoil
!rmUSE                             InducedVel
!rmUSE                             Precision
!rmUSE                             Switch
USE                             Airfoil,        ONLY: NumFoil
USE                             InducedVel,     ONLY: AToler
!bjj rm NWTC_Library: USE                             Precision
!USE                             Switch,         ONLY: DSTALL, DYNINFL, DYNINIT, FFWindFlag, FDWindFlag, CTWindFlag, UsrWndFlag
USE                             Switch,         ONLY: DSTALL, DYNINFL, DYNINIT
!bjj End of proposed change

!bjj start of proposed change ad v12.70w
!USE                             InflowWind !accessed through aerodyn?
!bjj end of proposed change

!bjj STart of proposed change vXX
USE                             Noise  !NoiseInput()
!bjj end of proposed change

IMPLICIT                        NONE


   ! Local variables.

REAL(ReKi)                   :: ComDenom                                        ! Common denominator used in computation of TEC model coefficients
REAL(ReKi)                   :: SumCosPreC                                      ! SUM( CosPreC(K) ) for K = 1,NumBl

INTEGER(4)                   :: I                                               ! A generic index for DO loops.
INTEGER(4)                   :: K                                               ! Index for blade number.
INTEGER(4)                   :: Sttus                                           ! Status of an attempted array allocation.

!bjj start of proposed change AD v12.70w
!these were going to be used, but I replaced them with another function
!REAL(ReKi)                   :: InputPosition(3)
!REAL(ReKi)                   :: WindStDev(3)
!INTEGER                      :: ErrStat
!REAL(ReKi), PARAMETER        :: TOL = 1E-5                                       ! The maximum amount of standard deviation allowed before wind is "non-constant"
!bjj end of proposed change

   ! User-defined functions.

!bjj rm AD 12.70b CHARACTER(15), EXTERNAL      :: Flt2LStr                                        ! A function to convert a floating-point number to a left-justified string.
!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.



!  -------------- PRIMARY PARAMETERS -------------------------------------------


   ! Read the primary parameter file:

CALL GetPrimary


   ! Read the platform parameter file if necessary, calculate some parameters
   !   that are not input directly, and convert units where appropriate:

IF ( ( PtfmModel == 1 ) .OR. ( PtfmModel == 2 ) .OR. ( PtfmModel == 3 ) )  THEN

   CALL GetPtfm

   PtfmRoll  = PtfmRoll *D2R
   PtfmPitch = PtfmPitch*D2R
   PtfmYaw   = PtfmYaw  *D2R

   rZYzt     = PtfmRef  - PtfmCM

ENDIF


   ! Read in the furling parameter file if necessary, calculate some parameters
   !   that are not input directly, and convert units where appropriate:

IF ( Furling )  THEN

   CALL GetFurl

   RotFurl   = RotFurl  *D2R
   TailFurl  = TailFurl *D2R

   ShftSkew  = ShftSkew *D2R

   TFinSkew  = TFinSkew *D2R
   TFinTilt  = TFinTilt *D2R
   TFinBank  = TFinBank *D2R

   RFrlSkew  = RFrlSkew *D2R
   RFrlTilt  = RFrlTilt *D2R

   TFrlSkew  = TFrlSkew *D2R
   TFrlTilt  = TFrlTilt *D2R

   RFrlUSSP  = RFrlUSSP *D2R
   RFrlDSSP  = RFrlDSSP *D2R
   RFrlUSDP  = RFrlUSDP *D2R
   RFrlDSDP  = RFrlDSDP *D2R

   TFrlUSSP  = TFrlUSSP *D2R
   TFrlDSSP  = TFrlDSSP *D2R
   TFrlUSDP  = TFrlUSDP *D2R
   TFrlDSDP  = TFrlDSDP *D2R

   CShftSkew = COS( ShftSkew )
   SShftSkew = SIN( ShftSkew )

   CTFinSkew = COS( TFinSkew )
   STFinSkew = SIN( TFinSkew )
   CTFinTilt = COS( TFinTilt )
   STFinTilt = SIN( TFinTilt )
   CTFinBank = COS( TFinBank )
   STFinBank = SIN( TFinBank )

   CRFrlSkew = COS( RFrlSkew )
   SRFrlSkew = SIN( RFrlSkew )
   CRFrlTilt = COS( RFrlTilt )
   SRFrlTilt = SIN( RFrlTilt )

   CTFrlSkew = COS( TFrlSkew )
   STFrlSkew = SIN( TFrlSkew )
   CTFrlTilt = COS( TFrlTilt )
   STFrlTilt = SIN( TFrlTilt )

   CRFrlSkw2 = CRFrlSkew*CRFrlSkew
   SRFrlSkw2 = SRFrlSkew*SRFrlSkew
   CSRFrlSkw = CRFrlSkew*SRFrlSkew
   CRFrlTlt2 = CRFrlTilt*CRFrlTilt
   SRFrlTlt2 = SRFrlTilt*SRFrlTilt
   CSRFrlTlt = CRFrlTilt*SRFrlTilt

   CTFrlSkw2 = CTFrlSkew*CTFrlSkew
   STFrlSkw2 = STFrlSkew*STFrlSkew
   CSTFrlSkw = CTFrlSkew*STfrlSkew
   CTFrlTlt2 = CTFrlTilt*CTFrlTilt
   STFrlTlt2 = STFrlTilt*STFrlTilt
   CSTFrlTlt = CTFrlTilt*STFrlTilt

   rWIxn     = BoomCMxn - TFrlPntxn
   rWIyn     = BoomCMyn - TFrlPntyn
   rWIzn     = BoomCMzn - TFrlPntzn

   rWJxn     = TFinCMxn - TFrlPntxn
   rWJyn     = TFinCMyn - TFrlPntyn
   rWJzn     = TFinCMzn - TFrlPntzn

   rWKxn     = TFinCPxn - TFrlPntxn
   rWKyn     = TFinCPyn - TFrlPntyn
   rWKzn     = TFinCPzn - TFrlPntzn

   rVDxn     = RFrlCMxn - RFrlPntxn
   rVDyn     = RFrlCMyn - RFrlPntyn
   rVDzn     = RFrlCMzn - RFrlPntzn

   rVPxn     =          - RFrlPntxn
   rVPyn     = Yaw2Shft - RFrlPntyn

   SQRTTFinA = SQRT( TFinArea )

ENDIF

rVPzn        = Twr2Shft - RFrlPntzn ! This computation must remain outside of the IF...ENDIF since it must also be computed for nonfurling machines.
rVIMUxn      =  NcIMUxn - RFrlPntxn ! "
rVIMUyn      =  NcIMUyn - RFrlPntyn ! "
rVIMUzn      =  NcIMUzn - RFrlPntzn ! "


   ! Calculate some parameters that are not input directly.  Convert units if appropriate.

DT24      = DT/24.0                                                             ! Time-step parameter needed for Solver().
rZT0zt    = TwrRBHt + PtfmRef - TwrDraft                                        ! zt-component of position vector rZT0.
RefTwrHt  = TowerHt + PtfmRef                                                   ! Vertical distance between FAST's undisplaced tower height (variable TowerHt) and FAST's inertia frame reference point (variable PtfmRef).
TwrFlexL  = TowerHt + TwrDraft - TwrRBHt                                        ! Height / length of the flexible portion of the tower.
BldFlexL  = TipRad             - HubRad                                         ! Length of the flexible portion of the blade.
TwoPiNB   = TwoPi/NumBl                                                         ! 2*Pi/NumBl is used in RtHS().
RotSpeed  = RotSpeed*RPM2RPS                                                    ! Rotor speed in rad/sec.
NacYaw    = D2R*NacYaw                                                          ! Nacelle yaw in radians.
NacYawF   = D2R*NacYawF                                                         ! Final nacelle yaw (after override yaw maneuver) in radians.
YawNeut   = D2R*YawNeut                                                         ! Neutral yaw in radians.
ShftTilt  = ShftTilt*D2R                                                        ! Rotor shaft tilt in radians.
CShftTilt = COS( ShftTilt )
SShftTilt = SIN( ShftTilt )
FASTHH    = TowerHt + Twr2Shft + OverHang*SShftTilt
GBoxEff   = GBoxEff*0.01
GenEff    = GenEff *0.01
SpdGenOn  = SpdGenOn*RPM2RPS
TBDepISp  = TBDepISp*RPM2RPS
THSSBrFl  = THSSBrDp + HSSBrDT

IF ( VSContrl == 1 )  THEN       ! Simple variable-speed control

   VS_RtGnSp = VS_RtGnSp*  RPM2RPS                                                                    ! Convert rated speed to rad/sec.
   VS_Rgn2K  = VS_Rgn2K /( RPM2RPS*RPM2RPS )                                                          ! Convert Region 2 torque constant to Nm/(rad/sec)^2.
   VS_SySp   = VS_RtGnSp/( 1.0 +  0.01*VS_SlPc )                                                      ! Synchronous speed of region 2 1/2 induction generator.
!BJJ START OF PROPOSED CHANGE: CERTTEST DEBUG FAIL
!RM   VS_Slope  = VS_RtTq  /( VS_RtGnSp - VS_SySp )                                                      ! Torque/speed slope of region 2 1/2 induction generator.
!RM!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!RM!jmj Modify the simple variable speed control parameters to ensure that:
!RM!jmj    (1) VS_RtGnSp cannot equal zero and
!RM!jmj    (2) VS_TrGnSp equals VS_SySp when VS_Rgn2K equals zero.
!RM!remove6.02a   VS_TrGnSp = ( VS_Slope - SQRT( VS_Slope*( VS_Slope - 4.0*VS_Rgn2K*VS_SySp ) ) )/( 2.0*VS_Rgn2K )      ! Transitional generator speed between regions 2 and 2 1/2.
!RM   IF ( VS_Rgn2K == 0.0 )  THEN  ! .TRUE. if the Region 2 torque is flat, and thus, the denominator in the ELSE condition is zero
!RM      VS_TrGnSp = VS_SySp                                                                                ! Transitional generator speed between regions 2 and 2 1/2.
!RM   ELSE                          ! .TRUE. if the Region 2 torque is quadratic with speed
!RM      VS_TrGnSp = ( VS_Slope - SQRT( VS_Slope*( VS_Slope - 4.0*VS_Rgn2K*VS_SySp ) ) )/( 2.0*VS_Rgn2K )   ! Transitional generator speed between regions 2 and 2 1/2.
!RM   ENDIF
!RM!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
   IF ( VS_SlPc < SQRT(EPSILON(VS_SlPc) ) ) THEN                                                      ! We don't have a region 2 so we'll use VS_TrGnSp = VS_RtGnSp
      VS_Slope = 9999.9
      VS_TrGnSp = VS_RtGnSp
   ELSE
      VS_Slope  = VS_RtTq  /( VS_RtGnSp - VS_SySp )                                                   ! Torque/speed slope of region 2 1/2 induction generator.
      IF ( ABS(VS_Rgn2K) < EPSILON(VS_SlPc) )  THEN  ! .TRUE. if the Region 2 torque is flat, and thus, the denominator in the ELSE condition is zero
         VS_TrGnSp = VS_SySp                                                                                ! Transitional generator speed between regions 2 and 2 1/2.
      ELSE                          ! .TRUE. if the Region 2 torque is quadratic with speed
         VS_TrGnSp = ( VS_Slope - SQRT( VS_Slope*( VS_Slope - 4.0*VS_Rgn2K*VS_SySp ) ) )/( 2.0*VS_Rgn2K )   ! Transitional generator speed between regions 2 and 2 1/2.
      ENDIF
   END IF
!write(*,'("VS_Slope: ",i2,1x,700(G15.7,1X))') 2, VS_Slope, SQRT( VS_Slope*( VS_Slope - 4.0*VS_Rgn2K*VS_SySp ) ) , VS_Slope - 4.0*VS_Rgn2K*VS_SySp, 2.0*VS_Rgn2K
!BJJ END OF PROPOSED CHANGE: CERTTEST DEBUG FAIL


ELSEIF ( GenModel == 1 )  THEN   ! Simple induction generator

   SIG_SySp  = SIG_SySp*RPM2RPS                                                 ! Convert synchronous speed to rad/sec.
   SIG_RtSp  = SIG_SySp*( 1.0 + 0.01*SIG_SlPc )                                 ! Rated speed.
   SIG_POSl  = SIG_PORt*( SIG_RtSp - SIG_SySp )                                 ! Pullout slip.
   SIG_POTq  = SIG_RtTq*SIG_PORt                                                ! Pullout torque.
   SIG_Slop  = SIG_RtTq/( SIG_RtSp - SIG_SySp )                                 ! SIG torque/speed slope.

ELSEIF ( GenModel == 2 )  THEN   ! Thevenin-equivalent generator

   ComDenom  = TEC_SRes**2 + ( TEC_SLR + TEC_MR )**2                            ! common denominator used in many of the following equations
   TEC_Re1   = TEC_SRes*( TEC_MR**2 )/ComDenom                                  ! Thevenin's equivalent stator resistance (ohms)
   TEC_Xe1   = TEC_MR*( TEC_SRes**2 + TEC_SLR*( TEC_SLR + TEC_MR) )/ComDenom    ! Thevenin's equivalent stator leakage reactance (ohms)
   TEC_V1a   = TEC_MR*TEC_VLL/SQRT( 3.0*ComDenom )                              ! Thevenin equivalent source voltage.
   TEC_SySp  = 4.0*Pi*TEC_Freq/TEC_NPol                                         ! Thevenin equivalent synchronous speed.
   TEC_K1    = ( TEC_Xe1 + TEC_RLR )**2                                         ! Thevenin equivalent K1 term.
   TEC_K2    = ( TEC_MR**2 )/ComDenom                                           ! Thevenin equivalent K2 term.
   TEC_A0    = TEC_RRes*TEC_K2/TEC_SySp                                         ! Thevenin equivalent A0 term.
   TEC_C0    = TEC_RRes**2                                                      ! Thevenin equivalent C0 term.
   TEC_C1    = -2.0*TEC_Re1*TEC_RRes                                            ! Thevenin equivalent C1 term.
   TEC_C2    = TEC_Re1**2 + TEC_K1                                              ! Thevenin equivalent C2 term.

ENDIF


   ! Calculate some array dimensions.

IF ( NumBl == 2 )  THEN
   NDOF = 22
ELSE
   NDOF = 24
ENDIF

NAug = NDOF + 1


   ! ALLOCATE some arrays:

ALLOCATE ( BlPitchInit(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort(' Error allocating memory for the BlPitchInit array.')
ENDIF

ALLOCATE ( BegPitMan(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort(' Error allocating memory for the BegPitMan array.')
ENDIF

ALLOCATE ( TTpBrFl(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort(' Error allocating memory for the TTpBrFl array.')
ENDIF

ALLOCATE ( CosPreC(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the CosPreC array.' )
ENDIF

ALLOCATE ( SinPreC(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the SinPreC array.' )
ENDIF


   ! Do some things for the 2-blader.

IF ( NumBl == 2 )  THEN
   TeetDefl = TeetDefl*D2R
   Delta3   = Delta3 *D2R
   CosDel3  = COS( Delta3 )
   SinDel3  = SIN( Delta3 )
   TeetSStP = TeetSStP*D2R
   TeetDmpP = TeetDmpP*D2R
   TeetHStP = TeetHStP*D2R
ENDIF


   ! Initialize this variable to zero:

SumCosPreC = 0.0

DO K=1,NumBl
   BlPitch    (K) = BlPitch (K)*D2R
   BlPitchF   (K) = BlPitchF(K)*D2R
   BlPitchInit(K) = BlPitch (K)
   BegPitMan  (K) = .TRUE.
   TTpBrFl    (K) = TTpBrDp (K) + TpBrDT
   Precone    (K) = Precone (K)*D2R
   CosPreC    (K) = COS( Precone(K) )
   SinPreC    (K) = SIN( Precone(K) )
   SumCosPreC     = SumCosPreC + CosPreC(K)
ENDDO ! K


   ! Calculate the average tip radius normal to the shaft (AvgNrmTpRd)
   !   and the swept area of the rotor (ProjArea):

AvgNrmTpRd = TipRad*SumCosPreC/NumBl   ! Average tip radius normal to the saft.
ProjArea   = Pi*( AvgNrmTpRd**2 )      ! Swept area of the rotor projected onto the rotor plane (the plane normal to the low-speed shaft).



!  -------------- TOWER PARAMETERS ---------------------------------------------


   ! Read the tower data.

CALL GetTower


   ! Check to see if all TwrGagNd(:) analysis points are existing analysis points:

DO I=1,NTwGages
   IF ( ( TwrGagNd(I) < 1 ) .OR. ( TwrGagNd(I) > TwrNodes ) )  &
      CALL ProgAbort  ( ' All TwrGagNd values must be between 1 and '//TRIM( Int2LStr( TwrNodes ) )//' (inclusive).' )
ENDDO ! I


!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Check the value of TwrRBHt in routines GetPtfm() and GetPrimary() instead
!jmj   of in routine Input():
!remove6.02a   ! Check value of TwrRBHt against TowerHt and TwrDraft:
!remove6.02a
!remove6.02aIF ( ( TwrRBHt < 0.0 ) .OR. ( TwrRBHt >= ( TowerHt + TwrDraft ) ) )  &
!remove6.02a   CALL ProgAbort ( ' TwrRBHt must be greater or equal to 0 and less than TowerHt + TwrDraft.' )
!remove6.02a
!remove6.02a
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

   ! Allocate arrays to hold tower data at the analysis nodes.

ALLOCATE ( HNodesNorm(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the HNodesNorm array.' )
ENDIF

ALLOCATE ( HNodes(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the HNodes array.' )
ENDIF

ALLOCATE ( DHNodes(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the DHNodes array.' )
ENDIF

ALLOCATE ( MassT(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the MassT array.' )
ENDIF

ALLOCATE ( StiffTFA(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the StiffTFA array.' )
ENDIF

ALLOCATE ( StiffTSS(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the StiffTSS array.' )
ENDIF

ALLOCATE ( StiffTGJ(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the StiffTGJ array.' )
ENDIF

ALLOCATE ( StiffTEA(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the StiffTEA array.' )
ENDIF

ALLOCATE ( InerTFA(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the InerTFA array.' )
ENDIF

ALLOCATE ( InerTSS(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the InerTSS array.' )
ENDIF

ALLOCATE ( cgOffTFA(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the cgOffTFA array.' )
ENDIF

ALLOCATE ( cgOffTSS(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the cgOffTSS array.' )
ENDIF

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:

ALLOCATE ( DiamT(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the DiamT array.' )
ENDIF
DiamT(:) = TwrDiam

ALLOCATE ( CAT(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the CAT array.' )
ENDIF
CAT(:) = TwrCA

ALLOCATE ( CDT(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the CDT array.' )
ENDIF
CDT(:) = TwrCD
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

   ! Interpolate tower data for analysis nodes.

CALL InterpTwr



!  -------------- AERODYN PARAMETERS -------------------------------------------


   ! Get the AeroDyn input now.

CALL AeroInput             ! Read in the ADFile


   ! Make sure TFinNFoil is an existing airfoil number:

IF ( ( TFinNFoil < 1 ) .OR. ( TFinNFoil > NumFoil ) )  &
   CALL ProgAbort ( ' TFinNFoil must be between 1 and NumFoil (inclusive).' )


   ! Check to see if all BldGagNd(:) analysis points are existing analysis points:

DO I=1,NBlGages
   IF ( ( BldGagNd(I) < 1 ) .OR. ( BldGagNd(I) > BldNodes ) )  &
      CALL ProgAbort  ( ' All BldGagNd values must be between 1 and '//TRIM( Int2LStr( BldNodes ) )//' (inclusive).' )
ENDDO ! I


   ! Check to see if PSpnElN is an existing analysis point:

IF ( ( PSpnElN < 1 ) .OR. ( PSpnElN > BldNodes ) )  &
   CALL ProgAbort(' PSpnElN must be between 1 and '//TRIM( Int2LStr( BldNodes ) )//' (inclusive).' )

!bjj start of proposed change v6.02d-bjj
!this check has been moved to AeroDYn
!rmCALL CheckRComp            ! Check to see if RNodes and DRNodes are compatible
!bjj end of proposedchange


   ! Convert RNodes to be relative to the hub:

RNodes = RNodes - HubRad   ! Radius to blade analysis nodes relative to root ( 0 < RNodes(:) < BldFlexL )


   ! Compute the index for the blade tip and tower top nodes:

TipNode  = BldNodes + 1
TTopNode = TwrNodes + 1



!  -------------- BLADE PARAMETERS ---------------------------------------------


   ! Allocate arrays to hold blade data at the analysis nodes.

ALLOCATE ( StiffBE(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the StiffBE array.' )
ENDIF

ALLOCATE ( RNodesNorm(BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the RNodesNorm array.' )
ENDIF

ALLOCATE ( MassB(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the MassB array.' )
ENDIF

ALLOCATE ( StiffBF(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the StiffBF array.' )
ENDIF

ALLOCATE ( AeroCent(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the AeroCent array.' )
ENDIF

ALLOCATE ( ThetaS(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the ThetaS array.' )
ENDIF

ALLOCATE ( CThetaS(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the CThetaS array.' )
ENDIF

ALLOCATE ( SThetaS(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the SThetaS array.' )
ENDIF

ALLOCATE ( StiffBGJ(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the StiffBGJ array.' )
ENDIF

ALLOCATE ( StiffBEA(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the StiffBEA array.' )
ENDIF

ALLOCATE ( BAlpha(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BAlpha array.' )
ENDIF

ALLOCATE ( InerBFlp(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the InerBFlp array.' )
ENDIF

ALLOCATE ( InerBEdg(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the InerBEdg array.' )
ENDIF

ALLOCATE ( RefAxisxb(NumBl,TipNode) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the RefAxisxb array.' )
ENDIF

ALLOCATE ( RefAxisyb(NumBl,TipNode) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the RefAxisyb array.' )
ENDIF

ALLOCATE ( cgOffBFlp(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the cgOffBFlp array.' )
ENDIF

ALLOCATE ( cgOffBEdg(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the cgOffBEdg array.' )
ENDIF

ALLOCATE ( EAOffBFlp(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the EAOffBFlp array.' )
ENDIF

ALLOCATE ( EAOffBEdg(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the EAOffBEdg array.' )
ENDIF

ALLOCATE ( CalcBModes(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the CalcBModes array.' )
ENDIF

ALLOCATE ( BldEDamp(NumBl,1) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BldEDamp array.' )
ENDIF

ALLOCATE ( BldFDamp(NumBl,2) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BldFDamp array.' )
ENDIF

ALLOCATE ( FStTunr(NumBl,2) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the FStTunr array.' )
ENDIF

!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Allow shape coefficient arrays to exceed the sixth position so as to
!eab   accommodate all coefficients of a polynomial of any order.
!remove6.10bALLOCATE ( BldEdgSh(2:6,NumBl) , STAT=Sttus )
ALLOCATE ( BldEdgSh(2:PolyOrd,NumBl) , STAT=Sttus )
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BldEdgSh array.' )
ENDIF

!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Allow shape coefficient arrays to exceed the sixth position so as to
!eab   accommodate all coefficients of a polynomial of any order.
!remove6.10bALLOCATE ( BldFl1Sh(2:6,NumBl) , STAT=Sttus )
ALLOCATE ( BldFl1Sh(2:PolyOrd,NumBl) , STAT=Sttus )
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BldFl1Sh array.' )
ENDIF

!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Allow shape coefficient arrays to exceed the sixth position so as to
!eab   accommodate all coefficients of a polynomial of any order.
!remove6.10bALLOCATE ( BldFl2Sh(2:6,NumBl) , STAT=Sttus )
ALLOCATE ( BldFl2Sh(2:PolyOrd,NumBl) , STAT=Sttus )
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BldFl2Sh array.' )
ENDIF


   ! Define RNodesNorm() which is common to all the blades:

RNodesNorm = RNodes/BldFlexL  ! Normalized radius to analysis nodes relative to hub ( 0 < RNodesNorm(:) < 1 )


   ! Let's work on the data for blade 1.

DO K=1,NumBl


      ! If we didn't just read BldFile(K), read it and interpolate the data.
      ! Otherwise, use data from last time we read it.

   IF ( K == 1 )  THEN                          ! We are on the first blade

      CALL GetBlade  ( K )
      CALL InterpBld ( K )

      DO I=1,BldNodes
         ThetaS (K,I) = D2R* ThetaS(K,I)    ! Convert structural twist to radians.
         CThetaS(K,I) = COS( ThetaS(K,I) )
         SThetaS(K,I) = SIN( ThetaS(K,I) )
      ENDDO ! I

      CalcBModes(K) = CalcBMode
      BldFDamp(K,1) = BldFlDmp(1)
      BldFDamp(K,2) = BldFlDmp(2)
      BldEDamp(K,1) = BldEdDmp(1)
      FStTunr (K,1) = FlStTunr(1)
      FStTunr (K,2) = FlStTunr(2)

   ELSEIF ( BldFile(K) /= BldFile(K-1) )  THEN  ! .TRUE. if this next blade is different

      CALL GetBlade  ( K )
      CALL InterpBld ( K )

      DO I=1,BldNodes
         ThetaS (K,I) = D2R* ThetaS(K,I)    ! Convert structural twist to radians.
         CThetaS(K,I) = COS( ThetaS(K,I) )
         SThetaS(K,I) = SIN( ThetaS(K,I) )
      ENDDO ! I

      CalcBModes(K) = CalcBMode
      BldFDamp(K,1) = BldFlDmp(1)
      BldFDamp(K,2) = BldFlDmp(2)
      BldEDamp(K,1) = BldEdDmp(1)
      FStTunr (K,1) = FlStTunr(1)
      FStTunr (K,2) = FlStTunr(2)

   ELSE                                         ! .TRUE. if this blade is the same as the previous blade

      CalcBModes(K) = CalcBModes(K-1)
      BldFDamp(K,1) = BldFDamp(K-1,1)
      BldFDamp(K,2) = BldFDamp(K-1,2)
      BldEDamp(K,1) = BldEDamp(K-1,1)
      FStTunr (K,1) = FStTunr (K-1,1)
      FStTunr (K,2) = FStTunr (K-1,2)

!eab Start of proposed change.  v6.10b-eab  24-Jul-2008.
!eab Loop through all mode shape coefficients.
!remove6.10b      DO I=2,6
      DO I=2,PolyOrd
!eab End of proposed change.  v6.10b-eab  24-Jul-2008.
         BldEdgSh(I,K) = BldEdgSh(I,K-1)
         BldFl1Sh(I,K) = BldFl1Sh(I,K-1)
         BldFl2Sh(I,K) = BldFl2Sh(I,K-1)
      ENDDO ! I

      DO I=1,BldNodes
         AeroCent(K,I) = AeroCent(K-1,I)
          ThetaS (K,I) =  ThetaS (K-1,I)
         CThetaS (K,I) = CThetaS (K-1,I)
         SThetaS (K,I) = SThetaS (K-1,I)
         MassB   (K,I) = MassB   (K-1,I)
         StiffBF (K,I) = StiffBF (K-1,I)
         StiffBE (K,I) = StiffBE (K-1,I)
         IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, read in all the cols.
            StiffBGJ (K,I) = StiffBGJ (K-1,I)
            StiffBEA (K,I) = StiffBEA (K-1,I)
            BAlpha   (K,I) = BAlpha   (K-1,I)
            InerBFlp (K,I) = InerBFlp (K-1,I)
            InerBEdg (K,I) = InerBEdg (K-1,I)
            RefAxisxb(K,I) = RefAxisxb(K-1,I)
            RefAxisyb(K,I) = RefAxisyb(K-1,I)
            IF ( I == BldNodes )  THEN ! Copy data for the tip also (I know this code is inefficient, but it is only computed once, so who cares!)
               RefAxisxb(K,TipNode) = RefAxisxb(K-1,TipNode)
               RefAxisyb(K,TipNode) = RefAxisyb(K-1,TipNode)
            ENDIF
            cgOffBFlp(K,I) = cgOffBFlp(K-1,I)
            cgOffBEdg(K,I) = cgOffBEdg(K-1,I)
            EAOffBFlp(K,I) = EAOffBFlp(K-1,I)
            EAOffBEdg(K,I) = EAOffBEdg(K-1,I)
         ENDIF
      ENDDO ! I

   ENDIF

ENDDO ! K



!  -------------- NOISE --------------------------------------------------------

IF ( CompNoise .AND. ( AnalMode == 1 ) .AND. ( ADAMSPrep /= 2 ) )  THEN ! We will be computing aerodynamic noise.
!JASON: Change this to "IF ( CompAero .AND. ( AnalMode == 1 ) )  THEN" if you can get ADAMS to compute noise as well as FAST.

!bjj start of proposed change
!rm   CALL NoiseInput                                       ! Read in the noise parameters from NoiseFile.
   CALL NoiseInput(UnIn, NoiseFile)                         ! Read in the noise parameters from NoiseFile.
!bjj end of proposed change

ENDIF



!  -------------- ADAMS --------------------------------------------------------

IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! Create equivalent ADAMS model.

   CALL GetADAMS                                         ! Read in the ADAMS-specific parameters from ADAMSFile.

ENDIF



!  -------------- FAST LINEARIZATION CONTROL -----------------------------------

IF ( ( AnalMode == 2 ) .AND. ( ADAMSPrep /= 2 ) )  THEN  ! Run a FAST linearization analysis

   CALL GetLin                                           ! Read in the FAST linearization parameters from LinFile.


   ! FAST linearization wont work for all possible input settings.
   ! Make sure FAST aborts if any of the following conditions are met:

   ! Conditions on FAST inputs:

   IF ( YCMode /= 0                         )  &
      CALL ProgAbort ( ' FAST can''t linearize a model with yaw control enabled.  Set YCMode to 0.' )
   IF ( TYawManS <= TMax                    )  &
      CALL ProgAbort ( ' FAST can''t linearize a model with time-varying controls.  Set TYawManS > TMax.' )
   IF ( PCMode /= 0                         )  &
      CALL ProgAbort ( ' FAST can''t linearize a model with pitch control enabled.  Set PCMode to 0.' )
   IF ( .NOT. GenTiStr                      )  &
      CALL ProgAbort ( ' FAST can''t linearize a model with time-varying controls.  Set GenTiStr to True and TimGenOn to 0.0.' )
   IF ( TimGenOn /= 0.0                     )  &
      CALL ProgAbort ( ' FAST can''t linearize a model with time-varying controls.  Set GenTiStr to True and TimGenOn to 0.0.' )
   IF ( .NOT. GenTiStp                      )  &
      CALL ProgAbort ( ' FAST can''t linearize a model with time-varying controls.  Set GenTiStp to True and TimGenOf > TMax.' )
   IF ( TimGenOf <= TMax                    )  &
      CALL ProgAbort ( ' FAST can''t linearize a model with time-varying controls.  Set GenTiStp to True and TimGenOf > TMax.' )
   IF ( THSSBrDp <= TMax                    )  &
      CALL ProgAbort ( ' FAST can''t linearize a model during a high-speed shaft brake shutdown event.  Set THSSBrDp > TMax.' )
!JASON:USE THIS CONDITION WHEN YOU ADD CODE/LOGIC FOR TiDynBrk:   IF ( TiDynBrk <= TMax                    )  CALL ProgAbort ( ' FAST can''t linearize a model during a dynamic generator brake shutdown event.  Set TiDynBrk > TMax.' )
   DO K = 1,NumBl       ! Loop through all blades
      IF ( TTpBrDp (K) <= TMax              )  &
         CALL ProgAbort ( ' FAST can''t linearize a model with time-varying controls.'// &
                      '  Set TTpBrDp( '//TRIM(Int2LStr(K))//') > TMax.'                )
      IF ( TBDepISp(K) <= 10.0*RotSpeed     )  &
         CALL ProgAbort ( ' FAST can''t linearize a model with time-varying controls.'// &
                      '  Set TBDepISp('//TRIM(Int2LStr(K))//') >> RotSpeed.'           )
      IF ( TPitManS(K) <= TMax              )  &
         CALL ProgAbort ( ' FAST can''t linearize a model with time-varying controls.'// &
                      '  Set TPitManS('//TRIM(Int2LStr(K))//') > TMax.'                )
   ENDDO                ! K - Blades
   IF ( CompNoise                           )  &
      CALL ProgAbort ( ' FAST can''t linearize a model and compute noise at the same time.  Set CompNoise to False.' )

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Put in some logic to ensure that the hydrodynamic loads are time invariant
!jmj   when linearizing a model:
   IF ( ( WaveMod  /= 0   ) .AND. CompHydro )  &
      CALL ProgAbort ( ' FAST can''t linearize a model with incident wave kinematics.  Set WaveMod to 0.'  )
   IF ( ( RdtnTMax /= 0.0 ) .AND. CompHydro )  &
      CALL ProgAbort ( ' FAST can''t linearize a model with wave radiation damping.  Set RdtnTMax to 0.0.' )
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

   ! NOTE: There is one additional requirement, which is that there must be at
   !       least one DOF enabled in order to run a linearization analysis.
   !       This condition is commented out below.  Instead of here, this
   !       condition is checked in PROGRAM FAST(), since variable NActvDOF has
   !       not been defined yet.  I would like to test all of the conditions
   !       in one place, but oh well :-(.
!   IF ( NActvDOF == 0                       )  CALL ProgAbort ( ' FAST can''t linearize a model with no DOFs.  Enable at least one DOF.' )

   ! Conditions on AeroDyn inputs:
   ! NOTE: I overwrite the value of AToler internally but force the users to
   !       change the values of StallMod and InfModel themselves.  This may
   !       appear contradictory in implementation.  However, the reason I do
   !       this is because I want the users to be aware of the conditions on
   !       StallMod and InfModel, whereas I don't care if they know about the
   !       conditions on AToler (this condition is less important).

   IF ( DSTALL .AND. CompAero                   )  &
      CALL ProgAbort ( ' FAST can''t linearize the model when dynamic stall is engaged.  Set StallMod to STEADY.' )
   IF ( ( DYNINFL .OR. DYNINIT ) .AND. CompAero )  &
      CALL ProgAbort ( ' FAST can''t linearize the model when DYNamic INflow is engaged.  Set InfModel to EQUIL.' ) ! .TRUE. if DYNamic INflow model is engaged.
   IF ( AToler > 1.0E-6                         )  THEN  ! We need a tight requirement on AToler in order to avoid numerical errors in AeroDyn during linearization.  This was discovered K. Stol when developing SymDyn.
      CALL WrScr1( ' NOTE: AToler changed from '//TRIM(Flt2LStr(ATOLER))//                   &
                   ' to 1E-6 in order to reduce numerical errors during FAST linearization.'   )
      AToler = 1.0E-6
   ENDIF
!bjj start of proposed change v12.70w
!rm   IF ( FFWindFlag .AND. CompAero               )  &
!rm      CALL ProgAbort ( ' FAST can''t linearize a model using full-field wind input.  Use a steady hub-height wind input instead.' )
!rm   IF ( FDWindFlag .AND. CompAero               )  &
!rm      CALL ProgAbort ( ' FAST can''t linearize a model using 4-D wind input.  Use a steady hub-height wind input instead.' )
!rm   IF ( CTWindFlag .AND. CompAero               )  &
!rm      CALL ProgAbort ( ' FAST can''t linearize a model using coherent turbulent winds.  Use a steady hub-height wind input instead.' )
!rm   IF ( UsrWndFlag .AND. CompAero               )  &
!rm      CALL ProgAbort ( ' FAST can''t linearize a model using user-defined wind input.  Use a steady hub-height wind input instead.' )

!   IF ( CompAero ) THEN ! check that the wind speed is not varying -- this is checked elsewhere so this is redundant
!      InputPosition = (/ 0.0, 0.0, FASTHH /)
!      WindStDev(:) = WindInf_GetStdDev(REAL(0.0, ReKi), TMax, DT, InputPosition,  ErrStat )
!      IF (ErrStat /= 0) CALL ProgWarn( ' FAST must have a steady reference wind file for linearization.')
!      IF ( ANY(ABS(WindStDev(:)) > TOL) ) CALL ProgAbort( ' Steady winds must be used for linearization, but the wind file has non-zero standard deviation.')
!   END IF

!bjj end of proposed change

   ! Determine whether we will try to find a periodic steady state solution
   !   (rotor spinning) or a static equilibrium solution (rotor parked).  If
   !   periodic, adjust the time increment, DT, so that there are an integer
   !   multiple of them in one Period.  For parked rotors (static equilibrium
   !   solutions), make the 2-norm convergence tests occur only every 1 second.
   !   Also if parked, change NAzimStep to unity:

   IF ( RotSpeed == 0.0 )  THEN        ! Rotor is parked, therefore we will find a static equilibrium position.

      NStep  = CEILING(    1.0 / DT )  ! Make each iteration one second long

      IF ( NAzimStep /= 1 )  CALL WrScr1( ' NOTE: NAzimStep changed from '//TRIM(Int2LStr(NAzimStep))//            &
                                          ' to 1 since RotSpeed = 0 and the steady solution will not be periodic.'   )
      NAzimStep = 1

      IF ( GenDOF         )  CALL WrScr1( ' NOTE: GenDOF changed from True to False since RotSpeed = 0'// &
                                          ' meaning the generator is locked in place.'                      )
      GenDOF = .FALSE.

   ELSE                                ! Rotor is spinning, therefore we will find a periodic steady state solution.

      Period = TwoPi / RotSpeed
      NStep  = CEILING( Period / DT )  ! The number of time steps (an integer) in one period

      DT     = Period / NStep          ! Update DT so that there is an integer multiple of them in one period
      DT24   = DT/24.0                 ! Update DT24 since it is used in SUBROUTINE Solver()

   ENDIF


   ! Allocate the arrays holding the operating point values:

   ALLOCATE ( Qop  (NDOF,NAzimStep) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the Qop array.' )
   ENDIF

   ALLOCATE ( QDop (NDOF,NAzimStep) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the QDop array.' )
   ENDIF

   ALLOCATE ( QD2op(NDOF,NAzimStep) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the QD2op array.' )
   ENDIF

ENDIF

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Move the CALL to SUBROUTINE FAST_IO.f90/ChckOutLst() from
!jmj   FAST_IO.f90/GetPrimary() to FAST_IO.f90/Input():
   ! Check to see if any inputted output channels are ill-conditioned (and if so, Abort)
   !    and set values for OutInd(:) and OutParam(:):

CALL ChckOutLst



!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


!  -------------- FINISHING UP -------------------------------------------------


   ! Close echo file, if appropriate.

IF ( Echo )  CLOSE ( UnEc )



RETURN
END SUBROUTINE Input
!=======================================================================
SUBROUTINE InterpBld ( K )


   ! InterpBld performs a linear interpolation of the input blade data
   ! and writes the result in six arrays with the specified interval.


USE                             Blades
USE                             General
!bjj rm NWTC_Library: USE                             Precision
USE                             TurbConf
!bjj start of proposed change vXX
!USE                             FASTGenSubs  !Interp()
!bjj end of proposed change vXX

IMPLICIT                        NONE


   ! Passed variables:

INTEGER(4), INTENT(IN )      :: K                                               ! Blade number.


   ! Local variables:

INTEGER(4)                   :: Ind                                             ! Index for the node arrays.
INTEGER(4)                   :: Sttus                                           ! Status of an attempted array allocation.
!bjj Start of proposed change vXX
INTEGER                      :: InterpInd                                       ! Index for the interpolated array
!bjj end of proposed change

   ! Global functions:

!bjj rm VXX: REAL(ReKi), EXTERNAL         :: Interp                                          ! A generic function to do the actual interpolation.



   ! Array definitions:

   !    Input      Interp    Description
   !    -----      ------    -----------
   !    BlFract    RNodesNorm Fractional radius (0 at root, 1 at tip)
   !    AerCen     AeroCent   Aerodynamic center (0 at LE, 1 at TE)
   !    StrcTwst   ThetaS     Structural twist
   !    BMassDen   MassB      Lineal mass density
   !    FlpStff    StiffBF    Flapwise stiffness
   !    EdgStff    StiffBE    Edgewise stiffness
   !    GJStff     StiffBGJ   Blade torsional stiffness
   !    EAStff     StiffBEA   Blade extensional stiffness
   !    Alpha      BAlpha     Blade flap/twist coupling coefficient
   !    FlpIner    InerBFlp   Blade flap (about local structural yb-axis) mass inertia per unit length
   !    EdgIner    InerBEdg   Blade edge (about local structural xb-axis) mass inertia per unit length
   !    PrecrvRef  RefAxisxb  Blade offset for defining the reference axis from the pitch axis for precurved blades (along xb-axis)
   !    PreswpRef  RefAxisyb  Blade offset for defining the reference axis from the pitch axis for preswept  blades (along yb-axis)
   !    FlpcgOf    cgOffBFlp  Blade flap mass cg offset
   !    EdgcgOf    cgOffBEdg  Blade edge mass cg offset
   !    FlpEAOf    EAOffBFlp  Blade flap elastic axis offset
   !    EdgEAOf    EAOffBEdg  Blade edge elastic axis offset



   ! Depending on the number of input locations, we'll do this differently.

IF ( NBlInpSt == 1 )  THEN


      ! Maybe we're lucky today and the user specified only one input station
      !  because the blade is a uniform beam.

   DO Ind=1,BldNodes
      AeroCent(K,Ind) = AerCen  (1)
      ThetaS  (K,Ind) = StrcTwst(1)
      MassB   (K,Ind) = BMassDen(1)
      StiffBF (K,Ind) = FlpStff (1)
      StiffBE (K,Ind) = EdgStff (1)
      IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, read in all the cols.
         StiffBGJ (K,Ind) = GJStff   (1)
         StiffBEA (K,Ind) = EAStff   (1)
         BAlpha   (K,Ind) = Alpha    (1)
         InerBFlp (K,Ind) = FlpIner  (1)
         InerBEdg (K,Ind) = EdgIner  (1)
         RefAxisxb(K,Ind) = PrecrvRef(1)
         RefAxisyb(K,Ind) = PreswpRef(1)
         IF ( Ind == BldNodes )  THEN  ! Copy data for the tip also (I know this code is inefficient, but it is only computed once, so who cares!)
            RefAxisxb(K,TipNode) = PrecrvRef(1)
            RefAxisyb(K,TipNode) = PreswpRef(1)
         ENDIF
         cgOffBFlp(K,Ind) = FlpcgOf  (1)
         cgOffBEdg(K,Ind) = EdgcgOf  (1)
         EAOffBFlp(K,Ind) = FlpEAOf  (1)
         EAOffBEdg(K,Ind) = EdgEAOf  (1)
      ENDIF
   ENDDO ! Ind

ELSE

      ! We have more than one input station, so we're just going to have to
      !  interpolate the data.

!bjj Start of proposed change vXX
InterpInd = 1
!bjj End of proposed change vXX

   DO Ind=1,BldNodes
!bjj Start of proposed change vXX
!bjj: it would probably be faster--since they all use the same InterpInd, xVal, and xAry--to interpolate differently
!rm      AeroCent(K,Ind) = Interp( RNodesNorm(Ind), BlFract, AerCen  , NBlInpSt )
!rm      ThetaS  (K,Ind) = Interp( RNodesNorm(Ind), BlFract, StrcTwst, NBlInpSt )
!rm      MassB   (K,Ind) = Interp( RNodesNorm(Ind), BlFract, BMassDen, NBlInpSt )
!rm      StiffBF (K,Ind) = Interp( RNodesNorm(Ind), BlFract, FlpStff , NBlInpSt )
!rm      StiffBE (K,Ind) = Interp( RNodesNorm(Ind), BlFract, EdgStff , NBlInpSt )
      AeroCent(K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, AerCen  , InterpInd, NBlInpSt )
      ThetaS  (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, StrcTwst, InterpInd, NBlInpSt )
      MassB   (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, BMassDen, InterpInd, NBlInpSt )
      StiffBF (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, FlpStff , InterpInd, NBlInpSt )
      StiffBE (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, EdgStff , InterpInd, NBlInpSt )
!bjj End of proposed change vXX
      IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, read in all the cols.
!bjj Start of proposed change vXX
!rm         StiffBGJ (K,Ind) = Interp( RNodesNorm(Ind), BlFract, GJStff   , NBlInpSt )
!rm         StiffBEA (K,Ind) = Interp( RNodesNorm(Ind), BlFract, EAStff   , NBlInpSt )
!rm         BAlpha   (K,Ind) = Interp( RNodesNorm(Ind), BlFract, Alpha    , NBlInpSt )
!rm         InerBFlp (K,Ind) = Interp( RNodesNorm(Ind), BlFract, FlpIner  , NBlInpSt )
!rm         InerBEdg (K,Ind) = Interp( RNodesNorm(Ind), BlFract, EdgIner  , NBlInpSt )
!rm         RefAxisxb(K,Ind) = Interp( RNodesNorm(Ind), BlFract, PrecrvRef, NBlInpSt )
!rm         RefAxisyb(K,Ind) = Interp( RNodesNorm(Ind), BlFract, PreswpRef, NBlInpSt )
         StiffBGJ (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, GJStff   , InterpInd, NBlInpSt )
         StiffBEA (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, EAStff   , InterpInd, NBlInpSt )
         BAlpha   (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, Alpha    , InterpInd, NBlInpSt )
         InerBFlp (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, FlpIner  , InterpInd, NBlInpSt )
         InerBEdg (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, EdgIner  , InterpInd, NBlInpSt )
         RefAxisxb(K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, PrecrvRef, InterpInd, NBlInpSt )
         RefAxisyb(K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, PreswpRef, InterpInd, NBlInpSt )
!bjj End of proposed change vXX
         IF ( Ind == BldNodes )  THEN  ! Copy data for the tip also (I know this code is inefficient, but it is only computed once, so who cares!)
            RefAxisxb(K,TipNode) = PrecrvRef(NBlInpSt)
            RefAxisyb(K,TipNode) = PreswpRef(NBlInpSt)
         ENDIF
!bjj Start of proposed change vXX
!rm         cgOffBFlp(K,Ind) = Interp( RNodesNorm(Ind), BlFract, FlpcgOf  , NBlInpSt )
!rm         cgOffBEdg(K,Ind) = Interp( RNodesNorm(Ind), BlFract, EdgcgOf  , NBlInpSt )
!rm         EAOffBFlp(K,Ind) = Interp( RNodesNorm(Ind), BlFract, FlpEAOf  , NBlInpSt )
!rm         EAOffBEdg(K,Ind) = Interp( RNodesNorm(Ind), BlFract, EdgEAOf  , NBlInpSt )
         cgOffBFlp(K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, FlpcgOf  , InterpInd, NBlInpSt )
         cgOffBEdg(K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, EdgcgOf  , InterpInd, NBlInpSt )
         EAOffBFlp(K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, FlpEAOf  , InterpInd, NBlInpSt )
         EAOffBEdg(K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, EdgEAOf  , InterpInd, NBlInpSt )
!bjj End of proposed change vXX
      ENDIF
   ENDDO ! Ind

ENDIF


   ! Deallocate the distributed input parameters.

DEALLOCATE ( BlFract , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the BlFract array.' )
ENDIF

DEALLOCATE ( AerCen , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the AerCen array.' )
ENDIF

DEALLOCATE ( StrcTwst , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the StrcTwst array.' )
ENDIF

DEALLOCATE ( BMassDen , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the BMassDen array.' )
ENDIF

DEALLOCATE ( FlpStff , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the FlpStff array.' )
ENDIF

DEALLOCATE ( EdgStff , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the EdgStff array.' )
ENDIF

DEALLOCATE ( GJStff , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the GJStff array.' )
ENDIF

DEALLOCATE ( EAStff , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the EAStff array.' )
ENDIF

DEALLOCATE ( Alpha , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the Alpha array.' )
ENDIF

DEALLOCATE ( FlpIner , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the FlpIner array.' )
ENDIF

DEALLOCATE ( EdgIner , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the EdgIner array.' )
ENDIF

DEALLOCATE ( PrecrvRef , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the PrecrvRef array.' )
ENDIF

DEALLOCATE ( PreswpRef , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the PreswpRef array.' )
ENDIF

DEALLOCATE ( FlpcgOf , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the FlpcgOf array.' )
ENDIF

DEALLOCATE ( EdgcgOf , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the EdgcgOf array.' )
ENDIF

DEALLOCATE ( FlpEAOf , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the FlpEAOf array.' )
ENDIF

DEALLOCATE ( EdgEAOf , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the EdgEAOf array.' )
ENDIF


RETURN
END SUBROUTINE InterpBld
!=======================================================================
SUBROUTINE InterpTwr


   ! InterpTwr performs a linear interpolation of the input tower data
   ! and writes the result in four arrays with the specified interval.


USE                             General
!bjj rm NWTC_Library: USE                             Precision
USE                             Tower
USE                             TurbConf

!bjj start of proposed change vXX
!USE                             FASTGenSubs  !Interp()
!bjj end of proposed change vXX

IMPLICIT                        NONE


   ! Local variables:

INTEGER(4)                   :: Ind                                             ! Index for the node arrays.
INTEGER(4)                   :: Sttus                                           ! Status of an attempted array allocation.
!bjj Start of proposed change vXX
INTEGER                      :: InterpInd                                       ! Index for the interpolated array
!bjj end of proposed change


   ! Global functions:

!bjj rm vXX: REAL(ReKi), EXTERNAL         :: Interp                                          ! A generic function to do the actual interpolation.



   ! Array definitions:

   !    Input      Interp    Description
   !    -----      ------    -----------
   !    HtFract    HNodesNorm Fractional height (0 at top of rigid section, 1 at tower top)
   !    TMassDen   MassT      Lineal mass density
   !    TwFAStif   StiffTFA   Tower fore-aft stiffness
   !    TwSSStif   StiffTSS   Tower side-to-side stiffness
   !    TwGJStif   StiffTGJ   Tower torsional stiffness
   !    TwEAStif   StiffTEA   Tower extensional stiffness
   !    TwFAIner   InerTFA    Tower fore-aft (about yt-axis) mass inertia per unit length
   !    TwSSIner   InerTSS    Tower side-to-side (about xt-axis) mass inertia per unit length
   !    TwFAcgOf   cgOffTFA   Tower fore-aft mass cg offset
   !    TwSScgOf   cgOffTSS   Tower side-to-side mass cg offset


   ! Depending on the number of input locations, we'll do this differently.

IF ( NTwInpSt == 1 )  THEN


      ! Maybe we're lucky today and the user specified only one input station
      !  because the tower is a uniform beam.

   DO Ind=1,TwrNodes
      DHNodes  (Ind) = TwrFlexL/TwrNodes   !Let's use constant-spaced nodes for now, but the rest of the code is written to handle variable-spaced nodes--this will be a future input!
      IF ( Ind == 1 ) THEN !Lowest analysis point
         HNodes(Ind) = 0.5*DHNodes(Ind)
      ELSE                 !All other analysis points
         HNodes(Ind) = HNodes( Ind - 1 ) + 0.5*( DHNodes(Ind) + DHNodes( Ind - 1 ) )
      ENDIF
      HNodesNorm(Ind) = HNodes(Ind)/TwrFlexL

      MassT    (Ind) = TMassDen(1)
      StiffTFA (Ind) = TwFAStif(1)
      StiffTSS (Ind) = TwSSStif(1)
      IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, read in all the cols.
         StiffTGJ (Ind) = TwGJStif(1)
         StiffTEA (Ind) = TwEAStif(1)
         InerTFA  (Ind) = TwFAIner(1)
         InerTSS  (Ind) = TwSSIner(1)
         cgOffTFA (Ind) = TwFAcgOf(1)
         cgOffTSS (Ind) = TwSScgOf(1)
      ENDIF
   ENDDO ! Ind

ELSE


      ! We have more than one input station, so we're just going to have to
      !  interpolate the data.
!bjj Start of proposed change vXX
   InterpInd = 1
!bjj End of proposed change vXX

   DO Ind=1,TwrNodes
      DHNodes   (Ind) = TwrFlexL/TwrNodes   !Lets used constant-spaced nodes for now, but the rest of the code is written to handle variable-spaced nodes--this will be a future input!
      IF ( Ind == 1 ) THEN !Lowest analysis point
         HNodes (Ind) = 0.5*DHNodes(Ind)
      ELSE                 !All other analysis points
         HNodes (Ind) = HNodes( Ind - 1 ) + 0.5*( DHNodes(Ind) + DHNodes( Ind - 1 ) )
      ENDIF
      HNodesNorm(Ind) = HNodes(Ind)/TwrFlexL

!bjj Start of proposed change vXX
!bjj: it would probably be faster, since they all use the same InterpInd xVal, and xAry, to interpolate differently
!rm      MassT     (Ind) = Interp( HNodesNorm(Ind), HtFract, TMassDen, NTwInpSt )
!rm      StiffTFA  (Ind) = Interp( HNodesNorm(Ind), HtFract, TwFAStif, NTwInpSt )
!rm      StiffTSS  (Ind) = Interp( HNodesNorm(Ind), HtFract, TwSSStif, NTwInpSt )
      MassT     (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TMassDen, InterpInd, NTwInpSt )
      StiffTFA  (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwFAStif, InterpInd, NTwInpSt )
      StiffTSS  (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwSSStif, InterpInd, NTwInpSt )
!bjj End of proposed change vXX
      IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, read in all the cols.
!bjj Start of proposed change vXX
!rm         StiffTGJ (Ind) = Interp( HNodesNorm(Ind), HtFract, TwGJStif, NTwInpSt )
!rm         StiffTEA (Ind) = Interp( HNodesNorm(Ind), HtFract, TwEAStif, NTwInpSt )
!rm         InerTFA  (Ind) = Interp( HNodesNorm(Ind), HtFract, TwFAIner, NTwInpSt )
!rm         InerTSS  (Ind) = Interp( HNodesNorm(Ind), HtFract, TwSSIner, NTwInpSt )
!rm         cgOffTFA (Ind) = Interp( HNodesNorm(Ind), HtFract, TwFAcgOf, NTwInpSt )
!rm         cgOffTSS (Ind) = Interp( HNodesNorm(Ind), HtFract, TwSScgOf, NTwInpSt )
         StiffTGJ (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwGJStif, InterpInd, NTwInpSt )
         StiffTEA (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwEAStif, InterpInd, NTwInpSt )
         InerTFA  (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwFAIner, InterpInd, NTwInpSt )
         InerTSS  (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwSSIner, InterpInd, NTwInpSt )
         cgOffTFA (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwFAcgOf, InterpInd, NTwInpSt )
         cgOffTSS (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwSScgOf, InterpInd, NTwInpSt )
!bjj End of proposed change vXX
      ENDIF
   ENDDO ! Ind

ENDIF


   ! Deallocate the distributed input parameters.

DEALLOCATE ( HtFract , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the HtFract array.' )
ENDIF

DEALLOCATE ( TMassDen , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the TMassDen array.' )
ENDIF

DEALLOCATE ( TwFAStif , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the TwFAStif array.' )
ENDIF

DEALLOCATE ( TwSSStif , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the TwSSStif array.' )
ENDIF

DEALLOCATE ( TwGJStif , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the TwGJStif array.' )
ENDIF

DEALLOCATE ( TwEAStif , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the TwEAStif array.' )
ENDIF

DEALLOCATE ( TwFAIner , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the TwFAIner array.' )
ENDIF

DEALLOCATE ( TwSSIner , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the TwSSIner array.' )
ENDIF

DEALLOCATE ( TwFAcgOf , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the TwFAcgOf array.' )
ENDIF

DEALLOCATE ( TwSScgOf , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error deallocating memory for the TwSScgOf array.' )
ENDIF


RETURN
END SUBROUTINE InterpTwr
!=======================================================================
SUBROUTINE PrintSum


   ! This routine generates the summary file, which contains a regurgitation of
   !  the input data and interpolated flexible body data.

!bjj start of proposed change
!bjj rm:USE                             AeroTime, ONLY:DTAero                           ! Use the ONLY attribute to avoid naming conflict with variable DT
USE                             AeroDyn
!bjj end of proposed change
USE                             Blades
!bjj rm NWTC_Library: USE                             Constants
USE                             DOFs
USE                             Features
USE                             General
USE                             MassInert
USE                             SimCont
USE                             Tower
USE                             TurbConf


IMPLICIT                        NONE


   ! Local variables.

INTEGER(4)                   :: I                                               ! Index for the nodes.
INTEGER(4)                   :: K                                               ! Index for the blade number.

!bjj start of proposed change
INTEGER                      :: ErrStat
!bjj end of proposed change

CHARACTER(24)                :: Fmt1      = "(34X,3(6X,'Blade',I2,:))"          ! Format for outputting blade headings.
CHARACTER(15)                :: Fmt2      = "(34X,3(6X,A,:))"                   ! Format for outputting blade headings.
CHARACTER(18)                :: FmtDat    = '(A,T35,3(:,F13.3))'                ! Format for outputting mass and modal data.
CHARACTER(18)                :: FmtDatT   = '(A,T35,1(:,F13.8))'                ! Format for outputting time steps.
CHARACTER( 8)                :: FmtHead   = '(//,A,/)'                          ! Format for outputting headings.
CHARACTER( 9)                :: FmtTitl   = '(//,1X,A)'                         ! Format for outputting title.
CHARACTER( 3)                :: FmtTxt    = '(A)'                               ! Format for outputting pure text.
CHARACTER(99)                :: RotorType                                       ! Text description of rotor.


   ! Global functions.

!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: CurDate                                         ! A function that returns the durrent date in the form "dd-mmm-ccyy".
!bjj rm AD 12.70b CHARACTER( 8), EXTERNAL      :: CurTime                                         ! A function that returns the durrent date in the form "hh:mm:ss".
!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.



   ! Open the summary file and give it a heading.

!bjj start of proposed change
!rmIF ( Cmpl4SFun )  THEN     ! FAST has been compiled as an S-Function for Simulink
!rm   CALL OpenFOutFile ( UnSu, TRIM( RootName )//'_SFunc.fsm' )
!rmELSE                       ! FAST has been compiled normally
!rm   CALL OpenFOutFile ( UnSu, TRIM( RootName )//'.fsm' )
!rmENDIF
CALL OpenFOutFile ( UnSu, TRIM( RootName )//'.fsm' )
!bjj end of proposed change

!bjj start of proposed change vxx
!rmWRITE (UnSu,'(/,A)')  'This summary information was generated by '//ProgName//TRIM( ProgVer )// &
!rm                      ' on '//CurDate()//' at '//CurTime()//'.'
WRITE (UnSu,'(/,A)')  'This summary information was generated by '//TRIM(ProgName)//' '//TRIM( ProgVer )// &
                      ' on '//CurDate()//' at '//CurTime()//'.'
!bjj end of proposed change
WRITE (UnSu,FmtTitl)  TRIM( FTitle )


   ! Turbine features.

WRITE (UnSu,FmtHead)  'Turbine features:'

IF ( OverHang > 0.0 )  THEN
   RotorType = 'Downwind,'
ELSE
   RotorType = 'Upwind,'
ENDIF
IF ( NumBl == 2 )  THEN
   RotorType = TRIM(RotorType)//' two-bladed rotor'
ELSE
   RotorType = TRIM(RotorType)//' three-bladed rotor'
ENDIF
IF ( TeetDOF )  THEN
   RotorType = TRIM(RotorType)//' with teetering hub.'
ELSE
   RotorType = TRIM(RotorType)//' with rigid hub.'
ENDIF
WRITE    (UnSu,FmtTxt)  '            '//TRIM(RotorType)

SELECT CASE ( PtfmModel )
CASE ( 0 )
   WRITE (UnSu,FmtTxt)  '            Rigid foundation.'
CASE ( 1 )
   WRITE (UnSu,FmtTxt)  '            Onshore turbine.'
CASE ( 2 )
   WRITE (UnSu,FmtTxt)  '            Fixed bottom offshore turbine.'
CASE ( 3 )
   WRITE (UnSu,FmtTxt)  '            Floating offshore turbine.'
ENDSELECT

WRITE    (UnSu,FmtTxt)  '            The model has '//TRIM(Int2LStr( NActvDOF ))//' of '// &
                                     TRIM(Int2LStr( NDOF ))//' DOFs active (enabled) at start-up.'

IF ( FlapDOF1 )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    First flapwise blade mode DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   First flapwise blade mode DOF.'
ENDIF

IF ( FlapDOF2 )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Second flapwise blade mode DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Second flapwise blade mode DOF.'
ENDIF

IF ( EdgeDOF )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Edgewise blade mode DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Edgewise blade mode DOF.'
ENDIF

IF ( TeetDOF )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Rotor-teeter DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Rotor-teeter DOF.'
ENDIF

IF ( DrTrDOF )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Drivetrain rotational-flexibility DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Drivetrain rotational-flexibility DOF.'
ENDIF

IF ( GenDOF )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Generator DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Generator DOF.'
ENDIF

IF ( RFrlDOF )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Rotor-furl DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Rotor-furl DOF.'
ENDIF

IF ( TFrlDOF )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Tail-furl DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Tail-furl DOF.'
ENDIF

IF ( YawDOF )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Yaw DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Yaw DOF.'
ENDIF

IF ( TwFADOF1 )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    First tower fore-aft bending-mode DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   First tower fore-aft bending-mode DOF.'
ENDIF

IF ( TwFADOF2 )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Second tower fore-aft bending-mode DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Second tower fore-aft bending-mode DOF.'
ENDIF

IF ( TwSSDOF1 )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    First tower side-to-side bending-mode DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   First tower side-to-side bending-mode DOF.'
ENDIF

IF ( TwSSDOF2 )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Second tower side-to-side bending-mode DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Second tower side-to-side bending-mode DOF.'
ENDIF

IF ( PtfmSgDOF )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Platform horizontal surge translation DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Platform horizontal surge translation DOF.'
ENDIF

IF ( PtfmSwDOF )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Platform horizontal sway translation DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Platform horizontal sway translation DOF.'
ENDIF

IF ( PtfmHvDOF )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Platform vertical heave translation DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Platform vertical heave translation DOF.'
ENDIF

IF ( PtfmRDOF )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Platform roll tilt rotation DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Platform roll tilt rotation DOF.'
ENDIF

IF ( PtfmPDOF )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Platform pitch tilt rotation DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Platform pitch tilt rotation DOF.'
ENDIF

IF ( PtfmYDOF )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Platform yaw rotation DOF.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Platform yaw rotation DOF.'
ENDIF

IF ( CompAero )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Computation of aerodynamic loads.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Computation of aerodynamic loads.'
ENDIF

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
IF ( CompHydro )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Computation of hydrodynamic loads.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Computation of hydrodynamic loads.'
ENDIF

!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

IF ( CompNoise )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Computation of aeroacoustics.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Computation of aeroacoustics.'
ENDIF


   ! Time steps.

WRITE (UnSu,FmtHead)  'Time steps:'

WRITE (UnSu,FmtDatT) '    Structural            (s)     ', DT
!bjj start of proposed change
!bjj: does this really belong in this file????
!rmWRITE (UnSu,FmtDatT) '    Aerodynamic           (s)     ', DT*CEILING(DTAero/DT) ! AeroDyn will be called at integer multiples of DT that are greater than or equal to DTAero, since FAST's integration scheme marches with a constant time step of DT.
WRITE (UnSu,FmtDatT) '    Aerodynamic           (s)     ', DT*CEILING( AD_GetConstant('dtAero',ErrStat) / DT) ! AeroDyn will be called at integer multiples of DT that are greater than or equal to DTAero, since FAST's integration scheme marches with a constant time step of DT.
!bjj end of proposed change

   ! Some calculated parameters.

WRITE (UnSu,FmtHead)  'Some calculated parameters:'

WRITE (UnSu,FmtDat ) '    Hub-Height            (m)     ', FASTHH
WRITE (UnSu,FmtDat ) '    Flexible Tower Length (m)     ', TwrFlexL
WRITE (UnSu,FmtDat ) '    Flexible Blade Length (m)     ', BldFlexL


   ! Rotor properties:

WRITE (UnSu,FmtHead)  'Rotor mass properties:'

WRITE (UnSu,FmtDat ) '    Rotor Mass            (kg)    ', RotMass
WRITE (UnSu,FmTDat ) '    Rotor Inertia         (kg-m^2)', RotINer

WRITE (UnSu,Fmt1   ) ( K,         K=1,NumBl )
WRITE (UnSu,Fmt2   ) ( '-------', K=1,NumBl )

WRITE (UnSu,FmtDat ) '    Mass                  (kg)    ', ( BldMass  (K), K=1,NumBl )
WRITE (UnSu,FmtDat ) '    Second Mass Moment    (kg-m^2)', ( SecondMom(K), K=1,NumBl )
WRITE (UnSu,FmtDat ) '    First Mass Moment     (kg-m)  ', ( FirstMom (K), K=1,NumBl )
WRITE (UnSu,FmtDat ) '    Center of Mass        (m)     ', ( BldCG    (K), K=1,NumBl )


   ! Output additional masses:

WRITE (UnSu,FmtHead)  'Additional mass properties:'

WRITE (UnSu,FmtDat ) '    Tower-top Mass        (kg)    ', TwrTpMass
WRITE (UnSu,FmtDat ) '    Tower Mass            (kg)    ', TwrMass
WRITE (UnSu,FmtDat ) '    Turbine Mass          (kg)    ', TurbMass
WRITE (UnSu,FmtDat ) '    Mass Incl. Platform   (kg)    ', TotalMass


   ! Interpolated tower properties.

WRITE (UnSu,"(//,'Interpolated tower properties:',/)")
IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, print out all the cols.

   WRITE (UnSu,FmtTxt)  'Node  TwFract   HNodes  DHNodes  TMassDen    FAStiff    SSStiff'// &
                        '    GJStiff    EAStiff    FAIner    SSIner  FAcgOff  SScgOff'
   WRITE (UnSu,FmtTxt)  ' (-)      (-)      (m)      (m)    (kg/m)     (Nm^2)     (Nm^2)'// &
                        '     (Nm^2)        (N)    (kg m)    (kg m)      (m)      (m)'

   DO I=1,TwrNodes
!bjj start of proposed change v6.02d-bjj
! parentheses around an I/O list is an extension to Standard F2003
!rm      WRITE(UnSu,'(I4,3F9.3,F10.3,4ES11.3,2F10.3,2F9.3)')  I, HNodesNorm(I), ( HNodes(I) + TwrRBHt ), DHNodes(I), MassT(I), &
!rm                                                              StiffTFA(I), StiffTSS(I), StiffTGJ(I), StiffTEA(I),           &
!rm                                                              InerTFA(I), InerTSS(I), cgOffTFA(I), cgOffTSS(I)
      WRITE(UnSu,'(I4,3F9.3,F10.3,4ES11.3,2F10.3,2F9.3)')  I, HNodesNorm(I), HNodes(I)+TwrRBHt, DHNodes(I), MassT(I), &
                                                              StiffTFA(I), StiffTSS(I), StiffTGJ(I), StiffTEA(I),           &
                                                              InerTFA(I), InerTSS(I), cgOffTFA(I), cgOffTSS(I)
!bjj end of proposed change
   ENDDO ! I

ELSE                                                     ! Only FAST will be run; thus, only print out the necessary cols.

   WRITE (UnSu,FmtTxt)  'Node  TwFract   HNodes  DHNodes  TMassDen    FAStiff    SSStiff'
   WRITE (UnSu,FmtTxt)  ' (-)      (-)      (m)      (m)    (kg/m)     (Nm^2)     (Nm^2)'

   DO I=1,TwrNodes
!bjj start of proposed change v6.02d-bjj
! parentheses around an I/O list is an extension to Standard F2003
!rm      WRITE(UnSu,'(I4,3F9.3,F10.3,2ES11.3)')  I, HNodesNorm(I), ( HNodes(I) + TwrRBHt ), DHNodes(I), MassT(I), &
!rm                                                 StiffTFA(I), StiffTSS(I)
      WRITE(UnSu,'(I4,3F9.3,F10.3,2ES11.3)')  I, HNodesNorm(I), HNodes(I) + TwrRBHt, DHNodes(I), MassT(I), &
                                                 StiffTFA(I), StiffTSS(I)
!bjj end of proposed change
   ENDDO ! I

ENDIF


   ! Interpolated blade properties.

DO K=1,NumBl

   WRITE (UnSu,'(//,A,I1,A,/)')  'Interpolated blade ', K, ' properties:'
   IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, print out all the cols.

      WRITE (UnSu,FmtTxt)  'Node  BlFract   RNodes  DRNodes  AeroCent  StrcTwst  BMassDen    FlpStff    EdgStff'//       &
                           '     GJStff     EAStff    Alpha   FlpIner   EdgIner PrecrvRef PreswpRef  FlpcgOf  EdgcgOf'// &
                           '  FlpEAOf  EdgEAOf'
      WRITE (UnSu,FmtTxt)  ' (-)      (-)      (m)      (m)       (-)     (deg)    (kg/m)     (Nm^2)     (Nm^2)'//       &
                           '     (Nm^2)     (Nm^2)      (-)    (kg m)    (kg m)       (m)       (m)      (m)      (m)'// &
                           '      (m)      (m)'

      DO I=1,BldNodes
!bjj start of proposed change v6.02d-bjj
! parentheses around an I/O list is an extension to Standard F2003
!rm         WRITE(UnSu,'(I4,3F9.3,3F10.3,4ES11.3,F9.3,4F10.3,4F9.3)')  I, RNodesNorm(I), ( RNodes(I) + HubRad ), DRNodes(I),        &
!rm                                                                       AeroCent(K,I), ThetaS(K,I)*R2D, MassB(K,I),               &
!rm                                                                       StiffBF(K,I), StiffBE(K,I), StiffBGJ(K,I), StiffBEA(K,I), &
!rm                                                                       BAlpha(K,I), InerBFlp(K,I), InerBEdg(K,I),                &
!rm                                                                       RefAxisxb(K,I), RefAxisyb(K,I),                           &
!rm                                                                       cgOffBFlp(K,I), cgOffBEdg(K,I),                           &
!rm                                                                       EAOffBFlp(K,I), EAOffBEdg(K,I)

         WRITE(UnSu,'(I4,3F9.3,3F10.3,4ES11.3,F9.3,4F10.3,4F9.3)')  I, RNodesNorm(I), RNodes(I) + HubRad, DRNodes(I),            &
                                                                       AeroCent(K,I), ThetaS(K,I)*R2D, MassB(K,I),               &
                                                                       StiffBF(K,I), StiffBE(K,I), StiffBGJ(K,I), StiffBEA(K,I), &
                                                                       BAlpha(K,I), InerBFlp(K,I), InerBEdg(K,I),                &
                                                                       RefAxisxb(K,I), RefAxisyb(K,I),                           &
                                                                       cgOffBFlp(K,I), cgOffBEdg(K,I),                           &
                                                                       EAOffBFlp(K,I), EAOffBEdg(K,I)
!bjj end of proposed change
      ENDDO ! I

   ELSE                                                     ! Only FAST will be run; thus, only print out the necessary cols.

      WRITE (UnSu,FmtTxt)  'Node  BlFract   RNodes  DRNodes  AeroCent  StrcTwst  BMassDen    FlpStff    EdgStff'
      WRITE (UnSu,FmtTxt)  ' (-)      (-)      (m)      (m)       (-)     (deg)    (kg/m)     (Nm^2)     (Nm^2)'

      DO I=1,BldNodes
!bjj start of proposed change v6.02d-bjj
! parentheses around an I/O list is an extension to Standard F2003
!rm         WRITE(UnSu,'(I4,3F9.3,3F10.3,2ES11.3)')  I, RNodesNorm(I), ( RNodes(I) + HubRad ), DRNodes(I), AeroCent(K,I), &
!rm                                                     ThetaS(K,I)*R2D, MassB(K,I), StiffBF(K,I), StiffBE(K,I)
         WRITE(UnSu,'(I4,3F9.3,3F10.3,2ES11.3)')  I, RNodesNorm(I), RNodes(I) + HubRad, DRNodes(I), AeroCent(K,I), &
                                                     ThetaS(K,I)*R2D, MassB(K,I), StiffBF(K,I), StiffBE(K,I)
!bjj end of proposed change
      ENDDO ! I

   ENDIF

ENDDO ! K


RETURN
END SUBROUTINE PrintSum
!=======================================================================
!bjj Start of proposed change vXX NWTC_Lib
!rmSUBROUTINE ReadCAry ( Un, Fil, CharAry, AryLen, AryName, AryDescr )
!rm
!rm
!rm   ! This routine reads a AryLen values into a character array from the next AryLen lines of the input file.
!rm
!rm
!rmUSE                             General
!rmUSE                             Output
!rm
!rmIMPLICIT                        NONE
!rm
!rm
!rm   ! Passed variables:
!rm
!rmINTEGER(4)                   :: AryLen                                          ! Length of the array.
!rmINTEGER(4)                   :: Un                                              ! I/O unit for input file.
!rm
!rmCHARACTER(*)                 :: CharAry(AryLen)                                 ! Real variable being read.
!rm
!rmCHARACTER(*)                 :: AryDescr                                        ! Text string describing the variable.
!rmCHARACTER(*)                 :: AryName                                         ! Text string containing the variable name.
!rmCHARACTER(*)                 :: Fil                                             ! Name of the input file.
!rm
!rm
!rm   ! Local variables:
!rm
!rmINTEGER(4)                   :: Ind                                             ! Index into the string array.  Assumed to be one digit.
!rmINTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
!rm
!rmCHARACTER(35)                :: Frmt = "( 15X, A, T27, ' - ', A, /, 2X, A )"    ! Output format for string parameters.
!rm
!rm
!rm!rm   ! Global functions.
!rm
!rm!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.
!rm
!rm
!rm
!rmDO Ind=1,AryLen
!rm
!rm   READ (UnIn,*,IOSTAT=IOS)  CharAry(Ind)
!rm
!rm   CALL CheckIOS ( IOS, Fil, TRIM( AryName )//'('//TRIM( Int2LStr( Ind ) )//')', String )
!rm
!rm   IF ( Echo )  THEN
!rm      WRITE (UnEc,Frmt)  TRIM( AryName )//'('//TRIM( Int2LStr( Ind ) )//')', AryDescr, CharAry(Ind)
!rm   ENDIF
!rm
!rmENDDO ! Ind
!rm
!rm
!rmRETURN
!rmEND SUBROUTINE ReadCAry
!rm!=======================================================================
!rmSUBROUTINE ReadCVar ( Un, Fil, CharVar, VarName, VarDescr )
!rm
!rm
!rm   ! This routine reads a single character variable from the next line of the input file.
!rm
!rm
!rmUSE                             General
!rmUSE                             Output
!rm
!rmIMPLICIT                        NONE
!rm
!rm
!rm   ! Passed variables:
!rm
!rmINTEGER(4)                   :: Un                                              ! I/O unit for input file.
!rm
!rmCHARACTER(*)                 :: CharVar                                         ! Integer variable being read.
!rmCHARACTER(*)                 :: Fil                                             ! Name of the input file.
!rmCHARACTER(*)                 :: VarDescr                                        ! Text string describing the variable.
!rmCHARACTER(*)                 :: VarName                                         ! Text string containing the variable name.
!rm
!rm
!rm   ! Local variables:
!rm
!rmINTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
!rm
!rmCHARACTER(35)                :: Frmt = "( 15X, A, T27, ' - ', A, /, 2X, A )"    ! Output format for string parameters.
!rm
!rm
!rm
!rm
!rmREAD (UnIn,*,IOSTAT=IOS)  CharVar
!rm
!rmCALL CheckIOS ( IOS, Fil, VarName, String )
!rm
!rmIF ( Echo )  THEN
!rm   WRITE (UnEc,Frmt)  VarName, VarDescr, CharVar
!rmENDIF
!rm
!rm
!rmRETURN
!rmEND SUBROUTINE ReadCVar
!rm!=======================================================================
!rmSUBROUTINE ReadIVar ( Un, Fil, IntVar, VarName, VarDescr )
!rm
!rm
!rm   ! This routine reads a single integer variable from the next line of the input file.
!rm
!rm
!rmUSE                             General
!rmUSE                             Output
!rm
!rmIMPLICIT                        NONE
!rm
!rm
!rm   ! Passed variables:
!rm
!rmINTEGER(4)                   :: IntVar                                          ! Integer variable being read.
!rmINTEGER(4)                   :: Un                                              ! I/O unit for input file.
!rm
!rmCHARACTER(*)                 :: Fil                                             ! Name of the input file.
!rmCHARACTER(*)                 :: VarDescr                                        ! Text string describing the variable.
!rmCHARACTER(*)                 :: VarName                                         ! Text string containing the variable name.
!rm
!rm
!rm   ! Local variables:
!rm
!rmINTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
!rm
!rmCHARACTER(33)                :: Frmt = "( 2X, I11, 2X, A, T27, ' - ', A )"      ! Output format for integer parameters.
!rm
!rm
!rm
!rm
!rmREAD (UnIn,*,IOSTAT=IOS)  IntVar
!rm
!rmCALL CheckIOS ( IOS, Fil, VarName, Numeric )
!rm
!rmIF ( Echo )  THEN
!rm   WRITE (UnEc,Frmt)  IntVar, VarName, VarDescr
!rmENDIF
!rm
!rm
!rmRETURN
!rmEND SUBROUTINE ReadIVar
!rm!=======================================================================
!rmSUBROUTINE ReadLVar ( Un, Fil, LogVar, VarName, VarDescr )
!rm
!rm
!rm   ! This routine reads a single logical variable from the next line of the input file.
!rm
!rm
!rmUSE                             General
!rmUSE                             Output
!rm
!rmIMPLICIT                        NONE
!rm
!rm
!rm   ! Passed variables:
!rm
!rmINTEGER(4)                   :: Un                                              ! I/O unit for input file.
!rm
!rmLOGICAL(1)                   :: LogVar                                          ! Integer variable being read.
!rm
!rmCHARACTER(*)                 :: Fil                                             ! Name of the input file.
!rmCHARACTER(*)                 :: VarDescr                                        ! Text string describing the variable.
!rmCHARACTER(*)                 :: VarName                                         ! Text string containing the variable name.
!rm
!rm
!rm   ! Local variables:
!rm
!rmINTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
!rm
!rmCHARACTER(33)                :: Frmt = "( 2X, L11, 2X, A, T27, ' - ', A )"      ! Output format for logical parameters.
!rm
!rm
!rm
!rm
!rmREAD (UnIn,*,IOSTAT=IOS)  LogVar
!rm
!rmCALL CheckIOS ( IOS, Fil, VarName, FlagType )
!rm
!rmIF ( Echo )  THEN
!rm   WRITE (UnEc,Frmt)  LogVar, VarName, VarDescr
!rmENDIF
!rm
!rm
!rmRETURN
!rmEND SUBROUTINE ReadLVar
!rm!=======================================================================
!rmSUBROUTINE ReadRAry ( Un, Fil, RealAry, AryLen, AryName, AryDescr )
!rm
!rm
!rm   ! This routine reads a AryLen values into a real array from the next AryLen lines of the input file.
!rm
!rm
!rmUSE                             General
!rmUSE                             Output
!rm!bjj rm NWTC_Library: USE                             Precision
!rm
!rmIMPLICIT                        NONE
!rm
!rm
!rm   ! Passed variables:
!rm
!rmINTEGER(4)                   :: AryLen                                          ! Length of the array.
!rmINTEGER(4)                   :: Un                                              ! I/O unit for input file.
!rm
!rmREAL(ReKi)                   :: RealAry(AryLen)                                 ! Real variable being read.
!rm
!rmCHARACTER(*)                 :: Fil                                             ! Name of the input file.
!rmCHARACTER(*)                 :: AryDescr                                        ! Text string describing the variable.
!rmCHARACTER(*)                 :: AryName                                         ! Text string containing the variable name.
!rm
!rm
!rm   ! Local variables:
!rm
!rmINTEGER(4)                   :: Ind                                             ! Index into the real array.  Assumed to be one digit.
!rmINTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
!rm
!rmCHARACTER(38)                :: Frmt = "( 2X, ES11.4e2, 2X, A, T27, ' - ', A )" ! Output format for real array parameters
!rm
!rm
!rm   ! Global functions.
!rm
!rm!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.
!rm
!rm
!rm
!rmDO Ind=1,AryLen
!rm
!rm   READ (UnIn,*,IOSTAT=IOS)  RealAry(Ind)
!rm!JASON: THE UnIn above should be Un! (TYP for all of these READ* routines)<--Don't worry about fixing this: Instead, wait until we merge Marshall's NWTC_Subs.f90 source file to the code.
!rm
!rm   CALL CheckIOS ( IOS, Fil, TRIM( AryName )//'('//TRIM( Int2LStr( Ind ) )//')', Numeric )
!rm
!rm   IF ( Echo )  THEN
!rm      WRITE (UnEc,Frmt)  RealAry(Ind), TRIM( AryName )//'('//TRIM( Int2LStr( Ind ) )//')', AryDescr
!rm   ENDIF
!rm
!rmENDDO ! Ind
!rm
!rm
!rmRETURN
!rmEND SUBROUTINE ReadRAry
!rm!=======================================================================
!rmSUBROUTINE ReadRVar ( Un, Fil, RealVar, VarName, VarDescr )
!rm
!rm
!rm   ! This routine reads a single real variable from the next line of the input file.
!rm
!rm
!rmUSE                             General
!rmUSE                             Output
!rm!bjj rm NWTC_Library: USE                             Precision
!rm
!rmIMPLICIT                        NONE
!rm
!rm
!rm   ! Passed variables:
!rm
!rmREAL(ReKi)                   :: RealVar                                         ! Real variable being read.
!rm
!rmINTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
!rmINTEGER(4)                   :: Un                                              ! I/O unit for input file.
!rm
!rmCHARACTER( *)                :: Fil                                             ! Name of the input file.
!rmCHARACTER(38)                :: Frmt = "( 2X, ES11.4e2, 2X, A, T27, ' - ', A )" ! Output format for real parameters
!rmCHARACTER( *)                :: VarDescr                                        ! Text string describing the variable.
!rmCHARACTER( *)                :: VarName                                         ! Text string containing the variable name.
!rm
!rm
!rm   ! Local variables:
!rm
!rm
!rm
!rm
!rm
!rmREAD (UnIn,*,IOSTAT=IOS)  RealVar
!rm
!rmCALL CheckIOS ( IOS, Fil, VarName, Numeric )
!rm
!rmIF ( Echo )  THEN
!rm   WRITE (UnEc,Frmt)  RealVar, VarName, VarDescr
!rmENDIF
!rm
!rm
!rmRETURN
!rmEND SUBROUTINE ReadRVar
!rm!=======================================================================
!bjj End of proposed change
SUBROUTINE RunTimes()


   ! This routine displays a message that gives that status of the simulation
   !  and the predicted end time of day.


USE                             General
!bjj rm NWTC_Library: USE                             Precision
USE                             SimCont
!bjj start of proposed change
!rm: USE                             SysSubs
!USE                              FAST_SysSubs
!bjj end of proposed change

IMPLICIT                        NONE


   ! Local variables.

REAL(ReKi)                   :: ClckTime                                        ! Elapsed clock time for the simulation phase of the run.
!bjj rm unused:REAL(ReKi)                   :: CurrTime                                        ! Current time in seconds past midnight.
!bjj rm unused:REAL(ReKi)                   :: DeltTime                                        ! The amount of time elapsed since the lst call.
!bjj rm unused:REAL(ReKi)                   :: EndTime                                         ! Approximate time of day when simulation will complete.
REAL(ReKi)                   :: Factor                                          ! Ratio of seconds to a specified time period.
!bjj rm unused:REAL(ReKi)                   :: InSecHr   = 1.0/3600.0                          ! Inverse of the number of seconds in an hour.
!bjj rm unused:REAL(ReKi)                   :: InSecMn   = 1.0/  60.0                          ! Inverse of the number of seconds in a minute.
!bjj rm unused:REAL(ReKi), SAVE             :: PrevTime                                        ! Previous time in seconds past midnight.
!bjj rm unused:REAL(ReKi)                   :: TimeLeft                                        ! Approximate clock time remaining before simulation completes.
REAL(ReKi)                   :: TRatio                                          ! Ration of simulation time to elapsed clock time.

REAL(4)                      :: UsrTime                                         ! User CPU time for entire run.
!bjj rm: REAL(4)                      :: UsrTime2                                        ! User CPU time for time-marching part of simulation.

!bjj rm unused:INTEGER(4)                   :: EndHour                                         ! The hour when the simulations is expected to complete.
!bjj rm unused:INTEGER(4)                   :: EndMin                                          ! The minute when the simulations is expected to complete.
!bjj rm unused:INTEGER(4)                   :: EndSec                                          ! The second when the simulations is expected to complete.
INTEGER(4)                   :: EndTimes (8)                                    ! An array holding the ending clock time of the simulation.
!bjj rm unused:INTEGER(4)                   :: TimeAry  (8)                                    ! An array containing the elements of the start time.

!bjj rm unused:CHARACTER( 8)                :: Date                                            ! String containing the current date.
!bjj rmCHARACTER( 8)                :: DumDate                                         ! A dummy variable to hold the date string.
!bjj rmCHARACTER(10)                :: DumTime                                         ! A dummy variable to hold the time string.
CHARACTER( 8)                :: TimePer
!bjj rmCHARACTER( 5)                :: Zone                                            ! String containing the name of the time zone.

   ! Global functions.

!bjj rm AD 12.70b CHARACTER(15), EXTERNAL      :: Flt2LStr                                        ! A function to convert a floating-point number to a left-justified string.
!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.


   ! Get the end times to compare with start times.

!bjj Start of proposed change
!rmCALL DATE_AND_TIME ( DumDate, DumTime, Zone, EndTimes )
CALL DATE_AND_TIME ( VALUES=EndTimes )
CALL CPU_TIME ( UsrTime )

!write( tmpchar, '( A,I4,5(1X,I2),1X,I3 )' ) "Start: ", StrtTime(1:3), StrtTime(5:8)
!call wrscr1(tmpchar)
!write( tmpchar, '( A,I4,5(1X,I2),1X,I3 )' ) "End  : ", EndTimes(1:3), EndTimes(5:8)
!call wrscr(tmpchar)
!call wrscr(' ')

!bjj end of proposed change

   ! Calculate the elapsed wall-clock time in seconds.
   
!bjj: I think this calculation will be wrong at certain times (e.g. if it's near midnight on the last day of the month), but to my knowledge, no one has complained...

ClckTime =  0.001*( EndTimes(8) - StrtTime(8) ) + ( EndTimes(7) - StrtTime(7) ) + 60.0*( EndTimes(6) - StrtTime(6) ) &
         + 3600.0*( EndTimes(5) - StrtTime(5) ) + 86400.0*( EndTimes(3) - StrtTime(3) )  
         


   ! Calculate CPU times.

!bjj start of proposed change
!rm UsrTime2 = UserTime()
!rm UsrTime  = UsrTime1 + UsrTime2
!CALL CPU_TIME ( UsrTime )
!UsrTime2 = UsrTime - UsrTime1
!UsrTime  = UsrTime - UsrTime0
UsrTime  = UsrTime - UsrTime1
!bjj end of proposed change

!bjj start of proposed change v6.02d-bjj
!rmIF ( UsrTime2 /= 0.0 )  THEN
!rm   TRatio = ZTime/UsrTime2
!rm
!rm   IF     ( UsrTime2 > 86400.0 )  THEN
!rm      Factor = 1.0/86400.0
!rm      TimePer = ' days'
!rm   ELSEIF ( UsrTime2 >  3600.0 )  THEN
!rm      Factor = 1.0/3600.0
!rm      TimePer = ' hours'
!rm   ELSEIF ( UsrTime2 >    60.0 )  THEN
!rm      Factor = 1.0/60.0
!rm      TimePer = ' minutes'
!rm   ELSE
!rm      Factor = 1.0
!rm      TimePer = ' seconds'
!rm   ENDIF
!rm
!rm   CALL WrOver( ' Total Real Time:       '//TRIM( Flt2LStr( Factor*ClckTime      ) )//TRIM( TimePer )// &
!rm                '                                            '                                            ) !bjj: this is actually "Simulation Real Time"
!rm   CALL WrScr ( ' Total CPU Time:        '//TRIM( Flt2LStr( Factor*UsrTime       ) )//TRIM( TimePer ) )
!rm   CALL WrScr ( ' Simulation Time:       '//TRIM( Flt2LStr( Factor*REAL( ZTime ) ) )//TRIM( TimePer ) )
!rm   CALL WrScr ( ' Simulation CPU Time:   '//TRIM( Flt2LStr( Factor*UsrTime2      ) )//TRIM( TimePer ) )
!rm   CALL WrScr ( ' Simulation Time Ratio: '//TRIM( Flt2LStr( TRatio ) ) )

IF ( UsrTime /= 0.0 )  THEN

   TRatio = ZTime / UsrTime

   IF     ( UsrTime > 86400.0 )  THEN
      Factor = 1.0/86400.0
      TimePer = ' days'
   ELSEIF ( UsrTime >  3600.0 )  THEN
      Factor = 1.0/3600.0
      TimePer = ' hours'
   ELSEIF ( UsrTime >    60.0 )  THEN
      Factor = 1.0/60.0
      TimePer = ' minutes'
   ELSE
      Factor = 1.0
      TimePer = ' seconds'
   ENDIF

   CALL WrOver( ' Total Real Time:       '//TRIM( Flt2LStr( Factor*ClckTime      ) )//TRIM( TimePer )// &
                '                                                 '                                   )
   CALL WrScr ( ' Total CPU Time:        '//TRIM( Flt2LStr( Factor*UsrTime       ) )//TRIM( TimePer ) )
   CALL WrScr ( ' Simulated Time:        '//TRIM( Flt2LStr( Factor*REAL( ZTime ) ) )//TRIM( TimePer ) )
!bjj start of proposed change v7.00.01a-bjj
!rm   CALL WrScr ( ' Time Ratio (Sim/Real): '//TRIM( Flt2LStr( TRatio ) ) )
   CALL WrScr ( ' Time Ratio (Sim/CPU):  '//TRIM( Flt2LStr( TRatio ) ) )
!bjj end of proposed change   

!bjj end of proposed change v6.02d-bjj


ENDIF


RETURN
END SUBROUTINE RunTimes
!=======================================================================
SUBROUTINE SimStatus


   ! This routine displays a message that gives that status of the simulation
   !  and the predicted end time of day.


!bjj rm NWTC_Library: USE                             Precision
USE                             SimCont
!bjj rm NWTC_Library: USE                             SysSubs

IMPLICIT                        NONE


   ! Local variables.

REAL(ReKi)                   :: CurrTime                                        ! Current time in seconds past midnight.
REAL(ReKi)                   :: DeltTime                                        ! The amount of time elapsed since the lst call.
REAL(ReKi)                   :: EndTime                                         ! Approximate time of day when simulation will complete.
REAL(ReKi)                   :: InSecHr   = 1.0/3600.0                          ! Inverse of the number of seconds in an hour.
REAL(ReKi)                   :: InSecMn   = 1.0/  60.0                          ! Inverse of the number of seconds in a minute.
REAL(ReKi), SAVE             :: PrevTime                                        ! Previous time in seconds past midnight.
REAL(ReKi)                   :: TimeLeft                                        ! Approximate clock time remaining before simulation completes.

INTEGER(4)                   :: EndHour                                         ! The hour when the simulations is expected to complete.
INTEGER(4)                   :: EndMin                                          ! The minute when the simulations is expected to complete.
INTEGER(4)                   :: EndSec                                          ! The second when the simulations is expected to complete.
INTEGER(4)                   :: TimeAry  (8)                                    ! An array containing the elements of the start time.

!bjj chg: LOGICAL(1), SAVE             :: FirstPas = .TRUE.                               ! When true, indicates we're on the first pass of sim.
LOGICAL,    SAVE             :: FirstPas = .TRUE.                               ! When true, indicates we're on the first pass of sim.

!bjj rm CHARACTER( 8)                :: Date                                            ! String containing the current date.
CHARACTER( 8)                :: ETimeStr                                        ! String containing the end time.
!bjj rmCHARACTER(10)                :: Time                                            ! String containing the current time.
!bjj rmCHARACTER( 5)                :: Zone                                            ! String containing the name of the time zone.


   ! Global functions.

!bjj rm AD 12.70b CHARACTER(15), EXTERNAL      :: Flt2LStr                                        ! A function to convert a floating-point number to a left-justified string.
!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.



   ! On the first pass, get the start time.

IF ( FirstPas )  THEN

!bjj Start of proposed change
!rm   CALL DATE_AND_TIME ( Date , Time , Zone , TimeAry )
   CALL DATE_AND_TIME ( Values=TimeAry )
!bjj end of proposed change

   PrevTime = 60.0*( 60.0*TimeAry(5) + TimeAry(6) ) + TimeAry(7) + 0.001*TimeAry(8)

   FirstPas = .FALSE.

   RETURN

ENDIF


   ! How many seconds past midnight?

!bjj Start of proposed change
!rmCALL DATE_AND_TIME ( Date , Time , Zone , TimeAry )
CALL DATE_AND_TIME ( Values=TimeAry )
!bjj end of proposed change

CurrTime = 60.0*( 60.0*TimeAry(5) + TimeAry(6) ) + TimeAry(7) + 0.001*TimeAry(8)


   ! Calculate elapsed time.

DeltTime = CurrTime - PrevTime


   ! We may have passed midnight since the last revoultion.  We will assume
   !  that one second of simulation time doesn't take more than a day.

IF ( CurrTime < PrevTime )  THEN
   DeltTime = DeltTime + 86400.0
ENDIF


   ! Estimate the end time in hours, minutes, and seconds.

TimeLeft = DeltTime*( TMax - ZTime )                                            ! Simulation time between calls is presumed to be 1 second.
EndTime  = MOD( CurrTime+TimeLeft, 86400.0 )
EndHour  =  INT(   EndTime*InSecHr )
EndMin   =  INT( ( EndTime - REAL( 3600*EndHour ) )*InSecMn )
EndSec   = NINT(   EndTime - REAL( 3600*EndHour + 60*EndMin ) )

WRITE (ETimeStr,"(I2.2,2(':',I2.2))")  EndHour, EndMin, EndSec

CALL WrOver ( ' Timestep: '//TRIM( Int2LStr( NINT( ZTime ) ) )//' of '//TRIM( Flt2LStr( TMax ) )// &
              ' seconds.  Estimated final completion at '//ETimeStr//'.'                             )


   ! Let's save this time as the previous time.

PrevTime = CurrTime


RETURN
END SUBROUTINE SimStatus
!=======================================================================
!bjj Start of proposed change vXX NWTC_Lib
!rmSUBROUTINE SkipComment ( Un, Fil, Label, Echo )
!rm
!rm
!rm   ! This routine prints out an invalid-numeric-input message and aborts the program.
!rm
!rm
!rmUSE                             General
!rm
!rmIMPLICIT                        NONE
!rm
!rm
!rm   ! Passed variables:
!rm
!rmINTEGER(4)                   :: Un                                              ! I/O unit for input file.
!rm
!rm!bjj chg: LOGICAL(1)                   :: Echo                                            ! Flag to specify whether or not to echo the comment.
!rmLOGICAL                      :: Echo                                            ! Flag to specify whether or not to echo the comment.
!rm
!rmCHARACTER(*)                 :: Fil                                             ! Name of the input file.
!rmCHARACTER(*)                 :: Label                                           ! String containing a description of the comment.
!rm
!rm
!rm   ! Local variables:
!rm
!rmINTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
!rm
!rmCHARACTER(100)               :: Comment                                        ! String to temporarily hold the comment line.
!rm
!rm
!rm
!rm   ! Read the comment line.
!rm
!rmREAD (Un,'(A)',IOSTAT=IOS)  Comment
!rm
!rmIF ( IOS < 0 )  THEN
!rm   CALL WrScr1 ( ' Premature EOF for file "'//TRIM( Fil )//'.' )
!rm   CALL ProgAbort  ( ' The error occurred while trying to skip the '//TRIM( Label )//' comment.' )
!rmENDIF
!rm
!rmIF ( Echo )  THEN
!rm   WRITE (UnEc,'(2X,A)')  Comment
!rmENDIF
!rm
!rm
!rmRETURN
!rmEND SUBROUTINE SkipComment
!bjj End of proposed change
!=======================================================================
SUBROUTINE WrOutHdr(turbnum)


   ! This routine generates the header for the primary output file.


USE                             Features
USE                             General
!bjj Start of propsoed change v12.70a-bjj
!rmUSE                             Identify
!USE                             Identify, ONLY: AeroProg, AeroVer
USE                             AeroDyn
!bjj End of proposed change
USE                             Output
!bjj rm NWTC_Library: USE                             SysSubs

!bjj Start of proposed change vXX
USE                             Noise     !WrNoiseOutHdr
!bjj end of proposed change

IMPLICIT                        NONE

Integer turbnum 

   ! Local variables

INTEGER(4)                   :: I                                               ! A generic index for DO loops.

Character(3) str



   ! Global functions.

!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: CurDate                                         ! A function that returns the durrent date in the form "dd-mmm-ccyy".
!bjj rm AD 12.70b CHARACTER( 8), EXTERNAL      :: CurTime                                         ! A function that returns the durrent date in the form "hh:mm:ss".



   ! Open the output file:

!bjj start of proposed change
!rmCALL OpenFOutFile ( UnOu, OutFile )

! SL: write turbin number in to "str"
write(str, '(i3)') turbnum

CALL OpenFOutFile ( UnOu, TRIM(RootName)//TRIM(adjustl(str))//'.out' )

! SL: original code
!CALL OpenFOutFile ( UnOu, TRIM(RootName)//'.out' )


   ! Add some file information:

!bjj start of proposed change VXX
!rmWRITE (UnOu,'(/,A)')  'These predictions were generated by '//ProgName//TRIM( ProgVer )//' on '//CurDate()//' at '//CurTime()//'.'
!rmWRITE (UnOu,'(  A)')  'The aerodynamic calculations were made by '//TRIM(AeroProg)//' '//TRIM(AeroVer)//'.'
WRITE (UnOu,'(/,A)')  'These predictions were generated by '//TRIM(ProgName)//' '//TRIM( ProgVer )//&
                      ' on '//CurDate()//' at '//CurTime()//'.'
WRITE (UnOu,'(  A)')  'The aerodynamic calculations were made by '//TRIM(AD_Prog%Name)//' '//TRIM(AD_Prog%Ver)//'.'
!bjj end of proposed change
WRITE (UnOu,'(/,1X,A,/)')  TRIM( FTitle )


   ! Write the names of the output parameters:

IF ( TabDelim )  THEN

   CALL WrFileNR ( UnOu, '    Time' )

   DO I=1,NumOuts
      CALL WrFileNR ( UnOu, Tab//TRIM( OutParam(I)%Name ) )
   ENDDO ! I

   WRITE (UnOu,'()')
   CALL WrFileNR ( UnOu, '   (sec)' )

   DO I=1,NumOuts
      CALL WrFileNR ( UnOu, Tab//TRIM( OutParam(I)%Units ) )
   ENDDO ! I

ELSE

   CALL WrFileNR ( UnOu, '    Time' )

   DO I=1,NumOuts
      CALL WrFileNR ( UnOu, ' '//ADJUSTR( OutParam(I)%Name ) )
   ENDDO ! I

   WRITE (UnOu,'()')
   CALL WrFileNR ( UnOu, '   (sec)' )

   DO I=1,NumOuts
      CALL WrFileNR ( UnOu, ' '//ADJUSTR( OutParam(I)%Units ) )
   ENDDO ! I

ENDIF

WRITE (UnOu,'()')



   ! Open and create noise file:

IF ( CompNoise )  CALL WrNoiseOutHdr



RETURN
END SUBROUTINE WrOutHdr
!=======================================================================
SUBROUTINE WrOutput


   ! This routine writes output to the primary output file.


USE                             Features
USE                             General
USE                             Output

!BJJ Start of proposed change AD_v12.70
USE                             AeroGenSubs, ONLY: ElemOut
!BJJ End of proposed change
!bjj start of proposed change vXX
USE                             NOISE  !WriteSPLOut()
!bjj end of proposed change

IMPLICIT                        NONE


   ! Local variables.

INTEGER(4)                   :: I                                               ! A generic index for DO loops.

CHARACTER(37)                :: Frmt                                            ! A string to hold a format specifier.



   ! Write normal tabular output:

IF ( TabDelim )  THEN
   Frmt = '(F8.3,200(:,A ,'//TRIM( OutFmt )//'))'
   WRITE(UnOu,Frmt)  OutData(Time), ( TAB, OutData(I), I=1,NumOuts )
ELSE
   Frmt = '(F8.3,200(:,1X,'//TRIM( OutFmt )//'))'
   WRITE(UnOu,Frmt)  OutData(Time), (      OutData(I), I=1,NumOuts )
ENDIF



   ! Generate element data if desired:

CALL ElemOut



   ! Output noise if desired:

IF ( CompNoise )  CALL WriteSPLOut



RETURN
END SUBROUTINE WrOutput
!=======================================================================
!BJJ Start of proposed change vXX NWTC_Lib
END MODULE FAST_IO_Subs
!bjj end of proposed change
