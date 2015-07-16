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
   
   ADInterfaceComponents%Hub%Orientation(:,:) = 0.0
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
   
     
      ! Initialize AeroDyn

   ADAeroMarkers = AD_Init(ADOptions, ADInterfaceComponents, ErrStat)    ! relative markers are returned
   IF ( ErrStat /= 0 ) CALL ProgAbort( ' Could not initialize AeroDyn. ')
    


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
SUBROUTINE FAST_Begin()


   ! This subroutine Prints out the name and version of the program, checks for
   !  command-line arguments, and creates the names of the output files.


USE                             General
USE                             TurbConf


IMPLICIT                        NONE


   ! Local variables.

INTEGER                      :: Stat                                            ! The status of the call to GET_CWD

CHARACTER(1024)              :: DirName                                         ! A CHARACTER string containing the path of the current working directory.



   ! Check for command line arguments.  Maybe the user specified an input file name.
   ! Don't perform this check when interfaced with Simulink or Labview:

IF ( .NOT. Cmpl4SFun .AND. .NOT. Cmpl4LV  )  CALL CheckArgs( PriFile )

CALL GetRoot( PriFile, RootName )


   ! Let's create a root file name variable, DirRoot, which includes the full
   !   path to the current working directory.

IF ( PathIsRelative(RootName) ) THEN
   CALL Get_CWD  ( DirName, Stat )
   IF (Stat /= 0) CALL ProgAbort('Error retreiving current working directory.')
   
   DirRoot = TRIM( DirName )//PathSep//TRIM( RootName )
ELSE
   DirRoot = RootName
END IF   

   ! Let's create the name of the output file.

IF ( Cmpl4SFun )  RootName = TRIM( RootName )//'_SFunc'


RETURN
END SUBROUTINE FAST_Begin
!=======================================================================
SUBROUTINE ChckOutLst


   ! This routine checks to see if any inputted output channels (stored
   !    in the OutList(:)) are ill-conditioned (and if so, FAST Aborts)
   !    and assigns the settings for OutParam(:) (i.e, the
   !    index, name, and units of the output channels, OutData(:)).


   USE                             Features
   USE                             General
   USE                             Output
   USE                             Platform
   USE                             TurbConf

   IMPLICIT                        NONE


      ! Local variables.

   INTEGER                      :: I                                               ! Generic loop-counting index.
   INTEGER                      :: J                                               ! Generic loop-counting index.
   INTEGER                      :: INDX                                            ! Index for valid arrays.
   INTEGER                      :: Sttus                                           ! Status returned from an allocation request.
   
   CHARACTER(10)                :: OutListTmp                                      ! A string to temporarily hold OutList(I).
   CHARACTER(28)                :: OutPFmt    = "( I4, 3X,A 10,1 X, A10 )"         ! Output format parameter output list.


! NOTE: The following lines of code were generated by a Matlab script called "Write_ChckOutLst.m"
!      using the parameters listed in the "OutListParameters.xlsx" Excel file. Any changes to these 
!      lines should be modified in the Matlab script and/or Excel worksheet as necessary. 
! This code was generated by Write_ChckOutLst.m at 30-Jan-2012 12:30:08.
   CHARACTER( 9),PARAMETER  :: ValidParamAry(1110) =  (/ &                         ! This lists the names of the allowed parameters, which must be sorted alphabetically
                               "ANCH1ANG ","ANCH1TEN ","ANCH2ANG ","ANCH2TEN ","ANCH3ANG ","ANCH3TEN ","ANCH4ANG ", &
                               "ANCH4TEN ","ANCH5ANG ","ANCH5TEN ","ANCH6ANG ","ANCH6TEN ","ANCH7ANG ","ANCH7TEN ", &
                               "ANCH8ANG ","ANCH8TEN ","ANCH9ANG ","ANCH9TEN ","AZIMUTH  ","BLDPITCH1","BLDPITCH2", &
                               "BLDPITCH3","BLPITCH1 ","BLPITCH2 ","BLPITCH3 ","CTHRSTARM","CTHRSTAZM","CTHRSTRAD", &
                               "FAIR1ANG ","FAIR1TEN ","FAIR2ANG ","FAIR2TEN ","FAIR3ANG ","FAIR3TEN ","FAIR4ANG ", &
                               "FAIR4TEN ","FAIR5ANG ","FAIR5TEN ","FAIR6ANG ","FAIR6TEN ","FAIR7ANG ","FAIR7TEN ", &
                               "FAIR8ANG ","FAIR8TEN ","FAIR9ANG ","FAIR9TEN ","GENACCEL ","GENCP    ","GENCQ    ", &
                               "GENPWR   ","GENSPEED ","GENTQ    ","HORWINDV ","HORWNDDIR","HSSBRTQ  ","HSSHFTA  ", &
                               "HSSHFTCP ","HSSHFTCQ ","HSSHFTPWR","HSSHFTTQ ","HSSHFTV  ","IPDEFL1  ","IPDEFL2  ", &
                               "IPDEFL3  ","LSSGAGA  ","LSSGAGAXA","LSSGAGAXS","LSSGAGFXA","LSSGAGFXS","LSSGAGFYA", &
                               "LSSGAGFYS","LSSGAGFZA","LSSGAGFZS","LSSGAGMXA","LSSGAGMXS","LSSGAGMYA","LSSGAGMYS", &
                               "LSSGAGMZA","LSSGAGMZS","LSSGAGP  ","LSSGAGPXA","LSSGAGPXS","LSSGAGV  ","LSSGAGVXA", &
                               "LSSGAGVXS","LSSHFTCP ","LSSHFTCQ ","LSSHFTCT ","LSSHFTFXA","LSSHFTFXS","LSSHFTFYA", &
                               "LSSHFTFYS","LSSHFTFZA","LSSHFTFZS","LSSHFTMXA","LSSHFTMXS","LSSHFTPWR","LSSHFTTQ ", &
                               "LSSTIPA  ","LSSTIPAXA","LSSTIPAXS","LSSTIPMYA","LSSTIPMYS","LSSTIPMZA","LSSTIPMZS", &
                               "LSSTIPP  ","LSSTIPPXA","LSSTIPPXS","LSSTIPV  ","LSSTIPVXA","LSSTIPVXS","NACYAW   ", &
                               "NACYAWA  ","NACYAWERR","NACYAWP  ","NACYAWV  ","NCIMURAXS","NCIMURAYS","NCIMURAZS", &
                               "NCIMURVXS","NCIMURVYS","NCIMURVZS","NCIMUTAXS","NCIMUTAYS","NCIMUTAZS","NCIMUTVXS", &
                               "NCIMUTVYS","NCIMUTVZS","OOPDEFL1 ","OOPDEFL2 ","OOPDEFL3 ","PTCHDEFL1","PTCHDEFL2", &
                               "PTCHDEFL3","PTCHPMZB1","PTCHPMZB2","PTCHPMZB3","PTCHPMZC1","PTCHPMZC2","PTCHPMZC3", &
                               "PTFMFXI  ","PTFMFXT  ","PTFMFYI  ","PTFMFYT  ","PTFMFZI  ","PTFMFZT  ","PTFMHEAVE", &
                               "PTFMMXI  ","PTFMMXT  ","PTFMMYI  ","PTFMMYT  ","PTFMMZI  ","PTFMMZT  ","PTFMPITCH", &
                               "PTFMRAXI ","PTFMRAXT ","PTFMRAYI ","PTFMRAYT ","PTFMRAZI ","PTFMRAZT ","PTFMRDXI ", &
                               "PTFMRDYI ","PTFMRDZI ","PTFMROLL ","PTFMRVXI ","PTFMRVXT ","PTFMRVYI ","PTFMRVYT ", &
                               "PTFMRVZI ","PTFMRVZT ","PTFMSURGE","PTFMSWAY ","PTFMTAXI ","PTFMTAXT ","PTFMTAYI ", &
                               "PTFMTAYT ","PTFMTAZI ","PTFMTAZT ","PTFMTDXI ","PTFMTDXT ","PTFMTDYI ","PTFMTDYT ", &
                               "PTFMTDZI ","PTFMTDZT ","PTFMTVXI ","PTFMTVXT ","PTFMTVYI ","PTFMTVYT ","PTFMTVZI ", &
                               "PTFMTVZT ","PTFMYAW  ","QD2_B1E1 ","QD2_B1F1 ","QD2_B1F2 ","QD2_B2E1 ","QD2_B2F1 ", &
                               "QD2_B2F2 ","QD2_B3E1 ","QD2_B3F1 ","QD2_B3F2 ","QD2_DRTR ","QD2_GEAZ ","QD2_HV   ", &
                               "QD2_P    ","QD2_R    ","QD2_RFRL ","QD2_SG   ","QD2_SW   ","QD2_TEET ","QD2_TFA1 ", &
                               "QD2_TFA2 ","QD2_TFRL ","QD2_TSS1 ","QD2_TSS2 ","QD2_Y    ","QD2_YAW  ","QD_B1E1  ", &
                               "QD_B1F1  ","QD_B1F2  ","QD_B2E1  ","QD_B2F1  ","QD_B2F2  ","QD_B3E1  ","QD_B3F1  ", &
                               "QD_B3F2  ","QD_DRTR  ","QD_GEAZ  ","QD_HV    ","QD_P     ","QD_R     ","QD_RFRL  ", &
                               "QD_SG    ","QD_SW    ","QD_TEET  ","QD_TFA1  ","QD_TFA2  ","QD_TFRL  ","QD_TSS1  ", &
                               "QD_TSS2  ","QD_Y     ","QD_YAW   ","Q_B1E1   ","Q_B1F1   ","Q_B1F2   ","Q_B2E1   ", &
                               "Q_B2F1   ","Q_B2F2   ","Q_B3E1   ","Q_B3F1   ","Q_B3F2   ","Q_DRTR   ","Q_GEAZ   ", &
                               "Q_HV     ","Q_P      ","Q_R      ","Q_RFRL   ","Q_SG     ","Q_SW     ","Q_TEET   ", &
                               "Q_TFA1   ","Q_TFA2   ","Q_TFRL   ","Q_TSS1   ","Q_TSS2   ","Q_Y      ","Q_YAW    ", &
                               "RFRLBRM  ","ROLLDEFL1","ROLLDEFL2","ROLLDEFL3","ROOTFXB1 ","ROOTFXB2 ","ROOTFXB3 ", &
                               "ROOTFXC1 ","ROOTFXC2 ","ROOTFXC3 ","ROOTFYB1 ","ROOTFYB2 ","ROOTFYB3 ","ROOTFYC1 ", &
                               "ROOTFYC2 ","ROOTFYC3 ","ROOTFZB1 ","ROOTFZB2 ","ROOTFZB3 ","ROOTFZC1 ","ROOTFZC2 ", &
                               "ROOTFZC3 ","ROOTMEDG1","ROOTMEDG2","ROOTMEDG3","ROOTMFLP1","ROOTMFLP2","ROOTMFLP3", &
                               "ROOTMIP1 ","ROOTMIP2 ","ROOTMIP3 ","ROOTMOOP1","ROOTMOOP2","ROOTMOOP3","ROOTMXB1 ", &
                               "ROOTMXB2 ","ROOTMXB3 ","ROOTMXC1 ","ROOTMXC2 ","ROOTMXC3 ","ROOTMYB1 ","ROOTMYB2 ", &
                               "ROOTMYB3 ","ROOTMYC1 ","ROOTMYC2 ","ROOTMYC3 ","ROOTMZB1 ","ROOTMZB2 ","ROOTMZB3 ", &
                               "ROOTMZC1 ","ROOTMZC2 ","ROOTMZC3 ","ROTACCEL ","ROTCP    ","ROTCQ    ","ROTCT    ", &
                               "ROTFURL  ","ROTFURLA ","ROTFURLP ","ROTFURLV ","ROTPWR   ","ROTSPEED ","ROTTEETA ", &
                               "ROTTEETP ","ROTTEETV ","ROTTHRUST","ROTTORQ  ","SPN1ALXB1","SPN1ALXB2","SPN1ALXB3", &
                               "SPN1ALYB1","SPN1ALYB2","SPN1ALYB3","SPN1ALZB1","SPN1ALZB2","SPN1ALZB3","SPN1FLXB1", &
                               "SPN1FLXB2","SPN1FLXB3","SPN1FLYB1","SPN1FLYB2","SPN1FLYB3","SPN1FLZB1","SPN1FLZB2", &
                               "SPN1FLZB3","SPN1MLXB1","SPN1MLXB2","SPN1MLXB3","SPN1MLYB1","SPN1MLYB2","SPN1MLYB3", &
                               "SPN1MLZB1","SPN1MLZB2","SPN1MLZB3","SPN1RDXB1","SPN1RDXB2","SPN1RDXB3","SPN1RDYB1", &
                               "SPN1RDYB2","SPN1RDYB3","SPN1RDZB1","SPN1RDZB2","SPN1RDZB3","SPN1TDXB1","SPN1TDXB2", &
                               "SPN1TDXB3","SPN1TDYB1","SPN1TDYB2","SPN1TDYB3","SPN1TDZB1","SPN1TDZB2","SPN1TDZB3", &
                               "SPN2ALXB1","SPN2ALXB2","SPN2ALXB3","SPN2ALYB1","SPN2ALYB2","SPN2ALYB3","SPN2ALZB1", &
                               "SPN2ALZB2","SPN2ALZB3","SPN2FLXB1","SPN2FLXB2","SPN2FLXB3","SPN2FLYB1","SPN2FLYB2", &
                               "SPN2FLYB3","SPN2FLZB1","SPN2FLZB2","SPN2FLZB3","SPN2MLXB1","SPN2MLXB2","SPN2MLXB3", &
                               "SPN2MLYB1","SPN2MLYB2","SPN2MLYB3","SPN2MLZB1","SPN2MLZB2","SPN2MLZB3","SPN2RDXB1", &
                               "SPN2RDXB2","SPN2RDXB3","SPN2RDYB1","SPN2RDYB2","SPN2RDYB3","SPN2RDZB1","SPN2RDZB2", &
                               "SPN2RDZB3","SPN2TDXB1","SPN2TDXB2","SPN2TDXB3","SPN2TDYB1","SPN2TDYB2","SPN2TDYB3", &
                               "SPN2TDZB1","SPN2TDZB2","SPN2TDZB3","SPN3ALXB1","SPN3ALXB2","SPN3ALXB3","SPN3ALYB1", &
                               "SPN3ALYB2","SPN3ALYB3","SPN3ALZB1","SPN3ALZB2","SPN3ALZB3","SPN3FLXB1","SPN3FLXB2", &
                               "SPN3FLXB3","SPN3FLYB1","SPN3FLYB2","SPN3FLYB3","SPN3FLZB1","SPN3FLZB2","SPN3FLZB3", &
                               "SPN3MLXB1","SPN3MLXB2","SPN3MLXB3","SPN3MLYB1","SPN3MLYB2","SPN3MLYB3","SPN3MLZB1", &
                               "SPN3MLZB2","SPN3MLZB3","SPN3RDXB1","SPN3RDXB2","SPN3RDXB3","SPN3RDYB1","SPN3RDYB2", &
                               "SPN3RDYB3","SPN3RDZB1","SPN3RDZB2","SPN3RDZB3","SPN3TDXB1","SPN3TDXB2","SPN3TDXB3", &
                               "SPN3TDYB1","SPN3TDYB2","SPN3TDYB3","SPN3TDZB1","SPN3TDZB2","SPN3TDZB3","SPN4ALXB1", &
                               "SPN4ALXB2","SPN4ALXB3","SPN4ALYB1","SPN4ALYB2","SPN4ALYB3","SPN4ALZB1","SPN4ALZB2", &
                               "SPN4ALZB3","SPN4FLXB1","SPN4FLXB2","SPN4FLXB3","SPN4FLYB1","SPN4FLYB2","SPN4FLYB3", &
                               "SPN4FLZB1","SPN4FLZB2","SPN4FLZB3","SPN4MLXB1","SPN4MLXB2","SPN4MLXB3","SPN4MLYB1", &
                               "SPN4MLYB2","SPN4MLYB3","SPN4MLZB1","SPN4MLZB2","SPN4MLZB3","SPN4RDXB1","SPN4RDXB2", &
                               "SPN4RDXB3","SPN4RDYB1","SPN4RDYB2","SPN4RDYB3","SPN4RDZB1","SPN4RDZB2","SPN4RDZB3", &
                               "SPN4TDXB1","SPN4TDXB2","SPN4TDXB3","SPN4TDYB1","SPN4TDYB2","SPN4TDYB3","SPN4TDZB1", &
                               "SPN4TDZB2","SPN4TDZB3","SPN5ALXB1","SPN5ALXB2","SPN5ALXB3","SPN5ALYB1","SPN5ALYB2", &
                               "SPN5ALYB3","SPN5ALZB1","SPN5ALZB2","SPN5ALZB3","SPN5FLXB1","SPN5FLXB2","SPN5FLXB3", &
                               "SPN5FLYB1","SPN5FLYB2","SPN5FLYB3","SPN5FLZB1","SPN5FLZB2","SPN5FLZB3","SPN5MLXB1", &
                               "SPN5MLXB2","SPN5MLXB3","SPN5MLYB1","SPN5MLYB2","SPN5MLYB3","SPN5MLZB1","SPN5MLZB2", &
                               "SPN5MLZB3","SPN5RDXB1","SPN5RDXB2","SPN5RDXB3","SPN5RDYB1","SPN5RDYB2","SPN5RDYB3", &
                               "SPN5RDZB1","SPN5RDZB2","SPN5RDZB3","SPN5TDXB1","SPN5TDXB2","SPN5TDXB3","SPN5TDYB1", &
                               "SPN5TDYB2","SPN5TDYB3","SPN5TDZB1","SPN5TDZB2","SPN5TDZB3","SPN6ALXB1","SPN6ALXB2", &
                               "SPN6ALXB3","SPN6ALYB1","SPN6ALYB2","SPN6ALYB3","SPN6ALZB1","SPN6ALZB2","SPN6ALZB3", &
                               "SPN6FLXB1","SPN6FLXB2","SPN6FLXB3","SPN6FLYB1","SPN6FLYB2","SPN6FLYB3","SPN6FLZB1", &
                               "SPN6FLZB2","SPN6FLZB3","SPN6MLXB1","SPN6MLXB2","SPN6MLXB3","SPN6MLYB1","SPN6MLYB2", &
                               "SPN6MLYB3","SPN6MLZB1","SPN6MLZB2","SPN6MLZB3","SPN6RDXB1","SPN6RDXB2","SPN6RDXB3", &
                               "SPN6RDYB1","SPN6RDYB2","SPN6RDYB3","SPN6RDZB1","SPN6RDZB2","SPN6RDZB3","SPN6TDXB1", &
                               "SPN6TDXB2","SPN6TDXB3","SPN6TDYB1","SPN6TDYB2","SPN6TDYB3","SPN6TDZB1","SPN6TDZB2", &
                               "SPN6TDZB3","SPN7ALXB1","SPN7ALXB2","SPN7ALXB3","SPN7ALYB1","SPN7ALYB2","SPN7ALYB3", &
                               "SPN7ALZB1","SPN7ALZB2","SPN7ALZB3","SPN7FLXB1","SPN7FLXB2","SPN7FLXB3","SPN7FLYB1", &
                               "SPN7FLYB2","SPN7FLYB3","SPN7FLZB1","SPN7FLZB2","SPN7FLZB3","SPN7MLXB1","SPN7MLXB2", &
                               "SPN7MLXB3","SPN7MLYB1","SPN7MLYB2","SPN7MLYB3","SPN7MLZB1","SPN7MLZB2","SPN7MLZB3", &
                               "SPN7RDXB1","SPN7RDXB2","SPN7RDXB3","SPN7RDYB1","SPN7RDYB2","SPN7RDYB3","SPN7RDZB1", &
                               "SPN7RDZB2","SPN7RDZB3","SPN7TDXB1","SPN7TDXB2","SPN7TDXB3","SPN7TDYB1","SPN7TDYB2", &
                               "SPN7TDYB3","SPN7TDZB1","SPN7TDZB2","SPN7TDZB3","SPN8ALXB1","SPN8ALXB2","SPN8ALXB3", &
                               "SPN8ALYB1","SPN8ALYB2","SPN8ALYB3","SPN8ALZB1","SPN8ALZB2","SPN8ALZB3","SPN8FLXB1", &
                               "SPN8FLXB2","SPN8FLXB3","SPN8FLYB1","SPN8FLYB2","SPN8FLYB3","SPN8FLZB1","SPN8FLZB2", &
                               "SPN8FLZB3","SPN8MLXB1","SPN8MLXB2","SPN8MLXB3","SPN8MLYB1","SPN8MLYB2","SPN8MLYB3", &
                               "SPN8MLZB1","SPN8MLZB2","SPN8MLZB3","SPN8RDXB1","SPN8RDXB2","SPN8RDXB3","SPN8RDYB1", &
                               "SPN8RDYB2","SPN8RDYB3","SPN8RDZB1","SPN8RDZB2","SPN8RDZB3","SPN8TDXB1","SPN8TDXB2", &
                               "SPN8TDXB3","SPN8TDYB1","SPN8TDYB2","SPN8TDYB3","SPN8TDZB1","SPN8TDZB2","SPN8TDZB3", &
                               "SPN9ALXB1","SPN9ALXB2","SPN9ALXB3","SPN9ALYB1","SPN9ALYB2","SPN9ALYB3","SPN9ALZB1", &
                               "SPN9ALZB2","SPN9ALZB3","SPN9FLXB1","SPN9FLXB2","SPN9FLXB3","SPN9FLYB1","SPN9FLYB2", &
                               "SPN9FLYB3","SPN9FLZB1","SPN9FLZB2","SPN9FLZB3","SPN9MLXB1","SPN9MLXB2","SPN9MLXB3", &
                               "SPN9MLYB1","SPN9MLYB2","SPN9MLYB3","SPN9MLZB1","SPN9MLZB2","SPN9MLZB3","SPN9RDXB1", &
                               "SPN9RDXB2","SPN9RDXB3","SPN9RDYB1","SPN9RDYB2","SPN9RDYB3","SPN9RDZB1","SPN9RDZB2", &
                               "SPN9RDZB3","SPN9TDXB1","SPN9TDXB2","SPN9TDXB3","SPN9TDYB1","SPN9TDYB2","SPN9TDYB3", &
                               "SPN9TDZB1","SPN9TDZB2","SPN9TDZB3","TAILFURL ","TAILFURLA","TAILFURLP","TAILFURLV", &
                               "TEETAYA  ","TEETDEFL ","TEETPYA  ","TEETVYA  ","TFINALPHA","TFINCDRAG","TFINCLIFT", &
                               "TFINCPFX ","TFINCPFY ","TFINDNPRS","TFRLBRM  ","TIP2TWR1 ","TIP2TWR2 ","TIP2TWR3 ", &
                               "TIPALXB1 ","TIPALXB2 ","TIPALXB3 ","TIPALYB1 ","TIPALYB2 ","TIPALYB3 ","TIPALZB1 ", &
                               "TIPALZB2 ","TIPALZB3 ","TIPCLRNC1","TIPCLRNC2","TIPCLRNC3","TIPDXB1  ","TIPDXB2  ", &
                               "TIPDXB3  ","TIPDXC1  ","TIPDXC2  ","TIPDXC3  ","TIPDYB1  ","TIPDYB2  ","TIPDYB3  ", &
                               "TIPDYC1  ","TIPDYC2  ","TIPDYC3  ","TIPDZB1  ","TIPDZB2  ","TIPDZB3  ","TIPDZC1  ", &
                               "TIPDZC2  ","TIPDZC3  ","TIPRDXB1 ","TIPRDXB2 ","TIPRDXB3 ","TIPRDYB1 ","TIPRDYB2 ", &
                               "TIPRDYB3 ","TIPRDZB1 ","TIPRDZB2 ","TIPRDZB3 ","TIPRDZC1 ","TIPRDZC2 ","TIPRDZC3 ", &
                               "TIPSPDRAT","TOTWINDV ","TSR      ","TTDSPAX  ","TTDSPFA  ","TTDSPPTCH","TTDSPROLL", &
                               "TTDSPSS  ","TTDSPTWST","TWHT1ALXT","TWHT1ALYT","TWHT1ALZT","TWHT1FLXT","TWHT1FLYT", &
                               "TWHT1FLZT","TWHT1MLXT","TWHT1MLYT","TWHT1MLZT","TWHT1RDXT","TWHT1RDYT","TWHT1RDZT", &
                               "TWHT1RPXI","TWHT1RPYI","TWHT1RPZI","TWHT1TDXT","TWHT1TDYT","TWHT1TDZT","TWHT1TPXI", &
                               "TWHT1TPYI","TWHT1TPZI","TWHT2ALXT","TWHT2ALYT","TWHT2ALZT","TWHT2FLXT","TWHT2FLYT", &
                               "TWHT2FLZT","TWHT2MLXT","TWHT2MLYT","TWHT2MLZT","TWHT2RDXT","TWHT2RDYT","TWHT2RDZT", &
                               "TWHT2RPXI","TWHT2RPYI","TWHT2RPZI","TWHT2TDXT","TWHT2TDYT","TWHT2TDZT","TWHT2TPXI", &
                               "TWHT2TPYI","TWHT2TPZI","TWHT3ALXT","TWHT3ALYT","TWHT3ALZT","TWHT3FLXT","TWHT3FLYT", &
                               "TWHT3FLZT","TWHT3MLXT","TWHT3MLYT","TWHT3MLZT","TWHT3RDXT","TWHT3RDYT","TWHT3RDZT", &
                               "TWHT3RPXI","TWHT3RPYI","TWHT3RPZI","TWHT3TDXT","TWHT3TDYT","TWHT3TDZT","TWHT3TPXI", &
                               "TWHT3TPYI","TWHT3TPZI","TWHT4ALXT","TWHT4ALYT","TWHT4ALZT","TWHT4FLXT","TWHT4FLYT", &
                               "TWHT4FLZT","TWHT4MLXT","TWHT4MLYT","TWHT4MLZT","TWHT4RDXT","TWHT4RDYT","TWHT4RDZT", &
                               "TWHT4RPXI","TWHT4RPYI","TWHT4RPZI","TWHT4TDXT","TWHT4TDYT","TWHT4TDZT","TWHT4TPXI", &
                               "TWHT4TPYI","TWHT4TPZI","TWHT5ALXT","TWHT5ALYT","TWHT5ALZT","TWHT5FLXT","TWHT5FLYT", &
                               "TWHT5FLZT","TWHT5MLXT","TWHT5MLYT","TWHT5MLZT","TWHT5RDXT","TWHT5RDYT","TWHT5RDZT", &
                               "TWHT5RPXI","TWHT5RPYI","TWHT5RPZI","TWHT5TDXT","TWHT5TDYT","TWHT5TDZT","TWHT5TPXI", &
                               "TWHT5TPYI","TWHT5TPZI","TWHT6ALXT","TWHT6ALYT","TWHT6ALZT","TWHT6FLXT","TWHT6FLYT", &
                               "TWHT6FLZT","TWHT6MLXT","TWHT6MLYT","TWHT6MLZT","TWHT6RDXT","TWHT6RDYT","TWHT6RDZT", &
                               "TWHT6RPXI","TWHT6RPYI","TWHT6RPZI","TWHT6TDXT","TWHT6TDYT","TWHT6TDZT","TWHT6TPXI", &
                               "TWHT6TPYI","TWHT6TPZI","TWHT7ALXT","TWHT7ALYT","TWHT7ALZT","TWHT7FLXT","TWHT7FLYT", &
                               "TWHT7FLZT","TWHT7MLXT","TWHT7MLYT","TWHT7MLZT","TWHT7RDXT","TWHT7RDYT","TWHT7RDZT", &
                               "TWHT7RPXI","TWHT7RPYI","TWHT7RPZI","TWHT7TDXT","TWHT7TDYT","TWHT7TDZT","TWHT7TPXI", &
                               "TWHT7TPYI","TWHT7TPZI","TWHT8ALXT","TWHT8ALYT","TWHT8ALZT","TWHT8FLXT","TWHT8FLYT", &
                               "TWHT8FLZT","TWHT8MLXT","TWHT8MLYT","TWHT8MLZT","TWHT8RDXT","TWHT8RDYT","TWHT8RDZT", &
                               "TWHT8RPXI","TWHT8RPYI","TWHT8RPZI","TWHT8TDXT","TWHT8TDYT","TWHT8TDZT","TWHT8TPXI", &
                               "TWHT8TPYI","TWHT8TPZI","TWHT9ALXT","TWHT9ALYT","TWHT9ALZT","TWHT9FLXT","TWHT9FLYT", &
                               "TWHT9FLZT","TWHT9MLXT","TWHT9MLYT","TWHT9MLZT","TWHT9RDXT","TWHT9RDYT","TWHT9RDZT", &
                               "TWHT9RPXI","TWHT9RPYI","TWHT9RPZI","TWHT9TDXT","TWHT9TDYT","TWHT9TDZT","TWHT9TPXI", &
                               "TWHT9TPYI","TWHT9TPZI","TWRBSFXT ","TWRBSFYT ","TWRBSFZT ","TWRBSMXT ","TWRBSMYT ", &
                               "TWRBSMZT ","TWRCLRNC1","TWRCLRNC2","TWRCLRNC3","TWSTDEFL1","TWSTDEFL2","TWSTDEFL3", &
                               "UWIND    ","VERWNDDIR","VWIND    ","WAVE1AXI ","WAVE1AYI ","WAVE1AZI ","WAVE1VXI ", &
                               "WAVE1VYI ","WAVE1VZI ","WAVE2AXI ","WAVE2AYI ","WAVE2AZI ","WAVE2VXI ","WAVE2VYI ", &
                               "WAVE2VZI ","WAVE3AXI ","WAVE3AYI ","WAVE3AZI ","WAVE3VXI ","WAVE3VYI ","WAVE3VZI ", &
                               "WAVE4AXI ","WAVE4AYI ","WAVE4AZI ","WAVE4VXI ","WAVE4VYI ","WAVE4VZI ","WAVE5AXI ", &
                               "WAVE5AYI ","WAVE5AZI ","WAVE5VXI ","WAVE5VYI ","WAVE5VZI ","WAVE6AXI ","WAVE6AYI ", &
                               "WAVE6AZI ","WAVE6VXI ","WAVE6VYI ","WAVE6VZI ","WAVE7AXI ","WAVE7AYI ","WAVE7AZI ", &
                               "WAVE7VXI ","WAVE7VYI ","WAVE7VZI ","WAVE8AXI ","WAVE8AYI ","WAVE8AZI ","WAVE8VXI ", &
                               "WAVE8VYI ","WAVE8VZI ","WAVE9AXI ","WAVE9AYI ","WAVE9AZI ","WAVE9VXI ","WAVE9VYI ", &
                               "WAVE9VZI ","WAVEELEV ","WINDVXI  ","WINDVYI  ","WINDVZI  ","WWIND    ","YAWACCEL ", &
                               "YAWAZN   ","YAWAZP   ","YAWBRFXN ","YAWBRFXP ","YAWBRFYN ","YAWBRFYP ","YAWBRFZN ", &
                               "YAWBRFZP ","YAWBRMXN ","YAWBRMXP ","YAWBRMYN ","YAWBRMYP ","YAWBRMZN ","YAWBRMZP ", &
                               "YAWBRRAXP","YAWBRRAYP","YAWBRRAZP","YAWBRRDXT","YAWBRRDYT","YAWBRRDZT","YAWBRRVXP", &
                               "YAWBRRVYP","YAWBRRVZP","YAWBRTAXP","YAWBRTAYP","YAWBRTAZP","YAWBRTDXP","YAWBRTDXT", &
                               "YAWBRTDYP","YAWBRTDYT","YAWBRTDZP","YAWBRTDZT","YAWMOM   ","YAWPOS   ","YAWPZN   ", &
                               "YAWPZP   ","YAWRATE  ","YAWVZN   ","YAWVZP   "/)
   INTEGER,      PARAMETER  :: ParamIndxAry(1110) =  (/ &                          ! This lists the index into AllOuts(:) of the allowed parameters ValidParamAry(:)
                                 Anch1Ang ,  Anch1Ten ,  Anch2Ang ,  Anch2Ten ,  Anch3Ang ,  Anch3Ten ,  Anch4Ang , &
                                 Anch4Ten ,  Anch5Ang ,  Anch5Ten ,  Anch6Ang ,  Anch6Ten ,  Anch7Ang ,  Anch7Ten , &
                                 Anch8Ang ,  Anch8Ten ,  Anch9Ang ,  Anch9Ten , LSSTipPxa , PtchPMzc1 , PtchPMzc2 , &
                                PtchPMzc3 , PtchPMzc1 , PtchPMzc2 , PtchPMzc3 , CThrstRad , CThrstAzm , CThrstRad , &
                                 Fair1Ang ,  Fair1Ten ,  Fair2Ang ,  Fair2Ten ,  Fair3Ang ,  Fair3Ten ,  Fair4Ang , &
                                 Fair4Ten ,  Fair5Ang ,  Fair5Ten ,  Fair6Ang ,  Fair6Ten ,  Fair7Ang ,  Fair7Ten , &
                                 Fair8Ang ,  Fair8Ten ,  Fair9Ang ,  Fair9Ten ,   HSShftA ,     GenCp ,     GenCq , &
                                   GenPwr ,   HSShftV ,     GenTq ,  HorWindV , HorWndDir ,   HSSBrTq ,   HSShftA , &
                                 HSShftCp ,  HSShftCq , HSShftPwr ,  HSShftTq ,   HSShftV ,   TipDyc1 ,   TipDyc2 , &
                                  TipDyc3 , LSSGagAxa , LSSGagAxa , LSSGagAxa , LSShftFxa , LSShftFxa , LSShftFya , &
                                LSShftFys , LSShftFza , LSShftFzs , LSShftMxa , LSShftMxa , LSSGagMya , LSSGagMys , &
                                LSSGagMza , LSSGagMzs , LSSGagPxa , LSSGagPxa , LSSGagPxa , LSSGagVxa , LSSGagVxa , &
                                LSSGagVxa ,     RotCp ,     RotCq ,     RotCt , LSShftFxa , LSShftFxa , LSShftFya , &
                                LSShftFys , LSShftFza , LSShftFzs , LSShftMxa , LSShftMxa ,    RotPwr , LSShftMxa , &
                                LSSTipAxa , LSSTipAxa , LSSTipAxa , LSSTipMya , LSSTipMys , LSSTipMza , LSSTipMzs , &
                                LSSTipPxa , LSSTipPxa , LSSTipPxa , LSSTipVxa , LSSTipVxa , LSSTipVxa ,    YawPzn , &
                                   YawAzn , NacYawErr ,    YawPzn ,    YawVzn , NcIMURAxs , NcIMURAys , NcIMURAzs , &
                                NcIMURVxs , NcIMURVys , NcIMURVzs , NcIMUTAxs , NcIMUTAys , NcIMUTAzs , NcIMUTVxs , &
                                NcIMUTVys , NcIMUTVzs ,   TipDxc1 ,   TipDxc2 ,   TipDxc3 ,  TipRDyb1 ,  TipRDyb2 , &
                                 TipRDyb3 , PtchPMzc1 , PtchPMzc2 , PtchPMzc3 , PtchPMzc1 , PtchPMzc2 , PtchPMzc3 , &
                                  PtfmFxi ,   PtfmFxt ,   PtfmFyi ,   PtfmFyt ,   PtfmFzi ,   PtfmFzt ,  PtfmTDzi , &
                                  PtfmMxi ,   PtfmMxt ,   PtfmMyi ,   PtfmMyt ,   PtfmMzi ,   PtfmMzt ,  PtfmRDyi , &
                                 PtfmRAxi ,  PtfmRAxt ,  PtfmRAyi ,  PtfmRAyt ,  PtfmRAzi ,  PtfmRAzt ,  PtfmRDxi , &
                                 PtfmRDyi ,  PtfmRDzi ,  PtfmRDxi ,  PtfmRVxi ,  PtfmRVxt ,  PtfmRVyi ,  PtfmRVyt , &
                                 PtfmRVzi ,  PtfmRVzt ,  PtfmTDxi ,  PtfmTDyi ,  PtfmTAxi ,  PtfmTAxt ,  PtfmTAyi , &
                                 PtfmTAyt ,  PtfmTAzi ,  PtfmTAzt ,  PtfmTDxi ,  PtfmTDxt ,  PtfmTDyi ,  PtfmTDyt , &
                                 PtfmTDzi ,  PtfmTDzt ,  PtfmTVxi ,  PtfmTVxt ,  PtfmTVyi ,  PtfmTVyt ,  PtfmTVzi , &
                                 PtfmTVzt ,  PtfmRDzi ,  QD2_B1E1 ,  QD2_B1F1 ,  QD2_B1F2 ,  QD2_B2E1 ,  QD2_B2F1 , &
                                 QD2_B2F2 ,  QD2_B3E1 ,  QD2_B3F1 ,  QD2_B3F2 ,  QD2_DrTr ,  QD2_GeAz ,    QD2_Hv , &
                                    QD2_P ,     QD2_R ,  QD2_RFrl ,    QD2_Sg ,    QD2_Sw ,  QD2_Teet ,  QD2_TFA1 , &
                                 QD2_TFA2 ,  QD2_TFrl ,  QD2_TSS1 ,  QD2_TSS2 ,     QD2_Y ,   QD2_Yaw ,   QD_B1E1 , &
                                  QD_B1F1 ,   QD_B1F2 ,   QD_B2E1 ,   QD_B2F1 ,   QD_B2F2 ,   QD_B3E1 ,   QD_B3F1 , &
                                  QD_B3F2 ,   QD_DrTr ,   QD_GeAz ,     QD_Hv ,      QD_P ,      QD_R ,   QD_RFrl , &
                                    QD_Sg ,     QD_Sw ,   QD_Teet ,   QD_TFA1 ,   QD_TFA2 ,   QD_TFrl ,   QD_TSS1 , &
                                  QD_TSS2 ,      QD_Y ,    QD_Yaw ,    Q_B1E1 ,    Q_B1F1 ,    Q_B1F2 ,    Q_B2E1 , &
                                   Q_B2F1 ,    Q_B2F2 ,    Q_B3E1 ,    Q_B3F1 ,    Q_B3F2 ,    Q_DrTr ,    Q_GeAz , &
                                     Q_Hv ,       Q_P ,       Q_R ,    Q_RFrl ,      Q_Sg ,      Q_Sw ,    Q_Teet , &
                                   Q_TFA1 ,    Q_TFA2 ,    Q_TFrl ,    Q_TSS1 ,    Q_TSS2 ,       Q_Y ,     Q_Yaw , &
                                  RFrlBrM ,  TipRDxb1 ,  TipRDxb2 ,  TipRDxb3 ,  RootFxb1 ,  RootFxb2 ,  RootFxb3 , &
                                 RootFxc1 ,  RootFxc2 ,  RootFxc3 ,  RootFyb1 ,  RootFyb2 ,  RootFyb3 ,  RootFyc1 , &
                                 RootFyc2 ,  RootFyc3 ,  RootFzc1 ,  RootFzc2 ,  RootFzc3 ,  RootFzc1 ,  RootFzc2 , &
                                 RootFzc3 ,  RootMxb1 ,  RootMxb2 ,  RootMxb3 ,  RootMyb1 ,  RootMyb2 ,  RootMyb3 , &
                                 RootMxc1 ,  RootMxc2 ,  RootMxc3 ,  RootMyc1 ,  RootMyc2 ,  RootMyc3 ,  RootMxb1 , &
                                 RootMxb2 ,  RootMxb3 ,  RootMxc1 ,  RootMxc2 ,  RootMxc3 ,  RootMyb1 ,  RootMyb2 , &
                                 RootMyb3 ,  RootMyc1 ,  RootMyc2 ,  RootMyc3 ,  RootMzc1 ,  RootMzc2 ,  RootMzc3 , &
                                 RootMzc1 ,  RootMzc2 ,  RootMzc3 , LSSTipAxa ,     RotCp ,     RotCq ,     RotCt , &
                                 RotFurlP ,  RotFurlA ,  RotFurlP ,  RotFurlV ,    RotPwr , LSSTipVxa ,   TeetAya , &
                                  TeetPya ,   TeetVya , LSShftFxa , LSShftMxa , Spn1ALxb1 , Spn1ALxb2 , Spn1ALxb3 , &
                                Spn1ALyb1 , Spn1ALyb2 , Spn1ALyb3 , Spn1ALzb1 , Spn1ALzb2 , Spn1ALzb3 , Spn1FLxb1 , &
                                Spn1FLxb2 , Spn1FLxb3 , Spn1FLyb1 , Spn1FLyb2 , Spn1FLyb3 , Spn1FLzb1 , Spn1FLzb2 , &
                                Spn1FLzb3 , Spn1MLxb1 , Spn1MLxb2 , Spn1MLxb3 , Spn1MLyb1 , Spn1MLyb2 , Spn1MLyb3 , &
                                Spn1MLzb1 , Spn1MLzb2 , Spn1MLzb3 , Spn1RDxb1 , Spn1RDxb2 , Spn1RDxb3 , Spn1RDyb1 , &
                                Spn1RDyb2 , Spn1RDyb3 , Spn1RDzb1 , Spn1RDzb2 , Spn1RDzb3 , Spn1TDxb1 , Spn1TDxb2 , &
                                Spn1TDxb3 , Spn1TDyb1 , Spn1TDyb2 , Spn1TDyb3 , Spn1TDzb1 , Spn1TDzb2 , Spn1TDzb3 , &
                                Spn2ALxb1 , Spn2ALxb2 , Spn2ALxb3 , Spn2ALyb1 , Spn2ALyb2 , Spn2ALyb3 , Spn2ALzb1 , &
                                Spn2ALzb2 , Spn2ALzb3 , Spn2FLxb1 , Spn2FLxb2 , Spn2FLxb3 , Spn2FLyb1 , Spn2FLyb2 , &
                                Spn2FLyb3 , Spn2FLzb1 , Spn2FLzb2 , Spn2FLzb3 , Spn2MLxb1 , Spn2MLxb2 , Spn2MLxb3 , &
                                Spn2MLyb1 , Spn2MLyb2 , Spn2MLyb3 , Spn2MLzb1 , Spn2MLzb2 , Spn2MLzb3 , Spn2RDxb1 , &
                                Spn2RDxb2 , Spn2RDxb3 , Spn2RDyb1 , Spn2RDyb2 , Spn2RDyb3 , Spn2RDzb1 , Spn2RDzb2 , &
                                Spn2RDzb3 , Spn2TDxb1 , Spn2TDxb2 , Spn2TDxb3 , Spn2TDyb1 , Spn2TDyb2 , Spn2TDyb3 , &
                                Spn2TDzb1 , Spn2TDzb2 , Spn2TDzb3 , Spn3ALxb1 , Spn3ALxb2 , Spn3ALxb3 , Spn3ALyb1 , &
                                Spn3ALyb2 , Spn3ALyb3 , Spn3ALzb1 , Spn3ALzb2 , Spn3ALzb3 , Spn3FLxb1 , Spn3FLxb2 , &
                                Spn3FLxb3 , Spn3FLyb1 , Spn3FLyb2 , Spn3FLyb3 , Spn3FLzb1 , Spn3FLzb2 , Spn3FLzb3 , &
                                Spn3MLxb1 , Spn3MLxb2 , Spn3MLxb3 , Spn3MLyb1 , Spn3MLyb2 , Spn3MLyb3 , Spn3MLzb1 , &
                                Spn3MLzb2 , Spn3MLzb3 , Spn3RDxb1 , Spn3RDxb2 , Spn3RDxb3 , Spn3RDyb1 , Spn3RDyb2 , &
                                Spn3RDyb3 , Spn3RDzb1 , Spn3RDzb2 , Spn3RDzb3 , Spn3TDxb1 , Spn3TDxb2 , Spn3TDxb3 , &
                                Spn3TDyb1 , Spn3TDyb2 , Spn3TDyb3 , Spn3TDzb1 , Spn3TDzb2 , Spn3TDzb3 , Spn4ALxb1 , &
                                Spn4ALxb2 , Spn4ALxb3 , Spn4ALyb1 , Spn4ALyb2 , Spn4ALyb3 , Spn4ALzb1 , Spn4ALzb2 , &
                                Spn4ALzb3 , Spn4FLxb1 , Spn4FLxb2 , Spn4FLxb3 , Spn4FLyb1 , Spn4FLyb2 , Spn4FLyb3 , &
                                Spn4FLzb1 , Spn4FLzb2 , Spn4FLzb3 , Spn4MLxb1 , Spn4MLxb2 , Spn4MLxb3 , Spn4MLyb1 , &
                                Spn4MLyb2 , Spn4MLyb3 , Spn4MLzb1 , Spn4MLzb2 , Spn4MLzb3 , Spn4RDxb1 , Spn4RDxb2 , &
                                Spn4RDxb3 , Spn4RDyb1 , Spn4RDyb2 , Spn4RDyb3 , Spn4RDzb1 , Spn4RDzb2 , Spn4RDzb3 , &
                                Spn4TDxb1 , Spn4TDxb2 , Spn4TDxb3 , Spn4TDyb1 , Spn4TDyb2 , Spn4TDyb3 , Spn4TDzb1 , &
                                Spn4TDzb2 , Spn4TDzb3 , Spn5ALxb1 , Spn5ALxb2 , Spn5ALxb3 , Spn5ALyb1 , Spn5ALyb2 , &
                                Spn5ALyb3 , Spn5ALzb1 , Spn5ALzb2 , Spn5ALzb3 , Spn5FLxb1 , Spn5FLxb2 , Spn5FLxb3 , &
                                Spn5FLyb1 , Spn5FLyb2 , Spn5FLyb3 , Spn5FLzb1 , Spn5FLzb2 , Spn5FLzb3 , Spn5MLxb1 , &
                                Spn5MLxb2 , Spn5MLxb3 , Spn5MLyb1 , Spn5MLyb2 , Spn5MLyb3 , Spn5MLzb1 , Spn5MLzb2 , &
                                Spn5MLzb3 , Spn5RDxb1 , Spn5RDxb2 , Spn5RDxb3 , Spn5RDyb1 , Spn5RDyb2 , Spn5RDyb3 , &
                                Spn5RDzb1 , Spn5RDzb2 , Spn5RDzb3 , Spn5TDxb1 , Spn5TDxb2 , Spn5TDxb3 , Spn5TDyb1 , &
                                Spn5TDyb2 , Spn5TDyb3 , Spn5TDzb1 , Spn5TDzb2 , Spn5TDzb3 , Spn6ALxb1 , Spn6ALxb2 , &
                                Spn6ALxb3 , Spn6ALyb1 , Spn6ALyb2 , Spn6ALyb3 , Spn6ALzb1 , Spn6ALzb2 , Spn6ALzb3 , &
                                Spn6FLxb1 , Spn6FLxb2 , Spn6FLxb3 , Spn6FLyb1 , Spn6FLyb2 , Spn6FLyb3 , Spn6FLzb1 , &
                                Spn6FLzb2 , Spn6FLzb3 , Spn6MLxb1 , Spn6MLxb2 , Spn6MLxb3 , Spn6MLyb1 , Spn6MLyb2 , &
                                Spn6MLyb3 , Spn6MLzb1 , Spn6MLzb2 , Spn6MLzb3 , Spn6RDxb1 , Spn6RDxb2 , Spn6RDxb3 , &
                                Spn6RDyb1 , Spn6RDyb2 , Spn6RDyb3 , Spn6RDzb1 , Spn6RDzb2 , Spn6RDzb3 , Spn6TDxb1 , &
                                Spn6TDxb2 , Spn6TDxb3 , Spn6TDyb1 , Spn6TDyb2 , Spn6TDyb3 , Spn6TDzb1 , Spn6TDzb2 , &
                                Spn6TDzb3 , Spn7ALxb1 , Spn7ALxb2 , Spn7ALxb3 , Spn7ALyb1 , Spn7ALyb2 , Spn7ALyb3 , &
                                Spn7ALzb1 , Spn7ALzb2 , Spn7ALzb3 , Spn7FLxb1 , Spn7FLxb2 , Spn7FLxb3 , Spn7FLyb1 , &
                                Spn7FLyb2 , Spn7FLyb3 , Spn7FLzb1 , Spn7FLzb2 , Spn7FLzb3 , Spn7MLxb1 , Spn7MLxb2 , &
                                Spn7MLxb3 , Spn7MLyb1 , Spn7MLyb2 , Spn7MLyb3 , Spn7MLzb1 , Spn7MLzb2 , Spn7MLzb3 , &
                                Spn7RDxb1 , Spn7RDxb2 , Spn7RDxb3 , Spn7RDyb1 , Spn7RDyb2 , Spn7RDyb3 , Spn7RDzb1 , &
                                Spn7RDzb2 , Spn7RDzb3 , Spn7TDxb1 , Spn7TDxb2 , Spn7TDxb3 , Spn7TDyb1 , Spn7TDyb2 , &
                                Spn7TDyb3 , Spn7TDzb1 , Spn7TDzb2 , Spn7TDzb3 , Spn8ALxb1 , Spn8ALxb2 , Spn8ALxb3 , &
                                Spn8ALyb1 , Spn8ALyb2 , Spn8ALyb3 , Spn8ALzb1 , Spn8ALzb2 , Spn8ALzb3 , Spn8FLxb1 , &
                                Spn8FLxb2 , Spn8FLxb3 , Spn8FLyb1 , Spn8FLyb2 , Spn8FLyb3 , Spn8FLzb1 , Spn8FLzb2 , &
                                Spn8FLzb3 , Spn8MLxb1 , Spn8MLxb2 , Spn8MLxb3 , Spn8MLyb1 , Spn8MLyb2 , Spn8MLyb3 , &
                                Spn8MLzb1 , Spn8MLzb2 , Spn8MLzb3 , Spn8RDxb1 , Spn8RDxb2 , Spn8RDxb3 , Spn8RDyb1 , &
                                Spn8RDyb2 , Spn8RDyb3 , Spn8RDzb1 , Spn8RDzb2 , Spn8RDzb3 , Spn8TDxb1 , Spn8TDxb2 , &
                                Spn8TDxb3 , Spn8TDyb1 , Spn8TDyb2 , Spn8TDyb3 , Spn8TDzb1 , Spn8TDzb2 , Spn8TDzb3 , &
                                Spn9ALxb1 , Spn9ALxb2 , Spn9ALxb3 , Spn9ALyb1 , Spn9ALyb2 , Spn9ALyb3 , Spn9ALzb1 , &
                                Spn9ALzb2 , Spn9ALzb3 , Spn9FLxb1 , Spn9FLxb2 , Spn9FLxb3 , Spn9FLyb1 , Spn9FLyb2 , &
                                Spn9FLyb3 , Spn9FLzb1 , Spn9FLzb2 , Spn9FLzb3 , Spn9MLxb1 , Spn9MLxb2 , Spn9MLxb3 , &
                                Spn9MLyb1 , Spn9MLyb2 , Spn9MLyb3 , Spn9MLzb1 , Spn9MLzb2 , Spn9MLzb3 , Spn9RDxb1 , &
                                Spn9RDxb2 , Spn9RDxb3 , Spn9RDyb1 , Spn9RDyb2 , Spn9RDyb3 , Spn9RDzb1 , Spn9RDzb2 , &
                                Spn9RDzb3 , Spn9TDxb1 , Spn9TDxb2 , Spn9TDxb3 , Spn9TDyb1 , Spn9TDyb2 , Spn9TDyb3 , &
                                Spn9TDzb1 , Spn9TDzb2 , Spn9TDzb3 , TailFurlP , TailFurlA , TailFurlP , TailFurlV , &
                                  TeetAya ,   TeetPya ,   TeetPya ,   TeetVya , TFinAlpha , TFinCDrag , TFinCLift , &
                                 TFinCPFx ,  TFinCPFy , TFinDnPrs ,   TFrlBrM , TipClrnc1 , TipClrnc2 , TipClrnc3 , &
                                 TipALxb1 ,  TipALxb2 ,  TipALxb3 ,  TipALyb1 ,  TipALyb2 ,  TipALyb3 ,  TipALzb1 , &
                                 TipALzb2 ,  TipALzb3 , TipClrnc1 , TipClrnc2 , TipClrnc3 ,   TipDxb1 ,   TipDxb2 , &
                                  TipDxb3 ,   TipDxc1 ,   TipDxc2 ,   TipDxc3 ,   TipDyb1 ,   TipDyb2 ,   TipDyb3 , &
                                  TipDyc1 ,   TipDyc2 ,   TipDyc3 ,   TipDzc1 ,   TipDzc2 ,   TipDzc3 ,   TipDzc1 , &
                                  TipDzc2 ,   TipDzc3 ,  TipRDxb1 ,  TipRDxb2 ,  TipRDxb3 ,  TipRDyb1 ,  TipRDyb2 , &
                                 TipRDyb3 ,  TipRDzc1 ,  TipRDzc2 ,  TipRDzc3 ,  TipRDzc1 ,  TipRDzc2 ,  TipRDzc3 , &
                                TipSpdRat ,  TotWindV , TipSpdRat , YawBrTDzt , YawBrTDxt , YawBrRDyt , YawBrRDxt , &
                                YawBrTDyt , YawBrRDzt , TwHt1ALxt , TwHt1ALyt , TwHt1ALzt , TwHt1FLxt , TwHt1FLyt , &
                                TwHt1FLzt , TwHt1MLxt , TwHt1MLyt , TwHt1MLzt , TwHt1RDxt , TwHt1RDyt , TwHt1RDzt , &
                                TwHt1RPxi , TwHt1RPyi , TwHt1RPzi , TwHt1TDxt , TwHt1TDyt , TwHt1TDzt , TwHt1TPxi , &
                                TwHt1TPyi , TwHt1TPzi , TwHt2ALxt , TwHt2ALyt , TwHt2ALzt , TwHt2FLxt , TwHt2FLyt , &
                                TwHt2FLzt , TwHt2MLxt , TwHt2MLyt , TwHt2MLzt , TwHt2RDxt , TwHt2RDyt , TwHt2RDzt , &
                                TwHt2RPxi , TwHt2RPyi , TwHt2RPzi , TwHt2TDxt , TwHt2TDyt , TwHt2TDzt , TwHt2TPxi , &
                                TwHt2TPyi , TwHt2TPzi , TwHt3ALxt , TwHt3ALyt , TwHt3ALzt , TwHt3FLxt , TwHt3FLyt , &
                                TwHt3FLzt , TwHt3MLxt , TwHt3MLyt , TwHt3MLzt , TwHt3RDxt , TwHt3RDyt , TwHt3RDzt , &
                                TwHt3RPxi , TwHt3RPyi , TwHt3RPzi , TwHt3TDxt , TwHt3TDyt , TwHt3TDzt , TwHt3TPxi , &
                                TwHt3TPyi , TwHt3TPzi , TwHt4ALxt , TwHt4ALyt , TwHt4ALzt , TwHt4FLxt , TwHt4FLyt , &
                                TwHt4FLzt , TwHt4MLxt , TwHt4MLyt , TwHt4MLzt , TwHt4RDxt , TwHt4RDyt , TwHt4RDzt , &
                                TwHt4RPxi , TwHt4RPyi , TwHt4RPzi , TwHt4TDxt , TwHt4TDyt , TwHt4TDzt , TwHt4TPxi , &
                                TwHt4TPyi , TwHt4TPzi , TwHt5ALxt , TwHt5ALyt , TwHt5ALzt , TwHt5FLxt , TwHt5FLyt , &
                                TwHt5FLzt , TwHt5MLxt , TwHt5MLyt , TwHt5MLzt , TwHt5RDxt , TwHt5RDyt , TwHt5RDzt , &
                                TwHt5RPxi , TwHt5RPyi , TwHt5RPzi , TwHt5TDxt , TwHt5TDyt , TwHt5TDzt , TwHt5TPxi , &
                                TwHt5TPyi , TwHt5TPzi , TwHt6ALxt , TwHt6ALyt , TwHt6ALzt , TwHt6FLxt , TwHt6FLyt , &
                                TwHt6FLzt , TwHt6MLxt , TwHt6MLyt , TwHt6MLzt , TwHt6RDxt , TwHt6RDyt , TwHt6RDzt , &
                                TwHt6RPxi , TwHt6RPyi , TwHt6RPzi , TwHt6TDxt , TwHt6TDyt , TwHt6TDzt , TwHt6TPxi , &
                                TwHt6TPyi , TwHt6TPzi , TwHt7ALxt , TwHt7ALyt , TwHt7ALzt , TwHt7FLxt , TwHt7FLyt , &
                                TwHt7FLzt , TwHt7MLxt , TwHt7MLyt , TwHt7MLzt , TwHt7RDxt , TwHt7RDyt , TwHt7RDzt , &
                                TwHt7RPxi , TwHt7RPyi , TwHt7RPzi , TwHt7TDxt , TwHt7TDyt , TwHt7TDzt , TwHt7TPxi , &
                                TwHt7TPyi , TwHt7TPzi , TwHt8ALxt , TwHt8ALyt , TwHt8ALzt , TwHt8FLxt , TwHt8FLyt , &
                                TwHt8FLzt , TwHt8MLxt , TwHt8MLyt , TwHt8MLzt , TwHt8RDxt , TwHt8RDyt , TwHt8RDzt , &
                                TwHt8RPxi , TwHt8RPyi , TwHt8RPzi , TwHt8TDxt , TwHt8TDyt , TwHt8TDzt , TwHt8TPxi , &
                                TwHt8TPyi , TwHt8TPzi , TwHt9ALxt , TwHt9ALyt , TwHt9ALzt , TwHt9FLxt , TwHt9FLyt , &
                                TwHt9FLzt , TwHt9MLxt , TwHt9MLyt , TwHt9MLzt , TwHt9RDxt , TwHt9RDyt , TwHt9RDzt , &
                                TwHt9RPxi , TwHt9RPyi , TwHt9RPzi , TwHt9TDxt , TwHt9TDyt , TwHt9TDzt , TwHt9TPxi , &
                                TwHt9TPyi , TwHt9TPzi ,  TwrBsFxt ,  TwrBsFyt ,  TwrBsFzt ,  TwrBsMxt ,  TwrBsMyt , &
                                 TwrBsMzt , TipClrnc1 , TipClrnc2 , TipClrnc3 ,  TipRDzc1 ,  TipRDzc2 ,  TipRDzc3 , &
                                  WindVxi , VerWndDir ,   WindVyi ,  Wave1Axi ,  Wave1Ayi ,  Wave1Azi ,  Wave1Vxi , &
                                 Wave1Vyi ,  Wave1Vzi ,  Wave2Axi ,  Wave2Ayi ,  Wave2Azi ,  Wave2Vxi ,  Wave2Vyi , &
                                 Wave2Vzi ,  Wave3Axi ,  Wave3Ayi ,  Wave3Azi ,  Wave3Vxi ,  Wave3Vyi ,  Wave3Vzi , &
                                 Wave4Axi ,  Wave4Ayi ,  Wave4Azi ,  Wave4Vxi ,  Wave4Vyi ,  Wave4Vzi ,  Wave5Axi , &
                                 Wave5Ayi ,  Wave5Azi ,  Wave5Vxi ,  Wave5Vyi ,  Wave5Vzi ,  Wave6Axi ,  Wave6Ayi , &
                                 Wave6Azi ,  Wave6Vxi ,  Wave6Vyi ,  Wave6Vzi ,  Wave7Axi ,  Wave7Ayi ,  Wave7Azi , &
                                 Wave7Vxi ,  Wave7Vyi ,  Wave7Vzi ,  Wave8Axi ,  Wave8Ayi ,  Wave8Azi ,  Wave8Vxi , &
                                 Wave8Vyi ,  Wave8Vzi ,  Wave9Axi ,  Wave9Ayi ,  Wave9Azi ,  Wave9Vxi ,  Wave9Vyi , &
                                 Wave9Vzi ,  WaveElev ,   WindVxi ,   WindVyi ,   WindVzi ,   WindVzi ,    YawAzn , &
                                   YawAzn ,    YawAzn ,  YawBrFxn ,  YawBrFxp ,  YawBrFyn ,  YawBrFyp ,  YawBrFzn , &
                                 YawBrFzn ,  YawBrMxn ,  YawBrMxp ,  YawBrMyn ,  YawBrMyp ,  YawBrMzn ,  YawBrMzn , &
                                YawBrRAxp , YawBrRAyp , YawBrRAzp , YawBrRDxt , YawBrRDyt , YawBrRDzt , YawBrRVxp , &
                                YawBrRVyp , YawBrRVzp , YawBrTAxp , YawBrTAyp , YawBrTAzp , YawBrTDxp , YawBrTDxt , &
                                YawBrTDyp , YawBrTDyt , YawBrTDzp , YawBrTDzt ,  YawBrMzn ,    YawPzn ,    YawPzn , &
                                   YawPzn ,    YawVzn ,    YawVzn ,    YawVzn /)
   CHARACTER(10),PARAMETER  :: ParamUnitsAry(1110) =  (/ &                         ! This lists the units corresponding to the allowed parameters
                               "(deg)     ","(kN)      ","(deg)     ","(kN)      ","(deg)     ","(kN)      ","(deg)     ", &
                               "(kN)      ","(deg)     ","(kN)      ","(deg)     ","(kN)      ","(deg)     ","(kN)      ", &
                               "(deg)     ","(kN)      ","(deg)     ","(kN)      ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(-)       ","(deg)     ","(-)       ", &
                               "(deg)     ","(kN)      ","(deg)     ","(kN)      ","(deg)     ","(kN)      ","(deg)     ", &
                               "(kN)      ","(deg)     ","(kN)      ","(deg)     ","(kN)      ","(deg)     ","(kN)      ", &
                               "(deg)     ","(kN)      ","(deg)     ","(kN)      ","(deg/s^2) ","(-)       ","(-)       ", &
                               "(kW)      ","(rpm)     ","(kN·m)    ","(m/s)     ","(deg)     ","(kN·m)    ","(deg/s^2) ", &
                               "(-)       ","(-)       ","(kW)      ","(kN·m)    ","(rpm)     ","(m)       ","(m)       ", &
                               "(m)       ","(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(rpm)     ","(rpm)     ", &
                               "(rpm)     ","(-)       ","(-)       ","(-)       ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kW)      ","(kN·m)    ", &
                               "(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(deg)     ","(deg)     ","(deg)     ","(rpm)     ","(rpm)     ","(rpm)     ","(deg)     ", &
                               "(deg/s^2) ","(deg)     ","(deg)     ","(deg/s)   ","(deg/s^2) ","(deg/s^2) ","(deg/s^2) ", &
                               "(deg/s)   ","(deg/s)   ","(deg/s)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s)     ", &
                               "(m/s)     ","(m/s)     ","(m)       ","(m)       ","(m)       ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(m)       ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ", &
                               "(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg/s)   ","(deg/s)   ","(deg/s)   ","(deg/s)   ", &
                               "(deg/s)   ","(deg/s)   ","(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ", &
                               "(m/s)     ","(deg)     ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(rad/s^2) ","(rad/s^2) ","(m/s^2)   ", &
                               "(rad/s^2) ","(rad/s^2) ","(rad/s^2) ","(m/s^2)   ","(m/s^2)   ","(rad/s^2) ","(m/s^2)   ", &
                               "(m/s^2)   ","(rad/s^2) ","(m/s^2)   ","(m/s^2)   ","(rad/s^2) ","(rad/s^2) ","(m/s)     ", &
                               "(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ", &
                               "(m/s)     ","(rad/s)   ","(rad/s)   ","(m/s)     ","(rad/s)   ","(rad/s)   ","(rad/s)   ", &
                               "(m/s)     ","(m/s)     ","(rad/s)   ","(m/s)     ","(m/s)     ","(rad/s)   ","(m/s)     ", &
                               "(m/s)     ","(rad/s)   ","(rad/s)   ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(rad)     ","(rad)     ", &
                               "(m)       ","(rad)     ","(rad)     ","(rad)     ","(m)       ","(m)       ","(rad)     ", &
                               "(m)       ","(m)       ","(rad)     ","(m)       ","(m)       ","(rad)     ","(rad)     ", &
                               "(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg/s^2) ","(-)       ","(-)       ","(-)       ", &
                               "(deg)     ","(deg/s^2) ","(deg)     ","(deg/s)   ","(kW)      ","(rpm)     ","(deg/s^2) ", &
                               "(deg)     ","(deg/s)   ","(kN)      ","(kN·m)    ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(deg)     ","(deg/s^2) ","(deg)     ","(deg/s)   ", &
                               "(deg/s^2) ","(deg)     ","(deg)     ","(deg/s)   ","(deg)     ","(-)       ","(-)       ", &
                               "(kN)      ","(kN)      ","(Pa)      ","(kN·m)    ","(m)       ","(m)       ","(m)       ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ","(deg)     ", &
                               "(-)       ","(m/s)     ","(-)       ","(m)       ","(m)       ","(deg)     ","(deg)     ", &
                               "(m)       ","(deg)     ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(deg)     ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg)     ","(deg)     ","(m)       ","(m)       ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(kN)      ","(kN)      ","(kN)      ","(kN·m)    ","(kN·m)    ", &
                               "(kN·m)    ","(m)       ","(m)       ","(m)       ","(deg)     ","(deg)     ","(deg)     ", &
                               "(m/s)     ","(deg)     ","(m/s)     ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s)     ", &
                               "(m/s)     ","(m/s)     ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s)     ","(m/s)     ", &
                               "(m/s)     ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s)     ","(m/s)     ","(m/s)     ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s^2)   ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s^2)   ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ", &
                               "(m/s)     ","(m/s)     ","(m/s)     ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s)     ", &
                               "(m/s)     ","(m/s)     ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m/s)     ","(m/s)     ", &
                               "(m/s)     ","(m)       ","(m/s)     ","(m/s)     ","(m/s)     ","(m/s)     ","(deg/s^2) ", &
                               "(deg/s^2) ","(deg/s^2) ","(kN)      ","(kN)      ","(kN)      ","(kN)      ","(kN)      ", &
                               "(kN)      ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ","(kN·m)    ", &
                               "(deg/s^2) ","(deg/s^2) ","(deg/s^2) ","(deg)     ","(deg)     ","(deg)     ","(deg/s)   ", &
                               "(deg/s)   ","(deg/s)   ","(m/s^2)   ","(m/s^2)   ","(m/s^2)   ","(m)       ","(m)       ", &
                               "(m)       ","(m)       ","(m)       ","(m)       ","(kN·m)    ","(deg)     ","(deg)     ", &
                               "(deg)     ","(deg/s)   ","(deg/s)   ","(deg/s)   "/)
   LOGICAL                  :: InvalidOutput(0:MaxOutPts)                        ! This array determines if the output channel is valid for this configuration


   InvalidOutput            = .FALSE.
     
   InvalidOutput( Fair1Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 1 ) )
   InvalidOutput( Fair1Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 1 ) )
   InvalidOutput( Anch1Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 1 ) )
   InvalidOutput( Anch1Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 1 ) )
   InvalidOutput( Fair2Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 2 ) )
   InvalidOutput( Fair2Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 2 ) )
   InvalidOutput( Anch2Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 2 ) )
   InvalidOutput( Anch2Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 2 ) )
   InvalidOutput( Fair3Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 3 ) )
   InvalidOutput( Fair3Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 3 ) )
   InvalidOutput( Anch3Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 3 ) )
   InvalidOutput( Anch3Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 3 ) )
   InvalidOutput( Fair4Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 4 ) )
   InvalidOutput( Fair4Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 4 ) )
   InvalidOutput( Anch4Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 4 ) )
   InvalidOutput( Anch4Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 4 ) )
   InvalidOutput( Fair5Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 5 ) )
   InvalidOutput( Fair5Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 5 ) )
   InvalidOutput( Anch5Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 5 ) )
   InvalidOutput( Anch5Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 5 ) )
   InvalidOutput( Fair6Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 6 ) )
   InvalidOutput( Fair6Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 6 ) )
   InvalidOutput( Anch6Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 6 ) )
   InvalidOutput( Anch6Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 6 ) )
   InvalidOutput( Fair7Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 7 ) )
   InvalidOutput( Fair7Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 7 ) )
   InvalidOutput( Anch7Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 7 ) )
   InvalidOutput( Anch7Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 7 ) )
   InvalidOutput( Fair8Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 8 ) )
   InvalidOutput( Fair8Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 8 ) )
   InvalidOutput( Anch8Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 8 ) )
   InvalidOutput( Anch8Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 8 ) )
   InvalidOutput( Fair9Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 9 ) )
   InvalidOutput( Fair9Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 9 ) )
   InvalidOutput( Anch9Ten) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 9 ) )
   InvalidOutput( Anch9Ang) = ( ( .NOT. CompHydro ) .OR. ( NumLines < 9 ) )
   InvalidOutput( WaveElev) = (   .NOT. CompHydro )
   InvalidOutput( Wave1Vxi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 1 ) )
   InvalidOutput( Wave1Vyi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 1 ) )
   InvalidOutput( Wave1Vzi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 1 ) )
   InvalidOutput( Wave1Axi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 1 ) )
   InvalidOutput( Wave1Ayi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 1 ) )
   InvalidOutput( Wave1Azi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 1 ) )
   InvalidOutput( Wave2Vxi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 2 ) )
   InvalidOutput( Wave2Vyi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 2 ) )
   InvalidOutput( Wave2Vzi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 2 ) )
   InvalidOutput( Wave2Axi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 2 ) )
   InvalidOutput( Wave2Ayi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 2 ) )
   InvalidOutput( Wave2Azi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 2 ) )
   InvalidOutput( Wave3Vxi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 3 ) )
   InvalidOutput( Wave3Vyi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 3 ) )
   InvalidOutput( Wave3Vzi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 3 ) )
   InvalidOutput( Wave3Axi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 3 ) )
   InvalidOutput( Wave3Ayi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 3 ) )
   InvalidOutput( Wave3Azi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 3 ) )
   InvalidOutput( Wave4Vxi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 4 ) )
   InvalidOutput( Wave4Vyi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 4 ) )
   InvalidOutput( Wave4Vzi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 4 ) )
   InvalidOutput( Wave4Axi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 4 ) )
   InvalidOutput( Wave4Ayi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 4 ) )
   InvalidOutput( Wave4Azi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 4 ) )
   InvalidOutput( Wave5Vxi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 5 ) )
   InvalidOutput( Wave5Vyi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 5 ) )
   InvalidOutput( Wave5Vzi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 5 ) )
   InvalidOutput( Wave5Axi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 5 ) )
   InvalidOutput( Wave5Ayi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 5 ) )
   InvalidOutput( Wave5Azi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 5 ) )
   InvalidOutput( Wave6Vxi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 6 ) )
   InvalidOutput( Wave6Vyi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 6 ) )
   InvalidOutput( Wave6Vzi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 6 ) )
   InvalidOutput( Wave6Axi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 6 ) )
   InvalidOutput( Wave6Ayi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 6 ) )
   InvalidOutput( Wave6Azi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 6 ) )
   InvalidOutput( Wave7Vxi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 7 ) )
   InvalidOutput( Wave7Vyi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 7 ) )
   InvalidOutput( Wave7Vzi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 7 ) )
   InvalidOutput( Wave7Axi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 7 ) )
   InvalidOutput( Wave7Ayi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 7 ) )
   InvalidOutput( Wave7Azi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 7 ) )
   InvalidOutput( Wave8Vxi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 8 ) )
   InvalidOutput( Wave8Vyi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 8 ) )
   InvalidOutput( Wave8Vzi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 8 ) )
   InvalidOutput( Wave8Axi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 8 ) )
   InvalidOutput( Wave8Ayi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 8 ) )
   InvalidOutput( Wave8Azi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 8 ) )
   InvalidOutput( Wave9Vxi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 9 ) )
   InvalidOutput( Wave9Vyi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 9 ) )
   InvalidOutput( Wave9Vzi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 9 ) )
   InvalidOutput( Wave9Axi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 9 ) )
   InvalidOutput( Wave9Ayi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 9 ) )
   InvalidOutput( Wave9Azi) = ( ( .NOT. CompHydro ) .OR. ( NWaveKin < 9 ) )
!End of code generated by Matlab script


!BJJ: THE FOLLOWING USED TO BE PART OF THE SCRIPT, BUT I REWROTE IT TO BE MORE EFFICIENT
   
   IF ( .NOT. CompAero ) THEN
      InvalidOutput(  WindVxi) = .TRUE.
      InvalidOutput(  WindVyi) = .TRUE.
      InvalidOutput(  WindVzi) = .TRUE.
      InvalidOutput( TotWindV) = .TRUE.
      InvalidOutput( HorWindV) = .TRUE.
      InvalidOutput(HorWndDir) = .TRUE.
      InvalidOutput(VerWndDir) = .TRUE.
      
      InvalidOutput(TipSpdRat) = .TRUE.
      InvalidOutput(NacYawErr) = .TRUE.
      
      InvalidOutput(    RotCq) = .TRUE.
      InvalidOutput(    RotCp) = .TRUE.
      InvalidOutput(    RotCt) = .TRUE.
      InvalidOutput( HSShftCq) = .TRUE.
      InvalidOutput( HSShftCp) = .TRUE.
      InvalidOutput(    GenCq) = .TRUE.
      InvalidOutput(    GenCp) = .TRUE.
      InvalidOutput(TFinAlpha) = .TRUE.
      InvalidOutput(TFinCLift) = .TRUE.
      InvalidOutput(TFinCDrag) = .TRUE.
      InvalidOutput(TFinDnPrs) = .TRUE.
      InvalidOutput( TFinCPFx) = .TRUE.
      InvalidOutput( TFinCPFy) = .TRUE.
   END IF
                            
   DO I = NumBl+1,3  ! Invalid blades
      
         ! motions
      
      InvalidOutput(   TipDxc(  I) ) = .TRUE.
      InvalidOutput(   TipDyc(  I) ) = .TRUE.
      InvalidOutput(   TipDzc(  I) ) = .TRUE.
      InvalidOutput(   TipDxb(  I) ) = .TRUE.
      InvalidOutput(   TipDyb(  I) ) = .TRUE.
      InvalidOutput(  TipALxb(  I) ) = .TRUE.
      InvalidOutput(  TipALyb(  I) ) = .TRUE.
      InvalidOutput(  TipALzb(  I) ) = .TRUE.
      InvalidOutput(  TipRDxb(  I) ) = .TRUE.
      InvalidOutput(  TipRDyb(  I) ) = .TRUE.
      InvalidOutput(  TipRDzc(  I) ) = .TRUE.
      InvalidOutput( TipClrnc(  I) ) = .TRUE.

         ! loads
         
      InvalidOutput(  RootFxc(  I) ) = .TRUE.
      InvalidOutput(  RootFyc(  I) ) = .TRUE.
      InvalidOutput(  RootFzc(  I) ) = .TRUE.
      InvalidOutput(  RootFxb(  I) ) = .TRUE.
      InvalidOutput(  RootFyb(  I) ) = .TRUE.
      InvalidOutput(  RootMxc(  I) ) = .TRUE.
      InvalidOutput(  RootMyc(  I) ) = .TRUE.
      InvalidOutput(  RootMzc(  I) ) = .TRUE.
      InvalidOutput(  RootMxb(  I) ) = .TRUE.
      InvalidOutput(  RootMyb(  I) ) = .TRUE.

         ! Blade node motions
 
      InvalidOutput(  SpnALxb(:,I) ) = .TRUE.
      InvalidOutput(  SpnALyb(:,I) ) = .TRUE.
      InvalidOutput(  SpnALzb(:,I) ) = .TRUE.

      InvalidOutput(  SpnTDxb(:,I) ) = .TRUE.
      InvalidOutput(  SpnTDyb(:,I) ) = .TRUE.
      InvalidOutput(  SpnTDzb(:,I) ) = .TRUE.

      InvalidOutput(  SpnRDxb(:,I) ) = .TRUE.
      InvalidOutput(  SpnRDyb(:,I) ) = .TRUE.
      InvalidOutput(  SpnRDzb(:,I) ) = .TRUE.

         ! Blade node loads
                  
      InvalidOutput(  SpnMLxb(:,I) ) = .TRUE.
      InvalidOutput(  SpnMLyb(:,I) ) = .TRUE.
      InvalidOutput(  SpnMLzb(:,I) ) = .TRUE.

      InvalidOutput(  SpnFLxb(:,I) ) = .TRUE.
      InvalidOutput(  SpnFLyb(:,I) ) = .TRUE.
      InvalidOutput(  SpnFLzb(:,I) ) = .TRUE.


   END DO

                                     
   DO I = 1,NumBl    
      
      DO J = NBlGages+1,9 ! Invalid blade gages

         InvalidOutput(  SpnALxb(J,I) ) = .TRUE.
         InvalidOutput(  SpnALyb(J,I) ) = .TRUE.
         InvalidOutput(  SpnALzb(J,I) ) = .TRUE.

         InvalidOutput(  SpnTDxb(J,I) ) = .TRUE.
         InvalidOutput(  SpnTDyb(J,I) ) = .TRUE.
         InvalidOutput(  SpnTDzb(J,I) ) = .TRUE.

         InvalidOutput(  SpnRDxb(J,I) ) = .TRUE.
         InvalidOutput(  SpnRDyb(J,I) ) = .TRUE.
         InvalidOutput(  SpnRDzb(J,I) ) = .TRUE.

            ! Loads
            
         InvalidOutput(  SpnMLxb(J,I) ) = .TRUE.
         InvalidOutput(  SpnMLyb(J,I) ) = .TRUE.
         InvalidOutput(  SpnMLzb(J,I) ) = .TRUE.
            
         InvalidOutput(  SpnFLxb(J,I) ) = .TRUE.
         InvalidOutput(  SpnFLyb(J,I) ) = .TRUE.
         InvalidOutput(  SpnFLzb(J,I) ) = .TRUE.


      END DO !J
      
   END DO !I
   
   DO J = NTwGages+1,9 !Invalid tower gages

         ! Motions
         
      InvalidOutput( TwHtALxt(J) ) = .TRUE.
      InvalidOutput( TwHtALyt(J) ) = .TRUE.
      InvalidOutput( TwHtALzt(J) ) = .TRUE.

      InvalidOutput( TwHtTDxt(J) ) = .TRUE.
      InvalidOutput( TwHtTDyt(J) ) = .TRUE.
      InvalidOutput( TwHtTDzt(J) ) = .TRUE.

      InvalidOutput( TwHtRDxt(J) ) = .TRUE.
      InvalidOutput( TwHtRDyt(J) ) = .TRUE.
      InvalidOutput( TwHtRDzt(J) ) = .TRUE.

      InvalidOutput( TwHtTPxi(J) ) = .TRUE.
      InvalidOutput( TwHtTPyi(J) ) = .TRUE.
      InvalidOutput( TwHtTPzi(J) ) = .TRUE.

      InvalidOutput( TwHtRPxi(J) ) = .TRUE.
      InvalidOutput( TwHtRPyi(J) ) = .TRUE.
      InvalidOutput( TwHtRPzi(J) ) = .TRUE.

         ! Loads

      InvalidOutput( TwHtMLxt(J) ) = .TRUE.
      InvalidOutput( TwHtMLyt(J) ) = .TRUE.
      InvalidOutput( TwHtMLzt(J) ) = .TRUE.

      InvalidOutput( TwHtFLxt(J) ) = .TRUE.
      InvalidOutput( TwHtFLyt(J) ) = .TRUE.
      InvalidOutput( TwHtFLzt(J) ) = .TRUE.

   END DO      

   IF ( NumBl < 3 ) THEN
      InvalidOutput(PtchPMzc3) = .TRUE.
      
      InvalidOutput(   Q_B3E1) = .TRUE.
      InvalidOutput(   Q_B3F1) = .TRUE.
      InvalidOutput(   Q_B3F2) = .TRUE.
      
      InvalidOutput(  QD_B3E1) = .TRUE.
      InvalidOutput(  QD_B3F1) = .TRUE.
      InvalidOutput(  QD_B3F2) = .TRUE.
   
      InvalidOutput( QD2_B3E1) = .TRUE.
      InvalidOutput( QD2_B3F1) = .TRUE.
      InvalidOutput( QD2_B3F2) = .TRUE.
   ELSE IF ( NumBl > 2 ) THEN
      InvalidOutput(  TeetPya) = .TRUE.
      InvalidOutput(  TeetVya) = .TRUE.
      InvalidOutput(  TeetAya) = .TRUE.

      InvalidOutput(   Q_Teet) = .TRUE.
      InvalidOutput(  QD_Teet) = .TRUE.
      InvalidOutput( QD2_Teet) = .TRUE.
   END IF
     
!bjj: should we first check that NumOuts is a valid number (or at least initialize it somewhere)?   
   
      ! ALLOCATE some arrays:
   ALLOCATE ( OutData(0:NumOuts) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the OutData array.' )
   ENDIF
            
   ALLOCATE ( OutParam(0:NumOuts) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the OutParam array.' )
   ENDIF
     
   
      ! Set index, name, and units for the time output channel:
   
   OutParam(0)%Indx  = Time      !
   OutParam(0)%Name  = 'Time'    ! OutData(0) is the time channel by default.
   OutParam(0)%Units = '(s)'     !
   OutParam(0)%SignM = 1
   
   IF ( Echo )  THEN
      WRITE (UnEc,"(/, '  Col  Parameter  Units', /, '  ---  ---------  -----')")
      WRITE (UnEc,OutPFmt)  0, OutParam(0)%Name, OutParam(0)%Units
   ENDIF
   
   
      ! Set index, name, and units for all of the output channels.
      ! If a selected output channel is not available by FAST, ProgAbort.
   
   DO I = 1,NumOuts
   
      OutParam(I)%Name  = OutList(I)   
      OutListTmp        = OutList(I)
   
   
      ! Reverse the sign (+/-) of the output channel if the user prefixed the
      !   channel name with a '-', '_', 'm', or 'M' character indicating "minus".
      ! NOTE: If, in the future, any output channel is named with a character
      !   beginning with 'm' (or 'M'), this simple code will have to be made
      !   a lot more complex! (bjj: see HydroDyn's code to figure out how to fix this!!!)
   
      ! SEE NOTE ABOVE ON THIS IF...THEN STATEMENT BELOW:
      IF ( INDEX( 'mM-_', OutListTmp(1:1) ) > 0 ) THEN
      ! SEE NOTE ABOVE ON THIS IF...THEN STATEMENT ABOVE:
         OutParam(I)%SignM = -1     ! ex, '-TipDxc1' causes the sign of TipDxc1 to be switched.
         OutListTmp        = OutListTmp(2:)
      ELSE
         OutParam(I)%SignM = 1     
      END IF
      
      CALL Conv2UC( OutListTmp )    ! Convert OutListTmp to upper case
   
   
      Indx =  IndexCharAry( OutListTmp(1:9), ValidParamAry )
      
      IF ( Indx > 0 ) THEN
         OutParam(I)%Indx     = ParamIndxAry(Indx)
         IF ( InvalidOutput( ParamIndxAry(Indx) ) ) THEN
            OutParam(I)%Units = 'INVALID'            
         ELSE
            OutParam(I)%Units = ParamUnitsAry(Indx)
         END IF
      ELSE
         CALL ProgAbort( OutParam(I)%Name//' is not an available output channel.' )
      END IF
      
      IF ( Echo )  WRITE (UnEc,OutPFmt)  I, OutParam(I)%Name, OutParam(I)%Units

   END DO
   
      ! Initialize all invalid output channels to zero so that we can avoid doing resetting them to zero at every time step.
      ! ALSO, set others to zero: (e.g. TipRDzc1, TipRDzc2, TipRDzc3 are always zero and so are not calculated in CalcOuts()

!   AllOuts(InvalidOutput) = 0.0
   AllOuts = 0.0 
   
!bjj: perhaps InvalidOutput should be in the Output Module... then we can check that each time in RtHS() instead of checking the criteria again...
! or not.... (some if statements can be combined...)
   

RETURN
END SUBROUTINE ChckOutLst
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
CALL ReadCom ( UnIn, ADAMSFile, 'feature switches'  )

   ! SaveGrphcs - Save GRAPHICS output.

CALL ReadLVar ( UnIn, ADAMSFile, SaveGrphcs, 'SaveGrphcs', 'Save GRAPHICS output' )


   ! MakeLINacf - Make ADAMS/LINEAR control command file.

CALL ReadLVar ( UnIn, ADAMSFile, MakeLINacf, 'MakeLINacf', 'Make ADAMS/LINEAR control command file' )

IF ( MakeLINacf .AND. ( .NOT. SaveGrphcs ) )  &
   CALL ProgAbort ( ' SaveGrphcs must be True if MakeLINacf is True.' )



!  -------------- DAMPING PARAMETERS -------------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, ADAMSFile, 'damping parameters'  )


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

CALL ReadCom ( UnIn, ADAMSFile, 'blade pitch actuator parameters'  )

   ! BPActrSpr - Blade pitch actuator spring constant.

CALL ReadRVar ( UnIn, ADAMSFile, BPActrSpr, 'BPActrSpr', 'Blade pitch actuator spring constant' )

IF ( BPActrSpr < 0.0 )  CALL ProgAbort ( ' BPActrSpr must not be less than zero.' )


   ! BPActrDmp - Blade pitch actuator damping constant.

CALL ReadRVar ( UnIn, ADAMSFile, BPActrDmp, 'BPActrDmp', 'Blade pitch actuator damping constant' )

IF ( BPActrDmp < 0.0 )  CALL ProgAbort ( ' BPActrSpr must not be less than zero.' )



!  -------------- GRAPHICS PARAMETERS ------------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, ADAMSFile, 'GRAPHICS parameters'  )

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


   ! NacLength - Nacelle length.

CALL ReadRVar ( UnIn, ADAMSFile, NacLength, 'NacLength', 'Nacelle length' )

IF ( ( NacLength < 0.0 ) .OR. ( NacLength > 2.0*ABS(OverHang) ) )  &
   CALL ProgAbort ( ' NacLength must be between zero and 2*ABS(OverHang) (inclusive).' )


   ! NacRadBot - Bottom radius of nacelle.

CALL ReadRVar ( UnIn, ADAMSFile, NacRadBot, 'NacRadBot', 'Bottom radius of nacelle' )

IF ( NacRadBot < 0.0 )  CALL ProgAbort ( ' NacRadBot must not be less than zero.' )


   ! NacRadTop - Top radius of nacelle.

CALL ReadRVar ( UnIn, ADAMSFile, NacRadTop, 'NacRadTop', 'Top radius of nacelle' )

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
USE                             General
USE                             Modes
USE                             Output


IMPLICIT                        NONE


   ! Passed variables:

INTEGER(4), INTENT(IN )      :: K                                               ! Blade number.


   ! Local variables:

REAL(ReKi)                   :: TipDispl                                        ! Blade tip displacement for a mode shape.

INTEGER(4)                   :: I                                               ! A generic DO index.
INTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
INTEGER(4)                   :: Sttus                                           ! Status of allocation attempts.

CHARACTER(198)               :: Frmt                                            ! Format for element data.




   ! Add a separator to the echo file if appropriate.

IF ( Echo )  WRITE (UnEc,'(//,A,/)')  'Blade '//TRIM( Int2LStr( K ) )//' input data from file "'//TRIM( BldFile(K) )//'":'


   ! Open the input file for blade K.

CALL OpenFInpFile ( UnIn, BldFile(K) )



!  -------------- HEADER -------------------------------------------------------


   ! Ship the header.

READ (UnIn,'(//)',IOSTAT=IOS)

IF ( IOS < 0 )  THEN
   CALL PremEOF ( BldFile(K) , 'unused blade '//TRIM( Int2LStr( K ) )//' file header' )
ENDIF


!  -------------- BLADE PARAMETERS ---------------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, BldFile(K), 'blade parameters'  )

   ! NBlInpSt - Number of blade input stations.

CALL ReadIVar ( UnIn, BldFile(K), NBlInpSt, 'NBlInpSt', 'Number of blade input stations' )

IF ( NBlInpSt < 1 )  CALL ProgAbort ( ' NBlInpSt must be at least 1.' )


   ! CalcBMode - Calculate blade mode shapes (switch).

!JASON: ADD LOGIC FOR THIS NEW VARIABLE:
!JASON:CALL ReadLVar ( UnIn, BldFile(K), CalcBMode, 'CalcBMode', 'Calculate blade mode shapes' )
CALL ReadCom ( UnIn, BldFile(K), 'currently ignored CalcBMode'  )


   ! BldFlDmp - Blade structural damping ratios in flapwise direction.

CALL ReadAryLines( UnIn, BldFile(K), BldFlDmp, 2, 'BldFlDmp', 'Blade structural damping ratios in flapwise direction' )

IF ( BldFlDmp(1) < 0.0 )    CALL ProgAbort ( ' BldFlDmp(1) for blade '//TRIM( Int2LStr( K ) )//' must not be negative.' )
IF ( BldFlDmp(2) < 0.0 )    CALL ProgAbort ( ' BldFlDmp(2) for blade '//TRIM( Int2LStr( K ) )//' must not be negative.' )


   ! BldEdDmp - Blade structural damping ratios in edgewise direction.

CALL ReadAryLines( UnIn, BldFile(K), BldEdDmp, 1, 'BldEdDmp', 'Blade structural damping ratios in edgewise direction' )

IF ( BldEdDmp(1) < 0.0 )    CALL ProgAbort ( ' BldEdDmp(1) for blade '//TRIM( Int2LStr( K ) )//' must not be negative.' )



!  -------------- BLADE ADJUSTMENT FACTORS -------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, BldFile(K), 'blade adjustment factors'  )

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

CALL ReadCom ( UnIn, BldFile(K), 'distributed blade parameters'  )
CALL ReadCom ( UnIn, BldFile(K), 'distributed-blade-parameter names' )
CALL ReadCom ( UnIn, BldFile(K), 'distributed-blade-parameter units'  )


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

      IF ( PrecrvRef(I) /= 0.0 )  &
         CALL ProgAbort ( ' PrecrvRef(I) must be zero for Adams models.' )

      IF ( PreswpRef(I) /= 0.0 )  &
         CALL ProgAbort ( ' PreswpRef(I) must be zero for Adams models.' )


   ENDIF


      ! Apply the correction factors to the elemental data.

   BMassDen(I)  = BMassDen(I)*AdjBlMs
   FlpStff (I)  = FlpStff (I)*AdjFlSt
   EdgStff (I)  = EdgStff (I)*AdjEdSt

ENDDO ! I



!  -------------- BLADE MODE SHAPES --------------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, BldFile(K), 'blade mode shapes'  )


   ! BldFl1Sh - Blade-flap mode-1 shape coefficients.
CALL ReadAryLines ( UnIn, BldFile(K), BldFl1Sh(:,K), PolyOrd-1, 'BldFl1Sh', 'Blade-flap mode-1 shape coefficients' )



TipDispl = 0.0

DO I=2,PolyOrd
   TipDispl = TipDispl + BldFl1Sh(I,K)
ENDDO ! I

IF ( ABS( TipDispl - 1.0 ) > 0.001 )  CALL ProgAbort ( ' Blade-flap mode-1 shape coefficients must add to 1.0.' )


   ! BldFl2Sh - Blade-flap mode-2 shape coefficients.

CALL ReadAryLines ( UnIn, BldFile(K), BldFl2Sh(:,K), PolyOrd-1, 'BldFl2Sh', 'Blade-flap mode-2 shape coefficients' )

TipDispl = 0.0

DO I=2,PolyOrd
   TipDispl = TipDispl + BldFl2Sh(I,K)
ENDDO ! I

IF ( ABS( TipDispl - 1.0 ) > 0.001 )  CALL ProgAbort ( ' Blade-flap mode-2 shape coefficients must add to 1.0.' )


   ! BldEdgSh - Blade-edge mode shape coefficients.
   
CALL ReadAryLines ( UnIn, BldFile(K), BldEdgSh(:,K), PolyOrd-1, 'BldEdgSh', 'Blade-edge mode shape coefficients' )


TipDispl = 0.0

DO I=2,PolyOrd
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
USE                             TailAero
USE                             TailFurling
USE                             TurbConf


IMPLICIT                        NONE


   ! Local variables:

INTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.



   ! Open the FAST furling input file:

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

CALL ReadCom ( UnIn, FurlFile, 'degree of freedom switches (cont)'  )


   ! RFrlDOF - Rotor-furl DOF.

CALL ReadLVar ( UnIn, FurlFile, RFrlDOF, 'RFrlDOF', 'Rotor-furl DOF' )


   ! TFrlDOF - Tail-furl DOF.

CALL ReadLVar ( UnIn, FurlFile, TFrlDOF, 'TFrlDOF', 'Tail-furl DOF' )



!  -------------- INITIAL CONDITIONS (CONT) ------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, FurlFile, 'initial conditions (cont)'  )


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

CALL ReadCom ( UnIn, FurlFile, 'turbine configuration (cont)'  )


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

CALL ReadCom ( UnIn, FurlFile, 'mass and inertia (cont)'  )

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

CALL ReadCom ( UnIn, FurlFile, 'rotor-furl' )


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

CALL ReadCom ( UnIn, FurlFile, 'tail-furl' )


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

CALL ReadCom ( UnIn, FurlFile, 'tail fin aerodynamics' )


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




   ! Open the FAST linearization input file:

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

CALL ReadCom ( UnIn, LinFile, 'Periodic steady state solution' )


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

      CALL ReadCom ( UnIn, LinFile, 'unused TrimCase' )

   ENDIF


   ! DispTol - Convergence tolerance for the 2-norm of displacements in the periodic steady state calculation.

   CALL ReadRVar ( UnIn, LinFile, DispTol, 'DispTol', 'Convergence tolerance for displacements' )

   IF ( DispTol <= 0.0 )  CALL ProgAbort ( ' DispTol must be greater than 0.' )


   ! VelTol  - Convergence tolerance for the 2-norm of velocities    in the periodic steady state calculation.

   CALL ReadRVar ( UnIn, LinFile, VelTol, 'VelTol', 'Convergence tolerance for velocities' )

   IF ( VelTol <= 0.0 )  CALL ProgAbort ( ' VelTol must be greater than 0.' )


ELSE                    ! Don't read in these variables since we wont be computing a steady state solution


   CALL ReadCom ( UnIn, LinFile, 'unused TrimCase' )
   CALL ReadCom ( UnIn, LinFile, 'unused DispTol' )
   CALL ReadCom ( UnIn, LinFile, 'unused VelTol' )


ENDIF



!  -------------- MODEL LINEARIZATION ------------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, LinFile, 'Model linearization' )


   ! NAzimStep - Number of azimuth steps in periodic linearized model.

CALL ReadIVar ( UnIn, LinFile, NAzimStep, 'NAzimStep', 'Number of azimuth steps in periodic linearized model' )

IF ( NAzimStep <= 0 )  CALL ProgAbort ( ' NAzimStep must be greater than 0.' )


   ! MdlOrder - Order of output linearized model.

CALL ReadIVar ( UnIn, LinFile, MdlOrder, 'MdlOrder', 'Order of output linearized model' )

IF ( ( MdlOrder < 1 ) .OR. ( MdlOrder > 2 ) )  CALL ProgAbort ( ' MdlOrder must be 1 or 2.' )



!  -------------- INPUTS AND DISTURBANCES --------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, LinFile, 'Inputs and disturbances' )


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
INTEGER(4)                   :: Sttus                                           ! Status returned from an allocation request.
INTEGER(IntKi)               :: OutFileFmt                                      ! The switch indicating the format (text/binary) for the tabular output file(s)

CHARACTER(1024)              :: Comment                                         ! String to temporarily hold the comment line.
CHARACTER(  35)              :: Frmt      = "( 2X, L11, 2X, A, T30, ' - ', A )" ! Output format for logical parameters. (matches NWTC Subroutine Library format)
CHARACTER(1024)              :: PriPath                                         ! The path to the primary input file



   ! Open the primary input file.

CALL OpenFInpFile ( UnIn, PriFile )

CALL GetPath( PriFile, PriPath )    ! Input files will be relative to the path where the primary input file is located.

!-------------------------- HEADER ---------------------------------------------

READ (UnIn,'(//,A,/)',IOSTAT=IOS)  FTitle
CALL CheckIOS( IOS, PriFile, 'file title', StrType )

CALL WrScr1( ' Heading of the FAST input file: '//TRIM( FTitle ) )



!-------------------------- SIMULATION CONTROL PARAMETERS ----------------------


   ! Skip the comment line.

READ (UnIn,'(A)',IOSTAT=IOS)  Comment

CALL CheckIOS( IOS, PriFile, 'simulation control parameters comment', StrType )


   ! Echo - Echo input to "echo.out".

READ (UnIn,*,IOSTAT=IOS)  WrEcho
CALL CheckIOS ( IOS, PriFile, 'Echo', FlgType )
Echo = WrEcho

IF ( Echo )  THEN
   CALL OpenEcho ( UnEc, TRIM(RootName)//'.ech' )
   WRITE (UnEc,'(/,A)'   )  'This file of echoed input was generated by '//TRIM(ProgName)//' '//TRIM(ProgVer)// &
                            ' on '//CurDate()//' at '//CurTime()//'.'
   WRITE (UnEc,'(/,A,/)' )  'Turbine input data from file "'//TRIM( PriFile )//'":'
   WRITE (UnEc,'(2X,A,/)')  FTitle
   WRITE (UnEc,'(2X,A)'  )  Comment
   WRITE (UnEc,Frmt      )  Echo, 'Echo', 'Echo input to "echo.out"'
ENDIF


   ! ADAMSPrep - ADAMS preprocossor mode.

CALL ReadIVar ( UnIn, PriFile, ADAMSPrep, 'ADAMSPrep', 'ADAMS preprocessor mode' )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( ADAMSPrep /= 1 ) )  THEN
   CALL ProgWarn ( " An ADAMS dataset can't be built when FAST is interfaced with Simulink or Labview."// & 
      " ADAMSPrep is being set to 1.")
   ADAMSPrep = 1
ELSEIF ( ( ADAMSPrep < 1 ) .OR. ( ADAMSPrep > 3 ) )  THEN
   CALL ProgAbort ( ' ADAMSPrep must be 1, 2, or 3.' )
ENDIF


   ! AnalMode - FAST analysis mode.

CALL ReadIVar ( UnIn, PriFile, AnalMode, 'AnalMode', 'Analysis mode' )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( AnalMode /= 1 ) )  THEN
   CALL ProgAbort ( ' FAST can''t linearize the model when interfaced with Simulink or Labview.'// &
                '  Set AnalMode to 1 or use the standard version of FAST.'            )
ELSEIF ( ( AnalMode < 1 ) .OR. ( AnalMode > 2 ) )  THEN
   CALL ProgAbort ( ' AnalMode must be 1 or 2.' )
ENDIF


   ! NumBl - Number of blades.

CALL ReadIVar ( UnIn, PriFile, NumBl, 'NumBl', 'Number of blades' )

IF ( ( NumBl < 2 ) .OR. ( NumBl > 3 ) )  CALL ProgAbort ( ' NumBl must be either 2 or 3.' )


   ! TMax - Total run time.

CALL ReadRVar ( UnIn, PriFile, TMax, 'TMax', 'Total run time' )

!IF ( ( TMax < 0.0 ) .OR. ( TMax > 9999.999 ) )  CALL ProgAbort ( ' TMax must be between 0.0 and 9999.999 (inclusive).' )
IF ( TMax < 0.0  )  CALL ProgAbort ( ' TMax must not be a negative number.' )

   ! DT - Integration time step.

CALL ReadRVar ( UnIn, PriFile, DT, 'DT', 'Integration time step' )

IF ( DT <= 0.0 )  CALL ProgAbort ( ' DT must be greater than 0.' )
IF ( DT <= TMax*EPSILON(DT) )  CALL ProgAbort ( ' DT must be greater than '//TRIM ( Flt2Lstr( TMax*EPSILON(DT) ) )//' seconds.' ) ! Test DT and TMax to ensure numerical stability -- HINT: see the use of OnePlusEps.


!-------------------------- TURBINE CONTROL PARAMETERS -------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'Turbine control parameters' )


   ! YCMode - Yaw control mode.

CALL ReadIVar ( UnIn, PriFile, YCMode, 'YCMode', 'Yaw control mode' )

IF ( ( .NOT. Cmpl4SFun .AND. .NOT. Cmpl4LV ) .AND. ( YCMode == 2 ) )  THEN
   CALL ProgAbort ( ' YCMode can only equal 2 when FAST is interfaced with Simulink or Labview.'// &
                '  Set YCMode to 0 or 1 or interface FAST with Simulink or Labview.'             )
ELSEIF ( ( YCMode < 0 ) .OR. ( YCMode > 2 ) )  THEN
   CALL ProgAbort ( ' YCMode must be 0, 1, or 2.' )
ENDIF


   ! TYCOn - Time to enable yaw control.

CALL ReadRVar ( UnIn, PriFile, TYCOn, 'TYCOn', 'Time to enable yaw control' )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( YCMode == 2 ) .AND. ( TYCOn /= 0.0 ) )  THEN
   CALL ProgAbort ( ' Yaw control must be enabled at time zero when implemented in Simulink or Labview.'//      &
                '  Set TYCon to 0.0, set YCMode to 0 or 1, or use the standard version of FAST.'   )
ELSEIF ( TYCOn < 0.0 )  THEN
   CALL ProgAbort ( ' TYCOn must not be negative.' )
ENDIF


   ! PCMode - Pitch control mode.

CALL ReadIVar ( UnIn, PriFile, PCMode, 'PCMode', 'Pitch control mode' )

IF ( ( .NOT. Cmpl4SFun .AND. .NOT. Cmpl4LV ) .AND. ( PCMode == 2 ) )  THEN
   CALL ProgAbort ( ' PCMode can only equal 2 when FAST is interfaced with Simulink or Labview.'// &
                '  Set PCMode to 0 or 1 or interface FAST with Simulink or Labview.'             )
ELSEIF ( ( PCMode < 0 ) .OR. ( PCMode > 2 ) )  THEN
   CALL ProgAbort ( ' PCMode must be 0, 1, or 2.' )
ENDIF


   ! TPCOn - Time to enable pitch control.

CALL ReadRVar ( UnIn, PriFile, TPCOn, 'TPCOn', 'Time to enable pitch control' )

IF ( (Cmpl4SFun .OR. Cmpl4LV)  .AND. ( PCMode == 2 ) .AND. ( TPCOn /= 0.0 ) )  THEN
   CALL ProgAbort ( ' Pitch control must be enabled at time zero when implemented in Simulink or Labview.'//    &
                '  Set TPCon to 0.0, set PCMode to 0 or 1, or use the standard version of FAST.'   )
ELSEIF ( TPCOn < 0.0 )  THEN
   CALL ProgAbort ( ' TPCOn must not be negative.' )
ENDIF


   ! VSContrl - Variable-speed-generator control switch.

CALL ReadIVar ( UnIn, PriFile, VSContrl, 'VSContrl', 'Variable-speed-generator control switch' )

IF ( ( .NOT. Cmpl4SFun .AND. .NOT. Cmpl4LV ) .AND. ( VSContrl == 3 ) )  THEN
   CALL ProgAbort ( ' VSContrl can only equal 3 when FAST is interfaced with Simulink or Labview.'// &
                '  Set VSContrl to 0, 1, or 2 or interface FAST with Simulink or Labview.'         )
ELSEIF ( ( VSContrl < 0 ) .OR. ( VSContrl > 3 ) )  THEN
   CALL ProgAbort ( ' VSContrl must be either 0, 1, 2, or 3.' )
ENDIF


   ! VS_RtGnSp - Rated generator speed for simple variable-speed generator control.

CALL ReadRVar ( UnIn, PriFile, VS_RtGnSp, 'VS_RtGnSp', 'Rated generator speed for simple variable-speed generator control' )

IF ( ( VSContrl == 1 ) .AND. ( VS_RtGnSp <= 0.0 ) )  CALL ProgAbort ( ' VS_RtGnSp must be greater than zero.' )


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

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( VSContrl == 3 ) .AND. ( .NOT. GenTiStr ) )  &
   CALL ProgAbort ( ' Variable-speed, generator torque control must be enabled at time zero when implemented in Simulink'//&
      'or Labview. Set GenTiStr to True and TimGenOn to 0.0, set VSContrl to 0, 1, or 2, or use the standard version of FAST.' )


   ! GenTiStp - Stop generator based upon T: time or F: generator power = 0.

CALL ReadLVar ( UnIn, PriFile, GenTiStp, 'GenTiStp', 'Stop generator based upon T: time or F: generator power = 0' )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( VSContrl == 3 ) .AND. ( .NOT. GenTiStp ) )  &
   CALL ProgAbort ( ' Variable-speed, generator torque control must not be disabled during simulation when'//   &
                ' implemented in Simulink or Labview.'//                          &
                '  Set GenTiStp to True and TimGenOf > TMax, set VSContrl to 0, 1, or 2, or use the standard version of FAST.'   )


   ! SpdGenOn - Generator speed to turn on the generator for a startup.

CALL ReadRVar ( UnIn, PriFile, SpdGenOn, 'SpdGenOn', 'Generator speed to turn on the generator' )

IF ( SpdGenOn < 0.0 )  CALL ProgAbort ( ' SpdGenOn must not be negative.' )


   ! TimGenOn - Time to turn on generator for startup.

CALL ReadRVar ( UnIn, PriFile, TimGenOn, 'TimGenOn', 'Time to turn on generator' )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( VSContrl == 3 ) .AND. ( TimGenOn /= 0.0 ) )  THEN
   CALL ProgAbort ( ' Variable-speed, generator torque control must be enabled at time zero when implemented in Simulink'//&
   ' or Labview. Set GenTiStr to True and TimGenOn to 0.0, set VSContrl to 0, 1, or 2, or use the standard version of FAST.' )
ELSEIF ( TimGenOn < 0.0 )  THEN
   CALL ProgAbort ( ' TimGenOn must not be negative.' )
ENDIF


   ! TimGenOf - Time to turn off generator for braking or modeling a run-away.

CALL ReadRVar ( UnIn, PriFile, TimGenOf, 'TimGenOf', 'Time to turn off generator' )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( VSContrl == 3 ) .AND. ( TimGenOf <= TMax ) )  THEN
   CALL ProgAbort ( ' Variable-speed, generator torque control must not be disabled during simulation when'//         &
                ' implemented in Simulink or Labview.'//                                                              &
                '  Set GenTiStp to True and TimGenOf > TMax, set VSContrl to 0, 1, or 2, or use the standard version of FAST.' )
ELSEIF ( TimGenOf < 0.0 )  THEN
   CALL ProgAbort ( ' TimGenOf must not be negative.' )
ENDIF


   ! HSSBrMode - HSS brake model.

CALL ReadIVar ( UnIn, PriFile, HSSBrMode, 'HSSBrMode', 'HSS brake model' )

IF ( ( HSSBrMode < 1 ) .OR. ( HSSBrMode > 3 ) )  CALL ProgAbort ( ' HSSBrMode must be 1, 2 or 3.' )

IF ( ( .NOT. Cmpl4LV) .AND. ( HSSBrMode == 3 ) )  THEN
   CALL ProgAbort ( ' HSSBrMode can be 3 only when when implemented in Labview.' )
ENDIF



   ! THSSBrDp - Time to initiate deployment of the HSS brake.

CALL ReadRVar ( UnIn, PriFile, THSSBrDp, 'THSSBrDp', 'Time to initiate deployment of the HSS brake' )
!bjj should this also check labview??? the error message says yes...
IF ( Cmpl4SFun .AND. ( THSSBrDp <= TMax ) )  THEN
   CALL ProgAbort ( ' A high-speed shaft brake shutdown event can''t be initiated when FAST is interfaced with Simulink'// &
                ' or Labview.  Set THSSBrDp > TMax or use the standard version of FAST.'        )
ELSEIF ( THSSBrDp < 0.0 )  THEN
   CALL ProgAbort ( ' THSSBrDp must not be negative.' )
ENDIF


   ! TiDynBrk - Time to initiate deployment of the dynamic generator brake.

!JASON: ADD LOGIC FOR THIS NEW VARIABLE:
!JASON:CALL ReadRVar ( UnIn, PriFile, TiDynBrk, 'TiDynBrk', 'Time to initiate deployment of the dynamic generator brake' )
!JASON:
!JASON:IF ( TiDynBrk < 0.0 )  CALL ProgAbort ( ' TiDynBrk must not be negative.' )
   CALL ReadCom ( UnIn, PriFile, 'currently ignored TiDynBrk' )


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
   CALL ReadCom ( UnIn, PriFile, 'unused TTpBrDp(3)' )
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
   CALL ReadCom ( UnIn, PriFile, 'unused TBDepISp(3)' )
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
   CALL ReadCom ( UnIn, PriFile, 'unused TPitManS(3)' )
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
   CALL ReadCom ( UnIn, PriFile, 'unused TPitManE(3)' )
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
   CALL ReadCom ( UnIn, PriFile, 'unused BlPitch(3)' )
ENDIF


   ! BlPitchF - Final pitch angle for maneuvers.

ALLOCATE ( BlPitchF(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BlPitchF array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, BlPitchF, NumBl, 'BlPitchF', 'Final pitch angle for maneuvers' )

IF ( NumBl == 2 )  THEN
   CALL ReadCom ( UnIn, PriFile, 'unused BlPitchF(3)' )
ENDIF



!  ------- ENVIRONMENTAL CONDITIONS --------------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'environmental conditions' )


   ! Gravity - Gravitational acceleration.

CALL ReadRVar ( UnIn, PriFile, Gravity, 'Gravity', 'Gravitational acceleration' )

IF ( Gravity < 0.0 )  CALL ProgAbort ( ' Gravity must not be negative.' )



!  -------------- FEATURE SWITCHES ---------------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'degree of freedom switches' )


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
   CALL ReadCom ( UnIn, PriFile, 'unused TeetDOF' )
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

   CALL ReadCom ( UnIn, PriFile, 'initial conditions' )


   ! OoPDefl - Initial out-of-plane blade-tip deflection.

CALL ReadRVar ( UnIn, PriFile, OoPDefl, 'OoPDefl', 'Initial out-of-plane blade-tip deflection' )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( OoPDefl /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial out-of-plane blade-tip displacements must be zero when FAST is interfaced with Simulink'// &
                ' or Labview. Set OoPDefl to 0.0 or use the standard version of FAST.'                )


   ! IPDefl - Initial in-plane blade-tip deflection.

CALL ReadRVar ( UnIn, PriFile, IPDefl, 'IPDefl', 'Initial in-plane blade-tip deflection' )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( IPDefl  /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial in-plane blade-tip displacements must be zero when FAST is interfaced with Simulink'// &
                ' or Labview. Set IPDefl to 0.0 or use the standard version of FAST.'                 )


   ! TeetDefl - Initial or fixed teeter angle.

IF ( NumBl == 2 )  THEN
   CALL ReadRVar ( UnIn, PriFile, TeetDefl, 'TeetDefl', 'Initial or fixed teeter angle' )
   IF ( ( TeetDefl <= -180.0 ) .OR. ( TeetDefl > 180.0 ) )  &
      CALL ProgAbort ( ' TeetDefl must be greater than -180 and less than or equal to 180.' )
ELSE
   CALL ReadCom ( UnIn, PriFile, 'unused Teeter' )
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

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( TTDspFA /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial fore-aft tower-top displacements must be zero when FAST is interfaced with Simulink'// &
                ' or Labview. Set TTDspFA to 0.0 or use the standard version of FAST.'               )


   ! TTDspSS - Initial side-to-side tower-top displacement.

CALL ReadRVar ( UnIn, PriFile, TTDspSS, 'TTDspSS', 'Initial side-to-side tower-top displacement' )

IF ( (Cmpl4SFun .OR. Cmpl4LV) .AND. ( TTDspSS /= 0.0 ) )  &
   CALL ProgAbort ( ' Initial side-to-side tower-top displacements must be zero when FAST is interfaced with Simulink'// &
                ' or Labview. Set TTDspSS to 0.0 or use the standard version of FAST.'                      )



!  -------------- TURBINE CONFIGURATION ----------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'turbine configuration' )


   ! TipRad - Preconed blade-tip radius.

CALL ReadRVar ( UnIn, PriFile, TipRad, 'TipRad', 'Preconed blade-tip radius' )

IF ( TipRad < 0.0 )  CALL ProgAbort ( ' TipRad must be greater than 0.' )


   ! HubRad - Preconed hub radius.

CALL ReadRVar ( UnIn, PriFile, HubRad, 'HubRad', 'Preconed hub radius' )

IF ( ( HubRad < 0.0 ) .OR. ( HubRad >= TipRad ) ) THEN
   CALL ProgAbort ( ' HubRad must be between 0 (inclusive) and TipRad (exclusive).' )
END IF


   ! PSpnElN - Number of the innermost blade element which is still part of the pitchable portion of the blade for partial-span pitch control.

!JASON: ADD LOGIC FOR THIS NEW VARIABLE:
!JASON:CALL ReadIVar ( UnIn, PriFile, PSpnElN, 'PSpnElN', 'Partial-span pitch control element number' )
   CALL ReadCom ( UnIn, PriFile, 'currently ignored PSpnElN' )

   ! UndSling - Undersling length.

IF ( NumBl == 2 )  THEN
   CALL ReadRVar ( UnIn, PriFile, UndSling, 'UndSling', 'Undersling length' )
ELSE
   CALL ReadCom ( UnIn, PriFile, 'unused UndSling' )
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

IF ( TwrRBHt < 0.0 )  CALL ProgAbort ( ' TwrRBHt must be greater or equal to 0 and less than TowerHt + TwrDraft.' )


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
   CALL ReadCom ( UnIn, PriFile, 'unused Delta3' )
ENDIF


   ! PreCone - Blade coning angle.

ALLOCATE ( PreCone(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the PreCone array.' )
ENDIF

CALL ReadAryLines ( UnIn, PriFile, PreCone, NumBl, 'PreCone', 'Blade coning angle' )

DO K=1,NumBl
   IF ( ( PreCone(K) <= -90.0 ) .OR. ( PreCone(K) >= 90.0 ) )  THEN
      CALL ProgAbort ( ' PreCone('//TRIM( Int2LStr( K ) )//') must be between -90 and 90 degrees (exclusive).' )
   ENDIF
ENDDO ! K

IF ( NumBl == 2 )  THEN
   CALL ReadCom ( UnIn, PriFile, 'unused Beta(3)' )
ENDIF


   ! AzimB1Up - Azimuth value to use for I/O when blade 1 points up.

CALL ReadRVar ( UnIn, PriFile, AzimB1Up, 'AzimB1Up', 'Azimuth value to use for I/O when blade 1 points up' )

IF ( ( AzimB1Up < 0.0 ) .OR. ( AzimB1Up > 360.0 ) )  CALL ProgAbort ( ' AzimB1Up must be between 0 and 360 (inclusive).' )



!  -------------- MASS AND INERTIA ---------------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'mass and inertia' )


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
   CALL ReadCom ( UnIn, PriFile, 'unused TipMass(3)' )
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

   CALL ReadCom ( UnIn, PriFile, 'drivetrain parameters' )


   ! GBoxEff - Gearbox efficiency.

CALL ReadRVar ( UnIn, PriFile, GBoxEff, 'GBoxEff', 'Gearbox efficiency' )

IF ( ( GBoxEff <= 0.0 ) .OR. ( GBoxEff > 100.0 ) ) THEN
   CALL ProgAbort ( ' GBoxEff must be greater than 0 and less than or equal to 100.' )
END IF   




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

   CALL ReadCom ( UnIn, PriFile, 'currently ignored DynBrkFi' )


   ! DTTorSpr - Drivetrain torsional spring.

CALL ReadRVar ( UnIn, PriFile, DTTorSpr, 'DTTorSpr', 'Drivetrain torsional spring' )

IF ( DTTorSpr < 0.0 )  CALL ProgAbort ( ' DTTorSpr must not be negative.' )


   ! DTTorDmp - Drivetrain torsional damper.

CALL ReadRVar ( UnIn, PriFile, DTTorDmp, 'DTTorDmp', 'Drivetrain torsional damper' )

IF ( DTTorDmp < 0.0 )  CALL ProgAbort ( ' DTTorDmp must not be negative.' )



!  -------------- SIMPLE-INDUCTION-GENERATOR PARAMETERS ------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'simple-induction-generator parameters' )


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

   CALL ReadCom ( UnIn, PriFile, 'Thevenin-equivalent induction-generator parameters' )


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


!  -------------- PLATFORM PARAMETERS ------------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, PriFile, 'platform parameters'  )

   ! PtfmModel - Platform model switch.

CALL ReadIVar ( UnIn, PriFile, PtfmModel, 'PtfmModel', 'Platform model switch' )

IF ( ( PtfmModel < 0 ) .OR. ( PtfmModel > 3 ) )  CALL ProgAbort ( ' PtfmModel must be either 0, 1, 2, or 3.' )


   ! PtfmFile - Name of file containing platform properties.

CALL ReadCVar ( UnIn, PriFile, PtfmFile, 'PtfmFile', 'Name of file containing platform properties' )

IF ( LEN_TRIM( PtfmFile ) == 0 .AND. PtfmModel /= 0 )  CALL ProgAbort ( ' PtfmFile must not be an empty string.' ) 
IF ( PathIsRelative( PtfmFile ) ) PtfmFile = TRIM(PriPath)//TRIM(PtfmFile)



!  -------------- TOWER PARAMETERS ---------------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, PriFile, 'tower parameters' )

   ! TwrNodes - Number of tower nodes used for analysis.

CALL ReadIVar ( UnIn, PriFile, TwrNodes, 'TwrNodes', 'Number of tower nodes used for analysis' )

IF ( TwrNodes < 1 )  CALL ProgAbort ( ' TwrNodes must not be less than 1.' )


   ! TwrFile - Name of file containing tower properties.

CALL ReadCVar ( UnIn, PriFile, TwrFile, 'TwrFile', 'Name of file containing tower properties' )

IF ( LEN_TRIM( TwrFile ) == 0 )  CALL ProgAbort ( ' TwrFile must not be an empty string.' )
IF ( PathIsRelative( TwrFile ) ) TwrFile = TRIM(PriPath)//TRIM(TwrFile)



!  -------------- NACELLE-YAW PARAMETERS ---------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'nacelle-yaw parameters' )


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

   CALL ReadCom ( UnIn, PriFile, 'furling parameters' )

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

IF ( LEN_TRIM( FurlFile ) == 0 .AND. Furling )  CALL ProgAbort ( ' FurlFile must not be an empty string.' )
IF ( PathIsRelative( FurlFile ) ) FurlFile = TRIM(PriPath)//TRIM(FurlFile)


!  -------------- ROTOR-TEETER PARAMETERS --------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'rotor-teeter parameters' )


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

   CALL ReadCom ( UnIn, PriFile, 'unused TeetMod'  )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetDmpP' )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetDmp'  )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetCDmp' )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetSStP' )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetHStP' )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetSSSp' )
   CALL ReadCom ( UnIn, PriFile, 'unused TeetHSSp' )

ENDIF



!  -------------- TIP-BRAKE PARAMETERS -----------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'tip-brake parameters' )


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

   CALL ReadCom ( UnIn, PriFile, 'blade parameters' )


   ! BldFile - Names of files containing blade properties.

ALLOCATE ( BldFile(NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BldFile array.' )
ENDIF

CALL ReadAryLines( UnIn, PriFile, BldFile, NumBl, 'BldFile', 'Names of files containing blade properties' )

DO K=1,NumBl
   IF ( LEN_TRIM( BldFile(K) ) == 0 )  THEN
      CALL ProgAbort ( 'BldFile('//TRIM( Int2LStr( K ) )//') must not be an empty string.' )
   ENDIF
   IF ( PathIsRelative( BldFile(K) ) ) BldFile(K) = TRIM(PriPath)//TRIM(BldFile(K))
ENDDO ! K

IF ( NumBl == 2 )  THEN
   CALL ReadCom ( UnIn, PriFile, 'unused BldFile(3)' )
ENDIF



!  -------------- AERODYN INPUT FILE PARAMETERS -------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'AeroDyn parameters' )


   ! ADFile - Name of file containing AeroDyn parameters.

CALL ReadCVar( UnIn, PriFile, ADFile, 'ADFile', 'Name of file containing AeroDyn parameters' )

IF ( LEN_TRIM( ADFile ) == 0 )  CALL ProgAbort ( 'ADFile must not be an empty string.' )
IF ( PathIsRelative( ADFile ) ) ADFile = TRIM(PriPath)//TRIM(ADFile)



!  -------------- NOISE INPUT FILE PARAMETERS --------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'Noise parameters' )


   ! NoiseFile - Name of file containing aerodynamic noise parameters.

CALL ReadCVar ( UnIn, PriFile, NoiseFile, 'NoiseFile', 'Name of file containing aerodynamic noise parameters' )

IF ( LEN_TRIM( NoiseFile ) == 0 .AND. CompNoise)  CALL ProgAbort ( ' NoiseFile must not be an empty string.' )
IF ( PathIsRelative( NoiseFile ) ) NoiseFile = TRIM(PriPath)//TRIM(NoiseFile)


!  -------------- ADAMS INPUT FILE PARAMETERS ----------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'ADAMS parameters' )

   ! ADAMSFile - Name of file containing ADAMS-specific parameters.

CALL ReadCVar ( UnIn, PriFile, ADAMSFile, 'ADAMSFile', 'Name of file containing ADAMS-specific properties' )

IF ( LEN_TRIM( ADAMSFile ) == 0 .AND. ADAMSPrep /= 1)  CALL ProgAbort ( ' ADAMSFile must not be an empty string.' )
IF ( PathIsRelative( ADAMSFile ) ) ADAMSFile = TRIM(PriPath)//TRIM(ADAMSFile)


!  -------------- FAST LINEARIZATION CONTROL PARAMETERS ------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'Linearization parameters' )


   ! LinFile - Name of file containing FAST linearization parameters.

CALL ReadCVar ( UnIn, PriFile, LinFile, 'LinFile', 'Name of file containing FAST linearization parameters' )

IF ( LEN_TRIM( LinFile ) == 0 .AND. AnalMode /= 1)  CALL ProgAbort ( ' LinFile must not be an empty string.' )
IF ( PathIsRelative( LinFile ) ) LinFile = TRIM(PriPath)//TRIM(LinFile)


!  -------------- OUTPUT PARAMETERS --------------------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'output parameters' )


   ! SumPrint - Print summary data to "*.fsm".

CALL ReadLVar ( UnIn, PriFile, SumPrint, 'SumPrint', 'Print summary data to "*.fsm"' )


   ! OutFileFmt - Format for output file(s).

CALL ReadVar ( UnIn, PriFile, OutFileFmt, 'OutFileFmt', 'Format for output file(s)' )
SELECT CASE (OutFileFmt)
   CASE (1_IntKi)
      WrBinOutFile = .FALSE.
      WrTxtOutFile = .TRUE.
   CASE (2_IntKi)
      WrBinOutFile = .TRUE.
      WrTxtOutFile = .FALSE.
   CASE (3_IntKi)
      WrBinOutFile = .TRUE.
      WrTxtOutFile = .TRUE.
   CASE DEFAULT
     CALL ProgAbort ( ' OutFileFmt must be 1, 2, or 3.' )
END SELECT

IF ( WrTxtOutFile .AND. ( TMax > 9999.999 ) )  THEN
   CALL ProgAbort ( ' TMax must not exceed 9999.999 seconds with text tabular (time-marching) output files.' )
END IF   


   ! TabDelim - Generate a tab-delimited output file.

CALL ReadLVar ( UnIn, PriFile, TabDelim, 'TabDelim', 'Use tab delimiters in text output file' )


   ! OutFmt - Output format for tabular data.

CALL ReadCVar ( UnIn, PriFile, OutFmt, 'OutFmt', 'Output format for text tabular data' )

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

IF ( ( NTwGages < 0 ) .OR. ( NTwGages > 9 ) )  CALL ProgAbort ( ' NTwGages must be between 0 and 9 (inclusive).' )


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
   WRITE (UnEc,'(9(I4,:))')  ( TwrGagNd(I), I=1,NTwGages )
ENDIF


   ! NBlGages - Number of blade "strain-gage" output stations.

CALL ReadIVar ( UnIn, PriFile, NBlGages, 'NBlGages', 'Number of blade "strain-gage" output stations' )

IF ( ( NBlGages < 0 ) .OR. ( NBlGages > 9 ) )  CALL ProgAbort ( ' NBlGages must be between 0 and 9 (inclusive).' )


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
   WRITE (UnEc,'(9(I4,:))')  ( BldGagNd(I), I=1,NBlGages )
ENDIF


   ! Skip the comment line.

   CALL ReadCom ( UnIn, PriFile, 'output-parameters list' )


   ! OutList - Output parameter list.

OutList = ''   ! Initialize OutList(:) to ''.
NumOuts = 0    ! Initialize NumOuts to zero.


   ! Lets read in all of the lines containing output parameters and store them in OutList(:).
   ! The end of this list (and the end of the output file) is specified with the line
   !    beginning with END.

CALL ReadOutputList ( UnIn, PriFile, OutList, NumOuts, 'OutList', 'Output list'  )     ! Routine in NWTC Subroutine Library


   ! Check to make sure some outputs have been entered when time-marching;
   !   if not, ProgAbort:

IF ( ( NumOuts == 0 ) .AND. ( AnalMode == 1 ) )  THEN
   CALL ProgAbort ( ' No output channels specified!' )
ENDIF



   ! Close primary input file.

CLOSE ( UnIn )


RETURN
END SUBROUTINE GetPrimary
!=======================================================================
SUBROUTINE GetPtfm

   ! This routine reads in the FAST platform input parameters from
   !   PtfmFile and validates the input.

USE                             Constants
USE                             EnvCond
USE                             Features
USE                             General
USE                             InitCond
USE                             MassInert
USE                             Output
USE                             Platform
USE                             SimCont
USE                             Tower
USE                             TurbConf
USE                             Waves, ONLY:WavePkShpDefault


IMPLICIT                        NONE


   ! Local variables:
REAL(ReKi)                   :: LAngAnch                                        ! Azimuth angle   of the current anchor   relative to the positive xi-axis of the inertial frame.
REAL(ReKi)                   :: LAngFair                                        ! Azimuth angle   of the current fairlead relative to the positive xt-axis of the platform.
REAL(ReKi)                   :: LDpthAnch                                       ! Depth           of the current anchor   relative to the origin           of the inertial frame.
REAL(ReKi)                   :: LDrftFair                                       ! Draft           of the current fairlead relative to the platform reference point.
REAL(ReKi)                   :: LRadAnch                                        ! Radial distance of the current anchor   relative to the origin           of the inertial frame.
REAL(ReKi)                   :: LRadFair                                        ! Radial distance of the current fairlead relative to the platform reference point.

INTEGER(4)                   :: I                                               ! A generic index.

INTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
INTEGER(4)                   :: Sttus                                           ! Status returned by an attempted allocation.

CHARACTER(80)                :: Line                                            ! String to temporarily hold the value of PtfmLdMod.
CHARACTER(80)                :: LineUC                                          ! String to temporarily hold the value of PtfmLdMod in upper case.
CHARACTER(156)               :: Frmt                                            ! Format for element data.
CHARACTER(1024)              :: FilePath                                        ! Path name of the PtfmFile



   ! Get the path name from the file so we can assume file names contained in this file are relative to this path
CALL GetPath( PtfmFile, FilePath )


   ! Open the FAST platform input file:

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

   CALL ReadCom ( UnIn, PtfmFile, 'degree of freedom switches (cont)' )



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

   CALL ReadCom ( UnIn, PtfmFile, 'initial conditions (cont)' )


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

   CALL ReadCom ( UnIn, PtfmFile, 'turbine configuration (cont)' )


   ! TwrDraft - Downward distance from the ground [onshore] or MSL [offshore] to the tower base platform connection.

CALL ReadRVar ( UnIn, PtfmFile, TwrDraft, 'TwrDraft', &
   'Downward distance from ground [onshore] or MSL [offshore] to tower base platform connection' )

IF ( TwrDraft <= -TowerHt )  CALL ProgAbort ( ' TwrDraft must be greater than -TowerHt.' )

IF ( TwrRBHt >= ( TowerHt + TwrDraft ) )  &
         CALL ProgAbort ( ' TwrRBHt must be greater or equal to 0 and less than TowerHt + TwrDraft.' )


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

   CALL ReadCom ( UnIn, PtfmFile, 'mass and inertia (cont)' )



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


!  -------------- PLATFORM (CONT) ----------------------------------------------


   ! Skip the comment line.

CALL ReadCom ( UnIn, PtfmFile, 'platform (cont)'  )


!JASON: MARSHALL WANTS PLATFORM/TOWER HYDRODYNAMICS TO WORK LIKE AERODYNAMICS IN AeroDyn.  THIS MEANS HAVING A COMPLETELY SEPARATE CODE FOR DOING THE HYDRODYNAMICS CALCULATIONS (CALL IT HydroDyn).  THIS MEANS THAT THE REST OF THESE PARAMETERS (EXCLUDING THE KINEMATICS/KINETICS PARAMERERS NEEDED BY FAST / ADAMS) SHOULD BE IN THEIR OWN FILE AND SOURCE CODE.  MAKE THIS CHANGE BEFORE YOU DOCUMENT THESE ROUTINES!!!!  DO THE SAME THING WITH THE MOORING SYSTEM (CALL IT LineDyn!)
!JASON: ONCE CompHydro BECOMES AN INPUT TO THE PROGRAM, USE CompHydro THROUGHOUT RtHS() LIKE CompAero!!!!
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
            IF ( PathIsRelative( GHWvFile ) ) GHWvFile = TRIM(FilePath)//TRIM(GHWvFile)

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
            WRITE (UnEc,"( 15X, A, T30, ' - ', A )")  'WaveKinNd', 'List of tower nodes that have wave kinematics sensors'
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
      IF ( PathIsRelative( WAMITFile ) ) WAMITFile = TRIM(FilePath)//TRIM(WAMITFile)

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
   ! NOTE: Use RdtnTMax = 0.0 to eliminate wave radiation damping.

      CALL ReadRVar ( UnIn, PtfmFile, RdtnTMax, 'RdtnTMax', 'Analysis time for wave radiation kernel calculations' )

      IF ( RdtnTMax < 0.0 )  CALL ProgAbort ( ' RdtnTMax must not be negative.' )


   ! RdtnDT - Time step for wave radiation kernel calculations.

      CALL ReadRVar ( UnIn, PtfmFile, RdtnDT, 'RdtnDT', 'Time step for wave radiation kernel calculations' )

      IF ( RdtnDT <= 0.0 )  CALL ProgAbort ( ' RdtnDT must be greater than zero.' )



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


               MaxLRadAnch = MAX( MaxLRadAnch, LRadAnch )   ! Find the maximum value of the input array LRadAnch

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



!  -------------- WAVES --------------------------------------------------------


   ! Skip the comment line.

      CALL ReadCom ( UnIn, PtfmFile, 'waves'  )


!JASON: MOVE THIS INPUT, WtrDens, TO AN ENVIRONMENTAL CONDITIONS SECTION OF THE INPUT FILE WHEN YOU DOCUMENT THIS NEW FEATURE!!!!
   ! WtrDens - Water density.

      CALL ReadRVar ( UnIn, PtfmFile, WtrDens, 'WtrDens', 'Water density' )

      IF ( WtrDens < 0.0 )  CALL ProgAbort ( ' WtrDens must not be negative.' )


   ! WtrDpth - Water depth.

      CALL ReadRVar ( UnIn, PtfmFile, WtrDpth, 'WtrDpth', 'Water depth' )

      IF (       WtrDpth <= PtfmDraft  )  CALL ProgAbort ( ' WtrDpth must be greater than PtfmDraft.' )
      IF ( LineMod == 1 )  THEN  ! .TRUE if we have standard quasi-static mooring lines.
         DO I = 1,NumLines ! Loop through all mooring lines
            IF ( WtrDpth < -LAnchzi(I) )  & 
                  CALL ProgAbort ( ' WtrDpth must not be less than LDpthAnch('//TRIM( Int2LStr( I ) )//').' )
         ENDDO             ! I - All mooring lines
      ENDIF


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

      IF ( Echo )  THEN
         WRITE (UnEc,"( 15X, A, T30, ' - ', A )")  'WaveKinNd', 'List of platform nodes that have wave kinematics sensors'
         WRITE (UnEc,'(9(I4,:))')  ( WaveKinNd(I), I=1,NWaveKin )
      ENDIF


   ! Check to see if all WaveKinNd(:) analysis points are existing analysis points:

      DO I=1,NWaveKin
         IF ( ( WaveKinNd(I) < 1 ) .OR. ( WaveKinNd(I) > PtfmNodes ) )  &
            CALL ProgAbort  ( ' All WaveKinNd values must be between 1 and '//TRIM( Int2LStr( PtfmNodes ) )//' (inclusive).' )
      ENDDO ! I


   ENDIF




ENDSELECT


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
USE                             Tower


IMPLICIT                        NONE


   ! Local variables.

REAL(ReKi)                   :: TopDispl                                        ! Tower-top displacement for a mode shape.


INTEGER(4)                   :: I                                               ! A generic index.
INTEGER(4)                   :: IOS                                             ! I/O status returned from the read statement.
INTEGER(4)                   :: Sttus                                           ! Status returned from an allocation request.

CHARACTER(132)               :: Frmt                                            ! Format for element data.



   ! Open the tower input file.

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

   CALL ReadCom ( UnIn, PtfmFile, 'tower parameters' )


   ! NTwInpSt - Number of tower input stations.

CALL ReadIVar ( UnIn, TwrFile, NTwInpSt, 'NTwInpSt', 'Number of tower input stations' )

IF ( NTwInpSt < 1 )  CALL ProgAbort ( ' NTwInpSt must be at least 1.' )


   ! CalcTMode - Calculate tower mode shapes (switch).

!JASON: ADD LOGIC FOR THIS NEW VARIABLE:
!JASON:CALL ReadLVar ( UnIn, TwrFile, CalcTMode, 'CalcTMode', 'Calculate tower mode shapes' )
   CALL ReadCom ( UnIn, TwrFile, 'currently ignored CalcTMode' )


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

   CALL ReadCom ( UnIn, TwrFile, 'tower adjustment factors' )



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

   CALL ReadCom ( UnIn, TwrFile, 'distributed tower parameters' )
   CALL ReadCom ( UnIn, TwrFile, 'distributed-tower-parameter names' )
   CALL ReadCom ( UnIn, TwrFile, 'distributed-tower-parameter units' )


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

   CALL CheckIOS ( IOS, TwrFile, 'line '//TRIM( Int2LStr( I ) )//' of the tower distributed properties', NumType )

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

   CALL ReadCom ( UnIn, TwrFile, 'tower fore-aft mode shapes' )


   ! TwFAM1Sh - Tower fore-aft mode-1 shape coefficients.

CALL ReadAryLines ( UnIn, TwrFile, TwFAM1Sh, PolyOrd-1, 'TwFAM1Sh', 'Tower fore-aft mode-1 shape coefficients' )


TopDispl = 0.0

DO I=2,PolyOrd
   TopDispl = TopDispl + TwFAM1Sh(I)
ENDDO ! I

IF ( ABS( TopDispl - 1.0 ) > 0.001 )  CALL ProgAbort ( ' Tower fore-aft mode-1 shape coefficients must add to 1.0.' )


   ! TwFAM2Sh - Tower fore-aft mode-2 shape coefficients.

CALL ReadAryLines ( UnIn, TwrFile, TwFAM2Sh, PolyOrd-1, 'TwFAM2Sh', 'Tower fore-aft mode-2 shape coefficients' )

TopDispl = 0.0

DO I=2,PolyOrd
   TopDispl = TopDispl + TwFAM2Sh(I)
ENDDO ! I

IF ( ABS( TopDispl - 1.0 ) > 0.001 )  CALL ProgAbort ( ' Tower fore-aft mode-2 shape coefficients must add to 1.0.' )



!  -------------- TOWER SIDE-TO-SIDE MODE SHAPES -------------------------------


   ! Skip the comment line.

   CALL ReadCom ( UnIn, TwrFile, 'tower side-to-side mode shapes' )



   ! TwSSM1Sh - Tower side-to-side mode-1 shape coefficients.


CALL ReadAryLines ( UnIn, TwrFile, TwSSM1Sh, PolyOrd-1, 'TwSSM1Sh', 'Tower side-to-side mode-1 shape coefficients' )

TopDispl = 0.0

DO I=2,PolyOrd
   TopDispl = TopDispl + TwSSM1Sh(I)
ENDDO ! I

IF ( ABS( TopDispl - 1.0 ) > 0.001 )  CALL ProgAbort ( ' Tower side-to-side mode-1 shape coefficients must add to 1.0.' )


   ! TwSSM2Sh - Tower side-to-side mode-2 shape coefficients.

CALL ReadAryLines ( UnIn, TwrFile, TwSSM2Sh, PolyOrd-1, 'TwSSM2Sh', 'Tower side-to-side mode-2 shape coefficients' )

TopDispl = 0.0

DO I=2,PolyOrd
   TopDispl = TopDispl + TwSSM2Sh(I)
ENDDO ! I

IF ( ABS( TopDispl - 1.0 ) > 0.001 )  CALL ProgAbort ( ' Tower side-to-side mode-2 shape coefficients must add to 1.0.' )


   ! Close the tower file.

CLOSE ( UnIn )


RETURN
END SUBROUTINE GetTower
!=======================================================================
SUBROUTINE FAST_Input


   ! This routine reads the input files and does some preliminary processing.


   ! FAST Modules:

USE                             Blades
USE                             Constants
USE                             DOFs
USE                             DriveTrain
USE                             EnvCond
USE                             Features
USE                             General
USE                             InitCond
USE                             Linear
USE                             Modes
USE                             NacelleYaw
USE                             Output
USE                             Platform
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
USE                             Airfoil,        ONLY: NumFoil
USE                             InducedVel,     ONLY: AToler
USE                             Switch,         ONLY: DSTALL, DYNINFL, DYNINIT

USE                             Noise  !NoiseInput()

IMPLICIT                        NONE


   ! Local variables.

REAL(ReKi)                   :: ComDenom                                        ! Common denominator used in computation of TEC model coefficients
REAL(ReKi)                   :: SumCosPreC                                      ! SUM( CosPreC(K) ) for K = 1,NumBl

INTEGER(4)                   :: I                                               ! A generic index for DO loops.
INTEGER(4)                   :: K                                               ! Index for blade number.
INTEGER(4)                   :: Sttus                                           ! Status of an attempted array allocation.



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

ALLOCATE ( BldEdgSh(2:PolyOrd,NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BldEdgSh array.' )
ENDIF

ALLOCATE ( BldFl1Sh(2:PolyOrd,NumBl) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the BldFl1Sh array.' )
ENDIF

ALLOCATE ( BldFl2Sh(2:PolyOrd,NumBl) , STAT=Sttus )
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

      DO I=2,PolyOrd
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

   CALL NoiseInput(UnIn, NoiseFile)                         ! Read in the noise parameters from NoiseFile.

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

   IF ( ( WaveMod  /= 0   ) .AND. CompHydro )  &
      CALL ProgAbort ( ' FAST can''t linearize a model with incident wave kinematics.  Set WaveMod to 0.'  )
   IF ( ( RdtnTMax /= 0.0 ) .AND. CompHydro )  &
      CALL ProgAbort ( ' FAST can''t linearize a model with wave radiation damping.  Set RdtnTMax to 0.0.' )

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

!   IF ( CompAero ) THEN ! check that the wind speed is not varying -- this is checked elsewhere so this is redundant
!      InputPosition = (/ 0.0, 0.0, FASTHH /)
!      WindStDev(:) = WindInf_GetStdDev(REAL(0.0, ReKi), TMax, DT, InputPosition,  ErrStat )
!      IF (ErrStat /= 0) CALL ProgWarn( ' FAST must have a steady reference wind file for linearization.')
!      IF ( ANY(ABS(WindStDev(:)) > TOL) ) CALL ProgAbort( ' Steady winds must be used for linearization, but the wind file has non-zero standard deviation.')
!   END IF


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

   ! Check to see if any inputted output channels are ill-conditioned (and if so, Abort)
   !    and set values for OutParam(:):

CALL ChckOutLst





!  -------------- FINISHING UP -------------------------------------------------


   ! Close echo file, if appropriate.

IF ( Echo )  CLOSE ( UnEc )



RETURN
END SUBROUTINE FAST_Input
!=======================================================================
SUBROUTINE InterpBld ( K )


   ! InterpBld performs a linear interpolation of the input blade data
   ! and writes the result in six arrays with the specified interval.


USE                             Blades
USE                             General
USE                             TurbConf

IMPLICIT                        NONE


   ! Passed variables:

INTEGER(4), INTENT(IN )      :: K                                               ! Blade number.


   ! Local variables:

INTEGER(4)                   :: Ind                                             ! Index for the node arrays.
INTEGER(4)                   :: Sttus                                           ! Status of an attempted array allocation.
INTEGER                      :: InterpInd                                       ! Index for the interpolated array



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

InterpInd = 1

   DO Ind=1,BldNodes
      AeroCent(K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, AerCen  , InterpInd, NBlInpSt )
      ThetaS  (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, StrcTwst, InterpInd, NBlInpSt )
      MassB   (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, BMassDen, InterpInd, NBlInpSt )
      StiffBF (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, FlpStff , InterpInd, NBlInpSt )
      StiffBE (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, EdgStff , InterpInd, NBlInpSt )
      IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, read in all the cols.
         StiffBGJ (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, GJStff   , InterpInd, NBlInpSt )
         StiffBEA (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, EAStff   , InterpInd, NBlInpSt )
         BAlpha   (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, Alpha    , InterpInd, NBlInpSt )
         InerBFlp (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, FlpIner  , InterpInd, NBlInpSt )
         InerBEdg (K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, EdgIner  , InterpInd, NBlInpSt )
         RefAxisxb(K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, PrecrvRef, InterpInd, NBlInpSt )
         RefAxisyb(K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, PreswpRef, InterpInd, NBlInpSt )
         IF ( Ind == BldNodes )  THEN  ! Copy data for the tip also (I know this code is inefficient, but it is only computed once, so who cares!)
            RefAxisxb(K,TipNode) = PrecrvRef(NBlInpSt)
            RefAxisyb(K,TipNode) = PreswpRef(NBlInpSt)
         ENDIF
         cgOffBFlp(K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, FlpcgOf  , InterpInd, NBlInpSt )
         cgOffBEdg(K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, EdgcgOf  , InterpInd, NBlInpSt )
         EAOffBFlp(K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, FlpEAOf  , InterpInd, NBlInpSt )
         EAOffBEdg(K,Ind) = InterpStp( RNodesNorm(Ind), BlFract, EdgEAOf  , InterpInd, NBlInpSt )
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
USE                             Tower
USE                             TurbConf


IMPLICIT                        NONE


   ! Local variables:

INTEGER(4)                   :: Ind                                             ! Index for the node arrays.
INTEGER(4)                   :: Sttus                                           ! Status of an attempted array allocation.
INTEGER                      :: InterpInd                                       ! Index for the interpolated array



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
   InterpInd = 1

   DO Ind=1,TwrNodes
      DHNodes   (Ind) = TwrFlexL/TwrNodes   !Lets used constant-spaced nodes for now, but the rest of the code is written to handle variable-spaced nodes--this will be a future input!
      IF ( Ind == 1 ) THEN !Lowest analysis point
         HNodes (Ind) = 0.5*DHNodes(Ind)
      ELSE                 !All other analysis points
         HNodes (Ind) = HNodes( Ind - 1 ) + 0.5*( DHNodes(Ind) + DHNodes( Ind - 1 ) )
      ENDIF
      HNodesNorm(Ind) = HNodes(Ind)/TwrFlexL

!bjj: it would probably be faster, since they all use the same InterpInd xVal, and xAry, to interpolate differently
      MassT     (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TMassDen, InterpInd, NTwInpSt )
      StiffTFA  (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwFAStif, InterpInd, NTwInpSt )
      StiffTSS  (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwSSStif, InterpInd, NTwInpSt )
      IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, read in all the cols.
         StiffTGJ (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwGJStif, InterpInd, NTwInpSt )
         StiffTEA (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwEAStif, InterpInd, NTwInpSt )
         InerTFA  (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwFAIner, InterpInd, NTwInpSt )
         InerTSS  (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwSSIner, InterpInd, NTwInpSt )
         cgOffTFA (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwFAcgOf, InterpInd, NTwInpSt )
         cgOffTSS (Ind) = InterpStp( HNodesNorm(Ind), HtFract, TwSScgOf, InterpInd, NTwInpSt )
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

USE                             AeroDyn
USE                             Blades
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

INTEGER                      :: ErrStat

CHARACTER(24)                :: Fmt1      = "(34X,3(6X,'Blade',I2,:))"          ! Format for outputting blade headings.
CHARACTER(15)                :: Fmt2      = "(34X,3(6X,A,:))"                   ! Format for outputting blade headings.
CHARACTER(18)                :: FmtDat    = '(A,T35,3(:,F13.3))'                ! Format for outputting mass and modal data.
CHARACTER(18)                :: FmtDatT   = '(A,T35,1(:,F13.8))'                ! Format for outputting time steps.
CHARACTER( 8)                :: FmtHead   = '(//,A,/)'                          ! Format for outputting headings.
CHARACTER( 9)                :: FmtTitl   = '(//,1X,A)'                         ! Format for outputting title.
CHARACTER( 3)                :: FmtTxt    = '(A)'                               ! Format for outputting pure text.
CHARACTER(99)                :: RotorType                                       ! Text description of rotor.



   ! Open the summary file and give it a heading.

CALL OpenFOutFile ( UnSu, TRIM( RootName )//'.fsm' )

WRITE (UnSu,'(/,A)')  'This summary information was generated by '//TRIM(ProgName)//' '//TRIM( ProgVer )// &
                      ' on '//CurDate()//' at '//CurTime()//'.'
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

IF ( CompHydro )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Computation of hydrodynamic loads.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Computation of hydrodynamic loads.'
ENDIF


IF ( CompNoise )  THEN
   WRITE (UnSu,FmtTxt)  ' Enabled    Computation of aeroacoustics.'
ELSE
   WRITE (UnSu,FmtTxt)  ' Disabled   Computation of aeroacoustics.'
ENDIF


   ! Time steps.

WRITE (UnSu,FmtHead)  'Time steps:'

WRITE (UnSu,FmtDatT) '    Structural            (s)     ', DT
!bjj: does this really belong in this file????
WRITE (UnSu,FmtDatT) '    Aerodynamic           (s)     ', DT*CEILING( AD_GetConstant('dtAero',ErrStat) / DT) ! AeroDyn will be called at integer multiples of DT that are greater than or equal to DTAero, since FAST's integration scheme marches with a constant time step of DT.

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
!WRITE (UnSu,FmtDat ) '    Turbine Mass          (kg)    ', TurbMass
WRITE (UnSu,FmtDat ) '    Platform Mass         (kg)    ', PtfmMass
WRITE (UnSu,FmtDat ) '    Mass Incl. Platform   (kg)    ', TotalMass


   ! Interpolated tower properties.

WRITE (UnSu,"(//,'Interpolated tower properties:',/)")
IF ( ( ADAMSPrep == 2 ) .OR. ( ADAMSPrep == 3 ) )  THEN  ! An ADAMS model will be created; thus, print out all the cols.

   WRITE (UnSu,FmtTxt)  'Node  TwFract   HNodes  DHNodes  TMassDen    FAStiff    SSStiff'// &
                        '    GJStiff    EAStiff    FAIner    SSIner  FAcgOff  SScgOff'
   WRITE (UnSu,FmtTxt)  ' (-)      (-)      (m)      (m)    (kg/m)     (Nm^2)     (Nm^2)'// &
                        '     (Nm^2)        (N)    (kg m)    (kg m)      (m)      (m)'

   DO I=1,TwrNodes
      WRITE(UnSu,'(I4,3F9.3,F10.3,4ES11.3,2F10.3,2F9.3)')  I, HNodesNorm(I), HNodes(I)+TwrRBHt, DHNodes(I), MassT(I), &
                                                              StiffTFA(I), StiffTSS(I), StiffTGJ(I), StiffTEA(I),           &
                                                              InerTFA(I), InerTSS(I), cgOffTFA(I), cgOffTSS(I)
   ENDDO ! I

ELSE                                                     ! Only FAST will be run; thus, only print out the necessary cols.

   WRITE (UnSu,FmtTxt)  'Node  TwFract   HNodes  DHNodes  TMassDen    FAStiff    SSStiff'
   WRITE (UnSu,FmtTxt)  ' (-)      (-)      (m)      (m)    (kg/m)     (Nm^2)     (Nm^2)'

   DO I=1,TwrNodes
      WRITE(UnSu,'(I4,3F9.3,F10.3,2ES11.3)')  I, HNodesNorm(I), HNodes(I) + TwrRBHt, DHNodes(I), MassT(I), &
                                                 StiffTFA(I), StiffTSS(I)
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

         WRITE(UnSu,'(I4,3F9.3,3F10.3,4ES11.3,F9.3,4F10.3,4F9.3)')  I, RNodesNorm(I), RNodes(I) + HubRad, DRNodes(I),            &
                                                                       AeroCent(K,I), ThetaS(K,I)*R2D, MassB(K,I),               &
                                                                       StiffBF(K,I), StiffBE(K,I), StiffBGJ(K,I), StiffBEA(K,I), &
                                                                       BAlpha(K,I), InerBFlp(K,I), InerBEdg(K,I),                &
                                                                       RefAxisxb(K,I), RefAxisyb(K,I),                           &
                                                                       cgOffBFlp(K,I), cgOffBEdg(K,I),                           &
                                                                       EAOffBFlp(K,I), EAOffBEdg(K,I)
      ENDDO ! I

   ELSE                                                     ! Only FAST will be run; thus, only print out the necessary cols.

      WRITE (UnSu,FmtTxt)  'Node  BlFract   RNodes  DRNodes  AeroCent  StrcTwst  BMassDen    FlpStff    EdgStff'
      WRITE (UnSu,FmtTxt)  ' (-)      (-)      (m)      (m)       (-)     (deg)    (kg/m)     (Nm^2)     (Nm^2)'

      DO I=1,BldNodes
         WRITE(UnSu,'(I4,3F9.3,3F10.3,2ES11.3)')  I, RNodesNorm(I), RNodes(I) + HubRad, DRNodes(I), AeroCent(K,I), &
                                                     ThetaS(K,I)*R2D, MassB(K,I), StiffBF(K,I), StiffBE(K,I)
      ENDDO ! I

   ENDIF

ENDDO ! K


RETURN
END SUBROUTINE PrintSum
!=======================================================================
SUBROUTINE RunTimes()


   ! This routine displays a message that gives that status of the simulation
   !  and the predicted end time of day.


USE                             General
USE                             SimCont

IMPLICIT                        NONE


   ! Local variables.

REAL(ReKi)                   :: ClckTime                                        ! Elapsed clock time for the simulation phase of the run.
REAL(ReKi)                   :: Factor                                          ! Ratio of seconds to a specified time period.
REAL(ReKi)                   :: TRatio                                          ! Ration of simulation time to elapsed clock time.

REAL(4)                      :: UsrTime                                         ! User CPU time for entire run.

INTEGER(4)                   :: EndTimes (8)                                    ! An array holding the ending clock time of the simulation.

CHARACTER( 8)                :: TimePer


   ! Get the end times to compare with start times.

CALL DATE_AND_TIME ( VALUES=EndTimes )
CALL CPU_TIME ( UsrTime )


   ! Calculate the elapsed wall-clock time in seconds.
   
!bjj: I think this calculation will be wrong at certain times (e.g. if it's near midnight on the last day of the month), but to my knowledge, no one has complained...

ClckTime =  0.001*( EndTimes(8) - StrtTime(8) ) + ( EndTimes(7) - StrtTime(7) ) + 60.0*( EndTimes(6) - StrtTime(6) ) &
         + 3600.0*( EndTimes(5) - StrtTime(5) ) + 86400.0*( EndTimes(3) - StrtTime(3) )  
         


   ! Calculate CPU times.

UsrTime  = UsrTime - UsrTime1


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

   CALL WrOver( '                                                                                   ' )
   CALL WrScr ( ' Total Real Time:       '//TRIM( Num2LStr( Factor*ClckTime      ) )//TRIM( TimePer ) )
   CALL WrScr ( ' Total CPU Time:        '//TRIM( Flt2LStr( Factor*UsrTime       ) )//TRIM( TimePer ) )
   CALL WrScr ( ' Simulated Time:        '//TRIM( Flt2LStr( Factor*REAL( ZTime ) ) )//TRIM( TimePer ) )
   CALL WrScr ( ' Time Ratio (Sim/CPU):  '//TRIM( Flt2LStr( TRatio ) ) )


ENDIF


RETURN
END SUBROUTINE RunTimes
!=======================================================================
SUBROUTINE SimStatus


   ! This routine displays a message that gives that status of the simulation
   !  and the predicted end time of day.


USE                             SimCont

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

LOGICAL,    SAVE             :: FirstPas = .TRUE.                               ! When true, indicates we're on the first pass of sim.

CHARACTER( 8)                :: ETimeStr                                        ! String containing the end time.



   ! On the first pass, get the start time.

IF ( FirstPas )  THEN

   CALL DATE_AND_TIME ( Values=TimeAry )

   PrevTime = 60.0*( 60.0*TimeAry(5) + TimeAry(6) ) + TimeAry(7) + 0.001*TimeAry(8)

   FirstPas = .FALSE.

   RETURN

ENDIF


   ! How many seconds past midnight?

CALL DATE_AND_TIME ( Values=TimeAry )

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
SUBROUTINE WrBinOutput(UnIn,FileID, DescStr,ChanName,ChanUnit,TimeData,AllOutData,ErrStat,ErrMsg)
! This subroutine takes the previously-opened binary file specified by UnIn, and writes a the AllOutData Matrix to a 16-bit packed
! binary file. A text DescStr is written to the file as well as the text in the ChanName and ChanUnit arrays. The file is closed 
! at the end of this subroutine call.
! Note that the file is opened at the start of the simulation to ensure that it's available before starting the simulation.
!..................................................................................................................................

USE                          Output, ONLY: FileFmtID_WithTime, FileFmtID_WithoutTime

IMPLICIT                     NONE

INTEGER(IntKi), PARAMETER     :: LenName     = 10                 ! Number of characters allowed in a channel name
INTEGER(IntKi), PARAMETER     :: LenUnit     = 10                 ! Number of characters allowed in a channel unit


INTEGER,           INTENT(IN) :: UnIn                             ! Unit number of the file being written
INTEGER(B2Ki),     INTENT(IN) :: FileID                           ! File ID, used to determine format of output file
CHARACTER(*),      INTENT(IN) :: DescStr                          ! Description to write to the binary file (e.g., program version, date, & time)
CHARACTER(LenName),INTENT(IN) :: ChanName(:)                      ! The output channel names (including Time)
CHARACTER(LenUnit),INTENT(IN) :: ChanUnit(:)                      ! The output channel units (including Time)
REAL(DbKi),        INTENT(IN) :: TimeData(:)                      ! The time being output to the file (element 1 is the first output time, element 2 is the delta t)
REAL(ReKi),        INTENT(IN) :: AllOutData(:,:)                  ! All of the data being written to the file (except time; note that the channels are the rows and time is the column--this is done for speed of saving the array)
INTEGER(IntKi),    INTENT(OUT):: ErrStat                          ! Indicates whether an error occurred (see NWTC_Library)
CHARACTER(*),      INTENT(OUT):: ErrMsg                           ! Error message associated with the ErrStat


      ! Parameters required for scaling Real data to 16-bit integers 
      
REAL(R8Ki), PARAMETER         :: Int32Max =  65535.0              ! Largest integer represented in 4 bytes
REAL(R8Ki), PARAMETER         :: Int32Min = -65536.0              ! Smallest integer represented in 4 bytes
REAL(R8Ki), PARAMETER         :: Int32Rng = Int32Max - Int32Min   ! Max Range of 4-byte integer

REAL(SiKi), PARAMETER         :: IntMax   =  32767.0              ! Largest integer represented in 2 bytes
REAL(SiKi), PARAMETER         :: IntMin   = -32768.0              ! Smallest integer represented in 2 bytes
REAL(SiKi), PARAMETER         :: IntRng   = IntMax - IntMin       ! Max Range of 2 byte integer


      ! Local variables

REAL(DbKi)                    :: TimeMax                          ! Maximum value of the time data
REAL(DbKi)                    :: TimeMin                          ! Minimum value of the time data   
REAL(R8Ki)                    :: TimeOff                          ! Offset for the time data
REAL(R8Ki)                    :: TimeScl                          ! Slope for the time data   
REAL(R8Ki)                    :: TimeOut1                         ! The first output time
REAL(R8Ki)                    :: TimeIncrement                    ! The delta t

REAL(ReKi), ALLOCATABLE       :: ColMax(:)                        ! Maximum value of the column data
REAL(ReKi), ALLOCATABLE       :: ColMin(:)                        ! Minimum value of the column data   
REAL(SiKi), ALLOCATABLE       :: ColOff(:)                        ! Offset for the column data
REAL(SiKi), ALLOCATABLE       :: ColScl(:)                        ! Slope for the column data


INTEGER(IntKi)                :: I                                ! Generic loop counter
INTEGER(IntKi)                :: IC                               ! Loop counter for the output channel
INTEGER(IntKi)                :: IT                               ! Loop counter for the timestep
INTEGER(IntKi)                :: J                                ! Generic counter
INTEGER(IntKi)                :: LenDesc                          ! Length of the description string, DescStr
INTEGER(IntKi)                :: NT                               ! Number of time steps
INTEGER(IntKi)                :: NumOutChans                      ! Number of output channels


INTEGER(B2Ki), ALLOCATABLE    :: TmpOutArray(:)                   ! This array holds the normalized output channels before being written to the binary file
INTEGER(B4Ki), ALLOCATABLE    :: TmpTimeArray(:)                  ! This array holds the normalized output time channel before being written to the binary file
INTEGER(B1Ki), ALLOCATABLE    :: DescStrASCII(:)                  ! The ASCII equivalent of DescStr 
INTEGER(B1Ki), ALLOCATABLE    :: ChanNameASCII(:)                 ! The ASCII equivalent of ChanName 
INTEGER(B1Ki), ALLOCATABLE    :: ChanUnitASCII(:)                 ! The ASCII equivalent of ChanUnit 


   !...............................................................................................................................
   ! Initialize some values
   !...............................................................................................................................
      
   ErrStat     = ErrID_None             ! No error has yet occurred
   ErrMsg      = ''                     ! No error has yet occurred
   NumOutChans = SIZE(AllOutData,1)     ! The number of output channels
   NT          = SIZE(AllOutData,2)     ! The number of time steps to be written
   LenDesc     = LEN_TRIM( DescStr )    ! Length of the string that contains program name, version, date, and time


   !...............................................................................................................................
   ! Allocate arrays
   !...............................................................................................................................

   ALLOCATE ( ColMax( NumOutChans ) , STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error allocating memory for ColMax array.')
      RETURN
   ENDIF
   
   
   ALLOCATE ( ColMin( NumOutChans ) , STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error allocating memory for ColMin array.')
      RETURN
   ENDIF
   
   
   ALLOCATE ( ColOff( NumOutChans ) , STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error allocating memory for ColOff array.')
      RETURN
   ENDIF


   ALLOCATE ( ColScl( NumOutChans ) , STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error allocating memory for ColScl array.')
      RETURN
   ENDIF
   

   ALLOCATE ( TmpOutArray( NumOutChans*NT ) , STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error allocating memory for the temporary output array.')
      RETURN
   ENDIF
   
   IF ( FileID == FileFmtID_WithTime ) THEN
      ALLOCATE ( TmpTimeArray( NT ) , STAT=ErrStat )
      IF ( ErrStat /= 0 )  THEN
         CALL ExitThisRoutine(ErrID_Fatal,'Error allocating memory for the temporary output time array.')
         RETURN
      ENDIF
   END IF 
   
   ALLOCATE ( ChanNameASCII( (1+NumOutChans)*LenName ) , STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error allocating memory for the temporary FAST channel names.')
      RETURN
   ENDIF


   ALLOCATE ( ChanUnitASCII( (1+NumOutChans)*LenUnit ) , STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error allocating memory for the temporary FAST channel unit names.')
      RETURN
   ENDIF


   ALLOCATE ( DescStrASCII( LenDesc ) , STAT=ErrStat )
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error allocating memory for the temporary FAST file description.')
      RETURN
   ENDIF      
   
   !...............................................................................................................................
   ! Convert character strings to ASCII
   !...............................................................................................................................
      
      ! Description string (DescStr)
      
   DO I=1,LenDesc
      DescStrASCII(I) = IACHAR( DescStr(I:I) )
   END DO
   
      ! Channel names (ChanName)      
   J = 1
   DO IC = 1,SIZE(ChanName)
      DO I=1,LenName
         ChanNameASCII(J) = IACHAR( ChanName(IC)(I:I) )
         J = J + 1
      END DO
   END DO

      ! Channel units (ChanUnit)
   J = 1
   DO IC = 1,SIZE(ChanUnit)
      DO I=1,LenUnit
         ChanUnitASCII(J) = IACHAR( ChanUnit(IC)(I:I) )
         J = J + 1
      END DO
   END DO
   
   !...............................................................................................................................
   ! Find the range of our output channels
   !...............................................................................................................................     
   ColMin(:) = AllOutData(:,1_IntKi)         ! Initialize the Min values for each channel
   ColMax(:) = AllOutData(:,1_IntKi)         ! Initialize the Max values for each channel

   DO IT=2,NT                                ! Loop through the remaining time steps 
      
      DO IC=1,NumOutChans                    ! Loop through the output channels
      
         IF ( AllOutData(IC,IT) > ColMax(IC) ) THEN
            ColMax(IC) = AllOutData(IC,IT)
         ELSEIF ( AllOutData(IC,IT) < ColMin(IC) ) THEN
            ColMin(IC) = AllOutData(IC,IT)
         ENDIF

      ENDDO !IC

   ENDDO !IT


   IF ( FileID == FileFmtID_WithTime ) THEN
      TimeMin   = TimeData(1)                   ! Initialize the Min time value
      TimeMax   = MAX(TimeData(1),TimeData(NT)) ! Initialize the Max time value
   
      DO IT=2,NT                                ! Loop through the remaining time steps    
         IF ( TimeData(IT) > TimeMax ) THEN
            TimeMax = TimeData(IT)
         ELSEIF ( TimeData(IT) < TimeMin ) THEN
            TimeMin = TimeData(IT)
         ENDIF
      ENDDO !IT
      
   ELSE ! FileFmtID_WithoutTime
         ! Convert DbKi to R8Ki, if necessary
      TimeOut1      = TimeData(1)                ! The first output time
      TimeIncrement = TimeData(2)                ! The time increment
   END IF ! FileID
   
   !...............................................................................................................................
   ! Calculate the scaling parameters for each channel
   !...............................................................................................................................   
   DO IC=1,NumOutChans                    ! Loop through the output channels
   
      IF ( ColMax(IC) == ColMin(IC) ) THEN
         ColScl(IC) = 1
      ELSE
         ColScl(IC) = IntRng/REAL( ColMax(IC) - ColMin(IC), SiKi )
      ENDIF
      
      ColOff(IC) = IntMin - ColScl(IC)*REAL( ColMin(IC), SiKi )
      
   ENDDO !IC


   IF ( FileID == FileFmtID_WithTime ) THEN
      IF ( TimeMax == TimeMin ) THEN
         TimeScl = 1
      ELSE
         TimeScl = Int32Rng/REAL( TimeMax - TimeMin, R8Ki )
      ENDIF
      
      TimeOff = Int32Min - TimeScl*REAL( TimeMin, R8Ki )

   END IF ! FileID
   
   !...............................................................................................................................
   ! Convert channels to 16-bit integers (packed binary)
   !...............................................................................................................................     
   J = 1
   DO IT=1,NT                                ! Loop through the time steps 
      DO IC=1,NumOutChans                    ! Loop through the output channels
      
         TmpOutArray(J) =  NINT( Max( Min( REAL( ColScl(IC)*AllOutData(IC,IT) + ColOff(IC), SiKi), IntMax ), IntMin) , B2Ki )
         J = J + 1

      ENDDO !IC
            
   ENDDO !IT


   IF ( FileID == FileFmtID_WithTime ) THEN  ! Pack the time into 32-bit integers
      DO IT=1,NT                             ! Loop through the time steps 
         TmpTimeArray(IT) = NINT( Max( Min( REAL( TimeScl*TimeData(IT) + TimeOff, R8Ki), Int32Max ), Int32Min) , B4Ki )      
      ENDDO !IT
   END IF ! FileID

   !...............................................................................................................................
   ! Write the output file header   
   !...............................................................................................................................

   WRITE (UnIn, IOSTAT=ErrStat)   INT( FileID             , B2Ki )            ! FAST output file format
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error writing FileID to the FAST binary file.')
      RETURN
   ENDIF

   WRITE (UnIn, IOSTAT=ErrStat)   INT( NumOutChans        , B4Ki )            ! The number of output channels
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error writing NumOutChans to the FAST binary file.')
      RETURN
   ENDIF

   WRITE (UnIn, IOSTAT=ErrStat)   INT( NT                 , B4Ki )            ! The number of time steps
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error writing NT to the FAST binary file.')
      RETURN
   ENDIF


   IF ( FileID == FileFmtID_WithTime ) THEN  
         ! Write the slope and offset for the time channel

      WRITE (UnIn, IOSTAT=ErrStat)  TimeScl                                  ! The time slope for scaling
      IF ( ErrStat /= 0 )  THEN
         CALL ExitThisRoutine(ErrID_Fatal,'Error writing TimeScl to the FAST binary file.')
         RETURN
      ENDIF

      WRITE (UnIn, IOSTAT=ErrStat)  TimeOff                                  ! The time offset for scaling
      IF ( ErrStat /= 0 )  THEN
         CALL ExitThisRoutine(ErrID_Fatal,'Error writing TimeOff to the FAST binary file.')
         RETURN
      ENDIF

   ELSE ! FileFmtID_WithoutTime
         ! Write the first output time and the time step
         
      WRITE (UnIn, IOSTAT=ErrStat)  TimeOut1                                  ! The first output time 
      IF ( ErrStat /= 0 )  THEN
         CALL ExitThisRoutine(ErrID_Fatal,'Error writing TimeOut1 to the FAST binary file.')
         RETURN
      ENDIF
      
      WRITE (UnIn, IOSTAT=ErrStat)  TimeIncrement                             ! The time increment (between subsequent outputs)
      IF ( ErrStat /= 0 )  THEN
         CALL ExitThisRoutine(ErrID_Fatal,'Error writing TimeIncrement to the FAST binary file.')
         RETURN
      ENDIF            
      
   END IF

   WRITE (UnIn, IOSTAT=ErrStat)  ColScl(:)                                    ! The channel slopes for scaling
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error writing ColScl to the FAST binary file.')
      RETURN
   ENDIF

   WRITE (UnIn, IOSTAT=ErrStat)  ColOff(:)                                    ! The channel offsets for scaling
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error writing ColOff to the FAST binary file.')
      RETURN
   ENDIF

   WRITE (UnIn, IOSTAT=ErrStat)   INT( LenDesc            , B4Ki )            ! The number of characters in the string
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error writing LenDesc to the FAST binary file.')
      RETURN
   ENDIF

   WRITE (UnIn, IOSTAT=ErrStat)  DescStrASCII                                 ! DescStr converted to ASCII
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error writing file description to the FAST binary file.')
      RETURN
   ENDIF


   WRITE (UnIn, IOSTAT=ErrStat)  ChanNameASCII                                 ! ChanName converted to ASCII
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error writing channel names to the FAST binary file.')
      RETURN
   ENDIF


   WRITE (UnIn, IOSTAT=ErrStat)  ChanUnitASCII                                 ! ChanUnit converted to ASCII
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error writing channel units to the FAST binary file.')
      RETURN
   ENDIF

   !...............................................................................................................................
   ! Write the channel data   
   !...............................................................................................................................
   IF ( FileID == FileFmtID_WithTime ) THEN  
      WRITE (UnIn, IOSTAT=ErrStat)  TmpTimeArray                               ! TimeData converted to packed binary (32-bit)
      IF ( ErrStat /= 0 )  THEN
         CALL ExitThisRoutine(ErrID_Fatal,'Error writing time data to the FAST binary file.')
         RETURN
      ENDIF
   END IF ! FileID


   WRITE (UnIn, IOSTAT=ErrStat)  TmpOutArray                                  ! AllOutData converted to packed binary (16-bit)
   IF ( ErrStat /= 0 )  THEN
      CALL ExitThisRoutine(ErrID_Fatal,'Error writing channel data to the FAST binary file.')
      RETURN
   ENDIF

   !...............................................................................................................................
   ! We're finished: clean up ALLOCATABLE arrays and close the file
   !...............................................................................................................................

   CALL ExitThisRoutine(ErrID_None,'')
   RETURN

!..................................................................................................................................
CONTAINS 
!..................................................................................................................................
   SUBROUTINE ExitThisRoutine(ErrID,Msg)
   ! This subroutine cleans up all the allocatable arrays, sets the error status/message and closes the binary file
   !...............................................................................................................................
   
         ! Passed arguments
      INTEGER(IntKi), INTENT(IN) :: ErrID       ! The error identifier (ErrStat)
      CHARACTER(*),   INTENT(IN) :: Msg         ! The error message (ErrMsg)
      
      
      !............................................................................................................................
      ! Set error status/message
      !............................................................................................................................
   
      ErrStat = ErrID
      ErrMsg  = Msg
      
      !............................................................................................................................
      ! Deallocate arrays
      !............................................................................................................................
      IF ( ALLOCATED( ColMax        ) ) DEALLOCATE( ColMax )
      IF ( ALLOCATED( ColMin        ) ) DEALLOCATE( ColMin )
      IF ( ALLOCATED( ColOff        ) ) DEALLOCATE( ColOff )
      IF ( ALLOCATED( ColScl        ) ) DEALLOCATE( ColScl )
      IF ( ALLOCATED( TmpTimeArray  ) ) DEALLOCATE( TmpTimeArray )
      IF ( ALLOCATED( TmpOutArray   ) ) DEALLOCATE( TmpOutArray )
      IF ( ALLOCATED( DescStrASCII  ) ) DEALLOCATE( DescStrASCII )
      IF ( ALLOCATED( ChanNameASCII ) ) DEALLOCATE( ChanNameASCII )
      IF ( ALLOCATED( ChanUnitASCII ) ) DEALLOCATE( ChanUnitASCII )
            
      !............................................................................................................................
      ! Close file
      !............................................................................................................................
      CLOSE ( UnIn )
         
   END SUBROUTINE ExitThisRoutine
   !...............................................................................................................................
END SUBROUTINE WrBinOutput
!==================================================================================================================================
! SL
!SUBROUTINE WrOutHdr
SUBROUTINE WrOutHdr(turbnum)



   ! This routine generates the header for the primary output file.


USE                             Features
USE                             General
USE                             AeroDyn
USE                             Output
USE                             SimCont, ONLY: TMax,DT !BJJ perhaps we should do this a better way

USE                             Noise     !WrNoiseOutHdr

IMPLICIT                        NONE


   ! Local variables.
INTEGER                      :: ErrStat

INTEGER(4)                   :: I                                               ! A generic index for DO loops.
CHARACTER(1)                 :: Delim                                           ! The delimiter character


! SL -- add turbine number to the *.out file
INTEGER                      :: turbnum
CHARACTER(3)                 :: str


! SL: write turbin number in to "str"
write(str, '(i3)') turbnum


   ! set the delimiter
   
IF ( TabDelim ) THEN
   Delim = TAB
ELSE
   Delim = ' '
END IF      

FileDesc = 'These predictions were generated by '//TRIM(ProgName)//' '//TRIM(ProgVer)//' on '//CurDate()//' at '//CurTime()//'.'


   ! Open the output file(s):

IF (WrTxtOutFile) THEN      
   ! SL
   !CALL OpenFOutFile ( UnOu, TRIM(RootName)//'.out' )
   CALL OpenFOutFile ( UnOu, TRIM(RootName)//TRIM(adjustl(str))//'.out' )

                 
      ! Add some file information:

   WRITE (UnOu,'(/,A)') TRIM(FileDesc)  
   WRITE (UnOu,'(  A)')  'The aerodynamic calculations were made by '//TRIM(GetNVD(AD_Prog))//'.'
   WRITE (UnOu,'(/,1X,A,/)')  TRIM( FTitle )


      ! Write the names of the output parameters:
      
         ! names
   CALL WrFileNR ( UnOu, TRIM( OutParam(0)%Name ) )
   DO I=1,NumOuts
      CALL WrFileNR ( UnOu, Delim//TRIM( OutParam(I)%Name ) )
   ENDDO ! I

         ! units
   WRITE (UnOu,'()')
   CALL WrFileNR ( UnOu, TRIM( OutParam(0)%Units ) )
   DO I=1,NumOuts
      CALL WrFileNR ( UnOu, Delim//TRIM( OutParam(I)%Units ) )
   ENDDO ! I
      
   WRITE (UnOu,'()')

END IF   

IF (WrBinOutFile) THEN      
   CALL OpenBin ( UnOuBin,   TRIM(RootName)//'.outb', 2, ErrStat )
   
   NOutSteps = NINT ( (TMax - TStart) / (DT*DecFact) ) + 1
   IF (.NOT. ALLOCATED(AllOutData) ) THEN
      ALLOCATE ( AllOutData(1:NumOuts,NOutSteps) , STAT=ErrStat )
      IF ( ErrStat /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the AllOutData array.' )
      END IF
   END IF
   
   IF ( ALLOCATED(TimeData) ) DEALLOCATE(TimeData)
   
   IF ( OutputFileFmtID == FileFmtID_WithoutTime ) THEN
   
      ALLOCATE ( TimeData(2) , STAT=ErrStat )
      IF ( ErrStat /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the TimeData array.' )
      END IF

      TimeData(1) = 0                  ! This is the first output time, which we will set later
      TimeData(2) = DT*DecFact         ! This
   
   ELSE
   
      ALLOCATE ( TimeData( SIZE(AllOutData) ) , STAT=ErrStat )
      IF ( ErrStat /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the TimeData array.' )
      END IF
   
   END IF
   
   CurrOutStep = 0
   FileDesc = TRIM(FileDesc)//' The aerodynamic calculations were made by '//TRIM(GetNVD(AD_Prog))//'.'
   
END IF




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

USE                             AeroGenSubs, ONLY: ElemOut
USE                             NOISE  !WriteSPLOut()

IMPLICIT                        NONE


   ! Local variables.

INTEGER(4)                   :: I                                               ! A generic index for DO loops.

CHARACTER(37)                :: Frmt                                            ! A string to hold a format specifier.
CHARACTER(1)                 :: Delim                                           ! The delimiter character



IF (WrTxtOutFile) THEN      

      ! Write normal tabular output:

   Frmt = '(F8.3,'//TRIM(Int2LStr(NumOuts))//'(:,A,'//TRIM( OutFmt )//'))'

   IF ( TabDelim ) THEN
      Delim = TAB
   ELSE
      Delim = ' '
   END IF      
   WRITE(UnOu,Frmt)  OutData(Time), ( Delim, OutData(I), I=1,NumOuts )
   
END IF

IF (WrBinOutFile) THEN               
   
      ! Write data to array for binary output file
      
   IF ( CurrOutStep == NOutSteps ) THEN
      CALL ProgWarn( 'Not all data could be written to the binary output file.' )
   ELSE      
      CurrOutStep = CurrOutStep + 1
      AllOutData(:,CurrOutStep) = OutData(1:NumOuts)
      
      IF ( CurrOutStep == 1_IntKi .OR. OutputFileFmtID == FileFmtID_WithTime ) THEN
         TimeData(CurrOutStep) = OutData(Time)   ! Time associated with these outputs (bjj: fix this when we convert time to double precision)         
      END IF
               
   END IF
   
END IF


   ! Generate AeroDyn's element data if desired:

CALL ElemOut()



   ! Output noise if desired:

IF ( CompNoise )  CALL WriteSPLOut



RETURN
END SUBROUTINE WrOutput
!=======================================================================
END MODULE FAST_IO_Subs
