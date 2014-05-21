!BJJ Start of proposed change vXX NWTC_Lib
MODULE FASTSubs

   USE   NWTC_Library

CONTAINS
!bjj end of proposed change

!JASON: PUT ALL SUBROUTINES IN A MODULE--ONE MODULE PER SOURCE FILE.
!=======================================================================
SUBROUTINE Alloc


   ! This routine allocates many of the variable-length arrays.


USE                             Blades
USE                             CoordSys
USE                             DOFs
USE                             DriveTrain
USE                             InitCond
USE                             MassInert
USE                             Modes
USE                             Output
USE                             RtHndSid
USE                             SimCont
USE                             Tower
USE                             TurbConf
USE                             TurbCont

!bjj start of proposed change vXX
USE                             NOISE !AllocNoise
!bjj end of proposed change

IMPLICIT                        NONE


   ! Local variables.

INTEGER(4)                   :: Sttus                                           ! Status returned by an attempted allocation.



   ! Allocate some arrays:

!BJJ CHECK FOR ALLOCATED HERE;  DO WE NEED TO RE-INITIALIZE IF THEY ARE ALLOCATED?
IF (.NOT. ALLOCATED( BlPitchFrct ) ) THEN
   ALLOCATE ( BlPitchFrct(NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the BlPitchFrct array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( BlPitchI ) ) THEN
   ALLOCATE ( BlPitchI(NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the BlPitchI array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( BlPitchCom ) ) THEN
   ALLOCATE ( BlPitchCom(NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the BlPitchCom array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( AxRedTFA ) ) THEN
   ALLOCATE ( AxRedTFA(2,2,TTopNode) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the AxRedTFA array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( AxRedTSS ) ) THEN
   ALLOCATE ( AxRedTSS(2,2,TTopNode) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the AxRedTSS array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( TwrFASF ) ) THEN
   ALLOCATE ( TwrFASF(2,TTopNode,0:2) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the TwrFASF array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( TwrSSSF ) ) THEN
   ALLOCATE ( TwrSSSF(2,TTopNode,0:2) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the TwrSSSF array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( AxRedBld ) ) THEN
   ALLOCATE ( AxRedBld(NumBl,3,3,TipNode) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the AxRedBld array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( TwistedSF ) ) THEN
   ALLOCATE ( TwistedSF(NumBl,2,3,TipNode,0:2) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the TwistedSF array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( BldCG ) ) THEN
   ALLOCATE ( BldCG(NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the BldCG array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( KBF ) ) THEN
   ALLOCATE ( KBF(NumBl,2,2) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the KBF array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( KBE ) ) THEN
   ALLOCATE ( KBE(NumBl,1,1) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the KBE array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( CBF ) ) THEN
   ALLOCATE ( CBF(NumBl,2,2) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the CBF array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( CBE ) ) THEN
   ALLOCATE ( CBE(NumBl,1,1) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the CBE array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( SecondMom ) ) THEN
   ALLOCATE ( SecondMom(NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the SecondMom array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FirstMom ) ) THEN
   ALLOCATE ( FirstMom(NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the FirstMom array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FreqBE ) ) THEN
   ALLOCATE ( FreqBE(NumBl,1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the FreqBE array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FreqBF ) ) THEN
   ALLOCATE ( FreqBF(NumBl,2,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the FreqBF array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( DOF_BE ) ) THEN
   ALLOCATE ( DOF_BE(NumBl,1) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the DOF_BE array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( DOF_BF ) ) THEN
   ALLOCATE ( DOF_BF(NumBl,2) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the DOF_BF array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( BldMass ) ) THEN
   ALLOCATE ( BldMass(NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the BldMass array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( rSAerCenn1 ) ) THEN
   ALLOCATE ( rSAerCenn1(NumBl,BldNodes) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the rSAerCenn1 array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( rSAerCenn2 ) ) THEN
   ALLOCATE ( rSAerCenn2(NumBl,BldNodes) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the rSAerCenn2 array.')
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( DOF_Flag ) ) THEN
   ALLOCATE ( DOF_Flag(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the DOF_Flag array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( DOF_FlagInit ) ) THEN
   ALLOCATE ( DOF_FlagInit(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the DOF_FlagInit array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( DOF_Desc ) ) THEN
   ALLOCATE ( DOF_Desc(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the DOF_Desc array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( NPSBE ) ) THEN
   ALLOCATE ( NPSBE(NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the NPSBE array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( NPSE ) ) THEN
   ALLOCATE ( NPSE(NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the NPSE array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PCE ) ) THEN
   ALLOCATE ( PCE(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PCE array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PDE ) ) THEN
   ALLOCATE ( PDE(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PDE array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PIE ) ) THEN
   ALLOCATE ( PIE(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PIE array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PTTE ) ) THEN
   ALLOCATE ( PTTE(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PTTE array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PTE ) ) THEN
   ALLOCATE ( PTE(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PTE array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PS ) ) THEN
   ALLOCATE ( PS(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PS array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PSBE ) ) THEN
   ALLOCATE ( PSBE(NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PSBE array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PSE ) ) THEN
   ALLOCATE ( PSE(NumBl,NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PSE array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PUE ) ) THEN
   ALLOCATE ( PUE(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PUE array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PYE ) ) THEN
   ALLOCATE ( PYE(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PYE array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( Diag ) ) THEN
   ALLOCATE ( Diag(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the Diag array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( SrtPS ) ) THEN
   ALLOCATE ( SrtPS(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the SrtPS array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( SrtPSNAUG ) ) THEN
   ALLOCATE ( SrtPSNAUG(NAUG) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the SrtPSNAUG array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( QD2 ) ) THEN
   ALLOCATE ( QD2(NDOF,NMX) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the QD2 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( IC ) ) THEN
   ALLOCATE ( IC(NMX) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the IC array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( Q ) ) THEN
   ALLOCATE ( Q(NDOF,NMX) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the Q array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( QD ) ) THEN
   ALLOCATE ( QD(NDOF,NMX) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the QD array.' )
   ENDIF
ENDIF


   ! Allocate RtHS arrays:

IF (.NOT. ALLOCATED( AugMat ) ) THEN
   ALLOCATE ( AugMat(NDOF,NAUG) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the AugMat array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( LinAccES ) ) THEN
   ALLOCATE ( LinAccES(NumBl,TipNode,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the LinAccES array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( LinAccESt ) ) THEN
   ALLOCATE ( LinAccESt(NumBl,TipNode,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the LinAccESt array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( LinAccET ) ) THEN
   ALLOCATE ( LinAccET(TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the LinAccET array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( LinAccETt ) ) THEN
   ALLOCATE ( LinAccETt(TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the LinAccETt array.' )
   ENDIF
ENDIF

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:
ALLOCATE ( AngAccEFt(TwrNodes,3) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the AngAccEFt array.' )
ENDIF

!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

!bjj start of proposed change - aerodyn loops
IF (.NOT. ALLOCATED( LinVelESm2 ) ) THEN
   ALLOCATE ( LinVelESm2(NumBl) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the LinVelESm2 array.' )
   ENDIF
ENDIF
!bjj end of proposed change

IF (.NOT. ALLOCATED( FrcS0B ) ) THEN
   ALLOCATE ( FrcS0B(NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the FrcS0B array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFrcS0B ) ) THEN
   ALLOCATE ( PFrcS0B(NumBl,NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFrcS0B array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FrcS0Bt ) ) THEN
   ALLOCATE ( FrcS0Bt(NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the FrcS0Bt array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( MomH0B ) ) THEN
   ALLOCATE ( MomH0B(NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the MomH0B array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMomH0B ) ) THEN
   ALLOCATE ( PMomH0B(NumBl,NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMomH0B array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( MomH0Bt ) ) THEN
   ALLOCATE ( MomH0Bt(NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the MomH0Bt array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFrcPRot ) ) THEN
   ALLOCATE ( PFrcPRot(NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFrcPRot array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMomLPRot ) ) THEN
   ALLOCATE ( PMomLPRot(NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMomLPRot array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFrcVGnRt ) ) THEN
   ALLOCATE ( PFrcVGnRt(NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFrcVGnRt array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMomNGnRt ) ) THEN
   ALLOCATE ( PMomNGnRt(NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMomNGnRt array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFrcWTail ) ) THEN
   ALLOCATE ( PFrcWTail(NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFrcWTail array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMomNTail ) ) THEN
   ALLOCATE ( PMomNTail(NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMomNTail array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFrcONcRt ) ) THEN
   ALLOCATE ( PFrcONcRt(NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFrcONcRt array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMomBNcRt ) ) THEN
   ALLOCATE ( PMomBNcRt(NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMomBNcRt array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFrcT0Trb ) ) THEN
   ALLOCATE ( PFrcT0Trb(NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFrcT0Trb array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMomX0Trb ) ) THEN
   ALLOCATE ( PMomX0Trb(NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMomX0Trb array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFrcZAll ) ) THEN
   ALLOCATE ( PFrcZAll(NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFrcZAll array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMomXAll ) ) THEN
   ALLOCATE ( PMomXAll(NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMomXAll array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FSAero ) ) THEN
   ALLOCATE ( FSAero(NumBl,BldNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the FSAero array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FSTipDrag ) ) THEN
   ALLOCATE ( FSTipDrag(NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the FSTipDrag array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( MMAero ) ) THEN
   ALLOCATE ( MMAero(NumBl,BldNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the MMAero array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( rS ) ) THEN
   ALLOCATE ( rS(NumBl,TipNode,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rS array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( rQS ) ) THEN
   ALLOCATE ( rQS(NumBl,TipNode,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rQS array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( rS0S ) ) THEN
   ALLOCATE ( rS0S(NumBl,TipNode,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rS0S array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( AngPosHM ) ) THEN
   ALLOCATE ( AngPosHM(NumBl,TipNode,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the AngPosHM array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FTAero ) ) THEN
   ALLOCATE ( FTAero(TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the FTAero array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( MFAero ) ) THEN
   ALLOCATE ( MFAero(TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the MFAero array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FTHydro ) ) THEN
   ALLOCATE ( FTHydro(TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the FTHydro array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( MFHydro ) ) THEN
   ALLOCATE ( MFHydro(TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the MFHydro array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PFTHydro ) ) THEN
   ALLOCATE ( PFTHydro(TwrNodes,NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PFTHydro array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PMFHydro ) ) THEN
   ALLOCATE ( PMFHydro(TwrNodes,NDOF,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PMFHydro array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( FTHydrot ) ) THEN
   ALLOCATE ( FTHydrot(TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the FTHydrot array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( MFHydrot ) ) THEN
   ALLOCATE ( MFHydrot(TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the MFHydrot array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( rZT ) ) THEN
   ALLOCATE ( rZT(TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rZT array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( rT0T ) ) THEN
   ALLOCATE ( rT0T(TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the rT0T array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEA ) ) THEN
   ALLOCATE ( PAngVelEA(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEA array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEB ) ) THEN
   ALLOCATE ( PAngVelEB(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEB array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEF ) ) THEN
   ALLOCATE ( PAngVelEF(TwrNodes,NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEF array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEG ) ) THEN
   ALLOCATE ( PAngVelEG(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEG array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEH ) ) THEN
   ALLOCATE ( PAngVelEH(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEH array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEL ) ) THEN
   ALLOCATE ( PAngVelEL(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEL array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEM ) ) THEN
   ALLOCATE ( PAngVelEM(NumBl,TipNode,NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEM array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEN ) ) THEN
   ALLOCATE ( PAngVelEN(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEN array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelER ) ) THEN
   ALLOCATE ( PAngVelER(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelER array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PAngVelEX ) ) THEN
   ALLOCATE ( PAngVelEX(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PAngVelEX array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEC ) ) THEN
   ALLOCATE ( PLinVelEC(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEC array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelED ) ) THEN
   ALLOCATE ( PLinVelED(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelED array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEI ) ) THEN
   ALLOCATE ( PLinVelEI(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEI array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEIMU ) ) THEN
   ALLOCATE ( PLinVelEIMU(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEIMU array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEJ ) ) THEN
   ALLOCATE ( PLinVelEJ(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEJ array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEK ) ) THEN
   ALLOCATE ( PLinVelEK(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEK array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEO ) ) THEN
   ALLOCATE ( PLinVelEO(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEO array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEP ) ) THEN
   ALLOCATE ( PLinVelEP(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEP array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEQ ) ) THEN
   ALLOCATE ( PLinVelEQ(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEQ array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelES ) ) THEN
   ALLOCATE ( PLinVelES(NumBl,TipNode,NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelES array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelET ) ) THEN
   ALLOCATE ( PLinVelET(TwrNodes,NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelET array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEU ) ) THEN
   ALLOCATE ( PLinVelEU(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEU array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEV ) ) THEN
   ALLOCATE ( PLinVelEV(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEV array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEW ) ) THEN
   ALLOCATE ( PLinVelEW(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEW array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEY ) ) THEN
   ALLOCATE ( PLinVelEY(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEY array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( PLinVelEZ ) ) THEN
   ALLOCATE ( PLinVelEZ(NDOF,0:1,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the PLinVelEZ array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( SolnVec ) ) THEN
   ALLOCATE ( SolnVec(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the SolnVec array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( QD2T ) ) THEN
   ALLOCATE ( QD2T(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the QD2T array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( QDT ) ) THEN
   ALLOCATE ( QDT(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the QDT array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( QT ) ) THEN
   ALLOCATE ( QT(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the QT array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( QD2TC ) ) THEN
   ALLOCATE ( QD2TC(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the QD2TC array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( OgnlGeAzRo ) ) THEN
   ALLOCATE ( OgnlGeAzRo(NAUG) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the OgnlGeAzRo array.' )
   ENDIF
ENDIF


   ! Allocate coordinate system arrays:

IF (.NOT. ALLOCATED( t1 ) ) THEN
   ALLOCATE ( t1(TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the t1 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( t2 ) ) THEN
   ALLOCATE ( t2(TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the t2 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( t3 ) ) THEN
   ALLOCATE ( t3(TwrNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the t3 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( i1 ) ) THEN
   ALLOCATE ( i1(NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the i1 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( i2 ) ) THEN
   ALLOCATE ( i2(NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the i2 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( i3 ) ) THEN
   ALLOCATE ( i3(NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the i3 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( j1 ) ) THEN
   ALLOCATE ( j1(NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the j1 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( j2 ) ) THEN
   ALLOCATE ( j2(NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the j2 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( j3 ) ) THEN
   ALLOCATE ( j3(NumBl,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the j3 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( m1 ) ) THEN
   ALLOCATE ( m1(NumBl,BldNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the m1 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( m2 ) ) THEN
   ALLOCATE ( m2(NumBl,BldNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the m2 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( m3 ) ) THEN
   ALLOCATE ( m3(NumBl,BldNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the m3 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( n1 ) ) THEN
   ALLOCATE ( n1(NumBl,BldNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the n1 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( n2 ) ) THEN
   ALLOCATE ( n2(NumBl,BldNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the n2 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( n3 ) ) THEN
   ALLOCATE ( n3(NumBl,BldNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the n3 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( te1 ) ) THEN
   ALLOCATE ( te1(NumBl,BldNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the te1 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( te2 ) ) THEN
   ALLOCATE ( te2(NumBl,BldNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the te2 array.' )
   ENDIF
ENDIF

IF (.NOT. ALLOCATED( te3 ) ) THEN
   ALLOCATE ( te3(NumBl,BldNodes,3) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the te3 array.' )
   ENDIF
ENDIF

   ! Allocate noise arrays:

CALL AllocNoise



RETURN
END SUBROUTINE Alloc
!=======================================================================
SUBROUTINE CalcOuts


   ! This SUBROUTINE is used to compute the selected output channels
   !   (motions and loads) and place them in the OutData() array.
   ! NOTE: the descriptions of the output channels are not given here.
   !   Please see the FAST User's Guide for a complete description of
   !   each output parameter.
   ! NOTE: no matter how many output channels are selected, all of the
   !   outputs are calcalated since it would be more time consuming to
   !   check to see if an output need be calculated than to actually
   !   calculate it.  This is also important since some users may want to
   !   access any of of the output channels in their user-defined routines
   !   without actually outputting those values to the output file.  All
   !   of the calculated output channels are placed into the AllOuts(:)
   !   array.


USE                             Blades
!bjj rm NWTC_Library: USE                             Constants
USE                             CoordSys
USE                             DOFs
USE                             DriveTrain
USE                             EnvCond
USE                             Features
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
USE                             FloatingPlatform, ONLY:AnchorTension, FairleadTension
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
USE                             Linear
USE                             MassInert
USE                             Output
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
USE                             Platform
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
!bjj rm NWTC_Library: USE                             Precision
USE                             RtHndSid
USE                             SimCont
USE                             TailAero
USE                             Tower
USE                             TurbConf
USE                             TurbCont
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for outputting the incident wave elevation at
!jmj   the platform reference point and the incident wave kinematics at up to 9
!jmj   nodes along the undeflected tower [not floating] or undisplaced platform
!jmj   [floating]:
USE                             Waves, ONLY:WaveElevation, WaveVelocity, WaveAcceleration
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

!BJJ Start of proposed change AD_v12.70
USE                             AeroDyn
!USE                             AeroSubs, ONLY: GetHubWind
!BJJ End of proposed change


IMPLICIT                        NONE


   ! Local variables:

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
REAL(ReKi)                   :: AnchTe                                          ! Instantaneous effective tension in a mooring line at the anchor   (N  )
REAL(ReKi)                   :: AnchTeAng                                       ! Instantaneous vertical angle    of a mooring line at the anchor   (rad)
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
REAL(ReKi)                   :: AngAccEB  (3)                                   ! Angular acceleration of the base plate                                                (body B) in the inertia frame (body E for earth).
REAL(ReKi)                   :: AngAccER  (3)                                   ! Angular acceleration of the structure that furls with the rotor (not including rotor) (body R) in the inertia frame (body E for earth).
REAL(ReKi)                   :: AngAccEX  (3)                                   ! Angular acceleration of the platform                                                  (body X) in the inertia frame (body E for earth).
REAL(ReKi)                   :: ComDenom                                        ! Common denominator used in several expressions.
REAL(ReKi)                   :: CThrstys                                        ! Estimate of the ys-location of the center of thrust.
REAL(ReKi)                   :: CThrstzs                                        ! Estimate of the zs-location of the center of thrust.
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
REAL(ReKi)                   :: FairTe                                          ! Instantaneous effective tension in a mooring line at the fairlead (N  )
REAL(ReKi)                   :: FairTeAng                                       ! Instantaneous vertical angle    of a mooring line at the fairlead (rad)
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.
REAL(ReKi)                   :: FrcONcRt  (3)                                   ! Total force at the yaw bearing (point O  ) due to the nacelle, generator, and rotor.
REAL(ReKi)                   :: FrcPRot   (3)                                   ! Total force at the teeter pin  (point P  ) due to the rotor.
REAL(ReKi)                   :: FrcT0Trb  (3)                                   ! Total force at the base of flexible portion of the tower (point T(0)) due to the entire wind turbine.
REAL(ReKi)                   :: FZHydro   (3)                                   ! Total platform hydrodynamic force at the platform reference (point Z).
!bjj rm unused:REAL(ReKi)                   :: HalfLngth                                       ! 1/2 the length of the element with the strain gage.
REAL(ReKi)                   :: HHWndVec  (3)                                   ! Hub-height wind vector in the AeroDyn coordinate system.
REAL(ReKi)                   :: LinAccEIMU(3)                                   ! Total linear acceleration of the nacelle IMU (point IMU) in the inertia frame (body E for earth).
REAL(ReKi)                   :: LinAccEO  (3)                                   ! Total linear acceleration of the base plate (point O) in the inertia frame (body E for earth).
REAL(ReKi)                   :: LinAccEZ  (3)                                   ! Total linear acceleration of the platform refernce (point Z) in the inertia frame (body E for earth).
REAL(ReKi)                   :: MomBNcRt  (3)                                   ! Total moment at the base plate      (body B) / yaw bearing                           (point O) due to the nacelle, generator, and rotor.
REAL(ReKi)                   :: MomFGagT  (3)                                   ! Total moment at the tower element   (body F) / tower strain gage location            (point T) due to the nacelle and rotor and tower above the strain gage.
REAL(ReKi)                   :: MomLPRot  (3)                                   ! Total moment at the low-speed shaft (body L) / teeter pin                            (point P) due to the rotor.
REAL(ReKi)                   :: MomMGagB  (3)                                   ! Total moment at the blade element   (body M) / blade strain gage location            (point S) due to the blade above the strain gage.
REAL(ReKi)                   :: MomNGnRt  (3)                                   ! Total moment at the nacelle         (body N) / specified point on rotor-furl axis    (point V) due to the structure that furls with the rotor, generator, and rotor.
REAL(ReKi)                   :: MomNTail  (3)                                   ! Total moment at the nacelle         (body N) / specified point on  tail-furl axis    (point W) due to the tail.
REAL(ReKi)                   :: MomX0Trb  (3)                                   ! Total moment at the tower base      (body X) / base of flexible portion of the tower (point T(0)) due to the entire wind turbine.
REAL(ReKi)                   :: MXHydro   (3)                                   ! Total platform hydrodynamic moment acting at the platform (body X) / platform reference (point Z).
REAL(ReKi)                   :: rOPO      (3)                                   ! Position vector from the undeflected tower top (point O prime) to the deflected tower top (point O).
REAL(ReKi)                   :: rOSTip    (3)                                   ! Position vector from the deflected tower top (point O) to the deflected blade tip (point S tip).
REAL(ReKi)                   :: rOSTipxn                                        ! Component of rOSTip directed along the xn-axis.
REAL(ReKi)                   :: rOSTipyn                                        ! Component of rOSTip directed along the yn-axis.
REAL(ReKi)                   :: rOSTipzn                                        ! Component of rOSTip directed along the zn-axis.
REAL(ReKi)                   :: rSTipPSTip(3)                                   ! Position vector from the undeflected blade tip (point S tip prime) to the deflected blade tip (point S tip).
REAL(ReKi)                   :: TmpVec    (3)                                   ! A temporary vector used in various computations.

INTEGER(4)                   :: I                                               ! Generic index
INTEGER(4)                   :: J                                               ! Loops through nodes / elements.
INTEGER(4)                   :: K                                               ! Loops through blades.

!bjj start of proposed change v13.00b
INTEGER                      :: ErrStat
!bjj end of proposed change

   ! Global functions:

!bjj rm, replace with DOT_PRODUCT(): REAL(ReKi), EXTERNAL         :: DotProd                                         ! A function returning the dot product of two vectors.



   ! Initialize AllOuts() to zero.  Doing this will ensure that all
   !   "INVALID CHANNEL"s (channels that can't be calculated for a
   !   given turbine configuration) are output as zero.

AllOuts = 0.0


   ! Calculate all of the total forces and moments using all of the
   !   partial forces and moments calculated in RtHS().  Also,
   !   calculate all of the total angular and linear accelerations
   !   using all of the partial accelerations calculated in RtHS().
   !   To do this, first initialize the variables using the portions
   !   not associated with the accelerations.  Then add the portions
   !   associated with the accelerations one by one:

AngAccEB   = AngAccEBt
AngAccER   = AngAccERt
AngAccEX   = AngAccEXt
LinAccEIMU = LinAccEIMUt
LinAccEO   = LinAccEOt
LinAccEZ   = LinAccEZt
FrcONcRt   = FrcONcRtt
FrcPRot    = FrcPRott
FrcT0Trb   = FrcT0Trbt
FZHydro    = FZHydrot
MomBNcRt   = MomBNcRtt
MomLPRot   = MomLPRott
MomNGnRt   = MomNGnRtt
MomNTail   = MomNTailt
MomX0Trb   = MomX0Trbt
MXHydro    = MXHydrot

DO I = 1,NActvDOF ! Loop through all active (enabled) DOFs
   AngAccEB   = AngAccEB   + PAngVelEB  (SrtPS(I),0,:)*QD2T(SrtPS(I))
   AngAccER   = AngAccER   + PAngVelER  (SrtPS(I),0,:)*QD2T(SrtPS(I))
   LinAccEIMU = LinAccEIMU + PLinVelEIMU(SrtPS(I),0,:)*QD2T(SrtPS(I))
   LinAccEO   = LinAccEO   + PLinVelEO  (SrtPS(I),0,:)*QD2T(SrtPS(I))
   FrcONcRt   = FrcONcRt   + PFrcONcRt  (SrtPS(I),  :)*QD2T(SrtPS(I))
   FrcPRot    = FrcPRot    + PFrcPRot   (SrtPS(I),  :)*QD2T(SrtPS(I))
   FrcT0Trb   = FrcT0Trb   + PFrcT0Trb  (SrtPS(I),  :)*QD2T(SrtPS(I))
   MomBNcRt   = MomBNcRt   + PMomBNcRt  (SrtPS(I),  :)*QD2T(SrtPS(I))
   MomLPRot   = MomLPRot   + PMomLPRot  (SrtPS(I),  :)*QD2T(SrtPS(I))
   MomNGnRt   = MomNGnRt   + PMomNGnRt  (SrtPS(I),  :)*QD2T(SrtPS(I))
   MomNTail   = MomNTail   + PMomNTail  (SrtPS(I),  :)*QD2T(SrtPS(I))
   MomX0Trb   = MomX0Trb   + PMomX0Trb  (SrtPS(I),  :)*QD2T(SrtPS(I))
ENDDO             ! I - All active (enabled) DOFs
DO I = 1,NPYE     ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)
   AngAccEX   = AngAccEX   + PAngVelEX  (PYE  (I),0,:)*QD2T(PYE  (I))
   LinAccEZ   = LinAccEZ   + PLinVelEZ  (PYE  (I),0,:)*QD2T(PYE  (I))
   FZHydro    = FZHydro    + PFZHydro   (PYE  (I),  :)*QD2T(PYE  (I))
   MXHydro    = MXHydro    + PMXHydro   (PYE  (I),  :)*QD2T(PYE  (I))
ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)



DO K = 1,NumBl ! Loop through all blades

   LinAccES(K,TipNode,:) = LinAccESt(K,TipNode,:)
   FrcS0B  (K,        :) = FrcS0Bt  (K,        :)
   MomH0B  (K,        :) = MomH0Bt  (K,        :)

   DO I = 1,NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K
      LinAccES(K,TipNode,:) = LinAccES(K,TipNode,:) + PLinVelES(K,TipNode,PSE(K,I),0,:)*QD2T(PSE(K,I))
      FrcS0B  (K,        :) = FrcS0B  (K,        :) + PFrcS0B  (K,        PSE(K,I),  :)*QD2T(PSE(K,I))
      MomH0B  (K,        :) = MomH0B  (K,        :) + PMomH0B  (K,        PSE(K,I),  :)*QD2T(PSE(K,I))
   ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Add blade strain gage output parameters for the local loads and motions of
!jmj   blades 2 and 3:
!remove6.02b
!remove6.02b   IF ( K == 1 )  THEN  ! If blade 1
!remove6.02b
!remove6.02b      DO J = 1,BldNodes ! Loop through the blade nodes / elements
!remove6.02b
!remove6.02b         LinAccES(K,J,:) = LinAccESt(K,J,:)
!remove6.02b
!remove6.02b         DO I = 1,NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K
!remove6.02b            LinAccES(K,J,:) = LinAccES(K,J,:) + PLinVelES(K,J,PSE(K,I),0,:)*QD2T(PSE(K,I))
!remove6.02b         ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K
!remove6.02b
!remove6.02b      ENDDO             ! J - Blade nodes / elements
!remove6.02b
!remove6.02b   ENDIF                ! Blade 1
   DO J = 1,BldNodes ! Loop through the blade nodes / elements

      LinAccES(K,J,:) = LinAccESt(K,J,:)

      DO I = 1,NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K
         LinAccES(K,J,:) = LinAccES(K,J,:) + PLinVelES(K,J,PSE(K,I),0,:)*QD2T(PSE(K,I))
      ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

   ENDDO             ! J - Blade nodes / elements
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

ENDDO          ! K - All blades

DO J = 1,TwrNodes  ! Loop through the tower nodes / elements

   LinAccET(J,:) = LinAccETt(J,:)
   FTHydro (J,:) = FTHydrot (J,:)
   MFHydro (J,:) = MFHydrot (J,:)

   DO I = 1,NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)
      LinAccET(J,:) = LinAccET(J,:) + PLinVelET(J,PTE(I),0,:)*QD2T(PTE(I))
      FTHydro (J,:) = FTHydro (J,:) + PFTHydro (J,PTE(I),  :)*QD2T(PTE(I))
      MFHydro (J,:) = MFHydro (J,:) + PMFHydro (J,PTE(I),  :)*QD2T(PTE(I))
   ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)

ENDDO ! J - Tower nodes / elements


   ! Convert the units of the forces and moments from N and N-m
   !    to kN and kN-m:

FrcONcRt = 0.001*FrcONcRt
FrcPRot  = 0.001*FrcPRot
FrcT0Trb = 0.001*FrcT0Trb
FZHydro  = 0.001*FZHydro
MomBNcRt = 0.001*MomBNcRt
MomLPRot = 0.001*MomLPRot
MomNGnRt = 0.001*MomNGnRt
MomNTail = 0.001*MomNTail
MomX0Trb = 0.001*MomX0Trb
MXHydro  = 0.001*MXHydro
FrcS0B   = 0.001*FrcS0B
MomH0B   = 0.001*MomH0B



   ! Define the output channel specifying the current simulation time:

AllOuts(  Time) = ZTime


IF ( CompAero )  THEN   ! AeroDyn has been used


   ! Wind Motions:

!bjj start of proposed change ad v13.00b
!rm   CALL GetHubWind( HHWndVec )
   HHWndVec(:) = AD_GetUndisturbedWind( ZTime, (/REAL(0.0, ReKi), REAL(0.0, ReKi), FASTHH /), ErrStat )
!bjj end of proposed change

   AllOuts(  WindVxi) = HHWndVec(1)
   AllOuts(  WindVyi) = HHWndVec(2)
   AllOuts(  WindVzi) = HHWndVec(3)
   AllOuts( TotWindV) = SQRT(   ( AllOuts(  WindVxi)*AllOuts(  WindVxi) ) &
                              + ( AllOuts(  WindVyi)*AllOuts(  WindVyi) ) &
                              + ( AllOuts(  WindVzi)*AllOuts(  WindVzi) )   )
   AllOuts( HorWindV) = SQRT(   ( AllOuts(  WindVxi)*AllOuts(  WindVxi) ) &
                              + ( AllOuts(  WindVyi)*AllOuts(  WindVyi) )   )
   AllOuts(HorWndDir) = ATAN2( AllOuts(  WindVyi), AllOuts(  WindVxi) )*R2D
   AllOuts(VerWndDir) = ATAN2( AllOuts(  WindVzi), AllOuts( HorWindV) )*R2D


   ! Tail Fin Element Aerodynamics:

   AllOuts(TFinAlpha) = TFinAOA*R2D
   AllOuts(TFinCLift) = TFinCL
   AllOuts(TFinCDrag) = TFinCD
   AllOuts(TFinDnPrs) = TFinQ
   AllOuts(TFinCPFx ) = TFinKFx*0.001
   AllOuts(TFinCPFy ) = TFinKFy*0.001


ENDIF

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for outputting the incident wave elevation at
!jmj   the platform reference point and the incident wave kinematics at up to 9
!jmj   nodes along the undeflected tower [not floating] or undisplaced platform
!jmj   [floating]:
IF ( CompHydro )  THEN  ! Hydrodynamics have been used


   ! Wave Motions:

   AllOuts(WaveElev ) = WaveElevation ( 1, ZTime )

   IF ( NWaveKin >= 1 )  THEN

      AllOuts(Wave1Vxi ) = WaveVelocity     ( WaveKinNd(1), 1, ZTime )
      AllOuts(Wave1Vyi ) = WaveVelocity     ( WaveKinNd(1), 2, ZTime )
      AllOuts(Wave1Vzi ) = WaveVelocity     ( WaveKinNd(1), 3, ZTime )
      AllOuts(Wave1Axi ) = WaveAcceleration ( WaveKinNd(1), 1, ZTime )
      AllOuts(Wave1Ayi ) = WaveAcceleration ( WaveKinNd(1), 2, ZTime )
      AllOuts(Wave1Azi ) = WaveAcceleration ( WaveKinNd(1), 3, ZTime )

      IF ( NWaveKin >= 2 )  THEN

         AllOuts(Wave2Vxi ) = WaveVelocity     ( WaveKinNd(2), 1, ZTime )
         AllOuts(Wave2Vyi ) = WaveVelocity     ( WaveKinNd(2), 2, ZTime )
         AllOuts(Wave2Vzi ) = WaveVelocity     ( WaveKinNd(2), 3, ZTime )
         AllOuts(Wave2Axi ) = WaveAcceleration ( WaveKinNd(2), 1, ZTime )
         AllOuts(Wave2Ayi ) = WaveAcceleration ( WaveKinNd(2), 2, ZTime )
         AllOuts(Wave2Azi ) = WaveAcceleration ( WaveKinNd(2), 3, ZTime )

         IF ( NWaveKin >= 3 )  THEN

            AllOuts(Wave3Vxi ) = WaveVelocity     ( WaveKinNd(3), 1, ZTime )
            AllOuts(Wave3Vyi ) = WaveVelocity     ( WaveKinNd(3), 2, ZTime )
            AllOuts(Wave3Vzi ) = WaveVelocity     ( WaveKinNd(3), 3, ZTime )
            AllOuts(Wave3Axi ) = WaveAcceleration ( WaveKinNd(3), 1, ZTime )
            AllOuts(Wave3Ayi ) = WaveAcceleration ( WaveKinNd(3), 2, ZTime )
            AllOuts(Wave3Azi ) = WaveAcceleration ( WaveKinNd(3), 3, ZTime )

            IF ( NWaveKin >= 4 )  THEN

               AllOuts(Wave4Vxi ) = WaveVelocity     ( WaveKinNd(4), 1, ZTime )
               AllOuts(Wave4Vyi ) = WaveVelocity     ( WaveKinNd(4), 2, ZTime )
               AllOuts(Wave4Vzi ) = WaveVelocity     ( WaveKinNd(4), 3, ZTime )
               AllOuts(Wave4Axi ) = WaveAcceleration ( WaveKinNd(4), 1, ZTime )
               AllOuts(Wave4Ayi ) = WaveAcceleration ( WaveKinNd(4), 2, ZTime )
               AllOuts(Wave4Azi ) = WaveAcceleration ( WaveKinNd(4), 3, ZTime )

               IF ( NWaveKin >= 5 )  THEN

                  AllOuts(Wave5Vxi ) = WaveVelocity     ( WaveKinNd(5), 1, ZTime )
                  AllOuts(Wave5Vyi ) = WaveVelocity     ( WaveKinNd(5), 2, ZTime )
                  AllOuts(Wave5Vzi ) = WaveVelocity     ( WaveKinNd(5), 3, ZTime )
                  AllOuts(Wave5Axi ) = WaveAcceleration ( WaveKinNd(5), 1, ZTime )
                  AllOuts(Wave5Ayi ) = WaveAcceleration ( WaveKinNd(5), 2, ZTime )
                  AllOuts(Wave5Azi ) = WaveAcceleration ( WaveKinNd(5), 3, ZTime )

                  IF ( NWaveKin >= 6 )  THEN

                     AllOuts(Wave6Vxi ) = WaveVelocity     ( WaveKinNd(6), 1, ZTime )
                     AllOuts(Wave6Vyi ) = WaveVelocity     ( WaveKinNd(6), 2, ZTime )
                     AllOuts(Wave6Vzi ) = WaveVelocity     ( WaveKinNd(6), 3, ZTime )
                     AllOuts(Wave6Axi ) = WaveAcceleration ( WaveKinNd(6), 1, ZTime )
                     AllOuts(Wave6Ayi ) = WaveAcceleration ( WaveKinNd(6), 2, ZTime )
                     AllOuts(Wave6Azi ) = WaveAcceleration ( WaveKinNd(6), 3, ZTime )

                     IF ( NWaveKin >= 7 )  THEN

                        AllOuts(Wave7Vxi ) = WaveVelocity     ( WaveKinNd(7), 1, ZTime )
                        AllOuts(Wave7Vyi ) = WaveVelocity     ( WaveKinNd(7), 2, ZTime )
                        AllOuts(Wave7Vzi ) = WaveVelocity     ( WaveKinNd(7), 3, ZTime )
                        AllOuts(Wave7Axi ) = WaveAcceleration ( WaveKinNd(7), 1, ZTime )
                        AllOuts(Wave7Ayi ) = WaveAcceleration ( WaveKinNd(7), 2, ZTime )
                        AllOuts(Wave7Azi ) = WaveAcceleration ( WaveKinNd(7), 3, ZTime )

                        IF ( NWaveKin >= 8 )  THEN

                           AllOuts(Wave8Vxi ) = WaveVelocity     ( WaveKinNd(8), 1, ZTime )
                           AllOuts(Wave8Vyi ) = WaveVelocity     ( WaveKinNd(8), 2, ZTime )
                           AllOuts(Wave8Vzi ) = WaveVelocity     ( WaveKinNd(8), 3, ZTime )
                           AllOuts(Wave8Axi ) = WaveAcceleration ( WaveKinNd(8), 1, ZTime )
                           AllOuts(Wave8Ayi ) = WaveAcceleration ( WaveKinNd(8), 2, ZTime )
                           AllOuts(Wave8Azi ) = WaveAcceleration ( WaveKinNd(8), 3, ZTime )

                           IF ( NWaveKin == 9 )  THEN

                              AllOuts(Wave9Vxi ) = WaveVelocity     ( WaveKinNd(9), 1, ZTime )
                              AllOuts(Wave9Vyi ) = WaveVelocity     ( WaveKinNd(9), 2, ZTime )
                              AllOuts(Wave9Vzi ) = WaveVelocity     ( WaveKinNd(9), 3, ZTime )
                              AllOuts(Wave9Axi ) = WaveAcceleration ( WaveKinNd(9), 1, ZTime )
                              AllOuts(Wave9Ayi ) = WaveAcceleration ( WaveKinNd(9), 2, ZTime )
                              AllOuts(Wave9Azi ) = WaveAcceleration ( WaveKinNd(9), 3, ZTime )

                           ENDIF

                        ENDIF

                     ENDIF

                  ENDIF

               ENDIF

            ENDIF

         ENDIF

      ENDIF

   ENDIF


ENDIF


!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! Blade 1 Tip Motions:

rSTipPSTip = rS0S(1,TipNode,:) - BldFlexL*j3(1,:)  ! Position vector from the undeflected blade tip (point S tip prime) to the deflected blade tip (point S tip) of blade 1.
rOSTip     = rS  (1,TipNode,:) - rO                ! Position vector from the deflected tower top (point O) to the deflected blade tip (point S tip) of blade 1.
rOSTipxn   =  DOT_PRODUCT( rOSTip, d1 )                ! Component of rOSTip directed along the xn-axis.
!BJJ START OF PROPOSED CHANGE vXX
!rmrOSTipyn   = -DodProd( rOSTip, d3 )                ! Component of rOSTip directed along the yn-axis.
rOSTipyn   = -1.0*DOT_PRODUCT( rOSTip, d3 )                ! Component of rOSTip directed along the yn-axis.
!BJJ End of proposed change
rOSTipzn   =  DOT_PRODUCT( rOSTip, d2 )                ! Component of rOSTip directed along the zn-axis.

AllOuts(  TipDxc1) = DOT_PRODUCT(            rSTipPSTip, i1(1,         :) )
AllOuts(  TipDyc1) = DOT_PRODUCT(            rSTipPSTip, i2(1,         :) )
AllOuts(  TipDzc1) = DOT_PRODUCT(            rSTipPSTip, i3(1,         :) )
AllOuts(  TipDxb1) = DOT_PRODUCT(            rSTipPSTip, j1(1,         :) )
AllOuts(  TipDyb1) = DOT_PRODUCT(            rSTipPSTip, j2(1,         :) )
!JASON: USE TipNode HERE INSTEAD OF BldNodes IF YOU ALLOCATE AND DEFINE n1, n2, n3, m1, m2, AND m3 TO USE TipNode.  THIS WILL REQUIRE THAT THE AERODYNAMIC AND STRUCTURAL TWISTS, AeroTwst() AND ThetaS(), BE KNOWN AT THE TIP!!!
AllOuts( TipALxb1) = DOT_PRODUCT( LinAccES(1,TipNode,:), n1(1,BldNodes,:) )
AllOuts( TipALyb1) = DOT_PRODUCT( LinAccES(1,TipNode,:), n2(1,BldNodes,:) )
AllOuts( TipALzb1) = DOT_PRODUCT( LinAccES(1,TipNode,:), n3(1,BldNodes,:) )
AllOuts( TipRDxb1) = DOT_PRODUCT( AngPosHM(1,TipNode,:), j1(1,         :) )*R2D
AllOuts( TipRDyb1) = DOT_PRODUCT( AngPosHM(1,TipNode,:), j2(1,         :) )*R2D
! There is no sense computing AllOuts( TipRDzc1) here since it is always zero for FAST simulation results.
IF ( rOSTipzn > 0.0 )  THEN   ! Tip of blade 1 is above the yaw bearing.
   AllOuts(TipClrnc1) = SQRT( rOSTipxn*rOSTipxn + rOSTipyn*rOSTipyn + rOSTipzn*rOSTipzn ) ! Absolute distance from the tower top / yaw bearing to the tip of blade 1.
ELSE                          ! Tip of blade 1 is below the yaw bearing.
   AllOuts(TipClrnc1) = SQRT( rOSTipxn*rOSTipxn + rOSTipyn*rOSTipyn                     ) ! Perpendicular distance from the yaw axis / tower centerline to the tip of blade 1.
ENDIF


   ! Blade 1 Local Span Motions:

IF ( NBlGages >= 1 )  THEN

   AllOuts(Spn1ALxb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(1),:), n1(1,BldGagNd(1),:) )
   AllOuts(Spn1ALyb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(1),:), n2(1,BldGagNd(1),:) )
   AllOuts(Spn1ALzb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(1),:), n3(1,BldGagNd(1),:) )

   IF ( NBlGages >= 2 )  THEN

      AllOuts(Spn2ALxb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(2),:), n1(1,BldGagNd(2),:) )
      AllOuts(Spn2ALyb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(2),:), n2(1,BldGagNd(2),:) )
      AllOuts(Spn2ALzb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(2),:), n3(1,BldGagNd(2),:) )

      IF ( NBlGages >= 3 )  THEN

         AllOuts(Spn3ALxb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(3),:), n1(1,BldGagNd(3),:) )
         AllOuts(Spn3ALyb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(3),:), n2(1,BldGagNd(3),:) )
         AllOuts(Spn3ALzb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(3),:), n3(1,BldGagNd(3),:) )

         IF ( NBlGages >= 4 )  THEN

            AllOuts(Spn4ALxb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(4),:), n1(1,BldGagNd(4),:) )
            AllOuts(Spn4ALyb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(4),:), n2(1,BldGagNd(4),:) )
            AllOuts(Spn4ALzb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(4),:), n3(1,BldGagNd(4),:) )

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:
!remove6.02a            IF ( NBlGages == 5 )  THEN
            IF ( NBlGages >= 5 )  THEN
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

               AllOuts(Spn5ALxb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(5),:), n1(1,BldGagNd(5),:) )
               AllOuts(Spn5ALyb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(5),:), n2(1,BldGagNd(5),:) )
               AllOuts(Spn5ALzb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(5),:), n3(1,BldGagNd(5),:) )
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:
               IF ( NBlGages >= 6 )  THEN

                  AllOuts(Spn6ALxb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(6),:), n1(1,BldGagNd(6),:) )
                  AllOuts(Spn6ALyb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(6),:), n2(1,BldGagNd(6),:) )
                  AllOuts(Spn6ALzb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(6),:), n3(1,BldGagNd(6),:) )

                  IF ( NBlGages >= 7 )  THEN

                     AllOuts(Spn7ALxb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(7),:), n1(1,BldGagNd(7),:) )
                     AllOuts(Spn7ALyb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(7),:), n2(1,BldGagNd(7),:) )
                     AllOuts(Spn7ALzb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(7),:), n3(1,BldGagNd(7),:) )

                     IF ( NBlGages >= 8 )  THEN

                        AllOuts(Spn8ALxb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(8),:), n1(1,BldGagNd(8),:) )
                        AllOuts(Spn8ALyb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(8),:), n2(1,BldGagNd(8),:) )
                        AllOuts(Spn8ALzb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(8),:), n3(1,BldGagNd(8),:) )

                        IF ( NBlGages == 9 )  THEN

                           AllOuts(Spn9ALxb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(9),:), n1(1,BldGagNd(9),:) )
                           AllOuts(Spn9ALyb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(9),:), n2(1,BldGagNd(9),:) )
                           AllOuts(Spn9ALzb1) = DOT_PRODUCT( LinAccES(1,BldGagNd(9),:), n3(1,BldGagNd(9),:) )

                        ENDIF

                     ENDIF

                  ENDIF

               ENDIF

!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
            ENDIF

         ENDIF

      ENDIF

   ENDIF

ENDIF


   ! Blade 2 Tip Motions:

rSTipPSTip = rS0S(2,TipNode,:) - BldFlexL*j3(2,:)  ! Position vector from the undeflected blade tip (point S tip prime) to the deflected blade tip (point S tip) of blade 2.
rOSTip     = rS  (2,TipNode,:) - rO                ! Position vector from the deflected tower top (point O) to the deflected blade tip (point S tip) of blade 2.
rOSTipxn   =  DOT_PRODUCT( rOSTip, d1 )                ! Component of rOSTip directed along the xn-axis.
!bjj start of proposed change vXX
!rmrOSTipyn   = -DodProd( rOSTip, d3 )                ! Component of rOSTip directed along the yn-axis.
rOSTipyn   = -1.0*DOT_PRODUCT( rOSTip, d3 )                ! Component of rOSTip directed along the yn-axis.
!bjj end of proposed change
rOSTipzn   =  DOT_PRODUCT( rOSTip, d2 )                ! Component of rOSTip directed along the zn-axis.

AllOuts(  TipDxc2) = DOT_PRODUCT(            rSTipPSTip, i1(2,         :) )
AllOuts(  TipDyc2) = DOT_PRODUCT(            rSTipPSTip, i2(2,         :) )
AllOuts(  TipDzc2) = DOT_PRODUCT(            rSTipPSTip, i3(2,         :) )
AllOuts(  TipDxb2) = DOT_PRODUCT(            rSTipPSTip, j1(2,         :) )
AllOuts(  TipDyb2) = DOT_PRODUCT(            rSTipPSTip, j2(2,         :) )
!JASON: USE TipNode HERE INSTEAD OF BldNodes IF YOU ALLOCATE AND DEFINE n1, n2, n3, m1, m2, AND m3 TO USE TipNode.  THIS WILL REQUIRE THAT THE AERODYNAMIC AND STRUCTURAL TWISTS, AeroTwst() AND ThetaS(), BE KNOWN AT THE TIP!!!
AllOuts( TipALxb2) = DOT_PRODUCT( LinAccES(2,TipNode,:), n1(2,BldNodes,:) )
AllOuts( TipALyb2) = DOT_PRODUCT( LinAccES(2,TipNode,:), n2(2,BldNodes,:) )
AllOuts( TipALzb2) = DOT_PRODUCT( LinAccES(2,TipNode,:), n3(2,BldNodes,:) )
AllOuts( TipRDxb2) = DOT_PRODUCT( AngPosHM(2,TipNode,:), j1(2,         :) )*R2D
AllOuts( TipRDyb2) = DOT_PRODUCT( AngPosHM(2,TipNode,:), j2(2,         :) )*R2D
! There is no sense computing AllOuts( TipRDzc2) here since it is always zero for FAST simulation results.
IF ( rOSTipzn > 0.0 )  THEN   ! Tip of blade 2 is above the yaw bearing.
   AllOuts(TipClrnc2) = SQRT( rOSTipxn*rOSTipxn + rOSTipyn*rOSTipyn + rOSTipzn*rOSTipzn ) ! Absolute distance from the tower top / yaw bearing to the tip of blade 2.
ELSE                          ! Tip of blade 2 is below the yaw bearing.
   AllOuts(TipClrnc2) = SQRT( rOSTipxn*rOSTipxn + rOSTipyn*rOSTipyn                     ) ! Perpendicular distance from the yaw axis / tower centerline to the tip of blade 2.
ENDIF

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Add blade strain gage output parameters for the local loads and motions of
!jmj   blades 2 and 3:


   ! Blade 2 Local Span Motions:

IF ( NBlGages >= 1 )  THEN

   AllOuts(Spn1ALxb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(1),:), n1(2,BldGagNd(1),:) )
   AllOuts(Spn1ALyb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(1),:), n2(2,BldGagNd(1),:) )
   AllOuts(Spn1ALzb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(1),:), n3(2,BldGagNd(1),:) )

   IF ( NBlGages >= 2 )  THEN

      AllOuts(Spn2ALxb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(2),:), n1(2,BldGagNd(2),:) )
      AllOuts(Spn2ALyb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(2),:), n2(2,BldGagNd(2),:) )
      AllOuts(Spn2ALzb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(2),:), n3(2,BldGagNd(2),:) )

      IF ( NBlGages >= 3 )  THEN

         AllOuts(Spn3ALxb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(3),:), n1(2,BldGagNd(3),:) )
         AllOuts(Spn3ALyb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(3),:), n2(2,BldGagNd(3),:) )
         AllOuts(Spn3ALzb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(3),:), n3(2,BldGagNd(3),:) )

         IF ( NBlGages >= 4 )  THEN

            AllOuts(Spn4ALxb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(4),:), n1(2,BldGagNd(4),:) )
            AllOuts(Spn4ALyb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(4),:), n2(2,BldGagNd(4),:) )
            AllOuts(Spn4ALzb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(4),:), n3(2,BldGagNd(4),:) )

            IF ( NBlGages >= 5 )  THEN

               AllOuts(Spn5ALxb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(5),:), n1(2,BldGagNd(5),:) )
               AllOuts(Spn5ALyb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(5),:), n2(2,BldGagNd(5),:) )
               AllOuts(Spn5ALzb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(5),:), n3(2,BldGagNd(5),:) )

               IF ( NBlGages >= 6 )  THEN

                  AllOuts(Spn6ALxb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(6),:), n1(2,BldGagNd(6),:) )
                  AllOuts(Spn6ALyb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(6),:), n2(2,BldGagNd(6),:) )
                  AllOuts(Spn6ALzb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(6),:), n3(2,BldGagNd(6),:) )

                  IF ( NBlGages >= 7 )  THEN

                     AllOuts(Spn7ALxb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(7),:), n1(2,BldGagNd(7),:) )
                     AllOuts(Spn7ALyb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(7),:), n2(2,BldGagNd(7),:) )
                     AllOuts(Spn7ALzb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(7),:), n3(2,BldGagNd(7),:) )

                     IF ( NBlGages >= 8 )  THEN

                        AllOuts(Spn8ALxb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(8),:), n1(2,BldGagNd(8),:) )
                        AllOuts(Spn8ALyb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(8),:), n2(2,BldGagNd(8),:) )
                        AllOuts(Spn8ALzb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(8),:), n3(2,BldGagNd(8),:) )

                        IF ( NBlGages == 9 )  THEN

                           AllOuts(Spn9ALxb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(9),:), n1(2,BldGagNd(9),:) )
                           AllOuts(Spn9ALyb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(9),:), n2(2,BldGagNd(9),:) )
                           AllOuts(Spn9ALzb2) = DOT_PRODUCT( LinAccES(2,BldGagNd(9),:), n3(2,BldGagNd(9),:) )

                        ENDIF

                     ENDIF

                  ENDIF

               ENDIF

            ENDIF

         ENDIF

      ENDIF

   ENDIF

ENDIF
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

IF ( NumBl == 3 )  THEN ! 3-blader


   ! Blade 3 Tip Motions:

   rSTipPSTip = rS0S(3,TipNode,:) - BldFlexL*j3(3,:)  ! Position vector from the undeflected blade tip (point S tip prime) to the deflected blade tip (point S tip) of blade 3.
   rOSTip     = rS  (3,TipNode,:) - rO                ! Position vector from the deflected tower top (point O) to the deflected blade tip (point S tip) of blade 3.
   rOSTipxn   =  DOT_PRODUCT( rOSTip, d1 )                ! Component of rOSTip directed along the xn-axis.
!bjj start of proposed change vXX
!rm   rOSTipyn   = -DotProd( rOSTip, d3 )                ! Component of rOSTip directed along the yn-axis.
   rOSTipyn   = -1.0*DOT_PRODUCT( rOSTip, d3 )                ! Component of rOSTip directed along the yn-axis.
!bjj end of proposed change vXX
   rOSTipzn   =  DOT_PRODUCT( rOSTip, d2 )                ! Component of rOSTip directed along the zn-axis.

   AllOuts(  TipDxc3) = DOT_PRODUCT(            rSTipPSTip, i1(3,         :) )
   AllOuts(  TipDyc3) = DOT_PRODUCT(            rSTipPSTip, i2(3,         :) )
   AllOuts(  TipDzc3) = DOT_PRODUCT(            rSTipPSTip, i3(3,         :) )
   AllOuts(  TipDxb3) = DOT_PRODUCT(            rSTipPSTip, j1(3,         :) )
   AllOuts(  TipDyb3) = DOT_PRODUCT(            rSTipPSTip, j2(3,         :) )
!JASON: USE TipNode HERE INSTEAD OF BldNodes IF YOU ALLOCATE AND DEFINE n1, n2, n3, m1, m2, AND m3 TO USE TipNode.  THIS WILL REQUIRE THAT THE AERODYNAMIC AND STRUCTURAL TWISTS, AeroTwst() AND ThetaS(), BE KNOWN AT THE TIP!!!
   AllOuts( TipALxb3) = DOT_PRODUCT( LinAccES(3,TipNode,:), n1(3,BldNodes,:) )
   AllOuts( TipALyb3) = DOT_PRODUCT( LinAccES(3,TipNode,:), n2(3,BldNodes,:) )
   AllOuts( TipALzb3) = DOT_PRODUCT( LinAccES(3,TipNode,:), n3(3,BldNodes,:) )
   AllOuts( TipRDxb3) = DOT_PRODUCT( AngPosHM(3,TipNode,:), j1(3,         :) )*R2D
   AllOuts( TipRDyb3) = DOT_PRODUCT( AngPosHM(3,TipNode,:), j2(3,         :) )*R2D
   ! There is no sense computing AllOuts( TipRDzc3) here since it is always zero for FAST simulation results.
   IF ( rOSTipzn > 0.0 )  THEN   ! Tip of blade 3 is above the yaw bearing.
      AllOuts(TipClrnc3) = SQRT( rOSTipxn*rOSTipxn + rOSTipyn*rOSTipyn + rOSTipzn*rOSTipzn ) ! Absolute distance from the tower top / yaw bearing to the tip of blade 3.
   ELSE                          ! Tip of blade 3 is below the yaw bearing.
      AllOuts(TipClrnc3) = SQRT( rOSTipxn*rOSTipxn + rOSTipyn*rOSTipyn                     ) ! Perpendicular distance from the yaw axis / tower centerline to the tip of blade 3.
   ENDIF

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Add blade strain gage output parameters for the local loads and motions of
!jmj   blades 2 and 3:


   ! Blade 3 Local Span Motions:

   IF ( NBlGages >= 1 )  THEN

      AllOuts(Spn1ALxb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(1),:), n1(3,BldGagNd(1),:) )
      AllOuts(Spn1ALyb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(1),:), n2(3,BldGagNd(1),:) )
      AllOuts(Spn1ALzb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(1),:), n3(3,BldGagNd(1),:) )

      IF ( NBlGages >= 2 )  THEN

         AllOuts(Spn2ALxb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(2),:), n1(3,BldGagNd(2),:) )
         AllOuts(Spn2ALyb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(2),:), n2(3,BldGagNd(2),:) )
         AllOuts(Spn2ALzb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(2),:), n3(3,BldGagNd(2),:) )

         IF ( NBlGages >= 3 )  THEN

            AllOuts(Spn3ALxb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(3),:), n1(3,BldGagNd(3),:) )
            AllOuts(Spn3ALyb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(3),:), n2(3,BldGagNd(3),:) )
            AllOuts(Spn3ALzb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(3),:), n3(3,BldGagNd(3),:) )

            IF ( NBlGages >= 4 )  THEN

               AllOuts(Spn4ALxb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(4),:), n1(3,BldGagNd(4),:) )
               AllOuts(Spn4ALyb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(4),:), n2(3,BldGagNd(4),:) )
               AllOuts(Spn4ALzb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(4),:), n3(3,BldGagNd(4),:) )

               IF ( NBlGages >= 5 )  THEN

                  AllOuts(Spn5ALxb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(5),:), n1(3,BldGagNd(5),:) )
                  AllOuts(Spn5ALyb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(5),:), n2(3,BldGagNd(5),:) )
                  AllOuts(Spn5ALzb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(5),:), n3(3,BldGagNd(5),:) )

                  IF ( NBlGages >= 6 )  THEN

                     AllOuts(Spn6ALxb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(6),:), n1(3,BldGagNd(6),:) )
                     AllOuts(Spn6ALyb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(6),:), n2(3,BldGagNd(6),:) )
                     AllOuts(Spn6ALzb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(6),:), n3(3,BldGagNd(6),:) )

                     IF ( NBlGages >= 7 )  THEN

                        AllOuts(Spn7ALxb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(7),:), n1(3,BldGagNd(7),:) )
                        AllOuts(Spn7ALyb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(7),:), n2(3,BldGagNd(7),:) )
                        AllOuts(Spn7ALzb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(7),:), n3(3,BldGagNd(7),:) )

                        IF ( NBlGages >= 8 )  THEN

                           AllOuts(Spn8ALxb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(8),:), n1(3,BldGagNd(8),:) )
                           AllOuts(Spn8ALyb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(8),:), n2(3,BldGagNd(8),:) )
                           AllOuts(Spn8ALzb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(8),:), n3(3,BldGagNd(8),:) )

                           IF ( NBlGages == 9 )  THEN

                              AllOuts(Spn9ALxb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(9),:), n1(3,BldGagNd(9),:) )
                              AllOuts(Spn9ALyb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(9),:), n2(3,BldGagNd(9),:) )
                              AllOuts(Spn9ALzb3) = DOT_PRODUCT( LinAccES(3,BldGagNd(9),:), n3(3,BldGagNd(9),:) )

                           ENDIF

                        ENDIF

                     ENDIF

                  ENDIF

               ENDIF

            ENDIF

         ENDIF

      ENDIF

   ENDIF
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

ENDIF


   ! Blade Pitch Motions:

AllOuts(PtchPMzc1) = BlPitch(1)*R2D
AllOuts(PtchPMzc2) = BlPitch(2)*R2D
IF ( NumBl == 3 )  THEN ! 3-blader

   AllOuts(PtchPMzc3) = BlPitch(3)*R2D

ELSE  ! 2-blader


   ! Teeter Motions:

   AllOuts(  TeetPya) = QT  (DOF_Teet)*R2D
   AllOuts(  TeetVya) = QDT (DOF_Teet)*R2D
   AllOuts(  TeetAya) = QD2T(DOF_Teet)*R2D

ENDIF


   ! Shaft Motions:

IF ( IgnoreMOD )  THEN  ! Don't use MOD when computing AllOuts(LSSTipPxa) and AllOuts(LSSGagPxa) -- IgnoreMOD is needed when computing CMat for output measurements during FAST linearization.
   AllOuts(LSSTipPxa) =      ( QT  (DOF_GeAz) + QT  (DOF_DrTr) )*R2D + AzimB1Up + 90.0
   AllOuts(LSSGagPxa) =        QT  (DOF_GeAz)                   *R2D + AzimB1Up + 90.0
ELSE                    ! Do    use MOD when computing AllOuts(LSSTipPxa) and AllOuts(LSSGagPxa)
   AllOuts(LSSTipPxa) = MOD( ( QT  (DOF_GeAz) + QT  (DOF_DrTr) )*R2D + AzimB1Up + 90.0, 360.0 )
   AllOuts(LSSGagPxa) = MOD(   QT  (DOF_GeAz)                   *R2D + AzimB1Up + 90.0, 360.0 )
ENDIF
AllOuts(   LSSTipVxa) =      ( QDT (DOF_GeAz) + QDT (DOF_DrTr) )*RPS2RPM
AllOuts(   LSSTipAxa) =      ( QD2T(DOF_GeAz) + QD2T(DOF_DrTr) )*R2D
AllOuts(   LSSGagVxa) =        QDT (DOF_GeAz)                   *RPS2RPM
AllOuts(   LSSGagAxa) =        QD2T(DOF_GeAz)                   *R2D
AllOuts(     HSShftV) = GBRatio*AllOuts(LSSGagVxa)
AllOuts(     HSShftA) = GBRatio*AllOuts(LSSGagAxa)
IF ( AllOuts(  WindVxi) /= 0.0 )  THEN  ! .TRUE. if the denominator in the following equation is not zero.

   AllOuts(TipSpdRat) =      ( QDT (DOF_GeAz) + QDT (DOF_DrTr) )*AvgNrmTpRd / AllOuts(  WindVxi)

ENDIF


   ! Nacelle IMU Motions:

AllOuts(NcIMUTVxs) =  DOT_PRODUCT( LinVelEIMU, c1 )
!bjj start of proposed change vXX
!rmAllOuts(NcIMUTVys) = -DOT_PRODUCT( LinVelEIMU, c3 )
AllOuts(NcIMUTVys) = -1.0*DOT_PRODUCT( LinVelEIMU, c3 )
!bjj end of proposed change vXX
AllOuts(NcIMUTVzs) =  DOT_PRODUCT( LinVelEIMU, c2 )
AllOuts(NcIMUTAxs) =  DOT_PRODUCT( LinAccEIMU, c1 )
!bjj start of proposed change vXX
!rmAllOuts(NcIMUTAys) = -DOT_PRODUCT( LinAccEIMU, c3 )
AllOuts(NcIMUTAys) = -1.0*DOT_PRODUCT( LinAccEIMU, c3 )
!bjj end of proposed change vXX
AllOuts(NcIMUTAzs) =  DOT_PRODUCT( LinAccEIMU, c2 )
AllOuts(NcIMURVxs) =  DOT_PRODUCT( AngVelER  , c1 )*R2D
!bjj start of proposed change vXX
!rmAllOuts(NcIMURVys) = -DOT_PRODUCT( AngVelER  , c3 )*R2D
AllOuts(NcIMURVys) = -1.0*DOT_PRODUCT( AngVelER  , c3 )*R2D
!bjj end of proposed change vXX
AllOuts(NcIMURVzs) =  DOT_PRODUCT( AngVelER  , c2 )*R2D
AllOuts(NcIMURAxs) =  DOT_PRODUCT( AngAccER  , c1 )*R2D
!bjj start of proposed change vXX
!rmAllOuts(NcIMURAys) = -DOT_PRODUCT( AngAccER  , c3 )*R2D
AllOuts(NcIMURAys) = -1.0*DOT_PRODUCT( AngAccER  , c3 )*R2D
!bjj end of proposed change vXX
AllOuts(NcIMURAzs) =  DOT_PRODUCT( AngAccER  , c2 )*R2D


   ! Rotor-Furl Motions:

AllOuts( RotFurlP) = QT  (DOF_RFrl)*R2D
AllOuts( RotFurlV) = QDT (DOF_RFrl)*R2D
AllOuts( RotFurlA) = QD2T(DOF_RFrl)*R2D


   ! Tail-Furl Motions:

AllOuts(TailFurlP) = QT  (DOF_TFrl)*R2D
AllOuts(TailFurlV) = QDT (DOF_TFrl)*R2D
AllOuts(TailFurlA) = QD2T(DOF_TFrl)*R2D


   ! Yaw Motions:

AllOuts(   YawPzn) = QT  (DOF_Yaw )*R2D
AllOuts(   YawVzn) = QDT (DOF_Yaw )*R2D
AllOuts(   YawAzn) = QD2T(DOF_Yaw )*R2D


   ! Tower-Top / Yaw Bearing Motions:

rOPO     = rT0O - TwrFlexL*a2 ! Position vector from the undeflected tower top (point O prime) to the deflected tower top (point O).

AllOuts(YawBrTDxp) =  DOT_PRODUCT(     rOPO, b1 )
AllOuts(YawBrTDyp) = -DOT_PRODUCT(     rOPO, b3 )
AllOuts(YawBrTDzp) =  DOT_PRODUCT(     rOPO, b2 )
AllOuts(YawBrTDxt) =  DOT_PRODUCT(     rOPO, a1 )
AllOuts(YawBrTDyt) = -DOT_PRODUCT(     rOPO, a3 )
AllOuts(YawBrTDzt) =  DOT_PRODUCT(     rOPO, a2 )
AllOuts(YawBrTAxp) =  DOT_PRODUCT( LinAccEO, b1 )
AllOuts(YawBrTAyp) = -DOT_PRODUCT( LinAccEO, b3 )
AllOuts(YawBrTAzp) =  DOT_PRODUCT( LinAccEO, b2 )
AllOuts(YawBrRDxt) =  DOT_PRODUCT( AngPosXB, a1 )*R2D
AllOuts(YawBrRDyt) = -DOT_PRODUCT( AngPosXB, a3 )*R2D
! There is no sense computing AllOuts(YawBrRDzt) here since it is always zero for FAST simulation results.
AllOuts(YawBrRVxp) =  DOT_PRODUCT( AngVelEB, b1 )*R2D
AllOuts(YawBrRVyp) = -DOT_PRODUCT( AngVelEB, b3 )*R2D
AllOuts(YawBrRVzp) =  DOT_PRODUCT( AngVelEB, b2 )*R2D
AllOuts(YawBrRAxp) =  DOT_PRODUCT( AngAccEB, b1 )*R2D
AllOuts(YawBrRAyp) = -DOT_PRODUCT( AngAccEB, b3 )*R2D
AllOuts(YawBrRAzp) =  DOT_PRODUCT( AngAccEB, b2 )*R2D


   ! Local Tower Motions:

IF ( NTwGages >= 1 )  THEN

   AllOuts(TwHt1ALxt) =  DOT_PRODUCT( LinAccET(TwrGagNd(1),:), t1(TwrGagNd(1),:) )
   AllOuts(TwHt1ALyt) = -DOT_PRODUCT( LinAccET(TwrGagNd(1),:), t3(TwrGagNd(1),:) )
   AllOuts(TwHt1ALzt) =  DOT_PRODUCT( LinAccET(TwrGagNd(1),:), t2(TwrGagNd(1),:) )

   IF ( NTwGages >= 2 )  THEN

      AllOuts(TwHt2ALxt) =  DOT_PRODUCT( LinAccET(TwrGagNd(2),:), t1(TwrGagNd(2),:) )
      AllOuts(TwHt2ALyt) = -DOT_PRODUCT( LinAccET(TwrGagNd(2),:), t3(TwrGagNd(2),:) )
      AllOuts(TwHt2ALzt) =  DOT_PRODUCT( LinAccET(TwrGagNd(2),:), t2(TwrGagNd(2),:) )

      IF ( NTwGages >= 3 )  THEN

         AllOuts(TwHt3ALxt) =  DOT_PRODUCT( LinAccET(TwrGagNd(3),:), t1(TwrGagNd(3),:) )
         AllOuts(TwHt3ALyt) = -DOT_PRODUCT( LinAccET(TwrGagNd(3),:), t3(TwrGagNd(3),:) )
         AllOuts(TwHt3ALzt) =  DOT_PRODUCT( LinAccET(TwrGagNd(3),:), t2(TwrGagNd(3),:) )

         IF ( NTwGages >= 4 )  THEN

            AllOuts(TwHt4ALxt) =  DOT_PRODUCT( LinAccET(TwrGagNd(4),:), t1(TwrGagNd(4),:) )
            AllOuts(TwHt4ALyt) = -DOT_PRODUCT( LinAccET(TwrGagNd(4),:), t3(TwrGagNd(4),:) )
            AllOuts(TwHt4ALzt) =  DOT_PRODUCT( LinAccET(TwrGagNd(4),:), t2(TwrGagNd(4),:) )

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:
!remove6.02a            IF ( NTwGages == 5 )  THEN
            IF ( NTwGages >= 5 )  THEN
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

               AllOuts(TwHt5ALxt) =  DOT_PRODUCT( LinAccET(TwrGagNd(5),:), t1(TwrGagNd(5),:) )
               AllOuts(TwHt5ALyt) = -DOT_PRODUCT( LinAccET(TwrGagNd(5),:), t3(TwrGagNd(5),:) )
               AllOuts(TwHt5ALzt) =  DOT_PRODUCT( LinAccET(TwrGagNd(5),:), t2(TwrGagNd(5),:) )
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:
               IF ( NTwGages >= 6 )  THEN

                  AllOuts(TwHt6ALxt) =     DOT_PRODUCT( LinAccET(TwrGagNd(6),:), t1(TwrGagNd(6),:) )
                  AllOuts(TwHt6ALyt) = -1.*DOT_PRODUCT( LinAccET(TwrGagNd(6),:), t3(TwrGagNd(6),:) )
                  AllOuts(TwHt6ALzt) =     DOT_PRODUCT( LinAccET(TwrGagNd(6),:), t2(TwrGagNd(6),:) )

                  IF ( NTwGages >= 7 )  THEN

                     AllOuts(TwHt7ALxt) =     DOT_PRODUCT( LinAccET(TwrGagNd(7),:), t1(TwrGagNd(7),:) )
                     AllOuts(TwHt7ALyt) = -1.*DOT_PRODUCT( LinAccET(TwrGagNd(7),:), t3(TwrGagNd(7),:) )
                     AllOuts(TwHt7ALzt) =     DOT_PRODUCT( LinAccET(TwrGagNd(7),:), t2(TwrGagNd(7),:) )

                     IF ( NTwGages >= 8 )  THEN

                        AllOuts(TwHt8ALxt) =     DOT_PRODUCT( LinAccET(TwrGagNd(8),:), t1(TwrGagNd(8),:) )
                        AllOuts(TwHt8ALyt) = -1.*DOT_PRODUCT( LinAccET(TwrGagNd(8),:), t3(TwrGagNd(8),:) )
                        AllOuts(TwHt8ALzt) =     DOT_PRODUCT( LinAccET(TwrGagNd(8),:), t2(TwrGagNd(8),:) )

                        IF ( NTwGages == 9 )  THEN

                           AllOuts(TwHt9ALxt) =     DOT_PRODUCT( LinAccET(TwrGagNd(9),:), t1(TwrGagNd(9),:) )
                           AllOuts(TwHt9ALyt) = -1.*DOT_PRODUCT( LinAccET(TwrGagNd(9),:), t3(TwrGagNd(9),:) )
                           AllOuts(TwHt9ALzt) =     DOT_PRODUCT( LinAccET(TwrGagNd(9),:), t2(TwrGagNd(9),:) )

                        ENDIF

                     ENDIF

                  ENDIF

               ENDIF

!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
            ENDIF

         ENDIF

      ENDIF

   ENDIF

ENDIF


   ! Platform Motions:

AllOuts( PtfmTDxt) =  DOT_PRODUCT(       rZ, a1 )
AllOuts( PtfmTDyt) = -DOT_PRODUCT(       rZ, a3 )
AllOuts( PtfmTDzt) =  DOT_PRODUCT(       rZ, a2 )
AllOuts( PtfmTDxi) = QT  (DOF_Sg  )
AllOuts( PtfmTDyi) = QT  (DOF_Sw  )
AllOuts( PtfmTDzi) = QT  (DOF_Hv  )
AllOuts( PtfmTVxt) =  DOT_PRODUCT( LinVelEZ, a1 )
AllOuts( PtfmTVyt) = -DOT_PRODUCT( LinVelEZ, a3 )
AllOuts( PtfmTVzt) =  DOT_PRODUCT( LinVelEZ, a2 )
AllOuts( PtfmTVxi) = QDT (DOF_Sg  )
AllOuts( PtfmTVyi) = QDT (DOF_Sw  )
AllOuts( PtfmTVzi) = QDT (DOF_Hv  )
AllOuts( PtfmTAxt) =  DOT_PRODUCT( LinAccEZ, a1 )
AllOuts( PtfmTAyt) = -DOT_PRODUCT( LinAccEZ, a3 )
AllOuts( PtfmTAzt) =  DOT_PRODUCT( LinAccEZ, a2 )
AllOuts( PtfmTAxi) = QD2T(DOF_Sg  )
AllOuts( PtfmTAyi) = QD2T(DOF_Sw  )
AllOuts( PtfmTAzi) = QD2T(DOF_Hv  )
AllOuts( PtfmRDxi) = QT  (DOF_R   )          *R2D
AllOuts( PtfmRDyi) = QT  (DOF_P   )          *R2D
AllOuts( PtfmRDzi) = QT  (DOF_Y   )          *R2D
AllOuts( PtfmRVxt) =  DOT_PRODUCT( AngVelEX, a1 )*R2D
AllOuts( PtfmRVyt) = -DOT_PRODUCT( AngVelEX, a3 )*R2D
AllOuts( PtfmRVzt) =  DOT_PRODUCT( AngVelEX, a2 )*R2D
AllOuts( PtfmRVxi) = QDT (DOF_R   )          *R2D
AllOuts( PtfmRVyi) = QDT (DOF_P   )          *R2D
AllOuts( PtfmRVzi) = QDT (DOF_Y   )          *R2D
AllOuts( PtfmRAxt) =  DOT_PRODUCT( AngAccEX, a1 )*R2D
AllOuts( PtfmRAyt) = -DOT_PRODUCT( AngAccEX, a3 )*R2D
AllOuts( PtfmRAzt) =  DOT_PRODUCT( AngAccEX, a2 )*R2D
AllOuts( PtfmRAxi) = QD2T(DOF_R   )          *R2D
AllOuts( PtfmRAyi) = QD2T(DOF_P   )          *R2D
AllOuts( PtfmRAzi) = QD2T(DOF_Y   )          *R2D


   ! Nacelle Yaw Error Estimate:

IF ( CompAero )  THEN   ! AeroDyn has been used

   AllOuts(NacYawErr) = AllOuts(HorWndDir) - AllOuts(YawPzn) - AllOuts(YawBrRDzt) - AllOuts(PtfmRDzi)

ENDIF



   ! Blade 1 Root Loads:

AllOuts( RootFxc1) = DOT_PRODUCT( FrcS0B(1,:), i1(1,:) )
AllOuts( RootFyc1) = DOT_PRODUCT( FrcS0B(1,:), i2(1,:) )
AllOuts( RootFzc1) = DOT_PRODUCT( FrcS0B(1,:), i3(1,:) )
AllOuts( RootFxb1) = DOT_PRODUCT( FrcS0B(1,:), j1(1,:) )
AllOuts( RootFyb1) = DOT_PRODUCT( FrcS0B(1,:), j2(1,:) )
AllOuts( RootMxc1) = DOT_PRODUCT( MomH0B(1,:), i1(1,:) )
AllOuts( RootMyc1) = DOT_PRODUCT( MomH0B(1,:), i2(1,:) )
AllOuts( RootMzc1) = DOT_PRODUCT( MomH0B(1,:), i3(1,:) )
AllOuts( RootMxb1) = DOT_PRODUCT( MomH0B(1,:), j1(1,:) )
AllOuts( RootMyb1) = DOT_PRODUCT( MomH0B(1,:), j2(1,:) )


   ! Blade 1 Local Span Loads:

IF ( NBlGages >= 1 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
   CALL CrossProd( MomMGagB, rS0S(1,TipNode,:) - rS0S(1,BldGagNd(1),:), &
                   FSTipDrag(1,:) - TipMass(1)*( Gravity*z2 + LinAccES(1,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
   DO J = ( BldGagNd(1) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
      CALL CrossProd( TmpVec, rS0S(1,J,:) - rS0S(1,BldGagNd(1),:), &                ! Portion of MomMGagB associated with element J
                      FSAero(1,J,:) - MassB(1,J)*( Gravity*z2 + LinAccES(1,J,:) ) )
      MomMGagB = MomMGagB + ( TmpVec + MMAero(1,J,:) )*DRNodes(J)
   ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
   CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(1)) )*j3(1,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                   FSAero(1,BldGagNd(1),:) - MassB(1,BldGagNd(1))* &
                      ( Gravity*z2 + LinAccES(1,BldGagNd(1),:) )     )
   MomMGagB = MomMGagB + ( TmpVec + MMAero(1,BldGagNd(1),:) )* &
                            ( 0.5 *DRNodes(BldGagNd(1)) )
   MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

   AllOuts(Spn1MLxb1) = DOT_PRODUCT( MomMGagB, n1(1,BldGagNd(1),:) )
   AllOuts(Spn1MLyb1) = DOT_PRODUCT( MomMGagB, n2(1,BldGagNd(1),:) )
   AllOuts(Spn1MLzb1) = DOT_PRODUCT( MomMGagB, n3(1,BldGagNd(1),:) )


   IF ( NBlGages >= 2 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
      CALL CrossProd( MomMGagB, rS0S(1,TipNode,:) - rS0S(1,BldGagNd(2),:), &
                      FSTipDrag(1,:) - TipMass(1)*( Gravity*z2 + LinAccES(1,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
      DO J = ( BldGagNd(2) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
         CALL CrossProd( TmpVec, rS0S(1,J,:) - rS0S(1,BldGagNd(2),:), &                ! Portion of MomMGagB associated with element J
                         FSAero(1,J,:) - MassB(1,J)*( Gravity*z2 + LinAccES(1,J,:) ) )
         MomMGagB = MomMGagB + ( TmpVec + MMAero(1,J,:) )*DRNodes(J)
      ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
      CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(2)) )*j3(1,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                      FSAero(1,BldGagNd(2),:) - MassB(1,BldGagNd(2))* &
                         ( Gravity*z2 + LinAccES(1,BldGagNd(2),:) )     )
      MomMGagB = MomMGagB + ( TmpVec + MMAero(1,BldGagNd(2),:) )* &
                               ( 0.5 *DRNodes(BldGagNd(2)) )
      MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

      AllOuts(Spn2MLxb1) = DOT_PRODUCT( MomMGagB, n1(1,BldGagNd(2),:) )
      AllOuts(Spn2MLyb1) = DOT_PRODUCT( MomMGagB, n2(1,BldGagNd(2),:) )
      AllOuts(Spn2MLzb1) = DOT_PRODUCT( MomMGagB, n3(1,BldGagNd(2),:) )


      IF ( NBlGages >= 3 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
         CALL CrossProd( MomMGagB, rS0S(1,TipNode,:) - rS0S(1,BldGagNd(3),:), &
                         FSTipDrag(1,:) - TipMass(1)*( Gravity*z2 + LinAccES(1,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
         DO J = ( BldGagNd(3) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
            CALL CrossProd( TmpVec, rS0S(1,J,:) - rS0S(1,BldGagNd(3),:), &                ! Portion of MomMGagB associated with element J
                            FSAero(1,J,:) - MassB(1,J)*( Gravity*z2 + LinAccES(1,J,:) ) )
            MomMGagB = MomMGagB + ( TmpVec + MMAero(1,J,:) )*DRNodes(J)
         ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
         CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(3)) )*j3(1,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                         FSAero(1,BldGagNd(3),:) - MassB(1,BldGagNd(3))* &
                            ( Gravity*z2 + LinAccES(1,BldGagNd(3),:) )     )
         MomMGagB = MomMGagB + ( TmpVec + MMAero(1,BldGagNd(3),:) )* &
                                  ( 0.5 *DRNodes(BldGagNd(3)) )
         MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

         AllOuts(Spn3MLxb1) = DOT_PRODUCT( MomMGagB, n1(1,BldGagNd(3),:) )
         AllOuts(Spn3MLyb1) = DOT_PRODUCT( MomMGagB, n2(1,BldGagNd(3),:) )
         AllOuts(Spn3MLzb1) = DOT_PRODUCT( MomMGagB, n3(1,BldGagNd(3),:) )


         IF ( NBlGages >= 4 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
            CALL CrossProd( MomMGagB, rS0S(1,TipNode,:) - rS0S(1,BldGagNd(4),:), &
                            FSTipDrag(1,:) - TipMass(1)*( Gravity*z2 + LinAccES(1,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
            DO J = ( BldGagNd(4) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
               CALL CrossProd( TmpVec, rS0S(1,J,:) - rS0S(1,BldGagNd(4),:), &                ! Portion of MomMGagB associated with element J
                               FSAero(1,J,:) - MassB(1,J)*( Gravity*z2 + LinAccES(1,J,:) ) )
               MomMGagB = MomMGagB + ( TmpVec + MMAero(1,J,:) )*DRNodes(J)
            ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
            CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(4)) )*j3(1,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                            FSAero(1,BldGagNd(4),:) - MassB(1,BldGagNd(4))* &
                               ( Gravity*z2 + LinAccES(1,BldGagNd(4),:) )     )
            MomMGagB = MomMGagB + ( TmpVec + MMAero(1,BldGagNd(4),:) )* &
                                     ( 0.5 *DRNodes(BldGagNd(4)) )
            MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

            AllOuts(Spn4MLxb1) = DOT_PRODUCT( MomMGagB, n1(1,BldGagNd(4),:) )
            AllOuts(Spn4MLyb1) = DOT_PRODUCT( MomMGagB, n2(1,BldGagNd(4),:) )
            AllOuts(Spn4MLzb1) = DOT_PRODUCT( MomMGagB, n3(1,BldGagNd(4),:) )


!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:
!remove6.02a            IF ( NBlGages == 5 )  THEN
            IF ( NBlGages >= 5 )  THEN
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

   ! Initialize MomMGagB using the tip brake effects:
               CALL CrossProd( MomMGagB, rS0S(1,TipNode,:) - rS0S(1,BldGagNd(5),:), &
                               FSTipDrag(1,:) - TipMass(1)*( Gravity*z2 + LinAccES(1,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
               DO J = ( BldGagNd(5) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                  CALL CrossProd( TmpVec, rS0S(1,J,:) - rS0S(1,BldGagNd(5),:), &                ! Portion of MomMGagB associated with element J
                                  FSAero(1,J,:) - MassB(1,J)*( Gravity*z2 + LinAccES(1,J,:) ) )
                  MomMGagB = MomMGagB + ( TmpVec + MMAero(1,J,:) )*DRNodes(J)
               ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
               CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(5)) )*j3(1,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                               FSAero(1,BldGagNd(5),:) - MassB(1,BldGagNd(5))* &
                                  ( Gravity*z2 + LinAccES(1,BldGagNd(5),:) )     )
               MomMGagB = MomMGagB + ( TmpVec + MMAero(1,BldGagNd(5),:) )* &
                                        ( 0.5 *DRNodes(BldGagNd(5)) )
               MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

               AllOuts(Spn5MLxb1) = DOT_PRODUCT( MomMGagB, n1(1,BldGagNd(5),:) )
               AllOuts(Spn5MLyb1) = DOT_PRODUCT( MomMGagB, n2(1,BldGagNd(5),:) )
               AllOuts(Spn5MLzb1) = DOT_PRODUCT( MomMGagB, n3(1,BldGagNd(5),:) )

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:

               IF ( NBlGages >= 6 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
                  CALL CrossProd( MomMGagB, rS0S(1,TipNode,:) - rS0S(1,BldGagNd(6),:), &
                                  FSTipDrag(1,:) - TipMass(1)*( Gravity*z2 + LinAccES(1,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
                  DO J = ( BldGagNd(6) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                     CALL CrossProd( TmpVec, rS0S(1,J,:) - rS0S(1,BldGagNd(6),:), &                ! Portion of MomMGagB associated with element J
                                     FSAero(1,J,:) - MassB(1,J)*( Gravity*z2 + LinAccES(1,J,:) ) )
                     MomMGagB = MomMGagB + ( TmpVec + MMAero(1,J,:) )*DRNodes(J)
                  ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
                  CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(6)) )*j3(1,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                                  FSAero(1,BldGagNd(6),:) - MassB(1,BldGagNd(6))* &
                                     ( Gravity*z2 + LinAccES(1,BldGagNd(6),:) )     )
                  MomMGagB = MomMGagB + ( TmpVec + MMAero(1,BldGagNd(6),:) )* &
                                           ( 0.5 *DRNodes(BldGagNd(6)) )
                  MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

                  AllOuts(Spn6MLxb1) = DOT_PRODUCT( MomMGagB, n1(1,BldGagNd(6),:) )
                  AllOuts(Spn6MLyb1) = DOT_PRODUCT( MomMGagB, n2(1,BldGagNd(6),:) )
                  AllOuts(Spn6MLzb1) = DOT_PRODUCT( MomMGagB, n3(1,BldGagNd(6),:) )


                  IF ( NBlGages >= 7 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
                     CALL CrossProd( MomMGagB, rS0S(1,TipNode,:) - rS0S(1,BldGagNd(7),:), &
                                     FSTipDrag(1,:) - TipMass(1)*( Gravity*z2 + LinAccES(1,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
                     DO J = ( BldGagNd(7) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                        CALL CrossProd( TmpVec, rS0S(1,J,:) - rS0S(1,BldGagNd(7),:), &                ! Portion of MomMGagB associated with element J
                                        FSAero(1,J,:) - MassB(1,J)*( Gravity*z2 + LinAccES(1,J,:) ) )
                        MomMGagB = MomMGagB + ( TmpVec + MMAero(1,J,:) )*DRNodes(J)
                     ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
                     CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(7)) )*j3(1,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                                     FSAero(1,BldGagNd(7),:) - MassB(1,BldGagNd(7))* &
                                        ( Gravity*z2 + LinAccES(1,BldGagNd(7),:) )     )
                     MomMGagB = MomMGagB + ( TmpVec + MMAero(1,BldGagNd(7),:) )* &
                                              ( 0.5 *DRNodes(BldGagNd(7)) )
                     MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

                     AllOuts(Spn7MLxb1) = DOT_PRODUCT( MomMGagB, n1(1,BldGagNd(7),:) )
                     AllOuts(Spn7MLyb1) = DOT_PRODUCT( MomMGagB, n2(1,BldGagNd(7),:) )
                     AllOuts(Spn7MLzb1) = DOT_PRODUCT( MomMGagB, n3(1,BldGagNd(7),:) )


                     IF ( NBlGages >= 8 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
                        CALL CrossProd( MomMGagB, rS0S(1,TipNode,:) - rS0S(1,BldGagNd(8),:), &
                                        FSTipDrag(1,:) - TipMass(1)*( Gravity*z2 + LinAccES(1,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
                        DO J = ( BldGagNd(8) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                           CALL CrossProd( TmpVec, rS0S(1,J,:) - rS0S(1,BldGagNd(8),:), &                ! Portion of MomMGagB associated with element J
                                           FSAero(1,J,:) - MassB(1,J)*( Gravity*z2 + LinAccES(1,J,:) ) )
                           MomMGagB = MomMGagB + ( TmpVec + MMAero(1,J,:) )*DRNodes(J)
                        ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
                        CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(8)) )*j3(1,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                                        FSAero(1,BldGagNd(8),:) - MassB(1,BldGagNd(8))* &
                                           ( Gravity*z2 + LinAccES(1,BldGagNd(8),:) )     )
                        MomMGagB = MomMGagB + ( TmpVec + MMAero(1,BldGagNd(8),:) )* &
                                                 ( 0.5 *DRNodes(BldGagNd(8)) )
                        MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

                        AllOuts(Spn8MLxb1) = DOT_PRODUCT( MomMGagB, n1(1,BldGagNd(8),:) )
                        AllOuts(Spn8MLyb1) = DOT_PRODUCT( MomMGagB, n2(1,BldGagNd(8),:) )
                        AllOuts(Spn8MLzb1) = DOT_PRODUCT( MomMGagB, n3(1,BldGagNd(8),:) )


                        IF ( NBlGages == 9 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
                           CALL CrossProd( MomMGagB, rS0S(1,TipNode,:) - rS0S(1,BldGagNd(9),:), &
                                           FSTipDrag(1,:) - TipMass(1)*( Gravity*z2 + LinAccES(1,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
                           DO J = ( BldGagNd(9) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                              CALL CrossProd( TmpVec, rS0S(1,J,:) - rS0S(1,BldGagNd(9),:), &                ! Portion of MomMGagB associated with element J
                                              FSAero(1,J,:) - MassB(1,J)*( Gravity*z2 + LinAccES(1,J,:) ) )
                              MomMGagB = MomMGagB + ( TmpVec + MMAero(1,J,:) )*DRNodes(J)
                           ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
                           CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(9)) )*j3(1,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                                           FSAero(1,BldGagNd(9),:) - MassB(1,BldGagNd(9))* &
                                              ( Gravity*z2 + LinAccES(1,BldGagNd(9),:) )     )
                           MomMGagB = MomMGagB + ( TmpVec + MMAero(1,BldGagNd(9),:) )* &
                                                    ( 0.5 *DRNodes(BldGagNd(9)) )
                           MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

                           AllOuts(Spn9MLxb1) = DOT_PRODUCT( MomMGagB, n1(1,BldGagNd(9),:) )
                           AllOuts(Spn9MLyb1) = DOT_PRODUCT( MomMGagB, n2(1,BldGagNd(9),:) )
                           AllOuts(Spn9MLzb1) = DOT_PRODUCT( MomMGagB, n3(1,BldGagNd(9),:) )

                        ENDIF

                     ENDIF

                  ENDIF

               ENDIF

!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

            ENDIF

         ENDIF

      ENDIF

   ENDIF

ENDIF


   ! Blade 2 Root Loads:

AllOuts( RootFxc2) = DOT_PRODUCT( FrcS0B(2,:), i1(2,:) )
AllOuts( RootFyc2) = DOT_PRODUCT( FrcS0B(2,:), i2(2,:) )
AllOuts( RootFzc2) = DOT_PRODUCT( FrcS0B(2,:), i3(2,:) )
AllOuts( RootFxb2) = DOT_PRODUCT( FrcS0B(2,:), j1(2,:) )
AllOuts( RootFyb2) = DOT_PRODUCT( FrcS0B(2,:), j2(2,:) )
AllOuts( RootMxc2) = DOT_PRODUCT( MomH0B(2,:), i1(2,:) )
AllOuts( RootMyc2) = DOT_PRODUCT( MomH0B(2,:), i2(2,:) )
AllOuts( RootMzc2) = DOT_PRODUCT( MomH0B(2,:), i3(2,:) )
AllOuts( RootMxb2) = DOT_PRODUCT( MomH0B(2,:), j1(2,:) )
AllOuts( RootMyb2) = DOT_PRODUCT( MomH0B(2,:), j2(2,:) )

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Add blade strain gage output parameters for the local loads and motions of
!jmj   blades 2 and 3:


   ! Blade 2 Local Span Loads:

IF ( NBlGages >= 1 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
   CALL CrossProd( MomMGagB, rS0S(2,TipNode,:) - rS0S(2,BldGagNd(1),:), &
                   FSTipDrag(2,:) - TipMass(2)*( Gravity*z2 + LinAccES(2,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
   DO J = ( BldGagNd(1) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
      CALL CrossProd( TmpVec, rS0S(2,J,:) - rS0S(2,BldGagNd(1),:), &                ! Portion of MomMGagB associated with element J
                      FSAero(2,J,:) - MassB(2,J)*( Gravity*z2 + LinAccES(2,J,:) ) )
      MomMGagB = MomMGagB + ( TmpVec + MMAero(2,J,:) )*DRNodes(J)
   ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
   CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(1)) )*j3(2,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                   FSAero(2,BldGagNd(1),:) - MassB(2,BldGagNd(1))* &
                      ( Gravity*z2 + LinAccES(2,BldGagNd(1),:) )     )
   MomMGagB = MomMGagB + ( TmpVec + MMAero(2,BldGagNd(1),:) )* &
                            ( 0.5 *DRNodes(BldGagNd(1)) )
   MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

   AllOuts(Spn1MLxb2) = DOT_PRODUCT( MomMGagB, n1(2,BldGagNd(1),:) )
   AllOuts(Spn1MLyb2) = DOT_PRODUCT( MomMGagB, n2(2,BldGagNd(1),:) )
   AllOuts(Spn1MLzb2) = DOT_PRODUCT( MomMGagB, n3(2,BldGagNd(1),:) )


   IF ( NBlGages >= 2 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
      CALL CrossProd( MomMGagB, rS0S(2,TipNode,:) - rS0S(2,BldGagNd(2),:), &
                      FSTipDrag(2,:) - TipMass(2)*( Gravity*z2 + LinAccES(2,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
      DO J = ( BldGagNd(2) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
         CALL CrossProd( TmpVec, rS0S(2,J,:) - rS0S(2,BldGagNd(2),:), &                ! Portion of MomMGagB associated with element J
                         FSAero(2,J,:) - MassB(2,J)*( Gravity*z2 + LinAccES(2,J,:) ) )
         MomMGagB = MomMGagB + ( TmpVec + MMAero(2,J,:) )*DRNodes(J)
      ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
      CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(2)) )*j3(2,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                      FSAero(2,BldGagNd(2),:) - MassB(2,BldGagNd(2))* &
                         ( Gravity*z2 + LinAccES(2,BldGagNd(2),:) )     )
      MomMGagB = MomMGagB + ( TmpVec + MMAero(2,BldGagNd(2),:) )* &
                               ( 0.5 *DRNodes(BldGagNd(2)) )
      MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

      AllOuts(Spn2MLxb2) = DOT_PRODUCT( MomMGagB, n1(2,BldGagNd(2),:) )
      AllOuts(Spn2MLyb2) = DOT_PRODUCT( MomMGagB, n2(2,BldGagNd(2),:) )
      AllOuts(Spn2MLzb2) = DOT_PRODUCT( MomMGagB, n3(2,BldGagNd(2),:) )


      IF ( NBlGages >= 3 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
         CALL CrossProd( MomMGagB, rS0S(2,TipNode,:) - rS0S(2,BldGagNd(3),:), &
                         FSTipDrag(2,:) - TipMass(2)*( Gravity*z2 + LinAccES(2,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
         DO J = ( BldGagNd(3) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
            CALL CrossProd( TmpVec, rS0S(2,J,:) - rS0S(2,BldGagNd(3),:), &                ! Portion of MomMGagB associated with element J
                            FSAero(2,J,:) - MassB(2,J)*( Gravity*z2 + LinAccES(2,J,:) ) )
            MomMGagB = MomMGagB + ( TmpVec + MMAero(2,J,:) )*DRNodes(J)
         ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
         CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(3)) )*j3(2,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                         FSAero(2,BldGagNd(3),:) - MassB(2,BldGagNd(3))* &
                            ( Gravity*z2 + LinAccES(2,BldGagNd(3),:) )     )
         MomMGagB = MomMGagB + ( TmpVec + MMAero(2,BldGagNd(3),:) )* &
                                  ( 0.5 *DRNodes(BldGagNd(3)) )
         MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

         AllOuts(Spn3MLxb2) = DOT_PRODUCT( MomMGagB, n1(2,BldGagNd(3),:) )
         AllOuts(Spn3MLyb2) = DOT_PRODUCT( MomMGagB, n2(2,BldGagNd(3),:) )
         AllOuts(Spn3MLzb2) = DOT_PRODUCT( MomMGagB, n3(2,BldGagNd(3),:) )


         IF ( NBlGages >= 4 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
            CALL CrossProd( MomMGagB, rS0S(2,TipNode,:) - rS0S(2,BldGagNd(4),:), &
                            FSTipDrag(2,:) - TipMass(2)*( Gravity*z2 + LinAccES(2,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
            DO J = ( BldGagNd(4) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
               CALL CrossProd( TmpVec, rS0S(2,J,:) - rS0S(2,BldGagNd(4),:), &                ! Portion of MomMGagB associated with element J
                               FSAero(2,J,:) - MassB(2,J)*( Gravity*z2 + LinAccES(2,J,:) ) )
               MomMGagB = MomMGagB + ( TmpVec + MMAero(2,J,:) )*DRNodes(J)
            ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
            CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(4)) )*j3(2,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                            FSAero(2,BldGagNd(4),:) - MassB(2,BldGagNd(4))* &
                               ( Gravity*z2 + LinAccES(2,BldGagNd(4),:) )     )
            MomMGagB = MomMGagB + ( TmpVec + MMAero(2,BldGagNd(4),:) )* &
                                     ( 0.5 *DRNodes(BldGagNd(4)) )
            MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

            AllOuts(Spn4MLxb2) = DOT_PRODUCT( MomMGagB, n1(2,BldGagNd(4),:) )
            AllOuts(Spn4MLyb2) = DOT_PRODUCT( MomMGagB, n2(2,BldGagNd(4),:) )
            AllOuts(Spn4MLzb2) = DOT_PRODUCT( MomMGagB, n3(2,BldGagNd(4),:) )


            IF ( NBlGages >= 5 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
               CALL CrossProd( MomMGagB, rS0S(2,TipNode,:) - rS0S(2,BldGagNd(5),:), &
                               FSTipDrag(2,:) - TipMass(2)*( Gravity*z2 + LinAccES(2,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
               DO J = ( BldGagNd(5) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                  CALL CrossProd( TmpVec, rS0S(2,J,:) - rS0S(2,BldGagNd(5),:), &                ! Portion of MomMGagB associated with element J
                                  FSAero(2,J,:) - MassB(2,J)*( Gravity*z2 + LinAccES(2,J,:) ) )
                  MomMGagB = MomMGagB + ( TmpVec + MMAero(2,J,:) )*DRNodes(J)
               ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
               CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(5)) )*j3(2,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                               FSAero(2,BldGagNd(5),:) - MassB(2,BldGagNd(5))* &
                                  ( Gravity*z2 + LinAccES(2,BldGagNd(5),:) )     )
               MomMGagB = MomMGagB + ( TmpVec + MMAero(2,BldGagNd(5),:) )* &
                                        ( 0.5 *DRNodes(BldGagNd(5)) )
               MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

               AllOuts(Spn5MLxb2) = DOT_PRODUCT( MomMGagB, n1(2,BldGagNd(5),:) )
               AllOuts(Spn5MLyb2) = DOT_PRODUCT( MomMGagB, n2(2,BldGagNd(5),:) )
               AllOuts(Spn5MLzb2) = DOT_PRODUCT( MomMGagB, n3(2,BldGagNd(5),:) )


               IF ( NBlGages >= 6 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
                  CALL CrossProd( MomMGagB, rS0S(2,TipNode,:) - rS0S(2,BldGagNd(6),:), &
                                  FSTipDrag(2,:) - TipMass(2)*( Gravity*z2 + LinAccES(2,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
                  DO J = ( BldGagNd(6) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                     CALL CrossProd( TmpVec, rS0S(2,J,:) - rS0S(2,BldGagNd(6),:), &                ! Portion of MomMGagB associated with element J
                                     FSAero(2,J,:) - MassB(2,J)*( Gravity*z2 + LinAccES(2,J,:) ) )
                     MomMGagB = MomMGagB + ( TmpVec + MMAero(2,J,:) )*DRNodes(J)
                  ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
                  CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(6)) )*j3(2,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                                  FSAero(2,BldGagNd(6),:) - MassB(2,BldGagNd(6))* &
                                     ( Gravity*z2 + LinAccES(2,BldGagNd(6),:) )     )
                  MomMGagB = MomMGagB + ( TmpVec + MMAero(2,BldGagNd(6),:) )* &
                                           ( 0.5 *DRNodes(BldGagNd(6)) )
                  MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

                  AllOuts(Spn6MLxb2) = DOT_PRODUCT( MomMGagB, n1(2,BldGagNd(6),:) )
                  AllOuts(Spn6MLyb2) = DOT_PRODUCT( MomMGagB, n2(2,BldGagNd(6),:) )
                  AllOuts(Spn6MLzb2) = DOT_PRODUCT( MomMGagB, n3(2,BldGagNd(6),:) )


                  IF ( NBlGages >= 7 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
                     CALL CrossProd( MomMGagB, rS0S(2,TipNode,:) - rS0S(2,BldGagNd(7),:), &
                                     FSTipDrag(2,:) - TipMass(2)*( Gravity*z2 + LinAccES(2,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
                     DO J = ( BldGagNd(7) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                        CALL CrossProd( TmpVec, rS0S(2,J,:) - rS0S(2,BldGagNd(7),:), &                ! Portion of MomMGagB associated with element J
                                        FSAero(2,J,:) - MassB(2,J)*( Gravity*z2 + LinAccES(2,J,:) ) )
                        MomMGagB = MomMGagB + ( TmpVec + MMAero(2,J,:) )*DRNodes(J)
                     ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
                     CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(7)) )*j3(2,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                                     FSAero(2,BldGagNd(7),:) - MassB(2,BldGagNd(7))* &
                                        ( Gravity*z2 + LinAccES(2,BldGagNd(7),:) )     )
                     MomMGagB = MomMGagB + ( TmpVec + MMAero(2,BldGagNd(7),:) )* &
                                              ( 0.5 *DRNodes(BldGagNd(7)) )
                     MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

                     AllOuts(Spn7MLxb2) = DOT_PRODUCT( MomMGagB, n1(2,BldGagNd(7),:) )
                     AllOuts(Spn7MLyb2) = DOT_PRODUCT( MomMGagB, n2(2,BldGagNd(7),:) )
                     AllOuts(Spn7MLzb2) = DOT_PRODUCT( MomMGagB, n3(2,BldGagNd(7),:) )


                     IF ( NBlGages >= 8 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
                        CALL CrossProd( MomMGagB, rS0S(2,TipNode,:) - rS0S(2,BldGagNd(8),:), &
                                        FSTipDrag(2,:) - TipMass(2)*( Gravity*z2 + LinAccES(2,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
                        DO J = ( BldGagNd(8) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                           CALL CrossProd( TmpVec, rS0S(2,J,:) - rS0S(2,BldGagNd(8),:), &                ! Portion of MomMGagB associated with element J
                                           FSAero(2,J,:) - MassB(2,J)*( Gravity*z2 + LinAccES(2,J,:) ) )
                           MomMGagB = MomMGagB + ( TmpVec + MMAero(2,J,:) )*DRNodes(J)
                        ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
                        CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(8)) )*j3(2,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                                        FSAero(2,BldGagNd(8),:) - MassB(2,BldGagNd(8))* &
                                           ( Gravity*z2 + LinAccES(2,BldGagNd(8),:) )     )
                        MomMGagB = MomMGagB + ( TmpVec + MMAero(2,BldGagNd(8),:) )* &
                                                 ( 0.5 *DRNodes(BldGagNd(8)) )
                        MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

                        AllOuts(Spn8MLxb2) = DOT_PRODUCT( MomMGagB, n1(2,BldGagNd(8),:) )
                        AllOuts(Spn8MLyb2) = DOT_PRODUCT( MomMGagB, n2(2,BldGagNd(8),:) )
                        AllOuts(Spn8MLzb2) = DOT_PRODUCT( MomMGagB, n3(2,BldGagNd(8),:) )


                        IF ( NBlGages == 9 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
                           CALL CrossProd( MomMGagB, rS0S(2,TipNode,:) - rS0S(2,BldGagNd(9),:), &
                                           FSTipDrag(2,:) - TipMass(2)*( Gravity*z2 + LinAccES(2,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
                           DO J = ( BldGagNd(9) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                              CALL CrossProd( TmpVec, rS0S(2,J,:) - rS0S(2,BldGagNd(9),:), &                ! Portion of MomMGagB associated with element J
                                              FSAero(2,J,:) - MassB(2,J)*( Gravity*z2 + LinAccES(2,J,:) ) )
                              MomMGagB = MomMGagB + ( TmpVec + MMAero(2,J,:) )*DRNodes(J)
                           ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
                           CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(9)) )*j3(2,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                                           FSAero(2,BldGagNd(9),:) - MassB(2,BldGagNd(9))* &
                                              ( Gravity*z2 + LinAccES(2,BldGagNd(9),:) )     )
                           MomMGagB = MomMGagB + ( TmpVec + MMAero(2,BldGagNd(9),:) )* &
                                                    ( 0.5 *DRNodes(BldGagNd(9)) )
                           MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

                           AllOuts(Spn9MLxb2) = DOT_PRODUCT( MomMGagB, n1(2,BldGagNd(9),:) )
                           AllOuts(Spn9MLyb2) = DOT_PRODUCT( MomMGagB, n2(2,BldGagNd(9),:) )
                           AllOuts(Spn9MLzb2) = DOT_PRODUCT( MomMGagB, n3(2,BldGagNd(9),:) )

                        ENDIF

                     ENDIF

                  ENDIF

               ENDIF

            ENDIF

         ENDIF

      ENDIF

   ENDIF

ENDIF
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

IF ( NumBl == 3 )  THEN ! 3-blader

   ! Blade 3 Root Loads:

   AllOuts( RootFxc3) = DOT_PRODUCT( FrcS0B(3,:), i1(3,:) )
   AllOuts( RootFyc3) = DOT_PRODUCT( FrcS0B(3,:), i2(3,:) )
   AllOuts( RootFzc3) = DOT_PRODUCT( FrcS0B(3,:), i3(3,:) )
   AllOuts( RootFxb3) = DOT_PRODUCT( FrcS0B(3,:), j1(3,:) )
   AllOuts( RootFyb3) = DOT_PRODUCT( FrcS0B(3,:), j2(3,:) )
   AllOuts( RootMxc3) = DOT_PRODUCT( MomH0B(3,:), i1(3,:) )
   AllOuts( RootMyc3) = DOT_PRODUCT( MomH0B(3,:), i2(3,:) )
   AllOuts( RootMzc3) = DOT_PRODUCT( MomH0B(3,:), i3(3,:) )
   AllOuts( RootMxb3) = DOT_PRODUCT( MomH0B(3,:), j1(3,:) )
   AllOuts( RootMyb3) = DOT_PRODUCT( MomH0B(3,:), j2(3,:) )

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Add blade strain gage output parameters for the local loads and motions of
!jmj   blades 2 and 3:


   ! Blade 3 Local Span Loads:

   IF ( NBlGages >= 1 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
      CALL CrossProd( MomMGagB, rS0S(3,TipNode,:) - rS0S(3,BldGagNd(1),:), &
                      FSTipDrag(3,:) - TipMass(3)*( Gravity*z2 + LinAccES(3,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
      DO J = ( BldGagNd(1) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
         CALL CrossProd( TmpVec, rS0S(3,J,:) - rS0S(3,BldGagNd(1),:), &                ! Portion of MomMGagB associated with element J
                         FSAero(3,J,:) - MassB(3,J)*( Gravity*z2 + LinAccES(3,J,:) ) )
         MomMGagB = MomMGagB + ( TmpVec + MMAero(3,J,:) )*DRNodes(J)
      ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
      CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(1)) )*j3(3,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                      FSAero(3,BldGagNd(1),:) - MassB(3,BldGagNd(1))* &
                         ( Gravity*z2 + LinAccES(3,BldGagNd(1),:) )     )
      MomMGagB = MomMGagB + ( TmpVec + MMAero(3,BldGagNd(1),:) )* &
                               ( 0.5 *DRNodes(BldGagNd(1)) )
      MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

      AllOuts(Spn1MLxb3) = DOT_PRODUCT( MomMGagB, n1(3,BldGagNd(1),:) )
      AllOuts(Spn1MLyb3) = DOT_PRODUCT( MomMGagB, n2(3,BldGagNd(1),:) )
      AllOuts(Spn1MLzb3) = DOT_PRODUCT( MomMGagB, n3(3,BldGagNd(1),:) )


      IF ( NBlGages >= 2 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
         CALL CrossProd( MomMGagB, rS0S(3,TipNode,:) - rS0S(3,BldGagNd(2),:), &
                         FSTipDrag(3,:) - TipMass(3)*( Gravity*z2 + LinAccES(3,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
         DO J = ( BldGagNd(2) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
            CALL CrossProd( TmpVec, rS0S(3,J,:) - rS0S(3,BldGagNd(2),:), &                ! Portion of MomMGagB associated with element J
                            FSAero(3,J,:) - MassB(3,J)*( Gravity*z2 + LinAccES(3,J,:) ) )
            MomMGagB = MomMGagB + ( TmpVec + MMAero(3,J,:) )*DRNodes(J)
         ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
         CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(2)) )*j3(3,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                         FSAero(3,BldGagNd(2),:) - MassB(3,BldGagNd(2))* &
                            ( Gravity*z2 + LinAccES(3,BldGagNd(2),:) )     )
         MomMGagB = MomMGagB + ( TmpVec + MMAero(3,BldGagNd(2),:) )* &
                                  ( 0.5 *DRNodes(BldGagNd(2)) )
         MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

         AllOuts(Spn2MLxb3) = DOT_PRODUCT( MomMGagB, n1(3,BldGagNd(2),:) )
         AllOuts(Spn2MLyb3) = DOT_PRODUCT( MomMGagB, n2(3,BldGagNd(2),:) )
         AllOuts(Spn2MLzb3) = DOT_PRODUCT( MomMGagB, n3(3,BldGagNd(2),:) )


         IF ( NBlGages >= 3 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
            CALL CrossProd( MomMGagB, rS0S(3,TipNode,:) - rS0S(3,BldGagNd(3),:), &
                            FSTipDrag(3,:) - TipMass(3)*( Gravity*z2 + LinAccES(3,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
            DO J = ( BldGagNd(3) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
               CALL CrossProd( TmpVec, rS0S(3,J,:) - rS0S(3,BldGagNd(3),:), &                ! Portion of MomMGagB associated with element J
                               FSAero(3,J,:) - MassB(3,J)*( Gravity*z2 + LinAccES(3,J,:) ) )
               MomMGagB = MomMGagB + ( TmpVec + MMAero(3,J,:) )*DRNodes(J)
            ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
            CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(3)) )*j3(3,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                            FSAero(3,BldGagNd(3),:) - MassB(3,BldGagNd(3))* &
                               ( Gravity*z2 + LinAccES(3,BldGagNd(3),:) )     )
            MomMGagB = MomMGagB + ( TmpVec + MMAero(3,BldGagNd(3),:) )* &
                                     ( 0.5 *DRNodes(BldGagNd(3)) )
            MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

            AllOuts(Spn3MLxb3) = DOT_PRODUCT( MomMGagB, n1(3,BldGagNd(3),:) )
            AllOuts(Spn3MLyb3) = DOT_PRODUCT( MomMGagB, n2(3,BldGagNd(3),:) )
            AllOuts(Spn3MLzb3) = DOT_PRODUCT( MomMGagB, n3(3,BldGagNd(3),:) )


            IF ( NBlGages >= 4 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
               CALL CrossProd( MomMGagB, rS0S(3,TipNode,:) - rS0S(3,BldGagNd(4),:), &
                               FSTipDrag(3,:) - TipMass(3)*( Gravity*z2 + LinAccES(3,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
               DO J = ( BldGagNd(4) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                  CALL CrossProd( TmpVec, rS0S(3,J,:) - rS0S(3,BldGagNd(4),:), &                ! Portion of MomMGagB associated with element J
                                  FSAero(3,J,:) - MassB(3,J)*( Gravity*z2 + LinAccES(3,J,:) ) )
                  MomMGagB = MomMGagB + ( TmpVec + MMAero(3,J,:) )*DRNodes(J)
               ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
               CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(4)) )*j3(3,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                               FSAero(3,BldGagNd(4),:) - MassB(3,BldGagNd(4))* &
                                  ( Gravity*z2 + LinAccES(3,BldGagNd(4),:) )     )
               MomMGagB = MomMGagB + ( TmpVec + MMAero(3,BldGagNd(4),:) )* &
                                        ( 0.5 *DRNodes(BldGagNd(4)) )
               MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

               AllOuts(Spn4MLxb3) = DOT_PRODUCT( MomMGagB, n1(3,BldGagNd(4),:) )
               AllOuts(Spn4MLyb3) = DOT_PRODUCT( MomMGagB, n2(3,BldGagNd(4),:) )
               AllOuts(Spn4MLzb3) = DOT_PRODUCT( MomMGagB, n3(3,BldGagNd(4),:) )


               IF ( NBlGages >= 5 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
                  CALL CrossProd( MomMGagB, rS0S(3,TipNode,:) - rS0S(3,BldGagNd(5),:), &
                                  FSTipDrag(3,:) - TipMass(3)*( Gravity*z2 + LinAccES(3,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
                  DO J = ( BldGagNd(5) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                     CALL CrossProd( TmpVec, rS0S(3,J,:) - rS0S(3,BldGagNd(5),:), &                ! Portion of MomMGagB associated with element J
                                     FSAero(3,J,:) - MassB(3,J)*( Gravity*z2 + LinAccES(3,J,:) ) )
                     MomMGagB = MomMGagB + ( TmpVec + MMAero(3,J,:) )*DRNodes(J)
                  ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
                  CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(5)) )*j3(3,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                                  FSAero(3,BldGagNd(5),:) - MassB(3,BldGagNd(5))* &
                                     ( Gravity*z2 + LinAccES(3,BldGagNd(5),:) )     )
                  MomMGagB = MomMGagB + ( TmpVec + MMAero(3,BldGagNd(5),:) )* &
                                           ( 0.5 *DRNodes(BldGagNd(5)) )
                  MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

                  AllOuts(Spn5MLxb3) = DOT_PRODUCT( MomMGagB, n1(3,BldGagNd(5),:) )
                  AllOuts(Spn5MLyb3) = DOT_PRODUCT( MomMGagB, n2(3,BldGagNd(5),:) )
                  AllOuts(Spn5MLzb3) = DOT_PRODUCT( MomMGagB, n3(3,BldGagNd(5),:) )


                  IF ( NBlGages >= 6 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
                     CALL CrossProd( MomMGagB, rS0S(3,TipNode,:) - rS0S(3,BldGagNd(6),:), &
                                     FSTipDrag(3,:) - TipMass(3)*( Gravity*z2 + LinAccES(3,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
                     DO J = ( BldGagNd(6) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                        CALL CrossProd( TmpVec, rS0S(3,J,:) - rS0S(3,BldGagNd(6),:), &                ! Portion of MomMGagB associated with element J
                                        FSAero(3,J,:) - MassB(3,J)*( Gravity*z2 + LinAccES(3,J,:) ) )
                        MomMGagB = MomMGagB + ( TmpVec + MMAero(3,J,:) )*DRNodes(J)
                     ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
                     CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(6)) )*j3(3,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                                     FSAero(3,BldGagNd(6),:) - MassB(3,BldGagNd(6))* &
                                        ( Gravity*z2 + LinAccES(3,BldGagNd(6),:) )     )
                     MomMGagB = MomMGagB + ( TmpVec + MMAero(3,BldGagNd(6),:) )* &
                                              ( 0.5 *DRNodes(BldGagNd(6)) )
                     MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

                     AllOuts(Spn6MLxb3) = DOT_PRODUCT( MomMGagB, n1(3,BldGagNd(6),:) )
                     AllOuts(Spn6MLyb3) = DOT_PRODUCT( MomMGagB, n2(3,BldGagNd(6),:) )
                     AllOuts(Spn6MLzb3) = DOT_PRODUCT( MomMGagB, n3(3,BldGagNd(6),:) )


                     IF ( NBlGages >= 7 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
                        CALL CrossProd( MomMGagB, rS0S(3,TipNode,:) - rS0S(3,BldGagNd(7),:), &
                                        FSTipDrag(3,:) - TipMass(3)*( Gravity*z2 + LinAccES(3,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
                        DO J = ( BldGagNd(7) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                           CALL CrossProd( TmpVec, rS0S(3,J,:) - rS0S(3,BldGagNd(7),:), &                ! Portion of MomMGagB associated with element J
                                           FSAero(3,J,:) - MassB(3,J)*( Gravity*z2 + LinAccES(3,J,:) ) )
                           MomMGagB = MomMGagB + ( TmpVec + MMAero(3,J,:) )*DRNodes(J)
                        ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
                        CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(7)) )*j3(3,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                                        FSAero(3,BldGagNd(7),:) - MassB(3,BldGagNd(7))* &
                                           ( Gravity*z2 + LinAccES(3,BldGagNd(7),:) )     )
                        MomMGagB = MomMGagB + ( TmpVec + MMAero(3,BldGagNd(7),:) )* &
                                                 ( 0.5 *DRNodes(BldGagNd(7)) )
                        MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

                        AllOuts(Spn7MLxb3) = DOT_PRODUCT( MomMGagB, n1(3,BldGagNd(7),:) )
                        AllOuts(Spn7MLyb3) = DOT_PRODUCT( MomMGagB, n2(3,BldGagNd(7),:) )
                        AllOuts(Spn7MLzb3) = DOT_PRODUCT( MomMGagB, n3(3,BldGagNd(7),:) )


                        IF ( NBlGages >= 8 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
                           CALL CrossProd( MomMGagB, rS0S(3,TipNode,:) - rS0S(3,BldGagNd(8),:), &
                                           FSTipDrag(3,:) - TipMass(3)*( Gravity*z2 + LinAccES(3,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
                           DO J = ( BldGagNd(8) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                              CALL CrossProd( TmpVec, rS0S(3,J,:) - rS0S(3,BldGagNd(8),:), &                ! Portion of MomMGagB associated with element J
                                              FSAero(3,J,:) - MassB(3,J)*( Gravity*z2 + LinAccES(3,J,:) ) )
                              MomMGagB = MomMGagB + ( TmpVec + MMAero(3,J,:) )*DRNodes(J)
                           ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
                           CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(8)) )*j3(3,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                                           FSAero(3,BldGagNd(8),:) - MassB(3,BldGagNd(8))* &
                                              ( Gravity*z2 + LinAccES(3,BldGagNd(8),:) )     )
                           MomMGagB = MomMGagB + ( TmpVec + MMAero(3,BldGagNd(8),:) )* &
                                                    ( 0.5 *DRNodes(BldGagNd(8)) )
                           MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

                           AllOuts(Spn8MLxb3) = DOT_PRODUCT( MomMGagB, n1(3,BldGagNd(8),:) )
                           AllOuts(Spn8MLyb3) = DOT_PRODUCT( MomMGagB, n2(3,BldGagNd(8),:) )
                           AllOuts(Spn8MLzb3) = DOT_PRODUCT( MomMGagB, n3(3,BldGagNd(8),:) )


                           IF ( NBlGages == 9 )  THEN

   ! Initialize MomMGagB using the tip brake effects:
                              CALL CrossProd( MomMGagB, rS0S(3,TipNode,:) - rS0S(3,BldGagNd(9),:), &
                                              FSTipDrag(3,:) - TipMass(3)*( Gravity*z2 + LinAccES(3,TipNode,:) ) )

   ! Integrate to find MomMGagB using all of the nodes / elements above
   !   the current strain gage location:
                              DO J = ( BldGagNd(9) + 1 ),BldNodes ! Loop through blade nodes / elements above strain gage node
                                 CALL CrossProd( TmpVec, rS0S(3,J,:) - rS0S(3,BldGagNd(9),:), &                ! Portion of MomMGagB associated with element J
                                                 FSAero(3,J,:) - MassB(3,J)*( Gravity*z2 + LinAccES(3,J,:) ) )
                                 MomMGagB = MomMGagB + ( TmpVec + MMAero(3,J,:) )*DRNodes(J)
                              ENDDO ! J - Blade nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to blade bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DRNodes() and the element
   !   length is 1/2 of DRNodes().
                              CALL CrossProd( TmpVec, ( 0.25*DRNodes(BldGagNd(9)) )*j3(3,:),  &    ! Portion of MomMGagB associated with 1/2 of the strain gage element
                                              FSAero(3,BldGagNd(9),:) - MassB(3,BldGagNd(9))* &
                                                 ( Gravity*z2 + LinAccES(3,BldGagNd(9),:) )     )
                              MomMGagB = MomMGagB + ( TmpVec + MMAero(3,BldGagNd(9),:) )* &
                                                       ( 0.5 *DRNodes(BldGagNd(9)) )
                              MomMGagB = 0.001*MomMGagB  ! Convert the local moment to kN-m

                              AllOuts(Spn9MLxb3) = DOT_PRODUCT( MomMGagB, n1(3,BldGagNd(9),:) )
                              AllOuts(Spn9MLyb3) = DOT_PRODUCT( MomMGagB, n2(3,BldGagNd(9),:) )
                              AllOuts(Spn9MLzb3) = DOT_PRODUCT( MomMGagB, n3(3,BldGagNd(9),:) )

                           ENDIF

                        ENDIF

                     ENDIF

                  ENDIF

               ENDIF

            ENDIF

         ENDIF

      ENDIF

   ENDIF
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.


ENDIF


   ! Hub and Rotor Loads:

ComDenom = 0.5*AirDens*ProjArea*AllOuts(  WindVxi)*AllOuts(  WindVxi)   ! Common denominator used in several expressions

AllOuts(LSShftFxa) =  DOT_PRODUCT(  FrcPRot, e1 )
AllOuts(LSShftFya) =  DOT_PRODUCT(  FrcPRot, e2 )
AllOuts(LSShftFza) =  DOT_PRODUCT(  FrcPRot, e3 )
AllOuts(LSShftFys) = -DOT_PRODUCT(  FrcPRot, c3 )
AllOuts(LSShftFzs) =  DOT_PRODUCT(  FrcPRot, c2 )
AllOuts(LSShftMxa) =  DOT_PRODUCT( MomLPRot, e1 )
AllOuts(LSSTipMya) =  DOT_PRODUCT( MomLPRot, e2 )
AllOuts(LSSTipMza) =  DOT_PRODUCT( MomLPRot, e3 )
AllOuts(LSSTipMys) = -DOT_PRODUCT( MomLPRot, c3 )
AllOuts(LSSTipMzs) =  DOT_PRODUCT( MomLPRot, c2 )
IF ( AllOuts(LSShftFxa) /= 0.0 )  THEN ! .TRUE. if the denominator in the following equations is not zero.

   CThrstys = -AllOuts(LSSTipMzs)/AllOuts(LSShftFxa)  ! Estimate of the ys-location of the center of thrust
   CThrstzs =  AllOuts(LSSTipMys)/AllOuts(LSShftFxa)  ! Estimate of the zs-location of the center of thrust

   IF ( IgnoreMOD )  THEN  ! Don't use MOD when computing AllOuts(CThrstAzm) -- IgnoreMOD is needed when computing CMat for azimuth measurements during FAST linearization.
      AllOuts(CThrstAzm) =      ATAN2( -CThrstzs, -CThrstys )*R2D + 360.0 + AzimB1Up + 90.0
   ELSE                    ! Do    use MOD when computing AllOuts(CThrstAzm)
      AllOuts(CThrstAzm) = MOD( ATAN2( -CThrstzs, -CThrstys )*R2D + 360.0 + AzimB1Up + 90.0, 360.0 )
   ENDIF
   AllOuts(   CThrstRad) = MIN( 1.0, SQRT( CThrstys*CThrstys + CThrstzs*CThrstzs )/AvgNrmTpRd )

ENDIF
AllOuts(   RotPwr) = ( QDT(DOF_GeAz) + QDT(DOF_DrTr) )*AllOuts(LSShftMxa)
IF ( ComDenom /= 0.0 )  THEN  ! .TRUE. if the denominator in the following equations is not zero.

   AllOuts( RotCq) = 1000.0*AllOuts(LSShftMxa) / ( ComDenom*TipRad )
   AllOuts( RotCp) = 1000.0*AllOuts(   RotPwr) / ( ComDenom*AllOuts(  WindVxi) )
   AllOuts( RotCt) = 1000.0*AllOuts(LSShftFxa) /   ComDenom

ENDIF


   ! Shaft Strain Gage Loads:

AllOuts(LSSGagMya) = AllOuts(LSSTipMya) + ShftGagL*AllOuts(LSShftFza)
AllOuts(LSSGagMza) = AllOuts(LSSTipMza) - ShftGagL*AllOuts(LSShftFya)
AllOuts(LSSGagMys) = AllOuts(LSSTipMys) + ShftGagL*AllOuts(LSShftFzs)
AllOuts(LSSGagMzs) = AllOuts(LSSTipMzs) - ShftGagL*AllOuts(LSShftFys)


   ! Generator and High-Speed Shaft Loads:

AllOuts( HSShftTq) = AllOuts(LSShftMxa)*GBoxEffFac/GBRatio
AllOuts(HSShftPwr) = AllOuts( HSShftTq)*GBRatio*QDT(DOF_GeAz)
AllOuts(  HSSBrTq) = 0.001*HSSBrTrq
AllOuts(    GenTq) = 0.001*GenTrq
AllOuts(   GenPwr) = 0.001*ElecPwr
IF ( ComDenom /= 0.0 )  THEN  ! .TRUE. if the denominator in the following equations is not zero (ComDenom is the same as it is calculated above).

   AllOuts( HSShftCq) = 1000.0*AllOuts( HSShftTq) / ( ComDenom*TipRad )
   AllOuts( HSShftCp) = 1000.0*AllOuts(HSShftPwr) / ( ComDenom*AllOuts(  WindVxi) )
   AllOuts(    GenCq) = 1000.0*AllOuts(    GenTq) / ( ComDenom*TipRad )
   AllOuts(    GenCp) = 1000.0*AllOuts(   GenPwr) / ( ComDenom*AllOuts(  WindVxi) )

ENDIF


   ! Rotor-Furl Axis Loads:

AllOuts(RFrlBrM  ) =  DOT_PRODUCT( MomNGnRt, rfa )


   ! Tail-Furl Axis Loads:

AllOuts(TFrlBrM  ) =  DOT_PRODUCT( MomNTail, tfa )


   ! Tower-Top / Yaw Bearing Loads:

AllOuts( YawBrFxn) =  DOT_PRODUCT( FrcONcRt, d1 )
AllOuts( YawBrFyn) = -DOT_PRODUCT( FrcONcRt, d3 )
AllOuts( YawBrFzn) =  DOT_PRODUCT( FrcONcRt, d2 )
AllOuts( YawBrFxp) =  DOT_PRODUCT( FrcONcRt, b1 )
AllOuts( YawBrFyp) = -DOT_PRODUCT( FrcONcRt, b3 )
AllOuts( YawBrMxn) =  DOT_PRODUCT( MomBNcRt, d1 )
AllOuts( YawBrMyn) = -DOT_PRODUCT( MomBNcRt, d3 )
AllOuts( YawBrMzn) =  DOT_PRODUCT( MomBNcRt, d2 )
AllOuts( YawBrMxp) =  DOT_PRODUCT( MomBNcRt, b1 )
AllOuts( YawBrMyp) = -DOT_PRODUCT( MomBNcRt, b3 )


   ! Tower Base Loads:

AllOuts( TwrBsFxt) =  DOT_PRODUCT( FrcT0Trb, a1 )
AllOuts( TwrBsFyt) = -DOT_PRODUCT( FrcT0Trb, a3 )
AllOuts( TwrBsFzt) =  DOT_PRODUCT( FrcT0Trb, a2 )
AllOuts( TwrBsMxt) =  DOT_PRODUCT( MomX0Trb, a1 )
AllOuts( TwrBsMyt) = -DOT_PRODUCT( MomX0Trb, a3 )
AllOuts( TwrBsMzt) =  DOT_PRODUCT( MomX0Trb, a2 )


   ! Local Tower Loads:

FrcONcRt = 1000.0*FrcONcRt ! Convert the units of these forces and moments
MomBNcRt = 1000.0*MomBNcRt ! from kN and kN-m back to N and N-m, respectively.

IF ( NTwGages >= 1 )  THEN

   ! Initialize MomFGagT using the tower-top and yaw bearing mass effects:
   CALL CrossProd( MomFGagT, rZO - rZT(TwrGagNd(1),:), &
                   FrcONcRt - YawBrMass*( Gravity*z2 + LinAccEO ) )
   MomFGagT = MomFGagT + MomBNcRt

   ! Integrate to find MomFGagT using all of the nodes / elements above
   !   the current strain gage location:
   DO J = ( TwrGagNd(1) + 1 ),TwrNodes ! Loop through tower nodes / elements above strain gage node
      CALL CrossProd( TmpVec, rZT(J,:) - rZT(TwrGagNd(1),:), &                               ! Portion of MomFGagT associated with element J
                      FTAero(J,:) + FTHydro(J,:) - MassT(J)*( Gravity*z2 + LinAccET(J,:) ) )
      MomFGagT = MomFGagT + ( TmpVec + MFAero(J,:) + MFHydro(J,:) )*DHNodes(J)
   ENDDO ! J -Tower nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to tower bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DHNodes() and the element
   !   length is 1/2 of DHNodes().
   CALL CrossProd( TmpVec, ( 0.25*DHNodes( TwrGagNd(1)) )*a2,      &                ! Portion of MomFGagT associated with 1/2 of the strain gage element
                   FTAero( TwrGagNd(1),:) + FTHydro(TwrGagNd(1),:) &
                   - MassT(TwrGagNd(1))*( Gravity*z2 + LinAccET(TwrGagNd(1),:) ) )
   MomFGagT = MomFGagT + ( TmpVec + MFAero(TwrGagNd(1),:) + MFHydro(TwrGagNd(1),:) )*&
                           ( 0.5 *DHNodes( TwrGagNd(1)) )
   MomFGagT = 0.001*MomFGagT  ! Convert the local moment to kN-m

   AllOuts(TwHt1MLxt) =  DOT_PRODUCT( MomFGagT, t1(TwrGagNd(1),:) )
   AllOuts(TwHt1MLyt) = -DOT_PRODUCT( MomFGagT, t3(TwrGagNd(1),:) )
   AllOuts(TwHt1MLzt) =  DOT_PRODUCT( MomFGagT, t2(TwrGagNd(1),:) )


   IF ( NTwGages >= 2 )  THEN

   ! Initialize MomFGagT using the tower-top and yaw bearing mass effects:
      CALL CrossProd( MomFGagT, rZO - rZT(TwrGagNd(2),:), &
                      FrcONcRt - YawBrMass*( Gravity*z2 + LinAccEO ) )
      MomFGagT = MomFGagT + MomBNcRt

   ! Integrate to find MomFGagT using all of the nodes / elements above
   !   the current strain gage location:
      DO J = ( TwrGagNd(2) + 1 ),TwrNodes ! Loop through tower nodes / elements above strain gage node
         CALL CrossProd( TmpVec, rZT(J,:) - rZT(TwrGagNd(2),:), &                               ! Portion of MomFGagT associated with element J
                         FTAero(J,:) + FTHydro(J,:) - MassT(J)*( Gravity*z2 + LinAccET(J,:) ) )
         MomFGagT = MomFGagT + ( TmpVec + MFAero(J,:) + MFHydro(J,:) )*DHNodes(J)
      ENDDO ! J -Tower nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to tower bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DHNodes() and the element
   !   length is 1/2 of DHNodes().
      CALL CrossProd( TmpVec, ( 0.25*DHNodes( TwrGagNd(2)) )*a2,      &                ! Portion of MomFGagT associated with 1/2 of the strain gage element
                      FTAero( TwrGagNd(2),:) + FTHydro(TwrGagNd(2),:) &
                      - MassT(TwrGagNd(2))*( Gravity*z2 + LinAccET(TwrGagNd(2),:) ) )
      MomFGagT = MomFGagT + ( TmpVec + MFAero(TwrGagNd(2),:) + MFHydro(TwrGagNd(2),:) )*&
                              ( 0.5 *DHNodes( TwrGagNd(2)) )
      MomFGagT = 0.001*MomFGagT  ! Convert the local moment to kN-m

      AllOuts(TwHt2MLxt) =  DOT_PRODUCT( MomFGagT, t1(TwrGagNd(2),:) )
      AllOuts(TwHt2MLyt) = -DOT_PRODUCT( MomFGagT, t3(TwrGagNd(2),:) )
      AllOuts(TwHt2MLzt) =  DOT_PRODUCT( MomFGagT, t2(TwrGagNd(2),:) )


      IF ( NTwGages >= 3 )  THEN

   ! Initialize MomFGagT using the tower-top and yaw bearing mass effects:
         CALL CrossProd( MomFGagT, rZO - rZT(TwrGagNd(3),:), &
                         FrcONcRt - YawBrMass*( Gravity*z2 + LinAccEO ) )
         MomFGagT = MomFGagT + MomBNcRt

   ! Integrate to find MomFGagT using all of the nodes / elements above
   !   the current strain gage location:
         DO J = ( TwrGagNd(3) + 1 ),TwrNodes ! Loop through tower nodes / elements above strain gage node
            CALL CrossProd( TmpVec, rZT(J,:) - rZT(TwrGagNd(3),:), &                               ! Portion of MomFGagT associated with element J
                            FTAero(J,:) + FTHydro(J,:) - MassT(J)*( Gravity*z2 + LinAccET(J,:) ) )
            MomFGagT = MomFGagT + ( TmpVec + MFAero(J,:) + MFHydro(J,:) )*DHNodes(J)
         ENDDO ! J -Tower nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to tower bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DHNodes() and the element
   !   length is 1/2 of DHNodes().
         CALL CrossProd( TmpVec, ( 0.25*DHNodes( TwrGagNd(3)) )*a2,      &                ! Portion of MomFGagT associated with 1/2 of the strain gage element
                         FTAero( TwrGagNd(3),:) + FTHydro(TwrGagNd(3),:) &
                         - MassT(TwrGagNd(3))*( Gravity*z2 + LinAccET(TwrGagNd(3),:) ) )
         MomFGagT = MomFGagT + ( TmpVec + MFAero(TwrGagNd(3),:) + MFHydro(TwrGagNd(3),:) )*&
                                 ( 0.5 *DHNodes( TwrGagNd(3)) )
         MomFGagT = 0.001*MomFGagT  ! Convert the local moment to kN-m

         AllOuts(TwHt3MLxt) =  DOT_PRODUCT( MomFGagT, t1(TwrGagNd(3),:) )
         AllOuts(TwHt3MLyt) = -DOT_PRODUCT( MomFGagT, t3(TwrGagNd(3),:) )
         AllOuts(TwHt3MLzt) =  DOT_PRODUCT( MomFGagT, t2(TwrGagNd(3),:) )


         IF ( NTwGages >= 4 )  THEN

   ! Initialize MomFGagT using the tower-top and yaw bearing mass effects:
            CALL CrossProd( MomFGagT, rZO - rZT(TwrGagNd(4),:), &
                            FrcONcRt - YawBrMass*( Gravity*z2 + LinAccEO ) )
            MomFGagT = MomFGagT + MomBNcRt

   ! Integrate to find MomFGagT using all of the nodes / elements above
   !   the current strain gage location:
            DO J = ( TwrGagNd(4) + 1 ),TwrNodes ! Loop through tower nodes / elements above strain gage node
               CALL CrossProd( TmpVec, rZT(J,:) - rZT(TwrGagNd(4),:), &                               ! Portion of MomFGagT associated with element J
                               FTAero(J,:) + FTHydro(J,:) - MassT(J)*( Gravity*z2 + LinAccET(J,:) ) )
               MomFGagT = MomFGagT + ( TmpVec + MFAero(J,:) + MFHydro(J,:) )*DHNodes(J)
            ENDDO ! J -Tower nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to tower bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DHNodes() and the element
   !   length is 1/2 of DHNodes().
            CALL CrossProd( TmpVec, ( 0.25*DHNodes( TwrGagNd(4)) )*a2,      &                ! Portion of MomFGagT associated with 1/2 of the strain gage element
                            FTAero( TwrGagNd(4),:) + FTHydro(TwrGagNd(4),:) &
                            - MassT(TwrGagNd(4))*( Gravity*z2 + LinAccET(TwrGagNd(4),:) ) )
            MomFGagT = MomFGagT + ( TmpVec + MFAero(TwrGagNd(4),:) + MFHydro(TwrGagNd(4),:) )*&
                                    ( 0.5 *DHNodes( TwrGagNd(4)) )
            MomFGagT = 0.001*MomFGagT  ! Convert the local moment to kN-m

            AllOuts(TwHt4MLxt) =  DOT_PRODUCT( MomFGagT, t1(TwrGagNd(4),:) )
            AllOuts(TwHt4MLyt) = -DOT_PRODUCT( MomFGagT, t3(TwrGagNd(4),:) )
            AllOuts(TwHt4MLzt) =  DOT_PRODUCT( MomFGagT, t2(TwrGagNd(4),:) )

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:
!remove6.02a            IF ( NTwGages == 5 )  THEN
            IF ( NTwGages >= 5 )  THEN
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

   ! Initialize MomFGagT using the tower-top and yaw bearing mass effects:
               CALL CrossProd( MomFGagT, rZO - rZT(TwrGagNd(5),:), &
                               FrcONcRt - YawBrMass*( Gravity*z2 + LinAccEO ) )
               MomFGagT = MomFGagT + MomBNcRt

   ! Integrate to find MomFGagT using all of the nodes / elements above
   !   the current strain gage location:
               DO J = ( TwrGagNd(5) + 1 ),TwrNodes ! Loop through tower nodes / elements above strain gage node
                  CALL CrossProd( TmpVec, rZT(J,:) - rZT(TwrGagNd(5),:), &                               ! Portion of MomFGagT associated with element J
                                  FTAero(J,:) + FTHydro(J,:) - MassT(J)*( Gravity*z2 + LinAccET(J,:) ) )
                  MomFGagT = MomFGagT + ( TmpVec + MFAero(J,:) + MFHydro(J,:) )*DHNodes(J)
               ENDDO ! J -Tower nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to tower bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DHNodes() and the element
   !   length is 1/2 of DHNodes().
               CALL CrossProd( TmpVec, ( 0.25*DHNodes( TwrGagNd(5)) )*a2,      &                ! Portion of MomFGagT associated with 1/2 of the strain gage element
                               FTAero( TwrGagNd(5),:) + FTHydro(TwrGagNd(5),:) &
                               - MassT(TwrGagNd(5))*( Gravity*z2 + LinAccET(TwrGagNd(5),:) ) )
               MomFGagT = MomFGagT + ( TmpVec + MFAero(TwrGagNd(5),:) + MFHydro(TwrGagNd(5),:) )*&
                                       ( 0.5 *DHNodes( TwrGagNd(5)) )
               MomFGagT = 0.001*MomFGagT  ! Convert the local moment to kN-m

               AllOuts(TwHt5MLxt) =  DOT_PRODUCT( MomFGagT, t1(TwrGagNd(5),:) )
               AllOuts(TwHt5MLyt) = -DOT_PRODUCT( MomFGagT, t3(TwrGagNd(5),:) )
               AllOuts(TwHt5MLzt) =  DOT_PRODUCT( MomFGagT, t2(TwrGagNd(5),:) )
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Increase the upper limit for the number of blade and tower strain gage
!jmj   locations from 5 to 9 and add new output parameters for the local loads
!jmj   and motions at the additional strain gage locations:

               IF ( NTwGages >= 6 )  THEN

   ! Initialize MomFGagT using the tower-top and yaw bearing mass effects:
                  CALL CrossProd( MomFGagT, rZO - rZT(TwrGagNd(6),:), &
                                  FrcONcRt - YawBrMass*( Gravity*z2 + LinAccEO ) )
                  MomFGagT = MomFGagT + MomBNcRt

   ! Integrate to find MomFGagT using all of the nodes / elements above
   !   the current strain gage location:
                  DO J = ( TwrGagNd(6) + 1 ),TwrNodes ! Loop through tower nodes / elements above strain gage node
                     CALL CrossProd( TmpVec, rZT(J,:) - rZT(TwrGagNd(6),:), &                               ! Portion of MomFGagT associated with element J
                                     FTAero(J,:) + FTHydro(J,:) - MassT(J)*( Gravity*z2 + LinAccET(J,:) ) )
                     MomFGagT = MomFGagT + ( TmpVec + MFAero(J,:) + MFHydro(J,:) )*DHNodes(J)
                  ENDDO ! J -Tower nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to tower bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DHNodes() and the element
   !   length is 1/2 of DHNodes().
                  CALL CrossProd( TmpVec, ( 0.25*DHNodes( TwrGagNd(6)) )*a2,      &                ! Portion of MomFGagT associated with 1/2 of the strain gage element
                                  FTAero( TwrGagNd(6),:) + FTHydro(TwrGagNd(6),:) &
                                  - MassT(TwrGagNd(6))*( Gravity*z2 + LinAccET(TwrGagNd(6),:) ) )
                  MomFGagT = MomFGagT + ( TmpVec + MFAero(TwrGagNd(6),:) + MFHydro(TwrGagNd(6),:) )*&
                                          ( 0.5 *DHNodes( TwrGagNd(6)) )
                  MomFGagT = 0.001*MomFGagT  ! Convert the local moment to kN-m

                  AllOuts(TwHt6MLxt) =     DOT_PRODUCT( MomFGagT, t1(TwrGagNd(6),:) )
                  AllOuts(TwHt6MLyt) = -1.*DOT_PRODUCT( MomFGagT, t3(TwrGagNd(6),:) )
                  AllOuts(TwHt6MLzt) =     DOT_PRODUCT( MomFGagT, t2(TwrGagNd(6),:) )


                  IF ( NTwGages >= 7 )  THEN

   ! Initialize MomFGagT using the tower-top and yaw bearing mass effects:
                     CALL CrossProd( MomFGagT, rZO - rZT(TwrGagNd(7),:), &
                                     FrcONcRt - YawBrMass*( Gravity*z2 + LinAccEO ) )
                     MomFGagT = MomFGagT + MomBNcRt

   ! Integrate to find MomFGagT using all of the nodes / elements above
   !   the current strain gage location:
                     DO J = ( TwrGagNd(7) + 1 ),TwrNodes ! Loop through tower nodes / elements above strain gage node
                        CALL CrossProd( TmpVec, rZT(J,:) - rZT(TwrGagNd(7),:), &                               ! Portion of MomFGagT associated with element J
                                        FTAero(J,:) + FTHydro(J,:) - MassT(J)*( Gravity*z2 + LinAccET(J,:) ) )
                        MomFGagT = MomFGagT + ( TmpVec + MFAero(J,:) + MFHydro(J,:) )*DHNodes(J)
                     ENDDO ! J -Tower nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to tower bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DHNodes() and the element
   !   length is 1/2 of DHNodes().
                     CALL CrossProd( TmpVec, ( 0.25*DHNodes( TwrGagNd(7)) )*a2,      &                ! Portion of MomFGagT associated with 1/2 of the strain gage element
                                     FTAero( TwrGagNd(7),:) + FTHydro(TwrGagNd(7),:) &
                                     - MassT(TwrGagNd(7))*( Gravity*z2 + LinAccET(TwrGagNd(7),:) ) )
                     MomFGagT = MomFGagT + ( TmpVec + MFAero(TwrGagNd(7),:) + MFHydro(TwrGagNd(7),:) )*&
                                             ( 0.5 *DHNodes( TwrGagNd(7)) )
                     MomFGagT = 0.001*MomFGagT  ! Convert the local moment to kN-m

                     AllOuts(TwHt7MLxt) =     DOT_PRODUCT( MomFGagT, t1(TwrGagNd(7),:) )
                     AllOuts(TwHt7MLyt) = -1.*DOT_PRODUCT( MomFGagT, t3(TwrGagNd(7),:) )
                     AllOuts(TwHt7MLzt) =     DOT_PRODUCT( MomFGagT, t2(TwrGagNd(7),:) )


                     IF ( NTwGages >= 8 )  THEN

   ! Initialize MomFGagT using the tower-top and yaw bearing mass effects:
                        CALL CrossProd( MomFGagT, rZO - rZT(TwrGagNd(8),:), &
                                        FrcONcRt - YawBrMass*( Gravity*z2 + LinAccEO ) )
                        MomFGagT = MomFGagT + MomBNcRt

   ! Integrate to find MomFGagT using all of the nodes / elements above
   !   the current strain gage location:
                        DO J = ( TwrGagNd(8) + 1 ),TwrNodes ! Loop through tower nodes / elements above strain gage node
                           CALL CrossProd( TmpVec, rZT(J,:) - rZT(TwrGagNd(8),:), &                               ! Portion of MomFGagT associated with element J
                                           FTAero(J,:) + FTHydro(J,:) - MassT(J)*( Gravity*z2 + LinAccET(J,:) ) )
                           MomFGagT = MomFGagT + ( TmpVec + MFAero(J,:) + MFHydro(J,:) )*DHNodes(J)
                        ENDDO ! J -Tower nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to tower bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DHNodes() and the element
   !   length is 1/2 of DHNodes().
                        CALL CrossProd( TmpVec, ( 0.25*DHNodes( TwrGagNd(8)) )*a2,      &                ! Portion of MomFGagT associated with 1/2 of the strain gage element
                                        FTAero( TwrGagNd(8),:) + FTHydro(TwrGagNd(8),:) &
                                        - MassT(TwrGagNd(8))*( Gravity*z2 + LinAccET(TwrGagNd(8),:) ) )
                        MomFGagT = MomFGagT + ( TmpVec + MFAero(TwrGagNd(8),:) + MFHydro(TwrGagNd(8),:) )*&
                                                ( 0.5 *DHNodes( TwrGagNd(8)) )
                        MomFGagT = 0.001*MomFGagT  ! Convert the local moment to kN-m

                        AllOuts(TwHt8MLxt) =     DOT_PRODUCT( MomFGagT, t1(TwrGagNd(8),:) )
                        AllOuts(TwHt8MLyt) = -1.*DOT_PRODUCT( MomFGagT, t3(TwrGagNd(8),:) )
                        AllOuts(TwHt8MLzt) =     DOT_PRODUCT( MomFGagT, t2(TwrGagNd(8),:) )


                        IF ( NTwGages == 9 )  THEN

   ! Initialize MomFGagT using the tower-top and yaw bearing mass effects:
                           CALL CrossProd( MomFGagT, rZO - rZT(TwrGagNd(9),:), &
                                           FrcONcRt - YawBrMass*( Gravity*z2 + LinAccEO ) )
                           MomFGagT = MomFGagT + MomBNcRt

   ! Integrate to find MomFGagT using all of the nodes / elements above
   !   the current strain gage location:
                           DO J = ( TwrGagNd(9) + 1 ),TwrNodes ! Loop through tower nodes / elements above strain gage node
                              CALL CrossProd( TmpVec, rZT(J,:) - rZT(TwrGagNd(9),:), &                               ! Portion of MomFGagT associated with element J
                                              FTAero(J,:) + FTHydro(J,:) - MassT(J)*( Gravity*z2 + LinAccET(J,:) ) )
                              MomFGagT = MomFGagT + ( TmpVec + MFAero(J,:) + MFHydro(J,:) )*DHNodes(J)
                           ENDDO ! J -Tower nodes / elements above strain gage node

   ! Add the effects of 1/2 the strain gage element:
   ! NOTE: for the radius in this calculation, assume that there is no
   !   shortening effect (due to tower bending) within the element.  Thus,
   !   the moment arm for the force is 1/4 of DHNodes() and the element
   !   length is 1/2 of DHNodes().
                           CALL CrossProd( TmpVec, ( 0.25*DHNodes( TwrGagNd(9)) )*a2,      &                ! Portion of MomFGagT associated with 1/2 of the strain gage element
                                           FTAero( TwrGagNd(9),:) + FTHydro(TwrGagNd(9),:) &
                                           - MassT(TwrGagNd(9))*( Gravity*z2 + LinAccET(TwrGagNd(9),:) ) )
                           MomFGagT = MomFGagT + ( TmpVec + MFAero(TwrGagNd(9),:) + MFHydro(TwrGagNd(9),:) )*&
                                                   ( 0.5 *DHNodes( TwrGagNd(9)) )
                           MomFGagT = 0.001*MomFGagT  ! Convert the local moment to kN-m

                           AllOuts(TwHt9MLxt) =     DOT_PRODUCT( MomFGagT, t1(TwrGagNd(9),:) )
                           AllOuts(TwHt9MLyt) = -1.*DOT_PRODUCT( MomFGagT, t3(TwrGagNd(9),:) )
                           AllOuts(TwHt9MLzt) =     DOT_PRODUCT( MomFGagT, t2(TwrGagNd(9),:) )

                        ENDIF

                     ENDIF

                  ENDIF

               ENDIF

!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

            ENDIF

         ENDIF

      ENDIF

   ENDIF

ENDIF


   ! Platform Loads:

AllOuts(  PtfmFxt) =  DOT_PRODUCT( FZHydro, a1 )
AllOuts(  PtfmFyt) = -DOT_PRODUCT( FZHydro, a3 )
AllOuts(  PtfmFzt) =  DOT_PRODUCT( FZHydro, a2 )
AllOuts(  PtfmFxi) =  DOT_PRODUCT( FZHydro, z1 )
AllOuts(  PtfmFyi) = -DOT_PRODUCT( FZHydro, z3 )
AllOuts(  PtfmFzi) =  DOT_PRODUCT( FZHydro, z2 )
AllOuts(  PtfmMxt) =  DOT_PRODUCT( MXHydro, a1 )
AllOuts(  PtfmMyt) = -DOT_PRODUCT( MXHydro, a3 )
AllOuts(  PtfmMzt) =  DOT_PRODUCT( MXHydro, a2 )
AllOuts(  PtfmMxi) =  DOT_PRODUCT( MXHydro, z1 )
AllOuts(  PtfmMyi) = -DOT_PRODUCT( MXHydro, z3 )
AllOuts(  PtfmMzi) =  DOT_PRODUCT( MXHydro, z2 )

!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:


IF ( CompHydro )  THEN  ! Hydrodynamics have been used


   ! Mooring Line Loads:

   IF ( NumLines >= 1 )  THEN

      CALL FairleadTension ( 1, FairTe, FairTeAng )
      CALL AnchorTension   ( 1, AnchTe, AnchTeAng )
      AllOuts(Fair1Ten ) = FairTe   *0.001   ! Convert to kN
      AllOuts(Fair1Ang ) = FairTeAng*R2D     ! Convert to degrees
      AllOuts(Anch1Ten ) = AnchTe   *0.001   ! Convert to kN
      AllOuts(Anch1Ang ) = AnchTeAng*R2D     ! Convert to degrees

      IF ( NumLines >= 2 )  THEN

         CALL FairleadTension ( 2, FairTe, FairTeAng )
         CALL AnchorTension   ( 2, AnchTe, AnchTeAng )
         AllOuts(Fair2Ten ) = FairTe   *0.001   ! Convert to kN
         AllOuts(Fair2Ang ) = FairTeAng*R2D     ! Convert to degrees
         AllOuts(Anch2Ten ) = AnchTe   *0.001   ! Convert to kN
         AllOuts(Anch2Ang ) = AnchTeAng*R2D     ! Convert to degrees

         IF ( NumLines >= 3 )  THEN

            CALL FairleadTension ( 3, FairTe, FairTeAng )
            CALL AnchorTension   ( 3, AnchTe, AnchTeAng )
            AllOuts(Fair3Ten ) = FairTe   *0.001   ! Convert to kN
            AllOuts(Fair3Ang ) = FairTeAng*R2D     ! Convert to degrees
            AllOuts(Anch3Ten ) = AnchTe   *0.001   ! Convert to kN
            AllOuts(Anch3Ang ) = AnchTeAng*R2D     ! Convert to degrees

            IF ( NumLines >= 4 )  THEN

               CALL FairleadTension ( 4, FairTe, FairTeAng )
               CALL AnchorTension   ( 4, AnchTe, AnchTeAng )
               AllOuts(Fair4Ten ) = FairTe   *0.001   ! Convert to kN
               AllOuts(Fair4Ang ) = FairTeAng*R2D     ! Convert to degrees
               AllOuts(Anch4Ten ) = AnchTe   *0.001   ! Convert to kN
               AllOuts(Anch4Ang ) = AnchTeAng*R2D     ! Convert to degrees

               IF ( NumLines >= 5 )  THEN

                  CALL FairleadTension ( 5, FairTe, FairTeAng )
                  CALL AnchorTension   ( 5, AnchTe, AnchTeAng )
                  AllOuts(Fair5Ten ) = FairTe   *0.001   ! Convert to kN
                  AllOuts(Fair5Ang ) = FairTeAng*R2D     ! Convert to degrees
                  AllOuts(Anch5Ten ) = AnchTe   *0.001   ! Convert to kN
                  AllOuts(Anch5Ang ) = AnchTeAng*R2D     ! Convert to degrees

                  IF ( NumLines >= 6 )  THEN

                     CALL FairleadTension ( 6, FairTe, FairTeAng )
                     CALL AnchorTension   ( 6, AnchTe, AnchTeAng )
                     AllOuts(Fair6Ten ) = FairTe   *0.001   ! Convert to kN
                     AllOuts(Fair6Ang ) = FairTeAng*R2D     ! Convert to degrees
                     AllOuts(Anch6Ten ) = AnchTe   *0.001   ! Convert to kN
                     AllOuts(Anch6Ang ) = AnchTeAng*R2D     ! Convert to degrees

                     IF ( NumLines >= 7 )  THEN

                        CALL FairleadTension ( 7, FairTe, FairTeAng )
                        CALL AnchorTension   ( 7, AnchTe, AnchTeAng )
                        AllOuts(Fair7Ten ) = FairTe   *0.001   ! Convert to kN
                        AllOuts(Fair7Ang ) = FairTeAng*R2D     ! Convert to degrees
                        AllOuts(Anch7Ten ) = AnchTe   *0.001   ! Convert to kN
                        AllOuts(Anch7Ang ) = AnchTeAng*R2D     ! Convert to degrees

                        IF ( NumLines >= 8 )  THEN

                           CALL FairleadTension ( 8, FairTe, FairTeAng )
                           CALL AnchorTension   ( 8, AnchTe, AnchTeAng )
                           AllOuts(Fair8Ten ) = FairTe   *0.001   ! Convert to kN
                           AllOuts(Fair8Ang ) = FairTeAng*R2D     ! Convert to degrees
                           AllOuts(Anch8Ten ) = AnchTe   *0.001   ! Convert to kN
                           AllOuts(Anch8Ang ) = AnchTeAng*R2D     ! Convert to degrees

                           IF ( NumLines >= 9 )  THEN

                              CALL FairleadTension ( 9, FairTe, FairTeAng )
                              CALL AnchorTension   ( 9, AnchTe, AnchTeAng )
                              AllOuts(Fair9Ten ) = FairTe   *0.001   ! Convert to kN
                              AllOuts(Fair9Ang ) = FairTeAng*R2D     ! Convert to degrees
                              AllOuts(Anch9Ten ) = AnchTe   *0.001   ! Convert to kN
                              AllOuts(Anch9Ang ) = AnchTeAng*R2D     ! Convert to degrees

                           ENDIF

                        ENDIF

                     ENDIF

                  ENDIF

               ENDIF

            ENDIF

         ENDIF

      ENDIF

   ENDIF


ENDIF
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.


   ! Place the selected output channels into the OutData(:) array with
   !   the proper sign:

DO I = 0,NumOuts  ! Loop through all selected output channels

   OutData(I) = OutSign(I)*AllOuts( OutInd(I) )

ENDDO             ! I - All selected output channels



RETURN
END SUBROUTINE CalcOuts
!=======================================================================
SUBROUTINE Coeff


   ! This routine is used to compute rotor (blade and hub) properties:
   !   KBF(), KBE(), CBF(), CBE(), FreqBF(), FreqBE(), AxRedBld(),
   !   TwistedSF(), BldMass(), FirstMom(), SecondMom(), BldCG(),
   !   RotMass, RotIner, Hubg1Iner, Hubg2Iner, rSAerCenn1(), and
   !   rSAerCenn2()
   ! tower properties:
   !   KTFA(), KTSS(), CTFA(), CTSS(), FreqTFA(), FreqTSS(),
   !   AxRedTFA(), AxRedTSS(), TwrFASF(), TwrSSSF(), TwrMass, and
   !   TwrTpMass
   ! structure that furls with the rotor (not including rotor) properties:
   !   RrfaIner
   ! tail boom properties:
   !   AtfaIner
   ! nacelle properties:
   !   Nacd2Iner
   ! and generator properties:
   !   GenDir.


USE                             Blades
USE                             Constants    !bjj: why don't you define inv2pi here since it's the only subroutine/function in which it is used?
USE                             DriveTrain
USE                             EnvCond
USE                             Features
USE                             InitCond
USE                             MassInert
USE                             Modes
!bjj rm NWTC_Library: USE                             Precision
USE                             Tower
USE                             TurbConf


IMPLICIT                        NONE


   ! Local variables.

REAL(ReKi)                   :: AxRdBld   (3,3)                                 ! Temporary result holding the current addition to the AxRedBld() array.
REAL(ReKi)                   :: AxRdBldOld(3,3)                                 ! Previous AxRdBld (i.e., AxRdBld from the previous node)
REAL(ReKi)                   :: AxRdTFA   (2,2)                                 ! Temporary result holding the current addition to the AxRedTFA() array.
REAL(ReKi)                   :: AxRdTFAOld(2,2)                                 ! Previous AxRdTFA (i.e., AxRdTFA from the previous node)
REAL(ReKi)                   :: AxRdTSS   (2,2)                                 ! Temporary result holding the current addition to the AxRedTSS() array.
REAL(ReKi)                   :: AxRdTSSOld(2,2)                                 ! Previous AxRdTSS (i.e., AxRdTSS from the previous node)
REAL(ReKi)                   :: TmpDist                                         ! Temporary distance used in the calculation of the aero center locations.
REAL(ReKi)                   :: TmpDistj1                                       ! Temporary distance used in the calculation of the aero center locations.
REAL(ReKi)                   :: TmpDistj2                                       ! Temporary distance used in the calculation of the aero center locations.
REAL(ReKi)                   :: ElMassOld                                       ! Previous ElmntMass (i.e., ElmntMass from the previous node)
REAL(ReKi)                   :: ElmntMass                                       ! (Temporary) mass of an element.
REAL(ReKi)                   :: ElmntStff                                       ! (Temporary) stiffness of an element.
REAL(ReKi)                   :: ElStffFA                                        ! (Temporary) tower fore-aft stiffness of an element
REAL(ReKi)                   :: ElStffSS                                        ! (Temporary) tower side-to-side  stiffness of an element
REAL(ReKi), ALLOCATABLE      :: FMomAbvNd (:,:)                                 ! FMomAbvNd(K,J) = portion of the first moment of blade K about the rotor centerline (not root, like FirstMom(K)) associated with everything above node J (including tip brake masses).
REAL(ReKi), ALLOCATABLE      :: KBECent   (:,:,:)                               ! Centrifugal-term of generalized edgewise stiffness of the blades.
REAL(ReKi), ALLOCATABLE      :: KBFCent   (:,:,:)                               ! Centrifugal-term of generalized flapwise stiffness of the blades.
REAL(ReKi)                   :: KTFAGrav  (2,2)                                 ! Gravitational-term of generalized fore-aft stiffness of the tower.
REAL(ReKi)                   :: KTSSGrav  (2,2)                                 ! Gravitational-term of generalized side-to-side stiffness of the tower.
REAL(ReKi), ALLOCATABLE      :: MBE       (:,:,:)                               ! Generalized edgewise mass of the blades.
REAL(ReKi), ALLOCATABLE      :: MBF       (:,:,:)                               ! Generalized flapwise mass of the blades.
REAL(ReKi)                   :: MTFA      (2,2)                                 ! Generalized fore-aft mass of the tower.
REAL(ReKi)                   :: MTSS      (2,2)                                 ! Generalized side-to-side mass of the tower.
REAL(ReKi)                   :: Shape                                           ! Temporary result holding a value from the SHP function
REAL(ReKi)                   :: Shape1                                          ! Temporary result holding a value from the SHP function
REAL(ReKi)                   :: Shape2                                          ! Temporary result holding a value from the SHP function
REAL(ReKi), ALLOCATABLE      :: TMssAbvNd (:)                                   ! Portion of the tower mass associated with everything above node J (including tower-top effects)
REAL(ReKi)                   :: TwstdSF   (2,3,0:1)                             ! Temperory result holding the current addition to the TwistedSF() array.
REAL(ReKi)                   :: TwstdSFOld(2,3,0:1)                             ! Previous TwstdSF (i.e., TwstdSF from the previous node)

INTEGER(4)                   :: I                                               ! Generic index.
INTEGER(4)                   :: J                                               ! Loops through nodes / elements.
INTEGER(4)                   :: K                                               ! Loops through blades.
INTEGER(4)                   :: L                                               ! Generic index
INTEGER(4)                   :: Sttus                                           ! Status returned from an allocation request.


   ! ALLOCATE some local arrays:
!BJJ CHECK ALLOCATION:
Sttus = 0.0

IF (.NOT. ALLOCATED( FMomAbvNd )) ALLOCATE ( FMomAbvNd(NumBl,BldNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the FMomAbvNd array.' )
ENDIF

IF (.NOT. ALLOCATED( KBECent )) ALLOCATE ( KBECent(NumBl,1,1) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the KBECent array.' )
ENDIF

IF (.NOT. ALLOCATED( KBFCent )) ALLOCATE ( KBFCent(NumBl,2,2) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the KBFCent array.' )
ENDIF

IF (.NOT. ALLOCATED( MBE )) ALLOCATE ( MBE(NumBl,1,1) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the MBE array.' )
ENDIF

IF (.NOT. ALLOCATED( MBF )) ALLOCATE ( MBF(NumBl,2,2) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the MBF array.' )
ENDIF

IF (.NOT. ALLOCATED( TMssAbvNd )) ALLOCATE ( TMssAbvNd(TwrNodes) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the TMssAbvNd array.' )
ENDIF



   ! Calculate the distances from point S on a blade to the aerodynamic
   !   center in the j1 and j2 directions:

DO K = 1,NumBl          ! Loop through the blades

   DO J = 1,BldNodes    ! Loop through the blade nodes / elements

      TmpDist         = ( AeroCent(K,J) - 0.25 )*Chord(J)   ! Distance along the chordline from point S (25% chord) to the aerodynamic center of the blade element J--positive towards the trailing edge.
      TmpDistj1       = TmpDist*SAeroTwst(J)                ! Distance along the j1-axis   from point S (25% chord) to the aerodynamic center of the blade element J
      TmpDistj2       = TmpDist*CAeroTwst(J)                ! Distance along the j2-axis   from point S (25% chord) to the aerodynamic center of the blade element J
      rSAerCenn1(K,J) = TmpDistj1*CThetaS(K,J) - TmpDistj2*SThetaS(K,J)
      rSAerCenn2(K,J) = TmpDistj1*SThetaS(K,J) + TmpDistj2*CThetaS(K,J)

   ENDDO ! J - Blade nodes / elements

ENDDO    ! K - Blades


   ! Calculate the generator direction using GBRevers:

IF ( GBRevers )  THEN   ! HSS and LSS rotate in opposite directions
   GenDir = -1
ELSE                    ! HSS and LSS rotate in the same direction
   GenDir =  1
ENDIF


   ! Calculate the structure that furls with the rotor inertia term:

RrfaIner  = RFrlIner - RFrlMass*(       rVDxn*rVDxn*( 1.0 - CRFrlSkw2*CRFrlTlt2 ) &
                                  +     rVDzn*rVDzn*                  CRFrlTlt2   &
                                  +     rVDyn*rVDyn*( 1.0 - SRFrlSkw2*CRFrlTlt2 ) &
                                  - 2.0*rVDxn*rVDzn*        CRFrlSkew*CSRFrlTlt   &
                                  - 2.0*rVDxn*rVDyn*        CSRFrlSkw*CRFrlTlt2   &
                                  - 2.0*rVDzn*rVDyn*        SRFrlSkew*CSRFrlTlt     )
IF ( RrfaIner < 0.0 )   CALL ProgAbort ( ' RFrlIner must not be less than RFrlMass*( perpendicular distance between rotor-furl'// &
                                     ' axis and CM of the structure that furls with the rotor [not including rotor] )^2.'       )


   ! Calculate the tail boom inertia term:

AtfaIner  = TFrlIner - BoomMass*(       rWIxn*rWIxn*( 1.0 - CTFrlSkw2*CTFrlTlt2 ) &
                                  +     rWIzn*rWIzn*                  CTFrlTlt2   &
                                  +     rWIyn*rWIyn*( 1.0 - STFrlSkw2*CTFrlTlt2 ) &
                                  - 2.0*rWIxn*rWIzn*        CTFrlSkew*CSTFrlTlt   &
                                  - 2.0*rWIxn*rWIyn*        CSTFrlSkw*CTFrlTlt2   &
                                  - 2.0*rWIzn*rWIyn*        STFrlSkew*CSTFrlTlt     )
IF ( AtfaIner < 0.0 )   CALL ProgAbort ( ' TFrlIner must not be less than BoomMass*( perpendicular distance between tail-furl'// &
                                     ' axis and tail boom CM )^2.'                                                             )


   ! Calculate the nacelle inertia terms:

Nacd2Iner = NacYIner - NacMass*( NacCMxn*NacCMxn + NacCMyn*NacCMyn ) ! Nacelle inertia about the d2-axis
IF ( Nacd2Iner < 0.0 )  CALL ProgAbort ( ' NacYIner must not be less than NacMass*( NacCMxn^2 + NacCMyn^2 ).' )


   ! Calculate hub inertia about its centerline passing through its c.g..
   !   This calculation assumes that the hub for a 2-blader is essentially
   !   a uniform cylinder whose centerline is transverse through the cylinder
   !   passing through its c.g..  That is, for a 2-blader, Hubg1Iner =
   !   Hubg2Iner is the inertia of the hub about both the g1- and g2- axes.  For
   !   3-bladers, Hubg1Iner is simply equal to HubIner and Hubg2Iner is zero.
   ! Also, Initialize RotMass and RotIner to associated hub properties:

IF ( NumBl == 2 )  THEN ! 2-blader
   Hubg1Iner = ( HubIner - HubMass*( ( UndSling - HubCM )**2 ) )/( ( COS(Delta3) )**2 )
   Hubg2Iner = Hubg1Iner
   IF ( Hubg1Iner < 0.0 )  CALL ProgAbort ( ' HubIner must not be less than HubMass*( UndSling - HubCM )^2 for 2-blader.' )
ELSE                    ! 3-blader
   Hubg1Iner = HubIner
   Hubg2Iner = 0.0
ENDIF

RotMass   = HubMass
RotIner   = Hubg1Iner



   ! Initialize several variables to 0.0:

KBF       = 0.0
KBE       = 0.0
KBFCent   = 0.0
KBECent   = 0.0

TwrMass   = 0.0
KTFA      = 0.0
KTSS      = 0.0
KTFAGrav  = 0.0
KTSSGrav  = 0.0



DO K = 1,NumBl          ! Loop through the blades


   ! Initialize BldMass(), FirstMom(), and SecondMom() using TipMass() effects:

   BldMass  (K) = TipMass(K)
   FirstMom (K) = TipMass(K)*BldFlexL
   SecondMom(K) = TipMass(K)*BldFlexL*BldFlexL


   DO J = BldNodes,1,-1 ! Loop through the blade nodes / elements in reverse


   ! Calculate the mass of the current element

      ElmntMass    = MassB(K,J)*DRNodes(J)                        ! Mass of blade element J


   ! Integrate to find some blade properties which will be output in .fsm

      BldMass  (K) = BldMass  (K) + ElmntMass
      FirstMom (K) = FirstMom (K) + ElmntMass*RNodes(J)
      SecondMom(K) = SecondMom(K) + ElmntMass*RNodes(J)*RNodes(J)


   ! Integrate to find FMomAbvNd:

      FMomAbvNd   (K,J) = ( 0.5*ElmntMass )*( HubRad + RNodes(J  ) + 0.5*DRNodes(J  ) )

      IF ( J == BldNodes )  THEN ! Outermost blade element
   ! Add the TipMass() effects:

         FMomAbvNd(K,J) = FmomAbvNd(K,J) + TipMass(K)*TipRad
      ELSE                       ! All other blade elements
   ! Add to FMomAbvNd(K,J) the effects from the (not yet used) portion of element J+1

         FMomAbvNd(K,J) = FMomAbvNd(K,J) + FMomAbvNd(K,J+1) &
                        + ( 0.5*ElMassOld )*( HubRad + RNodes(J+1) - 0.5*DRNodes(J+1) )
      ENDIF


   ! Store the mass of the current element (this will be used for the next element)

      ElMassOld    = ElmntMass


   ENDDO ! J - Blade nodes / elements in reverse


   ! Calculate BldCG() using FirstMom() and BldMass(); and calculate
   !   RotMass and RotIner:

   BldCG    (K) = FirstMom (K) / BldMass    (K)
   RotMass      = RotMass      + BldMass    (K)
   RotIner      = RotIner      + ( SecondMom(K) + BldMass  (K)*HubRad*( 2.0*BldCG(K) + HubRad ) )*( ( COS(PreCone(K)) )**2 )

ENDDO ! K - Blades



DO K = 1,NumBl          ! Loop through the blades


   ! Initialize the generalized blade masses using tip mass effects:

   MBF(K,1,1) = TipMass(K)
   MBF(K,2,2) = TipMass(K)
   MBE(K,1,1) = TipMass(K)


   DO J = 1,BldNodes    ! Loop through the blade nodes / elements


   ! Integrate to find the generalized mass of the blade (including tip mass effects).
   !   Ignore the cross-correlation terms of MBF (i.e. MBF(i,j) where i /= j) since
   !   these terms will never be used.

      ElmntMass     = MassB(K,J)*DRNodes(J)                          ! Mass of blade element J

      Shape1 = SHP( RNodesNorm(J), BldFlexL, BldFl1Sh(:,K), 0 )
      Shape2 = SHP( RNodesNorm(J), BldFlexL, BldFl2Sh(:,K), 0 )
      MBF    (K,1,1) = MBF    (K,1,1) + ElmntMass*Shape1*Shape1
      MBF    (K,2,2) = MBF    (K,2,2) + ElmntMass*Shape2*Shape2

      Shape  = SHP( RNodesNorm(J), BldFlexL, BldEdgSh(:,K), 0 )
      MBE    (K,1,1) = MBE    (K,1,1) + ElmntMass*Shape *Shape


   ! Integrate to find the generalized stiffness of the blade (not including centrifugal
   !    effects).

      ElmntStff      = StiffBF(K,J)*DRNodes(J)                       ! Flapwise stiffness of blade element J
      Shape1 = SHP( RNodesNorm(J), BldFlexL, BldFl1Sh(:,K), 2 )
      Shape2 = SHP( RNodesNorm(J), BldFlexL, BldFl2Sh(:,K), 2 )
      KBF    (K,1,1) = KBF    (K,1,1) + ElmntStff*Shape1*Shape1
      KBF    (K,1,2) = KBF    (K,1,2) + ElmntStff*Shape1*Shape2
      KBF    (K,2,1) = KBF    (K,2,1) + ElmntStff*Shape2*Shape1
      KBF    (K,2,2) = KBF    (K,2,2) + ElmntStff*Shape2*Shape2

      ElmntStff      = StiffBE(K,J)*DRNodes(J)                       ! Edgewise stiffness of blade element J
      Shape  = SHP( RNodesNorm(J), BldFlexL, BldEdgSh(:,K), 2 )
      KBE    (K,1,1) = KBE    (K,1,1) + ElmntStff*Shape *Shape


   ! Integrate to find the centrifugal-term of the generalized flapwise and edgewise
   !   stiffness of the blades.  Ignore the cross-correlation terms of KBFCent (i.e.
   !   KBFCent(i,j) where i /= j) since these terms will never be used.

      ElmntStff      = FMomAbvNd(K,J)*DRNodes(J)*RotSpeed*RotSpeed   ! Centrifugal stiffness of blade element J

      Shape1 = SHP( RNodesNorm(J), BldFlexL, BldFl1Sh(:,K), 1 )
      Shape2 = SHP( RNodesNorm(J), BldFlexL, BldFl2Sh(:,K), 1 )
      KBFCent(K,1,1) = KBFCent(K,1,1) + ElmntStff*Shape1*Shape1
      KBFCent(K,2,2) = KBFCent(K,2,2) + ElmntStff*Shape2*Shape2

      Shape  = SHP( RNodesNorm(J), BldFlexL, BldEdgSh(:,K), 1 )
      KBECent(K,1,1) = KBECent(K,1,1) + ElmntStff*Shape *Shape


   ! Calculate the 2nd derivatives of the twisted shape functions:

      Shape  = SHP( RNodesNorm(J), BldFlexL, BldFl1Sh(:,K), 2 )
      TwistedSF(K,1,1,J,2) =  Shape*CThetaS(K,J)                  ! 2nd deriv. of Phi1(J) for blade K
      TwistedSF(K,2,1,J,2) = -Shape*SThetaS(K,J)                  ! 2nd deriv. of Psi1(J) for blade K

      Shape  = SHP( RNodesNorm(J), BldFlexL, BldFl2Sh(:,K), 2 )
      TwistedSF(K,1,2,J,2) =  Shape*CThetaS(K,J)                  ! 2nd deriv. of Phi2(J) for blade K
      TwistedSF(K,2,2,J,2) = -Shape*SThetaS(K,J)                  ! 2nd deriv. of Psi2(J) for blade K

      Shape  = SHP( RNodesNorm(J), BldFlexL, BldEdgSh(:,K), 2 )
      TwistedSF(K,1,3,J,2) =  Shape*SThetaS(K,J)                  ! 2nd deriv. of Phi3(J) for blade K
      TwistedSF(K,2,3,J,2) =  Shape*CThetaS(K,J)                  ! 2nd deriv. of Psi3(J) for blade K


   ! Integrate to find the 1st derivatives of the twisted shape functions:

      DO I = 1,2     ! Loop through Phi and Psi
         DO L = 1,3  ! Loop through all blade DOFs
            TwstdSF     (  I,L,  1) = TwistedSF(K,I,L,J,2)*0.5*DRNodes(J)
            TwistedSF   (K,I,L,J,1) = TwstdSF   ( I,L,  1)
         ENDDO       ! L - All blade DOFs
      ENDDO          ! I - Phi and Psi

      IF ( J /= 1 )  THEN  ! All but the innermost blade element
   ! Add the effects from the (not yet used) portion of element J-1

         DO I = 1,2     ! Loop through Phi and Psi
            DO L = 1,3  ! Loop through all blade DOFs
               TwistedSF(K,I,L,J,1) = TwistedSF(K,I,L,J,1) + TwistedSF(K,I,L,J-1,1) &
                                    + TwstdSFOld( I,L,  1)
            ENDDO       ! L - All blade DOFs
         ENDDO          ! I - Phi and Psi
      ENDIF


   ! Integrate to find the twisted shape functions themselves (i.e., their zeroeth
   !   derivative):

      DO I = 1,2     ! Loop through Phi and Psi
         DO L = 1,3  ! Loop through all blade DOFs
            TwstdSF     (  I,L,  0) = TwistedSF(K,I,L,J,1)*0.5*DRNodes(J)
            TwistedSF   (K,I,L,J,0) = TwstdSF   ( I,L,  0)
         ENDDO       ! L - All blade DOFs
      ENDDO          ! I - Phi and Psi

      IF ( J /= 1 )  THEN  ! All but the innermost blade element
   ! Add the effects from the (not yet used) portion of element J-1

         DO I = 1,2     ! Loop through Phi and Psi
            DO L = 1,3  ! Loop through all blade DOFs
               TwistedSF(K,I,L,J,0) = TwistedSF(K,I,L,J,0) + TwistedSF(K,I,L,J-1,0) &
                                    + TwstdSFOld( I,L,  0)
            ENDDO       ! L - All blade DOFs
         ENDDO          ! I - Phi and Psi
      ENDIF


   ! Integrate to find the blade axial reduction shape functions:

      DO I = 1,3     ! Loop through all blade DOFs
         DO L = 1,3  ! Loop through all blade DOFs
            AxRdBld    (  I,L  ) = 0.5*DRNodes(J)*(                          &
                                   TwistedSF(K,1,I,J,1)*TwistedSF(K,1,L,J,1) &
                                 + TwistedSF(K,2,I,J,1)*TwistedSF(K,2,L,J,1) )
            AxRedBld   (K,I,L,J) = AxRdBld(I,L)
         ENDDO       ! L - All blade DOFs
      ENDDO          ! I - All blade DOFs

      IF ( J /= 1 )  THEN  ! All but the innermost blade element
   ! Add the effects from the (not yet used) portion of element J-1

         DO I = 1,3     ! Loop through all blade DOFs
            DO L = 1,3  ! Loop through all blade DOFs
               AxRedBld(K,I,L,J) = AxRedBld(K,I,L,J) + AxRedBld(K,I,L,J-1)   &
                                 + AxRdBldOld(I,L)
            ENDDO       ! L - All blade DOFs
         ENDDO          ! I - All blade DOFs
      ENDIF


   ! Store the TwstdSF and AxRdBld terms of the current element (these will be used for the next element)

      TwstdSFOld = TwstdSF
      AxRdBldOld = AxRdBld


   ENDDO ! J - Blade nodes / elements


   ! Apply the flapwise modal stiffness tuners of the blades to KBF():

   DO I = 1,2     ! Loop through flap DOFs
      DO L = 1,2  ! Loop through flap DOFs
         KBF(K,I,L) = SQRT( FStTunr(K,I)*FStTunr(K,L) )*KBF(K,I,L)
      ENDDO       ! L - Flap DOFs
   ENDDO          ! I - Flap DOFs


   ! Calculate the blade natural frequencies:


   DO I = 1,2     ! Loop through flap DOFs
      FreqBF(K,I,1) = Inv2Pi*SQRT(   KBF(K,I,I)                   /( MBF(K,I,I) - TipMass(K) ) )   ! Natural blade I-flap frequency w/o centrifugal stiffening nor     tip mass effects
      FreqBF(K,I,2) = Inv2Pi*SQRT(   KBF(K,I,I)                   /  MBF(K,I,I)                )   ! Natural blade I-flap frequency w/o centrifugal stiffening, but w/ tip mass effects
      FreqBF(K,I,3) = Inv2Pi*SQRT( ( KBF(K,I,I) + KBFCent(K,I,I) )/  MBF(K,I,I)                )   ! Natural blade I-flap frequency w/  centrifugal stiffening and     tip mass effects
   ENDDO          ! I - Flap DOFs

   FreqBE   (K,1,1) = Inv2Pi*SQRT(   KBE(K,1,1)                   /( MBE(K,1,1) - TipMass(K) ) )   ! Natural blade 1-edge frequency w/o centrifugal stiffening nor      tip mass effects
   FreqBE   (K,1,2) = Inv2Pi*SQRT(   KBE(K,1,1)                   /  MBE(K,1,1)                )   ! Natural Blade 1-edge frequency w/o  centrifugal stiffening, but w/ tip mass effects
   FreqBE   (K,1,3) = Inv2Pi*SQRT( ( KBE(K,1,1) + KBECent(K,1,1) )/  MBE(K,1,1)                )   ! Natural Blade 1-edge frequency w/  centrifugal stiffening and      tip mass effects


   ! Calculate the generalized damping of the blades:

   DO I = 1,2     ! Loop through flap DOFs
      DO L = 1,2  ! Loop through flap DOFs
         CBF(K,I,L) = ( 0.01*BldFDamp(K,L) )*KBF(K,I,L)/( Pi*FreqBF(K,L,1) )
      ENDDO       ! L - Flap DOFs
   ENDDO          ! I - Flap DOFs

   CBE      (K,1,1) = ( 0.01*BldEDamp(K,1) )*KBE(K,1,1)/( Pi*FreqBE(K,1,1) )


   ! Calculate the 2nd derivatives of the twisted shape functions at the tip:

   Shape  = SHP( 1.0, BldFlexL, BldFl1Sh(:,K), 2 )
   TwistedSF(K,1,1,TipNode,2) =  Shape*CThetaS(K,BldNodes)        ! 2nd deriv. of Phi1(TipNode) for blade K
   TwistedSF(K,2,1,TipNode,2) = -Shape*SThetaS(K,BldNodes)        ! 2nd deriv. of Psi1(TipNode) for blade K

   Shape  = SHP( 1.0, BldFlexL, BldFl2Sh(:,K), 2 )
   TwistedSF(K,1,2,TipNode,2) =  Shape*CThetaS(K,BldNodes)        ! 2nd deriv. of Phi2(TipNode) for blade K
   TwistedSF(K,2,2,TipNode,2) = -Shape*SThetaS(K,BldNodes)        ! 2nd deriv. of Psi2(TipNode) for blade K

   Shape  = SHP( 1.0, BldFlexL, BldEdgSh(:,K), 2 )
   TwistedSF(K,1,3,TipNode,2) =  Shape*SThetaS(K,BldNodes)        ! 2nd deriv. of Phi3(TipNode) for blade K
   TwistedSF(K,2,3,TipNode,2) =  Shape*CThetaS(K,BldNodes)        ! 2nd deriv. of Psi3(TipNode) for blade K


   ! Integrate to find the 1st and zeroeth derivatives of the twisted shape functions
   !   at the tip:

   DO I = 1,2     ! Loop through Phi and Psi
      DO L = 1,3  ! Loop through all blade DOFs
         TwistedSF(K,I,L,TipNode,1) = TwistedSF(K,I,L,BldNodes,1) + TwstdSFOld(I,L,1)
         TwistedSF(K,I,L,TipNode,0) = TwistedSF(K,I,L,BldNodes,0) + TwstdSFOld(I,L,0)
      ENDDO       ! L - All blade DOFs
   ENDDO          ! I - Phi and Psi


   ! Integrate to find the blade axial reduction shape functions at the tip:

   DO I = 1,3     ! Loop through all blade DOFs
      DO L = 1,3  ! Loop through all blade DOFs
         AxRedBld(K,I,L,TipNode) = AxRedBld(K,I,L,BldNodes) + AxRdBldOld(I,L)
      ENDDO       ! L - All blade DOFs
   ENDDO          ! I - All blade DOFs


ENDDO ! K - Blades



   ! Calculate the tower-top mass:

TwrTpMass = RotMass + RFrlMass + BoomMass + TFinMass + NacMass + YawBrMass


DO J = TwrNodes,1,-1 ! Loop through the tower nodes / elements in reverse


   ! Calculate the mass of the current element

   ElmntMass    = MassT(J)*DHNodes(J)     ! Mass of tower element J


   ! Integrate to find the tower mass which will be output in .fsm

   TwrMass      = TwrMass + ElmntMass


   ! Integrate to find TMssAbvNd:

   TMssAbvNd   (J) = 0.5*ElmntMass

   IF ( J == TwrNodes )  THEN ! Uppermost tower element
   ! Add the TwrTpMass effects:

      TMssAbvNd(J) = TMssAbvNd(J) + TwrTpMass
   ELSE                       ! All other tower elements
   ! Add to TMssAbvNd(J) the effects from the (not yet used) portion of element J+1

      TMssAbvNd(J) = 0.5*ElMassOld + TMssAbvNd(J) + TMssAbvNd(J+1)
   ENDIF


   ! Store the mass of the current element (this will be used for the next element)

   ElMassOld    = ElmntMass


ENDDO ! J - Tower nodes / elements in reverse



   ! Initialize the generalized tower masses using tower-top mass effects:

DO I = 1,2  ! Loop through all tower modes in a single direction
   MTFA(I,I) = TwrTpMass
   MTSS(I,I) = TwrTpMass
ENDDO       ! I - All tower modes in a single direction


DO J = 1,TwrNodes    ! Loop through the tower nodes / elements


   ! Calculate the tower shape functions (all derivatives):

   TwrFASF(1,J,2) = SHP( HNodesNorm(J), TwrFlexL, TwFAM1Sh(:), 2 )
   TwrFASF(2,J,2) = SHP( HNodesNorm(J), TwrFlexL, TwFAM2Sh(:), 2 )
   TwrFASF(1,J,1) = SHP( HNodesNorm(J), TwrFlexL, TwFAM1Sh(:), 1 )
   TwrFASF(2,J,1) = SHP( HNodesNorm(J), TwrFlexL, TwFAM2Sh(:), 1 )
   TwrFASF(1,J,0) = SHP( HNodesNorm(J), TwrFlexL, TwFAM1Sh(:), 0 )
   TwrFASF(2,J,0) = SHP( HNodesNorm(J), TwrFlexL, TwFAM2Sh(:), 0 )

   TwrSSSF(1,J,2) = SHP( HNodesNorm(J), TwrFlexL, TwSSM1Sh(:), 2 )
   TwrSSSF(2,J,2) = SHP( HNodesNorm(J), TwrFlexL, TwSSM2Sh(:), 2 )
   TwrSSSF(1,J,1) = SHP( HNodesNorm(J), TwrFlexL, TwSSM1Sh(:), 1 )
   TwrSSSF(2,J,1) = SHP( HNodesNorm(J), TwrFlexL, TwSSM2Sh(:), 1 )
   TwrSSSF(1,J,0) = SHP( HNodesNorm(J), TwrFlexL, TwSSM1Sh(:), 0 )
   TwrSSSF(2,J,0) = SHP( HNodesNorm(J), TwrFlexL, TwSSM2Sh(:), 0 )


   ! Integrate to find the generalized mass of the tower (including tower-top mass effects).
   !   Ignore the cross-correlation terms of MTFA (i.e. MTFA(i,j) where i /= j) and MTSS
   !   since these terms will never be used.

   ElmntMass      = MassT(J)*DHNodes(J)                           ! Mass of tower element J

   DO I = 1,2     ! Loop through all tower DOFs in one direction
      MTFA  (I,I) = MTFA  (I,I) + ElmntMass*TwrFASF(I,J,0)*TwrFASF(I,J,0)
      MTSS  (I,I) = MTSS  (I,I) + ElmntMass*TwrSSSF(I,J,0)*TwrSSSF(I,J,0)
   ENDDO          ! I - through all tower DOFs in one direction


   ! Integrate to find the generalized stiffness of the tower (not including gravitational
   !    effects).

   ElStffFA       = StiffTFA(J)*DHNodes(J)                        ! Fore-aft stiffness of tower element J
   ElStffSS       = StiffTSS(J)*DHNodes(J)                        ! Side-to-side stiffness of tower element J

   DO I = 1,2     ! Loop through all tower DOFs in one direction
      DO L = 1,2  ! Loop through all tower DOFs in one direction
         KTFA (I,L) = KTFA    (I,L) + ElStffFA *TwrFASF(I,J,2)*TwrFASF(L,J,2)
         KTSS (I,L) = KTSS    (I,L) + ElStffSS *TwrSSSF(I,J,2)*TwrSSSF(L,J,2)
      ENDDO       ! L - All tower DOFs in one direction
   ENDDO          ! I - through all tower DOFs in one direction


   ! Integrate to find the gravitational-term of the generalized stiffness of the tower.
   !   Ignore the cross-correlation terms of KTFAGrav (i.e. KTFAGrav(i,j) where i /= j)
   !   and KTSSGrav since these terms will never be used.

   ElmntStff      = -TMssAbvNd(J)*DHNodes(J)*Gravity              ! Gravitational stiffness of tower element J

   DO I = 1,2     ! Loop through all tower DOFs in one direction
      KTFAGrav(I,I) = KTFAGrav(I,I) + ElmntStff*TwrFASF(I,J,1)*TwrFASF(I,J,1)
      KTSSGrav(I,I) = KTSSGrav(I,I) + ElmntStff*TwrSSSF(I,J,1)*TwrSSSF(I,J,1)
   ENDDO


   ! Integrate to find the tower axial reduction shape functions:

   DO I = 1,2     ! Loop through all tower DOFs in one direction
      DO L = 1,2  ! Loop through all tower DOFs in one direction
         AxRdTFA (I,L) = 0.5*DHNodes(J)*TwrFASF(I,J,1)*TwrFASF(L,J,1)
         AxRdTSS (I,L) = 0.5*DHNodes(J)*TwrSSSF(I,J,1)*TwrSSSF(L,J,1)

         AxRedTFA(I,L,J) = AxRdTFA(I,L)
         AxRedTSS(I,L,J) = AxRdTSS(I,L)
      ENDDO       ! L - All tower DOFs in one direction
   ENDDO

   IF ( J /= 1 )  THEN  ! All but the lowermost tower element
   ! Add the effects from the (not yet used) portion of element J-1

      DO I = 1,2     ! Loop through all tower DOFs in one direction
         DO L = 1,2  ! Loop through all tower DOFs in one direction
            AxRedTFA(I,L,J) = AxRedTFA(I,L,J) + AxRedTFA(I,L,J-1)+ AxRdTFAOld(I,L)
            AxRedTSS(I,L,J) = AxRedTSS(I,L,J) + AxRedTSS(I,L,J-1)+ AxRdTSSOld(I,L)
         ENDDO       ! L - All tower DOFs in one direction
      ENDDO
   ENDIF


   ! Store the AxRdTFA and AxRdTSS terms of the current element (these will be used for the next element)

   AxRdTFAOld = AxRdTFA
   AxRdTSSOld = AxRdTSS


ENDDO ! J - Tower nodes / elements


! Apply the modal stiffness tuners of the tower to KTFA() and KTSS():

DO I = 1,2     ! Loop through all tower DOFs in one direction
   DO L = 1,2  ! Loop through all tower DOFs in one direction
      KTFA(I,L) = SQRT( FAStTunr(I)*FAStTunr(L) )*KTFA(I,L)

      KTSS(I,L) = SQRT( SSStTunr(I)*SSStTunr(L) )*KTSS(I,L)
   ENDDO       ! L - All tower DOFs in one direction
ENDDO          ! I - through all tower DOFs in one direction


   ! Calculate the tower natural frequencies:

DO I = 1,2     ! Loop through all tower DOFs in one direction
   FreqTFA(I,1) = Inv2Pi*SQRT(   KTFA(I,I)                  /( MTFA(I,I) - TwrTpMass ) )  ! Natural tower I-fore-aft frequency w/o gravitational destiffening nor tower-top mass effects
   FreqTFA(I,2) = Inv2Pi*SQRT( ( KTFA(I,I) + KTFAGrav(I,I) )/  MTFA(I,I)               )  ! Natural tower I-fore-aft frequency w/  gravitational destiffening and tower-top mass effects
   FreqTSS(I,1) = Inv2Pi*SQRT(   KTSS(I,I)                  /( MTSS(I,I) - TwrTpMass ) )  ! Natural tower I-side-to-side frequency w/o gravitational destiffening nor tower-top mass effects
   FreqTSS(I,2) = Inv2Pi*SQRT( ( KTSS(I,I) + KTSSGrav(I,I) )/  MTSS(I,I)               )  ! Natural tower I-side-to-side frequency w/  gravitational destiffening and tower-top mass effects
ENDDO          ! I - All tower DOFs in one direction


   ! Calculate the generalized damping of the tower:

DO I = 1,2     ! Loop through all tower DOFs in one direction
   DO L = 1,2  ! Loop through all tower DOFs in one direction
      CTFA(I,L) = ( 0.01*TwrFADmp(L) )*KTFA(I,L)/( Pi*FreqTFA(L,1) )

      CTSS(I,L) = ( 0.01*TwrSSDmp(L) )*KTSS(I,L)/( Pi*FreqTSS(L,1) )
   ENDDO       ! L - All tower DOFs in one direction
ENDDO          ! I - All tower DOFs in one direction


   ! Calculate the tower shape functions (all derivatives) at the tower-top:

TwrFASF(1,TTopNode,2) = SHP( 1.0, TwrFlexL, TwFAM1Sh(:), 2 )
TwrFASF(2,TTopNode,2) = SHP( 1.0, TwrFlexL, TwFAM2Sh(:), 2 )
TwrFASF(1,TTopNode,1) = SHP( 1.0, TwrFlexL, TwFAM1Sh(:), 1 )
TwrFASF(2,TTopNode,1) = SHP( 1.0, TwrFlexL, TwFAM2Sh(:), 1 )
TwrFASF(1,TTopNode,0) = SHP( 1.0, TwrFlexL, TwFAM1Sh(:), 0 )
TwrFASF(2,TTopNode,0) = SHP( 1.0, TwrFlexL, TwFAM2Sh(:), 0 )

TwrSSSF(1,TTopNode,2) = SHP( 1.0, TwrFlexL, TwSSM1Sh(:), 2 )
TwrSSSF(2,TTopNode,2) = SHP( 1.0, TwrFlexL, TwSSM2Sh(:), 2 )
TwrSSSF(1,TTopNode,1) = SHP( 1.0, TwrFlexL, TwSSM1Sh(:), 1 )
TwrSSSF(2,TTopNode,1) = SHP( 1.0, TwrFlexL, TwSSM2Sh(:), 1 )
TwrSSSF(1,TTopNode,0) = SHP( 1.0, TwrFlexL, TwSSM1Sh(:), 0 )
TwrSSSF(2,TTopNode,0) = SHP( 1.0, TwrFlexL, TwSSM2Sh(:), 0 )


   ! Integrate to find the tower axial reduction shape functions at the tower-top:

DO I = 1,2     ! Loop through all tower DOFs in one direction
   DO L = 1,2  ! Loop through all tower DOFs in one direction
      AxRedTFA(I,L,TTopNode) = AxRedTFA(I,L,TwrNodes)+ AxRdTFAOld(I,L)
      AxRedTSS(I,L,TTopNode) = AxRedTSS(I,L,TwrNodes)+ AxRdTSSOld(I,L)
   ENDDO       ! L - All tower DOFs in one direction
ENDDO


   ! Calculate the turbine and total masses:

TurbMass  = TwrTpMass + TwrMass
TotalMass = TurbMass + PtfmMass

!bjj start of proposed change
   ! deallocate local variables:

!BJJ CHECK ALLOCATION:
IF (ALLOCATED( FMomAbvNd )) DEALLOCATE ( FMomAbvNd )
IF (ALLOCATED( KBECent   )) DEALLOCATE ( KBECent   )
IF (ALLOCATED( KBFCent   )) DEALLOCATE ( KBFCent   )
IF (ALLOCATED( MBE       )) DEALLOCATE ( MBE       )
IF (ALLOCATED( MBF       )) DEALLOCATE ( MBF       )
IF (ALLOCATED( TMssAbvNd )) DEALLOCATE ( TMssAbvNd )

!BJJ end of proposed change

RETURN
END SUBROUTINE Coeff
!=======================================================================
SUBROUTINE Control


   ! This is the main control routine.


USE                             CoordSys
USE                             DOFs
USE                             DriveTrain
USE                             Features
USE                             General
USE                             InitCond
USE                             NacelleYaw
!bjj rm NWTC_Library: USE                             Precision
USE                             RtHndSid
USE                             SimCont
USE                             TurbConf
USE                             TurbCont


!BJJ Start of proposed change AD_v12.70
USE                             AeroDyn
!USE                             AeroSubs, ONLY: GetHubWind
!BJJ End of proposed change

IMPLICIT                        NONE


   ! Local variables:

REAL(ReKi)                   :: HHWndVec  (3)                                   ! Hub-height wind vector in the AeroDyn coordinate system, m/s.
REAL(ReKi)                   :: LinAccEO  (3)                                   ! Total linear acceleration of the base plate (point O) in the inertia frame (body E for earth).
REAL(ReKi), SAVE             :: NacYawFrct                                      ! Nacelle yaw angle fractions used for the override yaw maneuver calculation.
REAL(ReKi), SAVE             :: NacYawI                                         ! Initial yaw angle at the start of the override yaw maneuver.
REAL(ReKi)                   :: TwrAccel                                        ! Tower Acceleration.  Used for tower feedback control.
REAL(ReKi)                   :: WindDir                                         ! Horizontal hub-height wind direction (positive about the zi-axis), rad.
REAL(ReKi)                   :: YawError                                        ! Nacelle yaw error (positve about the zi-axis), rad.

INTEGER(4)                   :: I                                               ! Generic index
INTEGER(4)                   :: K                                               ! Loops through blades.

!bjj start of proposed change ad v13.00b
INTEGER                      :: ErrStat
!bjj end of proposed change


!bjj chg: LOGICAL(1), SAVE             :: BegYawMan = .TRUE.                              ! .TRUE. before the override yaw manuever has begun (begin yaw manuever).
LOGICAL,    SAVE             :: BegYawMan = .TRUE.                              ! .TRUE. before the override yaw manuever has begun (begin yaw manuever).


   ! Global functions:

!bjj rm DOT_PROD: REAL(ReKi), EXTERNAL         :: DotProd                                         ! A function returning the dot product of two vectors.



   ! ------------------------------ YAW CONTROL -------------------------------
   ! Control yaw if requested:

IF ( ZTime >= TYCOn )  THEN   ! Time now to enable active yaw control.


   SELECT CASE ( YCMode )  ! Which yaw control mode are we using?

   CASE ( 0 )              ! None!


   ! Use the initial yaw angle and rate:

      IF ( DOF_Flag(DOF_Yaw) )  THEN   ! Yaw DOF is currently enabled (use FAST's built-in actuator initial conditions).

         YawPosCom  = YawNeut
         YawRateCom = YawRateNeut

      ELSE                             ! Yaw DOF is currently disabled (no built-in actuator) (use FAST's initial yaw conditions).

         YawPosCom  = NacYaw
         YawRateCom = 0.0

      ENDIF


   CASE ( 1 )              ! User-defined from routine UserYawCont().


   ! Calculate horizontal hub-height wind direction and the nacelle yaw error
   !   estimate (both positive about zi-axis); these are zero if there is no
   !   wind input when AeroDyn is not used:

      IF ( CompAero )  THEN   ! AeroDyn has been used.

!bjj start of proposed change ad v13.00b
!rm         CALL GetHubWind( HHWndVec )
         HHWndVec(:) = AD_GetUndisturbedWind( ZTime, (/ REAL(0.0, ReKi), REAL(0.0, ReKi), FASTHH /), ErrStat )
!bjj end of proposed change

         WindDir  = ATAN2( HHWndVec(2), HHWndVec(1) )
         YawError = WindDir - QT(DOF_Yaw) - QT(DOF_Y)

      ELSE                    ! No AeroDynamics.

         WindDir  = 0.0
         YawError = 0.0

      ENDIF


   ! Call the user-defined yaw control routine:

      CALL UserYawCont ( QT(DOF_Yaw), QDT(DOF_Yaw), WindDir, YawError, NumBl, ZTime, DT, DirRoot, YawPosCom, YawRateCom )


   CASE ( 2 )              ! User-defined from Simulink.


   ! Do nothing here since yaw angle and yaw rate are defined externally from Simulink.


   ENDSELECT


ELSE                          ! Do not control yaw yet, maintain initial yaw angles.


   ! Use the initial yaw angle and rate:

   IF ( DOF_Flag(DOF_Yaw) )  THEN   ! Yaw DOF is currently enabled (use FAST's built-in actuator initial conditions).

      YawPosCom  = YawNeut
      YawRateCom = YawRateNeut

   ELSE                             ! Yaw DOF is currently disabled (no built-in actuator) (use FAST's initial yaw conditions).

      YawPosCom  = NacYaw
      YawRateCom = 0.0

   ENDIF


ENDIF


   ! Override standard yaw control with a linear maneuver if necessary:

IF ( ZTime >= TYawManE )  THEN      ! Override yaw maneuver has ended, yaw is locked at NacYawF.


   YawPosCom     = NacYawF
   YawRateCom    = 0.0


ELSEIF ( ZTime >= TYawManS )  THEN  ! Override yaw maneuver is occuring.


   IF ( BegYawMan )  THEN  ! Override yaw maneuver is just beginning.

      NacYawI    = QT(DOF_Yaw)                                 ! Store the initial (current) yaw, at the start of the yaw maneuver.

      NacYawFrct = ( NacYawF  - NacYawI  ) / &                 ! Calculate the yaw rate (fraction) that will occur during the maneuver.
                   ( TYawManE - TYawManS )


      BegYawMan  = .FALSE.                                     ! Don't enter this part of the IF-structure again

   ENDIF


   YawPosCom     = NacYawI + NacYawFrct*( ZTime - TYawManS )   ! Increment the command yaw
   YawRateCom    = NacYawFrct                                  !   and rate using NacYawFrct


ENDIF


   ! If the yaw DOF is enabled, the command yaw angle and rate become the
   !   neutral yaw angle and rate in FAST's built-in second-order actuator
   !   model defined by inputs YawSpr and YawDamp.  If the yaw DOF is disabled
   !   (no yaw DOF), then the command yaw angle and rate become the actual yaw
   !   angle and rate (no built-in actuator) and the yaw acceleration will be
   !   zero.
   ! NOTE: I don't want to test the value of YawDOF here, since the value of
   !       DOF_Flag(DOF_Yaw) can be controlled by the user-defined routine:

IF ( DOF_Flag(DOF_Yaw) )  THEN   ! Yaw DOF is currently enabled (use FAST's built-in actuator).

   YawNeut              = YawPosCom
   YawRateNeut          = YawRateCom

ELSE                             ! Yaw DOF is currently disabled (no built-in actuator).

   Q  (DOF_Yaw,IC(NMX)) = YawPosCom    ! Update the saved values
   QD (DOF_Yaw,IC(NMX)) = YawRateCom   !   used in routine Solver()
   QT (DOF_Yaw)         = YawPosCom    ! Update the current, intermediate
   QDT(DOF_Yaw)         = YawRateCom   !    values used in routine RtHS()

ENDIF



   ! ----------------------------- PITCH CONTROL ------------------------------
   ! Control pitch if requested:

IF ( ZTime >= TPCOn )  THEN   ! Time now to enable active pitch control.


   SELECT CASE ( PCMode )  ! Which pitch control mode are we using?

   CASE ( 0 )              ! None!


   ! Use the initial blade pitch angles:

      BlPitchCom = BlPitchInit


   CASE ( 1 )              ! User-defined from routine PitchCntrl().


   ! Calculate tower-top acceleration (fore-aft mode only) in the tower-top
   !   system:

      LinAccEO = LinAccEOt
      DO I = 1,NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)
         LinAccEO = LinAccEO + PLinVelEO(PTE(I),0,:)*QD2T(PTE(I))
      ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)

      TwrAccel = DOT_PRODUCT( LinAccEO, b1 )


   ! Call the user-defined pitch control routine:

      CALL PitchCntrl ( BlPitch, ElecPwr, GBRatio*QDT(DOF_GeAz), GBRatio, TwrAccel, NumBl, ZTime, DT, DirRoot, BlPitchCom )


   CASE ( 2 )              ! User-defined from Simulink.


   ! Do nothing here since blade pitch is defined externally from Simulink.


   ENDSELECT


ELSE                          ! Do not control pitch yet, maintain initial pitch angles.


   ! Use the initial blade pitch angles:

   BlPitchCom = BlPitchInit


ENDIF


   ! Override standard pitch control with a linear maneuver if necessary:

DO K = 1,NumBl ! Loop through all blades


   IF ( ZTime >= TPitManE(K) )  THEN      ! Override pitch maneuver has ended, blade is locked at BlPitchF.


      BlPitchCom    (K) = BlPitchF(K)


   ELSEIF ( ZTime >= TPitManS(K) )  THEN  ! Override pitch maneuver is occuring for this blade.


      IF ( BegPitMan(K) )  THEN  ! Override pitch maneuver is just beginning.

         BlPitchI   (K) = BlPitch(K)                                             ! Store the initial (current) pitch, at the start of the pitch maneuver.

         BlPitchFrct(K) = ( BlPitchF(K) - BlPitchI(K) ) / &                      ! Calculate the pitch rate (fraction) that will occur during the maneuver.
                          ( TPitManE(K) - TPitManS(K) )


         BegPitMan  (K) = .FALSE.                                                ! Don't enter this part of the IF-structure again

      ENDIF


      BlPitchCom    (K) = BlPitchI(K) + BlPitchFrct(K)*( ZTime - TPitManS(K) )   ! Increment the blade pitch using BlPitchFrct


   ENDIF


ENDDO ! K - blades


   ! Set the command pitch angles to the actual pitch angles since we have no
   !   built-in pitch actuator:

BlPitch = BlPitchCom



RETURN
END SUBROUTINE Control
!=======================================================================
SUBROUTINE CrossProd(VecResult, Vector1, Vector2)


   ! VecResult = Vector1 X Vector2 (resulting in a vector)


!bjj rm NWTC_Library: USE                             Precision

IMPLICIT NONE


   ! Passed variables:

REAL(ReKi), INTENT(OUT)         :: VecResult (3)   ! = Vector1 X Vector2 (resulting in a vector)
REAL(ReKi), INTENT(IN )         :: Vector1   (3)
REAL(ReKi), INTENT(IN )         :: Vector2   (3)


VecResult(1) = Vector1(2)*Vector2(3) - Vector1(3)*Vector2(2)
VecResult(2) = Vector1(3)*Vector2(1) - Vector1(1)*Vector2(3)
VecResult(3) = Vector1(1)*Vector2(2) - Vector1(2)*Vector2(1)


RETURN
END SUBROUTINE CrossProd
!=======================================================================
!bjj start of proposed change
!rmFUNCTION DotProd(Vector1,Vector2)
!rm!JASON: REPLACE THIS WITH INTRINSIC DOT_PRODUCT()
!rm
!rm   ! DotProd = Vector1 Dot Vector2 (resulting in a scalar)
!rm
!rm
!rm!bjj rm NWTC_Library: USE                             Precision
!rm
!rm
!rmIMPLICIT NONE
!rm
!rm
!rm!rm   ! Passed variables:
!rm
!rmREAL(ReKi)                   :: DotProd
!rmREAL(ReKi), INTENT(IN )      :: Vector1   (3)
!rmREAL(ReKi), INTENT(IN )      :: Vector2   (3)
!rm
!rm
!rmDotProd = Vector1(1)*Vector2(1) + &
!rm          Vector1(2)*Vector2(2) + &
!rm          Vector1(3)*Vector2(3)
!rm
!rm
!rm
!rmRETURN
!rmEND FUNCTION DotProd
!bjj end of proposed change
!=======================================================================
SUBROUTINE DrvTrTrq ( LSS_Spd, GBoxTrq )


   ! This routine calculates the drive-train torque.


USE                             DriveTrain
USE                             General
USE                             Linear
!bjj rm NWTC_Library: USE                             Precision
USE                             SimCont
USE                             TurbConf
USE                             TurbCont

IMPLICIT                        NONE


   ! Passed variables:

REAL(ReKi), INTENT(OUT)      :: GBoxTrq                                         ! Gearbox torque on the LSS side in N-m (output).
REAL(ReKi), INTENT(IN )      :: LSS_Spd                                         ! LSS speed in rad/sec (input).


   ! Local variables:

COMPLEX(ReKi)                :: Current1                                        ! Current passing through the stator (amps)
COMPLEX(ReKi)                :: Current2                                        ! Current passing through the rotor (amps)
COMPLEX(ReKi)                :: Currentm                                        ! Magnitizing current (amps)

REAL(ReKi)                   :: ComDenom                                        ! Common denominator of variables used in the TEC model
REAL(ReKi)                   :: HSS_Spd                                         ! HSS speed in rad/sec.
REAL(ReKi)                   :: HSSBrFrac                                       ! Fraction of full braking torque: 0 (off) <= HSSBrFrac <= 1 (full), (-).
REAL(ReKi)                   :: PwrLossS                                        ! Power loss in the stator (watts)
REAL(ReKi)                   :: PwrLossR                                        ! Power loss in the rotor (watts)
REAL(ReKi)                   :: PwrMech                                         ! Mechanical power (watts)
REAL(ReKi)                   :: Slip                                            ! Generator slip.
REAL(ReKi)                   :: SlipRat                                         ! Generator slip ratio.

!bjj chg: LOGICAL(1), SAVE             :: GenOnLin = .FALSE.                              ! Is the generator online?
LOGICAL,    SAVE             :: GenOnLin = .FALSE.                              ! Is the generator online?
!bjj chg: LOGICAL(1), SAVE             :: Off4Good = .FALSE.                              ! Is the generator offline for good?
LOGICAL,    SAVE             :: Off4Good = .FALSE.                              ! Is the generator offline for good?


   ! Calculate the generator speed.

HSS_Spd = GBRatio*LSS_Spd


   ! See if the generator is on line.

IF ( .NOT. Off4Good )  THEN

   ! The generator is either on-line or has never been turned online.

   IF ( GenOnLin )  THEN   ! The generator is on-line.

      IF ( ( GenTiStp ) .AND. ( ZTime >= TimGenOf ) )  THEN   ! Shut-down of generator determined by time, TimGenOf
         GenOnLin = .FALSE.
         Off4Good = .TRUE.
      ENDIF

   ELSE                    ! The generator has never been turned online.

      IF ( GenTiStr )  THEN   ! Start-up of generator determined by time, TimGenOn
         IF ( ZTime >= TimGenOn )    GenOnLin = .TRUE.
      ELSE                    ! Start-up of generator determined by HSS speed, SpdGenOn
         IF ( HSS_Spd >= SpdGenOn )  GenOnLin = .TRUE.
      ENDIF

   ENDIF

ENDIF


IF ( GenOnLin )  THEN                     ! Generator is on line.


   ! Are we doing simple variable-speed control, or using a generator model?

   SELECT CASE ( VSContrl )               ! Are we using variable-speed control?

   CASE ( 0 )                             ! No variable-speed control.  Using a generator model.


      SELECT CASE ( GenModel )            ! Which generator model are we using?

      CASE ( 1 )                          ! Simple induction-generator model.


         Slip = HSS_Spd - SIG_SySp

         IF ( ABS( Slip ) > SIG_POSl  )  THEN
            GenTrq  = SIGN( SIG_POTq, Slip )
         ELSE
            GenTrq  = Slip*SIG_Slop
         ENDIF
         GenTrq     = GenTrq + DelGenTrq  ! Add the pertubation on generator torque, DelGenTrq.  This is used only for FAST linearization (it is zero otherwise).


   ! The generator efficiency is either additive for motoring,
   !   or subtractive for generating power.

         IF ( GenTrq > 0.0 )  THEN
            ElecPwr = GenTrq*HSS_Spd*GenEff
         ELSE
            ElecPwr = GenTrq*HSS_Spd/GenEff
         ENDIF


      CASE ( 2 )                          ! Thevenin-equivalent generator model.


         SlipRat  = ( HSS_Spd - TEC_SySp )/TEC_SySp

         GenTrq   = TEC_A0*TEC_VLL*TEC_VLL*SlipRat &
                    /( TEC_C0 + TEC_C1*SlipRat + TEC_C2*SlipRat*SlipRat )
         GenTrq   = GenTrq + DelGenTrq ! Add the pertubation on generator torque, DelGenTrq.  This is used only for FAST linearization (it is zero otherwise).

         ComDenom = ( TEC_Re1 - TEC_RRes/SlipRat )**2 + ( TEC_Xe1 + TEC_RLR )**2
         Current2 = CMPLX(  TEC_V1a*( TEC_Re1 - TEC_RRes/SlipRat )/ComDenom , &
                           -TEC_V1a*( TEC_Xe1 + TEC_RLR          )/ComDenom     )
         Currentm = CMPLX( 0.0 , -TEC_V1a/TEC_MR )
         Current1 = Current2 + Currentm
         PwrLossS = 3.0*( ( ABS( Current1 ) )**2 )*TEC_SRes
         PwrLossR = 3.0*( ( ABS( Current2 ) )**2 )*TEC_RRes
         PwrMech  = GenTrq*HSS_Spd
         ElecPwr  = PwrMech - PwrLossS - PwrLossR


      CASE ( 3 )                          ! User-defined generator model.


         CALL UserGen ( HSS_Spd, GBRatio, NumBl, ZTime, DT, GenEff, DelGenTrq, DirRoot, GenTrq, ElecPwr )


      ENDSELECT


   CASE ( 1 )                             ! Simple variable-speed control.


   ! Compute the generator torque, which depends on which region we are in:

      IF ( HSS_Spd >= VS_RtGnSp )  THEN      ! We are in region 3 - torque is constant
         GenTrq = VS_RtTq
      ELSEIF ( HSS_Spd < VS_TrGnSp )  THEN   ! We are in region 2 - torque is proportional to the square of the generator speed
         GenTrq = VS_Rgn2K*HSS_Spd*HSS_Spd
      ELSE                                   ! We are in region 2 1/2 - simple induction generator transition region
         GenTrq = VS_Slope*( HSS_Spd - VS_SySp )
      ENDIF

      GenTrq  = GenTrq + DelGenTrq  ! Add the pertubation on generator torque, DelGenTrq.  This is used only for FAST linearization (it is zero otherwise).


   ! It's not possible to motor using this control scheme,
   !   so the generator efficiency is always subtractive.

      ElecPwr = GenTrq*HSS_Spd*GenEff


   CASE ( 2 )                             ! User-defined variable-speed control for routine UserVSCont().


      CALL UserVSCont ( HSS_Spd, GBRatio, NumBl, ZTime, DT, GenEff, DelGenTrq, DirRoot, GenTrq, ElecPwr )


   CASE ( 3 )                             ! User-defined variable-speed control from Simulink.


   ! No need to define GenTrq or ElecPwr here since this is defined externally
   !   by Simulink.  Also, no reason to perturb generator torque here either,
   !   since linearization does not work with Simulink.


   CASE ( 9999 )                          ! Overridden generator torque caused by trimming generator torque during a FAST linearization analysis (TrimCase == 2)


   ! The generator torque during the trim analysis is computed in SUBROUTINE
   !   FAST_Lin.f90/CalcSteady() and the generator torque pertubation is
   !   computed in FAST_Lin.f90/Linearize(); thus, there is no reason to define
   !   the generator torque here.


   ! The generator efficiency is either additive for motoring,
   !   or subtractive for generating power.

      IF ( GenTrq > 0.0 )  THEN
         ElecPwr = GenTrq*HSS_Spd*GenEff
      ELSE
         ElecPwr = GenTrq*HSS_Spd/GenEff
      ENDIF


   ENDSELECT


   ! Lets turn the generator offline for good if ( GenTiStp = .FALSE. )
   !   .AND. ( ElecPwr <= 0.0 ):

   IF ( ( .NOT. GenTiStp ) .AND. ( ElecPwr <= 0.0 ) ) THEN   ! Shut-down of generator determined by generator power = 0
      GenTrq   = 0.0
      ElecPwr  = 0.0

      GenOnLin = .FALSE.
      Off4Good = .TRUE.
   ENDIF

ELSE                                     ! Generator is off line.

   GenTrq  = 0.0
   ElecPwr = 0.0

ENDIF



   ! Calculate the fraction of applied HSS-brake torque, HSSBrFrac:

IF ( ZTime < THSSBrDp )  THEN    ! HSS brake not deployed yet.


   HSSBrFrac = 0.0


ELSE                             ! HSS brake deployed.


   SELECT CASE ( HSSBrMode )                 ! Which HSS brake model are we using?

   CASE ( 1 )                                ! Simple built-in HSS brake model with linear ramp.

      IF ( ZTime < THSSBrFl )  THEN ! Linear ramp
         HSSBrFrac = ( ZTime - THSSBrDp )/HSSBrDT
      ELSE                          ! Full braking torque
         HSSBrFrac = 1.0
      ENDIF

   CASE ( 2 )                                ! User-defined HSS brake model.

      CALL UserHSSBr ( GenTrq, ElecPwr, HSS_Spd, GBRatio, NumBl, ZTime, DT, DirRoot, HSSBrFrac )

      IF ( ( HSSBrFrac < 0.0 ) .OR. ( HSSBrFrac > 1.0 ) )  &   ! 0 (off) <= HSSBrFrac <= 1 (full); else Abort.
         CALL ProgAbort ( ' HSSBrFrac must be between 0.0 (off) and 1.0 (full) (inclusive).  Fix logic in routine UserHSSBr().' )

   ENDSELECT


ENDIF


   ! Calculate the magnitude of HSS brake torque:

HSSBrTrq = SIGN( HSSBrFrac*HSSBrTqF, HSS_Spd )  ! Scale the full braking torque by the brake torque fraction and make sure the brake torque resists motion.


   ! Make a copy of the current value of HSSBrTrq for future use:

HSSBrTrqC = HSSBrTrq



   ! Add the gearbox losses to total HSS torque and project to the LSS side of
   !   the gearbox.  The gearbox efficiency effects, however, are included in
   !   FAST.f90/RtHS().

GBoxTrq = ( GenTrq + HSSBrTrq )*GBRatio



RETURN
END SUBROUTINE DrvTrTrq
!=======================================================================
SUBROUTINE FixHSSBrTq ( Integrator )


   ! This routine is used to adjust the HSSBrTrq value if the absolute
   !   magnitudue of the HSS brake torque was strong enough to reverse
   !   the direction of the HSS, which is a physically impossible
   !   situation.  The problem arises since we are integrating in
   !   discrete time, not continuous time.


   ! AeroDyn MODULES:

USE                             Switch


   ! FAST MODULES:

USE                             DOFs
USE                             DriveTrain
!bjj rm NWTC_Library: USE                             Precision
USE                             RtHndSid
USE                             SimCont
!bjj Start of proposed change vXX-NWTC_Library
!rmUSE                             SysSubs
!USE                             FAST_SysSubs
!bjj Start of proposed change vXX-NWTC_Library


IMPLICIT                        NONE


   ! Passed variables:

CHARACTER(9), INTENT(IN )    :: Integrator                                      ! A string holding the current integrator being used.


   ! Local variables:

REAL(ReKi)                   :: RqdFrcGeAz                                      ! The force term required to produce RqdQD2GeAz.
REAL(ReKi)                   :: RqdQD2GeAz                                      ! The required QD2T(DOF_GeAz) to cause the HSS to stop rotating.

INTEGER(4)                   :: I                                               ! Loops through all DOFs.
!bjj rm unused:INTEGER(4)                   :: Sttus                                           ! Status returned from an attempt to allocate an array.



   ! Make a copy of the current value of QD2T for future use:

QD2TC = QD2T


   ! The absolute magnitude of the HSS brake must have been too great
   !   that the HSS direction was reversed.  What should have happened
   !   is that the HSS should have stopped rotating.  In other words,
   !   QD(DOF_GeAz,IC(NMX)) should equal zero!  Determining what
   !   QD2T(DOF_GeAz) will make QD(DOF_GeAz,IC(NMX)) = 0, depends on
   !   which integrator we are using.

SELECT CASE (Integrator)

CASE ('Corrector')

   ! Find the required QD2T(DOF_GeAz) to cause the HSS to stop rotating (RqdQD2GeAz).
   ! This is found by solving the corrector formula for QD2(DOF_GeAz,IC(NMX))
   !   when QD(DOF_GeAz,IC(NMX)) equals zero.

   RqdQD2GeAz = ( -      QD (DOF_GeAz,IC(1))/DT24 - 19.0*QD2(DOF_GeAz,IC(1)) &
                  +  5.0*QD2(DOF_GeAz,IC(2))      -      QD2(DOF_GeAz,IC(3))   )/ 9.0

CASE ('Predictor')

   ! Find the required QD2T(DOF_GeAz) to cause the HSS to stop rotating (RqdQD2GeAz).
   ! This is found by solving the predictor formula for QD2(DOF_GeAz,IC(1))
   !   when QD(DOF_GeAz,IC(NMX)) equals zero.

   RqdQD2GeAz = ( -      QD (DOF_GeAz,IC(1))/DT24 + 59.0*QD2(DOF_GeAz,IC(2)) &
                  - 37.0*QD2(DOF_GeAz,IC(3))      +  9.0*QD2(DOF_GeAz,IC(4))   )/55.0

END SELECT


   ! Rearrange the augmented matrix of equations of motion to account
   !   for the known acceleration of the generator azimuth DOF.  To
   !   do this, make the known inertia like an applied force to the
   !   system.  Then set force QD2T(DOF_GeAz) to equal the known
   !   acceleration in the augmented matrix of equations of motion:
   ! Here is how the new equations are derived.  First partition the
   !   augmented matrix as follows, where Qa are the unknown
   !   accelerations, Qb are the known accelerations, Fa are the
   !   known forces, and Fb are the unknown forces:
   !      [Caa Cab]{Qa}={Fa}
   !      [Cba Cbb]{Qb}={Fb}
   !   By rearranging, the equations for the unknown and known
   !   accelerations are as follows:
   !      [Caa]{Qa}={Fa}-[Cab]{Qb} and [I]{Qb}={Qb}
   !   Combining these two sets of equations into one set yields:
   !      [Caa 0]{Qa}={{Fa}-[Cab]{Qb}}
   !      [  0 I]{Qb}={          {Qb}}
   !   Once this equation is solved, the unknown force can be found from:
   !      {Fb}=[Cba]{Qa}+[Cbb]{Qb}

DO I = 1,NActvDOF ! Loop through all active (enabled) DOFs

   AugMat(SrtPS(I),    NAUG) = AugMat(SrtPS(I),NAUG) - AugMat(SrtPS(I),DOF_GeAz)*RqdQD2GeAz  ! {{Fa}-[Cab]{Qb}}
   AugMat(SrtPS(I),DOF_GeAz) = 0.0                                                           ! [0]
   AugMat(DOF_GeAz,SrtPS(I)) = 0.0                                                           ! [0]

ENDDO             ! I - All active (enabled) DOFs

   AugMat(DOF_GeAz,DOF_GeAz) = 1.0                                                           ! [I]{Qb}={Qb}
   AugMat(DOF_GeAz,    NAUG) = RqdQD2GeAz                                                    !


   ! Invert the matrix to solve for the new (updated) accelerations.  Like in
   !   RtHS(), the accelerations are returned by Gauss() in the first NActvDOF
   !   elements of the solution vector, SolnVec().  These are transfered to the
   !   proper index locations of the acceleration vector QD2T() using the
   !   vector subscript array SrtPS(), after Gauss() has been called:
   ! NOTE: QD2T( SrtPS(1:NActvDOF) ) cannot be sent directly because arrays
   !   sections with vector subscripts must not be used in INTENT(OUT)
   !   arguments.

CALL Gauss( AugMat( SrtPS(1:NActvDOF), SrtPSNAUG(1:(NActvDOF+1)) ), NActvDOF, SolnVec )

QD2T = 0.0
DO I = 1,NActvDOF ! Loop through all active (enabled) DOFs
   QD2T(SrtPS(I)) = SolnVec(I)
ENDDO             ! I - All active (enabled) DOFs


   ! Find the force required to produce RqdQD2GeAz from the equations of
   !   motion using the new accelerations:

RqdFrcGeAz = 0.0
DO I = 1,NActvDOF ! Loop through all active (enabled) DOFs
   RqdFrcGeAz = RqdFrcGeAz + OgnlGeAzRo(SrtPS(I))*QD2T(SrtPS(I))  ! {Fb}=[Cba]{Qa}+[Cbb]{Qb}
ENDDO             ! I - All active (enabled) DOFs


   ! Find the HSSBrTrq necessary to bring about this force:

HSSBrTrq = HSSBrTrqC + ( ( OgnlGeAzRo(NAUG) - RqdFrcGeAz )*GBoxEffFac/GBRatio )


   ! Make sure this new HSSBrTrq isn't larger in absolute magnitude than
   !   the original HSSBrTrq.  Indeed, the new HSSBrTrq can't be larger than
   !   the old HSSBrTrq, since the old HSSBrTrq was found solely as a
   !   function of time--and is thus the maximum possible at the current
   !   time.  If the new HSSBrTrq is larger, then the reversal in direction
   !   was caused by factors other than the HSS brake--thus the original HSS
   !   brake torque values were OK to begin with.  Thus, restore the
   !   variables changed by this subroutine, back to their original values:

IF ( ABS( HSSBrTrq ) > ABS( HSSBrTrqC ) )  THEN

   HSSBrTrq = HSSBrTrqC
   QD2T     = QD2TC

ELSE


   ! Use the new accelerations to update the DOF values.  Again, this
   !   depends on the integrator type:

   SELECT CASE (Integrator)

   CASE ('Corrector')

   ! Update QD and QD2 with the new accelerations using the corrector.
   ! This will make QD(DOF_GeAz,IC(NMX)) equal to zero and adjust all
   !    of the other QDs as necessary.
   ! The Q's are unnaffected by this change.

      QD2(:,IC(NMX)) = QD2T

      DO I = 1,NDOF  ! Loop through all DOFs
         QD(I,IC(NMX)) = QD(I,IC(1)) + DT24*( 9.0*QD2(I,IC(NMX)) + 19.0*QD2(I,IC(1)) - 5.0*QD2(I,IC(2)) + QD2(I,IC(3)) )
      ENDDO          ! I - All DOFs

   CASE ('Predictor')

   ! Update QD2 with the new accelerations.  Use IC(1) instead of IC(NMX)
   !   since the IC array has already been incremented.
   ! This will make QD(DOF_GeAz,IC(NMX)) equal to zero and adjust all
   !    of the other QDs as necessary during the next time step.

      QD2(:,IC(  1)) = QD2T

   END SELECT


!JASON: GET RID OF THIS LOGIC WHEN YOU INTERFACE DAVID LAINO'S NEW VERSION OF AeroDyn WITH DYNAMIC INFLOW INSTABILITIES FIXED:
   ! NOTE: I don't like the following IF...THEN construct, but it works.
   !       AeroDyn should be able to prevent itself from exploding when
   !          the rotor slows down!  This shouldn't need to be controlled
   !          by the dynamics program!
   ! Switch to EQUIL Inflow model since many variables in DYNIN are
   !    normalized by tip speed, which is now very small!:

   IF ( DYNINFL .OR. DYNINIT )  THEN   ! .TRUE. if DYNamic INflow model is engaged.

      DYNINFL = .FALSE.
      DYNINIT = .FALSE.


   ! Inform the user of this switch!

      CALL WrOver(' WARNING:                                           ')
      CALL WrScr ('  "DYNIN" InfModel switched to "EQUIL" by FAST to   ')
      CALL WrScr ('    prevent instability of AeroDyn.                 ')
      CALL WrScr ('  This happened since the rotor has nearly stopped. ')
      CALL WrScr ('                                                    ')

      CALL UsrAlarm


! NOTE: This method suggested by D. Laino did not work:
!       Turn off all induction terms since the rotor speed is so low
!          and we don't want to have the DYNamic INflow model explode
!
!      WAKE  = .FALSE.
!      SWIRL = .FALSE.
   END IF


ENDIF



RETURN
END SUBROUTINE FixHSSBrTq
!bjj start of proposed change
!=======================================================================
SUBROUTINE FAST_Terminate( ErrStat )
! This subroutine is called at program termination.  It deallocates variables and closes files.
!----------------------------------------------------------------------------------------------------

   USE            AeroElem
   USE            Blades
   USE            CoordSys
   USE            DOFs
   USE            General                                   ! contains file units, too
   USE            InitCond
   USE            Linear
   USE            MassInert
   USE            Modes
   USE            Output
   USE            RtHndSid
   USE            Tower
   USE            TurbConf
   USE            TurbCont

   INTEGER,       INTENT(OUT) :: ErrStat                    ! Determines if an error was encountered

   !-------------------------------------------------------------------------------------------------
   ! Deallocate arrays
   !-------------------------------------------------------------------------------------------------

      ! MODULE AeroElem

   IF ( ALLOCATED(ADAeroMarkers%Blade                ) ) DEALLOCATE(ADAeroMarkers%Blade                )
   IF ( ALLOCATED(ADAeroMarkers%Hub                  ) ) DEALLOCATE(ADAeroMarkers%Hub                  )
   IF ( ALLOCATED(ADAeroMarkers%RotorFurl            ) ) DEALLOCATE(ADAeroMarkers%RotorFurl            )
   IF ( ALLOCATED(ADAeroMarkers%Nacelle              ) ) DEALLOCATE(ADAeroMarkers%Nacelle              )
   IF ( ALLOCATED(ADAeroMarkers%Tower                ) ) DEALLOCATE(ADAeroMarkers%Tower                )
   IF ( ALLOCATED(ADAeroMarkers%Tail                 ) ) DEALLOCATE(ADAeroMarkers%Tail                 )

   IF ( ALLOCATED(ADAeroLoads%Blade                  ) ) DEALLOCATE(ADAeroLoads%Blade                  )
   IF ( ALLOCATED(ADAeroLoads%Hub                    ) ) DEALLOCATE(ADAeroLoads%Hub                    )
   IF ( ALLOCATED(ADAeroLoads%RotorFurl              ) ) DEALLOCATE(ADAeroLoads%RotorFurl              )
   IF ( ALLOCATED(ADAeroLoads%Nacelle                ) ) DEALLOCATE(ADAeroLoads%Nacelle                )
   IF ( ALLOCATED(ADAeroLoads%Tower                  ) ) DEALLOCATE(ADAeroLoads%Tower                  )
   IF ( ALLOCATED(ADAeroLoads%Tail                   ) ) DEALLOCATE(ADAeroLoads%Tail                   )
   
!   IF ( ALLOCATED(ADCurrentTurbineState%AzimuthAngle ) ) DEALLOCATE(ADCurrentTurbineState%AzimuthAngle )
!   IF ( ALLOCATED(ADCurrentTurbineState%ElementPitch ) ) DEALLOCATE(ADCurrentTurbineState%ElementPitch )
!   IF ( ALLOCATED(ADCurrentTurbineState%RLocal       ) ) DEALLOCATE(ADCurrentTurbineState%RLocal       )
   IF ( ALLOCATED(ADIntrfaceOptions%SetMulTabLoc     ) ) DEALLOCATE(ADIntrfaceOptions%SetMulTabLoc     )
   IF ( ALLOCATED(ADIntrfaceOptions%MulTabLoc        ) ) DEALLOCATE(ADIntrfaceOptions%MulTabLoc        )
   
   IF ( ALLOCATED(ADInterfaceComponents%Blade        ) ) DEALLOCATE(ADInterfaceComponents%Blade        )
   
   

      ! MODULE Blades

   IF ( ALLOCATED(AerCen                             ) ) DEALLOCATE(AerCen                             )
   IF ( ALLOCATED(AeroCent                           ) ) DEALLOCATE(AeroCent                           )
   IF ( ALLOCATED(AeroTwst                           ) ) DEALLOCATE(AeroTwst                           )
   IF ( ALLOCATED(Alpha                              ) ) DEALLOCATE(Alpha                              )
   IF ( ALLOCATED(AxRedBld                           ) ) DEALLOCATE(AxRedBld                           )
   IF ( ALLOCATED(BAlpha                             ) ) DEALLOCATE(BAlpha                             )
   IF ( ALLOCATED(BldEDamp                           ) ) DEALLOCATE(BldEDamp                           )
   IF ( ALLOCATED(BldFDamp                           ) ) DEALLOCATE(BldFDamp                           )
   IF ( ALLOCATED(BlFract                            ) ) DEALLOCATE(BlFract                            )
   IF ( ALLOCATED(BMassDen                           ) ) DEALLOCATE(BMassDen                           )
   IF ( ALLOCATED(CAeroTwst                          ) ) DEALLOCATE(CAeroTwst                          )
   IF ( ALLOCATED(CBE                                ) ) DEALLOCATE(CBE                                )
   IF ( ALLOCATED(CBF                                ) ) DEALLOCATE(CBF                                )
   IF ( ALLOCATED(cgOffBEdg                          ) ) DEALLOCATE(cgOffBEdg                          )
   IF ( ALLOCATED(cgOffBFlp                          ) ) DEALLOCATE(cgOffBFlp                          )
   IF ( ALLOCATED(Chord                              ) ) DEALLOCATE(Chord                              )
   IF ( ALLOCATED(CThetaS                            ) ) DEALLOCATE(CThetaS                            )
   IF ( ALLOCATED(DRNodes                            ) ) DEALLOCATE( DRNodes                           )
   IF ( ALLOCATED(EAOffBEdg                          ) ) DEALLOCATE(EAOffBEdg                          )
   IF ( ALLOCATED(EAOffBFlp                          ) ) DEALLOCATE(EAOffBFlp                          )
   IF ( ALLOCATED(EAStff                             ) ) DEALLOCATE(EAStff                             )
   IF ( ALLOCATED(EdgcgOf                            ) ) DEALLOCATE(EdgcgOf                            )
   IF ( ALLOCATED(EdgEAOf                            ) ) DEALLOCATE(EdgEAOf                            )
   IF ( ALLOCATED(EdgIner                            ) ) DEALLOCATE(EdgIner                            )
   IF ( ALLOCATED(EdgStff                            ) ) DEALLOCATE(EdgStff                            )
   IF ( ALLOCATED(FlpcgOf                            ) ) DEALLOCATE(FlpcgOf                            )
   IF ( ALLOCATED(FlpEAOf                            ) ) DEALLOCATE(FlpEAOf                            )
   IF ( ALLOCATED(FlpIner                            ) ) DEALLOCATE(FlpIner                            )
   IF ( ALLOCATED(FlpStff                            ) ) DEALLOCATE(FlpStff                            )
   IF ( ALLOCATED(FStTunr                            ) ) DEALLOCATE(FStTunr                            )
   IF ( ALLOCATED(GJStff                             ) ) DEALLOCATE(GJStff                             )
   IF ( ALLOCATED(InerBEdg                           ) ) DEALLOCATE(InerBEdg                           )
   IF ( ALLOCATED(InerBFlp                           ) ) DEALLOCATE(InerBFlp                           )
   IF ( ALLOCATED(KBE                                ) ) DEALLOCATE(KBE                                )
   IF ( ALLOCATED(KBF                                ) ) DEALLOCATE(KBF                                )
   IF ( ALLOCATED(MassB                              ) ) DEALLOCATE(MassB                              )
   IF ( ALLOCATED(PrecrvRef                          ) ) DEALLOCATE(PrecrvRef                          )
   IF ( ALLOCATED(PreswpRef                          ) ) DEALLOCATE(PreswpRef                          )
   IF ( ALLOCATED(RefAxisxb                          ) ) DEALLOCATE(RefAxisxb                          )
   IF ( ALLOCATED(RefAxisyb                          ) ) DEALLOCATE(RefAxisyb                          )
   IF ( ALLOCATED(RNodes                             ) ) DEALLOCATE(RNodes                             )
   IF ( ALLOCATED(RNodesNorm                         ) ) DEALLOCATE(RNodesNorm                         )
   IF ( ALLOCATED(rSAerCenn1                         ) ) DEALLOCATE(rSAerCenn1                         )
   IF ( ALLOCATED(rSAerCenn2                         ) ) DEALLOCATE(rSAerCenn2                         )
   IF ( ALLOCATED(SAeroTwst                          ) ) DEALLOCATE(SAeroTwst                          )
   IF ( ALLOCATED(StiffBE                            ) ) DEALLOCATE(StiffBE                            )
   IF ( ALLOCATED(StiffBEA                           ) ) DEALLOCATE(StiffBEA                           )
   IF ( ALLOCATED(StiffBF                            ) ) DEALLOCATE(StiffBF                            )
   IF ( ALLOCATED(StiffBGJ                           ) ) DEALLOCATE(StiffBGJ                           )
   IF ( ALLOCATED(SThetaS                            ) ) DEALLOCATE(SThetaS                            )
   IF ( ALLOCATED(StrcTwst                           ) ) DEALLOCATE(StrcTwst                           )
   IF ( ALLOCATED(ThetaS                             ) ) DEALLOCATE(ThetaS                             )
   IF ( ALLOCATED(TwistedSF                          ) ) DEALLOCATE(TwistedSF                          )

      ! MODULE CoordSys

   IF ( ALLOCATED(i1                                 ) ) DEALLOCATE(i1                                 )
   IF ( ALLOCATED(i2                                 ) ) DEALLOCATE(i2                                 )
   IF ( ALLOCATED(i3                                 ) ) DEALLOCATE(i3                                 )
   IF ( ALLOCATED(j1                                 ) ) DEALLOCATE(j1                                 )
   IF ( ALLOCATED(j2                                 ) ) DEALLOCATE(j2                                 )
   IF ( ALLOCATED(j3                                 ) ) DEALLOCATE(j3                                 )
   IF ( ALLOCATED(m1                                 ) ) DEALLOCATE(m1                                 )
   IF ( ALLOCATED(m2                                 ) ) DEALLOCATE(m2                                 )
   IF ( ALLOCATED(m3                                 ) ) DEALLOCATE(m3                                 )
   IF ( ALLOCATED(n1                                 ) ) DEALLOCATE(n1                                 )
   IF ( ALLOCATED(n2                                 ) ) DEALLOCATE(n2                                 )
   IF ( ALLOCATED(n3                                 ) ) DEALLOCATE(n3                                 )
   IF ( ALLOCATED(t1                                 ) ) DEALLOCATE(t1                                 )
   IF ( ALLOCATED(t2                                 ) ) DEALLOCATE(t2                                 )
   IF ( ALLOCATED(t3                                 ) ) DEALLOCATE(t3                                 )
   IF ( ALLOCATED(te1                                ) ) DEALLOCATE(te1                                )
   IF ( ALLOCATED(te2                                ) ) DEALLOCATE(te2                                )
   IF ( ALLOCATED(te3                                ) ) DEALLOCATE(te3                                )


      ! MODULE DOFs

   IF ( ALLOCATED(Q                                  ) ) DEALLOCATE(Q                                  )
   IF ( ALLOCATED(QD                                 ) ) DEALLOCATE(QD                                 )
   IF ( ALLOCATED(QD2                                ) ) DEALLOCATE(QD2                                )
   IF ( ALLOCATED(Diag                               ) ) DEALLOCATE(Diag                               )
   IF ( ALLOCATED(DOF_BE                             ) ) DEALLOCATE(DOF_BE                             )
   IF ( ALLOCATED(DOF_BF                             ) ) DEALLOCATE(DOF_BF                             )
   IF ( ALLOCATED(IC                                 ) ) DEALLOCATE(IC                                 )
   IF ( ALLOCATED(NPSBE                              ) ) DEALLOCATE(NPSBE                              )
   IF ( ALLOCATED(NPSE                               ) ) DEALLOCATE(NPSE                               )
   IF ( ALLOCATED(PA                                 ) ) DEALLOCATE(PA                                 )
   IF ( ALLOCATED(PB                                 ) ) DEALLOCATE(PB                                 )
   IF ( ALLOCATED(PCE                                ) ) DEALLOCATE(PCE                                )
   IF ( ALLOCATED(PDE                                ) ) DEALLOCATE(PDE                                )
   IF ( ALLOCATED(PF                                 ) ) DEALLOCATE(PF                                 )
   IF ( ALLOCATED(PG                                 ) ) DEALLOCATE(PG                                 )
   IF ( ALLOCATED(PH                                 ) ) DEALLOCATE(PH                                 )
   IF ( ALLOCATED(PIE                                ) ) DEALLOCATE(PIE                                )
   IF ( ALLOCATED(PL                                 ) ) DEALLOCATE(PL                                 )
   IF ( ALLOCATED(PM                                 ) ) DEALLOCATE(PM                                 )
   IF ( ALLOCATED(PN                                 ) ) DEALLOCATE(PN                                 )
   IF ( ALLOCATED(PTE                                ) ) DEALLOCATE(PTE                                )
   IF ( ALLOCATED(PTTE                               ) ) DEALLOCATE(PTTE                               )
   IF ( ALLOCATED(PR                                 ) ) DEALLOCATE(PR                                 )
   IF ( ALLOCATED(PS                                 ) ) DEALLOCATE(PS                                 )
   IF ( ALLOCATED(PSBE                               ) ) DEALLOCATE(PSBE                               )
   IF ( ALLOCATED(PSE                                ) ) DEALLOCATE(PSE                                )
   IF ( ALLOCATED(PUE                                ) ) DEALLOCATE(PUE                                )
   IF ( ALLOCATED(PX                                 ) ) DEALLOCATE(PX                                 )
   IF ( ALLOCATED(PYE                                ) ) DEALLOCATE(PYE                                )
   IF ( ALLOCATED(SrtPS                              ) ) DEALLOCATE(SrtPS                              )
   IF ( ALLOCATED(PH                                 ) ) DEALLOCATE(PH                                 )
   IF ( ALLOCATED(SrtPSNAUG                          ) ) DEALLOCATE(SrtPSNAUG                          )
   IF ( ALLOCATED(DOF_Flag                           ) ) DEALLOCATE(DOF_Flag                           )
   IF ( ALLOCATED(DOF_Desc                           ) ) DEALLOCATE(DOF_Desc                           )


      ! MODULE General

   IF ( ALLOCATED(BldFile                            ) ) DEALLOCATE(BldFile                            )


      ! MODULE InitCond

   IF ( ALLOCATED(BlPitchInit                        ) ) DEALLOCATE(BlPitchInit                        )
   IF ( ALLOCATED(DOF_FlagInit                       ) ) DEALLOCATE(DOF_FlagInit                       )

      ! MODULE Linear

   IF ( ALLOCATED(QD2op                              ) ) DEALLOCATE(QD2op                              )
   IF ( ALLOCATED(QDop                               ) ) DEALLOCATE(QDop                               )
   IF ( ALLOCATED(Qop                                ) ) DEALLOCATE(Qop                                )


      ! MODULE MassInert

   IF ( ALLOCATED(BldCG                              ) ) DEALLOCATE(BldCG                              )
   IF ( ALLOCATED(BldMass                            ) ) DEALLOCATE(BldMass                            )
   IF ( ALLOCATED(FirstMom                           ) ) DEALLOCATE(FirstMom                           )
   IF ( ALLOCATED(SecondMom                          ) ) DEALLOCATE(SecondMom                          )
   IF ( ALLOCATED(TipMass                            ) ) DEALLOCATE(TipMass                            )


      ! MODULE Modes

   IF ( ALLOCATED(BldEdgSh                           ) ) DEALLOCATE(BldEdgSh                           )
   IF ( ALLOCATED(BldFl1Sh                           ) ) DEALLOCATE(BldFl1Sh                           )
   IF ( ALLOCATED(BldFl2Sh                           ) ) DEALLOCATE(BldFl2Sh                           )
   IF ( ALLOCATED(FreqBE                             ) ) DEALLOCATE(FreqBE                             )
   IF ( ALLOCATED(FreqBF                             ) ) DEALLOCATE(FreqBF                             )
   IF ( ALLOCATED(CalcBModes                         ) ) DEALLOCATE(CalcBModes                         )


      ! MODULE Output

   IF ( ALLOCATED(LinAccES                           ) ) DEALLOCATE(LinAccES                           )
   IF ( ALLOCATED(LinAccET                           ) ) DEALLOCATE(LinAccET                           )
   IF ( ALLOCATED(FrcS0B                             ) ) DEALLOCATE(FrcS0B                             )
   IF ( ALLOCATED(FTHydro                            ) ) DEALLOCATE(FTHydro                            )
   IF ( ALLOCATED(MFHydro                            ) ) DEALLOCATE(MFHydro                            )
   IF ( ALLOCATED(MomH0B                             ) ) DEALLOCATE(MomH0B                             )
   IF ( ALLOCATED(OutData                            ) ) DEALLOCATE(OutData                            )
   IF ( ALLOCATED(OutInd                             ) ) DEALLOCATE(OutInd                             )
   IF ( ALLOCATED(OutSign                            ) ) DEALLOCATE(OutSign                            )
   IF ( ALLOCATED(OutParam                           ) ) DEALLOCATE(OutParam                           )


      ! MODULE RtHndSid

   IF ( ALLOCATED(AngAccEFt                          ) ) DEALLOCATE(AngAccEFt                          )
   IF ( ALLOCATED(AngPosHM                           ) ) DEALLOCATE(AngPosHM                           )
   IF ( ALLOCATED(AugMat                             ) ) DEALLOCATE(AugMat                             )
   IF ( ALLOCATED(FrcS0Bt                            ) ) DEALLOCATE(FrcS0Bt                            )
   IF ( ALLOCATED(FSAero                             ) ) DEALLOCATE(FSAero                             )
   IF ( ALLOCATED(FSTipDrag                          ) ) DEALLOCATE(FSTipDrag                          )
   IF ( ALLOCATED(FTAero                             ) ) DEALLOCATE(FTAero                             )
   IF ( ALLOCATED(FTHydrot                           ) ) DEALLOCATE(FTHydrot                           )
   IF ( ALLOCATED(LinAccESt                          ) ) DEALLOCATE(LinAccESt                          )
   IF ( ALLOCATED(LinAccETt                          ) ) DEALLOCATE(LinAccETt                          )
   IF ( ALLOCATED(LinVelESm2                         ) ) DEALLOCATE(LinVelESm2                         )
   IF ( ALLOCATED(MFAero                             ) ) DEALLOCATE(MFAero                             )
   IF ( ALLOCATED(MFHydrot                           ) ) DEALLOCATE(MFHydrot                           )
   IF ( ALLOCATED(MomH0Bt                            ) ) DEALLOCATE(MomH0Bt                            )
   IF ( ALLOCATED(MMAero                             ) ) DEALLOCATE(MMAero                             )
   IF ( ALLOCATED(PAngVelEA                          ) ) DEALLOCATE(PAngVelEA                          )
   IF ( ALLOCATED(PAngVelEB                          ) ) DEALLOCATE(PAngVelEB                          )
   IF ( ALLOCATED(PAngVelEF                          ) ) DEALLOCATE(PAngVelEF                          )
   IF ( ALLOCATED(PAngVelEG                          ) ) DEALLOCATE(PAngVelEG                          )
   IF ( ALLOCATED(PAngVelEH                          ) ) DEALLOCATE(PAngVelEH                          )
   IF ( ALLOCATED(PAngVelEL                          ) ) DEALLOCATE(PAngVelEL                          )
   IF ( ALLOCATED(PAngVelEM                          ) ) DEALLOCATE(PAngVelEM                          )
   IF ( ALLOCATED(PAngVelEN                          ) ) DEALLOCATE(PAngVelEN                          )
   IF ( ALLOCATED(PAngVelER                          ) ) DEALLOCATE(PAngVelER                          )
   IF ( ALLOCATED(PAngVelEX                          ) ) DEALLOCATE(PAngVelEX                          )
   IF ( ALLOCATED(PFrcONcRt                          ) ) DEALLOCATE(PFrcONcRt                          )
   IF ( ALLOCATED(PFrcPRot                           ) ) DEALLOCATE(PFrcPRot                           )
   IF ( ALLOCATED(PFrcS0B                            ) ) DEALLOCATE(PFrcS0B                            )
   IF ( ALLOCATED(PFrcT0Trb                          ) ) DEALLOCATE(PFrcT0Trb                          )
   IF ( ALLOCATED(PFrcVGnRt                          ) ) DEALLOCATE(PFrcVGnRt                          )
   IF ( ALLOCATED(PFrcWTail                          ) ) DEALLOCATE(PFrcWTail                          )
   IF ( ALLOCATED(PFrcZAll                           ) ) DEALLOCATE(PFrcZAll                           )
   IF ( ALLOCATED(PFTHydro                           ) ) DEALLOCATE(PFTHydro                           )
   IF ( ALLOCATED(PLinVelEC                          ) ) DEALLOCATE(PLinVelEC                          )
   IF ( ALLOCATED(PLinVelED                          ) ) DEALLOCATE(PLinVelED                          )
   IF ( ALLOCATED(PLinVelEI                          ) ) DEALLOCATE(PLinVelEI                          )
   IF ( ALLOCATED(PLinVelEIMU                        ) ) DEALLOCATE(PLinVelEIMU                        )
   IF ( ALLOCATED(PLinVelEJ                          ) ) DEALLOCATE(PLinVelEJ                          )
   IF ( ALLOCATED(PLinVelEK                          ) ) DEALLOCATE(PLinVelEK                          )
   IF ( ALLOCATED(PLinVelEO                          ) ) DEALLOCATE(PLinVelEO                          )
   IF ( ALLOCATED(PLinVelEP                          ) ) DEALLOCATE(PLinVelEP                          )
   IF ( ALLOCATED(PLinVelEQ                          ) ) DEALLOCATE(PLinVelEQ                          )
   IF ( ALLOCATED(PLinVelES                          ) ) DEALLOCATE(PLinVelES                          )
   IF ( ALLOCATED(PLinVelET                          ) ) DEALLOCATE(PLinVelET                          )
   IF ( ALLOCATED(PLinVelEU                          ) ) DEALLOCATE(PLinVelEU                          )
   IF ( ALLOCATED(PLinVelEV                          ) ) DEALLOCATE(PLinVelEV                          )
   IF ( ALLOCATED(PLinVelEW                          ) ) DEALLOCATE(PLinVelEW                          )
   IF ( ALLOCATED(PLinVelEY                          ) ) DEALLOCATE(PLinVelEY                          )
   IF ( ALLOCATED(PLinVelEZ                          ) ) DEALLOCATE(PLinVelEZ                          )
   IF ( ALLOCATED(PMFHydro                           ) ) DEALLOCATE(PMFHydro                           )
   IF ( ALLOCATED(PMomBNcRt                          ) ) DEALLOCATE(PMomBNcRt                          )
   IF ( ALLOCATED(PMomH0B                            ) ) DEALLOCATE(PMomH0B                            )
   IF ( ALLOCATED(PMomLPRot                          ) ) DEALLOCATE(PMomLPRot                          )
   IF ( ALLOCATED(PMomNGnRt                          ) ) DEALLOCATE(PMomNGnRt                          )
   IF ( ALLOCATED(PMomNTail                          ) ) DEALLOCATE(PMomNTail                          )
   IF ( ALLOCATED(PMomX0Trb                          ) ) DEALLOCATE(PMomX0Trb                          )
   IF ( ALLOCATED(PMomXAll                           ) ) DEALLOCATE(PMomXAll                           )
   IF ( ALLOCATED(QDT                                ) ) DEALLOCATE(QDT                                )
   IF ( ALLOCATED(QD2T                               ) ) DEALLOCATE(QD2T                               )
   IF ( ALLOCATED(QD2TC                              ) ) DEALLOCATE(QD2TC                              )
   IF ( ALLOCATED(OgnlGeAzRo                         ) ) DEALLOCATE(OgnlGeAzRo                         )
   IF ( ALLOCATED(QT                                 ) ) DEALLOCATE(QT                                 )
   IF ( ALLOCATED(rQS                                ) ) DEALLOCATE(rQS                                )
   IF ( ALLOCATED(rS                                 ) ) DEALLOCATE(rS                                 )
   IF ( ALLOCATED(rS0S                               ) ) DEALLOCATE(rS0S                               )
   IF ( ALLOCATED(rT0T                               ) ) DEALLOCATE(rT0T                               )
   IF ( ALLOCATED(rZT                                ) ) DEALLOCATE(rZT                                )
   IF ( ALLOCATED(SolnVec                            ) ) DEALLOCATE(SolnVec                            )


      ! MODULE Tower


   IF ( ALLOCATED(AxRedTFA                           ) ) DEALLOCATE(AxRedTFA                           )
   IF ( ALLOCATED(AxRedTSS                           ) ) DEALLOCATE(AxRedTSS                           )
   IF ( ALLOCATED(CAT                                ) ) DEALLOCATE(CAT                                )
   IF ( ALLOCATED(CDT                                ) ) DEALLOCATE(CDT                                )
   IF ( ALLOCATED(cgOffTFA                           ) ) DEALLOCATE(cgOffTFA                           )
   IF ( ALLOCATED(cgOffTSS                           ) ) DEALLOCATE(cgOffTSS                           )
   IF ( ALLOCATED(DHNodes                            ) ) DEALLOCATE(DHNodes                            )
   IF ( ALLOCATED(DiamT                              ) ) DEALLOCATE(DiamT                              )
   IF ( ALLOCATED(HNodes                             ) ) DEALLOCATE(HNodes                             )
   IF ( ALLOCATED(HNodesNorm                         ) ) DEALLOCATE(HNodesNorm                         )
   IF ( ALLOCATED(HtFract                            ) ) DEALLOCATE(HtFract                            )
   IF ( ALLOCATED(InerTFA                            ) ) DEALLOCATE(InerTFA                            )
   IF ( ALLOCATED(InerTSS                            ) ) DEALLOCATE(InerTSS                            )
   IF ( ALLOCATED(MassT                              ) ) DEALLOCATE(MassT                              )
   IF ( ALLOCATED(StiffTEA                           ) ) DEALLOCATE(StiffTEA                           )
   IF ( ALLOCATED(StiffTFA                           ) ) DEALLOCATE(StiffTFA                           )
   IF ( ALLOCATED(StiffTGJ                           ) ) DEALLOCATE(StiffTGJ                           )
   IF ( ALLOCATED(StiffTSS                           ) ) DEALLOCATE(StiffTSS                           )
   IF ( ALLOCATED(TMassDen                           ) ) DEALLOCATE(TMassDen                           )
   IF ( ALLOCATED(TwEAStif                           ) ) DEALLOCATE(TwEAStif                           )
   IF ( ALLOCATED(TwFAcgOf                           ) ) DEALLOCATE(TwFAcgOf                           )
   IF ( ALLOCATED(TwFAIner                           ) ) DEALLOCATE(TwFAIner                           )
   IF ( ALLOCATED(TwFAStif                           ) ) DEALLOCATE(TwFAStif                           )
   IF ( ALLOCATED(TwGJStif                           ) ) DEALLOCATE(TwGJStif                           )
   IF ( ALLOCATED(TwrFASF                            ) ) DEALLOCATE(TwrFASF                            )
   IF ( ALLOCATED(TwrSSSF                            ) ) DEALLOCATE(TwrSSSF                            )
   IF ( ALLOCATED(TwSScgOf                           ) ) DEALLOCATE(TwSScgOf                           )
   IF ( ALLOCATED(TwSSIner                           ) ) DEALLOCATE(TwSSIner                           )
   IF ( ALLOCATED(TwSSStif                           ) ) DEALLOCATE(TwSSStif                           )


      ! MODULE TurbConf

   IF ( ALLOCATED(CosPreC                            ) ) DEALLOCATE(CosPreC                            )
   IF ( ALLOCATED(PreCone                            ) ) DEALLOCATE(PreCone                            )
   IF ( ALLOCATED(SinPreC                            ) ) DEALLOCATE(SinPreC                            )

      ! MODULE TurbCont

   IF ( ALLOCATED(BlPitch                            ) ) DEALLOCATE(BlPitch                            )
   IF ( ALLOCATED(BlPitchCom                         ) ) DEALLOCATE(BlPitchCom                         )
   IF ( ALLOCATED(BlPitchF                           ) ) DEALLOCATE(BlPitchF                           )
   IF ( ALLOCATED(BlPitchFrct                        ) ) DEALLOCATE(BlPitchFrct                        )
   IF ( ALLOCATED(BlPitchI                           ) ) DEALLOCATE(BlPitchI                           )
   IF ( ALLOCATED(TBDepISp                           ) ) DEALLOCATE(TBDepISp                           )
   IF ( ALLOCATED(TPitManE                           ) ) DEALLOCATE(TPitManE                           )
   IF ( ALLOCATED(TPitManS                           ) ) DEALLOCATE(TPitManS                           )
   IF ( ALLOCATED(TTpBrDp                            ) ) DEALLOCATE(TTpBrDp                            )
   IF ( ALLOCATED(TTpBrFl                            ) ) DEALLOCATE(TTpBrFl                            )
   IF ( ALLOCATED(BegPitMan                          ) ) DEALLOCATE(BegPitMan                          )

   !-------------------------------------------------------------------------------------------------
   ! Close any open files
   !-------------------------------------------------------------------------------------------------
   CALL CloseEcho()           ! NWTC Library


   CLOSE( UnIn )     !20      ! I/O unit number for the input files.
   CLOSE( UnOu )     !21      ! I/O unit number for the tabular output file.
   CLOSE( UnSu )     !22      ! I/O unit number for the summary output file.
   CLOSE( UnAD )     !23      ! I/O unit number for the ADAMS dataset output file (.adm).
   CLOSE( UnAC )     !24      ! I/O unit number for the ADAMS control output file (.acf) useful for an ADAMS SIMULATE analysis.
   CLOSE( UnAL )     !25      ! I/O unit number for the ADAMS control output file (.acf) useful for an ADAMS LINEAR analysis.
   CLOSE( UnLn )     !26      ! I/O unit number for the FAST linear output file (.lin).
   CLOSE( UnNoSpec ) !27      ! I/O unit number for the noise spectr output file.
   CLOSE( UnNoSPL )  !28      ! I/O unit number for the noise SPL output file.

   !-------------------------------------------------------------------------------------------------
   ! Reset the initialization flag
   !-------------------------------------------------------------------------------------------------
!   Initialized = .FALSE.
   ErrStat     = 0


END SUBROUTINE FAST_Terminate
!bjj end of proposed change
!=======================================================================
SUBROUTINE Gauss( AugMatIn, NumEq, SolnVec )


   ! This routine uses the Gauss-Jordan elimination method for the
   !   solution of a given set of simultaneous linear equations.
   ! NOTE: this routine works if no pivot points are zero and you
   !   don't want the eschelon or reduced eschelon form of the
   !   augmented matrix.  The form of the original augmented matrix
   !   IS preserved in this call.


!bjj rm NWTC_Library: USE                             Precision

IMPLICIT                        NONE


   ! Passed variables:

INTEGER(4), INTENT(IN )      :: NumEq                                           ! Number of equations in augmented matrix.

REAL(ReKi), INTENT(IN )      :: AugMatIn (NumEq,( NumEq + 1 ))                  ! Augmented matrix passed into this subroutine.
REAL(ReKi), INTENT(OUT)      :: SolnVec  (NumEq)                                ! Solution vector.


   ! Local variables:

REAL(ReKi)                   :: AugMat   (NumEq,( NumEq + 1 ))                  ! A copy of the augmented matrix.

INTEGER(4)                   :: I                                               ! Steps through columns
INTEGER(4)                   :: J                                               ! Steps through rows
INTEGER(4)                   :: L                                               ! Steps through rows
INTEGER(4)                   :: NAug                                            ! Column dimension of augmented matrix



   ! Transfer the data from AugMatIn to AugMat:

AugMat = AugMatIn


   ! Find the column dimension of the augmented matrix:

NAug = NumEq + 1


   ! Perform Gauss-Jordan elimination and store the solution vector
   !   in the last column of the augmented matrix:

DO L = 1,NumEq             ! Loop through all rows
   DO I = ( L + 1 ), NAug  ! Loop through all columns above current row number
      AugMat(L,I) = AugMat(L,I) / AugMat(L,L)
      DO J = 1,NumEq       ! Loop through all rows except L
         IF ( J /= L )  AugMat(J,I) = AugMat(J,I) - ( AugMat(J,L)*AugMat(L,I) )
      ENDDO                ! J - All rows except L
   ENDDO                   ! I - All columns above current row number
ENDDO                      ! L - All rows


   ! Transfer the solution vector from AugMat() to SolnVec():

SolnVec = AugMat(:,NAug)



RETURN
END SUBROUTINE Gauss
!=======================================================================
SUBROUTINE InitBlDefl ( K, InitQF1, InitQF2, InitQE1 )


   ! This routine calculates the initial blade deflections.
   ! Base the intial values of the blade DOFs, INITQF1, INITQF2, and
   !   INITQE1, on OoPDefl and IPDefl.
   ! Write messages to the screen if the specified initial tip displacements
   !  are incompatible with the enabled DOFs.


USE                             Blades
USE                             Features
USE                             InitCond
!bjj rm NWTC_Library: USE                             Precision
!bjj Start of proposed change vXX-NWTC_Library
!rmUSE                             SysSubs
!USE                             FAST_SysSubs
!bjj Start of proposed change vXX-NWTC_Library
USE                             TurbCont

IMPLICIT                        NONE


   ! Passed variables:

REAL(ReKi), INTENT(OUT)      :: InitQE1                                         ! Initial edge deflection (output).
REAL(ReKi), INTENT(OUT)      :: InitQF1                                         ! Initial flap deflection for mode 1 (output).
REAL(ReKi), INTENT(OUT)      :: InitQF2                                         ! Initial flap deflection for mode 2 (output).

INTEGER(4), INTENT(IN )      :: K                                               ! Blade number (input).


   ! Local variables:

REAL(ReKi)                   :: A(2,3)                                          ! Augmented matrix for solution of initial deflections.
REAL(ReKi)                   :: CosPitch                                        ! Cosine of the pitch for this blade.
REAL(ReKi)                   :: Det                                             ! Determinate of right-hand side of A.
REAL(ReKi)                   :: SinPitch                                        ! Sine of the pitch for this blade.
REAL(ReKi)                   :: TotResid                                        ! Generator torque.

CHARACTER(*), PARAMETER      :: Approx   = ' An approximate characterization of the specified blade deflection will be made.'
CHARACTER(*), PARAMETER      :: BadIP    = ' Initial blade in-plane tip displacement will be ignored.'
CHARACTER(*), PARAMETER      :: BadOoP   = ' Initial blade out-of-plane tip displacement will be ignored.'
CHARACTER(9)                 :: BladeStr = ' Blade 1:'                          ! String to hold the blade name.
CHARACTER(*), PARAMETER      :: Ignore   = ' All initial blade tip displacements will be ignored.'
CHARACTER(*), PARAMETER      :: Incompat = ' Initial blade tip displacements are Incompat with enabled DOFs.'



   ! Generate a string to hold the blade name.

WRITE (BladeStr(8:8),'(I1)')  K


   ! Calculate the array of deflections(???).

CosPitch = COS( BlPitch(K) )
SinPitch = SIN( BlPitch(K) )

A(1,2) =  TwistedSF(K,1,3,TipNode,0)*CosPitch + TwistedSF(K,2,3,TipNode,0)*SinPitch
A(2,2) = -TwistedSF(K,1,3,TipNode,0)*SinPitch + TwistedSF(K,2,3,TipNode,0)*CosPitch
A(1,3) =  OoPDefl
A(2,3) =  IPDefl

IF ( FlapDOF1 )  THEN                       ! Blade flap mode 1 is enabled

   InitQF2 = 0.0

   A(1,1) =  TwistedSF(K,1,1,TipNode,0)*CosPitch + TwistedSF(K,2,1,TipNode,0)*SinPitch
   A(2,1) = -TwistedSF(K,1,1,TipNode,0)*SinPitch + TwistedSF(K,2,1,TipNode,0)*CosPitch

   DET = ( A(1,1)*A(2,2) - A(1,2)*A(2,1) )

   IF ( DET /= 0.0 )  THEN                  ! Apply all flap deflection to mode 1

      InitQF1 = ( A(1,3)*A(2,2) - A(1,2)*A(2,3) )/DET
      InitQE1 = ( A(1,1)*A(2,3) - A(1,3)*A(2,1) )/DET

   ELSEIF ( .NOT. EdgeDOF )  THEN          ! Blade edge mode 1 is not enabled which caused DET = 0.

      InitQE1 = 0.0

      IF ( A(1,1) /= 0.0 )  THEN

         IF ( A(2,1) /= 0.0 )  THEN         ! Find a solution of the 2 equations in 1 variable that
                                            !  minimizes the sum of the squares of the equation's residuals.

            InitQF1 = ( A(1,1)*A(1,3) + A(2,1)*A(2,3) )/( A(1,1)**2 + A(2,1)**2 )

            TotResid = SQRT( ( A(1,1)*InitQF1 - A(1,3) )**2 + ( A(2,1)*InitQF1 - A(2,3) )**2 )

            IF (TotResid /= 0.0 )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               CALL WrScr ( BladeStr//Approx   )
               CALL WrScr ( '' )
            ENDIF

         ELSE

            InitQF1 = A(1,3)/A(1,1)

            IF ( IPDefl /= 0.0 )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               CALL WrScr ( BladeStr//BadIP    )
               CALL WrScr ( '' )
            ENDIF

         ENDIF

      ELSE

         IF ( A(2,1) /= 0.0 )  THEN

            InitQF1 = A(2,3)/A(2,1)

            IF ( OoPDefl /= 0.0 )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               CALL WrScr ( BladeStr//BadOoP   )
               CALL WrScr ( ' ' )
            ENDIF

         ELSE

            InitQF1 = 0.0

            IF ( ( OoPDefl /= 0.0 ) .OR. ( IPDefl /= 0.0 ) )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               IF ( OoPDefl /= 0.0 )  CALL WrScr ( BladeStr//BadOoP  )
               IF ( IPDefl  /= 0.0 )  CALL WrScr ( BladeStr//BadIP   )
               CALL WrScr ( ' ' )
            ENDIF

         ENDIF
      ENDIF

   ELSE                                     ! It is impossible to find any "good" solution, so ignore the initial tip displacements

      InitQF1 = 0.0
      InitQE1 = 0.0

      IF ( ( OoPDefl /= 0.0 ) .OR. ( IPDefl /= 0.0 ) )  THEN
         CALL UsrAlarm
         CALL WrScr ( BladeStr//Incompat )
         CALL WrScr ( BladeStr//Ignore   )
         CALL WrScr ( ' ' )

      ENDIF

   ENDIF

ELSE                                        ! Blade flap mode 1 is not enabled.

   InitQF1 = 0.0

   IF ( FlapDOF2 )  THEN                    ! Blade flap mode 2 is enabled.

      A(1,1) =  TwistedSF(K,1,2,TipNode,0)*CosPitch + TwistedSF(K,2,2,TipNode,0)*SinPitch
      A(2,1) = -TwistedSF(K,1,2,TipNode,0)*SinPitch + TwistedSF(K,2,2,TipNode,0)*CosPitch

      DET = ( A(1,1)*A(2,2) - A(1,2)*A(2,1) )

      IF ( DET /= 0.0 )  THEN               ! Apply all flap deflection to mode 2
         InitQF2 = ( A(1,3)*A(2,2) - A(1,2)*A(2,3) )/DET
         InitQE1 = ( A(1,1)*A(2,3) - A(1,3)*A(2,1) )/DET

      ELSEIF ( .NOT. EdgeDOF )  THEN          ! Blade edge mode 1 is not enabled which caused DET = 0.

         InitQE1 = 0.0

         IF ( A(1,1) /= 0.0 )  THEN

            IF ( A(2,1) /= 0.0 )  THEN      ! Find a solution of the 2 equations in 1 variable that
                                            !  minimizes the sum of the squares of the equation's residuals
               InitQF2 = ( A(1,1)*A(1,3) + A(2,1)*A(2,3) )/( A(1,1)**2 + A(2,1)**2 )

               TotResid = SQRT( ( A(1,1)*InitQF2 - A(1,3))**2 + ( A(2,1)*InitQF2 - A(2,3) )**2 )

               IF ( TotResid /= 0.0 )  THEN
                  CALL UsrAlarm
                  CALL WrScr ( BladeStr//Incompat )
                  CALL WrScr ( BladeStr//Approx   )
                  CALL WrScr ( ' ' )
               ENDIF
            ELSE
               InitQF2 = A(1,3)/A(1,1)

               IF ( IPDefl /= 0.0 )  THEN
                  CALL UsrAlarm
                  CALL WrScr ( BladeStr//Incompat )
                  CALL WrScr ( BladeStr//BadIP    )
                  CALL WrScr ( ' ' )
               ENDIF
            ENDIF
         ELSE
            IF ( A(2,1) /= 0.0 )  THEN
               InitQF2 = A(2,3)/A(2,1)

               IF ( OoPDefl /= 0.0 )  THEN
                  CALL UsrAlarm
                  CALL WrScr ( BladeStr//Incompat )
                  CALL WrScr ( BladeStr//BadOoP   )
                  CALL WrScr ( ' ' )
               ENDIF
            ELSE
               InitQF2 = 0.0

               IF ( ( OoPDefl /= 0.0 ) .OR. ( IPDefl /= 0.0 ) )  THEN
                  CALL UsrAlarm
                  CALL WrScr ( BladeStr//Incompat )
                  IF ( OoPDefl /= 0.0 )  CALL WrScr ( BladeStr//BadOoP  )
                  IF ( IPDefl  /= 0.0 )  CALL WrScr ( BladeStr//BadIP   )
                  CALL WrScr ( ' ' )
               ENDIF
            ENDIF
         ENDIF

      ELSE                                  ! It is impossible to find any "good" solution, so ignore
                                            ! the initial tip displacements.
         InitQF2 = 0.0
         InitQE1 = 0.0

         IF ( ( OoPDefl /= 0.0 ) .OR. ( IPDefl /= 0.0 ) )  THEN
            CALL UsrAlarm
            CALL WrScr ( BladeStr//Incompat )
            CALL WrScr ( BladeStr//Ignore   )
            CALL WrScr ( ' ' )
          ENDIF
      ENDIF

   ELSE                                     ! Blade flap mode 2 is not enabled.

      InitQF2 = 0.0

      IF ( A(1,2) /= 0.0 )  THEN

         IF ( A(2,2) /= 0.0 )  THEN         ! Find a solution of the 2 equations in 1 variable that minimizes
                                            !  the sum of the squares of the equation's residuals.
            InitQE1 = ( A(1,2)*A(1,3) + A(2,2)*A(2,3) )/( A(1,2)**2 + A(2,2)**2 )

            TotResid = SQRT( ( A(1,2)*InitQE1 - A(1,3) )**2 + ( A(2,2)*InitQE1 - A(2,3) )**2)

            IF ( TotResid /= 0.0 )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               CALL WrScr ( BladeStr//Approx   )
               CALL WrScr ( ' ' )
            ENDIF

         ELSE

            InitQE1 = A(1,3)/A(1,2)

            IF ( IPDefl /= 0.0 )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               CALL WrScr ( BladeStr//BadIP    )
               CALL WrScr ( ' ' )
            ENDIF

         ENDIF

      ELSE

         IF ( A(2,2) /= 0.0 )  THEN

            InitQE1 = A(2,3)/A(2,2)

            IF ( OoPDefl /= 0.0 )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               CALL WrScr ( BladeStr//BadOoP   )
               CALL WrScr ( ' ' )
            ENDIF

         ELSE

            InitQE1 = 0.0

            IF ( ( OoPDefl /= 0. ) .OR. ( IPDefl /= 0. ) )  THEN
               CALL UsrAlarm
               CALL WrScr ( BladeStr//Incompat )
               IF ( OoPDefl /= 0.0 )  CALL WrScr ( BladeStr//BadOoP  )
               IF ( IPDefl  /= 0.0 )  CALL WrScr ( BladeStr//BadIP   )
               CALL WrScr ( ' ' )
            ENDIF

         ENDIF

      ENDIF

   ENDIF

ENDIF


RETURN
END SUBROUTINE InitBlDefl
!=======================================================================
SUBROUTINE Initialize


   ! Initialize sets up starting values for each degree of freedom.
   ! NOTE: DOF_Flag(L) is a LOGICAL array storing the value of the feature
   !       flag for the (L)th DOF.  It is used in routine RtHS() to ensure that
   !       the (L)th DOF acceleration is zero.


!bjj rm NWTC_Library: USE                             Constants
USE                             DOFs
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
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
USE                             FloatingPlatform, ONLY:InitFltngPtfmLd
USE                             General
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
USE                             InitCond
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
USE                             Output
USE                             Platform
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

!bjj rm NWTC_Library: USE                             Precision
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
USE                             SimCont
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
USE                             Waves, ONLY:InitWaves
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

IMPLICIT                        NONE


   ! Local variables:
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
REAL(ReKi), ALLOCATABLE      :: DZNodesPtfm(:)                                  ! Length of variable-length support platform elements (meters)
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

REAL(ReKi)                   :: InitQE1                                         ! Initial value of the 1st blade edge DOF
REAL(ReKi)                   :: InitQF1                                         ! Initial value of the 1st blade flap DOF
REAL(ReKi)                   :: InitQF2                                         ! Initial value of the 2nd blade flap DOF

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
REAL(ReKi), ALLOCATABLE      :: WaveKinzi0 (:)                                  ! zi-coordinates for points along a vertical line passing through the platform reference point where the incident wave kinematics will be computed; these are relative to the mean see level (meters)
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
INTEGER(4)                   :: I                                               ! Loops through all DOFs.
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
INTEGER(4)                   :: J                                               ! Loops through nodes / elements.
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
INTEGER(4)                   :: K                                               ! Loops through blades.

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
INTEGER(4)                   :: NWaveKin0                                       ! Number of points along a vertical line passing through the platform reference point where the incident wave kinematics will be computed (-)
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


INTEGER(4)                   :: Sttus                                           ! Status returned by an attempted allocation.

!bjj rm AD 12.70b CHARACTER(11), EXTERNAL      :: Int2LStr                                        ! A function to convert an interger to a left-justified string.



   ! Allocate many of the variable-length arrays:

CALL Alloc


   ! Initialize the IC array:

IC(1) = 1
DO I = 2,NMX
   IC(I) = IC(1) - I + 1 + NMX
ENDDO


   ! Define DOF index locations for the blade modes:

DO K = 1,NumBl ! Loop through all blades
   DOF_BF(K,1) = 16 + 3*(K-1)       ! 1st blade flap mode--DOFs 16, 19, and 22 for blade 1, 2, and 3, respectively
   DOF_BE(K,1) = 17 + 3*(K-1)       ! 1st blade edge mode--DOFs 17, 20, and 23 for blade 1, 2, and 3, respectively
   DOF_BF(K,2) = 18 + 3*(K-1)       ! 2nd blade flap mode--DOFs 18, 21, and 24 for blade 1, 2, and 3, respectively
ENDDO          ! K - All blades


   ! Define arrays of DOF indices (pointers) that contribute to the angular
   !   velocities of each rigid body of the wind turbine in the inertia frame:
   ! NOTE: We must include ALL of the appropriate DOF indices in these arrays,
   !       not just the indices of the enabled DOFs, since disabling a DOF only
   !       implies that each DOF acceleration is zero--it does not imply
   !       that each DOF velocity is zero (for example, consider disabled
   !       generator DOF, which still spins at constant speed).

NPX    =  3                         ! Number of DOFs that contribute to the angular velocity of the platform                                                  (body X) in the inertia frame.
NPF    =  7                         ! Number of DOFs that contribute to the angular velocity of the tower elements                                            (body F) in the inertia frame.
NPB    =  7                         ! Number of DOFs that contribute to the angular velocity of the tower top / baseplate                                     (body B) in the inertia frame.
NPN    =  8                         ! Number of DOFs that contribute to the angular velocity of the nacelle                                                   (body N) in the inertia frame.
NPR    =  9                         ! Number of DOFs that contribute to the angular velocity of the structure that furls with the rotor (not including rotor) (body R) in the inertia frame.
NPL    = 11                         ! Number of DOFs that contribute to the angular velocity of the low-speed shaft                                           (body L) in the inertia frame.
IF ( NumBl == 2 )  THEN ! 2-blader
   NPH = 12                         ! Number of DOFs that contribute to the angular velocity of the hub                                                       (body H) in the inertia frame.
   NPM = 15                         ! Number of DOFs that contribute to the angular velocity of the blade elements                                            (body M) in the inertia frame.
ELSE                    ! 3-blader
   NPH = 11                         ! Number of DOFs that contribute to the angular velocity of the hub                                                       (body H) in the inertia frame.
   NPM = 14                         ! Number of DOFs that contribute to the angular velocity of the blade elements                                            (body M) in the inertia frame.
ENDIF
NPG    = 10                         ! Number of DOFs that contribute to the angular velocity of the generator                                                 (body G) in the inertia frame.
NPA    =  9                         ! Number of DOFs that contribute to the angular velocity of the tail                                                      (body A) in the inertia frame.


ALLOCATE ( PX(NPX) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the PX array.' )
ENDIF

ALLOCATE ( PF(NPF) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the PF array.' )
ENDIF

ALLOCATE ( PB(NPB) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the PB array.' )
ENDIF

ALLOCATE ( PN(NPN) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the PN array.' )
ENDIF

ALLOCATE ( PR(NPR) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the PR array.' )
ENDIF

ALLOCATE ( PL(NPL) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the PL array.' )
ENDIF

ALLOCATE ( PH(NPH) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the PH array.' )
ENDIF

ALLOCATE ( PM(NumBl,NPM) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the PM array.' )
ENDIF

ALLOCATE ( PG(NPG) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the PG array.' )
ENDIF

ALLOCATE ( PA(NPA) , STAT=Sttus )
IF ( Sttus /= 0 )  THEN
   CALL ProgAbort ( ' Error allocating memory for the PA array.' )
ENDIF


PX            = (/ DOF_R, DOF_P, DOF_Y /)                                                                                          ! Array of DOF indices (pointers) that contribute to the angular velocity of the platform                                                  (body X) in the inertia frame.
PF            = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2 /)                                                  ! Array of DOF indices (pointers) that contribute to the angular velocity of the tower elements                                            (body F) in the inertia frame.
PB            = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2 /)                                                  ! Array of DOF indices (pointers) that contribute to the angular velocity of the tower top / baseplate                                     (body B) in the inertia frame.
PN            = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw /)                                         ! Array of DOF indices (pointers) that contribute to the angular velocity of the nacelle                                                   (body N) in the inertia frame.
PR            = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl /)                               ! Array of DOF indices (pointers) that contribute to the angular velocity of the structure that furls with the rotor (not including rotor) (body R) in the inertia frame.
PL            = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz, DOF_DrTr /)           ! Array of DOF indices (pointers) that contribute to the angular velocity of the low-speed shaft                                           (body L) in the inertia frame.

IF ( NumBl == 2 )  THEN ! 2-blader

   PH         = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz, DOF_DrTr, DOF_Teet /) ! Array of DOF indices (pointers) that contribute to the angular velocity of the hub                                                       (body H) in the inertia frame.
   DO K = 1,NumBl ! Loop through all blades
      PM(K,:) = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz, DOF_DrTr, DOF_Teet, & ! Array of DOF indices (pointers) that contribute to the angular velocity of the blade elements                                            (body M) in the inertia frame.
                   DOF_BF(K,1) , DOF_BE(K,1)    , DOF_BF(K,2)                                                                   /)
   ENDDO          ! K - All blades

ELSE                    ! 3-blader

   PH         = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz, DOF_DrTr /)           ! Array of DOF indices (pointers) that contribute to the angular velocity of the hub                                                       (body H) in the inertia frame.
   DO K = 1,NumBl ! Loop through all blades
      PM(K,:) = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz, DOF_DrTr, &           ! Array of DOF indices (pointers) that contribute to the angular velocity of the blade elements                                            (body M) in the inertia frame.
                   DOF_BF(K,1) , DOF_BE(K,1)    , DOF_BF(K,2)                                                         /)
   ENDDO          ! K - All blades

ENDIF

PG            = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_RFrl, DOF_GeAz /)                     ! Array of DOF indices (pointers) that contribute to the angular velocity of the generator                                                 (body G) in the inertia frame.
PA            = (/ DOF_R, DOF_P, DOF_Y, DOF_TFA1, DOF_TSS1, DOF_TFA2, DOF_TSS2, DOF_Yaw, DOF_TFrl /)                               ! Array of DOF indices (pointers) that contribute to the angular velocity of the tail                                                      (body A) in the inertia frame.


   ! Compute the constant blade and tower properties:

CALL Coeff


   ! Initialize the accelerations to zero.

QD2 = 0.0


   ! Blade motions:

DO K = 1,NumBl   ! Loop through all blades


   ! Calculate the initial blade deflections:

   CALL InitBlDefl ( K, InitQF1, InitQF2, InitQE1 )


   ! Apply these initial blade DOF values to the corresponding
   !   DOFs for this blade.

   Q ( DOF_BF(K,1), 1 ) = InitQF1   !
   Q ( DOF_BF(K,2), 1 ) = InitQF2   ! These come from InitBlDefl().
   Q ( DOF_BE(K,1), 1 ) = InitQE1   !
   QD( DOF_BF(K,1), 1 ) = 0.0
   QD( DOF_BF(K,2), 1 ) = 0.0
   QD( DOF_BE(K,1), 1 ) = 0.0

   DOF_Flag( DOF_BF(K,1) ) = FlapDOF1
   DOF_Flag( DOF_BF(K,2) ) = FlapDOF2
   DOF_Flag( DOF_BE(K,1) ) = EdgeDOF

   DOF_Desc( DOF_BF(K,1) ) = '1st flapwise bending-mode DOF of blade '//TRIM(Int2LStr( K ))// &
                             ' (internal DOF index = DOF_BF('         //TRIM(Int2LStr( K ))//',1))'
   DOF_Desc( DOF_BE(K,1) ) = '1st edgewise bending-mode DOF of blade '//TRIM(Int2LStr( K ))// &
                             ' (internal DOF index = DOF_BE('         //TRIM(Int2LStr( K ))//',1))'
   DOF_Desc( DOF_BF(K,2) ) = '2nd flapwise bending-mode DOF of blade '//TRIM(Int2LStr( K ))// &
                             ' (internal DOF index = DOF_BF('         //TRIM(Int2LStr( K ))//',2))'

ENDDO          ! K - All blades


IF ( NumBl == 2 )  THEN


   ! Teeter Motion

   ! Set initial teeter angle to TeetDefl and initial teeter angular velocity
   !   to 0.

   Q (DOF_Teet,1) = TeetDefl
   QD(DOF_Teet,1) = 0.0

   DOF_Flag(DOF_Teet) = TeetDOF

   DOF_Desc(DOF_Teet) = 'Hub teetering DOF (internal DOF index = DOF_Teet)'

ENDIF


   ! Shaft compliance

   ! The initial shaft compliance displacements and velocities are all zero.
   !   They will remain zero if the drivetrain DOF is disabled:

Q (DOF_DrTr,1) = 0.0
QD(DOF_DrTr,1) = 0.0

DOF_Flag(DOF_DrTr) = DrTrDOF

DOF_Desc(DOF_DrTr) = 'Drivetrain rotational-flexibility DOF (internal DOF index = DOF_DrTr)'


   ! Generator azimuth

   ! Set initial generator azimuth angle.  Turn rotor on, whether it is
   !   fixed or variable speed.  If it is fixed speed, set up the
   !   fixed rpm.

!JASON: CHANGE THESE MOD() FUNCTIONS INTO MODULO() FUNCTIONS SO THAT YOU CAN ELIMINATE ADDING 360:
QAzimInit      = MOD( Azimuth - AzimB1Up + 270.0 + 360.0, 360.0 )*D2R   ! Internal position of blade 1.

Q (DOF_GeAz,1) = QAzimInit
QD(DOF_GeAz,1) = RotSpeed                                               ! Rotor speed in rad/sec.

DOF_Flag(DOF_GeAz) = GenDOF

DOF_Desc(DOF_GeAz) = 'Variable speed generator DOF (internal DOF index = DOF_GeAz)'


   ! Rotor-furl motion

   ! Set initial rotor-furl angle to RotFurl.  If rotor-furl is off, this
   !   becomes a fixed rotor-furl angle.

Q (DOF_RFrl,1) = RotFurl
QD(DOF_RFrl,1) = 0.0

DOF_Flag(DOF_RFrl) = RFrlDOF

DOF_Desc(DOF_RFrl) = 'Rotor-furl DOF (internal DOF index = DOF_RFrl)'


   ! Tail-furl motion

   ! Set initial tail-furl angle to TailFurl.  If tail-furl is off, this
   !   becomes a fixed tail-furl angle.

Q (DOF_TFrl,1) = TailFurl
QD(DOF_TFrl,1) = 0.0

DOF_Flag(DOF_TFrl) = TFrlDOF

DOF_Desc(DOF_TFrl) = 'Tail-furl DOF (internal DOF index = DOF_TFrl)'


   ! Yaw Motion

   ! Set initial yaw angle to NacYaw.  If yaw is off, this becomes a
   !   fixed yaw angle.

Q (DOF_Yaw ,1) = NacYaw
QD(DOF_Yaw ,1) = 0.0

DOF_Flag(DOF_Yaw ) = YawDOF

DOF_Desc(DOF_Yaw ) = 'Nacelle yaw DOF (internal DOF index = DOF_Yaw)'


   ! Tower motion

   ! Assign all the displacements to mode 1 unless it is disabled.  If mode 1
   !   is disabled and mode 2 is enabled, assign all displacements to mode 2.
   ! If both modes are disabled, set the displacements to zero.

Q   (DOF_TFA1,1) =  0.0
Q   (DOF_TSS1,1) =  0.0
Q   (DOF_TFA2,1) =  0.0
Q   (DOF_TSS2,1) =  0.0

IF (    TwFADOF1 )  THEN   ! First fore-aft tower mode is enabled.
   Q(DOF_TFA1,1) =  TTDspFA
ELSEIF( TwFADOF2 )  THEN   ! Second fore-aft tower mode is enabled, but first is not.
   Q(DOF_TFA2,1) =  TTDspFA
ENDIF

IF (    TwSSDOF1 )  THEN   ! First side-to-side tower mode is enabled.
   Q(DOF_TSS1,1) = -TTDspSS
ELSEIF( TwSSDOF2 )  THEN   ! Second side-to-side tower mode is enabled, but first is not.
   Q(DOF_TSS2,1) = -TTDspSS
ENDIF

QD  (DOF_TFA1,1) =  0.0
QD  (DOF_TSS1,1) =  0.0
QD  (DOF_TFA2,1) =  0.0
QD  (DOF_TSS2,1) =  0.0

DOF_Flag(DOF_TFA1) = TwFADOF1
DOF_Flag(DOF_TSS1) = TwSSDOF1
DOF_Flag(DOF_TFA2) = TwFADOF2
DOF_Flag(DOF_TSS2) = TwSSDOF2

DOF_Desc(DOF_TFA1) = '1st tower fore-aft bending mode DOF (internal DOF index = DOF_TFA1)'
DOF_Desc(DOF_TSS1) = '1st tower side-to-side bending mode DOF (internal DOF index = DOF_TSS1)'
DOF_Desc(DOF_TFA2) = '2nd tower fore-aft bending mode DOF (internal DOF index = DOF_TFA2)'
DOF_Desc(DOF_TSS2) = '2nd tower side-to-side bending mode DOF (internal DOF index = DOF_TSS2)'


   ! Platform Motion

   ! Set initial platform displacements.  If platform DOFs are off, these
   !   become fixed platform displacements.

Q (DOF_Sg  ,1) = PtfmSurge
Q (DOF_Sw  ,1) = PtfmSway
Q (DOF_Hv  ,1) = PtfmHeave
Q (DOF_R   ,1) = PtfmRoll
Q (DOF_P   ,1) = PtfmPitch
Q (DOF_Y   ,1) = PtfmYaw
QD(DOF_Sg  ,1) = 0.0
QD(DOF_Sw  ,1) = 0.0
QD(DOF_Hv  ,1) = 0.0
QD(DOF_R   ,1) = 0.0
QD(DOF_P   ,1) = 0.0
QD(DOF_Y   ,1) = 0.0

DOF_Flag(DOF_Sg  ) = PtfmSgDOF
DOF_Flag(DOF_Sw  ) = PtfmSwDOF
DOF_Flag(DOF_Hv  ) = PtfmHvDOF
DOF_Flag(DOF_R   ) = PtfmRDOF
DOF_Flag(DOF_P   ) = PtfmPDOF
DOF_Flag(DOF_Y   ) = PtfmYDOF

DOF_Desc(DOF_Sg  ) = 'Platform horizontal surge translation DOF (internal DOF index = DOF_Sg)'
DOF_Desc(DOF_Sw  ) = 'Platform horizontal sway translation DOF (internal DOF index = DOF_Sw)'
DOF_Desc(DOF_Hv  ) = 'Platform vertical heave translation DOF (internal DOF index = DOF_Hv)'
DOF_Desc(DOF_R   ) = 'Platform roll tilt rotation DOF (internal DOF index = DOF_R)'
DOF_Desc(DOF_P   ) = 'Platform pitch tilt rotation DOF (internal DOF index = DOF_P)'
DOF_Desc(DOF_Y   ) = 'Platform yaw rotation DOF (internal DOF index = DOF_Y)'



   ! Define DOF_FlagInit, which is DOF_Flag during model initialization (at the
   !   start of the simulation):

DOF_FlagInit = DOF_Flag



   ! Calculate the number of active (enabled) DOFs in the model, NActvDOF:

CALL SetEnabledDOFIndexArrays

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, add an undocumented feature for modeling the hydrodynamic loading on
!jmj   a monopile.  Do this by reading in addition inputs from the platform
!jmj   file if they exist:
   ! Initialize the variables associated with the incident waves and
   !   hydrodynamic loading, if necessary:

IF (     ( PtfmModel == 2 ) .AND. CompHydro )  THEN   ! .TRUE. if we have a fixed bottom offshore turbine and we are using the undocumented monopile features.


   ! Increase the value of WaveTMax to ensure that the maximum value within
   !   array WaveTime is at least as large as TMax+DT:

   IF ( ( WaveTMax - WaveDT ) < ( TMax + DT ) )  WaveTMax = TMax + WaveDT + DT   ! NOTE: The + WaveDT is needed because the internal array WaveTime has a maximum value of WaveTMax - WaveDT; the + DT is needed since the simulation time can reach TMax + DT in ADAMS.


   ! Determine the zi-coordinates for points along a vertical line passing
   !   through the platform reference point where the incident wave kinematics
   !   will be computed.  These are the vertical locations of the undeflected
   !   tower nodes relative to the mean sea level when the support platform is
   !   undisplaced:

   NWaveKin0 = TwrNodes

   ALLOCATE ( WaveKinzi0 (NWaveKin0) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the WaveKinzi0 array.')
   ENDIF

   DO J = 1,NWaveKin0   ! Loop through the tower nodes / elements
      WaveKinzi0(J) = TwrRBHt + HNodes(J) - TwrDraft
   ENDDO                ! J - Tower nodes / elements


   ! Initialize the variables used in the undocumented time domain hydrodynamic
   !   loading routines:

   CALL InitWaves ( WtrDens   , WtrDpth   , WaveMod  , WaveStMod, &
                    WaveTMax  , WaveDT    , WaveHs   , WaveTp   , &
                    WavePkShp , WaveDir   , WaveSeed , GHWvFile , &
                    CurrMod   , CurrSSV0  , CurrSSDir, CurrNSRef, &
                    CurrNSV0  , CurrNSDir , CurrDIV  , CurrDIDir, &
                    NWaveKin0 , WaveKinzi0, DHNodes  , NWaveElev, &
                    WaveElevxi, WaveElevyi, Gravity  , DirRoot      )


ELSEIF ( ( PtfmModel == 3 ) .AND. CompHydro )  THEN   ! .TRUE. if we have floating offshore turbine and we are using the undocumented platform features.


   ! Increase the value of WaveTMax to ensure that the maximum value within
   !   array WaveTime is at least as large as TMax+DT:

   IF ( ( WaveTMax - WaveDT ) < ( TMax + DT ) )  WaveTMax = TMax + WaveDT + DT   ! NOTE: The + WaveDT is needed because the internal array WaveTime has a maximum value of WaveTMax - WaveDT; the + DT is needed since the simulation time can reach TMax + DT in ADAMS.


   ! Determine the zi-coordinates for points along a vertical line passing
   !   through the platform reference point where the incident wave kinematics
   !   will be computed.  These are the vertical locations of the support
   !   platform nodes relative to the mean sea level when the support platform
   !   is undisplaced.  Also, determine the lengths of the elements
   !   corresponding to these nodes:

   NWaveKin0 = PtfmNodes

   ALLOCATE ( DZNodesPtfm(NWaveKin0) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the DZNodesPtfm array.')
   ENDIF

   ALLOCATE ( WaveKinzi0 (NWaveKin0) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort(' Error allocating memory for the WaveKinzi0 array.')
   ENDIF

   DO J = 1,NWaveKin0   ! Loop through the platform nodes / elements
      DZNodesPtfm(J) = PtfmDraft/NWaveKin0   ! Lets used constant-spaced nodes for now, but the rest of the code is written to handle variable-spaced nodes--this will be a future input.
      IF ( J == 1 ) THEN   ! Lowest analysis point
         WaveKinzi0(J) = 0.5*DZNodesPtfm(J) - PtfmDraft
      ELSE                 ! All other analysis points
         WaveKinzi0(J) = WaveKinzi0( J - 1 ) + 0.5*( DZNodesPtfm(J) + DZNodesPtfm( J - 1 ) )
      ENDIF
   ENDDO                ! J - Platform nodes / elements


   ! Initialize the variables used in the undocumented time domain hydrodynamic
   !   loading and mooring system dynamics routines:

   CALL InitWaves       ( WtrDens   , WtrDpth   , WaveMod    , WaveStMod, &
                          WaveTMax  , WaveDT    , WaveHs     , WaveTp   , &
                          WavePkShp , WaveDir   , WaveSeed   , GHWvFile , &
                          CurrMod   , CurrSSV0  , CurrSSDir  , CurrNSRef, &
                          CurrNSV0  , CurrNSDir , CurrDIV    , CurrDIDir, &
                          NWaveKin0 , WaveKinzi0, DZNodesPtfm, NWaveElev, &
                          WaveElevxi, WaveElevyi, Gravity    , DirRoot      )
!jmj Start of proposed change.  v6.02b-jmj  15-Nov-2006.
!jmj Replace the hard-coded mooring line restoring calculation with a general
!jmj   purpose, quasi-static solution based on the analytical catenary cable
!jmj   equations with seabed interaction:
!remove6.02b   CALL InitFltngPtfmLd ( WAMITFile , PtfmVol0  , PtfmDiam   , PtfmCD   , &
!remove6.02b                          RdtnTMax  , RdtnDT                                )
   CALL InitFltngPtfmLd ( WAMITFile , PtfmVol0  , PtfmDiam   , PtfmCD   , &
                          RdtnTMax  , RdtnDT    , NumLines   , LineMod  , &
                          LAnchxi   , LAnchyi   , LAnchzi    , LFairxt  , &
                          LFairyt   , LFairzt   , LUnstrLen  , LDiam    , &
                          LMassDen  , LEAStff   , LSeabedCD  , LTenTol  , &
                          LineNodes , LSNodes   , Q(1:6,1)                  )
!jmj End of proposed change.  v6.02b-jmj  15-Nov-2006.

!bjj start of proposed change V6.02D-BJJ
   IF (ALLOCATED( DZNodesPtfm ) ) DEALLOCATE( DZNodesPtfm )
   IF (ALLOCATED( WaveKinzi0 ) )  DEALLOCATE( WaveKinzi0 )
!bjj end of proposed change


ENDIF




!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

RETURN
END SUBROUTINE Initialize
!=======================================================================
SUBROUTINE PtfmLoading


   ! This routine computes the platform loading; that is PtfmAM(1:6,1:6)
   !   and PtfmFt(1:6).

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!jmj Also, remove the reference to an unUSEd MODULE:
!remove6.02aUSE                             DOFs
USE                             FloatingPlatform, ONLY:FltngPtfmLd
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
USE                             General
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Rename MODULE PlatformLd() to Platform():
!remove6.02aUSE                             PlatformLd
USE                             Platform
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
!bjj rm NWTC_Library: USE                             Precision
USE                             RtHndSid
USE                             SimCont

IMPLICIT                        NONE


   ! Local variables:

REAL(ReKi), PARAMETER        :: SymTol   = 9.999E-4                             ! Tolerance used to determine if matrix PtfmAM is symmetric.

INTEGER(4)                   :: I                                               ! Loops through all platform DOFs.
INTEGER(4)                   :: J                                               ! Loops through all platform DOFs.



!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading and
!jmj   mooring system dynamics for floating wind turbines.  Do this by allowing
!jmj   a keyword in place of the integers 0 or 1 in input PtfmLdMod when
!jmj   PtfmModel = 3:
!remove6.02aSELECT CASE ( PtfmLdMod )  ! Which platform loading model are we using?
!remove6.02a
!remove6.02aCASE ( 0 )                 ! None!
!remove6.02a
!remove6.02a
!remove6.02a   ! Do nothing here since PtfmAM and PtfmFt are all initialized to zero.
!remove6.02a
!remove6.02a
!remove6.02aCASE ( 1 )                 ! User-defined platform loading.
!remove6.02a
!remove6.02a
!remove6.02a   ! CALL the user-defined platform loading model:
!remove6.02a
!remove6.02a   CALL UserPtfmLd ( QT(1:6), QDT(1:6), ZTime, DirRoot, PtfmAM, PtfmFt )
!remove6.02a
!remove6.02a
!remove6.02a   ! Ensure that the platform added mass matrix returned by UserPtfmLd,
!remove6.02a   !   PtfmAM, is symmetric; Abort if necessary:
!remove6.02a
!remove6.02a   DO I = 1,5        ! Loop through the 1st 5 rows (columns) of PtfmAM
!remove6.02a
!remove6.02a      DO J = (I+1),6 ! Loop through all columns (rows) passed I
!remove6.02a
!remove6.02a         IF ( ABS( PtfmAM(I,J) - PtfmAM(J,I) ) > SymTol )  &
!remove6.02a            CALL ProgAbort ( ' The user-defined platform added mass matrix is unsymmetric.'// &
!remove6.02a                             '  Make sure PtfmAM returned by UserPtfmLd() is symmetric.'        )
!remove6.02a
!remove6.02a      ENDDO          ! J - All columns (rows) passed I
!remove6.02a
!remove6.02a   ENDDO             ! I - The 1st 5 rows (columns) of PtfmAM
!remove6.02a
!remove6.02a
!remove6.02aENDSELECT
SELECT CASE ( PtfmModel )  ! Which platform model are we using?

CASE ( 0 )                 ! None!


   ! Do nothing here since PtfmAM and PtfmFt are all initialized to zero.


CASE ( 1 )                 ! Onshore.


   SELECT CASE ( PtfmLdMod )  ! Which platform loading model are we using?

   CASE ( 0 )                 ! None!


   ! Do nothing here since PtfmAM and PtfmFt are all initialized to zero.


   CASE ( 1 )                 ! User-defined platform loading.


   ! CALL the user-defined platform loading model:

      CALL UserPtfmLd ( QT(1:6), QDT(1:6), ZTime, DirRoot, PtfmAM, PtfmFt )


   ! Ensure that the platform added mass matrix returned by UserPtfmLd,
   !   PtfmAM, is symmetric; Abort if necessary:

      DO I = 1,5        ! Loop through the 1st 5 rows (columns) of PtfmAM

         DO J = (I+1),6 ! Loop through all columns (rows) passed I

            IF ( ABS( PtfmAM(I,J) - PtfmAM(J,I) ) > SymTol )  &
               CALL ProgAbort ( ' The user-defined platform added mass matrix is unsymmetric.'// &
                                '  Make sure PtfmAM returned by UserPtfmLd() is symmetric.'        )

         ENDDO          ! J - All columns (rows) passed I

      ENDDO             ! I - The 1st 5 rows (columns) of PtfmAM


   ENDSELECT


CASE ( 2 )                 ! Fixed bottom offshore.


   SELECT CASE ( PtfmLdMod )  ! Which platform loading model are we using?

   CASE ( 0 )                 ! None!


   ! Do nothing here since PtfmAM and PtfmFt are all initialized to zero.


   CASE ( 1 )                 ! User-defined platform loading.


   ! CALL the user-defined platform loading model:

      CALL UserPtfmLd ( QT(1:6), QDT(1:6), ZTime, DirRoot, PtfmAM, PtfmFt )


   ! Ensure that the platform added mass matrix returned by UserPtfmLd,
   !   PtfmAM, is symmetric; Abort if necessary:

      DO I = 1,5        ! Loop through the 1st 5 rows (columns) of PtfmAM

         DO J = (I+1),6 ! Loop through all columns (rows) passed I

            IF ( ABS( PtfmAM(I,J) - PtfmAM(J,I) ) > SymTol )  &
               CALL ProgAbort ( ' The user-defined platform added mass matrix is unsymmetric.'// &
                                '  Make sure PtfmAM returned by UserPtfmLd() is symmetric.'        )

         ENDDO          ! J - All columns (rows) passed I

      ENDDO             ! I - The 1st 5 rows (columns) of PtfmAM


   ENDSELECT


CASE ( 3 )                 ! Floating offshore.


   SELECT CASE ( PtfmLdMod )  ! Which platform loading model are we using?

   CASE ( 0 )                 ! None!


   ! Do nothing here since PtfmAM and PtfmFt are all initialized to zero.


   CASE ( 1 )                 ! User-defined platform loading.


   ! CALL the user-defined platform loading model:

      CALL UserPtfmLd ( QT(1:6), QDT(1:6), ZTime, DirRoot, PtfmAM, PtfmFt )


   ! Ensure that the platform added mass matrix returned by UserPtfmLd,
   !   PtfmAM, is symmetric; Abort if necessary:

      DO I = 1,5        ! Loop through the 1st 5 rows (columns) of PtfmAM

         DO J = (I+1),6 ! Loop through all columns (rows) passed I

            IF ( ABS( PtfmAM(I,J) - PtfmAM(J,I) ) > SymTol )  &
               CALL ProgAbort ( ' The user-defined platform added mass matrix is unsymmetric.'// &
                                '  Make sure PtfmAM returned by UserPtfmLd() is symmetric.'        )

         ENDDO          ! J - All columns (rows) passed I

      ENDDO             ! I - The 1st 5 rows (columns) of PtfmAM


   CASE ( 9999 )              ! Undocumented loading for a floating platform.


   ! CALL the undocumented time domain hydrodynamic loading and mooring system
   !   dynamics routine:

      CALL FltngPtfmLd ( QT(1:6), QDT(1:6), ZTime, PtfmAM, PtfmFt )


   ENDSELECT


ENDSELECT
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

RETURN
END SUBROUTINE PtfmLoading
!=======================================================================
SUBROUTINE RFurling( RFrlDef, RFrlRate, RFrlMom )


   ! This routine computes the rotor-furl moment due to rotor-furl deflection
   !   and rate.


USE                             General
!bjj rm NWTC_Library: USE                             Precision
USE                             SimCont
USE                             RotorFurling


IMPLICIT                        NONE


   ! Passed Variables:

REAL(ReKi), INTENT(IN )      :: RFrlDef                                         ! The rotor-furl deflection, QT(DOF_RFrl).
REAL(ReKi), INTENT(OUT)      :: RFrlMom                                         ! The total moment supplied by the springs, and dampers.
REAL(ReKi), INTENT(IN )      :: RFrlRate                                        ! The rotor-furl rate, QDT(DOF_RFrl).


   ! Local variables:

REAL(ReKi)                   :: RFrlDMom                                        ! The moment supplied by the rotor-furl dampers.
REAL(ReKi)                   :: RFrlSMom                                        ! The moment supplied by the rotor-furl springs.



SELECT CASE ( RFrlMod ) ! Which rotor-furl model are we using?

CASE ( 0 )              ! None!


   RFrlMom = 0.0


CASE ( 1 )              ! Standard (using inputs from the FAST furling input file).


   ! Linear spring:

   RFrlSMom = -RFrlSpr*RFrlDef


   ! Add spring-stops:

   IF ( RFrlDef > RFrlUSSP )  THEN       ! Up-stop
      RFrlSMom = RFrlSMom - RFrlUSSpr*( RFrlDef - RFrlUSSP )
   ELSEIF ( RFrlDef < RFrlDSSP )  THEN   ! Down-stop
      RFrlSMom = RFrlSMom - RFrlDSSpr*( RFrlDef - RFrlDSSP )
   ENDIF


   ! Linear damper:

   RFrlDMom = -RFrlDmp*RFrlRate


   ! Add coulomb friction:

   IF ( RFrlRate /= 0.0 )  THEN
      RFrlDMom = RFrlDMom - SIGN( RFrlCDmp, RFrlRate )
   ENDIF


   ! Add damper-stops:

   IF ( RFrlDef > RFrlUSDP )  THEN       ! Up-stop
      RFrlDMom = RFrlDMom - RFrlUSDmp*RFrlRate
   ELSEIF ( RFrlDef < RFrlDSDP )  THEN   ! Down-stop
      RFrlDMom = RFrlDMom - RFrlDSDmp*RFrlRate
   ENDIF


   ! Total up all the moments.

   RFrlMom = RFrlSMom + RFrlDMom


CASE ( 2 )              ! User-defined rotor-furl spring/damper model.


   CALL UserRFrl ( RFrlDef, RFrlRate, ZTime, DirRoot, RFrlMom )


ENDSELECT



RETURN
END SUBROUTINE RFurling
!=======================================================================
SUBROUTINE RtHS


   ! This routine is used to set up and solve the equations of motion
   !   for a particular time step.


USE                             AeroElem
USE                             Blades
USE                             CoordSys
USE                             DOFs
USE                             DriveTrain
USE                             EnvCond
USE                             Features
USE                             General
USE                             InitCond
USE                             MassInert
USE                             NacelleYaw
USE                             Output
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Rename MODULE PlatformLd() to Platform():
!remove6.02aUSE                             PlatformLd
USE                             Platform
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
!bjj rm NWTC_Library: USE                             Precision
USE                             RtHndSid
USE                             SimCont
USE                             TailAero
USE                             TipBrakes
USE                             Tower
USE                             TurbConf
USE                             TurbCont

!bjj start of proposed change - aerodyn loops
USE                             Constants

!USE                             AeroDyn
!USE                             AeroTime,     ONLY: DT, DTaero, TimFlag
!bjj use

IMPLICIT                        NONE


   ! Local variables:

!bjj rm unused:REAL(ReKi)                   :: AeroForces(3)                                   ! Aerodynamic forces (2) and moment (1) per unit span returned by AeroDyn for the current blade element.
REAL(ReKi)                   :: AngAccEAt (3)                                   ! Portion of the angular acceleration of the tail                                                      (body A) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: AngAccEGt (3)                                   ! Portion of the angular acceleration of the generator                                                 (body G) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: AngAccEHt (3)                                   ! Portion of the angular acceleration of the hub                                                       (body H) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: AngAccELt (3)                                   ! Portion of the angular acceleration of the low-speed shaft                                           (body L) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: AngAccENt (3)                                   ! Portion of the angular acceleration of the nacelle                                                   (body N) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:
REAL(ReKi)                   :: AngPosEF  (3)                                   ! Angular position of the current point on the tower (body F) in the inertial frame (body E for earth).
REAL(ReKi)                   :: AngPosEX  (3)                                   ! Angular position of the platform                   (body X) in the inertial frame (body E for earth).
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
REAL(ReKi)                   :: AngVelEA  (3)                                   ! Angular velocity of the tail                                                      (body A) in the inertia frame (body E for earth).
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:
REAL(ReKi)                   :: AngVelEF  (3)                                   ! Angular velocity of the current point on the tower                                (body F) in the inertia frame (body E for earth).
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
REAL(ReKi)                   :: AngVelEG  (3)                                   ! Angular velocity of the generator                                                 (body G) in the inertia frame (body E for earth).
REAL(ReKi)                   :: AngVelEH  (3)                                   ! Angular velocity of the hub                                                       (body H) in the inertia frame (body E for earth).
REAL(ReKi)                   :: AngVelEL  (3)                                   ! Angular velocity of the low-speed shaft                                           (body L) in the inertia frame (body E for earth).
REAL(ReKi)                   :: AngVelEN  (3)                                   ! Angular velocity of the nacelle                                                   (body N) in the inertia frame (body E for earth).
REAL(ReKi)                   :: ElmntMass                                       ! (Temporary) mass of an element.
REAL(ReKi)                   :: EwAXrWI   (3)                                   ! = AngVelEA X rWI
REAL(ReKi)                   :: EwAXrWJ   (3)                                   ! = AngVelEA X rWJ
REAL(ReKi)                   :: EwAXrWK   (3)                                   ! = AngVelEA X rWK
REAL(ReKi)                   :: EwHXrPQ   (3)                                   ! = AngVelEH X rPQ
REAL(ReKi)                   :: EwHXrQC   (3)                                   ! = AngVelEH X rQC
REAL(ReKi)                   :: EwHXrQS   (3)                                   ! = AngVelEH X rQS of the current blade point S.
REAL(ReKi)                   :: EwNXrOU   (3)                                   ! = AngVelEN X rOU
REAL(ReKi)                   :: EwNXrOV   (3)                                   ! = AngVelEN X rOV
REAL(ReKi)                   :: EwNXrOW   (3)                                   ! = AngVelEN X rOW
REAL(ReKi)                   :: EwRXrVD   (3)                                   ! = AngVelER X rVD
REAL(ReKi)                   :: EwRXrVIMU (3)                                   ! = AngVelER X rVIMU
REAL(ReKi)                   :: EwRXrVP   (3)                                   ! = AngVelER X rVP
REAL(ReKi)                   :: EwXXrZO   (3)                                   ! = AngVelEX X rZO
REAL(ReKi)                   :: EwXXrZT   (3)                                   ! = AngVelEX X rZT
REAL(ReKi)                   :: EwXXrZY   (3)                                   ! = AngVelEX X rZY
REAL(ReKi)                   :: GBoxEffFac2                                     ! A second gearbox efficiency factor = ( 1 / GBoxEff^SgnPrvLSTQ - 1 )
REAL(ReKi)                   :: GBoxTrq                                         ! Gearbox torque on the LSS side in N-m.
REAL(ReKi)                   :: LinAccECt (3)                                   ! Portion of the linear acceleration of the hub center of mass                                                              (point C) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEDt (3)                                   ! Portion of the linear acceleration of the center of mass of the structure that furls with the rotor (not including rotor) (point D) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEIt (3)                                   ! Portion of the linear acceleration of the tail boom center of mass                                                        (point I) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEJt (3)                                   ! Portion of the linear acceleration of the tail fin  center of mass                                                        (point J) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEKt (3)                                   ! Portion of the linear acceleration of the tail fin  center of pressure                                                    (point K) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEPt (3)                                   ! Portion of the linear acceleration of the teeter pin                                                                      (point P) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEQt (3)                                   ! Portion of the linear acceleration of the apex of rotation                                                                (point Q) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEUt (3)                                   ! Portion of the linear acceleration of the nacelle center of mass                                                          (point U) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEVt (3)                                   ! Portion of the linear acceleration of the selected point on the rotor-furl axis                                           (point V) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEWt (3)                                   ! Portion of the linear acceleration of the selected point on the  tail-furl axis                                           (point W) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinAccEYt (3)                                   ! Portion of the linear acceleration of the platform center of mass                                                         (point Y) in the inertia frame (body E for earth) associated with everything but the QD2T()'s.
REAL(ReKi)                   :: LinVelEK  (3)                                   ! Linear velocity of tail fin center-of-pressure        (point K) in the inertia frame.
REAL(ReKi)                   :: LinVelES  (3)                                   ! Linear velocity of current point on the current blade (point S) in the inertia frame.
!bjj move to RtHS mod: REAL(ReKi)                   :: LinVelESm2                                      ! The m2-component (closest to tip) of LinVelES.
REAL(ReKi)                   :: LinVelET  (3)                                   ! Linear velocity of current point on the tower         (point T) in the inertia frame.
!bjj start of proposed change - aerodyn loops
REAL(ReKi)                   :: LinVelEPYaw(3) ! This is the linear velocity of the hub in the inertia frame due solely to yaw and rotor-furl effects
!bjj end of proposed change
REAL(ReKi)                   :: LinVelHS  (3)                                   ! Relative linear velocity of the current point on the current blade (point S) in the hub frame (body H)
REAL(ReKi)                   :: LinVelXO  (3)                                   ! Relative linear velocity of the tower-top / base plate             (point O) in the platform  (body X).
REAL(ReKi)                   :: LinVelXT  (3)                                   ! Relative linear velocity of the current point on the tower         (point T) in the platform  (body X).
!bjj rm unused:REAL(ReKi)                   :: MomDrvTr                                        ! Moment resulting from torsion of the drive shaft.
REAL(ReKi)                   :: MomLPRot  (3)                                   ! The total moment on the low-speed shaft at point P caused by the rotor.
REAL(ReKi)                   :: PLinVelHS (3,3)                                 ! Partial  linear velocity of the current point on the current blade (point S) in the hub frame (body H) (this is like a relative partial linear velocity).
REAL(ReKi)                   :: rAerCen   (3)                                   ! Position vector from inertial frame origin to current blade analysis node aerodynamic center.
REAL(ReKi)                   :: RFrlMom                                         ! The total rotor-furl spring and damper moment.
REAL(ReKi)                   :: rK        (3)                                   ! Position vector from inertial frame origin to tail fin center of pressure (point K).
REAL(ReKi)                   :: rOU       (3)                                   ! Position vector from tower-top / base plate (point O) to nacelle center of mass (point U).
REAL(ReKi)                   :: rOV       (3)                                   ! Position vector from tower-top / base plate (point O) to specified point on rotor-furl axis (point V).
REAL(ReKi)                   :: rOW       (3)                                   ! Position vector from tower-top / base plate (point O) to specified point on  tail-furl axis (point W).
!bjj start of proposed change v6.02d-bjj
REAL(ReKi)                   :: rP        (3)                                   ! Position vector from inertial frame origin to teeter pin (point P).
!bjj end of proposed change
REAL(ReKi)                   :: rPAerCen  (3)                                   ! Position vector from teeter pin (point P) to current blade analysis node aerodynamic center.
REAL(ReKi)                   :: rPC       (3)                                   ! Position vector from teeter pin (point P) to hub center of mass (point C).
REAL(ReKi)                   :: rPQ       (3)                                   ! Position vector from teeter pin (point P) to apex of rotation (point Q).
REAL(ReKi)                   :: rPS0      (3)                                   ! Position vector from teeter pin (point P) to blade root (point S(0)).
REAL(ReKi)                   :: rQ        (3)                                   ! Position vector from inertial frame origin to apex of rotation (point Q).
REAL(ReKi)                   :: rQC       (3)                                   ! Position vector from apex of rotation (point Q) to hub center of mass (point C).
!bjj start of proposed change v6.02d-bjj
REAL(ReKi)                   :: rV        (3)                                   ! Position vector from inertial frame origin to specified point on rotor-furl axis (point V).
!bjj end of proposed change v6.02d-bjj
REAL(ReKi)                   :: rVD       (3)                                   ! Position vector from specified point on rotor-furl axis (point V) to center of mass of structure that furls with the rotor (not including rotor) (point D).
REAL(ReKi)                   :: rVIMU     (3)                                   ! Position vector from specified point on rotor-furl axis (point V) to nacelle IMU (point IMU).
REAL(ReKi)                   :: rVP       (3)                                   ! Position vector from specified point on rotor-furl axis (point V) to teeter pin (point P).
REAL(ReKi)                   :: rWI       (3)                                   ! Position vector from specified point on  tail-furl axis (point W) to tail boom center of mass     (point I).
REAL(ReKi)                   :: rWJ       (3)                                   ! Position vector from specified point on  tail-furl axis (point W) to tail fin  center of mass     (point J).
REAL(ReKi)                   :: rWK       (3)                                   ! Position vector from specified point on  tail-furl axis (point W) to tail fin  center of pressure (point K).
REAL(ReKi)                   :: rSAerCen  (3)                                   ! Position vector from a blade analysis node (point S) on the current blade to the aerodynamic center associated with the element.
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:
REAL(ReKi)                   :: rT        (3)                                   ! Position vector from inertial frame origin to the current node (point T(HNodes(J)).
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

REAL(ReKi)                   :: rZT0      (3)                                   ! Position vector from platform reference (point Z) to tower base (point T(0)).
REAL(ReKi)                   :: rZY       (3)                                   ! Position vector from platform reference (point Z) to platform mass center (point Y).
REAL(ReKi)                   :: TeetMom                                         ! The total moment supplied by the stop, spring, and damper of the teeter mechanism.
REAL(ReKi)                   :: TFrlMom                                         ! The total tail-furl spring and damper moment.
REAL(ReKi)                   :: TmpVec    (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TmpVec0   (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TmpVec1   (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TmpVec2   (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TmpVec3   (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TmpVec4   (3)                                   ! A temporary vector used in various computations.
REAL(ReKi)                   :: TmpVec5   (3)                                   ! A temporary vector used in various computations.

INTEGER(4)                   :: I                                               ! Loops through some or all of the DOFs.
INTEGER(4)                   :: J                                               ! Loops through nodes / elements.
INTEGER(4)                   :: K                                               ! Loops through blades.
INTEGER(4)                   :: L                                               ! Generic index
INTEGER(4), SAVE             :: SgnPrvLSTQ = 1                                  ! The sign of the low-speed shaft torque from the previous call to RtHS().  This is calculated at the end of RtHS().  NOTE: The low-speed shaft torque is assumed to be positive at the beginning of the run!

!BJJ START of proposed change
INTEGER                      :: ErrStat
!bjj end of proposed change

   ! Global functions:

!bjj rm DOT_PRODUCT: REAL(ReKi), EXTERNAL         :: DotProd                                         ! A function returning the dot product of two vectors.



   ! Determine how many DOFs are currently enabled (this can change within
   !   user-defined routines) and set vector subscript arrays accordingly:

CALL SetEnabledDOFIndexArrays



   ! Control the turbine's yaw and pitch, except during the first time step and
   !   only during a time-marching analysis (we can't call Control during the
   !   first time step since none of the output parameters needed for feedback
   !   of control measurements are computed until the end of the first time
   !   step):

IF ( ( ZTime > 0.0 ) .AND. ( AnalMode == 1 ) )  CALL Control



   ! Initialize several variables to 0.0:

AugMat      = 0.0


LinAccECt   = 0.0
LinAccEDt   = 0.0
LinAccEIMUt = 0.0
LinAccEIt   = 0.0
LinAccEJt   = 0.0
LinAccEKt   = 0.0
LinAccEOt   = 0.0
LinAccEPt   = 0.0
LinAccEQt   = 0.0
LinAccESt   = 0.0
LinAccETt   = 0.0
LinAccEUt   = 0.0
LinAccEVt   = 0.0
LinAccEWt   = 0.0
LinAccEYt   = 0.0
LinAccEZt   = 0.0



   ! Let's define the coordinate systems that will be used throughout this routine:

CALL SetCoordSy

   !-------------------------------------------------------------------------------------------------
   ! Positions
   !-------------------------------------------------------------------------------------------------

   ! Define the position vectors between the various points on the wind turbine
   !   that are not dependent on the distributed tower or blade parameters:

rZ    = QT(DOF_Sg)* z1 + QT(DOF_Hv)* z2 - QT(DOF_Sw)* z3                ! Position vector from inertia frame origin to platform reference (point Z).
rZY   =      rZYzt* a2                                                  ! Position vector from platform reference (point Z) to platform mass center (point Y).
rZT0  =     rZT0zt* a2                                                  ! Position vector from platform reference (point Z) to tower base (point T(0))
rZO   = ( QT(DOF_TFA1) + QT(DOF_TFA2)                                                 )*a1 & ! Position vector from platform reference (point Z) to tower-top / base plate (point O).
      + ( RefTwrHt - 0.5*(       AxRedTFA(1,1,TTopNode)*QT(DOF_TFA1)*QT(DOF_TFA1) &
                           +     AxRedTFA(2,2,TTopNode)*QT(DOF_TFA2)*QT(DOF_TFA2) &
                           + 2.0*AxRedTFA(1,2,TTopNode)*QT(DOF_TFA1)*QT(DOF_TFA2) &
                           +     AxRedTSS(1,1,TTopNode)*QT(DOF_TSS1)*QT(DOF_TSS1) &
                           +     AxRedTSS(2,2,TTopNode)*QT(DOF_TSS2)*QT(DOF_TSS2) &
                           + 2.0*AxRedTSS(1,2,TTopNode)*QT(DOF_TSS1)*QT(DOF_TSS2)   ) )*a2 &
      + ( QT(DOF_TSS1) + QT(DOF_TSS2)                                                 )*a3
rOU   =    NacCMxn* d1 +    NacCMzn* d2 -    NacCMyn* d3                ! Position vector from tower-top / base plate (point O) to nacelle center of mass (point U).
rOV   =  RFrlPntxn* d1 +  RFrlPntzn* d2 -  RFrlPntyn* d3                ! Position vector from tower-top / base plate (point O) to specified point on rotor-furl axis (point V).
rVIMU =    rVIMUxn*rf1 +    rVIMUzn*rf2 -    rVIMUyn*rf3                ! Position vector from specified point on rotor-furl axis (point V) to nacelle IMU (point IMU).
rVD   =      rVDxn*rf1 +      rVDzn*rf2 -      rVDyn*rf3                ! Position vector from specified point on rotor-furl axis (point V) to center of mass of structure that furls with the rotor (not including rotor) (point D).
rVP   =      rVPxn*rf1 +      rVPzn*rf2 -      rVPyn*rf3 + OverHang*c1  ! Position vector from specified point on rotor-furl axis (point V) to teeter pin (point P).
rPQ   =  -UndSling* g1                                                  ! Position vector from teeter pin (point P) to apex of rotation (point Q).
rQC   =      HubCM* g1                                                  ! Position vector from apex of rotation (point Q) to hub center of mass (point C).
rOW   =  TFrlPntxn* d1 +  TFrlPntzn* d2 -  TFrlPntyn* d3                ! Position vector from tower-top / base plate (point O) to specified point on  tail-furl axis (point W).
rWI   =      rWIxn*tf1 +      rWIzn*tf2 -      rWIyn*tf3                ! Position vector from specified point on  tail-furl axis (point W) to tail boom center of mass     (point I).
rWJ   =      rWJxn*tf1 +      rWJzn*tf2 -      rWJyn*tf3                ! Position vector from specified point on  tail-furl axis (point W) to tail fin  center of mass     (point J).
rWK   =      rWKxn*tf1 +      rWKzn*tf2 -      rWKyn*tf3                ! Position vector from specified point on  tail-furl axis (point W) to tail fin  center of pressure (point K).
rPC   = rPQ + rQC                                                       ! Position vector from teeter pin (point P) to hub center of mass (point C).
rT0O  = rZO - rZT0                                                      ! Position vector from the tower base (point T(0)) to tower-top / base plate (point O).
rO    = rZ  + rZO                                                       ! Position vector from inertial frame origin to tower-top / base plate (point O).
!bjj start of proposed change v6.02d-bjj
!rmrQ    = rO  + rOV + rVP + rPQ                                           ! Position vector from inertial frame origin to apex of rotation (point Q).
rV    = rO  + rOV                                                       ! Position vector from inertial frame origin to specified point on rotor-furl axis (point V)
!rP    = rO  + rOV + rVP                                                 ! Position vector from inertial frame origin to teeter pin (point P).
rP    = rV  + rVP                                                       ! Position vector from inertial frame origin to teeter pin (point P).
rQ    = rP  + rPQ                                                       ! Position vector from inertial frame origin to apex of rotation (point Q).
!bjj end of proposed change
rK    = rO  + rOW + rWK                                                 ! Position vector from inertial frame origin to tail fin center of pressure (point K).

!bjj start of proposed change AeroDyn loops
!JASON can move this to wherever he thinks his brain needs it ;-)

!ADCurrentTurbineState%YawAngle     = ATAN2( c1(3), c1(1) ) ! NOTE: c1(X) = DOT_PRODUCT( c1, zX ) where X = 1,2,3
!ADCurrentTurbineState%TiltAngle    = ATAN2( c1(2), SQRT( c1(1)*c1(1) + c1(3)*c1(3) ) )  ! NOTE: c1(X) = DOT_PRODUCT( c1, zX ) where X = 1,2,3

DO K = 1,NumBl ! Loop through all blades

!   ADCurrentTurbineState%AzimuthAngle(K) = QT(DOF_DrTr) + QT(DOF_GeAz) + 1.5*Pi + ( K - 1 )*TwoPiNB


   ! Calculate the position vector of the tip:

   rS0S(K,TipNode,:) = (   TwistedSF(K,1,1,TipNode,0)*QT( DOF_BF(K,1) ) &  ! Position vector from the blade root (point S(0)) to the blade tip (point S(BldFlexL)).
                         + TwistedSF(K,1,2,TipNode,0)*QT( DOF_BF(K,2) ) &
                         + TwistedSF(K,1,3,TipNode,0)*QT( DOF_BE(K,1) )                          )*j1(K,:) &
                     + (   TwistedSF(K,2,1,TipNode,0)*QT( DOF_BF(K,1) ) &
                         + TwistedSF(K,2,2,TipNode,0)*QT( DOF_BF(K,2) ) &
                         + TwistedSF(K,2,3,TipNode,0)*QT( DOF_BE(K,1) )                          )*j2(K,:) &
                     + ( BldFlexL - 0.5* &
                         (       AxRedBld(K,1,1,TipNode)*QT( DOF_BF(K,1) )*QT( DOF_BF(K,1) ) &
                           +     AxRedBld(K,2,2,TipNode)*QT( DOF_BF(K,2) )*QT( DOF_BF(K,2) ) &
                           +     AxRedBld(K,3,3,TipNode)*QT( DOF_BE(K,1) )*QT( DOF_BE(K,1) ) &
                           + 2.0*AxRedBld(K,1,2,TipNode)*QT( DOF_BF(K,1) )*QT( DOF_BF(K,2) ) &
                           + 2.0*AxRedBld(K,2,3,TipNode)*QT( DOF_BF(K,2) )*QT( DOF_BE(K,1) ) &
                           + 2.0*AxRedBld(K,1,3,TipNode)*QT( DOF_BF(K,1) )*QT( DOF_BE(K,1) )   ) )*j3(K,:)
   rQS (K,TipNode,:) = rS0S(K,TipNode,:) + HubRad*j3(K,:)                  ! Position vector from apex of rotation (point Q) to the blade tip (point S(BldFlexL)).
   rS  (K,TipNode,:) = rQS (K,TipNode,:) + rQ                              ! Position vector from inertial frame origin      to the blade tip (point S(BldFlexL)).


   DO J = 1,BldNodes ! Loop through the blade nodes / elements


   ! Calculate the position vector of the current node:

      rS0S(K,J,:) = (   TwistedSF(K,1,1,J,0)*QT( DOF_BF(K,1) ) &  ! Position vector from the blade root (point S(0)) to the current node (point S(RNodes(J)).
                      + TwistedSF(K,1,2,J,0)*QT( DOF_BF(K,2) ) &
                      + TwistedSF(K,1,3,J,0)*QT( DOF_BE(K,1) )                          )*j1(K,:) &
                  + (   TwistedSF(K,2,1,J,0)*QT( DOF_BF(K,1) ) &
                      + TwistedSF(K,2,2,J,0)*QT( DOF_BF(K,2) ) &
                      + TwistedSF(K,2,3,J,0)*QT( DOF_BE(K,1) )                          )*j2(K,:) &
                  + ( RNodes(J) - 0.5* &
                      (       AxRedBld(K,1,1,J)*QT( DOF_BF(K,1) )*QT( DOF_BF(K,1) ) &
                        +     AxRedBld(K,2,2,J)*QT( DOF_BF(K,2) )*QT( DOF_BF(K,2) ) &
                        +     AxRedBld(K,3,3,J)*QT( DOF_BE(K,1) )*QT( DOF_BE(K,1) ) &
                        + 2.0*AxRedBld(K,1,2,J)*QT( DOF_BF(K,1) )*QT( DOF_BF(K,2) ) &
                        + 2.0*AxRedBld(K,2,3,J)*QT( DOF_BF(K,2) )*QT( DOF_BE(K,1) ) &
                        + 2.0*AxRedBld(K,1,3,J)*QT( DOF_BF(K,1) )*QT( DOF_BE(K,1) )   ) )*j3(K,:)
      rQS (K,J,:) = rS0S(K,J,:) + HubRad*j3(K,:)                  ! Position vector from apex of rotation (point Q) to the current node (point S(RNodes(J)).
      rS  (K,J,:) = rQS (K,J,:) + rQ                              ! Position vector from inertial frame origin      to the current node (point S(RNodes(J)).

      IF ( CompAero )  THEN   ! Calculate the blade element aerodynamic loads using AeroDyn.


   ! Calculate the aerodynamic pitching moment arm (i.e., the position vector
   !   from point S on the blade to the aerodynamic center of the element):

         rSAerCen = rSAerCenn1(K,J)*n1(K,J,:) + rSAerCenn2(K,J)*n2(K,J,:) !bjj: make rSAerCen a matrix? we recalculate it later


   ! Define positions USEd by AeroDyn.

         rPAerCen     = rPQ + rQS(K,J,:) + rSAerCen         ! Position vector from teeter pin (point P)  to blade analysis node aerodynamic center.
         rAerCen      =       rS (K,J,:) + rSAerCen         ! Position vector from inertial frame origin to blade analysis node aerodynamic center.

         ADAeroMarkers%Blade(J,K)%Position(1)      =     rAerCen(1)              ! = the distance from the undeflected tower centerline                                     to the current blade aerodynamic center in the xi ( z1) direction
         ADAeroMarkers%Blade(J,K)%Position(2)      = -1.*rAerCen(3)              ! = the distance from the undeflected tower centerline                                     to the current blade aerodynamic center in the yi (-z3) direction
         ADAeroMarkers%Blade(J,K)%Position(3)      =     rAerCen(2) - PtfmRef    ! = the distance from the nominal tower base position (i.e., the undeflected position of the tower base) to the current blade aerodynamic center in the zi ( z2) direction


!JASON: WE SHOULD REALLY BE PASSING TO AERODYN THE LINEAR VELOCITIES OF THE AERODYNAMIC CENTER IN THE INERTIA FRAME, NOT SIMPLY THE LINEAR VELOCITIES OF POINT S.  IS THERE ANY WAY OF GETTING THIS VELOCITY?<--DO THIS, WHEN YOU ADD THE COUPLED MODE SHAPES!!!!
!         ADAeroMarkers%Blade(J,K)%TranslationVel(:)= (/ LinVelES(1), -1.*LinVelES(3),  LinVelES(2)  /)  !AeroDyn's coordinates

!         ADCurrentTurbineState%RLocal(J,K)          = SQRT(   ( DOT_PRODUCT( rPAerCen, e2) )**2 &    ! = the perpendicular distance from the low-speed shaft to the current blade aerodynamic center.
!                                                            + ( DOT_PRODUCT( rPAerCen, e3) )**2   )
!
!         ADCurrentTurbineState%ElementPitch(J,K)    = BlPitch( K ) + AeroTwst( J )
      END IF  ! CompAero

   END DO !J = 1,BldNodes ! Loop through the blade nodes / elements

END DO !K = 1,NumBl


   ! the hub position should use rQ instead of rP, but the current version of AeroDyn treats
   ! teeter deflections lake blade deflections:
   
!bjj start of proposed change v7.00.01a-bjj
!rm ADInterfaceComponents%Hub%Position(:)       = (/ rP(1), -1.*rP(3), rP(2) /)
!rm 
!rm 
!rm    ! Rotor furl position should be rP instead of rV, but AeroDyn needs this for the
!rm    ! HubVDue2Yaw calculation:
!rm    
!rm ADInterfaceComponents%RotorFurl%Position(:) = (/ rV(1), -1.*rV(3), rV(2) /)
!rm    
!rm ADInterfaceComponents%Nacelle%Position(:)   = (/ rO(1), -1.*rO(3), rO(2) /)
!rm    
!rm    ! Tower base position should be rT(0) instead of rZ, but AeroDyn needs this for
!rm    ! the HubVDue2Yaw calculation:
!rm ADInterfaceComponents%Tower%Position(:)     = (/ rZ(1), -1.*rZ(3), rZ(2) /)
!rm 

ADInterfaceComponents%Hub%Position(:)       = (/ rP(1), -1.*rP(3), rP(2) - PtfmRef /)


   ! Rotor furl position should be rP instead of rV, but AeroDyn needs this for the
   ! HubVDue2Yaw calculation:
   
ADInterfaceComponents%RotorFurl%Position(:) = (/ rV(1), -1.*rV(3), rV(2) - PtfmRef /)
   
ADInterfaceComponents%Nacelle%Position(:)   = (/ rO(1), -1.*rO(3), rO(2) - PtfmRef /)
   
   ! Tower base position should be rT(0) instead of rZ, but AeroDyn needs this for
   ! the HubVDue2Yaw calculation:
ADInterfaceComponents%Tower%Position(:)     = (/ rZ(1), -1.*rZ(3), rZ(2) - PtfmRef /)

!bjj end of proposed change v7.00.01a-bjj
      
   !-------------------------------------------------------------------------------------------------
   ! Orientations - bjj: should this be moved to SetCoordSys ?
   !-------------------------------------------------------------------------------------------------
   
DO K = 1,NumBl   
   DO J = 1,BldNodes   
   
      ADAeroMarkers%Blade(J,K)%Orientation(1,1) =     te1(K,J,1)
      ADAeroMarkers%Blade(J,K)%Orientation(2,1) =     te2(K,J,1)
      ADAeroMarkers%Blade(J,K)%Orientation(3,1) =     te3(K,J,1)
      ADAeroMarkers%Blade(J,K)%Orientation(1,2) = -1.*te1(K,J,3)
      ADAeroMarkers%Blade(J,K)%Orientation(2,2) = -1.*te2(K,J,3)
      ADAeroMarkers%Blade(J,K)%Orientation(3,2) = -1.*te3(K,J,3)
      ADAeroMarkers%Blade(J,K)%Orientation(1,3) =     te1(K,J,2)
      ADAeroMarkers%Blade(J,K)%Orientation(2,3) =     te2(K,J,2)
      ADAeroMarkers%Blade(J,K)%Orientation(3,3) =     te3(K,J,2)

   END DO !J = 1,BldNodes ! Loop through the blade nodes / elements
END DO !K = 1,NumBl

! ADAeroMarkers%Blade(J,K)%Orientation(1,:) = (/ m1(K,J,1),   -1.*m1(K,J,3),    m1(K,J,2) /)  !BJJ: if FAST's arrays are stored with J on the left (inner loop), these loops would probably be faster!
! ADAeroMarkers%Blade(J,K)%Orientation(2,:) = (/ m2(K,J,1),   -1.*m2(K,J,3),    m2(K,J,2) /)
! ADAeroMarkers%Blade(J,K)%Orientation(3,:) = (/ m3(K,J,1),   -1.*m3(K,J,3),    m3(K,J,2) /)
   
   
      ! Blade root orientations should use the j instead of i system, but the current version
      ! of AeroDyn calculates forces normal and tangential to the cone of rotation

ADInterfaceComponents%Blade(:)%Orientation(1,1) =     i1(:,1)
ADInterfaceComponents%Blade(:)%Orientation(2,1) =     i2(:,1)
ADInterfaceComponents%Blade(:)%Orientation(3,1) =     i3(:,1)
ADInterfaceComponents%Blade(:)%Orientation(1,2) = -1.*i1(:,3)
ADInterfaceComponents%Blade(:)%Orientation(2,2) = -1.*i2(:,3)
ADInterfaceComponents%Blade(:)%Orientation(3,2) = -1.*i3(:,3)
ADInterfaceComponents%Blade(:)%Orientation(1,3) =     i1(:,2)
ADInterfaceComponents%Blade(:)%Orientation(2,3) =     i2(:,2)
ADInterfaceComponents%Blade(:)%Orientation(3,3) =     i3(:,2)

     ! Hub orientation should use the g instead of e system, but the current version
     ! of AeroDyn calculates forces normal and tangential to the cone of rotation

ADInterfaceComponents%Hub%Orientation(:,1)       =     (/ e1(1), e2(1), e3(1) /) 
ADInterfaceComponents%Hub%Orientation(:,2)       = -1.*(/ e1(3), e2(3), e3(3) /)
ADInterfaceComponents%Hub%Orientation(:,3)       =     (/ e1(2), e2(2), e3(2) /)

     ! Rotor furl orientation (note the different order than hub and blade root!)

ADInterfaceComponents%RotorFurl%Orientation(:,1) = (/      c1(1), -1.*c3(1),     c2(1) /) 
ADInterfaceComponents%RotorFurl%Orientation(:,2) = (/ -1.* c1(3),     c3(3), -1.*c2(3) /)
ADInterfaceComponents%RotorFurl%Orientation(:,3) = (/      c1(2), -1.*c3(2),     c2(2) /)

      ! Nacelle orientation (note the different order than hub and blade root!)

ADInterfaceComponents%Nacelle%Orientation(:,1) = (/      d1(1), -1.*d3(1),     d2(1) /) 
ADInterfaceComponents%Nacelle%Orientation(:,2) = (/ -1.* d1(3),     d3(3), -1.*d2(3) /)
ADInterfaceComponents%Nacelle%Orientation(:,3) = (/      d1(2), -1.*d3(2),     d2(2) /)

!bjj end of proposed change


   !-------------------------------------------------------------------------------------------------
   ! Angular and partial angular velocities
   !-------------------------------------------------------------------------------------------------

   ! Define the angular and partial angular velocities of all of the rigid
   !   bodies in the inertia frame:
   ! NOTE: PAngVelEN(I,D,:) = the Dth-derivative of the partial angular velocity
   !   of DOF I for body N in body E.

PAngVelEX(       :,0,:) = 0.0
PAngVelEX(DOF_R   ,0,:) =  z1
PAngVelEX(DOF_P   ,0,:) = -z3
PAngVelEX(DOF_Y   ,0,:) =  z2
 AngVelEX               =             QDT(DOF_R   )*PAngVelEX(DOF_R   ,0,:) &
                                    + QDT(DOF_P   )*PAngVelEX(DOF_P   ,0,:) &
                                    + QDT(DOF_Y   )*PAngVelEX(DOF_Y   ,0,:)
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:
 AngPosEX               =             QT (DOF_R   )*PAngVelEX(DOF_R   ,0,:) &
                                    + QT (DOF_P   )*PAngVelEX(DOF_P   ,0,:) &
                                    + QT (DOF_Y   )*PAngVelEX(DOF_Y   ,0,:)
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

PAngVelEB(       :,0,:) = PAngVelEX(:,0,:)
PAngVelEB(DOF_TFA1,0,:) = -TwrFASF(1,TTopNode,1)*a3
PAngVelEB(DOF_TSS1,0,:) =  TwrSSSF(1,TTopNode,1)*a1
PAngVelEB(DOF_TFA2,0,:) = -TwrFASF(2,TTopNode,1)*a3
PAngVelEB(DOF_TSS2,0,:) =  TwrSSSF(2,TTopNode,1)*a1
 AngVelEB               =  AngVelEX + QDT(DOF_TFA1)*PAngVelEB(DOF_TFA1,0,:) &
                                    + QDT(DOF_TSS1)*PAngVelEB(DOF_TSS1,0,:) &
                                    + QDT(DOF_TFA2)*PAngVelEB(DOF_TFA2,0,:) &
                                    + QDT(DOF_TSS2)*PAngVelEB(DOF_TSS2,0,:)
 AngPosXB               =             QT (DOF_TFA1)*PAngVelEB(DOF_TFA1,0,:) &
                                    + QT (DOF_TSS1)*PAngVelEB(DOF_TSS1,0,:) &
                                    + QT (DOF_TFA2)*PAngVelEB(DOF_TFA2,0,:) &
                                    + QT (DOF_TSS2)*PAngVelEB(DOF_TSS2,0,:)

PAngVelEN(       :,0,:) = PAngVelEB(:,0,:)
PAngVelEN(DOF_Yaw ,0,:) =  d2
 AngVelEN               =  AngVelEB + QDT(DOF_Yaw )*PAngVelEN(DOF_Yaw ,0,:)

PAngVelER(       :,0,:) = PAngVelEN(:,0,:)
PAngVelER(DOF_RFrl,0,:) = rfa
 AngVelER               =  AngVelEN + QDT(DOF_RFrl)*PAngVelER(DOF_RFrl,0,:)

PAngVelEL(       :,0,:) = PAngVelER(:,0,:)
PAngVelEL(DOF_GeAz,0,:) =  c1
PAngVelEL(DOF_DrTr,0,:) =  c1
 AngVelEL               =  AngVelER + QDT(DOF_GeAz)*PAngVelEL(DOF_GeAz,0,:) &
                                    + QDT(DOF_DrTr)*PAngVelEL(DOF_DrTr,0,:)

PAngVelEH(       :,0,:) = PAngVelEL(:,0,:)
 AngVelEH               =  AngVelEL
IF ( NumBl == 2 )  THEN ! 2-blader
   PAngVelEH(DOF_Teet,0,:) = f2
    AngVelEH            =  AngVelEH + QDT(DOF_Teet)*PAngVelEH(DOF_Teet,0,:)
ENDIF

PAngVelEG(       :,0,:) = PAngVelER(:,0,:)
PAngVelEG(DOF_GeAz,0,:) = GenDir*GBRatio*c1
 AngVelEG               =  AngVelER + QDT(DOF_GeAz)*PAngVelEG(DOF_GeAz,0,:)

PAngVelEA(       :,0,:) = PAngVelEN(:,0,:)
PAngVelEA(DOF_TFrl,0,:) = tfa
 AngVelEA               =  AngVelEN + QDT(DOF_TFrl)*PAngVelEA(DOF_TFrl,0,:)

!bjj start of proposed change v6-02d-bjj
   
   ! Note the hub rotational velocity should be AngVelEH instead AngVelEL, but AeroDyn (13.00.00)
   ! treats teeter deflections like blade deflections:

ADInterfaceComponents%Hub%RotationVel(:)       = (/ AngVelEL(1), -1.*AngVelEL(3), AngVelEL(2) /) 
ADInterfaceComponents%RotorFurl%RotationVel(:) = (/ AngVelER(1), -1.*AngVelER(3), AngVelER(2) /) 
ADInterfaceComponents%Nacelle%RotationVel(:)   = (/ AngVelEN(1), -1.*AngVelEN(3), AngVelEN(2) /)
ADInterfaceComponents%Tower%RotationVel(:)     = (/ AngVelEX(1), -1.*AngVelEX(3), AngVelEX(2) /)
!bjj end of proposed change

   ! Define the 1st derivatives of the partial angular velocities of all
   !   of the rigid bodies in the inertia frame and the portion of the angular
   !   acceleration of the rigid bodies in the inertia frame associated with
   !   everything but the QD2T()'s:

                PAngVelEX(       :,1,:) = 0.0
                 AngAccEXt              = 0.0

                PAngVelEB(       :,1,:) = PAngVelEX(:,1,:)
CALL CrossProd( PAngVelEB(DOF_TFA1,1,:),   AngVelEX,                 PAngVelEB(DOF_TFA1,0,:) )
CALL CrossProd( PAngVelEB(DOF_TSS1,1,:),   AngVelEX,                 PAngVelEB(DOF_TSS1,0,:) )
CALL CrossProd( PAngVelEB(DOF_TFA2,1,:),   AngVelEX,                 PAngVelEB(DOF_TFA2,0,:) )
CALL CrossProd( PAngVelEB(DOF_TSS2,1,:),   AngVelEX,                 PAngVelEB(DOF_TSS2,0,:) )
                 AngAccEBt              =  AngAccEXt + QDT(DOF_TFA1)*PAngVelEB(DOF_TFA1,1,:) &
                                                     + QDT(DOF_TSS1)*PAngVelEB(DOF_TSS1,1,:) &
                                                     + QDT(DOF_TFA2)*PAngVelEB(DOF_TFA2,1,:) &
                                                     + QDT(DOF_TSS2)*PAngVelEB(DOF_TSS2,1,:)

                PAngVelEN(       :,1,:) = PAngVelEB(:,1,:)
CALL CrossProd( PAngVelEN(DOF_Yaw ,1,:),   AngVelEB,                 PAngVelEN(DOF_Yaw ,0,:) )
                 AngAccENt              =  AngAccEBt + QDT(DOF_Yaw )*PAngVelEN(DOF_Yaw ,1,:)

                PAngVelER(       :,1,:) = PAngVelEN(:,1,:)
CALL CrossProd( PAngVelER(DOF_RFrl,1,:),   AngVelEN,                 PAngVelER(DOF_RFrl,0,:) )
                 AngAccERt              =  AngAccENt + QDT(DOF_RFrl)*PAngVelER(DOF_RFrl,1,:)

                PAngVelEL(       :,1,:) = PAngVelER(:,1,:)
CALL CrossProd( PAngVelEL(DOF_GeAz,1,:),   AngVelER,                 PAngVelEL(DOF_GeAz,0,:) )
CALL CrossProd( PAngVelEL(DOF_DrTr,1,:),   AngVelER,                 PAngVelEL(DOF_DrTr,0,:) )
                 AngAccELt              =  AngAccERt + QDT(DOF_GeAz)*PAngVelEL(DOF_GeAz,1,:) &
                                                     + QDT(DOF_DrTr)*PAngVelEL(DOF_DrTr,1,:)

                PAngVelEH(       :,1,:) = PAngVelEL(:,1,:)
                 AngAccEHt              =  AngAccELt
IF ( NumBl == 2 )  THEN ! 2-blader
   CALL CrossProd( PAngVelEH(DOF_Teet,1,:),AngVelEH,                 PAngVelEH(DOF_Teet,0,:) )
                    AngAccEHt           =  AngAccEHt + QDT(DOF_Teet)*PAngVelEH(DOF_Teet,1,:)
ENDIF

                PAngVelEG(       :,1,:) = PAngVelER(:,1,:)
CALL CrossProd( PAngVelEG(DOF_GeAz,1,:),   AngVelER,                 PAngVelEG(DOF_GeAz,0,:) )
                 AngAccEGt              =  AngAccERt + QDT(DOF_GeAz)*PAngVelEG(DOF_GeAz,1,:)

                PAngVelEA(       :,1,:) = PAngVelEN(:,1,:)
CALL CrossProd( PAngVelEA(DOF_TFrl,1,:),   AngVelEN,                 PAngVelEA(DOF_TFrl,0,:) )
                 AngAccEAt              =  AngAccENt + QDT(DOF_TFrl)*PAngVelEA(DOF_TFrl,1,:)


!bjj start of proposed change AeroDyn loops
!JASON can move this to wherever he thinks his brain needs it ;-)
!ADCurrentTurbineState%RotorSpeed   = ABS( QDT(DOF_DrTr) + QDT(DOF_GeAz) )

DO K = 1,NumBl ! Loop through all blades

   ! Define the partial angular velocities of the tip (body M(BldFlexL)) in the  inertia frame:
   ! NOTE: PAngVelEM(K,J,I,D,:) = the Dth-derivative of the partial angular velocity of DOF I for body M of blade K, element J in body E.

   PAngVelEM(K,TipNode,          :,0,:) = PAngVelEH(:,0,:)
   PAngVelEM(K,TipNode,DOF_BF(K,1),0,:) = - TwistedSF(K,2,1,TipNode,1)*j1(K,:) &
                                          + TwistedSF(K,1,1,TipNode,1)*j2(K,:)
   PAngVelEM(K,TipNode,DOF_BF(K,2),0,:) = - TwistedSF(K,2,2,TipNode,1)*j1(K,:) &
                                          + TwistedSF(K,1,2,TipNode,1)*j2(K,:)
   PAngVelEM(K,TipNode,DOF_BE(K,1),0,:) = - TwistedSF(K,2,3,TipNode,1)*j1(K,:) &
                                          + TwistedSF(K,1,3,TipNode,1)*j2(K,:)
!    AngVelHM(K,TipNode              ,:) =  AngVelEH + QDT(DOF_BF(K,1))*PAngVelEM(K,TipNode,DOF_BF(K,1),0,:) & ! Currently
!                                                    + QDT(DOF_BF(K,2))*PAngVelEM(K,TipNode,DOF_BF(K,2),0,:) & ! unused
!                                                    + QDT(DOF_BE(K,1))*PAngVelEM(K,TipNode,DOF_BE(K,1),0,:)   ! calculations
    AngPosHM(K,TipNode              ,:) =             QT (DOF_BF(K,1))*PAngVelEM(K,TipNode,DOF_BF(K,1),0,:) &
                                                    + QT (DOF_BF(K,2))*PAngVelEM(K,TipNode,DOF_BF(K,2),0,:) &
                                                    + QT (DOF_BE(K,1))*PAngVelEM(K,TipNode,DOF_BE(K,1),0,:)


   ! Define the 1st derivatives of the partial angular velocities of the tip
   !   (body M(BldFlexL)) in the inertia frame:

! NOTE: These are currently unused by the code, therefore, they need not
!       be calculated.  Thus, they are currently commented out.  If it
!       turns out that they are ever needed (i.e., if inertias of the
!       blade elements are ever added, etc...) simply uncomment out these
!       computations:
!                   PAngVelEM(K,TipNode,          :,1,:) = PAngVelEH(:,1,:)
!   CALL CrossProd( PAngVelEM(K,TipNode,DOF_BF(K,1),1,:),   AngVelEH, &
!                   PAngVelEM(K,TipNode,DOF_BF(K,1),0,:)                )
!   CALL CrossProd( PAngVelEM(K,TipNode,DOF_BF(K,2),1,:),   AngVelEH, &
!                   PAngVelEM(K,TipNode,DOF_BF(K,2),0,:)                )
!   CALL CrossProd( PAngVelEM(K,TipNode,DOF_BE(K,1),1,:),   AngVelEH, &
!                   PAngVelEM(K,TipNode,DOF_BE(K,1),0,:)                )


   DO J = 1,BldNodes ! Loop through the blade nodes / elements
   ! Define the partial angular velocities of the current node (body M(RNodes(J))) in the inertia frame:
   ! NOTE: PAngVelEM(K,J,I,D,:) = the Dth-derivative of the partial angular velocity
   !   of DOF I for body M of blade K, element J in body E.

      PAngVelEM(K,J,          :,0,:) = PAngVelEH(:,0,:)
      PAngVelEM(K,J,DOF_BF(K,1),0,:) = - TwistedSF(K,2,1,J,1)*j1(K,:) &
                                       + TwistedSF(K,1,1,J,1)*j2(K,:)
      PAngVelEM(K,J,DOF_BF(K,2),0,:) = - TwistedSF(K,2,2,J,1)*j1(K,:) &
                                       + TwistedSF(K,1,2,J,1)*j2(K,:)
      PAngVelEM(K,J,DOF_BE(K,1),0,:) = - TwistedSF(K,2,3,J,1)*j1(K,:) &
                                       + TwistedSF(K,1,3,J,1)*j2(K,:)
!       AngVelHM(K,J              ,:) =  AngVelEH + QDT(DOF_BF(K,1))*PAngVelEM(K,J,DOF_BF(K,1),0,:) & ! Currently
!                                                 + QDT(DOF_BF(K,2))*PAngVelEM(K,J,DOF_BF(K,2),0,:) & ! unused
!                                                 + QDT(DOF_BE(K,1))*PAngVelEM(K,J,DOF_BE(K,1),0,:)   ! calculations
!       AngPosHM(K,J              ,:) =             QT (DOF_BF(K,1))*PAngVelEM(K,J,DOF_BF(K,1),0,:) & ! Currently
!                                                 + QT (DOF_BF(K,2))*PAngVelEM(K,J,DOF_BF(K,2),0,:) & ! unused
!                                                 + QT (DOF_BE(K,1))*PAngVelEM(K,J,DOF_BE(K,1),0,:)   ! calculations


   ! Define the 1st derivatives of the partial angular velocities of the current node (body M(RNodes(J))) in the inertia frame:

! NOTE: These are currently unused by the code, therefore, they need not
!       be calculated.  Thus, they are currently commented out.  If it
!       turns out that they are ever needed (i.e., if inertias of the
!       blade elements are ever added, etc...) simply uncomment out these
!       computations:
!                      PAngVelEM(K,J,          :,1,:) = PAngVelEH(:,1,:)
!      CALL CrossProd( PAngVelEM(K,J,DOF_BF(K,1),1,:),   AngVelEH, &
!                      PAngVelEM(K,J,DOF_BF(K,1),0,:)                )
!      CALL CrossProd( PAngVelEM(K,J,DOF_BF(K,2),1,:),   AngVelEH, &
!                      PAngVelEM(K,J,DOF_BF(K,2),0,:)                )
!      CALL CrossProd( PAngVelEM(K,J,DOF_BE(K,1),1,:),   AngVelEH, &
!                      PAngVelEM(K,J,DOF_BE(K,1),0,:)                )


   END DO !J = 1,BldNodes ! Loop through the blade nodes / elements

END DO !K = 1,NumBl

!bjj end of proposed change

   !-------------------------------------------------------------------------------------------------
   ! Partial linear velocities and accelerations
   !-------------------------------------------------------------------------------------------------

   ! Define the partial linear velocities (and their 1st derivatives) of all of
   !   the points on the wind turbine in the inertia frame that are not
   !   dependent on the distributed tower or blade parameters.  Also, define
   !   the portion of the linear acceleration of the points in the inertia
   !   frame associated with everything but the QD2T()'s:
   ! NOTE: PLinVelEX(I,D,:) = the Dth-derivative of the partial linear velocity
   !   of DOF I for point X in body E.

CALL CrossProd( EwXXrZY  , AngVelEX, rZY   ) !
CALL CrossProd( EwXXrZO  , AngVelEX, rZO   ) !
CALL CrossProd( EwNXrOU  , AngVelEN, rOU   ) !
CALL CrossProd( EwNXrOV  , AngVelEN, rOV   ) !
CALL CrossProd( EwRXrVD  , AngVelER, rVD   ) ! Cross products
CALL CrossProd( EwRXrVIMU, AngVelER, rVIMU ) ! that are used
CALL CrossProd( EwRXrVP  , AngVelER, rVP   ) ! in the following
CALL CrossProd( EwHXrPQ  , AngVelEH, rPQ   ) ! DO...LOOPs
CALL CrossProd( EwHXrQC  , AngVelEH, rQC   ) !
CALL CrossProd( EwNXrOW  , AngVelEN, rOW   ) !
CALL CrossProd( EwAXrWI  , AngVelEA, rWI   ) !
CALL CrossProd( EwAXrWJ  , AngVelEA, rWJ   ) !
CALL CrossProd( EwAXrWK  , AngVelEA, rWK   ) !


PLinVelEZ(       :,:,:) = 0.0
PLinVelEZ(DOF_Sg  ,0,:) =  z1
PLinVelEZ(DOF_Sw  ,0,:) = -z3
PLinVelEZ(DOF_Hv  ,0,:) =  z2

 LinVelEZ               =              QDT(DOF_Sg  )*PLinVelEZ(DOF_Sg  ,0,:) &
                                     + QDT(DOF_Sw  )*PLinVelEZ(DOF_Sw  ,0,:) &
                                     + QDT(DOF_Hv  )*PLinVelEZ(DOF_Hv  ,0,:)


PLinVelEY(       :,:,:) = PLinVelEZ(:,:,:)
DO I = 1,NPX   ! Loop through all DOFs associated with the angular motion of the platform (body X)

   CALL CrossProd(        TmpVec0,                   PAngVelEX(PX(I)   ,0,:),     rZY                 )
   CALL CrossProd(        TmpVec1,                   PAngVelEX(PX(I)   ,0,:), EwXXrZY                 )

   PLinVelEY(PX(I),0,:) = TmpVec0    +               PLinVelEY(PX(I)   ,0,:)
   PLinVelEY(PX(I),1,:) = TmpVec1    +               PLinVelEY(PX(I)   ,1,:)

    LinAccEYt           =  LinAccEYt + QDT(PX(I)   )*PLinVelEY(PX(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the platform (body X)


PLinVelEO(       :,:,:) = PLinVelEZ(:,:,:)
PLinVelEO(DOF_TFA1,0,:) = a1      - (   AxRedTFA(1,1,TTopNode)* QT(DOF_TFA1) &
                                      + AxRedTFA(1,2,TTopNode)* QT(DOF_TFA2)   )*a2
PLinVelEO(DOF_TSS1,0,:) = a3      - (   AxRedTSS(1,1,TTopNode)* QT(DOF_TSS1) &
                                      + AxRedTSS(1,2,TTopNode)* QT(DOF_TSS2)   )*a2
PLinVelEO(DOF_TFA2,0,:) = a1      - (   AxRedTFA(2,2,TTopNode)* QT(DOF_TFA2) &
                                      + AxRedTFA(1,2,TTopNode)* QT(DOF_TFA1)   )*a2
PLinVelEO(DOF_TSS2,0,:) = a3      - (   AxRedTSS(2,2,TTopNode)* QT(DOF_TSS2) &
                                      + AxRedTSS(1,2,TTopNode)* QT(DOF_TSS1)   )*a2

CALL    CrossProd(        TmpVec1,                    AngVelEX              , PLinVelEO(DOF_TFA1,0,:) )
CALL    CrossProd(        TmpVec2,                    AngVelEX              , PLinVelEO(DOF_TSS1,0,:) )
CALL    CrossProd(        TmpVec3,                    AngVelEX              , PLinVelEO(DOF_TFA2,0,:) )
CALL    CrossProd(        TmpVec4,                    AngVelEX              , PLinVelEO(DOF_TSS2,0,:) )

PLinVelEO(DOF_TFA1,1,:) = TmpVec1 - (   AxRedTFA(1,1,TTopNode)*QDT(DOF_TFA1) &
                                      + AxRedTFA(1,2,TTopNode)*QDT(DOF_TFA2)   )*a2
PLinVelEO(DOF_TSS1,1,:) = TmpVec2 - (   AxRedTSS(1,1,TTopNode)*QDT(DOF_TSS1) &
                                      + AxRedTSS(1,2,TTopNode)*QDT(DOF_TSS2)   )*a2
PLinVelEO(DOF_TFA2,1,:) = TmpVec3 - (   AxRedTFA(2,2,TTopNode)*QDT(DOF_TFA2) &
                                      + AxRedTFA(1,2,TTopNode)*QDT(DOF_TFA1)   )*a2
PLinVelEO(DOF_TSS2,1,:) = TmpVec4 - (   AxRedTSS(2,2,TTopNode)*QDT(DOF_TSS2) &
                                      + AxRedTSS(1,2,TTopNode)*QDT(DOF_TSS1)   )*a2

 LinVelXO               =              QDT(DOF_TFA1)*PLinVelEO(DOF_TFA1,0,:) &
                                     + QDT(DOF_TSS1)*PLinVelEO(DOF_TSS1,0,:) &
                                     + QDT(DOF_TFA2)*PLinVelEO(DOF_TFA2,0,:) &
                                     + QDT(DOF_TSS2)*PLinVelEO(DOF_TSS2,0,:)
 LinAccEOt              =              QDT(DOF_TFA1)*PLinVelEO(DOF_TFA1,1,:) &
                                     + QDT(DOF_TSS1)*PLinVelEO(DOF_TSS1,1,:) &
                                     + QDT(DOF_TFA2)*PLinVelEO(DOF_TFA2,1,:) &
                                     + QDT(DOF_TSS2)*PLinVelEO(DOF_TSS2,1,:)

DO I = 1,NPX   ! Loop through all DOFs associated with the angular motion of the platform (body X)

   CALL CrossProd(        TmpVec0,                   PAngVelEX(PX(I)   ,0,:),     rZO                 )
   CALL CrossProd(        TmpVec1,                   PAngVelEX(PX(I)   ,0,:), EwXXrZO + LinVelXO      )

   PLinVelEO(PX(I),0,:) = TmpVec0    +               PLinVelEO(PX(I)   ,0,:)
   PLinVelEO(PX(I),1,:) = TmpVec1    +               PLinVelEO(PX(I)   ,1,:)

    LinAccEOt           =  LinAccEOt + QDT(PX(I)   )*PLinVelEO(PX(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the platform (body X)


PLinVelEU(       :,:,:) = PLinVelEO(:,:,:)
DO I = 1,NPN   ! Loop through all DOFs associated with the angular motion of the nacelle (body N)

   CALL CrossProd(        TmpVec0,                   PAngVelEN(PN(I)   ,0,:),     rOU                 )
   CALL CrossProd(        TmpVec1,                   PAngVelEN(PN(I)   ,0,:), EwNXrOU                 )
   CALL CrossProd(        TmpVec2,                   PAngVelEN(PN(I)   ,1,:),     rOU                 )

   PLinVelEU(PN(I),0,:) = TmpVec0    +               PLinVelEU(PN(I)   ,0,:)
   PLinVelEU(PN(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEU(PN(I)   ,1,:)

    LinAccEUt           =  LinAccEUt + QDT(PN(I)   )*PLinVelEU(PN(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the nacelle (body N)


PLinVelEV(       :,:,:) = PLinVelEO(:,:,:)
DO I = 1,NPN   ! Loop through all DOFs associated with the angular motion of the nacelle (body N)

   CALL CrossProd(        TmpVec0,                   PAngVelEN(PN(I)   ,0,:),     rOV                 )
   CALL CrossProd(        TmpVec1,                   PAngVelEN(PN(I)   ,0,:), EwNXrOV                 )
   CALL CrossProd(        TmpVec2,                   PAngVelEN(PN(I)   ,1,:),     rOV                 )

   PLinVelEV(PN(I),0,:) = TmpVec0    +               PLinVelEV(PN(I)   ,0,:)
   PLinVelEV(PN(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEV(PN(I)   ,1,:)

    LinAccEVt           =  LinAccEVt + QDT(PN(I)   )*PLinVelEV(PN(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the nacelle (body N)


PLinVelED(       :,:,:) = PLinVelEV(:,:,:)
DO I = 1,NPR   ! Loop through all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)

   CALL CrossProd(        TmpVec0,                   PAngVelER(PR(I)   ,0,:),     rVD                 )
   CALL CrossProd(        TmpVec1,                   PAngVelER(PR(I)   ,0,:), EwRXrVD                 )
   CALL CrossProd(        TmpVec2,                   PAngVelER(PR(I)   ,1,:),     rVD                 )

   PLinVelED(PR(I),0,:) = TmpVec0    +               PLinVelED(PR(I)   ,0,:)
   PLinVelED(PR(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelED(PR(I)   ,1,:)

    LinAccEDt           =  LinAccEDt + QDT(PR(I)   )*PLinVelED(PR(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)


PLinVelEIMU(     :,:,:) = PLinVelEV(:,:,:)
 LinVelEIMU             =  LinVelEZ
DO I = 1,NPR   ! Loop through all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)

   CALL CrossProd(          TmpVec0,                 PAngVelER(PR(I)   ,0,:),     rVIMU               )
   CALL CrossProd(          TmpVec1,                 PAngVelER(PR(I)   ,0,:), EwRXrVIMU               )
   CALL CrossProd(          TmpVec2,                 PAngVelER(PR(I)   ,1,:),     rVIMU               )

   PLinVelEIMU(PR(I),0,:) = TmpVec0    +             PLinVelEIMU(PR(I) ,0,:)
   PLinVelEIMU(PR(I),1,:) = TmpVec1    + TmpVec2 +   PLinVelEIMU(PR(I) ,1,:)

    LinVelEIMU          =  LinVelEIMU  + QDT(PR(I) )*PLinVelEIMU(PR(I) ,0,:)
    LinAccEIMUt         =  LinAccEIMUt + QDT(PR(I) )*PLinVelEIMU(PR(I) ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)


PLinVelEP(       :,:,:) = PLinVelEV(:,:,:)
DO I = 1,NPR   ! Loop through all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)

   CALL CrossProd(        TmpVec0,                   PAngVelER(PR(I)   ,0,:),     rVP                 )
   CALL CrossProd(        TmpVec1,                   PAngVelER(PR(I)   ,0,:), EwRXrVP                 )
   CALL CrossProd(        TmpVec2,                   PAngVelER(PR(I)   ,1,:),     rVP                 )

   PLinVelEP(PR(I),0,:) = TmpVec0    +               PLinVelEP(PR(I)   ,0,:)
   PLinVelEP(PR(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEP(PR(I)   ,1,:)

    LinAccEPt           =  LinAccEPt + QDT(PR(I)   )*PLinVelEP(PR(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the structure that furls with the rotor (not including rotor) (body R)


PLinVelEQ(       :,:,:) = PLinVelEP(:,:,:)
DO I = 1,NPH   ! Loop through all DOFs associated with the angular motion of the hub (body H)

   CALL CrossProd(        TmpVec0,                   PAngVelEH(PH(I)   ,0,:),     rPQ                 )
   CALL CrossProd(        TmpVec1,                   PAngVelEH(PH(I)   ,0,:), EwHXrPQ                 )
   CALL CrossProd(        TmpVec2,                   PAngVelEH(PH(I)   ,1,:),     rPQ                 )

   PLinVelEQ(PH(I),0,:) = TmpVec0    +               PLinVelEQ(PH(I)   ,0,:)
   PLinVelEQ(PH(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEQ(PH(I)   ,1,:)

    LinAccEQt           =  LinAccEQt + QDT(PH(I)   )*PLinVelEQ(PH(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the hub (body H)


PLinVelEC(       :,:,:) = PLinVelEQ(:,:,:)
DO I = 1,NPH   ! Loop through all DOFs associated with the angular motion of the hub (body H)

   CALL CrossProd(        TmpVec0,                   PAngVelEH(PH(I)   ,0,:),     rQC                 )
   CALL CrossProd(        TmpVec1,                   PAngVelEH(PH(I)   ,0,:), EwHXrQC                 )
   CALL CrossProd(        TmpVec2,                   PAngVelEH(PH(I)   ,1,:),     rQC                 )

   PLinVelEC(PH(I),0,:) = TmpVec0    +               PLinVelEC(PH(I)   ,0,:)
   PLinVelEC(PH(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEC(PH(I)   ,1,:)

    LinAccECt           =  LinAccECt + QDT(PH(I)   )*PLinVelEC(PH(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the hub (body H)



!bjj start of proposed change AeroDyn loops
!JASON can move this to wherever he thinks his brain needs it ;-)

!bjj replaced with new interface
!!  HubVDue2Yaw is the hub's in-plane, horozontal velocity due to the yaw rate, used in the DiskVel subroutine as YAWVEL
!!  The nacelle-yaw, platform-yaw, and furl rates contribute to this velocity
!!  <--This is the velocity of the hub due to nacelle- and platform-yaw and rotor-furling effects projected in the plane of rotation and parallel to xi-yi plane
!LinVelEPYaw = PLinVelEP(DOF_RFrl,0,:)*QDT(DOF_RFrl) & ! This is the linear velocity of the hub in the inertia frame due solely to nacelle-and platform-yaw and rotor-furl effects
!            + PLinVelEP(DOF_Yaw ,0,:)*QDT(DOF_Yaw ) &
!            + PLinVelEP(DOF_Y   ,0,:)*QDT(DOF_Y   )
!
!ADCurrentTurbineState%HubVDue2Yaw  = -1.*LinVelEPYaw(1)*SIN( ADCurrentTurbineState%YawAngle ) &
!                                       + LinVelEPYaw(3)*COS( ADCurrentTurbineState%YawAngle )  !JASON: WITH ROTOR-FURL, THE ROTOR PLANE CAN TRANSLATE AN ANY DIRECTION DUE TO YAW RATES AND ROTOR-FURL RATES; THUS, WE SHOULD REALLY SPECIFY HUB VELOCITIES IN THE PLANE OF THE DISK IN TWO DIRECTIONS (HORIZONTAL AND VERTICAL) AND ADD A HUB VELOCITY NORMAL TO THE DISK.  THE ONLY VELOCITY CURRENTLY TAKEN INTO ACCOUNT IS THE HORIZONTAL COMPONENET WITHIN THE DISK (THROUGH VARIABLE HubVDue2Yaw)
! bjj end replace

DO K = 1,NumBl ! Loop through all blades

   ! Define the partial linear velocities (and their 1st derivatives) of the
   !   blade tip (point S(BldFlexL)) in the inertia frame.  Also define the
   !   overall linear velocity of the blade tip in the inertia frame.  Also,
   !   define the portion of the linear acceleration of the blade tip in the
   !   inertia frame associated with everything but the QD2T()'s:

   CALL CrossProd( EwHXrQS, AngVelEH, rQS(K,TipNode,:) )

   PLinVelES(K,TipNode,          :,:,:) = PLinVelEQ(:,:,:)
   PLinVelES(K,TipNode,DOF_BF(K,1),0,:) = TwistedSF(K,1,1,TipNode,0)                        *j1(K,:) &
                                        + TwistedSF(K,2,1,TipNode,0)                        *j2(K,:) &
                                        - (   AxRedBld(K,1,1,TipNode)*QT ( DOF_BF(K,1) ) &
                                            + AxRedBld(K,1,2,TipNode)*QT ( DOF_BF(K,2) ) &
                                            + AxRedBld(K,1,3,TipNode)*QT ( DOF_BE(K,1) )   )*j3(K,:)
   PLinVelES(K,TipNode,DOF_BE(K,1),0,:) = TwistedSF(K,1,3,TipNode,0)                        *j1(K,:) &
                                        + TwistedSF(K,2,3,TipNode,0)                        *j2(K,:) &
                                        - (   AxRedBld(K,3,3,TipNode)*QT ( DOF_BE(K,1) ) &
                                            + AxRedBld(K,2,3,TipNode)*QT ( DOF_BF(K,2) ) &
                                            + AxRedBld(K,1,3,TipNode)*QT ( DOF_BF(K,1) )   )*j3(K,:)
   PLinVelES(K,TipNode,DOF_BF(K,2),0,:) = TwistedSF(K,1,2,TipNode,0)                        *j1(K,:) &
                                        + TwistedSF(K,2,2,TipNode,0)                        *j2(K,:) &
                                        - (   AxRedBld(K,2,2,TipNode)*QT ( DOF_BF(K,2) ) &
                                            + AxRedBld(K,1,2,TipNode)*QT ( DOF_BF(K,1) ) &
                                            + AxRedBld(K,2,3,TipNode)*QT ( DOF_BE(K,1) )   )*j3(K,:)

   CALL CrossProd( TmpVec1, AngVelEH, PLinVelES(K,TipNode,DOF_BF(K,1),0,:) )
   CALL CrossProd( TmpVec2, AngVelEH, PLinVelES(K,TipNode,DOF_BE(K,1),0,:) )
   CALL CrossProd( TmpVec3, AngVelEH, PLinVelES(K,TipNode,DOF_BF(K,2),0,:) )

   PLinVelES(K,TipNode,DOF_BF(K,1),1,:) = TmpVec1 &
                                        - (   AxRedBld(K,1,1,TipNode)*QDT( DOF_BF(K,1) ) &
                                            + AxRedBld(K,1,2,TipNode)*QDT( DOF_BF(K,2) ) &
                                            + AxRedBld(K,1,3,TipNode)*QDT( DOF_BE(K,1) )   )*j3(K,:)
   PLinVelES(K,TipNode,DOF_BE(K,1),1,:) = TmpVec2 &
                                        - (   AxRedBld(K,3,3,TipNode)*QDT( DOF_BE(K,1) ) &
                                            + AxRedBld(K,2,3,TipNode)*QDT( DOF_BF(K,2) ) &
                                            + AxRedBld(K,1,3,TipNode)*QDT( DOF_BF(K,1) )   )*j3(K,:)
   PLinVelES(K,TipNode,DOF_BF(K,2),1,:) = TmpVec3 &
                                        - (   AxRedBld(K,2,2,TipNode)*QDT( DOF_BF(K,2) ) &
                                            + AxRedBld(K,1,2,TipNode)*QDT( DOF_BF(K,1) ) &
                                            + AxRedBld(K,2,3,TipNode)*QDT( DOF_BE(K,1) )   )*j3(K,:)

   LinVelHS               = QDT( DOF_BF(K,1) )*PLinVelES(K,TipNode,DOF_BF(K,1),0,:) &
                          + QDT( DOF_BE(K,1) )*PLinVelES(K,TipNode,DOF_BE(K,1),0,:) &
                          + QDT( DOF_BF(K,2) )*PLinVelES(K,TipNode,DOF_BF(K,2),0,:)
   LinAccESt(K,TipNode,:) = QDT( DOF_BF(K,1) )*PLinVelES(K,TipNode,DOF_BF(K,1),1,:) &
                          + QDT( DOF_BE(K,1) )*PLinVelES(K,TipNode,DOF_BE(K,1),1,:) &
                          + QDT( DOF_BF(K,2) )*PLinVelES(K,TipNode,DOF_BF(K,2),1,:)

   LinVelES               = LinVelHS + LinVelEZ
   DO I = 1,NPH   ! Loop through all DOFs associated with the angular motion of the hub (body H)

      CALL CrossProd( TmpVec0, PAngVelEH(PH(I),0,:),     rQS(K,TipNode,:)            )
      CALL CrossProd( TmpVec1, PAngVelEH(PH(I),0,:), EwHXrQS              + LinVelHS )
      CALL CrossProd( TmpVec2, PAngVelEH(PH(I),1,:),     rQS(K,TipNode,:)            )

      PLinVelES(K,TipNode,PH(I),0,:) = PLinVelES(K,TipNode,PH(I),0,:) + TmpVec0
      PLinVelES(K,TipNode,PH(I),1,:) = PLinVelES(K,TipNode,PH(I),1,:) + TmpVec1 + TmpVec2

      LinVelES                       = LinVelES                       + QDT(PH(I))*PLinVelES(K,TipNode,PH(I),0,:)
      LinAccESt(K,TipNode,        :) = LinAccESt(K,TipNode,        :) + QDT(PH(I))*PLinVelES(K,TipNode,PH(I),1,:)

   ENDDO          ! I - all DOFs associated with the angular motion of the hub (body H)

!bjj start of proposed change - aerodyn loops
! this is now reordered to use the correct LinVelES calculated from the TipNode
!JASON: USE TipNode HERE INSTEAD OF BldNodes IF YOU ALLOCATE AND DEFINE n1, n2, n3, m1, m2, AND m3 TO USE TipNode.  THIS WILL REQUIRE THAT THE AERODYNAMIC AND STRUCTURAL TWISTS, AeroTwst() AND ThetaS(), BE KNOWN AT THE TIP!!!
   LinVelESm2(K) = DOT_PRODUCT( LinVelES, m2(K,BldNodes,:) )
!bjj end of proposed change


   DO J = 1,BldNodes ! Loop through the blade nodes / elements

   ! Define the partial linear velocities (and their 1st derivatives) of the
   !   current node (point S(RNodes(J))) in the inertia frame.  Also define
   !   the overall linear velocity of the current node in the inertia frame.
   !   Also, define the portion of the linear acceleration of the current node
   !   in the inertia frame associated with everything but the QD2T()'s:

      CALL CrossProd( EwHXrQS, AngVelEH, rQS(K,J,:) )

      PLinVelES(K,J,          :,:,:) = PLinVelEQ(:,:,:)
      PLinVelES(K,J,DOF_BF(K,1),0,:) = TwistedSF(K,1,1,J,0)                        *j1(K,:) &
                                     + TwistedSF(K,2,1,J,0)                        *j2(K,:) &
                                     - (   AxRedBld(K,1,1,J)*QT ( DOF_BF(K,1) ) &
                                         + AxRedBld(K,1,2,J)*QT ( DOF_BF(K,2) ) &
                                         + AxRedBld(K,1,3,J)*QT ( DOF_BE(K,1) )   )*j3(K,:)
      PLinVelES(K,J,DOF_BE(K,1),0,:) = TwistedSF(K,1,3,J,0)                        *j1(K,:) &
                                     + TwistedSF(K,2,3,J,0)                        *j2(K,:) &
                                     - (   AxRedBld(K,3,3,J)*QT ( DOF_BE(K,1) ) &
                                         + AxRedBld(K,2,3,J)*QT ( DOF_BF(K,2) ) &
                                         + AxRedBld(K,1,3,J)*QT ( DOF_BF(K,1) )   )*j3(K,:)
      PLinVelES(K,J,DOF_BF(K,2),0,:) = TwistedSF(K,1,2,J,0)                        *j1(K,:) &
                                     + TwistedSF(K,2,2,J,0)                        *j2(K,:) &
                                     - (   AxRedBld(K,2,2,J)*QT ( DOF_BF(K,2) ) &
                                         + AxRedBld(K,1,2,J)*QT ( DOF_BF(K,1) ) &
                                         + AxRedBld(K,2,3,J)*QT ( DOF_BE(K,1) )   )*j3(K,:)

      CALL CrossProd( TmpVec1, AngVelEH, PLinVelES(K,J,DOF_BF(K,1),0,:) )
      CALL CrossProd( TmpVec2, AngVelEH, PLinVelES(K,J,DOF_BE(K,1),0,:) )
      CALL CrossProd( TmpVec3, AngVelEH, PLinVelES(K,J,DOF_BF(K,2),0,:) )

      PLinVelES(K,J,DOF_BF(K,1),1,:) = TmpVec1 &
                                     - (   AxRedBld(K,1,1,J)*QDT( DOF_BF(K,1) ) &
                                         + AxRedBld(K,1,2,J)*QDT( DOF_BF(K,2) ) &
                                         + AxRedBld(K,1,3,J)*QDT( DOF_BE(K,1) )   )*j3(K,:)
      PLinVelES(K,J,DOF_BE(K,1),1,:) = TmpVec2 &
                                     - (   AxRedBld(K,3,3,J)*QDT( DOF_BE(K,1) ) &
                                         + AxRedBld(K,2,3,J)*QDT( DOF_BF(K,2) ) &
                                         + AxRedBld(K,1,3,J)*QDT( DOF_BF(K,1) )   )*j3(K,:)
      PLinVelES(K,J,DOF_BF(K,2),1,:) = TmpVec3 &
                                     - (   AxRedBld(K,2,2,J)*QDT( DOF_BF(K,2) ) &
                                         + AxRedBld(K,1,2,J)*QDT( DOF_BF(K,1) ) &
                                         + AxRedBld(K,2,3,J)*QDT( DOF_BE(K,1) )   )*j3(K,:)

      LinVelHS         = QDT( DOF_BF(K,1) )*PLinVelES(K,J,DOF_BF(K,1),0,:) &
                       + QDT( DOF_BE(K,1) )*PLinVelES(K,J,DOF_BE(K,1),0,:) &
                       + QDT( DOF_BF(K,2) )*PLinVelES(K,J,DOF_BF(K,2),0,:)
      LinAccESt(K,J,:) = QDT( DOF_BF(K,1) )*PLinVelES(K,J,DOF_BF(K,1),1,:) &
                       + QDT( DOF_BE(K,1) )*PLinVelES(K,J,DOF_BE(K,1),1,:) &
                       + QDT( DOF_BF(K,2) )*PLinVelES(K,J,DOF_BF(K,2),1,:)

      LinVelES         = LinVelHS + LinVelEZ
      DO I = 1,NPH   ! Loop through all DOFs associated with the angular motion of the hub (body H)

         CALL CrossProd( TmpVec0, PAngVelEH(PH(I),0,:),     rQS(K,J,:)            )
         CALL CrossProd( TmpVec1, PAngVelEH(PH(I),0,:), EwHXrQS        + LinVelHS )
         CALL CrossProd( TmpVec2, PAngVelEH(PH(I),1,:),     rQS(K,J,:)            )

         PLinVelES(K,J,PH(I),0,:) = PLinVelES(K,J,PH(I),0,:) + TmpVec0
         PLinVelES(K,J,PH(I),1,:) = PLinVelES(K,J,PH(I),1,:) + TmpVec1 + TmpVec2

         LinVelES                 = LinVelES                 + QDT(PH(I))*PLinVelES(K,J,PH(I),0,:)
         LinAccESt(K,J,        :) = LinAccESt(K,J,        :) + QDT(PH(I))*PLinVelES(K,J,PH(I),1,:)

      END DO ! I - all DOFs associated with the angular motion of the hub (body H)

      ADAeroMarkers%Blade(J,K)%TranslationVel(:)= (/ LinVelES(1), -1.*LinVelES(3),  LinVelES(2)  /)  !AeroDyn's coordinates


   END DO !J = 1,BldNodes ! Loop through the blade nodes / elements

END DO !K = 1,NumBl

!bjj end of proposed change



PLinVelEW(       :,:,:) = PLinVelEO(:,:,:)
DO I = 1,NPN   ! Loop through all DOFs associated with the angular motion of the nacelle (body N)

   CALL CrossProd(        TmpVec0,                   PAngVelEN(PN(I)   ,0,:),     rOW                 )
   CALL CrossProd(        TmpVec1,                   PAngVelEN(PN(I)   ,0,:), EwNXrOW                 )
   CALL CrossProd(        TmpVec2,                   PAngVelEN(PN(I)   ,1,:),     rOW                 )

   PLinVelEW(PN(I),0,:) = TmpVec0    +               PLinVelEW(PN(I)   ,0,:)
   PLinVelEW(PN(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEW(PN(I)   ,1,:)

    LinAccEWt           =  LinAccEWt + QDT(PN(I)   )*PLinVelEW(PN(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the nacelle (body N)


PLinVelEI(       :,:,:) = PLinVelEW(:,:,:)
DO I = 1,NPA   ! Loop through all DOFs associated with the angular motion of the tail (body A)

   CALL CrossProd(        TmpVec0,                   PAngVelEA(PA(I)   ,0,:),     rWI                 )
   CALL CrossProd(        TmpVec1,                   PAngVelEA(PA(I)   ,0,:), EwAXrWI                 )
   CALL CrossProd(        TmpVec2,                   PAngVelEA(PA(I)   ,1,:),     rWI                 )

   PLinVelEI(PA(I),0,:) = TmpVec0    +               PLinVelEI(PA(I)   ,0,:)
   PLinVelEI(PA(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEI(PA(I)   ,1,:)

    LinAccEIt           =  LinAccEIt + QDT(PA(I)   )*PLinVelEI(PA(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the tail (body A)


PLinVelEJ(       :,:,:) = PLinVelEW(:,:,:)
DO I = 1,NPA   ! Loop through all DOFs associated with the angular motion of the tail (body A)

   CALL CrossProd(        TmpVec0,                   PAngVelEA(PA(I)   ,0,:),     rWJ                 )
   CALL CrossProd(        TmpVec1,                   PAngVelEA(PA(I)   ,0,:), EwAXrWJ                 )
   CALL CrossProd(        TmpVec2,                   PAngVelEA(PA(I)   ,1,:),     rWJ                 )

   PLinVelEJ(PA(I),0,:) = TmpVec0    +               PLinVelEJ(PA(I)   ,0,:)
   PLinVelEJ(PA(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEJ(PA(I)   ,1,:)

    LinAccEJt           =  LinAccEJt + QDT(PA(I)   )*PLinVelEJ(PA(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the tail (body A)

PLinVelEK(       :,:,:) = PLinVelEW(:,:,:)
 LinVelEK               =  LinVelEZ
DO I = 1,NPA   ! Loop through all DOFs associated with the angular motion of the tail (body A)

   CALL CrossProd(        TmpVec0,                   PAngVelEA(PA(I)   ,0,:),     rWK                 )
   CALL CrossProd(        TmpVec1,                   PAngVelEA(PA(I)   ,0,:), EwAXrWK                 )
   CALL CrossProd(        TmpVec2,                   PAngVelEA(PA(I)   ,1,:),     rWK                 )

   PLinVelEK(PA(I),0,:) = TmpVec0    +               PLinVelEK(PA(I)   ,0,:)
   PLinVelEK(PA(I),1,:) = TmpVec1    + TmpVec2 +     PLinVelEK(PA(I)   ,1,:)

    LinVelEK            =  LinVelEK  + QDT(PA(I)   )*PLinVelEK(PA(I)   ,0,:)
    LinAccEKt           =  LinAccEKt + QDT(PA(I)   )*PLinVelEK(PA(I)   ,1,:)

ENDDO          ! I - all DOFs associated with the angular motion of the tail (body A)



   ! Initialize the partial forces and moments (including those associated
   !   with the QD2T()'s and those that are not) at the teeter pin (point P)
   !   using the hub mass effects:

PFrcPRot  = 0.0   ! Initialize these partial
PMomLPRot = 0.0   ! forces and moments to zero
DO I = 1,NPCE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the hub center of mass (point C)

   TmpVec1 = -HubMass*PLinVelEC(PCE(I),0,:)     ! The portion of PFrcPRot  associated with the HubMass
   CALL CrossProd( TmpVec2, rPC, TmpVec1 )      ! The portion of PMomLPRot associated with the HubMass

   PFrcPRot (PCE(I),:) = TmpVec1

   PMomLPRot(PCE(I),:) = TmpVec2 - Hubg1Iner*g1*DOT_PRODUCT( g1, PAngVelEH(PCE(I),0,:) ) &
                                 - Hubg2Iner*g2*DOT_PRODUCT( g2, PAngVelEH(PCE(I),0,:) )

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the hub center of mass (point C)

TmpVec1 = -HubMass*( Gravity*z2 + LinAccECt )   ! The portion of FrcPRott  associated with the HubMass
CALL CrossProd( TmpVec2, rPC, TmpVec1 )         ! The portion of MomLPRott associated with the HubMass
TmpVec  = Hubg1Iner*g1*DOT_PRODUCT( g1, AngVelEH ) &   ! = ( Hub inertia dyadic ) dot ( angular velocity of hub in the inertia frame )
        + Hubg2Iner*g2*DOT_PRODUCT( g2, AngVelEH )
CALL CrossProd( TmpVec3, -AngVelEH, TmpVec )    ! = ( -angular velocity of hub in the inertia frame ) cross ( TmpVec )

FrcPRott  = TmpVec1
MomLPRott = TmpVec2 + TmpVec3 - Hubg1Iner*g1*DOT_PRODUCT( g1, AngAccEHt ) &
                              - Hubg2Iner*g2*DOT_PRODUCT( g2, AngAccEHt )

!BJJ START of proposed change - aerodyn loops
!Jason can move this to wherever he thinks it makes sense

   !-------------------------------------------------------------------------------------------------
   ! Call AeroDyn to calculate aerodynamic forces
   !-------------------------------------------------------------------------------------------------

!IF ( CompAero ) CALL AeroCalc ( ZTime, ErrStat )
IF ( CompAero ) ADAeroLoads = AD_CalculateLoads( ZTime, ADAeroMarkers, ADInterfaceComponents, ADIntrfaceOptions, ErrStat )

!BJJ END OF PROPOSED CHANGE

DO K = 1,NumBl ! Loop through all blades

   ! Calculate the position vector from the teeter pin to the blade root:

   rPS0 = rPQ + HubRad*j3(K,:)   ! Position vector from teeter pin (point P) to blade root (point S(0)).

!bjj: start of proposed change - aerodyn loops
!bjj: now these are calculated above
!
!
!   ! Calculate the position vector of the tip:
!
!   rS0S(K,TipNode,:) = (   TwistedSF(K,1,1,TipNode,0)*QT( DOF_BF(K,1) ) &  ! Position vector from the blade root (point S(0)) to the blade tip (point S(BldFlexL)).
!                         + TwistedSF(K,1,2,TipNode,0)*QT( DOF_BF(K,2) ) &
!                         + TwistedSF(K,1,3,TipNode,0)*QT( DOF_BE(K,1) )                          )*j1(K,:) &
!                     + (   TwistedSF(K,2,1,TipNode,0)*QT( DOF_BF(K,1) ) &
!                         + TwistedSF(K,2,2,TipNode,0)*QT( DOF_BF(K,2) ) &
!                         + TwistedSF(K,2,3,TipNode,0)*QT( DOF_BE(K,1) )                          )*j2(K,:) &
!                     + ( BldFlexL - 0.5* &
!                         (       AxRedBld(K,1,1,TipNode)*QT( DOF_BF(K,1) )*QT( DOF_BF(K,1) ) &
!                           +     AxRedBld(K,2,2,TipNode)*QT( DOF_BF(K,2) )*QT( DOF_BF(K,2) ) &
!                           +     AxRedBld(K,3,3,TipNode)*QT( DOF_BE(K,1) )*QT( DOF_BE(K,1) ) &
!                           + 2.0*AxRedBld(K,1,2,TipNode)*QT( DOF_BF(K,1) )*QT( DOF_BF(K,2) ) &
!                           + 2.0*AxRedBld(K,2,3,TipNode)*QT( DOF_BF(K,2) )*QT( DOF_BE(K,1) ) &
!                           + 2.0*AxRedBld(K,1,3,TipNode)*QT( DOF_BF(K,1) )*QT( DOF_BE(K,1) )   ) )*j3(K,:)
!   rQS (K,TipNode,:) = rS0S(K,TipNode,:) + HubRad*j3(K,:)                  ! Position vector from apex of rotation (point Q) to the blade tip (point S(BldFlexL)).
!   rS  (K,TipNode,:) = rQS (K,TipNode,:) + rQ                              ! Position vector from inertial frame origin      to the blade tip (point S(BldFlexL)).
!
!
!   ! Define the partial angular velocities of the tip (body M(BldFlexL)) in the
!   !   inertia frame:
!   ! NOTE: PAngVelEM(K,J,I,D,:) = the Dth-derivative of the partial angular velocity
!   !   of DOF I for body M of blade K, element J in body E.
!
!   PAngVelEM(K,TipNode,          :,0,:) = PAngVelEH(:,0,:)
!   PAngVelEM(K,TipNode,DOF_BF(K,1),0,:) = - TwistedSF(K,2,1,TipNode,1)*j1(K,:) &
!                                          + TwistedSF(K,1,1,TipNode,1)*j2(K,:)
!   PAngVelEM(K,TipNode,DOF_BF(K,2),0,:) = - TwistedSF(K,2,2,TipNode,1)*j1(K,:) &
!                                          + TwistedSF(K,1,2,TipNode,1)*j2(K,:)
!   PAngVelEM(K,TipNode,DOF_BE(K,1),0,:) = - TwistedSF(K,2,3,TipNode,1)*j1(K,:) &
!                                          + TwistedSF(K,1,3,TipNode,1)*j2(K,:)
!!    AngVelHM(K,TipNode              ,:) =  AngVelEH + QDT(DOF_BF(K,1))*PAngVelEM(K,TipNode,DOF_BF(K,1),0,:) & ! Currently
!!                                                    + QDT(DOF_BF(K,2))*PAngVelEM(K,TipNode,DOF_BF(K,2),0,:) & ! unused
!!                                                    + QDT(DOF_BE(K,1))*PAngVelEM(K,TipNode,DOF_BE(K,1),0,:)   ! calculations
!    AngPosHM(K,TipNode              ,:) =             QT (DOF_BF(K,1))*PAngVelEM(K,TipNode,DOF_BF(K,1),0,:) &
!                                                    + QT (DOF_BF(K,2))*PAngVelEM(K,TipNode,DOF_BF(K,2),0,:) &
!                                                    + QT (DOF_BE(K,1))*PAngVelEM(K,TipNode,DOF_BE(K,1),0,:)
!
!   ! Define the 1st derivatives of the partial angular velocities of the tip
!   !   (body M(BldFlexL)) in the inertia frame:
!
!! NOTE: These are currently unused by the code, therefore, they need not
!!       be calculated.  Thus, they are currently commented out.  If it
!!       turns out that they are ever needed (i.e., if inertias of the
!!       blade elements are ever added, etc...) simply uncomment out these
!!       computations:
!!                   PAngVelEM(K,TipNode,          :,1,:) = PAngVelEH(:,1,:)
!!   CALL CrossProd( PAngVelEM(K,TipNode,DOF_BF(K,1),1,:),   AngVelEH, &
!!                   PAngVelEM(K,TipNode,DOF_BF(K,1),0,:)                )
!!   CALL CrossProd( PAngVelEM(K,TipNode,DOF_BF(K,2),1,:),   AngVelEH, &
!!                   PAngVelEM(K,TipNode,DOF_BF(K,2),0,:)                )
!!   CALL CrossProd( PAngVelEM(K,TipNode,DOF_BE(K,1),1,:),   AngVelEH, &
!!                   PAngVelEM(K,TipNode,DOF_BE(K,1),0,:)                )
!
!
!   ! Define the partial linear velocities (and their 1st derivatives) of the
!   !   blade tip (point S(BldFlexL)) in the inertia frame.  Also define the
!   !   overall linear velocity of the blade tip in the inertia frame.  Also,
!   !   define the portion of the linear acceleration of the blade tip in the
!   !   inertia frame associated with everything but the QD2T()'s:
!
!   CALL CrossProd( EwHXrQS, AngVelEH, rQS(K,TipNode,:) )
!
!   PLinVelES(K,TipNode,          :,:,:) = PLinVelEQ(:,:,:)
!   PLinVelES(K,TipNode,DOF_BF(K,1),0,:) = TwistedSF(K,1,1,TipNode,0)                        *j1(K,:) &
!                                        + TwistedSF(K,2,1,TipNode,0)                        *j2(K,:) &
!                                        - (   AxRedBld(K,1,1,TipNode)*QT ( DOF_BF(K,1) ) &
!                                            + AxRedBld(K,1,2,TipNode)*QT ( DOF_BF(K,2) ) &
!                                            + AxRedBld(K,1,3,TipNode)*QT ( DOF_BE(K,1) )   )*j3(K,:)
!   PLinVelES(K,TipNode,DOF_BE(K,1),0,:) = TwistedSF(K,1,3,TipNode,0)                        *j1(K,:) &
!                                        + TwistedSF(K,2,3,TipNode,0)                        *j2(K,:) &
!                                        - (   AxRedBld(K,3,3,TipNode)*QT ( DOF_BE(K,1) ) &
!                                            + AxRedBld(K,2,3,TipNode)*QT ( DOF_BF(K,2) ) &
!                                            + AxRedBld(K,1,3,TipNode)*QT ( DOF_BF(K,1) )   )*j3(K,:)
!   PLinVelES(K,TipNode,DOF_BF(K,2),0,:) = TwistedSF(K,1,2,TipNode,0)                        *j1(K,:) &
!                                        + TwistedSF(K,2,2,TipNode,0)                        *j2(K,:) &
!                                        - (   AxRedBld(K,2,2,TipNode)*QT ( DOF_BF(K,2) ) &
!                                            + AxRedBld(K,1,2,TipNode)*QT ( DOF_BF(K,1) ) &
!                                            + AxRedBld(K,2,3,TipNode)*QT ( DOF_BE(K,1) )   )*j3(K,:)
!
!   CALL CrossProd( TmpVec1, AngVelEH, PLinVelES(K,TipNode,DOF_BF(K,1),0,:) )
!   CALL CrossProd( TmpVec2, AngVelEH, PLinVelES(K,TipNode,DOF_BE(K,1),0,:) )
!   CALL CrossProd( TmpVec3, AngVelEH, PLinVelES(K,TipNode,DOF_BF(K,2),0,:) )
!
!   PLinVelES(K,TipNode,DOF_BF(K,1),1,:) = TmpVec1 &
!                                        - (   AxRedBld(K,1,1,TipNode)*QDT( DOF_BF(K,1) ) &
!                                            + AxRedBld(K,1,2,TipNode)*QDT( DOF_BF(K,2) ) &
!                                            + AxRedBld(K,1,3,TipNode)*QDT( DOF_BE(K,1) )   )*j3(K,:)
!   PLinVelES(K,TipNode,DOF_BE(K,1),1,:) = TmpVec2 &
!                                        - (   AxRedBld(K,3,3,TipNode)*QDT( DOF_BE(K,1) ) &
!                                            + AxRedBld(K,2,3,TipNode)*QDT( DOF_BF(K,2) ) &
!                                            + AxRedBld(K,1,3,TipNode)*QDT( DOF_BF(K,1) )   )*j3(K,:)
!   PLinVelES(K,TipNode,DOF_BF(K,2),1,:) = TmpVec3 &
!                                        - (   AxRedBld(K,2,2,TipNode)*QDT( DOF_BF(K,2) ) &
!                                            + AxRedBld(K,1,2,TipNode)*QDT( DOF_BF(K,1) ) &
!                                            + AxRedBld(K,2,3,TipNode)*QDT( DOF_BE(K,1) )   )*j3(K,:)
!
!   LinVelHS               = QDT( DOF_BF(K,1) )*PLinVelES(K,TipNode,DOF_BF(K,1),0,:) &
!                          + QDT( DOF_BE(K,1) )*PLinVelES(K,TipNode,DOF_BE(K,1),0,:) &
!                          + QDT( DOF_BF(K,2) )*PLinVelES(K,TipNode,DOF_BF(K,2),0,:)
!   LinAccESt(K,TipNode,:) = QDT( DOF_BF(K,1) )*PLinVelES(K,TipNode,DOF_BF(K,1),1,:) &
!                          + QDT( DOF_BE(K,1) )*PLinVelES(K,TipNode,DOF_BE(K,1),1,:) &
!                          + QDT( DOF_BF(K,2) )*PLinVelES(K,TipNode,DOF_BF(K,2),1,:)
!
!   LinVelES               = LinVelHS + LinVelEZ
!   DO I = 1,NPH   ! Loop through all DOFs associated with the angular motion of the hub (body H)
!
!      CALL CrossProd( TmpVec0, PAngVelEH(PH(I),0,:),     rQS(K,TipNode,:)            )
!      CALL CrossProd( TmpVec1, PAngVelEH(PH(I),0,:), EwHXrQS              + LinVelHS )
!      CALL CrossProd( TmpVec2, PAngVelEH(PH(I),1,:),     rQS(K,TipNode,:)            )
!
!      PLinVelES(K,TipNode,PH(I),0,:) = PLinVelES(K,TipNode,PH(I),0,:) + TmpVec0
!      PLinVelES(K,TipNode,PH(I),1,:) = PLinVelES(K,TipNode,PH(I),1,:) + TmpVec1 + TmpVec2
!
!      LinVelES                       = LinVelES                       + QDT(PH(I))*PLinVelES(K,TipNode,PH(I),0,:)
!      LinAccESt(K,TipNode,        :) = LinAccESt(K,TipNode,        :) + QDT(PH(I))*PLinVelES(K,TipNode,PH(I),1,:)
!
!   ENDDO          ! I - all DOFs associated with the angular motion of the hub (body H)
!
!bjj end of proposed change

   ! Calculate the tip drag forces if necessary:

   IF ( CompAero )  THEN   ! Calculate the tip drag using the built-in model.

      IF ( ZTime >= TTpBrDp(K) )  THEN                                  ! The tip brakes have been deployed due to time.

         TBDrCon    = TBDrConN + ( TBDrConD - TBDrConN )*&
                      TBFract( ZTime, TTpBrDp(K), TTpBrFl(K) )

      ELSEIF ( ( QDT(DOF_GeAz) + QDT(DOF_DrTr) ) >= TBDepISp(K) )  THEN ! The tip brakes deploy due to speed.

         TTpBrDp(K) = ZTime                                             ! Use the check on time the next time step.
         TTpBrFl(K) = ZTime + TpBrDT

         TBDrCon    = TBDrConN

      ELSE                                                              ! The tip brakes haven't been deployed yet.

         TBDrCon    = TBDrConN

      ENDIF

!bjj start of proposed change - aerodyn loops
!bjj move this to earlier so we use the correct LinVelES...
!rm!JASON: USE TipNode HERE INSTEAD OF BldNodes IF YOU ALLOCATE AND DEFINE n1, n2, n3, m1, m2, AND m3 TO USE TipNode.  THIS WILL REQUIRE THAT THE AERODYNAMIC AND STRUCTURAL TWISTS, AeroTwst() AND ThetaS(), BE KNOWN AT THE TIP!!!
!rm      LinVelESm2 = DOT_PRODUCT( LinVelES, m2(K,BldNodes,:) )
!rm
!rm      FSTipDrag(K,:) = m2(K,BldNodes,:)*SIGN( 0.5*AirDens*LinVelESm2*LinVelESm2*TBDrCon, -LinVelESm2 )

      FSTipDrag(K,:) = m2(K,BldNodes,:)*SIGN( 0.5*AirDens*LinVelESm2(K)*LinVelESm2(K)*TBDrCon, -1.*LinVelESm2(K) )
!bjj end of proposed change

   ELSE                    ! Wind turbine in vacuum, no aerodynamic forces.

      FSTipDrag(K,:) = 0.0

   ENDIF


   ! Initialize the partial forces and moments (including those associated
   !   with the QD2T()'s and those that are not) at the blade root (point S(0))
   !   using the tip brake effects:

   PFrcS0B(K,:,:) = 0.0 ! Initialize these partial
   PMomH0B(K,:,:) = 0.0 ! forces and moments to zero
   DO I = 1,NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

      TmpVec1 = -TipMass(K)*PLinVelES(K,TipNode,PSE(K,I),0,:)  ! The portion of PFrcS0B associated with the tip brake
      CALL CrossProd( TmpVec2, rS0S(K,TipNode,:), TmpVec1 )    ! The portion of PMomH0B associated with the tip brake

      PFrcS0B(K,PSE(K,I),:) = TmpVec1

      PMomH0B(K,PSE(K,I),:) = TmpVec2

   ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

   TmpVec1 = FSTipDrag(K,:) - TipMass(K)*( Gravity*z2 + LinAccESt(K,TipNode,:) ) ! The portion of FrcS0Bt associated with the tip brake
   CALL CrossProd( TmpVec2, rS0S(K,TipNode,:), TmpVec1 )                         ! The portion of MomH0Bt associated with the tip brake

   FrcS0Bt(K,:) = TmpVec1

   MomH0Bt(K,:) = TmpVec2


   ! Initialize the portions of the mass matrix on and below the
   !   diagonal associated with purely blade DOFs (these portions can't
   !   be calculated using partial loads) using the tip mass effects.
   !   Also, initialize the portions of the forcing vector associated
   !   with purely blade DOFs (these portions can't be calculated using
   !   partial loads) using the tip mass effects:
   ! NOTE: The vector subscript array, PSBE(), used in the following loops must
   !       be sorted from smallest to largest DOF index in order for the loops
   !       to work to enter values only on and below the diagonal of AugMat():

   DO L = 1,NPSBE(K)    ! Loop through all active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the tip of blade K (point S(BldFlexL))
      DO I = L,NPSBE(K) ! Loop through all active (enabled) blade DOFs greater than or equal to L
         AugMat(PSBE(K,I),PSBE(K,L)) = TipMass(K)*DOT_PRODUCT( PLinVelES(K,TipNode,PSBE(K,I),0,:), &   ! [C(q,t)]B
                                                           PLinVelES(K,TipNode,PSBE(K,L),0,:)    )
      ENDDO             ! I - All active (enabled) blade DOFs greater than or equal to L
   ENDDO                ! L - All active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the tip of blade K (point S(BldFlexL))
   DO I = 1,NPSBE(K)    ! Loop through all active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the tip of blade K (point S(BldFlexL))
         AugMat(PSBE(K,I),     NAUG) =            DOT_PRODUCT( PLinVelES(K,TipNode,PSBE(K,I),0,:), &   ! {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                                           TmpVec1                               ) ! NOTE: TmpVec1 is still the portion of FrcS0Bt associated with the tip brake
   ENDDO                ! I - All active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the tip of blade K (point S(BldFlexL))



   DO J = 1,BldNodes ! Loop through the blade nodes / elements
!bjj start of proposed change - aerodyn loops
!
!
!   ! Calculate the position vector of the current node:
!
!      rS0S(K,J,:) = (   TwistedSF(K,1,1,J,0)*QT( DOF_BF(K,1) ) &  ! Position vector from the blade root (point S(0)) to the current node (point S(RNodes(J)).
!                      + TwistedSF(K,1,2,J,0)*QT( DOF_BF(K,2) ) &
!                      + TwistedSF(K,1,3,J,0)*QT( DOF_BE(K,1) )                          )*j1(K,:) &
!                  + (   TwistedSF(K,2,1,J,0)*QT( DOF_BF(K,1) ) &
!                      + TwistedSF(K,2,2,J,0)*QT( DOF_BF(K,2) ) &
!                      + TwistedSF(K,2,3,J,0)*QT( DOF_BE(K,1) )                          )*j2(K,:) &
!                  + ( RNodes(J) - 0.5* &
!                      (       AxRedBld(K,1,1,J)*QT( DOF_BF(K,1) )*QT( DOF_BF(K,1) ) &
!                        +     AxRedBld(K,2,2,J)*QT( DOF_BF(K,2) )*QT( DOF_BF(K,2) ) &
!                        +     AxRedBld(K,3,3,J)*QT( DOF_BE(K,1) )*QT( DOF_BE(K,1) ) &
!                        + 2.0*AxRedBld(K,1,2,J)*QT( DOF_BF(K,1) )*QT( DOF_BF(K,2) ) &
!                        + 2.0*AxRedBld(K,2,3,J)*QT( DOF_BF(K,2) )*QT( DOF_BE(K,1) ) &
!                        + 2.0*AxRedBld(K,1,3,J)*QT( DOF_BF(K,1) )*QT( DOF_BE(K,1) )   ) )*j3(K,:)
!      rQS (K,J,:) = rS0S(K,J,:) + HubRad*j3(K,:)                  ! Position vector from apex of rotation (point Q) to the current node (point S(RNodes(J)).
!      rS  (K,J,:) = rQS (K,J,:) + rQ                              ! Position vector from inertial frame origin      to the current node (point S(RNodes(J)).
!
!
!   ! Define the partial angular velocities of the current node (body M(RNodes(J)))
!   !   in the inertia frame:
!   ! NOTE: PAngVelEM(K,J,I,D,:) = the Dth-derivative of the partial angular velocity
!   !   of DOF I for body M of blade K, element J in body E.
!
!      PAngVelEM(K,J,          :,0,:) = PAngVelEH(:,0,:)
!      PAngVelEM(K,J,DOF_BF(K,1),0,:) = - TwistedSF(K,2,1,J,1)*j1(K,:) &
!                                       + TwistedSF(K,1,1,J,1)*j2(K,:)
!      PAngVelEM(K,J,DOF_BF(K,2),0,:) = - TwistedSF(K,2,2,J,1)*j1(K,:) &
!                                       + TwistedSF(K,1,2,J,1)*j2(K,:)
!      PAngVelEM(K,J,DOF_BE(K,1),0,:) = - TwistedSF(K,2,3,J,1)*j1(K,:) &
!                                       + TwistedSF(K,1,3,J,1)*j2(K,:)
!!       AngVelHM(K,J              ,:) =  AngVelEH + QDT(DOF_BF(K,1))*PAngVelEM(K,J,DOF_BF(K,1),0,:) & ! Currently
!!                                                 + QDT(DOF_BF(K,2))*PAngVelEM(K,J,DOF_BF(K,2),0,:) & ! unused
!!                                                 + QDT(DOF_BE(K,1))*PAngVelEM(K,J,DOF_BE(K,1),0,:)   ! calculations
!!       AngPosHM(K,J              ,:) =             QT (DOF_BF(K,1))*PAngVelEM(K,J,DOF_BF(K,1),0,:) & ! Currently
!!                                                 + QT (DOF_BF(K,2))*PAngVelEM(K,J,DOF_BF(K,2),0,:) & ! unused
!!                                                 + QT (DOF_BE(K,1))*PAngVelEM(K,J,DOF_BE(K,1),0,:)   ! calculations
!
!
!   ! Define the 1st derivatives of the partial angular velocities of the
!   !   current node (body M(RNodes(J))) in the inertia frame:
!
!! NOTE: These are currently unused by the code, therefore, they need not
!!       be calculated.  Thus, they are currently commented out.  If it
!!       turns out that they are ever needed (i.e., if inertias of the
!!       blade elements are ever added, etc...) simply uncomment out these
!!       computations:
!!                      PAngVelEM(K,J,          :,1,:) = PAngVelEH(:,1,:)
!!      CALL CrossProd( PAngVelEM(K,J,DOF_BF(K,1),1,:),   AngVelEH, &
!!                      PAngVelEM(K,J,DOF_BF(K,1),0,:)                )
!!      CALL CrossProd( PAngVelEM(K,J,DOF_BF(K,2),1,:),   AngVelEH, &
!!                      PAngVelEM(K,J,DOF_BF(K,2),0,:)                )
!!      CALL CrossProd( PAngVelEM(K,J,DOF_BE(K,1),1,:),   AngVelEH, &
!!                      PAngVelEM(K,J,DOF_BE(K,1),0,:)                )
!
!
!   ! Define the partial linear velocities (and their 1st derivatives) of the
!   !   current node (point S(RNodes(J))) in the inertia frame.  Also define
!   !   the overall linear velocity of the current node in the inertia frame.
!   !   Also, define the portion of the linear acceleration of the current node
!   !   in the inertia frame associated with everything but the QD2T()'s:
!
!      CALL CrossProd( EwHXrQS, AngVelEH, rQS(K,J,:) )
!
!      PLinVelES(K,J,          :,:,:) = PLinVelEQ(:,:,:)
!      PLinVelES(K,J,DOF_BF(K,1),0,:) = TwistedSF(K,1,1,J,0)                        *j1(K,:) &
!                                     + TwistedSF(K,2,1,J,0)                        *j2(K,:) &
!                                     - (   AxRedBld(K,1,1,J)*QT ( DOF_BF(K,1) ) &
!                                         + AxRedBld(K,1,2,J)*QT ( DOF_BF(K,2) ) &
!                                         + AxRedBld(K,1,3,J)*QT ( DOF_BE(K,1) )   )*j3(K,:)
!      PLinVelES(K,J,DOF_BE(K,1),0,:) = TwistedSF(K,1,3,J,0)                        *j1(K,:) &
!                                     + TwistedSF(K,2,3,J,0)                        *j2(K,:) &
!                                     - (   AxRedBld(K,3,3,J)*QT ( DOF_BE(K,1) ) &
!                                         + AxRedBld(K,2,3,J)*QT ( DOF_BF(K,2) ) &
!                                         + AxRedBld(K,1,3,J)*QT ( DOF_BF(K,1) )   )*j3(K,:)
!      PLinVelES(K,J,DOF_BF(K,2),0,:) = TwistedSF(K,1,2,J,0)                        *j1(K,:) &
!                                     + TwistedSF(K,2,2,J,0)                        *j2(K,:) &
!                                     - (   AxRedBld(K,2,2,J)*QT ( DOF_BF(K,2) ) &
!                                         + AxRedBld(K,1,2,J)*QT ( DOF_BF(K,1) ) &
!                                         + AxRedBld(K,2,3,J)*QT ( DOF_BE(K,1) )   )*j3(K,:)
!
!      CALL CrossProd( TmpVec1, AngVelEH, PLinVelES(K,J,DOF_BF(K,1),0,:) )
!      CALL CrossProd( TmpVec2, AngVelEH, PLinVelES(K,J,DOF_BE(K,1),0,:) )
!      CALL CrossProd( TmpVec3, AngVelEH, PLinVelES(K,J,DOF_BF(K,2),0,:) )
!
!      PLinVelES(K,J,DOF_BF(K,1),1,:) = TmpVec1 &
!                                     - (   AxRedBld(K,1,1,J)*QDT( DOF_BF(K,1) ) &
!                                         + AxRedBld(K,1,2,J)*QDT( DOF_BF(K,2) ) &
!                                         + AxRedBld(K,1,3,J)*QDT( DOF_BE(K,1) )   )*j3(K,:)
!      PLinVelES(K,J,DOF_BE(K,1),1,:) = TmpVec2 &
!                                     - (   AxRedBld(K,3,3,J)*QDT( DOF_BE(K,1) ) &
!                                         + AxRedBld(K,2,3,J)*QDT( DOF_BF(K,2) ) &
!                                         + AxRedBld(K,1,3,J)*QDT( DOF_BF(K,1) )   )*j3(K,:)
!      PLinVelES(K,J,DOF_BF(K,2),1,:) = TmpVec3 &
!                                     - (   AxRedBld(K,2,2,J)*QDT( DOF_BF(K,2) ) &
!                                         + AxRedBld(K,1,2,J)*QDT( DOF_BF(K,1) ) &
!                                         + AxRedBld(K,2,3,J)*QDT( DOF_BE(K,1) )   )*j3(K,:)
!
!      LinVelHS         = QDT( DOF_BF(K,1) )*PLinVelES(K,J,DOF_BF(K,1),0,:) &
!                       + QDT( DOF_BE(K,1) )*PLinVelES(K,J,DOF_BE(K,1),0,:) &
!                       + QDT( DOF_BF(K,2) )*PLinVelES(K,J,DOF_BF(K,2),0,:)
!      LinAccESt(K,J,:) = QDT( DOF_BF(K,1) )*PLinVelES(K,J,DOF_BF(K,1),1,:) &
!                       + QDT( DOF_BE(K,1) )*PLinVelES(K,J,DOF_BE(K,1),1,:) &
!                       + QDT( DOF_BF(K,2) )*PLinVelES(K,J,DOF_BF(K,2),1,:)
!
!      LinVelES         = LinVelHS + LinVelEZ
!      DO I = 1,NPH   ! Loop through all DOFs associated with the angular motion of the hub (body H)
!
!         CALL CrossProd( TmpVec0, PAngVelEH(PH(I),0,:),     rQS(K,J,:)            )
!         CALL CrossProd( TmpVec1, PAngVelEH(PH(I),0,:), EwHXrQS        + LinVelHS )
!         CALL CrossProd( TmpVec2, PAngVelEH(PH(I),1,:),     rQS(K,J,:)            )
!
!         PLinVelES(K,J,PH(I),0,:) = PLinVelES(K,J,PH(I),0,:) + TmpVec0
!         PLinVelES(K,J,PH(I),1,:) = PLinVelES(K,J,PH(I),1,:) + TmpVec1 + TmpVec2
!
!         LinVelES                 = LinVelES                 + QDT(PH(I))*PLinVelES(K,J,PH(I),0,:)
!         LinAccESt(K,J,        :) = LinAccESt(K,J,        :) + QDT(PH(I))*PLinVelES(K,J,PH(I),1,:)
!
!      ENDDO          ! I - all DOFs associated with the angular motion of the hub (body H)
!bjj end of proposed change

   ! Calculate the normal and tangential aerodynamic forces and the aerodynamic
   !   pitching moment at the current element per unit span by calling AeroDyn,
   !   if necessary:

      IF ( CompAero )  THEN   ! Calculate the blade element aerodynamic loads using AeroDyn.


   ! Calculate the aerodynamic pitching moment arm (i.e., the position vector
   !   from point S on the blade to the aerodynamic center of the element):

!bjj start of proposed change - aerodyn loops
         rSAerCen = rSAerCenn1(K,J)*n1(K,J,:) + rSAerCenn2(K,J)*n2(K,J,:)        ! bjj this is now re-calculated.
!
!!
!   ! Define ElAeroLoc(:), ElRad, and VES(:), which are stored in
!   !   FAST_Mods.f90/AeroElem(), and are then USEd by AeroDyn.
!
!         rPAerCen     = rPQ + rQS(K,J,:) + rSAerCen         ! Position vector from teeter pin (point P)  to blade analysis node aerodynamic center.
!         rAerCen      =       rS (K,J,:) + rSAerCen         ! Position vector from inertial frame origin to blade analysis node aerodynamic center.
!
!         ElAeroLoc(1) =  rAerCen(1)                         ! = the distance from the undeflected tower centerline                                     to the current blade aerodynamic center in the xi ( z1) direction
!         ElAeroLoc(2) = -rAerCen(3)                         ! = the distance from the undeflected tower centerline                                     to the current blade aerodynamic center in the yi (-z3) direction
!!bjj start of proposed change AD v13.00b
!!rm         ElAeroLoc(3) =  rAerCen(2) - RefHH                 ! = the distance from the nominal hub position (i.e., the undeflected position of the hub) to the current blade aerodynamic center in the zi ( z2) direction
!         ElAeroLoc(3) =  rAerCen(2) - PtfmRef               ! = the distance from the nominal tower base position (i.e., the undeflected position of the tower base) to the current blade aerodynamic center in the zi ( z2) direction
!!bjj end of proposed change
!
!         ElRad = SQRT(   ( DOT_PRODUCT( rPAerCen, e2) )**2 &    ! = the perpendicular distance from the low-speed shaft to the current blade aerodynamic center.
!                       + ( DOT_PRODUCT( rPAerCen, e3) )**2   )
!
!bjj end of proposed change


!JASON: WE SHOULD REALLY BE PASSING TO AERODYN THE LINEAR VELOCITIES OF THE AERODYNAMIC CENTER IN THE INERTIA FRAME, NOT SIMPLY THE LINEAR VELOCITIES OF POINT S.  IS THERE ANY WAY OF GETTING THIS VELOCITY?<--DO THIS, WHEN YOU ADD THE COUPLED MODE SHAPES!!!!
!bjj start of proposed change
!rm         VES(1) = DOT_PRODUCT( LinVelES, m1(K,J,:) )      ! Absolute velocity of the current node
!rm         VES(2) = DOT_PRODUCT( LinVelES, m2(K,J,:) )      ! in the inertia frame expressd in the
!rm         VES(3) = DOT_PRODUCT( LinVelES, m3(K,J,:) )      ! m-vector triad for this blade element.
!bjj end of proposed change

   ! Call AeroDyn through AeroCalc() and fill FSAero() and MMAero() with
   !   the resulting forces (AeroForces(:)):
   ! NOTE: AeroForces(1) = element normal     force per unit span in the  m1 direction (N/m).
   !       AeroForces(2) = element tangential force per unit span in the -m2 direction (N/m).
   !       AeroForces(3) = element pitching moment  per unit span in about the m3-axis (N-m/m).


!bjj start of proposed change - aerodyn loops

!rm         CALL AeroCalc( ZTime, K, J, LinVelES, AeroForces )
!rm
!rm         FSAero(K,J,:) = AeroForces(1)*m1(K,J,:) - AeroForces(2)*m2(K,J,:)
!rm
!rm         CALL CrossProd( MMAero(K,J,:), rSAerCen, FSAero(K,J,:) )
!rm         MMAero(K,J,:) = MMAero(K,J,:) + AeroForces(3)*m3(K,J,:)

!!      AeroForces = (/ ADCurrentOutputs(J, K)%DFN &
!!                      ADCurrentOutputs(J, K)%DFT &
!!                      ADCurrentOutputs(J, K)%PMA   /)


!BJJ: Multiply ADCurrentOutputs by te1, te2, and te3, instead of m1, m2 and m3
!old     FSAero(K,J,:) = ADCurrentOutputs(J, K)%DFN * m1(K,J,:) - ADCurrentOutputs(J, K)%DFT * m2(K,J,:)
         FSAero(K,J,:) = ADAeroLoads%Blade(J, K)%Force(1) * te1(K,J,:) + ADAeroLoads%Blade(J, K)%Force(2) * te2(K,J,:)

         CALL CrossProd( MMAero(K,J,:), rSAerCen, FSAero(K,J,:) )
!bjj:
!old     MMAero(K,J,:) = MMAero(K,J,:) + ADCurrentOutputs(J, K)%PMA * m3(K,J,:)
         MMAero(K,J,:) = MMAero(K,J,:) + ADAeroLoads%Blade(J, K)%Moment(3) * te3(K,J,:)

!write( *, '(20(G12.5,X))') ZTime, ADAeroMarkers%Blade(J,K)%Position, ADAeroMarkers%Blade(J,K)%TranslationVel, ADAeroMarkers%Blade(J,K)%Orientation, & !FSAero(K,J,:), MMAero(K,J,:)
!         ADCurrentOutputs(J, K)%DFN, ADCurrentOutputs(J, K)%DFT, ADCurrentOutputs(J, K)%PMA

!bjj end of proposed change

      ELSE                    ! Wind turbine in vacuum, no aerodynamic forces.

         FSAero(K,J,:) = 0.0
         MMAero(K,J,:) = 0.0

      ENDIF


   ! Calculate the mass of the current element

      ElmntMass = MassB(K,J)*DRNodes(J)   ! Mass of blade element J


   ! Integrate to find the partial forces and moments (including those associated
   !   with the QD2T()'s and those that are not) at the blade root (point S(0)):

      DO I = 1,NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

         TmpVec1 = -ElmntMass*PLinVelES(K,J,PSE(K,I),0,:)   ! The portion of PFrcS0B associated with blade element J
         CALL CrossProd( TmpVec2, rS0S(K,J,:), TmpVec1 )    ! The portion of PMomH0B associated with blade element J

         PFrcS0B(K,PSE(K,I),:) = PFrcS0B(K,PSE(K,I),:) + TmpVec1

         PMomH0B(K,PSE(K,I),:) = PMomH0B(K,PSE(K,I),:) + TmpVec2

      ENDDO             ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

      TmpVec1 = FSAero(K,J,:)*DRNodes(J) - ElmntMass*( Gravity*z2 + LinAccESt(K,J,:) ) ! The portion of FrcS0Bt associated with blade element J
      CALL CrossProd( TmpVec2, rS0S(K,J,:), TmpVec1 )                                  ! The portion of MomH0Bt associated with blade element J
      TmpVec3 = MMAero(K,J,:)*DRNodes(J)                                               ! The total external moment applied to blade element J

      FrcS0Bt(K,:) = FrcS0Bt(K,:) + TmpVec1

      MomH0Bt(K,:) = MomH0Bt(K,:) + TmpVec2 + TmpVec3


   ! Integrate to find the portions of the mass matrix on and below the
   !   diagonal associated with purely blade DOFs (these portions can't
   !   be calculated using partial loads).  Also, integrate to find the
   !   portions of the forcing vector associated with purely blade DOFs
   !   (these portions can't be calculated using partial loads):
   ! NOTE: The vector subscript array, PSBE(), used in the following loops must
   !       be sorted from smallest to largest DOF index in order for the loops
   !       to work to enter values only on and below the diagonal of AugMat():

      DO L = 1,NPSBE(K)    ! Loop through all active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade
         DO I = L,NPSBE(K) ! Loop through all active (enabled) blade DOFs greater than or equal to L
            AugMat(PSBE(K,I),PSBE(K,L)) = AugMat(PSBE(K,I),PSBE(K,L)) + ElmntMass*&
                                          DOT_PRODUCT( PLinVelES(K,J,PSBE(K,I),0,:), &           ! [C(q,t)]B
                                                   PLinVelES(K,J,PSBE(K,L),0,:)          )
         ENDDO             ! I - All active (enabled) blade DOFs greater than or equal to L
      ENDDO                ! L - All active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade
      DO I = 1,NPSBE(K)    ! Loop through all active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade
            AugMat(PSBE(K,I),     NAUG) = AugMat(PSBE(K,I),     NAUG)                      & ! {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                        + DOT_PRODUCT( PLinVelES(K,J,PSBE(K,I),0,:), TmpVec1 ) & ! NOTE: TmpVec1 is still the portion of FrcS0Bt associated with blade element J
                                        + DOT_PRODUCT( PAngVelEM(K,J,PSBE(K,I),0,:), TmpVec3 )   !       and TmpVec3 is still the total external moment applied to blade element J
      ENDDO                ! I - All active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade


   ENDDO ! J - Blade nodes / elements



   ! Add the blade effects to the partial forces and moments (including
   !   those associated with the QD2T()'s and those that are not) at the
   !   teeter pin (point P):

   DO I = 1,NPSE(K)  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

      CALL CrossProd( TmpVec, rPS0, PFrcS0B(K,PSE(K,I),:) ) ! The portion of PMomLPRot associated with PFrcS0B.

      PFrcPRot (PSE(K,I),:) = PFrcPRot (PSE(K,I),:) + PFrcS0B(K,PSE(K,I),:)

      PMomLPRot(PSE(K,I),:) = PMomLPRot(PSE(K,I),:) + PMomH0B(K,PSE(K,I),:) + TmpVec

   ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of blade K

   CALL CrossProd( TmpVec, rPS0, FrcS0Bt(K,:) )       ! The portion of MomLPRott associated with FrcS0Bt.

   FrcPRott  = FrcPRott  + FrcS0Bt(K,:)

   MomLPRott = MomLPRott + MomH0Bt(K,:) + TmpVec



   ! Initialize the portions of the mass matrix below the diagonal associated
   !   with the teeter and pure blade DOFs using the partial loads at the
   !   teeter pin; only do this if necessary:

   IF ( ( NumBl == 2 ) .AND. ( DOF_Flag(DOF_Teet) ) )  THEN
      DO L = 1,NPSBE(K) ! Loop through all active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade
         AugMat(DOF_Teet,PSBE(K,L)) = -DOT_PRODUCT( PAngVelEH(DOF_Teet,0,:), PMomLPRot(PSBE(K,L),:) )  ! [C(q,t)]B
      ENDDO             ! L - All active (enabled) blade DOFs that contribute to the QD2T-related linear accelerations of the blade
   ENDIF



   ! If the associated DOFs are enabled, add the blade elasticity and damping
   !   forces to the forcing vector (these portions can't be calculated using
   !   partial loads):

   IF ( DOF_Flag(DOF_BF(K,1)) )  THEN
      AugMat(    DOF_BF(K,1),NAUG) = AugMat(DOF_BF(K,1),NAUG)    & !
                                   - KBF(K,1,1)* QT(DOF_BF(K,1)) &
                                   - KBF(K,1,2)* QT(DOF_BF(K,2)) &
                                   - CBF(K,1,1)*QDT(DOF_BF(K,1)) &
                                   - CBF(K,1,2)*QDT(DOF_BF(K,2))
   ENDIF
   IF ( DOF_Flag(DOF_BF(K,2)) )  THEN
      AugMat(    DOF_BF(K,2),NAUG) = AugMat(DOF_BF(K,2),NAUG)    & ! {-f(qd,q,t)}ElasticB + {-f(qd,q,t)}DampB
                                   - KBF(K,2,1)* QT(DOF_BF(K,1)) &
                                   - KBF(K,2,2)* QT(DOF_BF(K,2)) &
                                   - CBF(K,2,1)*QDT(DOF_BF(K,1)) &
                                   - CBF(K,2,2)*QDT(DOF_BF(K,2))
   ENDIF
   IF ( DOF_Flag(DOF_BE(K,1)) )  THEN
      AugMat(    DOF_BE(K,1),NAUG) = AugMat(DOF_BE(K,1),NAUG)    & !
                                   - KBE(K,1,1)* QT(DOF_BE(K,1)) &
                                   - CBE(K,1,1)*QDT(DOF_BE(K,1))
   ENDIF


ENDDO ! K - Blades



   ! Define the partial forces and moments (including those associated with
   !   the QD2T()'s and those that are not) at the specified point on the
   !   rotor-furl axis (point V) / nacelle (body N) using the structure that
   !   furls with the rotor, generator, and rotor effects.

PFrcVGnRt = PFrcPRot    ! Initialize these partial forces and
PMomNGnRt = PMomLPRot   ! moments using the rotor effects
DO I = 1,NActvDOF ! Loop through all active (enabled) DOFs

   CALL CrossProd( TmpVec , rVP, PFrcPRot(SrtPS(I),:) )  ! The portion of PMomNGnRt associated with the PFrcPRot

   PMomNGnRt(SrtPS(I),:) = PMomNGnRt(SrtPS(I),:) + TmpVec

ENDDO             ! I - All active (enabled) DOFs
DO I = 1,NPDE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the center of mass of the structure that furls with the rotor (not including rotor) (point D)

   TmpVec1 = -RFrlMass*PLinVelED(PDE(I)  ,0,:)           ! The portion of PFrcVGnRt associated with the RFrlMass
   CALL CrossProd( TmpVec2, rVD,              TmpVec1 )  ! The portion of PMomNGnRt associated with the RFrlMass

   PFrcVGnRt(PDE(I)  ,:) = PFrcVGnRt(PDE(I)  ,:) + TmpVec1

   PMomNGnRt(PDE(I)  ,:) = PMomNGnRt(PDE(I)  ,:) + TmpVec2                      &
                         - RrfaIner*rfa*DOT_PRODUCT( rfa, PAngVelER(PDE(I)  ,0,:) ) &
                         -  GenIner*c1 *DOT_PRODUCT( c1 , PAngVelEG(PDE(I)  ,0,:) )

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the center of mass of the structure that furls with the rotor (not including rotor) (point D)
IF ( DOF_Flag(DOF_GeAz) )  THEN

   PMomNGnRt(DOF_GeAz,:) = PMomNGnRt(DOF_GeAz,:)                                &   ! The previous loop (DO I = 1,NPDE) misses the DOF_GeAz-contribution to: ( Generator inertia dyadic ) dot ( partial angular velocity of the generator in the inertia frame )
                         -  GenIner*c1 *DOT_PRODUCT( c1 , PAngVelEG(DOF_GeAz,0,:) )     ! Thus, add this contribution if necessary.

ENDIF

TmpVec1 = -RFrlMass*( Gravity*z2 + LinAccEDt )     ! The portion of FrcVGnRtt associated with the RFrlMass
CALL CrossProd( TmpVec2, rVD      ,  TmpVec1 )     ! The portion of MomNGnRtt associated with the RFrlMass
CALL CrossProd( TmpVec3, rVP      , FrcPRott )     ! The portion of MomNGnRtt associated with the FrcPRott
TmpVec  = RrfaIner*rfa*DOT_PRODUCT( rfa, AngVelER )    ! = ( R inertia dyadic ) dot ( angular velocity of structure that furls with the rotor in the inertia frame )
CALL CrossProd( TmpVec4, -AngVelER, TmpVec )       ! = ( -angular velocity of structure that furls with the rotor in the inertia frame ) cross ( TmpVec )
TmpVec  =  GenIner*c1* DOT_PRODUCT( c1 , AngVelEG )    ! = ( Generator inertia dyadic ) dot ( angular velocity of generator in the inertia frame )
CALL CrossProd( TmpVec5, -AngVelEG, TmpVec )       ! = ( -angular velocity of generator in the inertia frame ) cross ( TmpVec )

FrcVGnRtt = FrcPRott  + TmpVec1

MomNGnRtt = MomLPRott + TmpVec2 + TmpVec3 + TmpVec4 + TmpVec5 &
          - RrfaIner*rfa*DOT_PRODUCT( rfa, AngAccERt )            &
          -  GenIner*c1 *DOT_PRODUCT( c1 , AngAccEGt )



   ! Let's compute the tail aerodynamic loads, if necessary:

IF ( CompAero )  THEN   ! Calculate the tail aerodynamic forces using AeroDyn.


   ! Compute TFinKFx, TFinKFy, and TFinKMz:

   CALL TFinAero( rK(1), -rK(3), rK(2),     &
                  DOT_PRODUCT( LinVelEK,  p1 ), &
                  DOT_PRODUCT( LinVelEK, -p3 ), &
                  DOT_PRODUCT( LinVelEK,  p2 )    )


   ! Vectorize these values into FKAero and MAAero:

   FKAero = TFinKFx*p1 - TFinKFy*p3
   MAAero = TFinKMz*p2


ELSE                    ! Wind turbine in vacuum, no aerodynamic forces.


   FKAero = 0.0
   MAAero = 0.0


ENDIF



   ! Define the partial forces and moments (including those associated with
   !   the QD2T()'s and those that are not) at the specified point on the
   !   tail-furl axis (point W) / nacelle (body N) using the tail effects.

PFrcWTail = 0.0   ! Initialize these partial
PMomNTail = 0.0   ! forces and moments to zero
DO I = 1,NPIE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tail boom center of mass (point I)

   TmpVec1 = -BoomMass*PLinVelEI(PIE(I),0,:)    ! The portion of PFrcWTail associated with the BoomMass
   TmpVec2 = -TFinMass*PLinVelEJ(PIE(I),0,:)    ! The portion of PFrcWTail associated with the TFinMass
   CALL CrossProd( TmpVec3, rWI, TmpVec1 )      ! The portion of PMomNTail associated with the BoomMass
   CALL CrossProd( TmpVec4, rWJ, TmpVec2 )      ! The portion of PMomNTail associated with the TFinMass

   PFrcWTail(PIE(I),:) = TmpVec1 + TmpVec2

   PMomNTail(PIE(I),:) = TmpVec3 + TmpVec4 &
                       - AtfaIner*tfa*DOT_PRODUCT( tfa, PAngVelEA(PIE(I),0,:) )

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tail boom center of mass (point I)

TmpVec1 = -BoomMass*( Gravity*z2 + LinAccEIt )  ! The portion of FrcWTailt associated with the BoomMass
TmpVec2 = -TFinMass*( Gravity*z2 + LinAccEJt )  ! The portion of FrcWTailt associated with the TFinMass
CALL CrossProd( TmpVec3, rWI      , TmpVec1 )   ! The portion of MomNTailt associated with the BoomMass
CALL CrossProd( TmpVec4, rWJ      , TmpVec2 )   ! The portion of MomNTailt associated with the TFinMass
TmpVec  = AtfaIner*tfa*DOT_PRODUCT( tfa, AngVelEA ) ! = ( A inertia dyadic ) dot ( angular velocity of the tail in the inertia frame )
CALL CrossProd( TmpVec5, -AngVelEA, TmpVec  )   ! = ( -angular velocity of the tail in the inertia frame ) cross ( TmpVec )
CALL CrossProd( TmpVec , rWK      , FKAero  )   ! The portion of MomNTailt associated with FKAero

FrcWTailt = FKAero + TmpVec1 + TmpVec2

MomNTailt = MAAero + TmpVec3 + TmpVec4 + TmpVec5 + TmpVec &
          - AtfaIner*tfa*DOT_PRODUCT( tfa, AngAccEAt )



   ! Define the partial forces and moments (including those associated with
   !   the QD2T()'s and those that are not) at the yaw bearing (point O) /
   !   base plate (body B) using the nacelle, generator, rotor, and tail effects.

PFrcONcRt = PFrcVGnRt + PFrcWTail   ! Initialize these partial forces and moments using
PMomBNcRt = PMomNGnRt + PMomNTail   ! the rotor, rotor-furl, generator, and tail effects
DO I = 1,NActvDOF ! Loop through all active (enabled) DOFs

   CALL CrossProd( TmpVec , rOV, PFrcVGnRt(SrtPS(I),:) ) ! The portion of PMomBNcRt associated with the PFrcVGnRt

   PMomBNcRt(SrtPS(I),:) = PMomBNcRt(SrtPS(I),:) + TmpVec

ENDDO             ! I - All active (enabled) DOFs
DO I = 1,NPIE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tail boom center of mass (point I)

   CALL CrossProd( TmpVec , rOW, PFrcWTail(PIE(I)  ,:) ) ! The portion of PMomBNcRt associated with the PFrcWTail

   PMomBNcRt(PIE(I)  ,:) = PMomBNcRt(PIE(I)  ,:) + TmpVec

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tail boom center of mass (point I)
DO I = 1,NPUE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the nacelle center of mass (point U)

   TmpVec1 = -NacMass*PLinVelEU(PUE(I),0,:)              ! The portion of PFrcONcRt associated with the NacMass
   CALL CrossProd( TmpVec2, rOU,               TmpVec1 ) ! The portion of PMomBNcRt associated with the NacMass

   PFrcONcRt(PUE(I)  ,:) = PFrcONcRt(PUE(I)  ,:) + TmpVec1

   PMomBNcRt(PUE(I)  ,:) = PMomBNcRt(PUE(I)  ,:) + TmpVec2 &
                         - Nacd2Iner*d2*DOT_PRODUCT( d2, PAngVelEN(PUE(I),0,:) )

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the nacelle center of mass (point U)

TmpVec1 = -NacMass*( Gravity*z2 + LinAccEUt )      ! The portion of FrcONcRtt associated with the NacMass
CALL CrossProd( TmpVec2, rOU      ,   TmpVec1 )    ! The portion of MomBNcRtt associated with the NacMass
CALL CrossProd( TmpVec3, rOV      , FrcVGnRtt )    ! The portion of MomBNcRtt associated with the FrcVGnRtt
CALL CrossProd( TmpVec4, rOW      , FrcWTailt )    ! The portion of MomBNcRtt associated with the FrcWTailt
TmpVec  = Nacd2Iner*d2*DOT_PRODUCT( d2, AngVelEN )     ! = ( Nacelle inertia dyadic ) dot ( angular velocity of nacelle in the inertia frame )
CALL CrossProd( TmpVec5, -AngVelEN, TmpVec    )    ! = ( -angular velocity of nacelle in the inertia frame ) cross ( TmpVec )

FrcONcRtt = FrcVGnRtt + FrcWTailt + TmpVec1

MomBNcRtt = MomNGnRtt + MomNTailt + TmpVec2 + TmpVec3 + TmpVec4 + TmpVec5 &
          - Nacd2Iner*d2*DOT_PRODUCT( d2, AngAccENt )



   ! Initialize the partial forces and moments (including those associated
   !   with the QD2T()'s and those that are not) at the tower base (point T(0))
   !   using everything but the tower:

PFrcT0Trb = PFrcONcRt   ! Initialize these partial forces and moments
PMomX0Trb = PMomBNcRt   ! using all of the effects above the yaw bearing
DO I = 1,NActvDOF ! Loop through all active (enabled) DOFs

   CALL CrossProd( TmpVec , rT0O, PFrcONcRt(SrtPS(I),:) )   ! The portion of PMomX0Trb associated with the PFrcONcRt

   PMomX0Trb(SrtPS(I),:) = PMomX0Trb(SrtPS(I),:) + TmpVec

ENDDO             ! I - All active (enabled) DOFs
DO I = 1,NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)

   TmpVec1 = -YawBrMass*PLinVelEO(PTE(I),0,:)               ! The portion of PFrcT0Trb associated with the YawBrMass
   CALL CrossProd( TmpVec2, rT0O,               TmpVec1 )   ! The portion of PMomX0Trb associated with the YawBrMass

   PFrcT0Trb(PTE(I)  ,:) = PFrcT0Trb(PTE(I)  ,:) + TmpVec1

   PMomX0Trb(PTE(I)  ,:) = PMomX0Trb(PTE(I)  ,:) + TmpVec2

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing center of mass (point O)

TmpVec1 = -YawBrMass*( Gravity*z2 + LinAccEOt ) ! The portion of FrcT0Trbt associated with the YawBrMass
CALL CrossProd( TmpVec2, rT0O,   TmpVec1 )      ! The portion of MomX0Trbt associated with the YawBrMass
CALL CrossProd( TmpVec3, rT0O, FrcONcRtt )      ! The portion of MomX0Trbt associated with the FrcONcRtt

FrcT0Trbt = FrcONcRtt + TmpVec1

MomX0Trbt = MomBNcRtt + TmpVec2 + TmpVec3



   ! Initialize the portions of the mass matrix on and below the diagonal
   !   associated with purely tower DOFs (these portions can't be calculated
   !   using partial loads) using the yaw bearing mass effects.
   !   Also, initialize the portions of the forcing vector associated with
   !   purely blade DOFs (these portions can't be calculated using partial
   !   loads) using the yaw bearing mass effects:
   ! NOTE: The vector subscript array, PTTE(), used in the following loops must
   !       be sorted from smallest to largest DOF index in order for the loops
   !       to work to enter values only on and below the diagonal of AugMat():

DO L = 1,NPTTE    ! Loop through all active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing (point O)
   DO I = L,NPTTE ! Loop through all active (enabled) tower DOFs greater than or equal to L
      AugMat(PTTE(I),PTTE(L)) = YawBrMass*DOT_PRODUCT( PLinVelEO(PTTE(I),0,:), &     ! [C(q,t)]T of YawBrMass
                                                   PLinVelEO(PTTE(L),0,:)    )
   ENDDO          ! I - All active (enabled) tower DOFs greater than or equal to L
ENDDO             ! L - All active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing (point O)
DO I = 1,NPTTE    ! Loop through all active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing (point O)
      AugMat(PTTE(I),   NAUG) =           DOT_PRODUCT( PLinVelEO(PTTE(I),0,:), &     ! {-f(qd,q,t)}T + {-f(qd,q,t)}GravT of YawBrMass
                                                   TmpVec1                   )   ! NOTE: TmpVec1 is still the portion of FrcT0Trbt associated with YawBrMass
ENDDO             ! I - All active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the yaw bearing (point O)



DO J = 1,TwrNodes  ! Loop through the tower nodes / elements


   ! Calculate the position vector of the current node:

   rT0T(J,:) = ( TwrFASF(1,J,0)*QT(DOF_TFA1) + TwrFASF(2,J,0)*QT(DOF_TFA2)             )*a1 &   ! Position vector from base of flexible portion of tower (point T(0)) to current node (point T(J)).
             + ( HNodes(J) - 0.5*(       AxRedTFA(1,1,J)*QT(DOF_TFA1)*QT(DOF_TFA1) &
                                   +     AxRedTFA(2,2,J)*QT(DOF_TFA2)*QT(DOF_TFA2) &
                                   + 2.0*AxRedTFA(1,2,J)*QT(DOF_TFA1)*QT(DOF_TFA2) &
                                   +     AxRedTSS(1,1,J)*QT(DOF_TSS1)*QT(DOF_TSS1) &
                                   +     AxRedTSS(2,2,J)*QT(DOF_TSS2)*QT(DOF_TSS2) &
                                   + 2.0*AxRedTSS(1,2,J)*QT(DOF_TSS1)*QT(DOF_TSS2)   ) )*a2 &
             + ( TwrSSSF(1,J,0)*QT(DOF_TSS1) + TwrSSSF(2,J,0)*QT(DOF_TSS2)             )*a3
   rZT (J,:) = rZT0 + rT0T(J,:)                                                                 ! Position vector from platform reference (point Z) to the current node (point T(HNodes(J)).

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:
   rT        = rZ   + rZT (J,:)                                                                 ! Position vector from inertial frame origin        to the current node (point T(HNodes(J)).
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:
!remove6.02a   ! Define the partial angular velocities of the current node (body F(HNodes(J))
!remove6.02a   !   in the inertia frame:
   ! Define the partial angular velocities (and their 1st derivatives) of the
   !   current node (body F(HNodes(J))  in the inertia frame.  Also define
   !   the overall angular velocity of the current node in the inertia frame.
   !   Also, define the portion of the angular acceleration of the current node
   !   in the inertia frame associated with everything but the QD2T()'s:
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

   ! NOTE: PAngVelEF(J,I,D,:) = the Dth-derivative of the partial angular velocity
   !   of DOF I for body F of element J in body E.

   PAngVelEF(J,       :,0,:) = PAngVelEX(:,0,:)
   PAngVelEF(J,DOF_TFA1,0,:) = -TwrFASF(1,J,1)*a3
   PAngVelEF(J,DOF_TSS1,0,:) =  TwrSSSF(1,J,1)*a1
   PAngVelEF(J,DOF_TFA2,0,:) = -TwrFASF(2,J,1)*a3
   PAngVelEF(J,DOF_TSS2,0,:) =  TwrSSSF(2,J,1)*a1

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:
!remove6.02a
!remove6.02a   ! Define the 1st derivatives of the partial angular velocities of the
!remove6.02a   !   current node (body F(HNodes(J)) in the inertia frame:
!remove6.02a
!remove6.02a! NOTE: These are currently unused by the code, therefore, they need not
!remove6.02a!       be calculated.  Thus, they are currently commented out.  If it
!remove6.02a!       turns out that they are ever needed (i.e., if inertias of the
!remove6.02a!       tower elements are ever added, etc...) simply uncomment out these
!remove6.02a!       computations:
!remove6.02a!                   PAngVelEF(J,       :,1,:) = PAngVelEX(:,1,:)
!remove6.02a!   CALL CrossProd( PAngVelEF(J,DOF_TFA1,1,:) ,  AngVelEX       , PAngVelEF(J,DOF_TFA1,0,:) )
!remove6.02a!   CALL CrossProd( PAngVelEF(J,DOF_TSS1,1,:) ,  AngVelEX       , PAngVelEF(J,DOF_TSS1,0,:) )
!remove6.02a!   CALL CrossProd( PAngVelEF(J,DOF_TFA2,1,:) ,  AngVelEX       , PAngVelEF(J,DOF_TFA2,0,:) )
!remove6.02a!   CALL CrossProd( PAngVelEF(J,DOF_TSS2,1,:) ,  AngVelEX       , PAngVelEF(J,DOF_TSS2,0,:) )
!remove6.02a
                   PAngVelEF (J,       :,1,:) = PAngVelEX(:,1,:)
   CALL CrossProd( PAngVelEF (J,DOF_TFA1,1,:) ,  AngVelEX       ,          PAngVelEF(J,DOF_TFA1,0,:) )
   CALL CrossProd( PAngVelEF (J,DOF_TSS1,1,:) ,  AngVelEX       ,          PAngVelEF(J,DOF_TSS1,0,:) )
   CALL CrossProd( PAngVelEF (J,DOF_TFA2,1,:) ,  AngVelEX       ,          PAngVelEF(J,DOF_TFA2,0,:) )
   CALL CrossProd( PAngVelEF (J,DOF_TSS2,1,:) ,  AngVelEX       ,          PAngVelEF(J,DOF_TSS2,0,:) )

                    AngVelEF       =             AngVelEX  + QDT(DOF_TFA1)*PAngVelEF(J,DOF_TFA1,0,:) &
                                                           + QDT(DOF_TSS1)*PAngVelEF(J,DOF_TSS1,0,:) &
                                                           + QDT(DOF_TFA2)*PAngVelEF(J,DOF_TFA2,0,:) &
                                                           + QDT(DOF_TSS2)*PAngVelEF(J,DOF_TSS2,0,:)
                    AngPosEF       =             AngPosEX  + QT (DOF_TFA1)*PAngVelEF(J,DOF_TFA1,0,:) &
                                                           + QT (DOF_TSS1)*PAngVelEF(J,DOF_TSS1,0,:) &
                                                           + QT (DOF_TFA2)*PAngVelEF(J,DOF_TFA2,0,:) &
                                                           + QT (DOF_TSS2)*PAngVelEF(J,DOF_TSS2,0,:)
                    AngAccEFt(J,:) =             AngAccEXt + QDT(DOF_TFA1)*PAngVelEF(J,DOF_TFA1,1,:) &
                                                           + QDT(DOF_TSS1)*PAngVelEF(J,DOF_TSS1,1,:) &
                                                           + QDT(DOF_TFA2)*PAngVelEF(J,DOF_TFA2,1,:) &
                                                           + QDT(DOF_TSS2)*PAngVelEF(J,DOF_TSS2,1,:)
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! Define the partial linear velocities (and their 1st derivatives) of the
   !   current node (point T(HNodes(J))) in the inertia frame.  Also define
   !   the overall linear velocity of the current node in the inertia frame.
   !   Also, define the portion of the linear acceleration of the current node
   !   in the inertia frame associated with everything but the QD2T()'s:

   CALL CrossProd( EwXXrZT, AngVelEX, rZT(J,:) )

   PLinVelET(J,       :,:,:) = PLinVelEZ(:,:,:)
   PLinVelET(J,DOF_TFA1,0,:) = TwrFASF(1,J,0)*a1 - (   AxRedTFA(1,1,J)* QT(DOF_TFA1) &
                                                     + AxRedTFA(1,2,J)* QT(DOF_TFA2)   )*a2
   PLinVelET(J,DOF_TSS1,0,:) = TwrSSSF(1,J,0)*a3 - (   AxRedTSS(1,1,J)* QT(DOF_TSS1) &
                                                     + AxRedTSS(1,2,J)* QT(DOF_TSS2)   )*a2
   PLinVelET(J,DOF_TFA2,0,:) = TwrFASF(2,J,0)*a1 - (   AxRedTFA(2,2,J)* QT(DOF_TFA2) &
                                                     + AxRedTFA(1,2,J)* QT(DOF_TFA1)   )*a2
   PLinVelET(J,DOF_TSS2,0,:) = TwrSSSF(2,J,0)*a3 - (   AxRedTSS(2,2,J)* QT(DOF_TSS2) &
                                                     + AxRedTSS(1,2,J)* QT(DOF_TSS1)   )*a2

   CALL    CrossProd( TmpVec1, AngVelEX, PLinVelET(J,DOF_TFA1,0,:) )
   CALL    CrossProd( TmpVec2, AngVelEX, PLinVelET(J,DOF_TSS1,0,:) )
   CALL    CrossProd( TmpVec3, AngVelEX, PLinVelET(J,DOF_TFA2,0,:) )
   CALL    CrossProd( TmpVec4, AngVelEX, PLinVelET(J,DOF_TSS2,0,:) )

   PLinVelET(J,DOF_TFA1,1,:) = TmpVec1           - (   AxRedTFA(1,1,J)*QDT(DOF_TFA1) &
                                                     + AxRedTFA(1,2,J)*QDT(DOF_TFA2)   )*a2
   PLinVelET(J,DOF_TSS1,1,:) = TmpVec2           - (   AxRedTSS(1,1,J)*QDT(DOF_TSS1) &
                                                     + AxRedTSS(1,2,J)*QDT(DOF_TSS2)   )*a2
   PLinVelET(J,DOF_TFA2,1,:) = TmpVec3           - (   AxRedTFA(2,2,J)*QDT(DOF_TFA2) &
                                                     + AxRedTFA(1,2,J)*QDT(DOF_TFA1)   )*a2
   PLinVelET(J,DOF_TSS2,1,:) = TmpVec4           - (   AxRedTSS(2,2,J)*QDT(DOF_TSS2) &
                                                     + AxRedTSS(1,2,J)*QDT(DOF_TSS1)   )*a2

   LinVelXT       = QDT(DOF_TFA1)*PLinVelET(J,DOF_TFA1,0,:) &
                  + QDT(DOF_TSS1)*PLinVelET(J,DOF_TSS1,0,:) &
                  + QDT(DOF_TFA2)*PLinVelET(J,DOF_TFA2,0,:) &
                  + QDT(DOF_TSS2)*PLinVelET(J,DOF_TSS2,0,:)
   LinAccETt(J,:) = QDT(DOF_TFA1)*PLinVelET(J,DOF_TFA1,1,:) &
                  + QDT(DOF_TSS1)*PLinVelET(J,DOF_TSS1,1,:) &
                  + QDT(DOF_TFA2)*PLinVelET(J,DOF_TFA2,1,:) &
                  + QDT(DOF_TSS2)*PLinVelET(J,DOF_TSS2,1,:)

   LinVelET       = LinVelXT + LinVelEZ
   DO I = 1,NPX   ! Loop through all DOFs associated with the angular motion of the platform (body X)

      CALL CrossProd( TmpVec0, PAngVelEX(PX(I),0,:),     rZT(J,:)            )
      CALL CrossProd( TmpVec1, PAngVelEX(PX(I),0,:), EwXXrZT      + LinVelXT )

      PLinVelET(J,PX(I),0,:) = PLinVelET(J,PX(I),0,:) + TmpVec0
      PLinVelET(J,PX(I),1,:) = PLinVelET(J,PX(I),1,:) + TmpVec1

      LinVelET               = LinVelET               + QDT(PX(I))*PLinVelET(J,PX(I),0,:)
      LinAccETt(J,        :) = LinAccETt(J,        :) + QDT(PX(I))*PLinVelET(J,PX(I),1,:)

   ENDDO          ! I - all DOFs associated with the angular motion of the platform (body X)


   ! Calculate the aerodynamic forces and moments per unit length at the
   !   current tower element:
   ! NOTE: FTAero(J,:) = aerodynamic force per unit length acting on tower node J.
   ! NOTE: MFAero(J,:) = aerodynamic moment per unit length acting on tower element F at node J.

   IF ( CompAero )  THEN   ! Calculate the tower element aerodynamic loads using AeroDyn.

      FTAero(J,:) = 0.0    !JASON: ADD TOWER AERODYNAMIC LOAD CALCULATIONS HERE!!!
      MFAero(J,:) = 0.0    !JASON: ADD TOWER AERODYNAMIC LOAD CALCULATIONS HERE!!!

   ELSE                    ! Wind turbine in vacuum, no aerodynamic forces.

      FTAero(J,:) = 0.0
      MFAero(J,:) = 0.0

   ENDIF

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:
   ! Let's compute the tower hydrodynamic loading; that is TwrAM(1:6,1:6) and
   !   TwrFt(1:6).

   CALL TwrLoading ( J, rT      (1), -rT      (3), ( rT      (2) - PtfmRef ), AngPosEF(1), -AngPosEF(3), AngPosEF(2), &
                        LinVelET(1), -LinVelET(3),   LinVelET(2)            , AngVelEF(1), -AngVelEF(3), AngVelEF(2)    )


!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.


   ! Compute the partial hydrodynamic forces and moments per unit length
   !   (including those associated with the QD2T()'s and those that are not) at
   !   the current tower element (point T) / (body F):

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:
!remove6.02a
!remove6.02a   PFTHydro(J,:,:) = 0.0
!remove6.02a   PMFHydro(J,:,:) = 0.0
!remove6.02a   FTHydrot(J,  :) = 0.0
!remove6.02a   MFHydrot(J,  :) = 0.0
   ! NOTE: These forces are named PFTHydro, PMFHydro, FTHydrot, and MFHydrot.
   !       However, the names should not imply that the forces are a result of
   !       hydrodynamic contributions only.  These tower forces contain
   !       contributions from any external load acting on the tower other
   !       than loads transmitted from aerodynamics.  For example, these tower
   !       forces contain contributions from foundation stiffness and damping
   !       [not floating] or mooring line restoring and damping, as well as
   !       hydrostatic and hydrodynamic contributions [offshore].

   PFTHydro(J,:,:) = 0.0
   PMFHydro(J,:,:) = 0.0
   DO I = 1,NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tower

      PFTHydro(J,PTE(I),:) = z1*( - TwrAM(DOF_Sg,DOF_Sg)*PLinVelET(J,PTE(I),0,1) &
                                  + TwrAM(DOF_Sg,DOF_Sw)*PLinVelET(J,PTE(I),0,3) &
                                  - TwrAM(DOF_Sg,DOF_Hv)*PLinVelET(J,PTE(I),0,2) &
                                  - TwrAM(DOF_Sg,DOF_R )*PAngVelEF(J,PTE(I),0,1) &
                                  + TwrAM(DOF_Sg,DOF_P )*PAngVelEF(J,PTE(I),0,3) &
                                  - TwrAM(DOF_Sg,DOF_Y )*PAngVelEF(J,PTE(I),0,2)   ) &
                           - z3*( - TwrAM(DOF_Sw,DOF_Sg)*PLinVelET(J,PTE(I),0,1) &
                                  + TwrAM(DOF_Sw,DOF_Sw)*PLinVelET(J,PTE(I),0,3) &
                                  - TwrAM(DOF_Sw,DOF_Hv)*PLinVelET(J,PTE(I),0,2) &
                                  - TwrAM(DOF_Sw,DOF_R )*PAngVelEF(J,PTE(I),0,1) &
                                  + TwrAM(DOF_Sw,DOF_P )*PAngVelEF(J,PTE(I),0,3) &
                                  - TwrAM(DOF_Sw,DOF_Y )*PAngVelEF(J,PTE(I),0,2)   ) &
                           + z2*( - TwrAM(DOF_Hv,DOF_Sg)*PLinVelET(J,PTE(I),0,1) &
                                  + TwrAM(DOF_Hv,DOF_Sw)*PLinVelET(J,PTE(I),0,3) &
                                  - TwrAM(DOF_Hv,DOF_Hv)*PLinVelET(J,PTE(I),0,2) &
                                  - TwrAM(DOF_Hv,DOF_R )*PAngVelEF(J,PTE(I),0,1) &
                                  + TwrAM(DOF_Hv,DOF_P )*PAngVelEF(J,PTE(I),0,3) &
                                  - TwrAM(DOF_Hv,DOF_Y )*PAngVelEF(J,PTE(I),0,2)   )
      PMFHydro(J,PTE(I),:) = z1*( - TwrAM(DOF_R ,DOF_Sg)*PLinVelET(J,PTE(I),0,1) &
                                  + TwrAM(DOF_R ,DOF_Sw)*PLinVelET(J,PTE(I),0,3) &
                                  - TwrAM(DOF_R ,DOF_Hv)*PLinVelET(J,PTE(I),0,2) &
                                  - TwrAM(DOF_R ,DOF_R )*PAngVelEF(J,PTE(I),0,1) &
                                  + TwrAM(DOF_R ,DOF_P )*PAngVelEF(J,PTE(I),0,3) &
                                  - TwrAM(DOF_R ,DOF_Y )*PAngVelEF(J,PTE(I),0,2)   ) &
                           - z3*( - TwrAM(DOF_P ,DOF_Sg)*PLinVelET(J,PTE(I),0,1) &
                                  + TwrAM(DOF_P ,DOF_Sw)*PLinVelET(J,PTE(I),0,3) &
                                  - TwrAM(DOF_P ,DOF_Hv)*PLinVelET(J,PTE(I),0,2) &
                                  - TwrAM(DOF_P ,DOF_R )*PAngVelEF(J,PTE(I),0,1) &
                                  + TwrAM(DOF_P ,DOF_P )*PAngVelEF(J,PTE(I),0,3) &
                                  - TwrAM(DOF_P ,DOF_Y )*PAngVelEF(J,PTE(I),0,2)   ) &
                           + z2*( - TwrAM(DOF_Y ,DOF_Sg)*PLinVelET(J,PTE(I),0,1) &
                                  + TwrAM(DOF_Y ,DOF_Sw)*PLinVelET(J,PTE(I),0,3) &
                                  - TwrAM(DOF_Y ,DOF_Hv)*PLinVelET(J,PTE(I),0,2) &
                                  - TwrAM(DOF_Y ,DOF_R )*PAngVelEF(J,PTE(I),0,1) &
                                  + TwrAM(DOF_Y ,DOF_P )*PAngVelEF(J,PTE(I),0,3) &
                                  - TwrAM(DOF_Y ,DOF_Y )*PAngVelEF(J,PTE(I),0,2)   )

   ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tower

   FTHydrot(J,:) = z1*( TwrFt(DOF_Sg) - TwrAM(DOF_Sg,DOF_Sg)*LinAccETt(J,1) &
                                      + TwrAM(DOF_Sg,DOF_Sw)*LinAccETt(J,3) &
                                      - TwrAM(DOF_Sg,DOF_Hv)*LinAccETt(J,2) &
                                      - TwrAM(DOF_Sg,DOF_R )*AngAccEFt(J,1) &
                                      + TwrAM(DOF_Sg,DOF_P )*AngAccEFt(J,3) &
                                      - TwrAM(DOF_Sg,DOF_Y )*AngAccEFt(J,2)   ) &
                 - z3*( TwrFt(DOF_Sw) - TwrAM(DOF_Sw,DOF_Sg)*LinAccETt(J,1) &
                                      + TwrAM(DOF_Sw,DOF_Sw)*LinAccETt(J,3) &
                                      - TwrAM(DOF_Sw,DOF_Hv)*LinAccETt(J,2) &
                                      - TwrAM(DOF_Sw,DOF_R )*AngAccEFt(J,1) &
                                      + TwrAM(DOF_Sw,DOF_P )*AngAccEFt(J,3) &
                                      - TwrAM(DOF_Sw,DOF_Y )*AngAccEFt(J,2)   ) &
                 + z2*( TwrFt(DOF_Hv) - TwrAM(DOF_Hv,DOF_Sg)*LinAccETt(J,1) &
                                      + TwrAM(DOF_Hv,DOF_Sw)*LinAccETt(J,3) &
                                      - TwrAM(DOF_Hv,DOF_Hv)*LinAccETt(J,2) &
                                      - TwrAM(DOF_Hv,DOF_R )*AngAccEFt(J,1) &
                                      + TwrAM(DOF_Hv,DOF_P )*AngAccEFt(J,3) &
                                      - TwrAM(DOF_Hv,DOF_Y )*AngAccEFt(J,2)   )
   MFHydrot(J,:) = z1*( TwrFt(DOF_R ) - TwrAM(DOF_R ,DOF_Sg)*LinAccETt(J,1) &
                                      + TwrAM(DOF_R ,DOF_Sw)*LinAccETt(J,3) &
                                      - TwrAM(DOF_R ,DOF_Hv)*LinAccETt(J,2) &
                                      - TwrAM(DOF_R ,DOF_R )*AngAccEFt(J,1) &
                                      + TwrAM(DOF_R ,DOF_P )*AngAccEFt(J,3) &
                                      - TwrAM(DOF_R ,DOF_Y )*AngAccEFt(J,2)   ) &
                 - z3*( TwrFt(DOF_P ) - TwrAM(DOF_P ,DOF_Sg)*LinAccETt(J,1) &
                                      + TwrAM(DOF_P ,DOF_Sw)*LinAccETt(J,3) &
                                      - TwrAM(DOF_P ,DOF_Hv)*LinAccETt(J,2) &
                                      - TwrAM(DOF_P ,DOF_R )*AngAccEFt(J,1) &
                                      + TwrAM(DOF_P ,DOF_P )*AngAccEFt(J,3) &
                                      - TwrAM(DOF_P ,DOF_Y )*AngAccEFt(J,2)   ) &
                 + z2*( TwrFt(DOF_Y ) - TwrAM(DOF_Y ,DOF_Sg)*LinAccETt(J,1) &
                                      + TwrAM(DOF_Y ,DOF_Sw)*LinAccETt(J,3) &
                                      - TwrAM(DOF_Y ,DOF_Hv)*LinAccETt(J,2) &
                                      - TwrAM(DOF_Y ,DOF_R )*AngAccEFt(J,1) &
                                      + TwrAM(DOF_Y ,DOF_P )*AngAccEFt(J,3) &
                                      - TwrAM(DOF_Y ,DOF_Y )*AngAccEFt(J,2)   )
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

   ! Calculate the mass of the current element:

   ElmntMass = MassT(J)*DHNodes(J)   ! Mass of tower element J


   ! Integrate to find the total partial forces and moments (including those
   !   associated with the QD2T()'s and those that are not) at the tower base
   !   (point T(0)):

   DO I = 1,NPTE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tower

      TmpVec1 = PFTHydro(J,PTE(I),:)*DHNodes(J) &
              - ElmntMass*PLinVelET(J,PTE(I),0,:)           ! The portion of PFrcT0Trb associated with tower element J
      CALL CrossProd( TmpVec2, rT0T(J,:), TmpVec1 )         ! The portion of PMomX0Trb associated with tower element J
      TmpVec3 = PMFHydro(J,PTE(I),:)*DHNodes(J)             ! The added moment applied at tower element J

      PFrcT0Trb(PTE(I),:) = PFrcT0Trb(PTE(I),:) + TmpVec1

      PMomX0Trb(PTE(I),:) = PMomX0Trb(PTE(I),:) + TmpVec2 + TmpVec3

   ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the tower

   TmpVec1 = ( FTAero(J,:) + FTHydrot(J,:) )*DHNodes(J) &
           - ElmntMass*( Gravity*z2 + LinAccETt(J,:) )      ! The portion of FrcT0Trbt associated with tower element J
   CALL CrossProd( TmpVec2, rT0T(J,:), TmpVec1 )            ! The portion of MomX0Trbt associated with tower element J
   TmpVec3 = ( MFAero(J,:) + MFHydrot(J,:) )*DHNodes(J)     ! The external moment applied to tower element J

   FrcT0Trbt = FrcT0Trbt + TmpVec1

   MomX0Trbt = MomX0Trbt + TmpVec2 + TmpVec3


   ! Integrate to find the portions of the mass matrix on and below the
   !   diagonal associated with purely tower DOFs (these portions can't
   !   be calculated using partial loads).  Also, integrate to find the
   !   portions of the forcing vector associated with purely tower DOFs
   !   (these portions can't be calculated using partial loads).
   ! NOTE: The vector subscript array, PTTE(), used in the following loops must
   !       be sorted from smallest to largest DOF index in order for the loops
   !       to work to enter values only on and below the diagonal of AugMat():

   DO L = 1,NPTTE    ! Loop through all active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the tower
      DO I = L,NPTTE ! Loop through all active (enabled) tower DOFs greater than or equal to L
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Correct how the undocumented, zero-valued, partial tower hydrodynamic
!jmj   forces and moments per unit length are integrated into the equations of
!jmj   motion:
!remove6.02a         AugMat(PTTE(I),PTTE(L)) = AugMat(PTTE(I),PTTE(L)) + ElmntMass*&
!remove6.02a                                   DOT_PRODUCT( PLinVelET(J,PTTE(I),0,:), &             ! [C(q,t)]T
!remove6.02a                                                PLinVelET(J,PTTE(L),0,:)    )
         AugMat(PTTE(I),PTTE(L)) = AugMat(PTTE(I),PTTE(L))                                                  &
                                 + ElmntMass *DOT_PRODUCT( PLinVelET(J,PTTE(I),0,:), PLinVelET(J,PTTE(L),0,:) ) &  ! [C(q,t)]T + [C(q,t)]HydroT
                                 - DHNodes(J)*DOT_PRODUCT( PLinVelET(J,PTTE(I),0,:), PFTHydro (J,PTTE(L),  :) ) &
                                 - DHNodes(J)*DOT_PRODUCT( PAngVelEF(J,PTTE(I),0,:), PMFHydro (J,PTTE(L),  :) )
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
      ENDDO          ! I - All active (enabled) tower DOFs greater than or equal to L
   ENDDO             ! L - All active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the tower
   DO I = 1,NPTTE    ! Loop through all active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the tower
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Correct how the undocumented, zero-valued, partial tower hydrodynamic
!jmj   forces and moments per unit length are integrated into the equations of
!jmj   motion:
!remove6.02a         AugMat(PTTE(I),   NAUG) = AugMat(PTTE(I),   NAUG)                      &   ! {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT
!remove6.02a                                 + DOT_PRODUCT( PLinVelET(J,PTTE(I),0,:), TmpVec1 ) &   ! NOTE: TmpVec1 is still the portion of FrcT0Trbt associated with tower element J
!remove6.02a                                 + DOT_PRODUCT( PAngVelEF(J,PTTE(I),0,:), TmpVec3 )     !       and TmpVec3 is still the total external moment to tower element J
         AugMat(PTTE(I),   NAUG) = AugMat(PTTE(I),   NAUG)                                                  &  ! {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT
                                 +            DOT_PRODUCT( PLinVelET(J,PTTE(I),0,:), TmpVec1                  ) &  ! NOTE: TmpVec1 is still the portion of FrcT0Trbt associated with tower element J
                                 +            DOT_PRODUCT( PAngVelEF(J,PTTE(I),0,:), TmpVec3                  )    !       and TmpVec3 is still the total external moment to tower element J
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
   ENDDO             ! I - All active (enabled) tower DOFs that contribute to the QD2T-related linear accelerations of the tower

ENDDO ! J - Tower nodes / elements


   ! If the associated DOFs are enabled, add the tower elasticity and damping
   !   forces to the forcing vector (these portions can't be calculated using
   !   partial loads):

IF ( DOF_Flag(DOF_TFA1) )  THEN
   AugMat(    DOF_TFA1,NAUG) = AugMat(DOF_TFA1,NAUG)                             &
                             - KTFA(1,1)* QT(DOF_TFA1) - KTFA(1,2)* QT(DOF_TFA2) &  !
                             - CTFA(1,1)*QDT(DOF_TFA1) - CTFA(1,2)*QDT(DOF_TFA2)
ENDIF
IF ( DOF_Flag(DOF_TSS1) )  THEN
   AugMat(    DOF_TSS1,NAUG) = AugMat(DOF_TSS1,NAUG)                             &
                             - KTSS(1,1)* QT(DOF_TSS1) - KTSS(1,2)* QT(DOF_TSS2) &  ! {-f(qd,q,t)}ElasticT + {-f(qd,q,t)}DampT
                             - CTSS(1,1)*QDT(DOF_TSS1) - CTSS(1,2)*QDT(DOF_TSS2)
ENDIF
IF ( DOF_Flag(DOF_TFA2) )  THEN
   AugMat(    DOF_TFA2,NAUG) = AugMat(DOF_TFA2,NAUG)                             &
                             - KTFA(2,1)* QT(DOF_TFA1) - KTFA(2,2)* QT(DOF_TFA2) &  !
                             - CTFA(2,1)*QDT(DOF_TFA1) - CTFA(2,2)*QDT(DOF_TFA2)
ENDIF
IF ( DOF_Flag(DOF_TSS2) )  THEN
   AugMat(    DOF_TSS2,NAUG) = AugMat(DOF_TSS2,NAUG)                             &
                             - KTSS(2,1)* QT(DOF_TSS1) - KTSS(2,2)* QT(DOF_TSS2) &  !
                             - CTSS(2,1)*QDT(DOF_TSS1) - CTSS(2,2)*QDT(DOF_TSS2)
ENDIF



   ! Let's compute the platform loading; that is PtfmAM(1:6,1:6), and
   !   PtfmFt(1:6):

CALL PtfmLoading


   ! Compute the partial platform forces and moments (including those
   !   associated with the QD2T()'s and those that are not) at the platform
   !   reference (point Z) / (body X).
   ! NOTE: These forces are named PFZHydro, PMXHydro, FZHydrot, and MXHydrot.
   !       However, the names should not imply that the forces are a result of
   !       hydrodynamic contributions only.  These platform forces contain
   !       contributions from any external load acting on the platform other
   !       than loads transmitted from the wind turbine.  For example, these
   !       platform forces contain contributions from foundation stiffness and
   !       damping [not floating] or mooring line restoring and damping
   !       [floating], as well as hydrostatic and hydrodynamic contributions
   !       [offshore].

PFZHydro = 0.0
PMXHydro = 0.0
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Simplify how the partial platform forces and moments associated with the
!jmj   QD2T()'s are calculated:
!remove6.02aIF ( DOF_Flag(DOF_Sg) )  THEN
!remove6.02a   PFZHydro(  DOF_Sg,:) = - PtfmAM(DOF_Sg,DOF_Sg)*PLinVelEZ(DOF_Sg,0,:) &
!remove6.02a                          - PtfmAM(DOF_Sw,DOF_Sg)*PLinVelEZ(DOF_Sw,0,:) &
!remove6.02a                          - PtfmAM(DOF_Hv,DOF_Sg)*PLinVelEZ(DOF_Hv,0,:)
!remove6.02a   PMXHydro(  DOF_Sg,:) = - PtfmAM(DOF_R ,DOF_Sg)*PAngVelEX(DOF_R ,0,:) &
!remove6.02a                          - PtfmAM(DOF_P ,DOF_Sg)*PAngVelEX(DOF_P ,0,:) &
!remove6.02a                          - PtfmAM(DOF_Y ,DOF_Sg)*PAngVelEX(DOF_Y ,0,:)
!remove6.02aENDIF
!remove6.02aIF ( DOF_Flag(DOF_Sw) )  THEN
!remove6.02a   PFZHydro(  DOF_Sw,:) = - PtfmAM(DOF_Sg,DOF_Sw)*PLinVelEZ(DOF_Sg,0,:) &
!remove6.02a                          - PtfmAM(DOF_Sw,DOF_Sw)*PLinVelEZ(DOF_Sw,0,:) &
!remove6.02a                          - PtfmAM(DOF_Hv,DOF_Sw)*PLinVelEZ(DOF_Hv,0,:)
!remove6.02a   PMXHydro(  DOF_Sw,:) = - PtfmAM(DOF_R ,DOF_Sw)*PAngVelEX(DOF_R ,0,:) &
!remove6.02a                          - PtfmAM(DOF_P ,DOF_Sw)*PAngVelEX(DOF_P ,0,:) &
!remove6.02a                          - PtfmAM(DOF_Y ,DOF_Sw)*PAngVelEX(DOF_Y ,0,:)
!remove6.02aENDIF
!remove6.02aIF ( DOF_Flag(DOF_Hv) )  THEN
!remove6.02a   PFZHydro(  DOF_Hv,:) = - PtfmAM(DOF_Sg,DOF_Hv)*PLinVelEZ(DOF_Sg,0,:) &
!remove6.02a                          - PtfmAM(DOF_Sw,DOF_Hv)*PLinVelEZ(DOF_Sw,0,:) &
!remove6.02a                          - PtfmAM(DOF_Hv,DOF_Hv)*PLinVelEZ(DOF_Hv,0,:)
!remove6.02a   PMXHydro(  DOF_Hv,:) = - PtfmAM(DOF_R ,DOF_Hv)*PAngVelEX(DOF_R ,0,:) &
!remove6.02a                          - PtfmAM(DOF_P ,DOF_Hv)*PAngVelEX(DOF_P ,0,:) &
!remove6.02a                          - PtfmAM(DOF_Y ,DOF_Hv)*PAngVelEX(DOF_Y ,0,:)
!remove6.02aENDIF
!remove6.02aIF ( DOF_Flag(DOF_R ) )  THEN
!remove6.02a   PFZHydro(  DOF_R ,:) = - PtfmAM(DOF_Sg,DOF_R )*PLinVelEZ(DOF_Sg,0,:) &
!remove6.02a                          - PtfmAM(DOF_Sw,DOF_R )*PLinVelEZ(DOF_Sw,0,:) &
!remove6.02a                          - PtfmAM(DOF_Hv,DOF_R )*PLinVelEZ(DOF_Hv,0,:)
!remove6.02a   PMXHydro(  DOF_R ,:) = - PtfmAM(DOF_R ,DOF_R )*PAngVelEX(DOF_R ,0,:) &
!remove6.02a                          - PtfmAM(DOF_P ,DOF_R )*PAngVelEX(DOF_P ,0,:) &
!remove6.02a                          - PtfmAM(DOF_Y ,DOF_R )*PAngVelEX(DOF_Y ,0,:)
!remove6.02aENDIF
!remove6.02aIF ( DOF_Flag(DOF_P ) )  THEN
!remove6.02a   PFZHydro(  DOF_P ,:) = - PtfmAM(DOF_Sg,DOF_P )*PLinVelEZ(DOF_Sg,0,:) &
!remove6.02a                          - PtfmAM(DOF_Sw,DOF_P )*PLinVelEZ(DOF_Sw,0,:) &
!remove6.02a                          - PtfmAM(DOF_Hv,DOF_P )*PLinVelEZ(DOF_Hv,0,:)
!remove6.02a   PMXHydro(  DOF_P ,:) = - PtfmAM(DOF_R ,DOF_P )*PAngVelEX(DOF_R ,0,:) &
!remove6.02a                          - PtfmAM(DOF_P ,DOF_P )*PAngVelEX(DOF_P ,0,:) &
!remove6.02a                          - PtfmAM(DOF_Y ,DOF_P )*PAngVelEX(DOF_Y ,0,:)
!remove6.02aENDIF
!remove6.02aIF ( DOF_Flag(DOF_Y ) )  THEN
!remove6.02a   PFZHydro(  DOF_Y ,:) = - PtfmAM(DOF_Sg,DOF_Y )*PLinVelEZ(DOF_Sg,0,:) &
!remove6.02a                          - PtfmAM(DOF_Sw,DOF_Y )*PLinVelEZ(DOF_Sw,0,:) &
!remove6.02a                          - PtfmAM(DOF_Hv,DOF_Y )*PLinVelEZ(DOF_Hv,0,:)
!remove6.02a   PMXHydro(  DOF_Y ,:) = - PtfmAM(DOF_R ,DOF_Y )*PAngVelEX(DOF_R ,0,:) &
!remove6.02a                          - PtfmAM(DOF_P ,DOF_Y )*PAngVelEX(DOF_P ,0,:) &
!remove6.02a                          - PtfmAM(DOF_Y ,DOF_Y )*PAngVelEX(DOF_Y ,0,:)
!remove6.02aENDIF
DO I = 1,NPYE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)

   PFZHydro(PYE(I),:) = - PtfmAM(DOF_Sg,PYE(I))*PLinVelEZ(DOF_Sg,0,:) &
                        - PtfmAM(DOF_Sw,PYE(I))*PLinVelEZ(DOF_Sw,0,:) &
                        - PtfmAM(DOF_Hv,PYE(I))*PLinVelEZ(DOF_Hv,0,:)
   PMXHydro(PYE(I),:) = - PtfmAM(DOF_R ,PYE(I))*PAngVelEX(DOF_R ,0,:) &
                        - PtfmAM(DOF_P ,PYE(I))*PAngVelEX(DOF_P ,0,:) &
                        - PtfmAM(DOF_Y ,PYE(I))*PAngVelEX(DOF_Y ,0,:)

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.

FZHydrot = PtfmFt(DOF_Sg)*PLinVelEZ(DOF_Sg,0,:) &
         + PtfmFt(DOF_Sw)*PLinVelEZ(DOF_Sw,0,:) &
         + PtfmFt(DOF_Hv)*PLinVelEZ(DOF_Hv,0,:)
MXHydrot = PtfmFt(DOF_R )*PAngVelEX(DOF_R ,0,:) &
         + PtfmFt(DOF_P )*PAngVelEX(DOF_P ,0,:) &
         + PtfmFt(DOF_Y )*PAngVelEX(DOF_Y ,0,:)



   ! Define the partial forces and moments (including those associated with
   !   the QD2T()'s and those that are not) at the platform reference (point Z)
   !   / (body X) using the turbine and platform effects:

PFrcZAll = PFrcT0Trb ! Initialize these partial forces and moments
PMomXAll = PMomX0Trb ! using the effects from the wind turbine
DO I = 1,NActvDOF ! Loop through all active (enabled) DOFs

   CALL CrossProd( TmpVec , rZT0, PFrcT0Trb(SrtPS(I),:) )   ! The portion of PMomXAll associated with the PFrcT0Trb

   PMomXAll(SrtPS(I),:) = PMomXAll(SrtPS(I),:) + TmpVec

ENDDO             ! I - All active (enabled) DOFs
DO I = 1,NPYE  ! Loop through all active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)

   TmpVec1 = -PtfmMass*PLinVelEY(PYE(I),0,:)                ! The portion of PFrcZAll associated with the PtfmMass
   CALL CrossProd( TmpVec2, rZY ,               TmpVec1 )   ! The portion of PMomXAll associated with the PtfmMass

   PFrcZAll(PYE(I)  ,:) = PFrcZAll(PYE(I)  ,:) + PFZHydro(PYE(I),:) + TmpVec1

   PMomXAll(PYE(I)  ,:) = PMomXAll(PYE(I)  ,:) + PMXHydro(PYE(I),:) + TmpVec2 &
                        - PtfmRIner*a1*DOT_PRODUCT( a1, PAngVelEX(PYE(I),0,:) )   &
                        - PtfmYIner*a2*DOT_PRODUCT( a2, PAngVelEX(PYE(I),0,:) )   &
                        - PtfmPIner*a3*DOT_PRODUCT( a3, PAngVelEX(PYE(I),0,:) )

ENDDO          ! I - All active (enabled) DOFs that contribute to the QD2T-related linear accelerations of the platform center of mass (point Y)

TmpVec1 = -PtfmMass*( Gravity*z2 + LinAccEYt  )    ! The portion of FrcZAllt associated with the PtfmMass
CALL CrossProd( TmpVec2, rZY      ,   TmpVec1 )    ! The portion of MomXAllt associated with the PtfmMass
CALL CrossProd( TmpVec3, rZT0     , FrcT0Trbt )    ! The portion of MomXAllt associated with the FrcT0Trbt
TmpVec  = PtfmRIner*a1*DOT_PRODUCT( a1, AngVelEX  ) &  ! = ( Platform inertia dyadic ) dot ( angular velocity of platform in the inertia frame )
        + PtfmYIner*a2*DOT_PRODUCT( a2, AngVelEX  ) &
        + PtfmPIner*a3*DOT_PRODUCT( a3, AngVelEX  )
CALL CrossProd( TmpVec4, -AngVelEX,   TmpVec  )    ! = ( -angular velocity of platform in the inertia frame ) cross ( TmpVec )

FrcZAllt = FrcT0Trbt + FZHydrot + TmpVec1

MomXAllt = MomX0Trbt + MXHydrot + TmpVec2 + TmpVec3 + TmpVec4



   ! Compute the moments from teeter springs and dampers, rotor-furl springs
   !   and dampers, tail-furl springs and dampers, and the generator and
   !   high-speed shaft brake torque:

CALL Teeter  ( TeetAng     , TeetAngVel   , TeetMom ) ! Compute moment from teeter     springs and dampers, TeetMom; NOTE: TeetMom will be zero for a 3-blader since TeetAng = TeetAngVel = 0
CALL RFurling( QT(DOF_RFrl), QDT(DOF_RFrl), RFrlMom ) ! Compute moment from rotor-furl springs and dampers, RFrlMom
CALL TFurling( QT(DOF_TFrl), QDT(DOF_TFrl), TFrlMom ) ! Compute moment from tail-furl  springs and dampers, TFrlMom
CALL DrvTrTrq(               QDT(DOF_GeAz), GBoxTrq ) ! Compute generator and HSS-brake torque on LSS-side, GBoxTrq


   ! Now that all of the partial loads have been found, lets fill in the
   !   portions of the mass matrix on and below the diagonal that may be
   !   calculated with the help of the partial loads.  Also, lets fill in the
   !   portions of the forcing vector that may be calculated with the help of
   !   the partial loads.  Also let's add in additional terms to the forcing
   !   function that can't be added with the help of the partial loads:
   ! NOTE: The vector subscript array, SrtPS(), used in the following loops
   !       must be sorted from smallest to largest DOF index in order for the
   !       loops to work to enter values only on and below the diagonal of
   !       AugMat():

IF ( DOF_Flag (DOF_Sg  ) )  THEN
   DO I = Diag(DOF_Sg  ),NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Correct how the undocumented, zero-valued, partial tower hydrodynamic
!jmj   forces and moments per unit length are integrated into the equations of
!jmj   motion:
!remove6.02a      AugMat(SrtPS(I),DOF_Sg  ) = -DOT_PRODUCT( PLinVelEZ(DOF_Sg  ,0,:), PFrcZAll (SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]N + [C(q,t)]R + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
      AugMat(SrtPS(I),DOF_Sg  ) = -1.*DOT_PRODUCT( PLinVelEZ(DOF_Sg  ,0,:), PFrcZAll (SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Correct how the undocumented, zero-valued, partial tower hydrodynamic
!jmj   forces and moments per unit length are integrated into the equations of
!jmj   motion:
!remove6.02a      AugMat(DOF_Sg  ,    NAUG) =  DOT_PRODUCT( PLinVelEZ(DOF_Sg  ,0,:), FrcZAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}N + {-f(qd,q,t)}R + {-f(qd,q,t)}H + {-f(qd,q,t)}B + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}AeroA
      AugMat(DOF_Sg  ,    NAUG) =  DOT_PRODUCT( PLinVelEZ(DOF_Sg  ,0,:), FrcZAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}R + {-f(qd,q,t)}H + {-f(qd,q,t)}B + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}AeroA
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
ENDIF

IF ( DOF_Flag (DOF_Sw  ) )  THEN
   DO I = Diag(DOF_Sw  ),NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Correct how the undocumented, zero-valued, partial tower hydrodynamic
!jmj   forces and moments per unit length are integrated into the equations of
!jmj   motion:
!remove6.02a      AugMat(SrtPS(I),DOF_Sw  ) = -DOT_PRODUCT( PLinVelEZ(DOF_Sw  ,0,:), PFrcZAll (SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]N + [C(q,t)]R + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
      AugMat(SrtPS(I),DOF_Sw  ) = -1*DOT_PRODUCT( PLinVelEZ(DOF_Sw  ,0,:), PFrcZAll (SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Correct how the undocumented, zero-valued, partial tower hydrodynamic
!jmj   forces and moments per unit length are integrated into the equations of
!jmj   motion:
!remove6.02a      AugMat(DOF_Sw  ,    NAUG) =  DOT_PRODUCT( PLinVelEZ(DOF_Sw  ,0,:), FrcZAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}N + {-f(qd,q,t)}R + {-f(qd,q,t)}H + {-f(qd,q,t)}B + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}AeroA
      AugMat(DOF_Sw  ,    NAUG) =  DOT_PRODUCT( PLinVelEZ(DOF_Sw  ,0,:), FrcZAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}R + {-f(qd,q,t)}H + {-f(qd,q,t)}B + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}AeroA
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
ENDIF

IF ( DOF_Flag (DOF_Hv  ) )  THEN
   DO I = Diag(DOF_Hv  ),NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Correct how the undocumented, zero-valued, partial tower hydrodynamic
!jmj   forces and moments per unit length are integrated into the equations of
!jmj   motion:
!remove6.02a      AugMat(SrtPS(I),DOF_Hv  ) = -DOT_PRODUCT( PLinVelEZ(DOF_Hv  ,0,:), PFrcZAll (SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]N + [C(q,t)]R + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
      AugMat(SrtPS(I),DOF_Hv  ) = -1.*DOT_PRODUCT( PLinVelEZ(DOF_Hv  ,0,:), PFrcZAll (SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Correct how the undocumented, zero-valued, partial tower hydrodynamic
!jmj   forces and moments per unit length are integrated into the equations of
!jmj   motion:
!remove6.02a      AugMat(DOF_Hv  ,    NAUG) =  DOT_PRODUCT( PLinVelEZ(DOF_Hv  ,0,:), FrcZAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
      AugMat(DOF_Hv  ,    NAUG) =  DOT_PRODUCT( PLinVelEZ(DOF_Hv  ,0,:), FrcZAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
ENDIF

IF ( DOF_Flag (DOF_R   ) )  THEN
   DO I = Diag(DOF_R   ),NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal

!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Correct how the undocumented, zero-valued, partial tower hydrodynamic
!jmj   forces and moments per unit length are integrated into the equations of
!jmj   motion:
!remove6.02a      AugMat(SrtPS(I),DOF_R   ) = -DOT_PRODUCT( PAngVelEX(DOF_R   ,0,:), PMomXAll (SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
      AugMat(SrtPS(I),DOF_R   ) = -1.*DOT_PRODUCT( PAngVelEX(DOF_R   ,0,:), PMomXAll (SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Correct how the undocumented, zero-valued, partial tower hydrodynamic
!jmj   forces and moments per unit length are integrated into the equations of
!jmj   motion:
!remove6.02a      AugMat(DOF_R   ,    NAUG) =  DOT_PRODUCT( PAngVelEX(DOF_R   ,0,:), MomXAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
      AugMat(DOF_R   ,    NAUG) =  DOT_PRODUCT( PAngVelEX(DOF_R   ,0,:), MomXAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
ENDIF

IF ( DOF_Flag (DOF_P   ) )  THEN
   DO I = Diag(DOF_P   ),NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Correct how the undocumented, zero-valued, partial tower hydrodynamic
!jmj   forces and moments per unit length are integrated into the equations of
!jmj   motion:
!remove6.02a      AugMat(SrtPS(I),DOF_P   ) = -DOT_PRODUCT( PAngVelEX(DOF_P   ,0,:), PMomXAll (SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
      AugMat(SrtPS(I),DOF_P   ) = -1.*DOT_PRODUCT( PAngVelEX(DOF_P   ,0,:), PMomXAll (SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Correct how the undocumented, zero-valued, partial tower hydrodynamic
!jmj   forces and moments per unit length are integrated into the equations of
!jmj   motion:
!remove6.02a      AugMat(DOF_P   ,    NAUG) =  DOT_PRODUCT( PAngVelEX(DOF_P   ,0,:), MomXAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
      AugMat(DOF_P   ,    NAUG) =  DOT_PRODUCT( PAngVelEX(DOF_P   ,0,:), MomXAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.ENDIF
END IF

IF ( DOF_Flag (DOF_Y   ) )  THEN
   DO I = Diag(DOF_Y   ),NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Correct how the undocumented, zero-valued, partial tower hydrodynamic
!jmj   forces and moments per unit length are integrated into the equations of
!jmj   motion:
!remove6.02a      AugMat(SrtPS(I),DOF_Y   ) = -DOT_PRODUCT( PAngVelEX(DOF_Y   ,0,:), PMomXAll (SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
      AugMat(SrtPS(I),DOF_Y   ) = -1.*DOT_PRODUCT( PAngVelEX(DOF_Y   ,0,:), PMomXAll (SrtPS(I),:) )    ! [C(q,t)]X + [C(q,t)]HydroX + [C(q,t)]T + [C(q,t)]HydroT + [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Correct how the undocumented, zero-valued, partial tower hydrodynamic
!jmj   forces and moments per unit length are integrated into the equations of
!jmj   motion:
!remove6.02a      AugMat(DOF_Y   ,    NAUG) =  DotProd( PAngVelEX(DOF_Y   ,0,:), MomXAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
      AugMat(DOF_Y   ,    NAUG) =     DOT_PRODUCT( PAngVelEX(DOF_Y   ,0,:), MomXAllt              )    ! {-f(qd,q,t)}X + {-f(qd,q,t)}GravX + {-f(qd,q,t)}HydroX + {-f(qd,q,t)}T + {-f(qd,q,t)}GravT + {-f(qd,q,t)}AeroT + {-f(qd,q,t)}HydroT + {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
ENDIF

IF ( DOF_Flag (DOF_TFA1) )  THEN
   DO I = Diag(DOF_TFA1),NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(SrtPS(I),DOF_TFA1) = AugMat(SrtPS(I),DOF_TFA1)                                  &
                                -  DOT_PRODUCT( PLinVelEO(DOF_TFA1,0,:), PFrcONcRt(SrtPS(I),:) ) &  ! [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
                                -  DOT_PRODUCT( PAngVelEB(DOF_TFA1,0,:), PMomBNcRt(SrtPS(I),:) )
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_TFA1,    NAUG) = AugMat(DOF_TFA1,    NAUG)                                  &
                                +  DOT_PRODUCT( PLinVelEO(DOF_TFA1,0,:), FrcONcRtt             ) &  ! {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                +  DOT_PRODUCT( PAngVelEB(DOF_TFA1,0,:), MomBNcRtt             )
ENDIF

IF ( DOF_Flag (DOF_TSS1) )  THEN
   DO I = Diag(DOF_TSS1),NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(SrtPS(I),DOF_TSS1) = AugMat(SrtPS(I),DOF_TSS1)                                  &
                                -  DOT_PRODUCT( PLinVelEO(DOF_TSS1,0,:), PFrcONcRt(SrtPS(I),:) ) &  ! [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
                                -  DOT_PRODUCT( PAngVelEB(DOF_TSS1,0,:), PMomBNcRt(SrtPS(I),:) )
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_TSS1,    NAUG) = AugMat(DOF_TSS1,    NAUG)                                  &
                                +  DOT_PRODUCT( PLinVelEO(DOF_TSS1,0,:), FrcONcRtt             ) &  ! {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                +  DOT_PRODUCT( PAngVelEB(DOF_TSS1,0,:), MomBNcRtt             )
ENDIF

IF ( DOF_Flag (DOF_TFA2) )  THEN
   DO I = Diag(DOF_TFA2),NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(SrtPS(I),DOF_TFA2) = AugMat(SrtPS(I),DOF_TFA2)                                  &
                                -  DOT_PRODUCT( PLinVelEO(DOF_TFA2,0,:), PFrcONcRt(SrtPS(I),:) ) &  ! [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
                                -  DOT_PRODUCT( PAngVelEB(DOF_TFA2,0,:), PMomBNcRt(SrtPS(I),:) )
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_TFA2,    NAUG) = AugMat(DOF_TFA2,    NAUG)                                  &
                                +  DOT_PRODUCT( PLinVelEO(DOF_TFA2,0,:), FrcONcRtt             ) &  ! {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                +  DOT_PRODUCT( PAngVelEB(DOF_TFA2,0,:), MomBNcRtt             )
ENDIF

IF ( DOF_Flag (DOF_TSS2) )  THEN
   DO I = Diag(DOF_TSS2),NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(SrtPS(I),DOF_TSS2) = AugMat(SrtPS(I),DOF_TSS2)                                  &
                                -  DOT_PRODUCT( PLinVelEO(DOF_TSS2,0,:), PFrcONcRt(SrtPS(I),:) ) &  ! [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
                                -  DOT_PRODUCT( PAngVelEB(DOF_TSS2,0,:), PMomBNcRt(SrtPS(I),:) )
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_TSS2,    NAUG) = AugMat(DOF_TSS2,    NAUG)                                  &
                                +  DOT_PRODUCT( PLinVelEO(DOF_TSS2,0,:), FrcONcRtt             ) &  ! {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                +  DOT_PRODUCT( PAngVelEB(DOF_TSS2,0,:), MomBNcRtt             )
ENDIF

IF ( DOF_Flag (DOF_Yaw ) )  THEN
   DO I = Diag(DOF_Yaw ),NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(SrtPS(I),DOF_Yaw ) = -DOT_PRODUCT( PAngVelEN(DOF_Yaw ,0,:), PMomBNcRt(SrtPS(I),:) )    ! [C(q,t)]N + [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B + [C(q,t)]A
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_Yaw ,    NAUG) =  DOT_PRODUCT( PAngVelEN(DOF_Yaw ,0,:), MomBNcRtt             ) &  ! {-f(qd,q,t)}N + {-f(qd,q,t)}GravN + {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB + {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                -  YawSpr *( QT (DOF_Yaw) - YawNeut     )                    &  ! + {-f(qd,q,t)}SpringYaw
                                -  YawDamp*( QDT(DOF_Yaw) - YawRateNeut )                       ! + {-f(qd,q,t)}DampYaw; NOTE: The neutral yaw rate, YawRateNeut, defaults to zero.  It is only used for yaw control.
ENDIF

IF ( DOF_Flag (DOF_RFrl) )  THEN
   DO I = Diag(DOF_RFrl),NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(SrtPS(I),DOF_RFrl) = -DOT_PRODUCT( PAngVelER(DOF_RFrl,0,:), PMomNGnRt(SrtPS(I),:) )    ! [C(q,t)]R + [C(q,t)]G + [C(q,t)]H + [C(q,t)]B
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_RFrl,    NAUG) =  DOT_PRODUCT( PAngVelER(DOF_RFrl,0,:), MomNGnRtt             ) &  ! {-f(qd,q,t)}R + {-f(qd,q,t)}GravR + {-f(qd,q,t)}G + {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                +  RFrlMom                                                      ! + {-f(qd,q,t)}SpringRF + {-f(qd,q,t)}DampRF
ENDIF

!bjj start of proposed change: BUG FIX
!this must be defined for use later, regardless of DOF_Flag (DOF_GeAz)
TmpVec = GenIner*c1*DOT_PRODUCT( c1, PAngVelEG(DOF_GeAz,0,:) )  ! = ( generator inertia dyadic ) Dot ( partial angular velocity of G in E for DOF_GeAz )
!bjj end of proposed change

IF ( DOF_Flag (DOF_GeAz) )  THEN
   DO I = Diag(DOF_GeAz),NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(SrtPS(I),DOF_GeAz) = -DOT_PRODUCT( PAngVelEL(DOF_GeAz,0,:), PMomLPRot(SrtPS(I),:) )    ! [C(q,t)]H + [C(q,t)]B
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_GeAz,    NAUG) =  DOT_PRODUCT( PAngVelEL(DOF_GeAz,0,:), MomLPRott             ) &  ! {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                -  GBoxTrq                                                      ! + {-f(qd,q,t)}Gen + {-f(qd,q,t)}Brake


   ! The previous loop (DO I = Diag(DOF_GeAz),NActvDOF) misses the
   !   generator inertia-contribution to the mass matrix and forcing function.
   !   Thus, add these in as well:

!bjj start of proposed change: BUG FIX
!rm      TmpVec = GenIner*c1*DOT_PRODUCT( c1, PAngVelEG(DOF_GeAz,0,:) )  ! = ( generator inertia dyadic ) Dot ( partial angular velocity of G in E for DOF_GeAz )
!bjj end of proposed change

      AugMat(DOF_GeAz,DOF_GeAz) = AugMat(DOF_GeAz,DOF_GeAz)                                  &
                                +  DOT_PRODUCT( PAngVelEG(DOF_GeAz,0,:), TmpVec                )    ! [C(q,t)]G
      AugMat(DOF_GeAz,    NAUG) = AugMat(DOF_GeAz,    NAUG)                                  &
                                -  DOT_PRODUCT( AngAccEGt              , TmpVec                )    ! {-f(qd,q,t)}G


ENDIF

IF ( DOF_Flag (DOF_DrTr) )  THEN
   DO I = Diag(DOF_DrTr),NActvDOF   ! Loop through all active (enabled) DOFs on or below the diagonal
      AugMat(SrtPS(I),DOF_DrTr) = -DOT_PRODUCT( PAngVelEL(DOF_DrTr,0,:), PMomLPRot(SrtPS(I),:) )    ! [C(q,t)]H + [C(q,t)]B
   ENDDO                            ! I - All active (enabled) DOFs on or below the diagonal
      AugMat(DOF_DrTr,    NAUG) =  DOT_PRODUCT( PAngVelEL(DOF_DrTr,0,:), MomLPRott             ) &  ! {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                -  DTTorSpr*QT (DOF_DrTr)                                    &  ! + {-f(qd,q,t)}ElasticDrive
                                -  DTTorDmp*QDT(DOF_DrTr)                                       ! + {-f(qd,q,t)}DampDrive
ENDIF

IF ( DOF_Flag (DOF_TFrl) )  THEN
   ! The tail-furl DOF does not affect any DOF index larger than DOF_TFrl.  Therefore, there is no need to perform the loop: DO I = Diag(DOF_TFrl),NActvDOF
      AugMat(DOF_TFrl,DOF_TFrl) = -DOT_PRODUCT( PAngVelEA(DOF_TFrl,0,:), PMomNTail(DOF_TFrl,:) )    ! [C(q,t)]A
      AugMat(DOF_TFrl,    NAUG) =  DOT_PRODUCT( PAngVelEA(DOF_TFrl,0,:), MomNTailt             ) &  ! {-f(qd,q,t)}A + {-f(qd,q,t)}GravA + {-f(qd,q,t)}AeroA
                                +  TFrlMom                                                      ! + {-f(qd,q,t)}SpringTF + {-f(qd,q,t)}DampTF
ENDIF

IF ( ( NumBl == 2 ) .AND. ( DOF_Flag(DOF_Teet) ) )  THEN
   ! The teeter DOF does not affect any DOF index larger than DOF_Teet.  Therefore, there is no need to perform the loop: DO I = Diag(DOF_Teet),NActvDOF
      AugMat(DOF_Teet,DOF_Teet) = -DOT_PRODUCT( PAngVelEH(DOF_Teet,0,:), PMomLPRot(DOF_Teet,:) )    ! [C(q,t)]H + [C(q,t)]B
      AugMat(DOF_Teet,    NAUG) =  DOT_PRODUCT( PAngVelEH(DOF_Teet,0,:), MomLPRott             ) &  ! {-f(qd,q,t)}H + {-f(qd,q,t)}GravH + {-f(qd,q,t)}B + {-f(qd,q,t)}GravB + {-f(qd,q,t)}AeroB
                                +  TeetMom                                                      ! + {-f(qd,q,t)}SpringTeet + {-f(qd,q,t)}DampTeet
ENDIF



   ! So far, we have only filled in the portions of the mass matrix on and
   !   below the diagonal.  Since the mass matrix is symmetric up to this
   !   point, let's fill in the portion above the diagonal by mirroring the
   !   values from below:
   ! NOTE: The vector subscript array, SrtPS(), used in the following loops
   !       must be sorted from smallest to largest DOF index in order for the
   !       loops to work to enter values only on and below the diagonal of
   !       AugMat():

DO L = 2,NActvDOF ! Loop through all active (enabled) DOFs above the diagonal (columns)
   DO I = 1,L-1   ! Loop through all active (enabled) DOFs above the diagonal (rows)
      AugMat(SrtPS(I),SrtPS(L)) = AugMat(SrtPS(L),SrtPS(I))
   ENDDO          ! I - All active (enabled) DOFs above the diagonal (rows)
ENDDO             ! L - All active (enabled) DOFs above the diagonal (columns)



   ! Let's add the gearbox friction terms to the mass matrix and forcing
   !   function.  These only effect the equation for the generator azimuth
   !   DOF.
   ! NOTE: the MASS MATRIX WILL NO LONGER BE SYMMETRIC after adding these
   !       terms, unless the gearbox efficiency, GBoxEff, was set to 100%:

GBoxEffFac  = GBoxEff**SgnPrvLSTQ      ! = GBoxEff if SgnPrvLSTQ = 1 OR 1/GBoxEff if SgnPrvLSTQ = -1
GBoxEffFac2 = ( 1.0/GBoxEffFac - 1.0 ) ! = ( 1 / GBoxEff^SgnPrvLSTQ - 1 )

DO I = 1,NActvDOF ! Loop through all active (enabled) DOFs

   AugMat(DOF_GeAz,SrtPS(I)) = AugMat(DOF_GeAz,SrtPS(I)) &                                            ! NOTE: TmpVec is still = ( generator inertia dyadic ) Dot ( partial angular velocity of G in E for DOF_GeAz ) in the following equation
                             + GBoxEffFac2*  DOT_PRODUCT( PAngVelEG(SrtPS(I),0,:), TmpVec )               ! [C(q,t)]GBFric

ENDDO             ! I - All active (enabled) DOFs

AugMat(   DOF_GeAz,    NAUG) = AugMat(DOF_GeAz,    NAUG) &                                            ! NOTE: TmpVec is still = ( generator inertia dyadic ) Dot ( partial angular velocity of G in E for DOF_GeAz ) in the following equation
                             - GBoxEffFac2*( DOT_PRODUCT( AngAccEGt              , TmpVec ) + GBoxTrq )   ! {-f(qd,q,t)}GBFric



   ! Store the row of coefficients associated with the generator azimuth DOF
   !   for future use:

OgnlGeAzRo = AugMat(DOF_GeAz,:)



   ! Invert the matrix to solve for the accelerations.  The accelerations are
   !   returned by Gauss() in the first NActvDOF elements of the solution
   !   vector, SolnVec().  These are transfered to the proper index locations
   !   of the acceleration vector QD2T() using the vector subscript array
   !   SrtPS(), after Gauss() has been called:
   ! NOTE: QD2T( SrtPS(1:NActvDOF) ) cannot be sent directly because arrays
   !   sections with vector subscripts must not be used in INTENT(OUT)
   !   arguments.

!do i=1,ndof
!   write(*,'("GaussInp: ",i2,1x,700(G15.7,1X))') i, AugMat( i, : )
!end do

CALL Gauss( AugMat( SrtPS(1:NActvDOF), SrtPSNAUG(1:(NActvDOF+1)) ), NActvDOF, SolnVec )


QD2T = 0.0
DO I = 1,NActvDOF ! Loop through all active (enabled) DOFs
   QD2T(SrtPS(I)) = SolnVec(I)
ENDDO             ! I - All active (enabled) DOFs



   ! Lets calculate the sign (+/-1) of the low-speed shaft torque for
   !   this time step and store it in SgnPrvLSTQ.  This will be used
   !   during the next call to RtHS.  MomLPRot is the moment on the
   !   low-speed shaft at the teeter pin caused by the rotor.

MomLPRot = MomLPRott ! Initialize MomLPRot using MomLPRott
DO I = 1,NActvDOF ! Loop through all active (enabled) DOFs

   MomLPRot = MomLPRot + PMomLPRot(SrtPS(I),:)*QD2T(SrtPS(I))  ! Add the moments associated with the accelerations of the DOFs

ENDDO             ! I - All active (enabled) DOFs

   ! MomLProt has now been found.  Now dot this with e1 to get the
   !   low-speed shaft torque and take the SIGN of the result:

SgnPrvLSTQ = NINT( SIGN( 1.0, DOT_PRODUCT( MomLPRot, e1 ) ) )



   ! If we are linearizing a model and DOFs were enabled or disabled within the
   !   user-defined routines called from RtHS(), abort:

IF ( AnalMode == 2 )  THEN ! .TRUE. when we are in the process of linearizing the FAST model

   DO I = 1,NDOF     ! Loop through all DOFs
      IF ( DOF_Flag(I) .NEQV. DOF_FlagInit(I) )  &
         CALL ProgAbort ( ' FAST can''t linearize a model when DOFs are being switched on-or-off from within user-defined'// &
                      ' routines.  Make sure no user-defined routines change the value of DOF_Flag().'                     )
   ENDDO             ! I - All DOFs

ENDIF



RETURN



CONTAINS



!=======================================================================
   FUNCTION TBFract( ZTTmp, BrakStrt, BrakEnd )


      ! A math S-function for the fraction of tip brake drag b/n normal and
      !   fully deployed operation.


!bjj rm NWTC_Library:    USE                             Precision


   IMPLICIT                        NONE


      ! Passed Variables:

   REAL(ReKi), INTENT(IN )      :: BrakEnd                                         ! Time at which brakes are fully deployed
   REAL(ReKi), INTENT(IN )      :: BrakStrt                                        ! Time at which brakes are first deployed
   REAL(ReKi)                   :: TBFract                                         ! This function.
   REAL(ReKi), INTENT(IN )      :: ZTTmp                                           ! Current time


      ! Local Variables.

   REAL(ReKi)                   :: TmpVar                                          ! A temporary variable



   IF ( ZTTmp <= BrakStrt )  THEN

      TBFract = 0.0

   ELSEIF ( ZTTmp < BrakEnd )  THEN

      TmpVar  = ( ( ZTTmp - BrakStrt )/( BrakStrt - BrakEnd ) )**2
      TBFract = TmpVar*( 2.0 - TmpVar )

   ELSE

      TBFract = 1.0

   ENDIF



   RETURN
   END FUNCTION TBFract
!=======================================================================



END SUBROUTINE RtHS
!=======================================================================
SUBROUTINE SetCoordSy


   ! This routine is used to define the internal coordinate systems for
   !   this particular time step.


USE                             Blades
USE                             Constants
USE                             CoordSys
USE                             DOFs
!bjj rm NWTC_Library: USE                             Precision
USE                             RtHndSid
USE                             Tower
USE                             TurbConf
USE                             TurbCont


IMPLICIT                        NONE


   ! Local variables.

REAL(ReKi)                   :: CAzimuth                                        ! COS( rotor azimuth angle ).
REAL(ReKi)                   :: CgRotAng                                        ! COS( gRotAng ).
REAL(ReKi)                   :: CNacYaw                                         ! COS( nacelle yaw angle ).
REAL(ReKi)                   :: CosPitch                                        ! COS( the current pitch angle ).
REAL(ReKi)                   :: CPitPTwstA                                      ! COS( BlPitch(K) + AeroTwst(J) ) found using the sum of angles formulae of cosine.
REAL(ReKi)                   :: CPitPTwstS                                      ! COS( BlPitch(K) + ThetaS(K,J) ) found using the sum of angles formulae of cosine.
REAL(ReKi)                   :: CRotFurl                                        ! COS( rotor-furl angle ).
REAL(ReKi)                   :: CTailFurl                                       ! COS( tail-furl angle ).
REAL(ReKi)                   :: CTeetAng                                        ! COS( TeetAng ).
REAL(ReKi)                   :: g1Prime   (3)                                   ! = g1.
REAL(ReKi)                   :: g2Prime   (3)                                   ! completes the right-handed gPrime-vector triad
REAL(ReKi)                   :: g3Prime   (3)                                   ! = g3 rotated about g1 so that parallel to the pitching axis of blade K (i.e., the current blade in the blade loop).
REAL(ReKi)                   :: gRotAng                                         ! Angle of rotation about g1 to get from the g to the gPrime system.
REAL(ReKi)                   :: Lj1       (3)                                   ! vector / direction Lj1 at node J for blade K.
REAL(ReKi)                   :: Lj2       (3)                                   ! vector / direction Lj2 at node J for blade K.
REAL(ReKi)                   :: Lj3       (3)                                   ! vector / direction Lj3 at node J for blade K.
REAL(ReKi)                   :: SAzimuth                                        ! SIN( rotor azimuth angle ).
REAL(ReKi)                   :: SgRotAng                                        ! SIN( gRotAng ).
REAL(ReKi)                   :: SinPitch                                        ! SIN( the current pitch angle ).
REAL(ReKi)                   :: SNacYaw                                         ! SIN( nacelle yaw angle ).
REAL(ReKi)                   :: SPitPTwstA                                      ! SIN( BlPitch(K) + AeroTwst(J) ) found using the sum of angles formulae of sine.
REAL(ReKi)                   :: SPitPTwstS                                      ! SIN( BlPitch(K) + ThetaS(K,J) ) found using the sum of angles formulae of sine.
REAL(ReKi)                   :: SRotFurl                                        ! SIN( rotor-furl angle ).
REAL(ReKi)                   :: STailFurl                                       ! SIN( tail-furl angle ).
REAL(ReKi)                   :: STeetAng                                        ! SIN( TeetAng ).
REAL(ReKi)                   :: ThetaFA                                         ! Tower fore-aft tilt deflection angle.
REAL(ReKi)                   :: ThetaIP                                         ! Blade in-plane deflection angle at node J for blade K.
REAL(ReKi)                   :: ThetaLxb                                        ! Blade deflection angle about the Lxb (n1) -axis at node J for blade K.
REAL(ReKi)                   :: ThetaLyb                                        ! Blade deflection angle about the Lyb (n2) -axis at node J for blade K.
REAL(ReKi)                   :: ThetaOoP                                        ! Blade out-of-plane deflection angle at node J for blade K.
REAL(ReKi)                   :: ThetaSS                                         ! Tower side-to-side tilt deflection angle.
REAL(ReKi)                   :: TransMat  (3,3)                                 ! The resulting transformation matrix due to three orthogonal rotations, (-).

INTEGER(4)                   :: J                                               ! Loops through nodes / elements.
INTEGER(4)                   :: K                                               ! Loops through blades.



   ! Inertial frame coordinate system:

z1 = (/ 1.0, 0.0, 0.0 /)   ! Vector / direction z1 (=  xi from the IEC coord. system).
z2 = (/ 0.0, 1.0, 0.0 /)   ! Vector / direction z2 (=  zi from the IEC coord. system).
z3 = (/ 0.0, 0.0, 1.0 /)   ! Vector / direction z3 (= -yi from the IEC coord. system).


   ! Tower base / platform coordinate system:

CALL SmllRotTrans( 'platform displacement', QT(DOF_R), QT(DOF_Y), -QT(DOF_P), TransMat )  ! Get the transformation matrix, TransMat, from inertial frame to tower base / platform coordinate systems.

a1 = TransMat(1,1)*z1 + TransMat(1,2)*z2 + TransMat(1,3)*z3 ! Vector / direction a1 (=  xt from the IEC coord. system).
a2 = TransMat(2,1)*z1 + TransMat(2,2)*z2 + TransMat(2,3)*z3 ! Vector / direction a2 (=  zt from the IEC coord. system).
a3 = TransMat(3,1)*z1 + TransMat(3,2)*z2 + TransMat(3,3)*z3 ! Vector / direction a3 (= -yt from the IEC coord. system).


DO J = 1,TwrNodes ! Loop through the tower nodes / elements


   ! Tower element-fixed coordinate system:

   ThetaFA = -TwrFASF(1,J       ,1)*QT(DOF_TFA1) - TwrFASF(2,J       ,1)*QT(DOF_TFA2)
   ThetaSS =  TwrSSSF(1,J       ,1)*QT(DOF_TSS1) + TwrSSSF(2,J       ,1)*QT(DOF_TSS2)

   CALL SmllRotTrans( 'tower deflection', ThetaSS, 0.0, ThetaFA, TransMat )   ! Get the transformation matrix, TransMat, from tower-base to tower element-fixed coordinate systems.

   t1(J,:) = TransMat(1,1)*a1 + TransMat(1,2)*a2 + TransMat(1,3)*a3  ! Vector / direction t1 for tower node J (=  Lxt from the IEC coord. system).
   t2(J,:) = TransMat(2,1)*a1 + TransMat(2,2)*a2 + TransMat(2,3)*a3  ! Vector / direction t2 for tower node J (=  Lzt from the IEC coord. system).
   t3(J,:) = TransMat(3,1)*a1 + TransMat(3,2)*a2 + TransMat(3,3)*a3  ! Vector / direction t3 for tower node J (= -Lyt from the IEC coord. system).


ENDDO ! J - Tower nodes / elements


   ! Tower-top / base plate coordinate system:

ThetaFA    = -TwrFASF(1,TTopNode,1)*QT(DOF_TFA1) - TwrFASF(2,TTopNode,1)*QT(DOF_TFA2)
ThetaSS    =  TwrSSSF(1,TTopNode,1)*QT(DOF_TSS1) + TwrSSSF(2,TTopNode,1)*QT(DOF_TSS2)

CALL SmllRotTrans( 'tower deflection', ThetaSS, 0.0, ThetaFA, TransMat )   ! Get the transformation matrix, TransMat, from tower-base to tower-top/base-plate coordinate systems.

b1 = TransMat(1,1)*a1 + TransMat(1,2)*a2 + TransMat(1,3)*a3 ! Vector / direction b1 (=  xp from the IEC coord. system).
b2 = TransMat(2,1)*a1 + TransMat(2,2)*a2 + TransMat(2,3)*a3 ! Vector / direction b2 (=  zp from the IEC coord. system).
b3 = TransMat(3,1)*a1 + TransMat(3,2)*a2 + TransMat(3,3)*a3 ! Vector / direction b3 (= -yp from the IEC coord. system).


   ! Nacelle / yaw coordinate system:

CNacYaw  = COS( QT(DOF_Yaw ) )
SNacYaw  = SIN( QT(DOF_Yaw ) )

d1 = CNacYaw*b1 - SNacYaw*b3     ! Vector / direction d1 (=  xn from the IEC coord. system).
d2 = b2                          ! Vector / direction d2 (=  zn from the IEC coord. system).
d3 = SNacYaw*b1 + CNacYaw*b3     ! Vector / direction d3 (= -yn from the IEC coord. system).


   ! Rotor-furl coordinate system:

CRotFurl = COS( QT(DOF_RFrl) )
SRotFurl = SIN( QT(DOF_RFrl) )

rf1 = ( ( 1.0 - CRFrlSkw2*CRFrlTlt2 )*CRotFurl + CRFrlSkw2*CRFrlTlt2          )*d1 &
    + ( CRFrlSkew*CSRFrlTlt*( 1.0 - CRotFurl ) - SRFrlSkew*CRFrlTilt*SRotFurl )*d2 &
    + ( CSRFrlSkw*CRFrlTlt2*( CRotFurl - 1.0 ) -           SRFrlTilt*SRotFurl )*d3
rf2 = ( CRFrlSkew*CSRFrlTlt*( 1.0 - CRotFurl ) + SRFrlSkew*CRFrlTilt*SRotFurl )*d1 &
    + (           CRFrlTlt2          *CRotFurl +           SRFrlTlt2          )*d2 &
    + ( SRFrlSkew*CSRFrlTlt*( CRotFurl - 1.0 ) + CRFrlSkew*CRFrlTilt*SRotFurl )*d3
rf3 = ( CSRFrlSkw*CRFrlTlt2*( CRotFurl - 1.0 ) +           SRFrlTilt*SRotFurl )*d1 &
    + ( SRFrlSkew*CSRFrlTlt*( CRotFurl - 1.0 ) - CRFrlSkew*CRFrlTilt*SRotFurl )*d2 &
    + ( ( 1.0 - SRFrlSkw2*CRFrlTlt2 )*CRotFurl + SRFrlSkw2*CRFrlTlt2          )*d3
rfa = CRFrlSkew*CRFrlTilt*d1 + SRFrlTilt*d2 - SRFrlSkew*CRFrlTilt*d3


   ! Shaft coordinate system:

c1 =  CShftSkew*CShftTilt*rf1 + SShftTilt*rf2 - SShftSkew*CShftTilt*rf3  ! Vector / direction c1 (=  xs from the IEC coord. system).
c2 = -CShftSkew*SShftTilt*rf1 + CShftTilt*rf2 + SShftSkew*SShftTilt*rf3  ! Vector / direction c2 (=  zs from the IEC coord. system).
c3 =  SShftSkew*          rf1                 + CShftSkew*          rf3  ! Vector / direction c3 (= -ys from the IEC coord. system).


   ! Azimuth coordinate system:

CAzimuth = COS( QT(DOF_DrTr) + QT(DOF_GeAz) )
SAzimuth = SIN( QT(DOF_DrTr) + QT(DOF_GeAz) )

e1 =  c1                         ! Vector / direction e1 (=  xa from the IEC coord. system).
e2 =  CAzimuth*c2 + SAzimuth*c3  ! Vector / direction e2 (=  ya from the IEC coord. system).
e3 = -SAzimuth*c2 + CAzimuth*c3  ! Vector / direction e3 (=  za from the IEC coord. system).


   ! Teeter coordinate system:

   ! Lets define TeetAng, which is the current teeter angle (= QT(DOF_Teet) for
   !   2-blader or 0 for 3-blader) and is used in place of QT(DOF_Teet)
   !   throughout SUBROUTINE RtHS().  Doing it this way, we can run the same
   !   equations of motion for both the 2 and 3-blader configurations even
   !   though a 3-blader does not have a teetering DOF.

IF ( NumBL == 2 )  THEN ! 2-blader
   TeetAng    = QT (DOF_Teet)
   TeetAngVel = QDT(DOF_Teet)
ELSE                    ! 3-blader
   TeetAng    = 0.0  ! Teeter is not an available DOF for a 3-blader
   TeetAngVel = 0.0  ! Teeter is not an available DOF for a 3-blader
ENDIF
CTeetAng = COS( TeetAng )
STeetAng = SIN( TeetAng )

f1 = CTeetAng*e1 - STeetAng*e3   ! Vector / direction f1.
f2 = e2                          ! Vector / direction f2.
f3 = STeetAng*e1 + CTeetAng*e3   ! Vector / direction f3.


   ! Hub / delta-3 coordinate system:

g1 =  f1                         ! Vector / direction g1 (=  xh from the IEC coord. system).
g2 =  CosDel3*f2 + SinDel3*f3    ! Vector / direction g2 (=  yh from the IEC coord. system).
g3 = -SinDel3*f2 + CosDel3*f3    ! Vector / direction g3 (=  zh from the IEC coord. system).


DO K = 1,NumBl ! Loop through all blades


   ! Hub (Prime) coordinate system rotated to match blade K.

    gRotAng = TwoPiNB*(K-1)
   CgRotAng = COS( gRotAng )
   SgRotAng = SIN( gRotAng )

   g1Prime =  g1
   g2Prime =  CgRotAng*g2 + SgRotAng*g3
   g3Prime = -SgRotAng*g2 + CgRotAng*g3


   ! Coned coordinate system:

   i1(K,:) = CosPreC(K)*g1Prime - SinPreC(K)*g3Prime  ! i1(K,:) = vector / direction i1 for blade K (=  xcK from the IEC coord. system).
   i2(K,:) = g2Prime                                  ! i2(K,:) = vector / direction i2 for blade K (=  ycK from the IEC coord. system).
   i3(K,:) = SinPreC(K)*g1Prime + CosPreC(K)*g3Prime  ! i3(K,:) = vector / direction i3 for blade K (=  zcK from the IEC coord. system).


   ! Blade / pitched coordinate system:

   CosPitch = COS( BlPitch(K) )
   SinPitch = SIN( BlPitch(K) )

   j1(K,:) = CosPitch*i1(K,:) - SinPitch*i2(K,:)      ! j1(K,:) = vector / direction j1 for blade K (=  xbK from the IEC coord. system).
   j2(K,:) = SinPitch*i1(K,:) + CosPitch*i2(K,:)      ! j2(K,:) = vector / direction j2 for blade K (=  ybK from the IEC coord. system).
   j3(K,:) = i3(K,:)                                  ! j3(K,:) = vector / direction j3 for blade K (=  zbK from the IEC coord. system).


!JASON: USE TipNode HERE INSTEAD OF BldNodes IF YOU ALLOCATE AND DEFINE n1, n2, n3, m1, m2, AND m3 TO USE TipNode.  THIS WILL REQUIRE THAT THE AERODYNAMIC AND STRUCTURAL TWISTS, AeroTwst() AND ThetaS(), BE KNOWN AT THE TIP!!!
   DO J = 1,BldNodes ! Loop through the blade nodes / elements


   ! Blade coordinate system aligned with local structural axes (not element fixed):

      Lj1 = CThetaS(K,J)*j1(K,:) - SThetaS(K,J)*j2(K,:)  ! vector / direction Lj1 at node J for blade K
      Lj2 = SThetaS(K,J)*j1(K,:) + CThetaS(K,J)*j2(K,:)  ! vector / direction Lj2 at node J for blade K
      Lj3 = j3(K,:)                                      ! vector / direction Lj3 at node J for blade K


   ! Blade element-fixed coordinate system aligned with local structural axes:

      ThetaOoP =   TwistedSF(K,1,1,J,1)*QT( DOF_BF(K,1) ) &
                 + TwistedSF(K,1,2,J,1)*QT( DOF_BF(K,2) ) &
                 + TwistedSF(K,1,3,J,1)*QT( DOF_BE(K,1) )
      ThetaIP  = - TwistedSF(K,2,1,J,1)*QT( DOF_BF(K,1) ) &
                 - TwistedSF(K,2,2,J,1)*QT( DOF_BF(K,2) ) &
                 - TwistedSF(K,2,3,J,1)*QT( DOF_BE(K,1) )

      ThetaLxb = CThetaS(K,J)*ThetaIP - SThetaS(K,J)*ThetaOoP
      ThetaLyb = SThetaS(K,J)*ThetaIP + CThetaS(K,J)*ThetaOoP

      CALL SmllRotTrans( 'blade deflection', ThetaLxb, ThetaLyb, 0.0, TransMat ) ! Get the transformation matrix, TransMat, from blade coordinate system aligned with local structural axes (not element fixed) to blade element-fixed coordinate system aligned with local structural axes.

      n1(K,J,:) = TransMat(1,1)*Lj1 + TransMat(1,2)*Lj2 + TransMat(1,3)*Lj3   ! Vector / direction n1 for node J of blade K (= LxbK from the IEC coord. system).
      n2(K,J,:) = TransMat(2,1)*Lj1 + TransMat(2,2)*Lj2 + TransMat(2,3)*Lj3   ! Vector / direction n2 for node J of blade K (= LybK from the IEC coord. system).
      n3(K,J,:) = TransMat(3,1)*Lj1 + TransMat(3,2)*Lj2 + TransMat(3,3)*Lj3   ! Vector / direction n3 for node J of blade K (= LzbK from the IEC coord. system).


   ! Blade element-fixed coordinate system used for calculating and returning
   !    aerodynamics loads:
   ! This coordinate system is rotated about positive n3 by the angle
   !    BlPitch(K) + ThetaS(K,J) and is coincident with the i-vector triad
   !    when the blade is undeflected.

      CPitPTwstS = CosPitch*CThetaS(K,J) - SinPitch*SThetaS(K,J)  ! = COS( BlPitch(K) + ThetaS(K,J) ) found using the sum of angles formulae of cosine.
      SPitPTwstS = CosPitch*SThetaS(K,J) + SinPitch*CThetaS(K,J)  ! = SIN( BlPitch(K) + ThetaS(K,J) ) found using the sum of angles formulae of   sine.

      m1(K,J,:)  =  CPitPTwstS*n1(K,J,:) + SPitPTwstS*n2(K,J,:)   ! m1(K,J,:) = vector / direction m1 for node J of blade K (used to calc. and return aerodynamic loads from AeroDyn).
      m2(K,J,:)  = -SPitPTwstS*n1(K,J,:) + CPitPTwstS*n2(K,J,:)   ! m2(K,J,:) = vector / direction m2 for node J of blade K (used to calc. and return aerodynamic loads from AeroDyn).
      m3(K,J,:)  =  n3(K,J,:)                                     ! m3(K,J,:) = vector / direction m3 for node J of blade K (used to calc. and return aerodynamic loads from AeroDyn).


   ! Calculate the trailing edge coordinate system used in noise calculations.
   ! This coordinate system is blade element-fixed and oriented with the local
   !   aerodynamic axes (te2 points toward trailing edge, te1 points toward
   !   suction surface):

      CPitPTwstA = CosPitch*CAeroTwst(J) - SinPitch*SAeroTwst(J)  ! = COS( BlPitch(K) + AeroTwst(J) ) found using the sum of angles formulae of cosine.
      SPitPTwstA = CosPitch*SAeroTwst(J) + SinPitch*CAeroTwst(J)  ! = SIN( BlPitch(K) + AeroTwst(J) ) found using the sum of angles formulae of   sine.

!bjj: modified description of te system
      te1(K,J,:) =  CPitPTwstA*m1(K,J,:) - SPitPTwstA*m2(K,J,:)   ! te1(K,J,:) = vector / direction te1 for node J of blade K (used to calc. noise and to calc. and return aerodynamic loads from AeroDyn).
      te2(K,J,:) =  SPitPTwstA*m1(K,J,:) + CPitPTwstA*m2(K,J,:)   ! te2(K,J,:) = vector / direction te2 for node J of blade K (used to calc. noise and to calc. and return aerodynamic loads from AeroDyn).
      te3(K,J,:) =  m3(K,J,:)                                     ! te3(K,J,:) = vector / direction te3 for node J of blade K (used to calc. noise and to calc. and return aerodynamic loads from AeroDyn).


   ENDDO ! J - Blade nodes / elements


ENDDO ! K - Blades


   ! Tail-furl coordinate system:

CTailFurl = COS( QT(DOF_TFrl) )
STailFurl = SIN( QT(DOF_TFrl) )

tf1 = ( ( 1.0 - CTFrlSkw2*CTFrlTlt2 )*CTailFurl + CTFrlSkw2*CTFrlTlt2           )*d1 &
    + ( CTFrlSkew*CSTFrlTlt*( 1.0 - CTailFurl ) - STFrlSkew*CTFrlTilt*STailFurl )*d2 &
    + ( CSTFrlSkw*CTFrlTlt2*( CTailFurl - 1.0 ) -           STFrlTilt*STailFurl )*d3
tf2 = ( CTFrlSkew*CSTFrlTlt*( 1.0 - CTailFurl ) + STFrlSkew*CTFrlTilt*STailFurl )*d1 &
    + (           CTFrlTlt2          *CTailFurl +           STFrlTlt2           )*d2 &
    + ( STFrlSkew*CSTFrlTlt*( CTailFurl - 1.0 ) + CTFrlSkew*CTFrlTilt*STailFurl )*d3
tf3 = ( CSTFrlSkw*CTFrlTlt2*( CTailFurl - 1.0 ) +           STFrlTilt*STailFurl )*d1 &
    + ( STFrlSkew*CSTFrlTlt*( CTailFurl - 1.0 ) - CTFrlSkew*CTFrlTilt*STailFurl )*d2 &
    + ( ( 1.0 - STFrlSkw2*CTFrlTlt2 )*CTailFurl + STFrlSkw2*CTFrlTlt2           )*d3
tfa = CTFrlSkew*CTFrlTilt*d1 + STFrlTilt*d2 - STFrlSkew*CTFrlTilt*d3


   ! Tail fin coordinate system:

p1 = (                       CTFinSkew*CTFinTilt           )*tf1 &   ! Vector / direction p1 (= tail fin  x).
   + (                                 STFinTilt           )*tf2 &
   + (                     - STFinSkew*CTFinTilt           )*tf3
p2 = ( STFinSkew*STFinBank - CTFinSkew*STFinTilt*CTFinBank )*tf1 &   ! Vector / direction p2 (= tail fin  z).
   + (                                 CTFinTilt*CTFinBank )*tf2 &
   + ( CTFinSkew*STFinBank + STFinSkew*STFinTilt*CTFinBank )*tf3
p3 = ( STFinSkew*CTFinBank + CTFinSkew*STFinTilt*STFinBank )*tf1 &   ! Vector / direction p3 (= tail fin -y).
   + (                     -           CTFinTilt*STFinBank )*tf2 &
   + ( CTFinSkew*CTFinBank - STFinSkew*STFinTilt*STFinBank )*tf3



RETURN
END SUBROUTINE SetCoordSy
!=======================================================================
SUBROUTINE SetEnabledDOFIndexArrays


   ! This routine is used create arrays of DOF indices (pointers /
   !   (vector susbscript arrays) that contribute to the QD2T-related
   !   linear accelerations of various points within the system in the
   !   inertia frame, based on which DOFs are presently enabled.
   ! NOTE: The order in which the DOFs are tested within this routine,
   !       and hence the order in which the DOF indices appear in the
   !       vector subscript arrays, determines the order in which the
   !       states will appear in the linearized model created by FAST
   !       when AnalMode == 2.  This order is not necessarily sorted
   !       from smallest to largest DOF index.


USE                             DOFs
USE                             TurbConf

IMPLICIT                        NONE


   ! Local Variables:

INTEGER(4)                   :: I                                               ! Loops through all DOFs.
INTEGER(4)                   :: K                                               ! Loops through blades.



   ! Initialize total counts to zero.

NActvDOF = 0
NPCE     = 0
NPDE     = 0
NPIE     = 0
NPTTE    = 0
NPTE     = 0
NPSBE(:) = 0
NPSE (:) = 0
NPUE     = 0
NPYE     = 0


   ! Test each DOF and include the appropriate indices in the subscript arrays
   !  and total counts:

IF ( DOF_Flag(DOF_Sg  ) )  THEN  ! Platform surge.

   NActvDOF = NActvDOF + 1
   NPCE     = NPCE     + 1
   NPDE     = NPDE     + 1
   NPIE     = NPIE     + 1
   NPTE     = NPTE     + 1
   NPSE (:) = NPSE (:) + 1
   NPUE     = NPUE     + 1
   NPYE     = NPYE     + 1

    PS     (  NActvDOF) = DOF_Sg
    PCE    (  NPCE    ) = DOF_Sg
    PDE    (  NPDE    ) = DOF_Sg
    PIE    (  NPIE    ) = DOF_Sg
    PTE    (  NPTE    ) = DOF_Sg
    PSE    (:,NPSE (:)) = DOF_Sg
    PUE    (  NPUE    ) = DOF_Sg
    PYE    (  NPYE    ) = DOF_Sg

ENDIF


IF ( DOF_Flag(DOF_Sw  ) )  THEN  ! Platform sway.

   NActvDOF = NActvDOF + 1
   NPCE     = NPCE     + 1
   NPDE     = NPDE     + 1
   NPIE     = NPIE     + 1
   NPTE     = NPTE     + 1
   NPSE (:) = NPSE (:) + 1
   NPUE     = NPUE     + 1
   NPYE     = NPYE     + 1

    PS     (  NActvDOF) = DOF_Sw
    PCE    (  NPCE    ) = DOF_Sw
    PDE    (  NPDE    ) = DOF_Sw
    PIE    (  NPIE    ) = DOF_Sw
    PTE    (  NPTE    ) = DOF_Sw
    PSE    (:,NPSE (:)) = DOF_Sw
    PUE    (  NPUE    ) = DOF_Sw
    PYE    (  NPYE    ) = DOF_Sw

ENDIF


IF ( DOF_Flag(DOF_Hv  ) )  THEN  ! Platform heave.

   NActvDOF = NActvDOF + 1
   NPCE     = NPCE     + 1
   NPDE     = NPDE     + 1
   NPIE     = NPIE     + 1
   NPTE     = NPTE     + 1
   NPSE (:) = NPSE (:) + 1
   NPUE     = NPUE     + 1
   NPYE     = NPYE     + 1

    PS     (  NActvDOF) = DOF_Hv
    PCE    (  NPCE    ) = DOF_Hv
    PDE    (  NPDE    ) = DOF_Hv
    PIE    (  NPIE    ) = DOF_Hv
    PTE    (  NPTE    ) = DOF_Hv
    PSE    (:,NPSE (:)) = DOF_Hv
    PUE    (  NPUE    ) = DOF_Hv
    PYE    (  NPYE    ) = DOF_Hv

ENDIF


IF ( DOF_Flag(DOF_R   ) )  THEN  ! Platform roll.

   NActvDOF = NActvDOF + 1
   NPCE     = NPCE     + 1
   NPDE     = NPDE     + 1
   NPIE     = NPIE     + 1
   NPTE     = NPTE     + 1
   NPSE (:) = NPSE (:) + 1
   NPUE     = NPUE     + 1
   NPYE     = NPYE     + 1

    PS     (  NActvDOF) = DOF_R
    PCE    (  NPCE    ) = DOF_R
    PDE    (  NPDE    ) = DOF_R
    PIE    (  NPIE    ) = DOF_R
    PTE    (  NPTE    ) = DOF_R
    PSE    (:,NPSE (:)) = DOF_R
    PUE    (  NPUE    ) = DOF_R
    PYE    (  NPYE    ) = DOF_R

ENDIF


IF ( DOF_Flag(DOF_P   ) )  THEN  ! Platform pitch.

   NActvDOF = NActvDOF + 1
   NPCE     = NPCE     + 1
   NPDE     = NPDE     + 1
   NPIE     = NPIE     + 1
   NPTE     = NPTE     + 1
   NPSE (:) = NPSE (:) + 1
   NPUE     = NPUE     + 1
   NPYE     = NPYE     + 1

    PS     (  NActvDOF) = DOF_P
    PCE    (  NPCE    ) = DOF_P
    PDE    (  NPDE    ) = DOF_P
    PIE    (  NPIE    ) = DOF_P
    PTE    (  NPTE    ) = DOF_P
    PSE    (:,NPSE (:)) = DOF_P
    PUE    (  NPUE    ) = DOF_P
    PYE    (  NPYE    ) = DOF_P

ENDIF


IF ( DOF_Flag(DOF_Y   ) )  THEN  ! Platform yaw.

   NActvDOF = NActvDOF + 1
   NPCE     = NPCE     + 1
   NPDE     = NPDE     + 1
   NPIE     = NPIE     + 1
   NPTE     = NPTE     + 1
   NPSE (:) = NPSE (:) + 1
   NPUE     = NPUE     + 1
   NPYE     = NPYE     + 1

    PS     (  NActvDOF) = DOF_Y
    PCE    (  NPCE    ) = DOF_Y
    PDE    (  NPDE    ) = DOF_Y
    PIE    (  NPIE    ) = DOF_Y
    PTE    (  NPTE    ) = DOF_Y
    PSE    (:,NPSE (:)) = DOF_Y
    PUE    (  NPUE    ) = DOF_Y
    PYE    (  NPYE    ) = DOF_Y

ENDIF


IF ( DOF_Flag(DOF_TFA1) )  THEN  ! 1st tower fore-aft.

   NActvDOF = NActvDOF + 1
   NPCE     = NPCE     + 1
   NPDE     = NPDE     + 1
   NPIE     = NPIE     + 1
   NPTTE    = NPTTE    + 1
   NPTE     = NPTE     + 1
   NPSE (:) = NPSE (:) + 1
   NPUE     = NPUE     + 1

    PS     (  NActvDOF) = DOF_TFA1
    PCE    (  NPCE    ) = DOF_TFA1
    PDE    (  NPDE    ) = DOF_TFA1
    PIE    (  NPIE    ) = DOF_TFA1
    PTTE   (  NPTTE   ) = DOF_TFA1
    PTE    (  NPTE    ) = DOF_TFA1
    PSE    (:,NPSE (:)) = DOF_TFA1
    PUE    (  NPUE    ) = DOF_TFA1

ENDIF


IF ( DOF_Flag(DOF_TSS1) )  THEN  ! 1st tower side-to-side.

   NActvDOF = NActvDOF + 1
   NPCE     = NPCE     + 1
   NPDE     = NPDE     + 1
   NPIE     = NPIE     + 1
   NPTTE    = NPTTE    + 1
   NPTE     = NPTE     + 1
   NPSE (:) = NPSE (:) + 1
   NPUE     = NPUE     + 1

    PS     (  NActvDOF) = DOF_TSS1
    PCE    (  NPCE    ) = DOF_TSS1
    PDE    (  NPDE    ) = DOF_TSS1
    PIE    (  NPIE    ) = DOF_TSS1
    PTTE   (  NPTTE   ) = DOF_TSS1
    PTE    (  NPTE    ) = DOF_TSS1
    PSE    (:,NPSE (:)) = DOF_TSS1
    PUE    (  NPUE    ) = DOF_TSS1

ENDIF


IF ( DOF_Flag(DOF_TFA2) )  THEN  ! 2nd tower fore-aft.

   NActvDOF = NActvDOF + 1
   NPCE     = NPCE     + 1
   NPDE     = NPDE     + 1
   NPIE     = NPIE     + 1
   NPTTE    = NPTTE    + 1
   NPTE     = NPTE     + 1
   NPSE (:) = NPSE (:) + 1
   NPUE     = NPUE     + 1

    PS     (  NActvDOF) = DOF_TFA2
    PCE    (  NPCE    ) = DOF_TFA2
    PDE    (  NPDE    ) = DOF_TFA2
    PIE    (  NPIE    ) = DOF_TFA2
    PTTE   (  NPTTE   ) = DOF_TFA2
    PTE    (  NPTE    ) = DOF_TFA2
    PSE    (:,NPSE (:)) = DOF_TFA2
    PUE    (  NPUE    ) = DOF_TFA2

ENDIF


IF ( DOF_Flag(DOF_TSS2) )  THEN  ! 2nd tower side-to-side.

   NActvDOF = NActvDOF + 1
   NPCE     = NPCE     + 1
   NPDE     = NPDE     + 1
   NPIE     = NPIE     + 1
   NPTTE    = NPTTE    + 1
   NPTE     = NPTE     + 1
   NPSE (:) = NPSE (:) + 1
   NPUE     = NPUE     + 1

    PS     (  NActvDOF) = DOF_TSS2
    PCE    (  NPCE    ) = DOF_TSS2
    PDE    (  NPDE    ) = DOF_TSS2
    PIE    (  NPIE    ) = DOF_TSS2
    PTTE   (  NPTTE   ) = DOF_TSS2
    PTE    (  NPTE    ) = DOF_TSS2
    PSE    (:,NPSE (:)) = DOF_TSS2
    PUE    (  NPUE    ) = DOF_TSS2

ENDIF


IF ( DOF_Flag(DOF_Yaw ) )  THEN  ! Nacelle yaw.

   NActvDOF = NActvDOF + 1
   NPCE     = NPCE     + 1
   NPDE     = NPDE     + 1
   NPIE     = NPIE     + 1
   NPSE (:) = NPSE (:) + 1
   NPUE     = NPUE     + 1

    PS     (  NActvDOF) = DOF_Yaw
    PCE    (  NPCE    ) = DOF_Yaw
    PDE    (  NPDE    ) = DOF_Yaw
    PIE    (  NPIE    ) = DOF_Yaw
    PSE    (:,NPSE (:)) = DOF_Yaw
    PUE    (  NPUE    ) = DOF_Yaw

ENDIF


IF ( DOF_Flag(DOF_TFrl) )  THEN  ! Tail-furl.

   NActvDOF = NActvDOF + 1
   NPIE     = NPIE     + 1

    PS     (  NActvDOF) = DOF_TFrl
    PIE    (  NPIE    ) = DOF_TFrl

ENDIF


IF ( DOF_Flag(DOF_RFrl) )  THEN  ! Rotor-furl.

   NActvDOF = NActvDOF + 1
   NPCE     = NPCE     + 1
   NPDE     = NPDE     + 1
   NPSE (:) = NPSE (:) + 1

    PS     (  NActvDOF) = DOF_RFrl
    PCE    (  NPCE    ) = DOF_RFrl
    PDE    (  NPDE    ) = DOF_RFrl
    PSE    (:,NPSE (:)) = DOF_RFrl

ENDIF


IF ( DOF_Flag(DOF_GeAz) )  THEN  ! Generator azimuth.

   NActvDOF = NActvDOF + 1
   NPCE     = NPCE     + 1
   NPSE (:) = NPSE (:) + 1

    PS     (  NActvDOF) = DOF_GeAz
    PCE    (  NPCE    ) = DOF_GeAz
    PSE    (:,NPSE (:)) = DOF_GeAz

ENDIF


IF ( DOF_Flag(DOF_DrTr) )  THEN  ! Drivetrain torsion.

   NActvDOF = NActvDOF + 1
   NPCE     = NPCE     + 1
   NPSE (:) = NPSE (:) + 1

    PS     (  NActvDOF) = DOF_DrTr
    PCE    (  NPCE    ) = DOF_DrTr
    PSE    (:,NPSE (:)) = DOF_DrTr

ENDIF


IF ( NumBl == 2 )  THEN
   IF ( DOF_Flag(DOF_Teet   ) )  THEN  ! Rotor-teeter.

      NActvDOF = NActvDOF + 1
      NPCE     = NPCE     + 1
      NPSE (:) = NPSE (:) + 1

       PS     (  NActvDOF) = DOF_Teet
       PCE    (  NPCE    ) = DOF_Teet
       PSE    (:,NPSE (:)) = DOF_Teet

   ENDIF
ENDIF


DO K = 1,NumBl ! Loop through all blades
   IF ( DOF_Flag(DOF_BF(K,1)) )  THEN  ! 1st blade flap.

      NActvDOF = NActvDOF + 1
      NPSBE(K) = NPSBE(K) + 1
      NPSE (K) = NPSE (K) + 1

       PS     (  NActvDOF) = DOF_BF(K,1)
       PSBE   (K,NPSBE(K)) = DOF_BF(K,1)
       PSE    (K,NPSE (K)) = DOF_BF(K,1)

   ENDIF
ENDDO          ! K - Blades


DO K = 1,NumBl ! Loop through all blades
   IF ( DOF_Flag(DOF_BE(K,1)) )  THEN  ! 1st blade edge.

      NActvDOF = NActvDOF + 1
      NPSBE(K) = NPSBE(K) + 1
      NPSE (K) = NPSE (K) + 1

       PS     (  NActvDOF) = DOF_BE(K,1)
       PSBE   (K,NPSBE(K)) = DOF_BE(K,1)
       PSE    (K,NPSE (K)) = DOF_BE(K,1)

   ENDIF
ENDDO          ! K - Blades


DO K = 1,NumBl ! Loop through all blades
   IF ( DOF_Flag(DOF_BF(K,2)) )  THEN  ! 2nd blade flap.

      NActvDOF = NActvDOF + 1
      NPSBE(K) = NPSBE(K) + 1
      NPSE (K) = NPSE (K) + 1

       PS     (  NActvDOF) = DOF_BF(K,2)
       PSBE   (K,NPSBE(K)) = DOF_BF(K,2)
       PSE    (K,NPSE (K)) = DOF_BF(K,2)

   ENDIF
ENDDO          ! K - Blades



   ! Compute the sorted (from smallest to largest DOF index) version of PS(),
   !   SrtPS(), and SrtPSNAUG().  At the same time compute Diag(), which is an
   !   array containing the indices of SrtPS() associated with each enabled
   !   DOF; that is, SrtPS(Diag(I)) = I:
   ! NOTE: This calculation is recomputing NActvDOF as computed above.  This is
   !       of no concern however, since the resulting value will be the same.

NActvDOF = 0
DO I = 1,NDOF  ! Loop through all DOFs
   IF ( DOF_Flag(I) )  THEN   ! .TRUE. if the corresponding DOF is enabled

      NActvDOF = NActvDOF + 1

      SrtPS     (NActvDOF) = I
      SrtPSNAUG (NActvDOF) = I
      Diag      (I       ) = NActvDOF

   ENDIF
ENDDO          ! I - All DOFs

SrtPSNAUG ( NActvDOF + 1 ) = NAUG



RETURN
END SUBROUTINE SetEnabledDOFIndexArrays
!=======================================================================
!bjj start of proposed change
!rm!JASON: THIS ROUTINE (SmllRotTrans) SHOULD BE MOVED TO NWTC_Subs!!!
!rmSUBROUTINE SmllRotTrans( RotationType, Theta1, Theta2, Theta3, TransMat )
!rm
!rm
!rm   ! This routine computes the 3x3 transformation matrix, TransMat,
!rm   !   to a coordinate system x (with orthogonal axes x1, x2, x3)
!rm   !   resulting from three rotations (Theta1, Theta2, Theta3) about the
!rm   !   orthogonal axes (X1, X2, X3) of coordinate system X.  All angles
!rm   !   are assummed to be small, as such, the order of rotations does
!rm   !   not matter and Euler angles do not need to be used.  This routine
!rm   !   is used to compute the transformation matrix (TransMat) between
!rm   !   undeflected (X) and deflected (x) coordinate systems.  In matrix
!rm   !   form:
!rm   !      {x1}   [TransMat(Theta1, ] {X1}
!rm   !      {x2} = [         Theta2, ]*{X2}
!rm   !      {x3}   [         Theta3 )] {X3}
!rm
!rm   ! The transformation matrix, TransMat, is the closest orthonormal
!rm   !   matrix to the nonorthonormal, but skew-symmetric, Bernoulli-Euler
!rm   !   matrix:
!rm   !          [   1.0    Theta3 -Theta2 ]
!rm   !      A = [ -Theta3   1.0    Theta1 ]
!rm   !          [  Theta2 -Theta1   1.0   ]
!rm   !
!rm   !   In the Frobenius Norm sense, the closest orthornormal matrix is:
!rm   !      TransMat = U*V^T,
!rm   !
!rm   !   where the columns of U contain the eigenvectors of A*A^T and the
!rm   !   columns of V contain the eigenvectors of A^T*A (^T = transpose).
!rm   !   This result comes directly from the Singular Value Decomposition
!rm   !   (SVD) of A = U*S*V^T where S is a diagonal matrix containing the
!rm   !   singular values of A, which are SQRT( eigenvalues of A*A^T ) =
!rm   !   SQRT( eigenvalues of A^T*A ).
!rm
!rm   ! The algebraic form of the transformation matrix, as implemented
!rm   !   below, was derived symbolically by J. Jonkman by computing U*V^T
!rm   !   by hand with verification in Mathematica.
!rm
!rm
!rm!bjj rm NWTC_Library: USE                             Precision
!rm!bjj rm NWTC_Library: USE                             SysSubs
!rm
!rm
!rmIMPLICIT                        NONE
!rm
!rm
!rm   ! Passed Variables:
!rm
!rmREAL(ReKi), INTENT(IN )      :: Theta1                                          ! The small rotation about X1, (rad).
!rmREAL(ReKi), INTENT(IN )      :: Theta2                                          ! The small rotation about X2, (rad).
!rmREAL(ReKi), INTENT(IN )      :: Theta3                                          ! The small rotation about X3, (rad).
!rmREAL(ReKi), INTENT(OUT)      :: TransMat (3,3)                                  ! The resulting transformation matrix from X to x, (-).
!rm
!rmCHARACTER(*), INTENT(IN)     :: RotationType                                    ! The type of rotation; used to inform the user where a large rotation is occuring upon such an event.
!rm
!rm
!rm   ! Local Variables:
!rm
!rmREAL(ReKi)                   :: ComDenom                                        ! = ( Theta1^2 + Theta2^2 + Theta3^2 )*SQRT( 1.0 + Theta1^2 + Theta2^2 + Theta3^2 )
!rm!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!rm!jmj Increase this large angle threshold to 0.4 radians:
!rm!remove6.02aREAL(ReKi), PARAMETER        :: LrgAngle  = 0.3                                 ! Threshold for when a small angle becomes large (about 17deg).  This comes from:  cos(SmllAngle) ~ 1 and:  SmllAngle = 0.3rad results in ~ 5% error.
!rmREAL(ReKi), PARAMETER        :: LrgAngle  = 0.4                                 ! Threshold for when a small angle becomes large (about 23deg).  This comes from: COS(SmllAngle) ~ 1/SQRT( 1 + SmllAngle^2 ) and SIN(SmllAngle) ~ SmllAngle/SQRT( 1 + SmllAngle^2 ) results in ~5% error when SmllAngle = 0.4rad.
!rm!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
!rmREAL(ReKi)                   :: Theta11                                         ! = Theta1^2
!rmREAL(ReKi)                   :: Theta12S                                        ! = Theta1*Theta2*[ SQRT( 1.0 + Theta1^2 + Theta2^2 + Theta3^2 ) - 1.0 ]
!rmREAL(ReKi)                   :: Theta13S                                        ! = Theta1*Theta3*[ SQRT( 1.0 + Theta1^2 + Theta2^2 + Theta3^2 ) - 1.0 ]
!rmREAL(ReKi)                   :: Theta22                                         ! = Theta2^2
!rmREAL(ReKi)                   :: Theta23S                                        ! = Theta2*Theta3*[ SQRT( 1.0 + Theta1^2 + Theta2^2 + Theta3^2 ) - 1.0 ]
!rmREAL(ReKi)                   :: Theta33                                         ! = Theta3^2
!rmREAL(ReKi)                   :: SqrdSum                                         ! = Theta1^2 + Theta2^2 + Theta3^2
!rm!rmREAL(ReKi)                   :: SQRT1SqrdSum                                    ! = SQRT( 1.0 + Theta1^2 + Theta2^2 + Theta3^2 )
!rm
!rm!bjj chg: LOGICAL(1), SAVE             :: FrstWarn  = .TRUE.                              ! When .TRUE., indicates that we're on the first warning.
!rmLOGICAL,    SAVE             :: FrstWarn  = .TRUE.                              ! When .TRUE., indicates that we're on the first warning.
!rm
!rm
!rm
!rm   ! Display a warning message if at least one angle gets too large in
!rm   !   magnitude:
!rm
!rmIF ( ( ( ABS(Theta1) > LrgAngle ) .OR. ( ABS(Theta2) > LrgAngle ) .OR. ( ABS(Theta3) > LrgAngle ) ) .AND. FrstWarn )  THEN
!rm
!rm   CALL WrOver(' WARNING:                                                            ')
!rm   CALL WrScr ('  Small angle assumption violated in SUBROUTINE SmllRotTrans() due to')
!rm   CALL WrScr ('  a large '//TRIM(RotationType)//'.  The solution may be inaccurate. ')
!rm   CALL WrScr ('  Future warnings suppressed.  Simulation continuing...              ')
!rm   CALL WrScr ('                                                                     ')
!rm
!rm   CALL UsrAlarm
!rm
!rm
!rm   FrstWarn = .FALSE.   ! Don't enter here again!
!rm
!rmENDIF
!rm
!rm
!rm
!rm   ! Compute some intermediate results:
!rm
!rmTheta11      = Theta1*Theta1
!rmTheta22      = Theta2*Theta2
!rmTheta33      = Theta3*Theta3
!rm
!rmSqrdSum      = Theta11 + Theta22 + Theta33
!rmSQRT1SqrdSum = SQRT( 1.0 + SqrdSum )
!rmComDenom     = SqrdSum*SQRT1SqrdSum
!rm
!rmTheta12S     = Theta1*Theta2*( SQRT1SqrdSum - 1.0 )
!rm!rmTheta13S     = Theta1*Theta3*( SQRT1SqrdSum - 1.0 )
!rmTheta23S     = Theta2*Theta3*( SQRT1SqrdSum - 1.0 )
!rm
!rm
!rm   ! Define the transformation matrix:
!rm
!rmIF ( ComDenom == 0.0 )  THEN  ! All angles are zero and matrix is ill-conditioned (the matrix is derived assuming that the angles are not zero); return identity
!rm
!rm   TransMat(1,:) = (/ 1.0, 0.0, 0.0 /)
!rm   TransMat(2,:) = (/ 0.0, 1.0, 0.0 /)
!rm   TransMat(3,:) = (/ 0.0, 0.0, 1.0 /)
!rm
!rmELSE                          ! At least one angle is nonzero
!rm
!rm   TransMat(1,1) = ( Theta11*SQRT1SqrdSum + Theta22              + Theta33              )/ComDenom
!rm   TransMat(2,2) = ( Theta11              + Theta22*SQRT1SqrdSum + Theta33              )/ComDenom
!rm   TransMat(3,3) = ( Theta11              + Theta22              + Theta33*SQRT1SqrdSum )/ComDenom
!rm   TransMat(1,2) = (  Theta3*SqrdSum + Theta12S )/ComDenom
!rm   TransMat(2,1) = ( -Theta3*SqrdSum + Theta12S )/ComDenom
!rm   TransMat(1,3) = ( -Theta2*SqrdSum + Theta13S )/ComDenom
!rm   TransMat(3,1) = (  Theta2*SqrdSum + Theta13S )/ComDenom
!rm   TransMat(2,3) = (  Theta1*SqrdSum + Theta23S )/ComDenom
!rm   TransMat(3,2) = ( -Theta1*SqrdSum + Theta23S )/ComDenom
!rm
!rmENDIF
!rm
!rm
!rm
!rmRETURN
!rmEND SUBROUTINE SmllRotTrans
!rm!=======================================================================
!bjj end of proposed change
SUBROUTINE Solver


   ! Solver solves the equations of motion by marching in time using a
   !   predictor-corrector scheme.  Fourth order Runge-Kutta is used to
   !   get the first 4 points from the initial degrees of freedom and
   !   velocities.


!bjj rm NWTC_Library: USE                             Constants
USE                             DOFs
!bjj rm NWTC_Library: USE                             Precision
USE                             RtHndSid
USE                             SimCont
USE                             TurbCont


IMPLICIT                        NONE


   ! Local variables:

REAL(ReKi), ALLOCATABLE      :: ZK1      (:)                                    ! Runga-Kutta intermediate function used to estimate Q  at next time step.
REAL(ReKi), ALLOCATABLE      :: ZK1D     (:)                                    ! Runga-Kutta intermediate function used to estimate QD at next time step.
REAL(ReKi), ALLOCATABLE      :: ZK2      (:)                                    ! Runga-Kutta intermediate function used to estimate Q  at next time step.
REAL(ReKi), ALLOCATABLE      :: ZK2D     (:)                                    ! Runga-Kutta intermediate function used to estimate QD at next time step.
REAL(ReKi), ALLOCATABLE      :: ZK3      (:)                                    ! Runga-Kutta intermediate function used to estimate Q  at next time step.
REAL(ReKi), ALLOCATABLE      :: ZK3D     (:)                                    ! Runga-Kutta intermediate function used to estimate QD at next time step.
REAL(ReKi), ALLOCATABLE      :: ZK4      (:)                                    ! Runga-Kutta intermediate function used to estimate Q  at next time step.
REAL(ReKi), ALLOCATABLE      :: ZK4D     (:)                                    ! Runga-Kutta intermediate function used to estimate QD at next time step.


INTEGER(4)                   :: I                                               ! Loops through all DOFs
INTEGER(4)                   :: Sttus                                           ! Status returned from an attempt to allocate an array.



IF ( Step < 3 )  THEN   ! Use Runge-Kutta integration at the the start of the simulation (first 3 steps).


   ! Allocate arrays that vary with the number of DOFs..


!BJJ START OF PROPOSED CHANGE
   Sttus = 0
!bjj end of proposed change

   !BJJ REPLACE: ALLOCATE ( ZK1(NDOF) , STAT=Sttus )
   IF (.NOT. ALLOCATED(ZK1)) ALLOCATE ( ZK1(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK1 array.' )
   ENDIF

   !BJJ REPLACE: ALLOCATE ( ZK1D(NDOF) , STAT=Sttus )
   IF (.NOT. ALLOCATED(ZK1D)) ALLOCATE ( ZK1D(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK1D array.' )
   ENDIF

   !BJJ REPLACE: ALLOCATE ( ZK2(NDOF) , STAT=Sttus )
   IF (.NOT. ALLOCATED(ZK2)) ALLOCATE ( ZK2(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK2 array.' )
   ENDIF

   !BJJ REPLACE: ALLOCATE ( ZK2D(NDOF) , STAT=Sttus )
   IF (.NOT. ALLOCATED(ZK2D)) ALLOCATE ( ZK2D(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK2D array.' )
   ENDIF

   !BJJ REPLACE: ALLOCATE ( ZK3(NDOF) , STAT=Sttus )
   IF (.NOT. ALLOCATED(ZK3)) ALLOCATE ( ZK3(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK3 array.' )
   ENDIF

   !BJJ REPLACE: ALLOCATE ( ZK3D(NDOF) , STAT=Sttus )
   IF (.NOT. ALLOCATED(ZK3D)) ALLOCATE ( ZK3D(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK3D array.' )
   ENDIF

   !BJJ REPLACE: ALLOCATE ( ZK4(NDOF) , STAT=Sttus )
   IF (.NOT. ALLOCATED(ZK4)) ALLOCATE ( ZK4(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK4 array.' )
   ENDIF

   !BJJ REPLACE: ALLOCATE ( ZK4D(NDOF) , STAT=Sttus )
   IF (.NOT. ALLOCATED(ZK4D)) ALLOCATE ( ZK4D(NDOF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the ZK4D array.' )
   ENDIF


   ! First call to dynamics routine:

   QT  = Q (:,IC(1))
   QDT = QD(:,IC(1))

   CALL RtHS


   ! Compute intermediate functions to estimate next Q and QD.

   DO I = 1,NDOF  ! Loop through all DOFs
      ZK1 (I) = DT*QD  (I,IC(1))
      ZK1D(I) = DT*QD2T(I)

      QT  (I) = Q (I,IC(1)) + 0.5*ZK1 (I)
      QDT (I) = QD(I,IC(1)) + 0.5*ZK1D(I)
   ENDDO          ! I - All DOFs


   CALL RtHS


   ! Repeat above steps for each ZK, ZKD:

   DO I = 1,NDOF  ! Loop through all DOFs
      ZK2 (I) = DT*( QD  (I,IC(1)) + 0.5*ZK1D(I) )
      ZK2D(I) = DT*  QD2T(I)

      QT  (I) = Q (I,IC(1)) + 0.5*ZK2 (I)
      QDT (I) = QD(I,IC(1)) + 0.5*ZK2D(I)
   ENDDO          ! I - All DOFs


   CALL RtHS


   DO I = 1,NDOF  ! Loop through all DOFs
      ZK3 (I) = DT*( QD  (I,IC(1)) + 0.5*ZK2D(I) )
      ZK3D(I) = DT*  QD2T(I)

      QT  (I) = Q (I,IC(1)) + ZK3 (I)
      QDT (I) = QD(I,IC(1)) + ZK3D(I)
   ENDDO          ! I - All DOFs


   CALL RtHS


   ! Compute best estimate for Q, QD at next time step using
   !   the intermediate functions (Runge-Kutta).
   ! IC(NMX) locates the i + 1 value of Q, QD.

   DO I = 1,NDOF  ! Loop through all DOFs
      ZK4 (I) = DT*( QD  (I,IC(1)) + ZK3D(I) )
      ZK4D(I) = DT*  QD2T(I)

      Q (I,IC(NMX)) = Q (I,IC(1)) + ( ZK1 (I) + 2.0*ZK2 (I) + 2.0*ZK3 (I) + ZK4 (I) ) / 6.0
      QD(I,IC(NMX)) = QD(I,IC(1)) + ( ZK1D(I) + 2.0*ZK2D(I) + 2.0*ZK3D(I) + ZK4D(I) ) / 6.0
   ENDDO          ! I - All DOFs


!bjj start of proposed change V6.02D-BJJ
   IF (ALLOCATED(ZK1) ) DEALLOCATE ( ZK1  )
   IF (ALLOCATED(ZK1D)) DEALLOCATE ( ZK1D )
   IF (ALLOCATED(ZK2) ) DEALLOCATE ( ZK2  )
   IF (ALLOCATED(ZK2D)) DEALLOCATE ( ZK2D )
   IF (ALLOCATED(ZK3) ) DEALLOCATE ( ZK3  )
   IF (ALLOCATED(ZK3D)) DEALLOCATE ( ZK3D )
   IF (ALLOCATED(ZK4) ) DEALLOCATE ( ZK4  )
   IF (ALLOCATED(ZK4D)) DEALLOCATE ( ZK4D )
!bjj end of proposed change V6.02D-BJJ


ELSE                    ! User Adams-Bashforth predictor and Adams-Moulton corrector integration scheme for all other time steps.


   ! Predictor (Adams-Bashforth)

   ! Compute predictor from current (IC(1)) and 3 previous values of
   !   Q, QD, and QD2().  IC(1) = i, IC(2) = i-1, IC(3) = i-2 etc...

   DO I = 1,NDOF  ! Loop through all DOFs
      Q (I,IC(NMX)) = Q (I,IC(1)) + DT24*( 55.0*QD (I,IC(1)) - 59.0*QD (I,IC(2)) + 37.0*QD (I,IC(3)) - 9.0*QD (I,IC(4)) )
      QD(I,IC(NMX)) = QD(I,IC(1)) + DT24*( 55.0*QD2(I,IC(1)) - 59.0*QD2(I,IC(2)) + 37.0*QD2(I,IC(3)) - 9.0*QD2(I,IC(4)) )
   ENDDO          ! I - All DOFs

   QT  = Q (:,IC(NMX))
   QDT = QD(:,IC(NMX))

   CALL RtHS

   QD2(:,IC(NMX)) = QD2T


   ! Corrector (Adams-Moulton)

   ! Compute corrector from predictor value of Q, QD (IC(1)) and 3
   !   previous values of Q, QD, and QD2().  IC(1) = i, IC(2) = i-1,
   !   IC(3) = i-2 etc...

   DO I = 1,NDOF  ! Loop through all DOFs
      Q (I,IC(NMX)) = Q (I,IC(1)) + DT24*( 9.0*QD (I,IC(NMX)) + 19.0*QD (I,IC(1)) - 5.0*QD (I,IC(2)) + QD (I,IC(3)) )
      QD(I,IC(NMX)) = QD(I,IC(1)) + DT24*( 9.0*QD2(I,IC(NMX)) + 19.0*QD2(I,IC(1)) - 5.0*QD2(I,IC(2)) + QD2(I,IC(3)) )
   ENDDO          ! I - All DOFs


    ! Make sure the HSS brake has not reversed the direction of the HSS:

   IF ( DOF_Flag(DOF_GeAz) .AND. ( ZTime > THSSBrDp ) )  CALL FixHSSBrTq ( 'Corrector' )


ENDIF


   ! Compute the final value of QD2T from the best estimates for Q and
   !   QD, last call to RtHS:

QT  = Q (:,IC(NMX))
QDT = QD(:,IC(NMX))

CALL RtHS

QD2(:,IC(NMX)) = QD2T


   ! Update IC() index so IC(1) is the location of current Q values.

IC(1) = IC(1) + 1
IF ( IC(1) > NMX )  IC(1) = IC(1) - NMX
DO I = 2,NMX
   IC(I) = IC(1) - I + 1
   IF ( IC(I) <= 0 )  IC(I) = IC(I) + NMX
ENDDO


   ! Make sure the HSS brake will not reverse the direction of the HSS
   !   for the next time step.  Do this by computing the predicted value
   !   of QD(DOF_GeAz,IC(NMX)) as will be done during the next time step.
   ! Only do this after the first few time steps since it doesn't work
   !   for the Runga-Kutta integration scheme.

IF ( DOF_Flag(DOF_GeAz) .AND. ( ZTime > THSSBrDp ) .AND. ( Step >= 3 ) )  THEN

   QD(DOF_GeAz,IC(NMX)) = QD(DOF_GeAz,IC(1)) + DT24*(   55.0*QD2(DOF_GeAz,IC(1)) - 59.0*QD2(DOF_GeAz,IC(2)) &
                                                      + 37.0*QD2(DOF_GeAz,IC(3)) -  9.0*QD2(DOF_GeAz,IC(4))   )

   CALL FixHSSBrTq ( 'Predictor' )

ENDIF



RETURN
END SUBROUTINE Solver
!=======================================================================
SUBROUTINE Teeter( TeetDef, TeetRate, TeetMom )


   ! This routine computes the teeter moment due to teeter deflection
   !   and rate.


USE                             General
!bjj rm NWTC_Library: USE                             Precision
USE                             SimCont
USE                             TeeterVars


IMPLICIT                        NONE


   ! Passed Variables:

REAL(ReKi), INTENT(IN )      :: TeetDef                                         ! The teeter deflection, QT(DOF_Teet).
REAL(ReKi), INTENT(OUT)      :: TeetMom                                         ! The total moment supplied by the stop, spring, and damper.
REAL(ReKi), INTENT(IN )      :: TeetRate                                        ! The teeter rate, QDT(DOF_Teet).


   ! Local variables:

REAL(ReKi)                   :: AbsDef                                          ! Absolute value of the teeter deflection.
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Remove reference to these unused variables:
!remove6.02aREAL(ReKi)                   :: DampDef                                         ! Deflection past the damper.
!remove6.02aREAL(ReKi)                   :: DmpDef                                          ! Limited deflection past the damper.
!remove6.02aREAL(ReKi)                   :: FDamp                                           ! Some sort of damping function.
!jmj End of proposed change.  v6.02a-jmj  25-Aug-2006.
!bjj rm unused:REAL(ReKi)                   :: DampDef                                         ! Deflection past the damper.
!bjj rm unused:REAL(ReKi)                   :: DmpDef                                          ! Limited deflection past the damper.
!bjj rm unused:REAL(ReKi)                   :: FDamp                                           ! Some sort of damping function.
REAL(ReKi)                   :: SprgDef                                         ! Deflection past the spring.
REAL(ReKi)                   :: StopDef                                         ! Deflection past the stop.
REAL(ReKi)                   :: TeetDMom                                        ! The moment supplied by the damper.
REAL(ReKi)                   :: TeetFMom                                        ! The moment supplied by Coulomb-friction damping.
REAL(ReKi)                   :: TeetKMom                                        ! The moment supplied by the spring.
REAL(ReKi)                   :: TeetSMom                                        ! The moment supplied by the stop.



SELECT CASE ( TeetMod ) ! Which teeter model are we using?

CASE ( 0 )              ! None!


   TeetMom = 0.0


CASE ( 1 )              ! Standard (using inputs from the primary FAST input file).


   ! Compute the absulute value of the deflection.

   AbsDef  = ABS( TeetDef )


   ! Linear teeter spring.

   SprgDef = AbsDef - TeetSStP

   IF ( SprgDef > 0.0 )  THEN
      TeetKMom = -SIGN( SprgDef*TeetSSSp, TeetDef )
   ELSE
      TeetKMom = 0
   ENDIF


   ! Compute teeter-stop moment if hard stop has been contacted.

   StopDef = AbsDef - TeetHStP

   IF ( StopDef > 0.0 )  THEN
      TeetSMom = -TeetHSSp*SIGN( StopDef, TeetDef )
   ELSE
      TeetSMom = 0.0
   ENDIF


   ! Compute linear teeter-damper moment.

   TeetDMom = -TeetDmp*TeetRate


   ! Add coulomb friction to the teeter hinge.

   IF ( TeetRate == 0.0 )  THEN
      TeetFMom = 0.0
   ELSE
      TeetFMom = -SIGN( TeetCDmp, TeetRate )
   ENDIF


   ! Total up all the moments.

   TeetMom = TeetSMom + TeetDMom + TeetKMom + TeetFMom


CASE ( 2 )              ! User-defined teeter spring/damper model.


   CALL UserTeet ( TeetDef, TeetRate, ZTime, DirRoot, TeetMom )


ENDSELECT



RETURN
END SUBROUTINE Teeter
!=======================================================================
SUBROUTINE TFurling( TFrlDef, TFrlRate, TFrlMom )


   ! This routine computes the tail-furl moment due to tail-furl deflection
   !   and rate.


USE                             General
!bjj rm NWTC_Library: USE                             Precision
USE                             SimCont
USE                             TailFurling


IMPLICIT                        NONE


   ! Passed Variables:

REAL(ReKi), INTENT(IN )      :: TFrlDef                                         ! The tail-furl deflection, QT(DOF_TFrl).
REAL(ReKi), INTENT(OUT)      :: TFrlMom                                         ! The total moment supplied by the springs, and dampers.
REAL(ReKi), INTENT(IN )      :: TFrlRate                                        ! The tail-furl rate, QDT(DOF_TFrl).


   ! Local variables:

REAL(ReKi)                   :: TFrlDMom                                        ! The moment supplied by the tail-furl dampers.
REAL(ReKi)                   :: TFrlSMom                                        ! The moment supplied by the tail-furl springs.



SELECT CASE ( TFrlMod ) ! Which tail-furl model are we using?

CASE ( 0 )              ! None!


   TFrlMom = 0.0


CASE ( 1 )              ! Standard (using inputs from the FAST furling input file).


   ! Linear spring:

   TFrlSMom = -TFrlSpr*TFrlDef


   ! Add spring-stops:

   IF ( TFrlDef > TFrlUSSP )  THEN      ! Up-stop
      TFrlSMom = TFrlSMom - TFrlUSSpr*( TFrlDef - TFrlUSSP )
   ELSEIF ( TFrlDef < TFrlDSSP )  THEN  ! Down-stop
      TFrlSMom = TFrlSMom - TFrlDSSpr*( TFrlDef - TFrlDSSP )
   ENDIF


   ! Linear damper:

   TFrlDMom = -TFrlDmp*TFrlRate


   ! Add coulomb friction:

   IF ( TFrlRate /= 0.0 )  THEN
      TFrlDMom = TFrlDMom - SIGN( TFrlCDmp, TFrlRate )
   ENDIF


   ! Add damper-stops:

   IF ( TFrlDef > TFrlUSDP )  THEN      ! Up-stop
      TFrlDMom = TFrlDMom - TFrlUSDmp*TFrlRate
   ELSEIF ( TFrlDef < TFrlDSDP )  THEN  ! Down-stop
      TFrlDMom = TFrlDMom - TFrlDSDmp*TFrlRate
   ENDIF


   ! Total up all the moments.

   TFrlMom = TFrlSMom + TFrlDMom


CASE ( 2 )              ! User-defined tail-furl spring/damper model.


   CALL UserTFrl ( TFrlDef, TFrlRate, ZTime, DirRoot, TFrlMom )


ENDSELECT



RETURN
END SUBROUTINE TFurling
!=======================================================================
SUBROUTINE TimeMarch


   ! TimeMarch controls the execution of the typical time-marching
   !   simulation of the FAST code.


!bjj rm NWTC_Library: USE                             Constants
USE                             DOFs
USE                             Features
USE                             Output
!bjj rm NWTC_Library: USE                             Precision
USE                             SimCont
!bjj rm NWTC_Library: USE                             SysSubs
!bjj start of proposed change vXX
USE                             FAST_IO_Subs       ! WrOutHdr(),  SimStatus(), WrOutput()
USE                             NOISE              ! PredictNoise(), WriteAveSpecOut()
!bjj end of proposed change


IMPLICIT                        NONE


   ! Local variables.

REAL(ReKi)                   :: TiLstPrn  = 0.0                                 ! The time of the last print.



   ! Set up output file format.
! SL: changed from WrOutHdr
CALL WrOutHdr(0)


   ! Start simulation.  Initialize the simulation status.

CALL WrScr1 ( '' )
CALL SimStatus


   ! Loop through time.

DO


   ! Call predictor-corrector routine:

   CALL Solver


   ! Make sure the rotor azimuth is not greater or equal to 360 degrees:

   IF ( ( Q(DOF_GeAz,IC(1)) + Q(DOF_DrTr,IC(1)) ) >= TwoPi )  THEN
      Q(DOF_GeAz,IC(1)) = Q(DOF_GeAz,IC(1)) - TwoPi
   ENDIF


   ! Advance time:

   Step  = Step + 1
   ZTime = Step*DT


   ! Compute all of the output channels and fill in the OutData() array:

   CALL CalcOuts


   ! Check to see if we should output data this time step:

   IF ( ZTime >= TStart )  THEN
      IF ( CompNoise                 )  CALL PredictNoise
      IF ( MOD( Step, DecFact ) == 0 )  CALL WrOutput
   ENDIF


   ! Display simulation status every SttsTime-seconds:

   IF ( ZTime - TiLstPrn >= SttsTime )  THEN

      TiLstPrn = ZTime

      CALL SimStatus

   ENDIF


   ! If we've reached TMax, exit the DO loop:

   IF ( ZTime > TMax )  EXIT

ENDDO


   ! We're done!


   ! Output noise if desired:

IF ( CompNoise )  CALL WriteAveSpecOut



RETURN
END SUBROUTINE TimeMarch
!=======================================================================
!jmj Start of proposed change.  v6.02a-jmj  25-Aug-2006.
!jmj Add an undocumented feature for modeling the hydrodynamic loading on a
!jmj   monopile.  Do this by reading in addition inputs from the platform file
!jmj   if they exist:
SUBROUTINE TwrLoading ( JNode, X1 , X2 , X3 , X4 , X5 , X6 , &
                               XD1, XD2, XD3, XD4, XD5, XD6    )


   ! This routine computes the tower hydrodynamic loading; that is
   !   TwrAM(1:6,1:6) and TwrFt(1:6).


USE                             General
USE                             FixedBottomSupportStructure, ONLY:MorisonTwrLd
!bjj rm:USE                             Precision
USE                             SimCont
USE                             Tower

IMPLICIT                        NONE


   ! Passed Variables:

REAL(ReKi), INTENT(IN )      :: X1                                              ! The xi-component of the translational    displacement (in m    ) of the current tower node    relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: X2                                              ! The yi-component of the translational    displacement (in m    ) of the current tower node    relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: X3                                              ! The zi-component of the translational    displacement (in m    ) of the current tower node    relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: X4                                              ! The xi-component of the rotational       displacement (in rad  ) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: X5                                              ! The yi-component of the rotational       displacement (in rad  ) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: X6                                              ! The zi-component of the rotational       displacement (in rad  ) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: XD1                                             ! The xi-component of the translational        velocity (in m/s  ) of the current tower node    relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: XD2                                             ! The yi-component of the translational        velocity (in m/s  ) of the current tower node    relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: XD3                                             ! The zi-component of the translational        velocity (in m/s  ) of the current tower node    relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: XD4                                             ! The xi-component of the rotational (angular) velocity (in rad/s) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: XD5                                             ! The yi-component of the rotational (angular) velocity (in rad/s) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi), INTENT(IN )      :: XD6                                             ! The zi-component of the rotational (angular) velocity (in rad/s) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].

INTEGER(4), INTENT(IN )      :: JNode                                           ! The number of the current tower node / element. [1 to TwrNodes]


   ! Local variables:

REAL(ReKi), PARAMETER        :: SymTol   = 9.999E-4                             ! Tolerance used to determine if matrix PtfmAM is symmetric.
REAL(ReKi)                   :: X        (6)                                    ! The 3 components of the translational displacement (in m  ) of the current tower node and the 3 components of the rotational displacement       (in rad  ) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].
REAL(ReKi)                   :: XD       (6)                                    ! The 3 components of the translational velocity     (in m/s) of the current tower node and the 3 components of the rotational (angular) velocity (in rad/s) of the current tower element relative to the inertial frame origin at ground level [onshore] or MSL [offshore].

INTEGER(4)                   :: I                                               ! Loops through all platform DOFs.
INTEGER(4)                   :: J                                               ! Loops through all platform DOFs.



   ! Place the displacement and velocity arguments into the local arrays,
   !   X(1:6) and XD(1:6), respectively:

X (1) = X1
X (2) = X2
X (3) = X3
X (4) = X4
X (5) = X5
X (6) = X6
XD(1) = XD1
XD(2) = XD2
XD(3) = XD3
XD(4) = XD4
XD(5) = XD5
XD(6) = XD6



   ! Compute the tower hydrodynamic loading for the current tower node /
   !   element:

SELECT CASE ( PtfmModel )  ! Which platform model are we using?

CASE ( 0 )                 ! None!


   ! Do nothing here since TwrAM and TwrFt are all initialized to zero.


CASE ( 1 )                 ! Onshore.


   ! Do nothing here since TwrAM and TwrFt are all initialized to zero.


CASE ( 2 )                 ! Fixed bottom offshore.


   SELECT CASE ( TwrLdMod )   ! Which tower loading model are we using?

   CASE ( 0 )                 ! None!


   ! Do nothing here since TwrAM and TwrFt are all initialized to zero.


   CASE ( 1 )                 ! Undocumented hydrodynamic loading using Morison's equation.


   ! CALL the undocumented Morison's equation tower loading model:

      CALL MorisonTwrLd ( JNode, DiamT(JNode), CAT(JNode), CDT(JNode), X, XD, ZTime, TwrAM, TwrFt )


   CASE ( 2 )                 ! User-defined tower loading.


   ! CALL the user-defined tower loading model:

      CALL UserTwrLd ( JNode, X, XD, ZTime, DirRoot, TwrAM, TwrFt )


   ! Ensure that the tower element added mass matrix returned by UserTwrLd,
   !   TwrAM, is symmetric; Abort if necessary:

      DO I = 1,5        ! Loop through the 1st 5 rows (columns) of TwrAM

         DO J = (I+1),6 ! Loop through all columns (rows) passed I

            IF ( ABS( TwrAM(I,J) - TwrAM(J,I) ) > SymTol )  &
               CALL ProgAbort ( ' The user-defined tower element added mass matrix is unsymmetric.'// &
                                '  Make sure TwrAM returned by UserTwrLd() is symmetric.'               )

         ENDDO          ! J - All columns (rows) passed I

      ENDDO             ! I - The 1st 5 rows (columns) of TwrAM


   ENDSELECT


CASE ( 3 )                 ! Floating offshore.


   ! Do nothing here since TwrAM and TwrFt are all initialized to zero.


ENDSELECT



RETURN
END SUBROUTINE TwrLoading
!=======================================================================
!BJJ Start of proposed change vXX NWTC_Lib
END MODULE FASTSubs
!bjj end of proposed change
