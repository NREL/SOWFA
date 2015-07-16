   ! NOTE: This MODULE is virtually a copy of MODULE FFT_Module() from TurbSim.
   !       A few unused things have been eliminated (indicated by "!remove")
   !       and a little bit of code has been added where noted (indicated by
   !       "NEW").
   ! BJJ: 02/22/2008: Updated to work with NWTC_Library v1.01.09
   !      all Abort() functions changed to ProgAbort()
!=======================================================================
MODULE FFT_Module
!-----------------------------------------------------------------------
! DESCRIPTION OF THE INVERSE FOURIER TRANSFORM ROUTINE:
!
! Given an array, H, of N complex numbers, calculate an array, h, of N real
! numbers:
!     h(J) = the sum from K=1,...,N of [ H(K)*exp(i*(J-1)*(K-1)*2*pi/N) ]
!     for J = 1,...,N
!
! where:
!     i=sqrt(-1)
!
! In order for h to be real, the real components of H must be an even function
! of frequency and the imaginary components of H must be an odd function of
! frequency.  Thus, only the first N/2 + 1 values of H are unique.  (The first
! N/2 + 1 are the positive frequencies including zero; the last N/2 - 1 values
! are the negative frequencies.)
!
! We accomplish this by defining a real array, R, where:
!     R(1) = REAL( H(1) ),
!     R(2) = REAL( H(2) ), R(3) = IMAG( H(2) ),
!     R(4) = REAL( H(3) ), R(5) = IMAG( H(3) ),...
!     R(N) = REAL( H(N/2 + 1) ).
! Note that the values of IMAG( H(1) ) and IMAG( H(N/2 + 1) ) must be zero for
! the result to be real, else the routine will abort.
!
! We return the array, r = h, of real numbers as calculated by:
!     h(J) = r(J) = R(1) + (-1)**(J-1)*R(N)
!                 + the sum from K=2,...,N/2 of
!                   [  2*R(2*K-2)*COS((K-1)*(J-1)*2*PI/N)
!                     -2*R(2*K-1)*SIN((K-1)*(J-1)*2*PI/N) ]
!     for J = 1,...,N, where N is an even number
!
! The routine is most effecient when N is a product of small primes.
!
! If the Normalization flag is set to "TRUE" in the initialization, we
! normalize the result by 1/N.
!------------------------------------------------------------------------
! DESCRIPTION OF THE COSINE TRANSFORM ROUTINE:
!
! Given an array, X, of N real numbers, calculate an array, x, of N real
! numbers:
!     x(J) = X(1) + (-1)**(J-1)*X(N)
!          + the sum from K=2,...,N-1 of [ 2*X(K)*COS((K-1)*(J-1)*PI/(N-1)) ]
!     for J = 1,...,N, where N is an odd number
!
! The routine is most effecient when N-1 is a product of small primes.
!
! If the Normalization flag is set to "TRUE" in the initialization, we
! normalize the result by 1/(N-1).
!------------------------------------------------------------------------
! DESCRIPTION OF THE SINE TRANSFORM ROUTINE:
!
! Given an array, X, of N real numbers, calculate an array, x, of N real
! numbers:
!     x(1) = X(1) = 0
!     x(J) = the sum from K=2,...,N-1 of [ 2*X(K)*SIN((K-1)*(J-1)*PI/(N-1)) ]
!     for J = 2,...,N-1, where N is an odd number
!     x(N) = X(N) = 0
!
! Note that the values of X(1) and X(N) must be zero, else the routine will
! abort.
!
! The routine is most effecient when N-1 is a product of small primes.
!
! If the Normalization flag is set to "TRUE" in the initialization, we
! normalize the result by 1/(N-1).
!------------------------------------------------------------------------

!remove   ! This module is NEW:
!remove    USE                                       Precision
! We need the Precision module and the Abort() and
! Int2LStr() functions from the NWTC_IO module.

     USE                                  NWTC_Library


IMPLICIT                                  NONE

REAL(ReKi),              PRIVATE, SAVE :: InvN             ! Normization constant
REAL(ReKi), ALLOCATABLE, PRIVATE, SAVE :: wSave(:)         ! Working array for performing transforms

INTEGER,                 PRIVATE, SAVE :: N                ! Number of steps

LOGICAL,                 PRIVATE, SAVE :: Normalize        ! Whether or not to normalize

!------------------------------------------------------------------------
CONTAINS

   SUBROUTINE ApplyCOST( TRH )
         ! Perform cosine transform.

      IMPLICIT                         NONE

      REAL(ReKi), INTENT(INOUT)     :: TRH(:)



        ! Make sure the array isn't too small

      IF ( SIZE(TRH) < N )  &
          CALL ProgAbort( 'Error in call to cosine transform.  Array size is not large enough.' )

        ! Perform the cosine transform with a FFTpack routine

      CALL COST(N, TRH, wSave) ! FFTpack routine

      IF (Normalize) THEN
          TRH(1:N) = InvN * TRH(1:N)
      ENDIF

   END SUBROUTINE ApplyCOST
  !------------------------------------------------------------------------
   SUBROUTINE ApplyFFT( TRH )
         ! Perform Backward FFT: given TRH, a REAL array representing complex numbers,
         ! return an array TRH, of real numbers.
         !     CALL FOURTH ( TRH, NumSteps, 1, WorkT, NumSteps+2 ) ! Sandia

      IMPLICIT                         NONE

      REAL(ReKi), INTENT(INOUT)     :: TRH(:)



        ! Make sure the array isn't too small

      IF ( SIZE(TRH) < N )  &
          CALL ProgAbort( 'Error in call to FFT.  Array size is not large enough.' )

        ! Perform the FFT with a FFTpack routine

      CALL RFFTB(N, TRH, wSave) ! FFTpack routine

      IF (Normalize) THEN
          TRH(1:N) = InvN * TRH(1:N)
      ENDIF

   END SUBROUTINE ApplyFFT
  !------------------------------------------------------------------------
   SUBROUTINE ApplyFFT_cx( TRH, TRH_complex )
         ! Perform Backward FFT: given TRH_complex, a COMPLEX array,
         ! return an array TRH, of real numbers.

      IMPLICIT                         NONE

      REAL(ReKi),    INTENT(OUT)    :: TRH(:)
      COMPLEX(ReKi), INTENT(IN)     :: TRH_complex(:)

      INTEGER                       :: I
      INTEGER                       :: Indx


        ! Make sure the arrays aren't too small

      IF ( ( SIZE(TRH) < N ) .OR. ( SIZE(TRH_complex) < ( N/2 + 1 ) ) )  &
          CALL ProgAbort( 'Error in call to FFT.  Array size is not large enough.' )

        ! Make sure that the imaginary components at the zeroeth and largest
        ! positive frequency are zero, else abort.

      IF ( AIMAG( TRH_complex(1    ) ) /= 0.0 )  &
          CALL ProgAbort( 'Error in call to FFT.  The imaginary component at the zeroeth frequency must be zero.' )
      IF ( AIMAG( TRH_complex(N/2+1) ) /= 0.0 )  &
          CALL ProgAbort( 'Error in call to FFT.  The imaginary component at the largest positive frequency must be zero.' )

        ! Initialize the TRH array with Complex numbers

      TRH(1) = REAL( TRH_complex(1    ) )

      Indx = 1
      DO I=2,N-2, 2
        Indx     = Indx + 1  ! I/2 + 1

        TRH(I)   =  REAL( TRH_complex(Indx) )
        TRH(I+1) = AIMAG( TRH_complex(Indx) )
      ENDDO

      TRH(N) = REAL( TRH_complex(N/2+1) )


        ! Perform the FFT with a FFTpack routine

      CALL RFFTB(N, TRH, wSave)

      IF (Normalize) THEN
          TRH(1:N) = InvN * TRH(1:N)
      ENDIF


   END SUBROUTINE ApplyFFT_cx
  !------------------------------------------------------------------------
   SUBROUTINE ApplySINT( TRH )
         ! Perform sine transform.

      IMPLICIT                         NONE

      REAL(ReKi), INTENT(INOUT)     :: TRH(:)



        ! Make sure the array isn't too small

      IF ( SIZE(TRH) < N )  &
          CALL ProgAbort( 'Error in call to sine transform.  Array size is not large enough.' )

        ! Make sure that the value at the zeroeth and largest positive
        ! frequency are zero, else abort.

      IF ( TRH(1) /= 0.0 )  &
          CALL ProgAbort( 'Error in call to FFT.  The value at the zeroeth frequency must be zero.' )
      IF ( TRH(N) /= 0.0 )  &
          CALL ProgAbort( 'Error in call to FFT.  The value at the largest positive frequency must be zero.' )

        ! Perform the sine transform with a FFTpack routine

      CALL SINT(N-2, TRH(2:N-1), wSave) ! FFTpack routine

      IF (Normalize) THEN
          TRH(1:N) = InvN * TRH(1:N)
      ENDIF

   END SUBROUTINE ApplySINT
  !------------------------------------------------------------------------
   SUBROUTINE ExitCOST

        ! This subroutine cleans up the cosine transform working space

      IF ( ALLOCATED (wSave)    ) DEALLOCATE( wSave )

   END SUBROUTINE ExitCOST
  !------------------------------------------------------------------------
   SUBROUTINE ExitFFT

        ! This subroutine cleans up the backward FFT working space

      IF ( ALLOCATED (wSave)    ) DEALLOCATE( wSave )

   END SUBROUTINE ExitFFT
  !------------------------------------------------------------------------
   SUBROUTINE ExitSINT

        ! This subroutine cleans up the sine transform working space

      IF ( ALLOCATED (wSave)    ) DEALLOCATE( wSave )

   END SUBROUTINE ExitSINT
  !------------------------------------------------------------------------
   SUBROUTINE InitCOST( NumSteps, NormalizeIn )

        ! This subroutine initializes the cosine transform working space

      IMPLICIT                         NONE

      INTEGER, INTENT(IN)           :: NumSteps       ! Number of steps in the array
      INTEGER                       :: Sttus          ! Array allocation status

      LOGICAL, INTENT(IN), OPTIONAL :: NormalizeIn    ! Whether or not to normalize



        ! Number of timesteps in the time series returned from the cosine transform
        ! N should be odd:

      N  = NumSteps

      IF ( MOD(N,2) /= 1 ) THEN
         CALL ProgAbort ( 'The number of steps in the cosine transform must be odd' )
      ENDIF

        ! Determine if we should normalize the cosine transform:

      IF ( PRESENT( NormalizeIn ) ) THEN
          Normalize = NormalizeIn
          InvN      = 1. / ( N - 1 )
      ELSE
          Normalize = .FALSE.
      ENDIF

        ! According to FFTPACK documentation, the working array must be at
        ! least size 3N+15

      ALLOCATE ( wSave(3*N + 15) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( 'Error allocating memory for the cosine transform working array.' )
      ENDIF


        ! Initialize the FFTPACK working space

      CALL COSTI(N, wSave)


   END SUBROUTINE InitCOST
  !------------------------------------------------------------------------
   SUBROUTINE InitFFT( NumSteps, NormalizeIn )

        ! This subroutine initializes the backward FFT working space

      IMPLICIT                         NONE

      INTEGER, INTENT(IN)           :: NumSteps       ! Number of steps in the array
      INTEGER                       :: Sttus          ! Array allocation status

      LOGICAL, INTENT(IN), OPTIONAL :: NormalizeIn    ! Whether or not to normalize the FFT



        ! Number of timesteps in the time series returned from the backward FFT
        ! N should be even:

      N  = NumSteps

      IF ( MOD(N,2) /= 0 ) THEN
         CALL ProgAbort ( 'The number of steps in the FFT must be even' ) ! For this Real FFT
      ENDIF

        ! Determine if we should normalize the FFT

      IF ( PRESENT( NormalizeIn ) ) THEN
          Normalize = NormalizeIn
          InvN      = 1. / N
      ELSE
          Normalize = .FALSE.
      ENDIF

        ! According to FFTPACK documentation, the working array must be at
        ! least size 2N+15

      ALLOCATE ( wSave(2*N + 15) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( 'Error allocating memory for the FFT working array.' )
      ENDIF


        ! Initialize the FFTPACK working space

      CALL RFFTI(N, wSave)


   END SUBROUTINE InitFFT
  !------------------------------------------------------------------------
   SUBROUTINE InitSINT( NumSteps, NormalizeIn )

        ! This subroutine initializes the sine transform working space

      IMPLICIT                         NONE

      INTEGER, INTENT(IN)           :: NumSteps       ! Number of steps in the array
      INTEGER                       :: Sttus          ! Array allocation status

      LOGICAL, INTENT(IN), OPTIONAL :: NormalizeIn    ! Whether or not to normalize



        ! Number of timesteps in the time series returned from the sine transform
        ! N should be odd:

      N  = NumSteps

      IF ( MOD(N,2) /= 1 ) THEN
         CALL ProgAbort ( 'The number of steps in the sine transform must be odd' )
      ENDIF

        ! Determine if we should normalize the sine transform:

      IF ( PRESENT( NormalizeIn ) ) THEN
          Normalize = NormalizeIn
          InvN      = 1. / ( N - 1 )
      ELSE
          Normalize = .FALSE.
      ENDIF

        ! According to FFTPACK documentation, the working array must be at
        ! least size 2.5N+15; however, our N is +2 greater than their N

      ALLOCATE ( wSave( CEILING( 2.5*(N-2) ) + 15 ) , STAT=Sttus )

      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( 'Error allocating memory for the sine transform working array.' )
      ENDIF


        ! Initialize the FFTPACK working space

      CALL SINTI(N-2, wSave)


   END SUBROUTINE InitSINT
  !------------------------------------------------------------------------
    FUNCTION PSF ( Npsf , NumPrimes )


    ! This routine factors the number N into its primes.  If any of those
    ! prime factors is greater than the NumPrimes'th prime, a point is added to N
    ! and the new number is factored.  This process is repeated until no
    ! prime factors are greater than the NumPrimes'th prime.

    IMPLICIT                 NONE

    !Passed variables
    INTEGER, INTENT(IN)   :: Npsf                   ! Initial number we're trying to factor.
    INTEGER, INTENT(IN)   :: NumPrimes              ! Number of unique primes.
    INTEGER               :: PSF                    ! The smallest number at least as large as Npsf, that is the product of small factors when we return.

    !Other variables
    INTEGER               :: IPR                    ! A counter for the NPrime array
    INTEGER, PARAMETER    :: NFact = 9              ! The number of prime numbers (the first NFact primes)
    INTEGER               :: NP                     ! A temp variable to determine if NPr divides NTR
    INTEGER               :: NPr                    ! A small prime number
    INTEGER               :: NT                     ! A temp variable to determine if NPr divides NTR: INT( NTR / NPr )
    INTEGER               :: NTR                    ! The number we're trying to factor in each iteration
    INTEGER, PARAMETER    :: NPrime(NFact) = (/ 2, 3, 5, 7, 11, 13, 17, 19, 23 /) ! The first 9 prime numbers

    LOGICAL               :: DividesN1(NFact)       ! Does this factor divide NTR-1?

!remove   ! This variable is NEW:
!remove    CHARACTER(11), EXTERNAL :: Int2LStr             ! A function to convert an interger to a left-justified string.

    IF ( NumPrimes > NFact )  THEN
        CALL ProgAbort ( 'In the call to PSF, NumPrimes must be less than '//TRIM( Int2LStr( NFact ) )//'.' )
    ENDIF

    DividesN1(:) = .FALSE.                          ! We need to check all of the primes the first time through

    PSF = Npsf

    DO
        ! 1.0  Factor NTR into its primes.

    NTR = PSF

    DO IPR=1,NumPrimes

        IF ( DividesN1(IPR) ) THEN

                ! If P divides N-1, then P cannot divide N.

            DividesN1(IPR) = .FALSE.               ! We'll check it next time.

        ELSE

            NPr = NPrime(IPR)                      ! The small prime number we will try to find the the factorization of NTR

            DO
                NT = NTR/NPr                       ! Doing some modular arithmetic to see if
                NP = NT*NPr                        ! MOD( NTR, NPr ) == 0, i.e. if NPr divides NTR

                IF ( NP /= NTR )  EXIT             ! There aren't any more of this prime number in the factorization

                NTR = NT                           ! This is the new number we need to get factors for
                DividesN1(IPR) = .TRUE.            ! This prime number divides Npsf, so we won't check it next time (on Npsf+1).

            ENDDO

            IF ( NTR .EQ. 1 )  RETURN              ! We've found all the prime factors, so we're finished

        ENDIF !  DividesN1

    ENDDO ! IPR

        ! 2.0  There is at least one prime larger than NPrime(NumPrimes).  Add
        !      a point to NTR and factor again.

    PSF = PSF + 1

    ENDDO


    RETURN
    END FUNCTION PSF


END MODULE FFT_Module
!=======================================================================
