!=======================================================================
SUBROUTINE SetVersion


   ! This routine sets the version number.  By doing it this way instead
   !   of the old way of initializing it in a module, we will no longer
   !   have to recompile everything every time we change versions.


USE                             General
!bjj rm NWTC_Library: USE                             Precision
!bjj Start of proposed change vXX NWTC_Lib
USE                             NWTC_Library
!bjj End of proposed change vXX NWTC_Lib


IMPLICIT                        NONE


   ! Local Variables:

CHARACTER(6)                 :: Prcsn                                           ! String containing a description of the as-compiled precision.


!bjj Start of proposed change vXX NWTC_Lib
ProgName = 'FAST'
!bjj End of proposed change vXX NWTC_Lib

!bjj Start of proposed change vXX NWTC_Lib
!rm ProgVer = ' (v6.01, 12-Aug-2005)'
ProgVer = '(v7.00.01a-bjj, 5-Nov-2010)'
!bjj End of proposed change vXX NWTC_Lib



IF ( ReKi == 4 )  THEN     ! Single precision
   Prcsn = 'SINGLE'
ELSEIF ( ReKi == 8 )  THEN ! Double precision
   Prcsn = 'DOUBLE'
ELSE                       ! Unknown precision - it should be impossible to compile using a KIND that is not 4 or 8, but I'll put this check here just in case.
   Prcsn = 'UNKNWN'
ENDIF


IF ( Cmpl4SFun )  THEN     ! FAST has been compiled as an S-Function for Simulink
!bjj start of proposed change
!bjj: this requirement has been removed because I don't think it is necessary
!based on limited tests, I get the same results with Single or Double precision;  Double precision is faster, though.
!rm   IF ( ReKi /= 8 )  CALL ProgAbort ( ' FAST must be compiled in double precision for use with Simulink.'//           &
!rm                                  '  Recompile with project option ''/real_size:64'' and variable ReKi set to 8.'   )
!bjj end of change
   ProgVer = TRIM(ProgVer)//'-Compiled as S-Function for Simulink'

ELSEIF( ReKi /= 4 )  THEN  ! Compiled using something other than single precision

   ProgVer = TRIM(ProgVer)//'-Compiled using '//Prcsn//' precision'

ENDIF



RETURN
END SUBROUTINE SetVersion
