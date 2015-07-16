!=======================================================================
SUBROUTINE SetVersion


   ! This routine sets the version number.  By doing it this way instead
   !   of the old way of initializing it in a module, we will no longer
   !   have to recompile everything every time we change versions.


USE                             General
USE                             NWTC_Library

USE                             FloatingPlatform, ONLY: OC3HywindMods

IMPLICIT                        NONE


   ! Local Variables:

CHARACTER(6)                 :: Prcsn                                           ! String containing a description of the as-compiled precision.


ProgName = 'FAST'

ProgVer = '(v7.02.00d-bjj, 20-Feb-2013)'



IF ( ReKi == SiKi )  THEN     ! Single precision
   Prcsn = 'SINGLE'
ELSEIF ( ReKi == R8Ki )  THEN ! Double precision
   Prcsn = 'DOUBLE'
ELSE                          ! Unknown precision
   Prcsn = 'UNKNWN'
ENDIF


IF ( Cmpl4SFun )  THEN     ! FAST has been compiled as an S-Function for Simulink

   ProgVer = TRIM(ProgVer)//'-Compiled as S-Function for Simulink'
   
ELSEIF ( Cmpl4LV )  THEN     ! FAST has been compiled as a DLL for Labview
   
   ProgVer = TRIM(ProgVer)//'-Compiled as a DLL for Labview'
   
ELSEIF( ReKi /= SiKi )  THEN  ! Compiled using something other than single precision

   ProgVer = TRIM(ProgVer)//'-Compiled using '//Prcsn//' precision'

ENDIF



IF ( OC3HywindMods ) THEN
   ProgVer = TRIM(ProgVer)//' with OC3 Hywind Modifications'
END IF



RETURN
END SUBROUTINE SetVersion
