SUBROUTINE fastgetbldpos(bldptx, bldpty, bldptz)

USE  Blade
USE  Blades
USE  RtHndSid
USE  Precision

IMPLICIT                        NONE

! argument variables
REAL(ReKi) bldptx(NB*BldNodes)
REAL(ReKi) bldpty(NB*BldNodes)
REAL(ReKi) bldptz(NB*BldNodes)

Integer(4) J, K

!! blade nodal point data exchange
DO K = 1,NB   
  DO J = 1,BldNodes   
    
    bldptx(J + BldNodes*(K-1)) = rS(K,J,1)
    bldpty(J + BldNodes*(K-1)) = -1.0*rS(K,J,3)
    bldptz(J + BldNodes*(K-1)) = rS(K,J,2)

  END DO !J = 1,BldNodes ! Loop through the blade nodes / elements
END DO !K = 1,NumBl

END SUBROUTINE fastgetbldpos
