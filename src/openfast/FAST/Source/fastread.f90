!=======================================================================
SUBROUTINE fastread( u, v, w )

USE Precision
USE Blade
USE Blades
USE UserWind

IMPLICIT                        NONE

! argument variables
REAL(ReKi),        INTENT(IN) :: u(NB*BldNodes)
REAL(ReKi),        INTENT(IN) :: v(NB*BldNodes)
REAL(ReKi),        INTENT(IN) :: w(NB*BldNodes)

CALL UsrWnd_ReadFlow_OpenFOAM(u, v, w)

END SUBROUTINE fastread
!=======================================================================
