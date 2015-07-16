SUBROUTINE fastgetbldforce(bfx, bfy, bfz)

USE  Blade
USE  Blades
USE  AeroElem
USE  RtHndSid
USE  Precision
USE  EnvCond
USE  CoordSys

IMPLICIT                        NONE

! argument variables
REAL(ReKi) bfx(NB*BldNodes)
REAL(ReKi) bfy(NB*BldNodes)
REAL(ReKi) bfz(NB*BldNodes)

! local variables

Integer(4) J, K

!! force data exchange
DO K = 1,NB
  DO J = 1,BldNodes   

   bfx(J + BldNodes*(K-1)) = ( ADAeroMarkers%Blade(J,K)%Orientation(1,1)*ADAeroLoads%Blade(J, K)%Force(1) &
               + ADAeroMarkers%Blade(J,K)%Orientation(2,1)*ADAeroLoads%Blade(J, K)%Force(2) &
               + ADAeroMarkers%Blade(J,K)%Orientation(3,1)*ADAeroLoads%Blade(J, K)%Force(3) )*DRNodes(J)/AirDens

   bfy(J + BldNodes*(K-1)) = ( ADAeroMarkers%Blade(J,K)%Orientation(1,2)*ADAeroLoads%Blade(J, K)%Force(1) &
               + ADAeroMarkers%Blade(J,K)%Orientation(2,2)*ADAeroLoads%Blade(J, K)%Force(2) &
               + ADAeroMarkers%Blade(J,K)%Orientation(3,2)*ADAeroLoads%Blade(J, K)%Force(3) )*DRNodes(J)/AirDens

   bfz(J + BldNodes*(K-1)) = ( ADAeroMarkers%Blade(J,K)%Orientation(1,3)*ADAeroLoads%Blade(J, K)%Force(1) &
               + ADAeroMarkers%Blade(J,K)%Orientation(2,3)*ADAeroLoads%Blade(J, K)%Force(2) &
               + ADAeroMarkers%Blade(J,K)%Orientation(3,3)*ADAeroLoads%Blade(J, K)%Force(3) )*DRNodes(J)/AirDens

  END DO !J = 1,BldNodes ! Loop through the blade nodes / elements
END DO !K = 1,NumBl


!!=========================
! check data communication
!!=========================

!write(*,*) '!!!!!!!!!!!!!!!!!!!!'
!write(*,*) '!! Blade 1 forces !!'
!write(*,*) '!!!!!!!!!!!!!!!!!!!!'
!K=1
!DO J = 1,BldNodes   
!  write(*,*) fx_out(J + BldNodes*(K-1)), fy_out(J + BldNodes*(K-1)), fz_out(J + BldNodes*(K-1))   
!END DO

!write(*,*) '!!!!!!!!!!!!!!!!!!!!'
!write(*,*) '!! Blade 2 forces !!'
!write(*,*) '!!!!!!!!!!!!!!!!!!!!'
!K=2
!DO J = 1,BldNodes   
!  write(*,*) fx_out(J + BldNodes*(K-1)), fy_out(J + BldNodes*(K-1)), fz_out(J + BldNodes*(K-1))   
!END DO

!!=========================
! end check data communication
!!=========================

END SUBROUTINE fastgetbldforce

