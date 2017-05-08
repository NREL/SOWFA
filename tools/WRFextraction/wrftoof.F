! WRF to OpenFOAM processor
! reads in wrf_output files and outputs OpenFOAM bc.s

PROGRAM toof
   USE module_dm
   USE module_openfoam_bc
   IMPLICIT NONE
     ! domain specification variables
   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme  ! some of this is unnecessary for serial
   INTEGER :: ips , ipe , jps , jpe , kps , kpe  ! ditto
     ! variables for using netcdf
   INCLUDE 'netcdf.inc'
   INTEGER, PARAMETER :: MAXFILES = 20
   CHARACTER(LEN=255) :: flnm(MAXFILES),arg,outnameT,outnamePd,outnameU,outnameHFX,vname,comstr,dirpath
   CHARACTER(LEN=19)  :: Times(100),tmpstr,secstr
   LOGICAL ic, ctrl, have_hfx, use_hfx  ! whether or not to do an IC file too
   INTEGER it,ncid(MAXFILES),stat,iarg,narg,varid,strt(4),cnt(4),xtype,storeddim,dimids(4),natts
   REAL, EXTERNAL :: finterp
   INTEGER, EXTERNAL :: sec_of_day
   LOGICAL, EXTERNAL :: valid_date
   INTEGER  sec, sec_start, sec_offset, nfiles
   REAL    , PARAMETER :: g = 9.81  ! acceleration due to gravity (m {s}^-2)
   DOUBLE PRECISION of_lat, of_lon, of_lz  ! lat and lon of desired openfoam point
   CHARACTER*32, DIMENSION(nbdys) :: bdynames

   REAL, DIMENSION(:,:,:), ALLOCATABLE ::                zz     &   ! height in meters
                                                        ,w      &   ! w at cell center
                                                        ,ph     &   ! geop pert in wrf
                                                        ,phb    &   ! geop base in wrf
                                                        ,pres       ! pressure in millibars
     ! half level WRF variables
   REAL, DIMENSION(:,:,:), ALLOCATABLE ::                z      &   ! height in meters on cell ctrs
                                                        ,p      &   ! pres pert in wrf
                                                        ,pb     &   ! pres base in wrf
                                                        ,t      &   ! temp in K
                                                        ,u_     &   ! staggered u_
                                                        ,v_     &   ! staggered v_
                                                        ,u      &   ! u at cell center
                                                        ,v          ! v at cell center
     ! two-d WRF variables
   REAL, DIMENSION(:,:), ALLOCATABLE :: xlat, xlong , hfx
     ! temporaries
   REAL, DIMENSION(:,:,:), ALLOCATABLE :: zzcol
   REAL, DIMENSION(:,:,:), ALLOCATABLE :: zcol
   INTEGER kz(0:1,0:1), kzz(0:1,0:1), ibdy, ipoint
   REAL :: hfx_new, u_new, v_new, t_new, t_ground, pres_new, pd, w_new, theta, costheta, sintheta, dx
   REAL :: dx_check
   INTEGER :: ids_check , ide_check , jds_check , jde_check , kds_check , kde_check
   INTEGER :: i , j , k 
   INTEGER :: ii, jj, kk

     ! borrowed from NCL for computing T from Theta
   DOUBLE PRECISION PI
   DOUBLE PRECISION P1000MB,R_D,CP, RHO0
   PARAMETER (P1000MB=100000.D0,R_D=266.9D0,CP=7.D0*R_D/2.D0)

   !! Executable

   ic = .FALSE.
   use_hfx = .FALSE.
   it = 1
   sec_offset = 0
   sec_start =  0
   narg = iargc() 

   IF ( narg .EQ. 0 ) THEN
     CALL help
     STOP 99
   ENDIF
   iarg = 1
   nfiles = 0
   DO WHILE ( .TRUE. )
     CALL getarg(iarg,arg)
     IF ( arg(1:1) .EQ. '-' ) THEN
       IF ( TRIM(arg) .EQ. '-startdate' ) THEN
         iarg = iarg + 1 
         CALL getarg(iarg,arg)
         IF ( .NOT. valid_date( arg ) ) THEN
           WRITE(0,*)'Invalid data string in third argument to command: ',TRIM(arg)
           STOP 99
         ENDIF
         sec_start = sec_of_day(arg)
       ELSE IF ( TRIM(arg) .EQ. '-offset' ) THEN
         iarg = iarg + 1 
         CALL getarg(iarg,arg)
         READ(arg,*)sec_offset
       ELSE IF ( TRIM(arg) .EQ. '-ic' ) THEN
         ic = .TRUE.
       ELSE IF ( TRIM(arg) .EQ. '-qwall' ) THEN
         use_hfx = .TRUE.
       ENDIF
     ELSE
       nfiles = nfiles + 1
       IF ( nfiles .GT. MAXFILES ) THEN
          write(0,*)'Too many input files'
          STOP
       ENDIF
       flnm(nfiles) = arg
     ENDIF
     iarg = iarg + 1 
     IF ( iarg .GT. narg ) exit
   ENDDO

   bdynames(BDY_XS) = "west"
   bdynames(BDY_XE) = "east"
   bdynames(BDY_YS) = "south"
   bdynames(BDY_YE) = "north"
   bdynames(BDY_ZS) = "lower"
   bdynames(BDY_ZE) = "upper"
   bdynames(INTERIOR) = "interior"

   DO i = 1, nfiles
     WRITE(0,*)'opening : flnm(i) ',i,TRIM(flnm(i))
     stat = NF_OPEN(flnm(i), NF_NOWRITE, ncid(i))
     CALL ncderrcheck( __LINE__,stat )
     stat=NF_GET_ATT_REAL(ncid(i),NF_GLOBAL,'DX',dx) ; 
     IF( i .EQ. 1 ) dx_check = dx
     CALL ncderrcheck( __LINE__,stat )
     stat = NF_GET_ATT_INT (ncid(i),NF_GLOBAL,'WEST-EAST_PATCH_END_STAG',ide)    ; ids = 1 ; 
     IF( i .EQ. 1 ) ide_check = ide 
     CALL ncderrcheck( __LINE__,stat )
     stat = NF_GET_ATT_INT (ncid(i),NF_GLOBAL,'SOUTH-NORTH_PATCH_END_STAG',jde)  ; jds = 1 ; 
     IF( i .EQ. 1 ) jde_check = jde 
     CALL ncderrcheck( __LINE__,stat )
     stat = NF_GET_ATT_INT (ncid(i),NF_GLOBAL,'BOTTOM-TOP_PATCH_END_STAG',kde)   ; kds = 1 ; 
     IF( i .EQ. 1 ) kde_check = kde
     CALL ncderrcheck( __LINE__,stat )
     IF ( i .GT. 1 ) THEN
       stat = 0
       IF ( dx .NE. dx_check ) THEN
         stat = 1 ; write(0,*)'DX ',TRIM(flnm(i)),' does not match ',TRIM(flnm(i))
       ENDIF
       IF ( ide .NE. ide_check ) THEN
         stat = 1 ; write(0,*)'WEST-EAST_PATCH_END_STAG in ',TRIM(flnm(i)),' does not match ',TRIM(flnm(i))
       ENDIF
       IF ( jde .NE. jde_check ) THEN
         stat = 1 ; write(0,*)'SOUTH-NORTH_PATCH_END_STAG in ',TRIM(flnm(i)),' does not match ',TRIM(flnm(i))
       ENDIF
       IF ( kde .NE. kde_check ) THEN
         stat = 1 ; write(0,*)'BOTTOM-TOP_PATCH_END_STAG in ',TRIM(flnm(i)),' does not match ',TRIM(flnm(i))
       ENDIF
       IF ( stat .NE. 0 ) STOP
     ENDIF
   ENDDO

   strt = 1
   cnt(1) = 19

   stat = NF_INQ_VARID(ncid,'Times',varid) 
   CALL ncderrcheck(__LINE__,stat)
   stat = NF_INQ_VAR(ncid,varid,vname,xtype,storeddim,dimids,natts)
   CALL ncderrcheck(__LINE__,stat)
   stat = NF_INQ_DIMLEN(ncid,dimids(1),cnt(1))
   CALL ncderrcheck(__LINE__,stat)
   stat = NF_INQ_DIMLEN(ncid,dimids(2),cnt(2))
   CALL ncderrcheck(__LINE__,stat)
   stat = NF_GET_VARA_TEXT(ncid,varid,strt,cnt,Times)
   CALL ncderrcheck( __LINE__,stat )
   DO WHILE (.TRUE.)
     tmpstr = Times(it)
     i = INDEX(Times(it),':')
     IF ( i .EQ. 0 ) EXIT
     Times(it)(i:i) = '_'
   ENDDO

   ips = ids ; ipe = ide
   jps = jds ; jpe = jde
   kps = kds ; kpe = kde
   ims = ids ; ime = ide
   jms = jds ; jme = jde
   kms = kds ; kme = kde

   ALLOCATE(   xlat(ips:ipe-1,jps:jpe-1))
   ALLOCATE(  xlong(ips:ipe-1,jps:jpe-1))
   ctrl = .TRUE.  ! true value going in says this field is required
   CALL getvar_real(ctrl,ncid,nfiles,'XLAT' ,xlat ,it,2,ips,ipe-1,jps,jpe-1,1,1)
   CALL getvar_real(ctrl,ncid,nfiles,'XLONG',xlong,it,2,ips,ipe-1,jps,jpe-1,1,1)

   theta = rotation_angle ( xlat,dx,ids,ide,jds,jde,ips,ipe,jps,jpe,ims,ime,jms,jme )
   ! Computed theta is counterclockwise rotation in radians of the vector from X axis, so negate and
   ! convert to degrees for reporting rotation with respect to compass points
   write(*,'("WRF grid is clockwise rotated approx.",f9.5," deg. from true lat/lon. Compensating.")'),-theta*57.2957795
   costheta = cos(theta)
   sintheta = sin(theta)

   ALLOCATE(    p(ips:ipe-1,jps:jpe-1,kps:kpe-1))
   ALLOCATE(   pb(ips:ipe-1,jps:jpe-1,kps:kpe-1))
   ALLOCATE(   ph(ips:ipe-1,jps:jpe-1,kps:kpe  ))
   ALLOCATE(  phb(ips:ipe-1,jps:jpe-1,kps:kpe  ))
   ALLOCATE(   zz(ips:ipe-1,jps:jpe-1,kps:kpe  ))
   ALLOCATE(    w(ips:ipe-1,jps:jpe-1,kps:kpe  ))
   ALLOCATE( pres(ips:ipe-1,jps:jpe-1,kps:kpe-1))
   ALLOCATE(    z(ips:ipe-1,jps:jpe-1,kps:kpe-1))
   ALLOCATE(    t(ips:ipe-1,jps:jpe-1,kps:kpe-1))
   ALLOCATE(    u(ips:ipe-1,jps:jpe-1,kps:kpe-1))
   ALLOCATE(    v(ips:ipe-1,jps:jpe-1,kps:kpe-1))
   ALLOCATE(   u_(ips:ipe  ,jps:jpe-1,kps:kpe-1))
   ALLOCATE(   v_(ips:ipe-1,jps:jpe  ,kps:kpe-1))
   ALLOCATE(zzcol(0:1,0:1,kps:kpe  ))
   ALLOCATE( zcol(0:1,0:1,kps:kpe-1))
   ALLOCATE(   hfx(ips:ipe-1,jps:jpe-1))

   p = 0.
   pb = 0.
   ph = 0.
   phb = 0.
   zz  = 0.
   w  = 0.
   pres  = 0.
   z  = 0.
   t  = 0.
   u  = 0.
   v  = 0.
   u_  = 0.
   v_  = 0.
   zzcol = 0.
   zcol = 0.
   hfx = 0.

   CALL getvar_real(ctrl,ncid,nfiles,'PH' ,ph ,it,3,ips,ipe-1,jps,jpe-1,kps,kpe  )
   CALL getvar_real(ctrl,ncid,nfiles,'PHB',phb,it,3,ips,ipe-1,jps,jpe-1,kps,kpe  )
   CALL getvar_real(ctrl,ncid,nfiles,'W'  ,w  ,it,3,ips,ipe-1,jps,jpe-1,kps,kpe  )
   CALL getvar_real(ctrl,ncid,nfiles,'T'  ,t  ,it,3,ips,ipe-1,jps,jpe-1,kps,kpe-1)
   CALL getvar_real(ctrl,ncid,nfiles,'U'  ,u_ ,it,3,ips,ipe  ,jps,jpe-1,kps,kpe-1)
   CALL getvar_real(ctrl,ncid,nfiles,'V'  ,v_ ,it,3,ips,ipe-1,jps,jpe  ,kps,kpe-1)
   CALL getvar_real(ctrl,ncid,nfiles,'P'  ,p  ,it,3,ips,ipe-1,jps,jpe-1,kps,kpe-1)
   CALL getvar_real(ctrl,ncid,nfiles,'PB' ,pb ,it,3,ips,ipe-1,jps,jpe-1,kps,kpe-1)


   have_hfx = .FALSE.  ! false value going in says this field is required
   CALL getvar_real(have_hfx,ncid,nfiles,'HFX',hfx,it,3,ips,ipe-1,jps,jpe-1,1,1)

   zz   = (ph + phb )/g
   z    = (zz(:,:,kps:kpe-1) + zz(:,:,kps+1:kpe))*0.5
   u    = (u_(ips:ipe-1,:,:)+u_(ips+1:ipe,:,:))*0.5
   v    = (v_(:,jps:jpe-1,:)+v_(:,jps+1:jpe,:))*0.5

   pres = p + pb

   t    = t+300.

   DEALLOCATE(ph)
   DEALLOCATE(phb)
   DEALLOCATE(u_)
   DEALLOCATE(v_)

   DO ibdy = 1,nbdys
     IF ( ibdy .EQ. INTERIOR .AND. .NOT. ic ) cycle  ! short circuit if we do not want to generate interior file
     IF ( ibdy .NE. INTERIOR ) THEN
       CALL read_openfoam_bdy_coords(ibdy,TRIM(bdynames(ibdy))//'_bc.dat')
     ELSE
       CALL read_openfoam_bdy_coords(ibdy,TRIM(bdynames(ibdy))//'.dat')
     ENDIF
     IF ( ALLOCATED(bdy(ibdy)%point) ) THEN
       CALL precompute_openfoam_points(ibdy,xlat,xlong,ids,ide,jds,jde,ips,ipe,jps,jpe,ims,ime,jms,jme )
       sec = sec_of_day(TRIM(Times(it)))
       sec = sec - sec_start + sec_offset
       ! create the time directory if it doesn.t already exist
       IF ( ibdy .NE. INTERIOR ) THEN
         IF ( sec > 999999 ) THEN
           WRITE(0,*)sec,' is too many seconds from start.'
           WRITE(0,*)'Use -offset argument to make this a six digit number.'
           CALL help
           STOP 99
         ENDIF
         WRITE(secstr,'(I6.1)')sec
         dirpath = TRIM(bdynames(ibdy))//"/"
       ELSE
         WRITE(secstr,'(I6.1)')sec
         dirpath = ""
       ENDIF

       comstr = "mkdir -p " // TRIM(dirpath)//TRIM(ADJUSTL(secstr))
       CALL system (TRIM(comstr))

       outnameT = TRIM(dirpath)//TRIM(ADJUSTL(secstr))//"/T"
       OPEN( 76 , file=TRIM(outnameT), form="formatted" )
       CALL write_header_scalar( 76, bdy(ibdy)%npoints , ibdy.NE.INTERIOR , sec , "T"  )

       outnamePd = TRIM(dirpath)//TRIM(ADJUSTL(secstr))//"/p_rgh"
       OPEN( 77 , file=TRIM(outnamePd), form="formatted" )
       CALL write_header_scalar( 77, bdy(ibdy)%npoints , ibdy.NE.INTERIOR , sec , "p_rgh" )

       outnameU = TRIM(dirpath)//TRIM(ADJUSTL(secstr))//"/U"
       OPEN( 78 , file=TRIM(outnameU), form="formatted" )
       CALL write_header_vector( 78, bdy(ibdy)%npoints , ibdy.NE.INTERIOR , sec , "U" )

       IF ( ibdy .EQ. BDY_ZS .AND. have_hfx .AND. use_hfx ) THEN
         outnameHFX = TRIM(dirpath)//TRIM(ADJUSTL(secstr))//"/qwall"
         OPEN( 79 , file=TRIM(outnameHFX), form="formatted" )
         CALL write_header_vector( 79, bdy(ibdy)%npoints , ibdy.NE.INTERIOR , sec , "qwall" )
       ENDIF

       DO ipoint = 1,bdy(ibdy)%npoints
         of_lat = bdy(ibdy)%point(ipoint)%lat
         of_lon = bdy(ibdy)%point(ipoint)%lon
         of_lz = bdy(ibdy)%point(ipoint)%lz
         j = bdy(ibdy)%point(ipoint)%j   ! precomputed jcoord of cell center corresponding to lat
         i = bdy(ibdy)%point(ipoint)%i   ! precomputed icoord of cell center corresponding to lon

         DO kk = 1,size(zz,3)
           DO jj = 0,1
             DO ii = 0,1
               zzcol(ii,jj,kk)=zz(i+ii,j+jj,kk) - zz(i+ii,j+jj,1)   ! zz is full height at cell centers
               IF ( kk .LE. kpe-1 ) THEN
                 zcol (ii,jj,kk)= z(i+ii,j+jj,kk) - zz(i+ii,j+jj,1)   ! z  is half hieght at cell centers
               ENDIF
             ENDDO
           ENDDO
         ENDDO

          ! find the level index of the openfoam point in WRF, both in the full-level
          ! and half-level ranges.  Lowest index is closest to surface.  Also store the
          ! indices for the 3 neighbors to the north, east, and northeast, since these
          ! are needed for horizontally interpolating in the finterp function
         DO jj = 0,1
           DO ii = 0,1
             IF (zzcol(ii,jj,1).LE.of_lz.AND.of_lz.LT.zcol(ii,jj,1))THEN  ! special case, of_lz is below first half-level
               kzz(ii,jj) = 1                                             ! ignore other special case since open foam won.t go that high
               kz(ii,jj) = 0
             ELSE
               DO k = kps+1,kpe
                 IF (zzcol(ii,jj,k-1).LE.of_lz.AND.of_lz.LT.zzcol(ii,jj,k)) kzz(ii,jj) = k-1              ! full level
                 IF (k.LT.kpe) THEN
                   IF (zcol(ii,jj,k-1).LE.of_lz.AND.of_lz.LT.zcol(ii,jj,k)) kz(ii,jj) = k-1    ! half level
                 ENDIF
               ENDDO
             ENDIF
           ENDDO
         ENDDO
                                                                         
         !variables on half-levels                    open foam coords        dims of field                  dims of lat lon arrays
         u_new    = finterp(u   ,zcol ,xlat,xlong,kz ,of_lat,of_lon,of_lz,i,j,ips,ipe-1,jps,jpe-1,kps,kpe-1, ips,ipe-1,jps,jpe-1)
         v_new    = finterp(v   ,zcol ,xlat,xlong,kz ,of_lat,of_lon,of_lz,i,j,ips,ipe-1,jps,jpe-1,kps,kpe-1, ips,ipe-1,jps,jpe-1)
         t_new    = finterp(t   ,zcol ,xlat,xlong,kz ,of_lat,of_lon,of_lz,i,j,ips,ipe-1,jps,jpe-1,kps,kpe-1, ips,ipe-1,jps,jpe-1)
         pres_new = finterp(pres,zzcol,xlat,xlong,kzz,of_lat,of_lon,of_lz,i,j,ips,ipe-1,jps,jpe-1,kps,kpe-1, ips,ipe-1,jps,jpe-1)

         !variables on full-levels                    open foam coords        dims of field                  dims of lat lon arrays
         w_new    = finterp(w   ,zzcol,xlat,xlong,kzz,of_lat,of_lon,of_lz,i,j,ips,ipe-1,jps,jpe-1,kps,kpe  , ips,ipe-1,jps,jpe-1)

#if 0
! turns out we actually want theta for OpenFOAM, not absolute temperature. So skip this conversion. JM 20110117
           ! convert theta (potential temperature) in WRF to temperature at level of variable and
           ! also at ground level (for use below in computing pd)
           ! formula requires pressure to be in pascals: 1 pascal = 0.01 millibars
         t_new    =  t_new  * ( (pres_new / P1000MB)**(R_D/CP))
         t_ground =  t(i,j,1) * ( (pres(i,j,1) / P1000MB)**(R_D/CP))
#else
         t_ground =  t(i,j,1)
#endif

           ! compute "pd" which is defined as pressure divided by density at surface minus geopotential
           ! that is, pd = p / rho - g*z .  Note, however, that we don.t have density so compute density at
           ! surface as rho0 = p0 / (R*T0), where R is 286.9 and T0 is surface temp. Substituting for rho
           ! into the above, this becomes:
         pd = (pres_new*R_D*t_ground)/pres(i,j,1) - g * of_lz

         IF ( ibdy .EQ. BDY_ZS .AND. have_hfx .AND. use_hfx ) THEN
           kzz = 0 ! turn off vertical interpolation in call to finterp
           hfx_new = finterp(hfx,zzcol,xlat,xlong,kzz,of_lat,of_lon,of_lz,i,j,ips,ipe-1,jps,jpe-1,1,1, ips,ipe-1,jps,jpe-1)
           rho0 = pres(i,j,1) / ( R_D * t_ground ) 
           hfx_new = -( hfx_new / ( rho0 * CP ) )
         ENDIF

         WRITE(76,*) t_new 
         WRITE(77,*) pd
         ! note that positive theta angle (computed above) implies WRF grid is rotated counterclockwise w.r.t. true
         ! see http://en.wikipedia.org/wiki/Rotation_matrix
         WRITE(78,'("(",f12.7," ",f12.7," ",f12.7,")")')u_new*costheta-v_new*sintheta,u_new*sintheta+v_new*costheta,w_new
         IF ( ibdy .EQ. BDY_ZS .AND. have_hfx .AND. use_hfx ) THEN
           WRITE(79,'("(",f12.7," ",f12.7," ",f12.7,")")')0., 0., hfx_new
         ENDIF

       ENDDO
        ! close the unit and then run a system command to strip off the first space that Fortran insists on writing
       CALL write_trailer_T(76,ibdy.EQ.INTERIOR) ; CLOSE( 76 ) ; comstr = "sed 's/^ //' "//TRIM(outnameT) //"> foo_ ; /bin/mv -f foo_ "// TRIM(outnameT)
       CALL system(TRIM(comstr)) 
       CALL write_trailer_pd(77,ibdy.EQ.INTERIOR) ; CLOSE( 77 ) ; comstr = "sed 's/^ //' "//TRIM(outnamePd) //"> foo_ ; /bin/mv -f foo_ "// TRIM(outnamePd)
       CALL system(TRIM(comstr)) 
       CALL write_trailer_U(78,ibdy.EQ.INTERIOR) ;  CLOSE( 78 ) ; comstr = "sed 's/^ //' "//TRIM(outnameU) //"> foo_ ; /bin/mv -f foo_ "// TRIM(outnameU)
       CALL system(TRIM(comstr)) 
       IF ( ibdy .EQ. BDY_ZS .AND. have_hfx .AND. use_hfx ) THEN
         CALL write_trailer_HFX(79,.FALSE.) ;  CLOSE( 79 ) ; comstr = "sed 's/^ //' "//TRIM(outnameU) //"> foo_ ; /bin/mv -f foo_ "// TRIM(outnameU)
         CALL system(TRIM(comstr)) 
       ENDIF
     ENDIF
   ENDDO

END PROGRAM  toof

REAL FUNCTION finterp( f, zcol, lat, lon, kz, of_lat, of_lon, of_lz, i, j, is,ie,js,je,ks,ke,ims,ime,jms,jme )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: i,j,is,ie,js,je,ks,ke,ims,ime,jms,jme
   INTEGER, INTENT(IN) :: kz(0:1,0:1)
   REAL, INTENT(IN) :: f(is:ie,js:je,ks:ke), zcol(0:1,0:1,ks:ke)
   DOUBLE PRECISION, INTENT(IN) :: of_lat, of_lon, of_lz
   REAL, INTENT(IN) :: lat(ims:ime,jms:jme),lon(ims:ime,jms:jme)
   ! local
   INTEGER k
   REAL f00,f10,f01,f11,rm
   k = kz(0,0)
   IF ( k .GE. 1 ) THEN
     f00  = f(i  ,j  ,k) + (of_lz-zcol(0  ,0  ,k))*(f(i  ,j  ,k+1)-f(i  ,j  ,k))/(zcol(0  ,0  ,k+1)-zcol(0  ,0  ,k))
   ELSE
     f00  = f(i  ,j  ,1)
   ENDIF
   k = kz(1,0)
   IF ( k .GE. 1 ) THEN
     f10  = f(i+1,j  ,k) + (of_lz-zcol(0+1,0  ,k))*(f(i+1,j  ,k+1)-f(i+1,j  ,k))/(zcol(0+1,0  ,k+1)-zcol(0+1,0  ,k))
   ELSE
     f10  = f(i+1,j  ,1)
   ENDIF
   k = kz(0,1)
   IF ( k .GE. 1 ) THEN
     f01  = f(i  ,j+1,k) + (of_lz-zcol(0  ,0+1,k))*(f(i  ,j+1,k+1)-f(i  ,j+1,k))/(zcol(0  ,0+1,k+1)-zcol(0  ,0+1,k))
   ELSE
     f01  = f(i  ,j+1,1)
   ENDIF
   k = kz(1,1)
   IF ( k .GE. 1 ) THEN
     f11  = f(i+1,j+1,k) + (of_lz-zcol(0+1,0+1,k))*(f(i+1,j+1,k+1)-f(i+1,j+1,k))/(zcol(0+1,0+1,k+1)-zcol(0+1,0+1,k))
   ELSE
     f11  = f(i+1,j+1,1)
   ENDIF
   !
   rm = 1.0/((lon(i+1,j)-lon(i,j))*(lat(i,j+1)-lat(i,j)))
   finterp = f00*rm*(lon(i+1,j)-of_lon  )*(lat(i,j+1)-of_lat  ) + &
             f10*rm*(of_lon    -lon(i,j))*(lat(i,j+1)-of_lat  ) + &
             f01*rm*(lon(i+1,j)-of_lon  )*(of_lat    -lat(i,j)) + &
             f11*rm*(of_lon    -lon(i,j))*(of_lat    -lat(i,j))
   RETURN
END FUNCTION finterp

SUBROUTINE help
   IMPLICIT NONE
   CHARACTER(LEN=120) :: cmd
   CALL getarg(0, cmd)
   WRITE(*,'(/,"Usage: ", A, " ncdfile [ncdfiles*] [-startdate startdate [-offset offset]] [-ic]")') trim(cmd)
   WRITE(*,'("       startdate   date string of form yyyy-mm-dd_hh_mm_ss or yyyy-mm-dd_hh:mm:ss")')
   WRITE(*,'("       offset      number of seconds to start OpenFOAM directory naming (default 0)")')
   WRITE(*,'("       -ic         program should generate init conditions too")')
   WRITE(*,'("       -qwall      program should generate temp flux in lower bc file ",/)')
   STOP
END SUBROUTINE help

SUBROUTINE ncderrcheck( lineno, stat )
   IMPLICIT NONE
   INCLUDE 'netcdf.inc'
   INTEGER, INTENT(IN) :: lineno,stat
   IF ( stat .NE. NF_NOERR ) THEN
     WRITE(0,*)'Line ',lineno,NF_STRERROR(stat) 
     STOP 99
   ENDIF
END SUBROUTINE ncderrcheck

SUBROUTINE getvar_real(ctrl,ncids,numfiles,vname,buf,itime,ndim,ids,ide,jds,jde,kds,kde)
   IMPLICIT NONE
   INCLUDE 'netcdf.inc'
   INTEGER, INTENT(IN), DIMENSION(*) :: ncids
   INTEGER, INTENT(IN) :: numfiles
   LOGICAL, INTENT(INOUT):: ctrl
   REAL, INTENT(INOUT) :: buf(*)
   CHARACTER*(*), INTENT(IN) :: vname
   INTEGER, INTENT(IN) :: itime,ndim,ids,ide,jds,jde,kds,kde
   INTEGER strt(4),cnt(4)
   INTEGER stat,varid, i,ncid
   LOGICAL found
!
   found = .FALSE.
   DO i = 1,numfiles
     ncid = ncids(i)
     IF ( ncid .GT. 0 .AND. .NOT. found ) THEN
       stat = NF_INQ_VARID(ncid,vname,varid) 
       IF ( stat .EQ. 0 ) THEN
         strt = 1
         IF ( ndim .EQ. 3 ) THEN
           cnt(1) = ide-ids+1
           cnt(2) = jde-jds+1
           cnt(3) = kde-kds+1
           cnt(4) = itime
         ELSE
           cnt(1) = ide-ids+1
           cnt(2) = jde-jds+1
           cnt(3) = itime 
         ENDIF
         stat = NF_GET_VARA_REAL(ncid,varid,strt,cnt,buf)
         IF ( stat .EQ. 0 ) found = .TRUE.
       ENDIF
     ENDIF
   ENDDO
   IF ( .NOT. found .AND. ctrl ) THEN
     WRITE(0,*)'getvar_real: did not find ',TRIM(vname),' in any input file'
     STOP 99
   ENDIF
   ctrl = found
   RETURN
END SUBROUTINE getvar_real

SUBROUTINE write_header_scalar( unit, n, bc , location, objectname )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: unit, n
   LOGICAL, INTENT(IN) :: bc   ! is this a boundary file or interior
   INTEGER,  INTENT(IN) :: location
   CHARACTER*(*),  INTENT(IN) :: objectname

   WRITE(unit,*)'/*--------------------------------*- C++ -*----------------------------------*\'
   WRITE(unit,*)'| =========                 |                                                 |'
   WRITE(unit,*)'| \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox           |'
   WRITE(unit,*)'|  \\    /   O peration     | Version:  1.6                                   |'
   WRITE(unit,*)'|   \\  /    A nd           | Web:      http://www.OpenFOAM.org               |'
   WRITE(unit,*)'|    \\/     M anipulation  |                                                 |'
   WRITE(unit,*)'\*---------------------------------------------------------------------------*/'
   WRITE(unit,*)'FoamFile'
   WRITE(unit,*)'{'
   WRITE(unit,*)'    version     2.0;'
   WRITE(unit,*)'    format      ascii;'
   IF ( bc ) THEN
     WRITE(unit,*)'    class       scalarAverageField;'
     WRITE(unit,*)'    object      values;'
   ELSE
     WRITE(unit,*)'    class       volScalarField;'
     WRITE(unit,'("    location    """,i4.4,""";")')location
     WRITE(unit,*)'    object      ',TRIM(objectname),' ;'
   ENDIF
   WRITE(unit,*)'}'
   WRITE(unit,*)'// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //'
   WRITE(unit,*)' '
   IF ( bc ) THEN
     WRITE(unit,*)'// Average'
     IF      ( TRIM(objectname).EQ.'U' ) THEN
       WRITE(unit,*)'(0 0 0)'
     ELSE
       WRITE(unit,*)'0'
     ENDIF
     WRITE(unit,*)' '
     WRITE(unit,*)' '
   ELSE
     IF      ( TRIM(objectname).EQ.'p_rgh' ) THEN
       WRITE(unit,*)'dimensions      [0 2 -2 0 0 0 0];'  ! And this means m^2/s^2 !
     ELSE IF ( TRIM(objectname).EQ.'T' ) THEN
       WRITE(unit,*)'dimensions      [0 0 0 1 0 0 0];'   ! This means degrees Kelvin !
     ELSE
       WRITE(0,*)'write_header_scalar does not know about this variable: ',TRIM(objectname)
       STOP 122
     ENDIF
     WRITE(unit,*)' '
     WRITE(unit,*)'internalField   nonuniform List<scalar>'
   ENDIF
   WRITE(unit,'(I10)')n
   WRITE(unit,*)'('
END SUBROUTINE write_header_scalar

SUBROUTINE write_header_vector( unit, n, bc, location, objectname )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: unit, n
   LOGICAL, INTENT(IN) :: bc   ! boundary file or interior
   INTEGER,  INTENT(IN) :: location
   CHARACTER*(*),  INTENT(IN) :: objectname

   WRITE(unit,*)'/*--------------------------------*- C++ -*----------------------------------*\'
   WRITE(unit,*)'| =========                 |                                                 |'
   WRITE(unit,*)'| \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox           |'
   WRITE(unit,*)'|  \\    /   O peration     | Version:  1.6                                   |'
   WRITE(unit,*)'|   \\  /    A nd           | Web:      http://www.OpenFOAM.org               |'
   WRITE(unit,*)'|    \\/     M anipulation  |                                                 |'
   WRITE(unit,*)'\*---------------------------------------------------------------------------*/'
   WRITE(unit,*)'FoamFile'
   WRITE(unit,*)'{'
   WRITE(unit,*)'    version     2.0;'
   WRITE(unit,*)'    format      ascii;'
   IF ( bc ) THEN
     WRITE(unit,*)'    class       vectorAverageField;'
     WRITE(unit,*)'    object      values;'
   ELSE
     WRITE(unit,*)'    class       volVectorField;'
     WRITE(unit,'("    location    """,i4.4,""";")')location
     WRITE(unit,*)'    object      ',TRIM(objectname),';'
   ENDIF
   WRITE(unit,*)'}'
   WRITE(unit,*)'// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //'
   WRITE(unit,*)' '
   IF ( bc ) THEN
     WRITE(unit,*)'// Average'
     WRITE(unit,*)'(0 0 0)'
     WRITE(unit,*)' '
     WRITE(unit,*)' '
   ELSE
     IF      ( TRIM(objectname).EQ.'U' ) THEN
       WRITE(unit,*)'dimensions      [0 1 -1 0 0 0 0];'     ! this means m/s
     ELSE IF ( TRIM(objectname).EQ.'qwall' ) THEN
       WRITE(unit,*)'dimensions      [0 1 -1 1 0 0 0];'     ! this means ?
     ENDIF
     WRITE(unit,*)' '
     WRITE(unit,*)'internalField   nonuniform List<vector>'
   ENDIF
   WRITE(unit,'(I10)')n
   WRITE(unit,*)'('
END SUBROUTINE write_header_vector

SUBROUTINE write_trailer_U ( unit, interior )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: unit
   LOGICAL, INTENT(IN) :: interior
   WRITE(unit,*)')'
   WRITE(unit,*)';'
   IF ( interior ) THEN
     WRITE(unit,*)
     WRITE(unit,*)'boundaryField'
     WRITE(unit,*)'{'
     WRITE(unit,*)'    lower'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            velocityABLWallFunction;'
     WRITE(unit,*)'        print           1;'
     WRITE(unit,*)'        U               U;'
     WRITE(unit,*)'        value           uniform (0 0 0);'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'    upper'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            timeVaryingMappedFixedValue;'
     WRITE(unit,*)'        setAverage      0;'
     WRITE(unit,*)'        offset          (0 0 0);'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'    west'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            timeVaryingMappedFixedValue;'
     WRITE(unit,*)'        setAverage      0;'
     WRITE(unit,*)'        offset          (0 0 0);'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'    east'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            timeVaryingMappedFixedValue;'
     WRITE(unit,*)'        setAverage      0;'
     WRITE(unit,*)'        offset          (0 0 0);'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'    south'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            timeVaryingMappedFixedValue;'
     WRITE(unit,*)'        setAverage      0;'
     WRITE(unit,*)'        offset          (0 0 0);'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'    north'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            timeVaryingMappedFixedValue;'
     WRITE(unit,*)'        setAverage      0;'
     WRITE(unit,*)'        offset          (0 0 0);'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'}'
     WRITE(unit,*)
     WRITE(unit,*)
   ENDIF
   WRITE(unit,*)
   WRITE(unit,*)'// ************************************************************************* //'
   RETURN
END SUBROUTINE write_trailer_U

SUBROUTINE write_trailer_T ( unit, interior )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: unit
   LOGICAL, INTENT(IN) :: interior
   WRITE(unit,*)')'
   WRITE(unit,*)';'
   WRITE(unit,*)
   IF ( interior ) THEN
     WRITE(unit,*)'boundaryField'
     WRITE(unit,*)'{'
     WRITE(unit,*)'    lower'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            zeroGradient;'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'    upper'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            timeVaryingMappedFixedValue;'
     WRITE(unit,*)'        setAverage      0;'
     WRITE(unit,*)'        offset          0;'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'    west'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            timeVaryingMappedFixedValue;'
     WRITE(unit,*)'        setAverage      0;'
     WRITE(unit,*)'        offset          0;'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'    east'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            timeVaryingMappedFixedValue;'
     WRITE(unit,*)'        setAverage      0;'
     WRITE(unit,*)'        offset          0;'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'    south'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            timeVaryingMappedFixedValue;'
     WRITE(unit,*)'        setAverage      0;'
     WRITE(unit,*)'        offset          0;'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'    north'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            timeVaryingMappedFixedValue;'
     WRITE(unit,*)'        setAverage      0;'
     WRITE(unit,*)'        offset          0;'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'}'
     WRITE(unit,*)
   ENDIF
   WRITE(unit,*)
   WRITE(unit,*)'// ************************************************************************* //'
   RETURN
END SUBROUTINE write_trailer_T

SUBROUTINE write_trailer_pd( unit, interior )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: unit
   LOGICAL, INTENT(IN) :: interior
   WRITE(unit,*)')'
   WRITE(unit,*)';'
   WRITE(unit,*)
   IF ( interior ) THEN
     WRITE(unit,*)'boundaryField'
     WRITE(unit,*)'{'
     WRITE(unit,*)'    lower'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            buoyantPressureMod;'
     WRITE(unit,*)'        rho             rhok;'
     WRITE(unit,*)'        value           uniform 0;'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'    upper'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            buoyantPressureMod;'
     WRITE(unit,*)'        rho             rhok;'
     WRITE(unit,*)'        value           uniform 0;'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'    east'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            buoyantPressureMod;'
     WRITE(unit,*)'        rho             rhok;'
     WRITE(unit,*)'        value           uniform 0;'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'    west'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            buoyantPressureMod;'
     WRITE(unit,*)'        rho             rhok;'
     WRITE(unit,*)'        value           uniform 0;'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'    south'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            buoyantPressureMod;'
     WRITE(unit,*)'        rho             rhok;'
     WRITE(unit,*)'        value           uniform 0;'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'    north'
     WRITE(unit,*)'    {'
     WRITE(unit,*)'        type            buoyantPressureMod;'
     WRITE(unit,*)'        rho             rhok;'
     WRITE(unit,*)'        value           uniform 0;'
     WRITE(unit,*)'    }'
     WRITE(unit,*)'}'
     WRITE(unit,*)
   ENDIF
   WRITE(unit,*)
   WRITE(unit,*)'// ************************************************************************* //'
   RETURN
END SUBROUTINE write_trailer_pd

SUBROUTINE write_trailer_HFX ( unit, interior )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: unit
   LOGICAL, INTENT(IN) :: interior
   WRITE(unit,*)')'
   WRITE(unit,*)';'
   WRITE(unit,*)
   WRITE(unit,*)'// ************************************************************************* //'
   RETURN
END SUBROUTINE write_trailer_HFX

! WARNING OVERLY SIMPLE -- assumes same day! and assumes 19 char WRF style date str
INTEGER FUNCTION sec_of_day ( s )
   IMPLICIT NONE
   CHARACTER*(*), INTENT(IN) :: s
   INTEGER hh,mm,ss
!  0000000001111111111
!  1234567890123456789
!  2005-01-15_02_04_31
   READ(s(12:13),*)hh
   READ(s(15:16),*)mm
   READ(s(18:19),*)ss
   sec_of_day = hh*3600 + mm*60 + ss
END FUNCTION sec_of_day

LOGICAL FUNCTION valid_date ( s )
   IMPLICIT NONE
   CHARACTER*(*), INTENT(IN) :: s
   LOGICAL, EXTERNAL :: isnum
   LOGICAL retval
   retval = .FALSE.
   IF ( LEN(TRIM(s)) .EQ. 19 ) THEN
     IF ( isnum(1,s) .AND. isnum(2,s) .AND. isnum(3,s) .AND. isnum(4,s) .AND. &
          s(5:5).EQ.'-' .AND. &
          isnum(6,s) .AND. isnum(7,s) .AND. &
          s(8:8).EQ.'-' .AND. &
          isnum(9,s) .AND. isnum(10,s) .AND. &
          s(11:11).EQ.'_' .AND. &
          isnum(12,s) .AND. isnum(13,s) .AND. &
          (s(14:14).EQ.'_' .OR. s(14:14).EQ.':') .AND. &
          isnum(15,s) .AND. isnum(16,s) .AND. &
          (s(17:17).EQ.'_' .OR. s(17:17).EQ.':') .AND. &
          isnum(18,s) .AND. isnum(19,s) ) THEN
       retval = .TRUE.
     ENDIF
   ENDIF
   valid_date = retval
END

LOGICAL FUNCTION isnum ( i, str )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: i
   CHARACTER*(*), INTENT(IN) :: str
   isnum = (ICHAR('0').LE. ICHAR(str(i:i)).AND.ICHAR(str(i:i)) .LE. ICHAR('9'))
    
END FUNCTION isnum
