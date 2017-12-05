MODULE module_openfoam_bc
   USE module_dm
   IMPLICIT NONE
   TYPE of_pt_t
     DOUBLE PRECISION lat,lon,lz
     INTEGER i,j
   END TYPE of_pt_t
   TYPE bdy_t
     INTEGER npoints
     TYPE (of_pt_t), ALLOCATABLE :: point(:)
   END TYPE bdy_t
   INTEGER, PARAMETER :: nbdys = 7
   TYPE (bdy_t) bdy(nbdys)
   INTEGER, PARAMETER :: BDY_XS = 1, BDY_XE = 2, &
                         BDY_YS = 3, BDY_YE = 4, &
                         BDY_ZS = 5, BDY_ZE = 6, &
                         INTERIOR = 7 ! not a boundary per se but we can handle this too

  CONTAINS
#ifndef DM_PARALLEL
   SUBROUTINE wrf_error_fatal (message)
     CHARACTER*(*) :: message
     WRITE(0,*)TRIM(message)
     STOP 999
   END SUBROUTINE wrf_error_fatal
#endif
   SUBROUTINE read_openfoam_bdy_coords( whichbdy , fname )
     INTEGER, INTENT(IN)       :: whichbdy
     CHARACTER*(*), INTENT(IN) :: fname
     !local
     LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
     CHARACTER*256 message,latstr,lonstr
     INTEGER ipoint,ibdy, ierr, i
     !exec

     ierr = 0
     IF ( wrf_dm_on_monitor()) THEN
       OPEN ( 75, file=TRIM(fname), form="formatted", status="old", err=2222 )
       ! count up the number of lines in the file
       bdy(whichbdy)%npoints = 0
       DO WHILE ( .TRUE. )
         READ(75,*,END=2210)
         bdy(whichbdy)%npoints = bdy(whichbdy)%npoints + 1
       ENDDO
2210   CONTINUE
       CLOSE (75)
       ALLOCATE(bdy(whichbdy)%point(bdy(whichbdy)%npoints))
       ! now read them in
       OPEN ( 75, file=TRIM(fname), form="formatted", status="old", err=2222 )
       DO ipoint = 1,bdy(whichbdy)%npoints
         READ(75,*,ERR=2222)latstr, lonstr, bdy(whichbdy)%point(ipoint)%lz
         i = INDEX(latstr,'N')
         if ( i .NE. 0 ) latstr(i:i) = ' '
         i = INDEX(latstr,'S')
         if ( i .NE. 0 ) latstr(i:i) = '-'
         READ(latstr,*)bdy(whichbdy)%point(ipoint)%lat
         i = INDEX(lonstr,'E')
         if ( i .NE. 0 ) lonstr(i:i) = ' '
         i = INDEX(lonstr,'W')
         if ( i .NE. 0 ) lonstr(i:i) = '-'
         READ(lonstr,*)bdy(whichbdy)%point(ipoint)%lon
       ENDDO
       GOTO 2220
2222   CONTINUE
       ierr = 1
2220   CONTINUE
     ENDIF
     CALL wrf_dm_bcast_integer(ierr,1)
     IF ( ierr .NE. 0 ) THEN
       WRITE(message,*)'read_openfoam_bdy_coords: some error reading in bdy coords from ',TRIM(fname)
     ENDIF
     CALL wrf_dm_bcast_integer(bdy(whichbdy)%npoints,1)
     DO ipoint = 1,bdy(whichbdy)%npoints
       CALL wrf_dm_bcast_double(bdy(whichbdy)%point(ipoint)%lat)
       CALL wrf_dm_bcast_double(bdy(whichbdy)%point(ipoint)%lon)
       CALL wrf_dm_bcast_double(bdy(whichbdy)%point(ipoint)%lz)
     ENDDO
     RETURN
   END SUBROUTINE read_openfoam_bdy_coords

   SUBROUTINE precompute_openfoam_points(ibdy,xlat,xlong,ids,ide,jds,jde,ips,ipe,jps,jpe,ims,ime,jms,jme )
      INTEGER, INTENT(IN) :: ibdy,ids,ide,jds,jde,ips,ipe,jps,jpe,ims,ime,jms,jme
      REAL, DIMENSION(ids:ide-1,jds:jde-1), INTENT(IN) :: xlat, xlong
      ! local
      INTEGER i,j,ipoint,idummy
      REAL of_lat, of_lon
      REAL dsw,dse,dnw,dne,lim,dmin
      CHARACTER*256 bdy_cache_name, message
      LOGICAL incache

      IF ( ALLOCATED(bdy(ibdy)%point) ) THEN
        incache=.TRUE.
        WRITE(bdy_cache_name,'("bdy_cache_",I1)')ibdy
        OPEN(75,file=TRIM(bdy_cache_name),form="formatted",status="old",ERR=9911)
        GOTO 9910
9911    CONTINUE
        OPEN(75,file=TRIM(bdy_cache_name),form="formatted",status="new",ERR=9911)
        incache=.FALSE.
9910    CONTINUE
        DO ipoint = 1,bdy(ibdy)%npoints
          IF ( incache ) THEN
            READ(75,*)idummy,bdy(ibdy)%point(ipoint)%i,bdy(ibdy)%point(ipoint)%j
            IF ( idummy .NE. ipoint ) THEN
              WRITE(message,*)'problem reading: ',TRIM(bdy_cache_name),': ',idummy,' ne ',ipoint
              CALL wrf_error_fatal(message)
            ENDIF
          ELSE
            of_lat = bdy(ibdy)%point(ipoint)%lat
            of_lon = bdy(ibdy)%point(ipoint)%lon
            dmin = 999999.9
            DO j = jps,min(jpe,jde-2)
              DO i = ips,min(ipe,ide-2)
                ! ignore special case where of point lies outside the grid of cell centers
                ! should not put OF grid that close to a WRF boundary
                ! also note the cavalier way we ignore curvature and assume the
                ! grid cells are perfectly square and that lat and lon are Cartesian
                dsw = sqrt((of_lat-xlat(i  ,j  ))*(of_lat-xlat(i  ,j  )) + (of_lon-xlong(i  ,j  ))*(of_lon-xlong(i  ,j  )))
                !!absolute closest
                !IF ( dsw .LT. dmin ) THEN
                !alternate scheme, pick the point that is closest to the sw of the openfoam point
                IF ( dsw .LT. dmin .AND. of_lat .GE. xlat(i,j) .AND. of_lon .GE. xlong(i,j) ) THEN
                  bdy(ibdy)%point(ipoint)%i = i
                  bdy(ibdy)%point(ipoint)%j = j
                  dmin = dsw
                ENDIF
              ENDDO
            ENDDO
            WRITE(75,*)ipoint,bdy(ibdy)%point(ipoint)%i,bdy(ibdy)%point(ipoint)%j
          ENDIF
        ENDDO
        CLOSE(75)
      ENDIF
   END SUBROUTINE precompute_openfoam_points

   REAL FUNCTION rotation_angle( xlat,dx,ids,ide,jds,jde,ips,ipe,jps,jpe,ims,ime,jms,jme )
      INTEGER, INTENT(IN) :: ids,ide,jds,jde,ips,ipe,jps,jpe,ims,ime,jms,jme
      REAL, DIMENSION(ims:ime-1,jms:jme-1), INTENT(IN) :: xlat
      REAL, INTENT(IN) :: dx
      !local
      REAL  cen_lat_west, cen_lat_east, dlat, dist, domlen
      cen_lat_west = -9999.
      cen_lat_east = -9999.
      IF ( jps .LE. (jde-jds)/2 .AND. (jde-jds)/2 .LT. jpe ) THEN
        IF ( ips .EQ. ids ) cen_lat_west = xlat(ips,(jde-jds)/2)
        IF ( ipe .EQ. ide ) cen_lat_east = xlat(ipe-1,(jde-jds)/2)
      ENDIF
      cen_lat_west = wrf_dm_max_real( cen_lat_west )
      cen_lat_east = wrf_dm_max_real( cen_lat_east )
      dlat = (cen_lat_west-cen_lat_east)/360.
      dist = (dlat * 40000000)
      domlen = ( ide-ids )*dx
      rotation_angle = asin( dist / domlen )
   END FUNCTION rotation_angle


   SUBROUTINE check_inflow_on_boundary( ibdy, z,u,v, costheta,sintheta, is,ie,js,je,ks,ke, ims,ime,jms,jme )
      INTEGER, INTENT(IN) :: ibdy
      REAL, INTENT(IN) :: z(is:ie,js:je,ks:ke),u(is:ie,js:je,ks:ke),v(is:ie,js:je,ks:ke)
      REAL, INTENT(IN) :: costheta,sintheta
      INTEGER, INTENT(IN) :: is,ie,js,je,ks,ke,ims,ime,jms,jme
      ! local
      INTEGER :: ipoint, i,j,k
      REAL :: lat_min,lat_max,lon_min,lon_max,delta_lat,delta_lon
      REAL :: normal(3)
      LOGICAL :: check_column(is:ie,js:je)
      INTEGER :: cells_per_level(ks:ke), cells_checked_total
      REAL :: unorm_mag, unorm_min, unorm_max, inflow_mean
      REAL :: ucorr, vcorr, wdir
      REAL, DIMENSION(ks:ke) :: unorm_mean, z_mean
      REAL, DIMENSION(ks:ke) :: wind_mag, wind_dir
      REAL :: wind_dir_min, wind_dir_max
      CHARACTER(len=20) :: fmtstr

      lat_min = HUGE(lat_min)
      lon_min = HUGE(lon_min)
      lat_max = -HUGE(lat_max)
      lon_max = -HUGE(lon_max)
      DO ipoint = 2,bdy(ibdy)%npoints
         lat_min = MIN(lat_min, bdy(ibdy)%point(ipoint)%lat)
         lon_min = MIN(lon_min, bdy(ibdy)%point(ipoint)%lon)
         lat_max = MAX(lat_max, bdy(ibdy)%point(ipoint)%lat)
         lon_max = MAX(lon_max, bdy(ibdy)%point(ipoint)%lon)
      END DO
      delta_lat = lat_max-lat_min
      delta_lon = lon_max-lon_min
      WRITE(*,*) '  detected delta lat/long from input bc file:',delta_lat,delta_lon

      ! Estimate normal vector from SOWFA grid min/max
      ! normal vector = [ dx, dy, 0 ] x [ 0, 0, 1 ]
      normal(1) = delta_lat
      normal(2) = delta_lon
      normal(3) = 0.0
      normal(:) = normal(:) / SQRT( delta_lat*delta_lat + delta_lon*delta_lon )
      WRITE(*,*) '  detected normal (unit) vector:',normal

      ! Identify cells that will be used for interpolating to OpenFOAM points
      check_column(:,:) = .FALSE.
      DO ipoint = 1,bdy(ibdy)%npoints
         j = bdy(ibdy)%point(ipoint)%j   ! precomputed jcoord of cell center corresponding to lat
         i = bdy(ibdy)%point(ipoint)%i   ! precomputed icoord of cell center corresponding to lon
         check_column(i,j) = .TRUE.
      END DO

      ! Check all identified cells
      unorm_min = HUGE(unorm_min)
      unorm_max = -HUGE(unorm_max)
      wind_dir_min = HUGE(wind_dir_min)
      wind_dir_max = -HUGE(wind_dir_max)
      inflow_mean = 0.0
      z_mean(:) = 0.0
      unorm_mean(:) = 0.0
      wind_mag(:) = 0.0
      wind_dir(:) = 0.0
      cells_per_level(:) = 0
      DO k=ks,ke
         DO j=js,je
            DO i=is,ie
               IF ( check_column(i,j) ) THEN
                  ucorr = u(i,j,k)*costheta - v(i,j,k)*sintheta
                  vcorr = u(i,j,k)*sintheta + v(i,j,k)*costheta
                  unorm_mag = normal(1)*ucorr + normal(2)*vcorr
                  unorm_min = MIN(unorm_min, unorm_mag)
                  unorm_max = MAX(unorm_max, unorm_mag)
                  wdir = ATAN2(vcorr,ucorr)
                  !wdir = 90.0 - wdir*57.29577951308232 ! wind in +x (from west) is "0 deg"
                  wdir = 270.0 - wdir*57.29577951308232 ! wind in +x (from west) is 270 deg
                  wind_dir_min = MIN(wind_dir_min, wdir)
                  wind_dir_max = MAX(wind_dir_max, wdir)
                  IF ( wdir < 0 ) wdir = wdir + 360.0
                  z_mean(k) = z_mean(k) + z(i,j,k)
                  unorm_mean(k) = unorm_mean(k) + unorm_mag
                  wind_mag(k) = wind_mag(k) + SQRT(ucorr*ucorr + vcorr*vcorr)
                  wind_dir(k) = wind_dir(k) + wdir
                  IF ( unorm_mag .GE. 0.0 ) THEN
                     inflow_mean = inflow_mean + 1.0
                  END IF
                  cells_per_level(k) = cells_per_level(k) + 1
               END IF
            END DO
         END DO
         z_mean(k) = z_mean(k) / cells_per_level(k)
         unorm_mean(k) = unorm_mean(k) / cells_per_level(k)
         wind_mag(k) = wind_mag(k) / cells_per_level(k)
         wind_dir(k) = wind_dir(k) / cells_per_level(k)
      END DO
      IF ( wind_dir_min < 0 ) wind_dir_min = wind_dir_min + 360.0
      IF ( wind_dir_max < 0 ) wind_dir_max = wind_dir_max + 360.0
      cells_checked_total = SUM( cells_per_level )
      inflow_mean = inflow_mean / cells_checked_total
      WRITE(*,'(a,3(f12.5,x))') '  inflow fraction, unorm min, unorm max=',inflow_mean,unorm_min,unorm_max
      WRITE(fmtstr,'("(a,",i4,"(i8,x))")') ke-ks+1
      WRITE(*,fmtstr) '  cells checked per level:',cells_per_level
      WRITE(fmtstr,'("(a,",i4,"(f12.5,x))")') ke-ks+1
      WRITE(*,fmtstr) '  average cell heights per level:',z_mean
      WRITE(*,fmtstr) '  unorm mean by height:',unorm_mean
      WRITE(*,fmtstr) '  wind mag by height:',wind_mag
      WRITE(*,fmtstr) '  wind dir by height:',wind_dir
      WRITE(*,'(a,2(f12.5,x))') '  wind dir min,max=',wind_dir_min,wind_dir_max

   END SUBROUTINE check_inflow_on_boundary


END MODULE module_openfoam_bc


