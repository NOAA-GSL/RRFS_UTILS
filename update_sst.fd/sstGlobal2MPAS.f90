SUBROUTINE sstGlobal2MPAS(sstGlobal,imaskSST,xland,ncells,xlon,ylat,sstRR)
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2009-04-15
!
! ABSTRACT:
!     This routine map NESDIS SNOW/ICE data to RR grid
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  imssnow
!
!   OUTPUT FILES:  RRimssnow
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  wJET
!
!$$$
!
!_____________________________________________________________________


  implicit none

  REAL, intent(in):: sstGlobal(4321,2160)
  INTEGER, intent(in):: imaskSST(4321,2160)
!  grid
  integer, intent(in) :: ncells
  real, intent(in):: xlon(ncells)
  real, intent(in):: ylat(ncells)
  integer, intent(in):: xland(ncells)
!
  real, intent(out):: sstRR(ncells)
!
  integer :: iland,ncount,KOUNT,IPOINT,JPOINT
  real*8  :: ELAT,ELON
  real    :: DIFX,DIFY,AR1,AR2,AR3,AR4,AREA,AREA_min
  integer :: i,j,k,ifound,LL,JPE,JPB,IPE,IPB,NK,MK,NKK,MKK
  integer :: jmask1, jmask2, mkflip
  integer :: ILON1,ILON2,ILAT1,ILAT2
!
  real,   PARAMETER ::  H90=90.0,H1=1.0
  real*8, parameter ::  D00=0.0_8
  real*8, parameter ::  H360=360.0_8
  real*8, parameter ::  delxy=1.0_8/12_8
  real*8, parameter ::  start_lat=90.0_8-delxy/2.0_8
  real*8, parameter ::  start_lon=delxy/2.0_8
  !
  integer :: nfound, nnear,ninterp,nland

!--------------------------------------------------------------------
!
! ****** NOW BEGIN MAJOR LOOP OVER ALL GRID POINTS *******
!
    ncount=0
    nfound=0
    nnear=0
    ninterp=0
    nland=0

    do i=1,ncells

! RR land-water mask xLAND: 0 - water, 1 - land
!
!--------------- DETERMINE LAT/LON OF GRID POINT -------------
!                    (HERE LONG WILL BE EAST LONG)
! find i index
       ELON=xlon(i)
       if(xlon(i) < D00 ) then
          ELON=H360 + xlon(i)
       elseif(xlon(i) > H360) then
          ELON=xlon(i)-H360
       endif
       if(ELON < start_lon) then
          ELON = ELON + H360     ! add 4321 for ELON < start_lon
       endif
       ELON=max((ELON-start_lon),D00)
       ELON=ELON/delxy

       ILON1=INT(ELON)
       DIFX=ELON-ILON1
       ILON1=MIN(ILON1+1,4320)
       ILON2=ILON1+1

! find j index
       ELAT=start_lat-ylat(i)
       ELAT=max(D00,ELAT/delxy)
       ILAT1=INT(ELAT)
       if(ILAT1 + 1 < 2150) then
         DIFY=ELAT-ILAT1
         ILAT1=ILAT1+1
       else
         ILAT1 = 2159
         DIFY=1.0
       endif
       ILAT2=ILAT1+1

!  calculate weight
       AR1=DIFX*DIFY           ! for ILON2,ILAT2
       AR2=DIFX*(H1-DIFY)      ! for ILON2,ILAT1
       AR3=(H1-DIFX)*(H1-DIFY) ! for ILON1,ILAT1
       AR4=(H1-DIFX)*DIFY      ! for ILON1,ILAT2

       IF (xland(i) == 0 ) THEN
! Water points in RRFS domain
! Land/water mask goes from the South Pole north, while SST goes from North Pole south
! Need to flip J index for land/water mask
          jmask1=2160-ILAT1+1
          jmask2=2160-ILAT2+1

          if(imaskSST(ILON2,jmask2) == 0 .AND. imaskSST(ILON2,jmask1) == 0 .AND.   &
             imaskSST(ILON1,jmask1) == 0 .AND. imaskSST(ILON1,jmask2) == 0) THEN
! All 4 points in RTG_SST around the water point in RR are also water
             sstRR(i) = AR1*sstGlobal(ILON2,ILAT2)+AR2*sstGlobal(ILON2,ILAT1)+    &
                          AR3*sstGlobal(ILON1,ILAT1)+AR4*sstGlobal(ILON1,ILAT2)

             ninterp=ninterp+1
          else
! nearest neighbor
           KOUNT = 0

           IF (imaskSST(ILON1,jmask1) == 0) THEN
              KOUNT  = KOUNT + 1
              AREA   = AR3
              IPOINT = ILON1
              JPOINT = ILAT1
           END IF
!
           IF( imaskSST(ILON1, jmask2) == 0 ) THEN
              KOUNT = KOUNT +1
              IF (KOUNT .EQ. 1) THEN
                 IPOINT = ILON1
                 JPOINT = ILAT2
              ELSEIF (AR4 .GT. AREA) THEN
                 AREA   = AR4
                 IPOINT = ILON1
                 JPOINT = ILAT2
              END IF
           END IF
!
           IF( imaskSST(ILON2, jmask1) == 0 ) THEN
              KOUNT = KOUNT + 1
              IF (KOUNT .EQ. 1) THEN
                 AREA   = AR2
                 IPOINT = ILON2
                 JPOINT = ILAT1
              ELSEIF (AR2 .GT. AREA) THEN
                 AREA   = AR2
                 IPOINT = ILON2
                 JPOINT = ILAT1
              END IF
           END IF
!
!
           IF( imaskSST(ILON2, jmask2) == 0  ) THEN
              KOUNT = KOUNT + 1
              IF (KOUNT .EQ. 1) THEN
                 AREA   = AR1
                 IPOINT = ILON2
                 JPOINT = ILAT2
              ELSEIF (AR1.GT. AREA) THEN
                 AREA   = AR1
                 IPOINT = ILON2
                 JPOINT = ILAT2
              END IF
           END IF
!
!     DETERMINE SST USING NEAREST NEIGHBOR 
!
           IF(KOUNT .GT. 0) THEN
              sstRR(i) = sstGlobal(IPOINT,JPOINT)
              nfound=nfound+1
           ELSE
!
!         EXPAND SEARCH RADIUS AND TAKE FIRST WATER TYPE MATCH
!
              IPOINT = ILON1
              JPOINT = ILAT1
!
!  Define the frame (no. of grid points) over which to search for
!    a matching land/water type from IMS data for the model gridpoint.
              ifound=0
              area_min=100000.0
              DO LL=1,16
                JPE = MIN (2160, JPOINT+LL)
                JPB = MAX (1 , JPOINT-LL)
                IPE = MIN (4320, IPOINT+LL)
                IPB = MAX (1 , IPOINT-LL)
!
                DO NK=IPB,IPE
                DO MK=JPB,JPE
                   mkflip=2160-mk+1
                   area=abs((NK-IPOINT)*(NK-IPOINT)+(MK-JPOINT)*(MK-JPOINT))
                   IF ( imaskSST(nk,mkflip) == 0 .and. area_min > area ) THEN
                      NKK=NK
                      MKK=MK
                      area_min=area
                      ifound=ifound+1
                   ENDIF
                ENDDO  ! MK
                ENDDO  ! NK
              ENDDO  ! LL
              IF (ifound > 0) THEN
                 sstRR(i) = sstGlobal(NKK,MKK)
                 nnear=nnear+1
              ELSE
                 ncount=ncount+1
                 sstRR(i) = sstGlobal(IPOINT,JPOINT)
!                 print *,' Points with ifound=0, i ', I,sstRR(i)        
              ENDIF
           ENDIF   !  KOUNT .GT. 0
!
       endif
      
      ELSE
! Land
          sstRR(i) = sstGlobal(ILON1,ILAT1)
          nland=nland+1
      ENDIF
!
    ENDDO  ! ncells

    write(*,'(a,10I10)')' Number of water points from interp, within grid, near grid ', ninterp,nfound,nnear
    write(*,'(a,10I10)')' Number of water points no near by water from global ', ncount
    write(*,'(a,10I10)')' Number of land points and water point', nland, ninterp+nfound+nnear+ncount
    print *,'total points and  max/min SST ', ninterp+nfound+nnear+ncount+nland,maxval(sstRR),minval(sstRR)
                                                                                                                       
end subroutine sstGlobal2MPAS
