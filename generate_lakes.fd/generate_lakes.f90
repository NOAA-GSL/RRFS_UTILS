PROGRAM generate_lakes
!
!   PRGMMR: Ming Hu  ORG: GSL        DATE: 2026-05-12
!
! ABSTRACT: 
!     This routine reads in SST and map it to RRFS grid
!
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

  use mpi
  use module_ncio, only : ncio
  use module_esggrid_util, only: edp,esggrid_util

  implicit none

  type(ncio) :: ncfile
  type(esggrid_util) :: esggrid
  character(len=20) :: grid_type

! MPI variables
  integer :: npe, mype, mypeLocal,ierror

! MPAS grid
  integer :: ncells
  real,allocatable  :: xlon_mpas(:)
  real,allocatable  :: ylat_mpas(:)
  integer,allocatable  :: landmask_mpas(:)
!
!  FV3 grid
  integer :: nlon,nlat
  real, allocatable :: xlon(:,:)
  real, allocatable :: ylat(:,:)
  real, allocatable :: r4d2(:,:)
  integer,allocatable  :: landmask(:,:)
  integer,allocatable  :: lakemask(:,:)
!
!  MPAS lakemask
  integer,allocatable  :: lakemask_fv3(:)  ! MPAS lake mask from RRFSv1 FV3 lake mask
  integer,allocatable  :: lakemask_mpas(:)  ! MPAS lake mask: include great lakes and water point in land
  integer, allocatable :: index_i(:),index_j(:) ! MPAS lake point link to the nearest FV3 lake point
!
!
  real :: pi2deg
  CHARACTER*180 file
!
  real(edp) :: rlon,rlat,xc,yc
  real(8)   :: dlon,dlat,area,area_min
  real      :: dx,dy
  integer   :: n,i,j,ii,jj,iii,jjj
  integer   :: iwater,ilake,iice
  integer   :: ifound,nofound,nfound,nn
!

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
!  call MPI_INIT(ierror)
!  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
!  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

   pi2deg=180.0/3.1415926
   grid_type="RRFS_NA_3km"
!  if(mype==0) then
!
! get MPAS grid and land/water mask
!
     call ncfile%open("static.nc","r",0)
     call ncfile%get_dim("nCells",ncells)
     write(*,*) 'MPAS grid dimension=',ncells

     allocate(xlon_mpas(ncells))
     allocate(ylat_mpas(ncells))
     allocate(landmask_mpas(ncells))

     call ncfile%get_var("latCell",ncells,ylat_mpas)
     call ncfile%get_var("lonCell",ncells,xlon_mpas)
     call ncfile%get_var("landmask",ncells,landmask_mpas)   ! land-ocean mask (1=land ; 0=ocean/water)
     call ncfile%close()
     ylat_mpas=ylat_mpas*pi2deg
     xlon_mpas=xlon_mpas*pi2deg
     write(*,*) 'MPAS max/min lat=',maxval(ylat_mpas), minval(ylat_mpas)
     write(*,*) 'MPAS max/min lon=',maxval(xlon_mpas), minval(xlon_mpas)
!
! read in FV3 latlon
!
     call ncfile%open('fv3_grid_spec',"r",0)
     call ncfile%get_dim("grid_xt",nlon)
     call ncfile%get_dim("grid_yt",nlat)
     write(*,*) 'FV3 nlon,nlat=',nlon,nlat

     allocate(xlon(nlon,nlat))
     allocate(ylat(nlon,nlat))
     call ncfile%get_var("grid_lont",nlon,nlat,xlon)
     call ncfile%get_var("grid_latt",nlon,nlat,ylat)
     call ncfile%close()
     write(*,*) 'FV3 max/min lat=',maxval(ylat), minval(ylat)
     write(*,*) 'FV3 max/min lon=',maxval(xlon), minval(xlon)

!     call ncfile%open('../sfc/C3463_oro_data.tile7.halo0.nc',"r",0)
!     call ncfile%get_dim("lon",nlon)
!     call ncfile%get_dim("lat",nlat)
!     write(*,*) 'FV3 nlon,nlat=',nlon,nlat

!     allocate(xlon(nlon,nlat))
!     allocate(ylat(nlon,nlat))
!     call ncfile%get_var("geolon",nlon,nlat,xlon)
!     call ncfile%get_var("geolat",nlon,nlat,ylat)
!     call ncfile%close()
!     write(*,*) 'FV3 max/min lat=',maxval(ylat), minval(ylat)
!     write(*,*) 'FV3 max/min lon=',maxval(xlon), minval(xlon)
!
! read in FV3 land mask and lake mask
     call ncfile%open("fv3LandLakes.nc","r",0)
     allocate(r4d2(nlon,nlat))
     allocate(landmask(nlon,nlat))
     allocate(lakemask(nlon,nlat))
     call ncfile%get_var("slmsk",nlon,nlat,r4d2)
     landmask=int(r4d2+0.0001)
     call ncfile%get_var("clm_lake_initialized",nlon,nlat,r4d2)
     lakemask=int(r4d2+0.0001)
     deallocate(r4d2)
     call ncfile%close()
!  slmsk: water=0, land=1, ice=2
!  clm_lake_initialized: lake=1, others=0
     write(*,*) 'FV3 max/min land/ice mask=',maxval(landmask), minval(landmask)
     write(*,*) 'FV3 max/min lake mask=',maxval(lakemask), minval(lakemask)
     do j=1,nlat
       do i=1,nlon
          if(landmask(i,j)==2 ) landmask(i,j)=0   ! 0=water, 1=land
       enddo
     enddo
     do j=1,nlat
       do i=1,nlon
          if(landmask(i,j)==1 ) lakemask(i,j)=2   ! 0=ocean,1=lake,2=land
       enddo
     enddo
     do j=1,nlat
       do i=1,nlon
          if( (ylat(i,j) > 49.0 .and. ylat(i,j) < 56.0 ) .and. & 
              (xlon(i,j) > 257.0.and. xlon(i,j) < 269.0) ) then
              if(landmask(i,j)==0 .and. lakemask(i,j)==0) then
                lakemask(i,j)=2   ! 0=ocean,1=lake,2=land
              endif
          endif
       enddo
     enddo
     write(*,*) 'FV3 max/min land mask=',maxval(landmask), minval(landmask)
!
! Match MPAS grid point to nearest FV3 grid point
!
! define esg grid
     call esggrid%init(grid_type)
!
! initialize the memory
     allocate(lakemask_fv3(ncells))
     allocate(lakemask_mpas(ncells))
     allocate(index_i(ncells))
     allocate(index_j(ncells))
     lakemask_fv3=-1
     lakemask_mpas=-1
     index_i=-1
     index_j=-1

! convert to grid coordinate and check if it is inside the domain
     loop_cells: do n=1,ncells
        rlon=xlon_mpas(n)
        rlat=ylat_mpas(n)
        call esggrid%lltoxy(rlon,rlat,xc,yc)
        dlon=xc
        dlat=yc
        ii=int(dlon)
        jj=int(dlat)
        if( (ii >= 0 .and. ii < nlon+1) .and. &
            (jj >= 0 .and. jj < nlat+1) ) then
          ! inside the FV3 domain
          dx=dlon-float(ii)
          dy=dlat-float(jj)
! use the point right outside the boundary
          if(ii==0) then
             ii=1
             dx=0.0
          endif
          if(jj==0) then
             jj=1
             dy=0.0
          endif
          if(ii==nlon) then
             ii=nlon-1
             dx=1.0
          endif
          if(jj==nlat) then
             jj=nlat-1
             dy=1.0
          endif
! find the closest grid index
          iii=ii
          jjj=jj
          area_min=abs(dx*dy)
          
          area=abs((1.0-dx)*(1.0-dy))
          if( area_min > area ) then
             iii=ii+1
             jjj=jj+1
             area_min=area
          endif

          area=abs(dx*(1.0-dy))
          if( area_min > area ) then
             iii=ii
             jjj=jj+1
             area_min=area
          endif

          area=abs((1.0-dx)*dy)
          if( area_min > area ) then
             iii=ii+1
             jjj=jj
             area_min=area
          endif
! set the lake/ocean/land
          lakemask_fv3(n)=lakemask(iii,jjj)  ! 0=ocean,1=lake,2=land
          index_i(n)=iii
          index_j(n)=jjj
        else
          ! outside the FV3 domain, set to others (0)
          lakemask_fv3(n)=0
          index_i(n)=0
          index_j(n)=0
        endif
     enddo loop_cells
     lakemask_mpas=lakemask_fv3
     write(*,*) 'MPAS max/min lake mask from FV3=',maxval(lakemask_fv3), minval(lakemask_fv3)
     write(*,*) 'MPAS max/min lake index i from FV3=',maxval(index_i), minval(index_i)
     write(*,*) 'MPAS max/min lake index j from FV3=',maxval(index_j), minval(index_j)

! need to match MPAS landmask with MPAS lake mask
     ! MPAS landmask water 0 = MPAS lake mask water or lake (0/1)
     ! MPAS landmask land  1 = MPAS lake mask land (2)
     nofound=0
     nfound=0
     nn=10
     do n=1,ncells
        if(landmask_mpas(n)==0) then
           ! MPAS water point, so lakemask_fv3=0 and =1 are good already
           if(lakemask_fv3(n)==2) then
              ! need to find the nearest fv3 lake point
              ifound=0
              area_min=10000000.0
              do j=max(1,index_j(n)-nn),min(index_j(n)+nn,nlat)
              do i=max(1,index_i(n)-nn),min(index_i(n)+nn,nlon)
                 if(lakemask(i,j)==1 .or. lakemask(i,j)==0) then
                    ifound=ifound+1
                    area=abs(((i-index_i(n))*(i-index_i(n)))+(j-index_j(n)*(j-index_j(n))))
                    if(area_min > area) then
                       iii=i
                       jjj=j
                       area_min=area
                    endif
                 endif
              enddo
              enddo
              if(ifound > 0) then
                 if(lakemask(iii,jjj)==1) then
                    lakemask_mpas(n)=3 ! this point should be lake as the closest point is lake
                 else
                    lakemask_mpas(n)=0 ! this point should be ocean as it closest point is ocean
                 endif
                 index_i(n)=iii
                 index_j(n)=jjj
                 
                 nfound=nfound+1
              else
                 lakemask_mpas(n)=4
                 nofound=nofound+1
              endif
           endif
        else
           ! MPAS land, so lakemask_fv3=2 is good already
           if(lakemask_fv3(n)==0 .or. lakemask_fv3(n)==1) then
              lakemask_mpas(n)=2 ! this point should be land. do not change index_i,index_j
           endif
        endif
     enddo
     write(*,*) 'MPAS max/min lake mask=',maxval(lakemask_mpas), minval(lakemask_mpas)
     write(*,*)  'can found nearby lake point=',nfound
     write(*,*)  'can not found nearby lake point=',nofound
!
! now let's add the Great Lakes
!
     do n=1,ncells
        if(lakemask_mpas(n)==0) then
           if ( (ylat_mpas(n) < 49.3 .and. ylat_mpas(n) > 41.0) .and.   &
                (xlon_mpas(n) < 284.5 .and. xlon_mpas(n) > 267.5)) then
              lakemask_mpas(n)=5
           endif  ! Great Lakes
        endif  ! lakes correction
     enddo
     write(*,*) 'MPAS max/min lake mask with the Great Lakes=',maxval(lakemask_mpas), minval(lakemask_mpas)
!
!  now MPAS lake mask has following categories
!  0=ocean, 1=lake, 2=land, 3=lake nearby FV3 lake, 4= lake not nearby Fv3 lake, 5 = great lakes

     ylat_mpas=ylat_mpas/pi2deg
     xlon_mpas=xlon_mpas/pi2deg
     call write_nc(ncells, lakemask_fv3, lakemask_mpas, index_i, index_j, xlon_mpas, ylat_mpas)
!
!  release memory
!
     deallocate(xlon_mpas)
     deallocate(ylat_mpas)
     deallocate(landmask_mpas)

     deallocate(xlon)
     deallocate(ylat)
     deallocate(landmask)
     deallocate(lakemask)

     deallocate(lakemask_fv3)
     deallocate(lakemask_mpas)
     deallocate(index_i)
     deallocate(index_j)

     write(6,*) "=== GENERATE LAKES SUCCESS ==="

!  endif ! mype==0

!  call MPI_FINALIZE(ierror)
!
END PROGRAM generate_lakes
