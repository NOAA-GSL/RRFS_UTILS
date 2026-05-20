PROGRAM process_SST
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

  implicit none

  type(ncio) :: ncfile

! MPI variables
  integer :: npe, mype, ierror

! RRFSgrid
  integer :: ncells
! RR in RLL
  real,allocatable  :: xlon(:)
  real,allocatable  :: ylat(:)
  integer,allocatable  :: landmask(:)
  integer,allocatable  :: ivegtyp(:)

  real, allocatable :: sstRRFS(:)    ! sst in RRFS
  real, allocatable :: sstGlobal(:,:)  ! sst from global dataset
  integer, allocatable :: imaskSST(:,:)
!
! Lakes
!
  logical :: if_update_lake
!
  real :: pi2deg
  CHARACTER*180 file
  integer :: istatus
  integer :: i,j,iwater,ilake,iice
!
  INTEGER :: iyear, imonth, iday, ihr
!  namelist/setup/ iyear,imonth,iday,ihr

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

  pi2deg=180.0/3.1415926
  if(mype==0) then
  mype0_section: block
!
! read namelist
!
!     open(15, file='sst.namelist')
!       read(15,setup)
!     close(15)
!
! get RRFS grid and land/water mask
!
     call ncfile%open("static.nc","r",0,istatus)
     if(istatus > 0) then
        write(*,*) "cannot open static.nc"
        exit mype0_section
     endif
     call ncfile%get_dim("nCells",ncells)
     write(*,*) 'grid dimension=',ncells

     allocate(xlon(ncells))
     allocate(ylat(ncells))
     allocate(landmask(ncells))

     call ncfile%get_var("latCell",ncells,ylat)
     call ncfile%get_var("lonCell",ncells,xlon)
     call ncfile%get_var("landmask",ncells,landmask)   ! land-ocean mask (1=land ; 0=ocean)
     call ncfile%close()
     ylat=ylat*pi2deg
     xlon=xlon*pi2deg
     write(*,*) 'max/min lat=',maxval(ylat), minval(ylat)
     write(*,*) 'max/min lon=',maxval(xlon), minval(xlon)
!
! read in global 0.083333 degre SST
!
     write(*,*) "===> read global SST"
     allocate(sstGlobal(4321,2160))
     file='RGT_SST.grib2'
     call read_sstGlobal_grib2(file,sstGlobal(1:4320,:),iyear,imonth,iday,ihr)
     write(6,*)' read in global sst data', iyear, imonth, iday, ihr

! 
! Read in land sea tags (0 for ocean; 3 for land) 
     write(*,*) "===> read RTG SST landmask"
!
     allocate(imaskSST(4321,2160))
     OPEN (11,FILE='RTG_SST_landmask.dat')
        READ (11,'(80I1)') imaskSST(1:4320,:)
     CLOSE (11)

     ! make 4321 = 1 because of globlal 
     sstGlobal(4321,:)=sstGlobal(1,:)
     imaskSST(4321,:)=imaskSST(1,:)
     write(*,*) 'max/min sst from grib2 file=',maxval(sstGlobal),minval(sstGlobal)
     print *,'imaskSST(600,210) ', imaskSST(600,210)
!
! interpolate to RRFS grid
!
     allocate(sstRRFS(ncells))
     sstRRFS=0
     write(*,*) "===> call sstGlobal2MPAS"
     call sstGlobal2MPAS (sstGlobal,imaskSST,landmask,ncells,xlon,ylat,sstRRFS)
!     write(6,*)'from global  data ylat/xlon/sstRRFS(516)',   &
!                ylat(516),xlon(516),sstRRFS(516)
!
! update model sst and tsk
!
     write(*,*) "===> call update_SST_MPAS"
     xlon=xlon-360.0    ! convert to 180 to -180
     if_update_lake=.true.
     call update_SST_MPAS(sstRRFS, ylat, xlon, ncells, iyear,imonth,iday,if_update_lake)
!
!  release memory
!
     deallocate(xlon)
     deallocate(ylat)
     deallocate(landmask)

     write(6,*) "=== UPDATE SST SUCCESS ==="
  end block mype0_section
  endif ! mype==0

  call MPI_FINALIZE(ierror)
!
END PROGRAM process_SST

SUBROUTINE read_sstGlobal_grib2(filename,sst,idatayr,idatamon,idataday,idatahh)
!
!   PRGMMR: Ming Hu,    ORG: GSD        DATE: 2017-06-11
!
! ABSTRACT:
!     This routine read in GLOBAL SST data from a grib2 file
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT 
!   OUTPUT 
!      sst :  sea surface temperature
!      idatayr: Year
!      idatamon: month
!      idataday:   day
!      idatahh:  hour
!   INPUT FILES:  fiename
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


  use grib2_read_mod, only : read_grib2_head_dim,read_grib2_head_time
  use grib2_read_mod, only : read_grib2_sngle

!  USE GRIB_MOD
  implicit none

!  INTEGER JF
  character*100,intent(in) :: filename
  REAL, intent(out), target::  SST(4320,2160)
  INTEGER,intent(out) :: idatayr,idatamon,idataday,idatahh

  REAL,pointer ::  sst1d(:)
  real :: rlatmin,rlonmin
  real*8  :: rdx,rdy
  integer :: nx,ny
  
  integer :: idatamm

  logical :: fileexist
  integer :: ntot

!-----------------------------------------------------------------------
!  JF=4320*2160
  sst1d(1:size(SST)) => SST
     inquire(file=trim(filename),exist=fileexist)
     if( .not. fileexist) then
        write(*,*) 'file is not exist: ',trim(filename)
        write(*,*) 'stop update SST'
        stop
     endif
     call read_grib2_head_dim(filename,nx,ny,rlonmin,rlatmin,rdx,rdy)
     write(*,*) nx,ny,rlonmin,rlatmin,rdx,rdy
     call read_grib2_head_time(filename,idatayr,idatamon,idataday,idatahh,idatamm)   
     write(*,'(a,5I5)') 'data: idatayr,idatamon,idataday,idatahh,idatamm=',&
                      idatayr,idatamon,idataday,idatahh,idatamm

     ntot = nx*ny
     call read_grib2_sngle(filename,ntot,sst1d)

end subroutine 
