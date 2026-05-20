subroutine update_SST_MPAS (sstRR, glat, glon, ncells, iyear,imonth,iday,if_update_lake)
!$$$  documentation block
!                .      .    .                                       .
!   update_SST_netcdf_mass: read SST from MPASbackground file
!           and update SST in MPAS file
!   prgmmr: Ming Hu                 date: 2026-05-13
!
! program history log:
!
!
!   input argument list:
!       sstRR: sst
!       ncells: dimension
!
! attributes:
!   language: f90
!
!$$$

  use module_ncio, only : ncio

  implicit none

  type(ncio) :: ncfile

  integer,intent(in) :: ncells
  real,intent(inout) :: sstRR(ncells)
  real,intent(in)    :: glat(ncells)
  real,intent(in)    :: glon(ncells)
  integer,intent(in) :: iyear,imonth,iday
  logical,intent(in) :: if_update_lake

! Declare local parameters

  character(len=120) :: flnm1
  
  integer :: i,j,k
  real,allocatable ::surftemp(:)
  real,allocatable ::temp2m(:)
  real,allocatable ::sst(:)
  integer,allocatable :: xland(:) ! it is landmask. water=0
  integer,allocatable ::lu_index(:)
  integer,allocatable :: isltyp(:)
  !
  ! climatology lake T
  REAL salt_lake,salton,champ

  real xc1,yc1, xc2,yc2
  integer isup,jsup, iwin,jwin, isalton,jsalton, ifound

  integer :: in_SF_LAKE_PHYSICS
  integer :: iwater,ilake,iice
  integer :: nwater,nlake,nice
  integer :: ierr

!
! ===============================================================================
!
! High-resolution SST plus Great Lakes data
!      iwater=16 ! USGS
  iwater=17 ! MODIS
  ilake =21 ! MODIS
  iice = 15 ! MODIS
  nwater=0
  nlake=0
  nice=0
!
!tgs - in FV3 lkm=1 is turning on the lake model, we'll do a change later when
!      lake model is on
  in_SF_LAKE_PHYSICS=0
  ! get climatology lake T
  call cal_lake_climate_t(iyear,imonth,iday,salt_lake,salton,champ)
!
!-------------  get grid info

!  open and read background dimesion
!
  write(*,*) '================================================='
  write(*,*)"ncells", ncells

  allocate(surftemp(ncells))
  allocate(temp2m(ncells))
  allocate(sst(ncells))
  allocate(lu_index(ncells))  ! dominant vegetation category
  allocate(isltyp(ncells))    ! dominate soil category 
  allocate(xland(ncells))     ! landmask
!tgs - so far we do not have lake info in NA RRFS. The variables in FV3 are
!      lake_depth and lake_frac
!  allocate(lake_frac(nlon_regional,nlat_regional)) 
!  allocate(lake_depth(nlon_regional,nlat_regional))

  call ncfile%open("mpasout.nc","r",0,ierr)
  if(ierr>0) return
    call ncfile%get_var("sst",ncells,sst)
    call ncfile%get_var("skintemp",ncells,surftemp)
    call ncfile%get_var("t2m",ncells,temp2m)
    call ncfile%get_var("isltyp",ncells,isltyp)
    call ncfile%get_var("ivgtyp",ncells,lu_index)
    call ncfile%get_var("landmask",ncells,xland)   ! land-ocean mask (1=land ; 0=ocean)
  call ncfile%close()
!
  write(*,*) '================================================='
!  rmse_var='LAKE_FRAC'
!  lake_frac=0
!
!  update skin temperature over water
!
! find i,j for a point in northern Lake Superior
  suploop: DO I=1,ncells
   if((glat(i)>48.4 .and. glat(i)<49.6) .and. (glon(i)<-87.9 .and. glon(i)>-88.1)) then
     isup=i
     print *,' Lake Superior --> i,j,glat(i),glon(i)',i,glat(i),glon(i), &
                                       'lu_index(i)',lu_index(i),xland(i),isltyp(i)
     exit suploop
   endif
  ENDDO suploop

  write(*,*) 'in_SF_LAKE_PHYSICS:', in_SF_LAKE_PHYSICS
!-- update skin temp for water points
! water points:
!     lake:   lu_index(i) == ilake
!                 in_SF_LAKE_PHYSICS == 0 : UPDATE SST
!                 in_SF_LAKE_PHYSICS == 1 : where CLM lake model is applied, SST is
!                                           not updated at these points
!                 we do not have lake mask now, if we have
!                      lakemask == 0; water point that is not lake: update SST
!                      lakemask == 1; lake point
!     water:  update SST
       
  if( lu_index(i) == ilake) then
        nlake=nlake+1
  endif
  
  DO I=1,ncells
    if( xland(i) < 0.00001 ) then    ! water, xland = 0
    ! only unfrozen water points (sea or lakes)
       if( lu_index(i) == ilake) then
         if(in_SF_LAKE_PHYSICS == 0) then
           nlake=nlake+1
       ! --- CLM lake model is off, use climatology for several lakes
       ! --- Great Salt Lake, Utah Lake -- Utah
            if (glat(i).gt.39.5 .and. glat(i).lt.42. .and.  &
               glon(i).gt.-114..and. glon(i).lt.-111.) then
               write(*,*)'Global data Salt Lake temp',i,j,sstRR(i)
               sstRR(i) = salt_lake
                write(*,*)'Climatology Salt Lake temp',i,j,sstRR(i)  &
                          ,glat(i),glon(i)
            end if

            ! --- Salton Sea -- California
            if (glat(i).gt.33. .and. glat(i).lt.33.7 .and.  &
                glon(i).gt.-116.3 .and. glon(i).lt.-115.3) then
            write(6,*)'Global data Salton Sea temp',i,j,sstRR(i)
            sstRR(i) = salton
            write(6,*)'Climatology Salton Sea temp',i,j,sstRR(i)  &
                ,glat(i),glon(i)
              isalton=i
              jsalton=j
            end if

            ! --- Lake Champlain -- Vermont
            if (glat(i).gt.44. .and. glat(i).lt.45.2 .and.  &
               glon(i).gt.-74. .and. glon(i).lt.-73.) then
            write(*,*)'Global data Lake Champlain temp',i,j,sstRR(i)
            sstRR(i) = champ
            write(*,*)'Climatology Lake Champlain temp',i,j,sstRR(i)  &
                ,glat(i),glon(i)
            end if
            ! --- For Lake Nipigon, use point for n. Lake Superior
            !   -- Lake Nipigon is deep!
            if (glat(i).gt.49. .and. glat(i).lt.51. .and. &
               glon(i).gt.-90. .and. glon(i).lt.-87.) then
               write(*,*)'Global data Lake Nipigon temp',i,sstRR(i)
                sstRR(i) = sstRR(isup)
                write(*,*)'Lake Nipigon temp',i,j,sstRR(i) &
                 ,glat(i),glon(i)
            end if
            surftemp(i) = sstRR(i)
            sst(i)=sstRR(i)
         end if ! in_SF_LAKE_PHYSICS == 0
       else   ! not a lake but it is water
         surftemp(i) = sstRR(i)
         sst(i)=sstRR(i)
         nwater=nwater+1
       endif

       if(sstRR(i) > 400. .or. sstRR(i) < 100. ) then
          print *,'Bad SST at point i',i,sstRR(i)
       endif

    elseif (xland(i) > 1.99) then ! ice, xland = 2
    ! frozen water - xland = 2
       if(in_SF_LAKE_PHYSICS == 0) then
          ! CLM lake model is turned off
            if( lu_index(i)==ilake .and. sstRR(i) > 273.) then
            ! -- frozen lakes with CLM lake model turned off.
              print *,'Ice lake cannnot have SST > 274K'
              !set skin temp of frozen lakes to 2-m temperature
              sstRR(i)= min(273.15,temp2m(i))

              !update skin temp for frozen lakes
              surftemp(i)=sstRR(i)
              sst(i)=sstRR(i)
              nice=nice+1
            endif
       endif ! in_SF_LAKE_PHYSICS == 0

    endif  ! water or ice

    ! update SST 
  ENDDO
  write(*,*) 'Skin temperature updated with current SST'
       write(*,*)' updated water, lake, and ice point', nwater,nlake,nice
       write(*,*)' max,min skin temp =',maxval(surftemp),minval(surftemp)

       !
!  Now, update lakes
!
  if(if_update_lake) then
     call update_lakes_MPAS(surftemp,sst,ncells)
  endif
!
!
!           update fv3 netcdf file with new SST
!
  write(*,*) ' ============================= '
  write(*,*) ' update SST in background file '
  write(*,*) ' ============================= '
!-------------  get date info

  write(*,*) ' Update SST in background at time:'
  write(*,*)' iy,m,d,h,m,s=',iyear,imonth,iday

  write(*,*) '================================================='
  write(*,*)' max,min skin temp =',maxval(surftemp),minval(surftemp)
  write(*,*)' max,min SST =',maxval(sst),minval(sst)
  call ncfile%open("mpasout.nc","w",0,ierr)
  if(ierr>0) return
    call ncfile%replace_var("sst",ncells,sst)
    call ncfile%replace_var("skintemp",ncells,surftemp)
  call ncfile%close()

  deallocate(surftemp)
  deallocate(temp2m)
  deallocate(sst)
  deallocate(lu_index)
  deallocate(isltyp)
  deallocate(xland)


  write(*,*) "=== UPDATE SST SUCCESS ==="

end subroutine update_SST_MPAS


subroutine cal_lake_climate_t(iyear,imonth,iday,salt_lake,salton,champ)

          implicit none

          integer,intent(in) :: iyear,imonth,iday
          real,intent(out) :: salt_lake,salton,champ
! Lakes from RUC model
! -- Great Salt Lake lake surface temps
        REAL salt_lake_lst (13)
        data salt_lake_lst &
        /1.,3.,6.,13.,17.,20.,26.,25.,20.,14.,9.,3.,1./
! -- Salton Sea - California
        REAL salton_lst (13)
        data salton_lst &
        / 12.8, 12.8, 17.2, &
         21.1, 24.4, 26.7,  &
         30.0, 31.7, 29.4,  &
         25.0, 20.6, 15.0,  &
         12.8/
! -- Lake Champlain - Vermont
        REAL champ_lst (13)
        data champ_lst&
       /  1.3,  0.6,  1.0,  &
          3.0,  7.5, 15.5,  &
         20.5, 21.8, 18.2,  &
         13.0,  8.2,  4.5,  &
          1.3/

        real xc1,yc1, xc2,yc2
        integer isup,jsup, iwin,jwin, isalton,jsalton

      integer julm(13)
      data julm/0,31,59,90,120,151,181,212,243,273,304,334,365/

        INTEGER  mon1,mon2,day1,day2,juld
        real rday,wght1,wght2

        ! Compute weight for the current date
       juld = julm(imonth) + iday
       if(juld.le.15) juld=juld+365

       mon2 = imonth
       if(iday.gt.15) mon2 = mon2 + 1
       if(mon2.eq.1) mon2=13
       mon1=mon2-1
! **** Assume data valid at 15th of month
       day2=julm(mon2)+15
       day1=julm(mon1)+15
       rday=juld
       wght1=(day2-rday)/float(day2-day1)
       wght2=(rday-day1)/float(day2-day1)
       write(6,*)'Date weights =',wght1,wght2

! Climatology Salt Lake temp

       salt_lake = 273.15 + wght1*salt_lake_lst(mon1)  &
                       +wght2*salt_lake_lst(mon2)

! Climatology Salton Sea temp
       salton = 273.15 + wght1*salton_lst(mon1)  &
                       +wght2*salton_lst(mon2)

! Climatology Lake Champlain temp
       champ  = 273.15 + wght1*champ_lst(mon1)  &
                       +wght2*champ_lst(mon2)


end subroutine cal_lake_climate_t

subroutine update_lakes_MPAS (surftemp,sst,ncells)
!$$$  documentation block
!                .      .    .                                       .
!   update_SST_netcdf_mass: read SST from MPASbackground file
!           and update SST in MPAS file
!   prgmmr: Ming Hu                 date: 2026-05-13
!
! program history log:
!
!
!   input argument list:
!       sstRR: sst
!       ncells: dimension
!
! attributes:
!   language: f90
!
!$$$

  use module_ncio, only : ncio

  use grib2_read_mod, only : read_grib2_head_dim,read_grib2_head_time
  use grib2_read_mod, only : read_grib2_sngle

  implicit none

  type(ncio) :: ncfile

  integer,intent(in) :: ncells
  real,intent(inout) :: sst(ncells)
  real,intent(inout) :: surftemp(ncells)

!
  integer,allocatable :: lakemask(:)
  integer,allocatable :: index_i(:)
  integer,allocatable :: index_j(:)
!
  integer :: nlon,nlat
  real,allocatable :: tsfc(:,:)
!  
  logical :: fileexist
  integer :: n,i,j,ierr
!
!
! 
  inquire(file="sfc_data.nc",exist=fileexist)
  if( .not. fileexist) then
     write(*,*) 'WARNING: cannot update lake T: no RRFSv1 surface file'
     return
  endif
 
  allocate(lakemask(ncells))
  allocate(index_i(ncells))
  allocate(index_j(ncells))

  call ncfile%open("mpas_lake_mask.nc","r",200,ierr)
  if(ierr>0) return
    call ncfile%get_var("lakemask",ncells,lakemask)
    call ncfile%get_var("index_i",ncells,index_i)
    call ncfile%get_var("index_j",ncells,index_j)
  call ncfile%close()

  call ncfile%open('sfc_data.nc',"r",200,ierr)
  if(ierr>0) return
     call ncfile%get_dim("xaxis_1",nlon)
     call ncfile%get_dim("yaxis_1",nlat)
     write(*,*) 'FV3 surface nlon,nlat=',nlon,nlat

     allocate(tsfc(nlon,nlat))
     call ncfile%get_var("tsfc",nlon,nlat,tsfc)
  call ncfile%close()

  do n=1,ncells
     if(lakemask(n)==1 .or. lakemask(n)==3 .or. lakemask(n)==5) then
        i=index_i(n)
        j=index_j(n)
        if(( i >=1 .and. i <=nlon ) .and. (j >=1 .and. j <=nlat)) then
           sst(n)=tsfc(i,j)
           surftemp(n)=tsfc(i,j)
        else
           write(*,*) "i,j is out of fv3 domain=",n,i,j,nlon,nlat
        endif
     endif
  enddo

  deallocate(lakemask)
  deallocate(index_i)
  deallocate(index_j)
  deallocate(tsfc)
end subroutine
