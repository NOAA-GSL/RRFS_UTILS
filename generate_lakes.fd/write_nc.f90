SUBROUTINE write_nc(ncells, lakemask_fv3, lakemask_mpas, index_i, index_j, xlon_mpas, ylat_mpas)

  use netcdf
!  use netcdf, only: nf90_noerr
!  use netcdf, only: nf90_put_var,nf90_inq_varid
!
  implicit none
!
  integer,intent(in) :: ncells
  real,intent(in)    :: xlon_mpas(ncells)
  real,intent(in)    :: ylat_mpas(ncells)
!
!  MPAS lakemask
  integer,intent(in)  :: lakemask_fv3(ncells)  ! MPAS lake mask from RRFSv1 FV3 lake mask
  integer,intent(in)  :: lakemask_mpas(ncells)  ! MPAS lake mask: include great lakes and water point in land
  integer,  intent(in)  :: index_i(ncells) ! MPAS lake point link to the nearest FV3 lake point
  integer,  intent(in)  :: index_j(ncells) ! MPAS lake point link to the nearest FV3 lake point
!
! array
  real,allocatable :: tmpd1r4(:)
  integer,allocatable :: tmpd1i4(:)

  integer :: startloc(1)
  integer :: countloc(1)
  integer :: varid, dimids(4), start(4), count(4), chunksizes(4)
  character(len=20) :: local_varname
  integer :: cdfid,dimid_ncells
  integer :: oldMode

  integer :: i,j,iret
!
  iret=nf90_create("mpas_lake_mask.nc",IOR(nf90_netcdf4, nf90_clobber), ncid=cdfid)
  iret=nf90_set_fill(cdfid, NF90_NOFILL, oldMode)

  iret=nf90_redef(cdfid)
  iret=nf90_def_dim(cdfid, "ncells",  ncells,   dimid_ncells)
  dimids(1:4) = [dimid_ncells, 0,0,0]
  chunksizes(1:4) = [ncells,1,1,1]
  iret=nf90_def_var(cdfid, "latCell", NF90_FLOAT, dimids(1), varid)
  iret=nf90_put_att(cdfid, varid, "long_name", "cell latitude in radians")
  iret=nf90_def_var(cdfid, "lonCell", NF90_FLOAT, dimids(1), varid)
  iret=nf90_put_att(cdfid, varid, "long_name", "cell longitude in radians")
  iret=nf90_def_var(cdfid, "lakemask", NF90_INT, dimids(1), varid)
  iret=nf90_put_att(cdfid, varid, "long_name", "MPAS lake mask: 0=ocean, 1=lake, 2=land, 3=lake nearby FV3 lake, 4= lake not nearby Fv3 lake, 5 = great lakes ")
  iret=nf90_def_var(cdfid, "lakemask_fv3", NF90_INT, dimids(1), varid)
  iret=nf90_put_att(cdfid, varid, "long_name", "MPAS lake mask from FV3 lake mask:  0=ocean,1=lake,2=land")
  iret=nf90_def_var(cdfid, "index_i", NF90_INT, dimids(1), varid)
  iret=nf90_put_att(cdfid, varid, "long_name", "index i to find nearby FV3 grid point")
  iret=nf90_def_var(cdfid, "index_j", NF90_INT, dimids(1), varid)
  iret=nf90_put_att(cdfid, varid, "long_name", "index j to find nearby FV3 grid point")

  iret=nf90_enddef(cdfid)

  startloc(1)=1
  countloc(1)=ncells

  local_varname="latCell"
  iret=nf90_inq_varid(cdfid,trim(adjustl(local_varname)),varid)
  iret=nf90_put_var(cdfid,varid,values=ylat_mpas,start=startloc,count=countloc)

  local_varname="lonCell"
  iret=nf90_inq_varid(cdfid,trim(adjustl(local_varname)),varid)
  iret=nf90_put_var(cdfid,varid,values=xlon_mpas,start=startloc,count=countloc)

  local_varname="lakemask"
  iret=nf90_inq_varid(cdfid,trim(adjustl(local_varname)),varid)
  write(*,*) 'write lake mask MPAS=',maxval(lakemask_mpas),minval(lakemask_mpas)
  iret=nf90_put_var(cdfid,varid,values=lakemask_mpas,start=startloc,count=countloc)

  local_varname="lakemask_fv3"
  iret=nf90_inq_varid(cdfid,trim(adjustl(local_varname)),varid)
  write(*,*) 'write lake mask FV3=',maxval(lakemask_fv3),minval(lakemask_fv3)
  iret=nf90_put_var(cdfid,varid,values=lakemask_fv3,start=startloc,count=countloc)

  local_varname="index_i"
  iret=nf90_inq_varid(cdfid,trim(adjustl(local_varname)),varid)
  iret=nf90_put_var(cdfid,varid,values=index_i,start=startloc,count=countloc)

  local_varname="index_j"
  iret=nf90_inq_varid(cdfid,trim(adjustl(local_varname)),varid)
  iret=nf90_put_var(cdfid,varid,values=index_j,start=startloc,count=countloc)

  iret=nf90_close(cdfid)

END SUBROUTINE write_nc

