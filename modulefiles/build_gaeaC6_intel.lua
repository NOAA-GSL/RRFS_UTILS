help([[
This module loads libraries for building the RRFS workflow on
the NOAA RDHPC machine Hera using Intel-2023.2.0
]])

whatis([===[Loads libraries needed for building the RRFS workflow on Gaea C6 ]===])

prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/c6/spack-stack-1.9.2/envs/ue-intel-2023.2.0/install/modulefiles/Core")
prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/c6/modulefiles")

stack_intel_ver=os.getenv("stack_intel_ver") or "2023.2.0"
load(pathJoin("stack-intel", stack_intel_ver))

stack_cray_mpich_ver=os.getenv("stack_cray_mpich_ver") or "8.1.30"
load(pathJoin("stack-cray-mpich", stack_cray_mpich_ver))

cmake_ver=os.getenv("cmake_ver") or "3.27.9"
load(pathJoin("cmake", cmake_ver))

setenv("jasper_ver", "2.0.32")
setenv("zlib_ver", "1.2.13")
setenv("libpng_ver", "1.6.37")
setenv("g2_ver", "3.5.1")
setenv("g2tmpl_ver", "1.13.0")
setenv("w3emc_ver", "2.10.0")
setenv("w3nco_ver", "2.4.1")
setenv("nco_ver", "5.2.4")
setenv("pio_ver", "2.6.2")
setenv("bufr_ver", "12.1.0")

load("rrfs_common")
load(pathJoin("wgrib2", os.getenv("wgrib2_ver") or "3.6.0"))
load("prod_util/2.1.1")
load("fms/2024.02")

unload("darshan-runtime")
unload("cray-libsci")

setenv("CMAKE_C_COMPILER","cc")
setenv("CMAKE_CXX_COMPILER","CC")
setenv("CMAKE_Fortran_COMPILER","ftn")
setenv("CMAKE_Platform","gaea.intel")
setenv("BLENDINGPYTHON","/contrib/miniconda3/4.5.12/envs/pygraf/bin/python")

setenv("CC","cc")
setenv("CXX","CC")
setenv("FC","ftn")
