list(APPEND src_cloudanalysis
  pbl_height.f90
  rapidrefresh_cldsurf_mod.f90
  namelist_mod.f90
  get_mpas_bk_mod.f90
  cloudanalysis_mpas_driver.f90)

add_executable(mpas_nonvarcldana.exe ${src_cloudanalysis})
target_link_libraries(mpas_nonvarcldana.exe PRIVATE ${PROJECT_NAME}::kinds
					    PRIVATE ${PROJECT_NAME}::constants
					    PRIVATE ${PROJECT_NAME}::mpasio
					    PRIVATE ${PROJECT_NAME}::nonvar_cld_lib
                                            PRIVATE NetCDF::NetCDF_Fortran
                                            PRIVATE MPI::MPI_Fortran)

install(
  TARGETS mpas_nonvarcldana.exe
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

add_subdirectory(NonVarCldLib)
