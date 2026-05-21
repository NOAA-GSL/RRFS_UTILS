list(APPEND src_update_sst
  grib2_read_mod.f90
  process_SST.f90
  sstGlobal2MPAS.f90
  update_SST_MPAS.f90)

add_executable(process_update_sst.exe ${src_update_sst})
target_link_libraries(process_update_sst.exe PRIVATE ${PROJECT_NAME}::ncio
                                            PRIVATE NetCDF::NetCDF_Fortran
                                            PRIVATE g2::g2_4
                                            PRIVATE g2tmpl::g2tmpl
                                            PRIVATE w3nco::w3nco_4
                                            PRIVATE bacio::bacio_4
                                            PRIVATE MPI::MPI_Fortran)

if(OpenMP_Fortran_FOUND)
  target_link_libraries(process_update_sst.exe PRIVATE OpenMP::OpenMP_Fortran)
endif()

install(
  TARGETS process_update_sst.exe
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})
