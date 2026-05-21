list(APPEND src_generate_lakes
  generate_lakes.f90
  write_nc.f90)

add_executable(process_generate_lakes.exe ${src_generate_lakes})
target_link_libraries(process_generate_lakes.exe PRIVATE ${PROJECT_NAME}::ncio
	                                         PRIVATE ${PROJECT_NAME}::esggrid_util
                                            PRIVATE ${PROJECT_NAME}::pesglib
                                            PRIVATE NetCDF::NetCDF_Fortran
                                            PRIVATE w3nco::w3nco_4
                                            PRIVATE MPI::MPI_Fortran)

if(OpenMP_Fortran_FOUND)
  target_link_libraries(process_generate_lakes.exe PRIVATE OpenMP::OpenMP_Fortran)
endif()

install(
  TARGETS process_generate_lakes.exe
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})
