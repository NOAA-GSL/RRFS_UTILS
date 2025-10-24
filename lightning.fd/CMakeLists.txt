list(APPEND src_lightning
  module_lightning_bufr_io.f90
  module_Check_Lght_QC.f90
  module_netCDFsub_lightning.f90
  process_Lightning.f90)

add_executable(process_Lightning.exe ${src_lightning})
target_link_libraries(process_Lightning.exe PRIVATE ${PROJECT_NAME}::kinds
		 			    PRIVATE ${PROJECT_NAME}::mpasio
		 			    PRIVATE ${PROJECT_NAME}::wps_map_utils
					    PRIVATE ${PROJECT_NAME}::map_proj_helper
                                            PRIVATE NetCDF::NetCDF_Fortran
                                            PRIVATE bufr::bufr_4
                                            PRIVATE w3nco::w3nco_4
                                            PRIVATE MPI::MPI_Fortran)

install(
  TARGETS process_Lightning.exe
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})
