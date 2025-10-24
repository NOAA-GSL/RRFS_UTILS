list(APPEND src_larccld
  module_larccld_utils.f90
  module_larccld_bufr_io.f90
  process_NASALaRC_cloud.f90)

add_executable(process_larccld.exe ${src_larccld})
target_link_libraries(process_larccld.exe PRIVATE ${PROJECT_NAME}::kinds
	                                  PRIVATE ${PROJECT_NAME}::constants
					  PRIVATE ${PROJECT_NAME}::mpasio
					  PRIVATE ${PROJECT_NAME}::wps_map_utils
					  PRIVATE ${PROJECT_NAME}::map_proj_helper
                                          PRIVATE NetCDF::NetCDF_Fortran
                                          PRIVATE bufr::bufr_4
                                          PRIVATE MPI::MPI_Fortran)

install(
  TARGETS process_larccld.exe
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})
