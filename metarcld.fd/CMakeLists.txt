list(APPEND src_metarcld
  module_cld_parm_array.f90
  module_reorg_metar_cloud.f90
  module_read_prepbufr_metar.f90
  process_metar_cloud.f90)

add_executable(process_metarcld.exe ${src_metarcld})
target_link_libraries(process_metarcld.exe PRIVATE ${PROJECT_NAME}::kinds
					   PRIVATE ${PROJECT_NAME}::mpasio
					   PRIVATE ${PROJECT_NAME}::wps_map_utils
					   PRIVATE ${PROJECT_NAME}::map_proj_helper
                                           PRIVATE NetCDF::NetCDF_Fortran
                                           PRIVATE bufr::bufr_4
                                           PRIVATE w3nco::w3nco_4
                                           PRIVATE MPI::MPI_Fortran)

install(
  TARGETS process_metarcld.exe
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})
