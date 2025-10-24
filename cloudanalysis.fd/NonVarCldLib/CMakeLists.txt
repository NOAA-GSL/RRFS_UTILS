list(APPEND src_nonvar_cld_lib
  adaslib.f90
  ARPS_cldLib.f90
  BckgrndCC.f90
  build_missing_REFcone.f90
  cloudCover_NESDIS.f90
  cloudCover_Surface.f90
  cloudLayers.f90
  cloudLWC.f90
  cloud_saturation.f90
  cloudType.f90
  convert_lghtn2ref.f90
  hydro_mxr_thompson.f90
  map_ctp.f90
  map_ctp_lar.f90
  mthermo.f90
  PrecipType.f90
  read_Lightning_cld.f90
  read_nasalarc_cld.f90
  read_radar_ref.f90
  read_Surface.f90
  vinterp_radar_ref.f90)

add_library(nonvar_cld_lib STATIC ${src_nonvar_cld_lib})
add_library(${PROJECT_NAME}::nonvar_cld_lib ALIAS nonvar_cld_lib)
target_link_libraries(nonvar_cld_lib PRIVATE ${PROJECT_NAME}::kinds
                                     PRIVATE ${PROJECT_NAME}::constants)
set_target_properties(nonvar_cld_lib PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
target_include_directories(nonvar_cld_lib PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
                                                 $<INSTALL_INTERFACE:include>)
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include DESTINATION ${CMAKE_INSTALL_PREFIX})

install(
  TARGETS nonvar_cld_lib
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})
