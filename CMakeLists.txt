# The Fortran binding to the corvus-regex-ecma262 C library, for CMake projects (fpm.toml builds it for fpm).
#
#   find_package(corvus_regex_ecma262_fortran)  after installing, or add_subdirectory() on this directory
#   target_link_libraries(app PRIVATE corvus_regex_ecma262::fortran)
#
# The C library comes from its own CMake package (a release archive of corvus-regex-ecma262-rs, 0.2.1 or later): give
# its directory in CMAKE_PREFIX_PATH or corvus_regex_ecma262_DIR.
cmake_minimum_required(VERSION 3.16)
project(corvus_regex_ecma262_fortran VERSION 0.2.1 LANGUAGES Fortran)

find_package(corvus_regex_ecma262 0.2.1 REQUIRED)

add_library(corvus_regex_ecma262_fortran src/corvus_regex_ecma262.f90)
add_library(corvus_regex_ecma262::fortran ALIAS corvus_regex_ecma262_fortran)
set_target_properties(corvus_regex_ecma262_fortran PROPERTIES
  Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules"
  EXPORT_NAME fortran)
target_include_directories(corvus_regex_ecma262_fortran PUBLIC
  "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules>"
  "$<INSTALL_INTERFACE:include/corvus_regex_ecma262_fortran>")
target_link_libraries(corvus_regex_ecma262_fortran PUBLIC corvus_regex_ecma262::corvus_regex_ecma262)

option(CORVUS_REGEX_ECMA262_FORTRAN_TESTS "Build the tests (ctest)" ${PROJECT_IS_TOP_LEVEL})
if(CORVUS_REGEX_ECMA262_FORTRAN_TESTS)
  enable_testing()
  add_executable(api_test test/api_test.f90)
  target_link_libraries(api_test PRIVATE corvus_regex_ecma262_fortran)
  add_test(NAME api_test COMMAND api_test)
  add_executable(basic_example example/basic.f90)
  target_link_libraries(basic_example PRIVATE corvus_regex_ecma262_fortran)
  add_test(NAME basic_example COMMAND basic_example)
endif()

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)
install(TARGETS corvus_regex_ecma262_fortran EXPORT corvus_regex_ecma262_fortran_targets
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})
install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules/" DESTINATION include/corvus_regex_ecma262_fortran)
install(EXPORT corvus_regex_ecma262_fortran_targets NAMESPACE corvus_regex_ecma262::
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/corvus_regex_ecma262_fortran)
configure_package_config_file(cmake/corvus_regex_ecma262_fortranConfig.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/corvus_regex_ecma262_fortranConfig.cmake"
  INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/corvus_regex_ecma262_fortran)
write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/corvus_regex_ecma262_fortranConfigVersion.cmake"
  COMPATIBILITY SameMinorVersion)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/corvus_regex_ecma262_fortranConfig.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/corvus_regex_ecma262_fortranConfigVersion.cmake"
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/corvus_regex_ecma262_fortran)
