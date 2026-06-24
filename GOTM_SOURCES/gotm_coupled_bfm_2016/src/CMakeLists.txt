cmake_minimum_required(VERSION 3.0)

message(STATUS "code/src/CMakeLists.txt")

if(NOT PROJECT_NAME STREQUAL gotm)
   message(WARNING "You are using ${CMAKE_CURRENT_LIST_DIR} as source directory. This is deprecated. Please use its parent directory instead.")
   project(gotm VERSION 5.3.0 LANGUAGES Fortran)
   include("${CMAKE_CURRENT_LIST_DIR}/../CMakeLists.txt")
   return()
endif()

# ----------------------------
# Directory wide settings
# ----------------------------

# Common directory for Fortran modules
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules")

# Compiler flags
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
message(STATUS "GOTM CMAKE: setting compiler flags for GNU compiler")
  add_definitions(-DGFORTRAN)
  add_compile_options(-ffree-line-length-none)
  add_compile_options(-O0)
  #add_compile_options(-fdefault-real-8 -fdefault-double-8 -ffast-math -ftree-vectorize -funroll-loops --param max-unroll-times=4 -march=skylake -mtune=skylake)
  #set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -g -O0 -Wall -fbounds-check -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow -Wno-unused-parameter -Wno-unused-variable -Wno-unused-dummy-argument" )# -fbounds-check")
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -g -O0")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Cray")
  add_compile_options(-N 1023)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
  if(WIN32)
    add_compile_options("$<$<CONFIG:DEBUG>:/Od>")
  endif()
endif()

# Include directories
include_directories("${CMAKE_CURRENT_LIST_DIR}/../include")
include_directories("${CMAKE_CURRENT_BINARY_DIR}/modules")

# Preprocessor definitions
add_definitions(-DFORTRAN95)
if(GOTM_USE_NetCDF)
   add_definitions(-DNETCDF_FMT)
endif()
if(GOTM_USE_FABM)
  add_definitions(-D_FABM_)
endif()
if(GOTM_USE_BFM)
  add_definitions(-DBFM_GOTM)
endif()
if(GOTM_USE_STIM)
  add_definitions(-D_ICE_)
endif()
if(GOTM_USE_SEAGRASS)
   add_definitions(-DSEAGRASS)
endif()
if(GOTM_EXTRA_OUTPUT)
  add_definitions(-DEXTRA_OUTPUT)
endif()

# ----------------------------
# Targets
# ----------------------------

add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/meanflow meanflow)
#if(NOT GOTM_USE_BFM)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/airsea airsea)
#endif()
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/util util)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/turbulence turbulence)


if(GOTM_USE_FABM)
   add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/fabm fabm)
endif()

if(GOTM_USE_CVMIX)
    add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/cvmix gotm_cvmix)
endif()

message(STATUS "GOTM CMAKE: GOTM_USE_BFM=${GOTM_USE_BFM}")
message(STATUS "GOTM CMAKE: BFMDIR=$ENV{BFMDIR}")
if(GOTM_USE_BFM)
  find_path(BFM_BASE NAMES src/BFM PATHS $ENV{BFMDIR} DOC "Path to BFM source directory.")
  message(STATUS "GOTM CMAKE: BFM_BASE=${BFM_BASE}")
  set( BFMPATH "${BFM_BASE}/src/gotm/" )
else()
  set( BFMPATH "" )
  mark_as_advanced(FORCE BFM_PREFIX BFM_BASE)
endif()

if(GOTM_USE_BFM)
  message( STATUS  "GOTM CMAKE: in GOTM_USE_BFM")
  add_definitions(-DNETCDF_FMT -DREAL_4B=real\(4\))
  include_directories("${NetCDF_INCLUDE_DIRS}")
  mark_as_advanced(CLEAR BFM_BASE)
  if(BFM_BASE)
     message( STATUS  "GOTM CMAKE: in BFM_BASE")
     # Build BFM from source by including its CMakeLists.txt directory.
     set(BFM_EMBED_VERSION ${BFM_EMBED_VERSION} CACHE BOOL "Embed BFM version information" FORCE)
     set(BFM_FORCED_HOST gotm)
     # do a cmake with  the CmakeLists.txt in $BFM_BASE_src 
     add_subdirectory(${BFM_BASE}/src ${CMAKE_CURRENT_BINARY_DIR}/bfm_src)
     message( STATUS  "GOTM CMAKE: bfm_up  ${bfm_up}")
     message( STATUS  "GOTM CMAKE: bfm_sources found in  ${BFM_BASE}/src")
     mark_as_advanced(FORCE BFM_PREFIX BFM_EMBED_VERSION)
     include_directories(${CMAKE_CURRENT_BINARY_DIR}/bfm_src)

     # coupling routines
     add_subdirectory(${BFM_BASE}/src/gotm ${CMAKE_CURRENT_BINARY_DIR}/bfm_cpl)
     include_directories(${CMAKE_CURRENT_BINARY_DIR}/bfm_cpl)
     message( STATUS  "GOTM CMAKE: bfm_cpl  ${bfm_cpl}")

  endif(BFM_BASE)
endif()

if(GOTM_BUILD_LIBRARIES_ONLY)
   message(STATUS "GOTM CMAKE: GOTM_BUILD_LIBRARIES_ONLY")
   return()
endif()

#if(GOTM_USE_BFM)
#add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/airsea airsea)
#endif()
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/observations observations)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/input input)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/config config)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/stokes_drift stokes_drift)

if(GOTM_USE_SEAGRASS)
   add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/extras/seagrass seagrass)
endif()

add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/gotm gotmlib)


# Build GOTM executable.
message( STATUS  "adding executable")
add_executable(gotm_exe
   ${CMAKE_CURRENT_LIST_DIR}/gotm/main.F90
)
message( STATUS  "set_property")
set_property(TARGET gotm_exe PROPERTY RUNTIME_OUTPUT_NAME "gotm")
target_link_libraries(gotm_exe PRIVATE gotm)

if(GOTM_USE_BFM)
  message( STATUS  "link libraries bfm_up ${bfm_up}")
  message( STATUS  "link libraries bfm_cpl ${bfm_cpl}")
  add_dependencies(global_mod util)  #GetDelta.F90 depends on Time.F90
  add_dependencies(gotm bio)
  target_link_libraries(gotm_exe PRIVATE ${bfm_up} ${bfm_cpl})
endif()

# Install GOTM executable
message( STATUS  "install executable step 1")
install(TARGETS gotm_exe RUNTIME DESTINATION bin)

message( STATUS  "install executable step 2")
install(EXPORT gotmConfig DESTINATION cmake NAMESPACE gotm::)


