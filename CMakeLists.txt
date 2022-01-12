cmake_minimum_required(VERSION 3.19)

if (NOT DEFINED Fortran_COMPILER)
  message(FATAL_ERROR " please specify a MPI Fortran compiler with \
-DFortran_COMPILER=<compiler> (eg. mpif90)")
endif()
set(CMAKE_Fortran_COMPILER ${Fortran_COMPILER})


project(QUICKLB LANGUAGES Fortran)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -ffixed-form -fimplicit-none") # Shared Flags
if (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp") #Preprocessor
else()
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp") #Preprocessor
endif()

if (CMAKE_BUILD_TYPE MATCHES "DEBUG")
if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Og -Wall -Wextra -fcheck=all \
-Wno-compare-reals -fbacktrace -ffpe-trap=invalid,zero,overflow")
endif()
else()
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3")
endif()


file(GLOB fortran_source "lb/*.f")

add_library("quicklb" SHARED ${fortran_source})

message(STATUS "Fortran compiler : " ${CMAKE_Fortran_COMPILER})
message(STATUS "Fortran compiler flags : ${CMAKE_Fortran_FLAGS}")
