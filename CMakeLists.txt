cmake_minimum_required(VERSION 3.8)

project(Flint)

include(CTest)

if(WIN32)
    add_definitions(-DWIN32_LEAN_AND_MEAN)
endif(WIN32)

if(MSVC)
    add_definitions(-DNOMINMAX)
endif(MSVC)

# The previous versions of CMake does not come with FindSQLite3.cmake.
if(CMAKE_VERSION VERSION_LESS 3.14)
    set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")
endif(CMAKE_VERSION VERSION_LESS 3.14)

find_package(SQLite3 REQUIRED)

find_package(LibXml2 REQUIRED)
include_directories("${LIBXML2_INCLUDE_DIR}")

add_definitions(-DBOOST_ALL_DYN_LINK)
set(Boost_NO_BOOST_CMAKE ON)
find_package(Boost 1.70 REQUIRED COMPONENTS
             filesystem
             program_options
             random
             system
             unit_test_framework
             )
include_directories("${Boost_INCLUDE_DIRS}")

find_package(Protobuf REQUIRED)
include_directories("${Protobuf_INCLUDE_DIRS}")
add_definitions(-DPROTOBUF_USE_DLLS)

find_package(wxWidgets REQUIRED COMPONENTS
             core base adv html xml aui propgrid)
include(${wxWidgets_USE_FILE})

find_package(Ceres REQUIRED)
include_directories("${CERES_INCLUDE_DIRS}")

find_library(LIBZMQ
             NAMES libzmq libzmq-v141-mt-4_3_1
             )
if(NOT LIBZMQ)
    message(FATAL_ERROR "failed to find libzmq")
endif(NOT LIBZMQ)

find_library(CZMQ
             NAMES czmq
             )
if(NOT CZMQ)
    message(FATAL_ERROR "failed to find czmq")
endif(NOT CZMQ)

find_library(CLIBSEDML
             NAMES sedml
             )
if(NOT CLIBSEDML)
    message(FATAL_ERROR "failed to find clibsedml")
endif(NOT CLIBSEDML)

find_library(LIBMICROHTTPD
             NAMES libmicrohttpd libmicrohttpd-dll
             )
if(NOT LIBMICROHTTPD)
    message(FATAL_ERROR "failed to find libmicrohttpd")
endif(NOT LIBMICROHTTPD)

find_library(LIBSBML
             NAMES libsbml
             )
if(NOT LIBSBML)
    message(FATAL_ERROR "failed to find libsbml")
endif(NOT LIBSBML)

find_library(SOSLIB
             NAMES ODES
             )
if(NOT SOSLIB)
    message(FATAL_ERROR "failed to find SBML_odeSolver")
endif(NOT SOSLIB)

find_library(SUNDIALS_ARKODE
             NAMES sundials_arkode
             )
if(NOT SUNDIALS_ARKODE)
    message(FATAL_ERROR "failed to find sundials_arkode")
endif(NOT SUNDIALS_ARKODE)

find_library(SUNDIALS_CVODE
             NAMES sundials_cvode
             )
if(NOT SUNDIALS_CVODE)
    message(FATAL_ERROR "failed to find sundials_cvode")
endif(NOT SUNDIALS_CVODE)

find_library(SUNDIALS_CVODES
             NAMES sundials_cvodes
             )
if(NOT SUNDIALS_CVODES)
    message(FATAL_ERROR "failed to find sundials_cvodes")
endif(NOT SUNDIALS_CVODES)

find_library(SUNDIALS_IDA
             NAMES sundials_ida
             )
if(NOT SUNDIALS_IDA)
    message(FATAL_ERROR "failed to find sundials_ida")
endif(NOT SUNDIALS_IDA)

find_library(SUNDIALS_IDAS
             NAMES sundials_idas
             )
if(NOT SUNDIALS_IDAS)
    message(FATAL_ERROR "failed to find sundials_idas")
endif(NOT SUNDIALS_IDAS)

find_library(SUNDIALS_KINSOL
             NAMES sundials_kinsol
             )
if(NOT SUNDIALS_KINSOL)
    message(FATAL_ERROR "failed to find sundials_kinsol")
endif(NOT SUNDIALS_KINSOL)

find_library(SUNDIALS_NVECSERIAL
             NAMES sundials_nvecserial
             )
if(NOT SUNDIALS_NVECSERIAL)
    message(FATAL_ERROR "failed to find sundials_nvecserial")
endif(NOT SUNDIALS_NVECSERIAL)

include_directories("include")

add_subdirectory(src)
add_subdirectory(test)
