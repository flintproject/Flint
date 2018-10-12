#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([Flint], [2.1.0], [tabe@fixedpoint.jp])
AC_CONFIG_SRCDIR([src/isd2csv-main.cc])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([-Wall -Wno-extra-portability -Werror foreign subdir-objects])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([config])
AC_LANG([C++])

# Checks for programs.

# check if our compiler is gcc/g++-compatible, and then use the WARNINGCFLAGS trick
# to append warning flags as follows:
# http://www.gnu.org/software/automake/manual/automake.html#Flag-Variables-Ordering
AC_PROG_CC
: ${WARNINGCFLAGS=""}
AS_IF([test "$GCC" = yes],
      [WARNINGCFLAGS="-Wall -Wextra -Wc++-compat -Wcast-align -Wcast-qual -Wpointer-arith -Wmissing-prototypes -Wstrict-prototypes -Wnested-externs -Wwrite-strings"])
AC_SUBST([WARNINGCFLAGS])
AC_PROG_CXX
: ${WARNINGCXXFLAGS=""}
AS_IF([test "$GXX" = yes],
      [WARNINGCXXFLAGS="-Wall -Wextra -Wcast-align -Wcast-qual -Wpointer-arith -Wmissing-declarations"])
AC_SUBST([WARNINGCXXFLAGS])

LT_INIT([win32-dll disable-static pic-only])
AC_PROG_INSTALL

AC_ARG_WITH([protoc],
        [AS_HELP_STRING([--with-protoc],
                [Specify path of protoc])])
AS_IF([test "x$with_protoc" = "x"],[AC_SUBST([PROTOC_EXECUTABLE],[protoc])],
      [test -f "$with_protoc"],[AC_SUBST([PROTOC_EXECUTABLE],[$with_protoc])],
      [AC_MSG_ERROR([could not find protoc, please specify its right location by --with-protoc])])

# Checks for libraries.
BOOST_REQUIRE([1.68.0])
BOOST_ASIO
BOOST_CHRONO
BOOST_FILESYSTEM
BOOST_HASH
BOOST_MATH
BOOST_PROGRAM_OPTIONS
BOOST_UUID
BOOST_XPRESSIVE
BOOST_TEST
boost_timer_save_LIBS=$LIBS
boost_timer_save_LDFLAGS=$LDFLAGS
LIBS="$LIBS $BOOST_CHRONO_LIBS"
LDFLAGS="$LDFLAGS $BOOST_CHRONO_LDFLAGS"
BOOST_FIND_LIBS([timer],[timer],[],[boost/timer.hpp],[boost::timer t;])
BOOST_TIMER_LIBS="$BOOST_TIMER_LIBS $BOOST_CHRONO_LIBS"
LIBS=$boost_timer_save_LIBS
LDFLAGS=$boost_timer_save_LDFLAGS

# Boost.Interprocess uses pthread system calls in POSIX systems;
# see <https://www.boost.org/doc/libs/1_68_0/doc/html/interprocess.html>.
AC_SEARCH_LIBS([shm_open],[rt])

AC_ARG_ENABLE(release,
        [AS_HELP_STRING([--enable-release],
                [Enable the release (production) mode. Default: no])],
        [],
        [enable_release=no])
AS_IF([test "x$enable_release" != xno],[AC_DEFINE([NDEBUG],[1],[Defined as 1 if the release mode is enabled])])

PKG_CHECK_MODULES([CLIBSEDML],[clibsedml >= 0.1.2])
PKG_CHECK_MODULES([LIBMICROHTTPD],[libmicrohttpd >= 0.9.33])
PKG_CHECK_MODULES([LIBSBML],[libsbml])
PKG_CHECK_MODULES([SOSLIB],[libODES])
PKG_CHECK_MODULES([PROTOBUF],[protobuf])
PKG_CHECK_MODULES([ZEROMQ],[libzmq >= 4.2.4])
PKG_CHECK_MODULES([CZMQ],[libczmq >= 4.1.1])

AC_ARG_WITH([ceres-solver],
        [AS_HELP_STRING([--with-ceres-solver],
                [Specify prefix of Ceres Solver])])
CERES_CPPFLAGS=
CERES_LIBS=-lceres
if test -d "$with_ceres_solver"; then
   CERES_CPPFLAGS="-I$with_ceres_solver/include"
   CERES_LIBS="-L$with_ceres_solver/lib $CERES_LIBS"
elif test ! -z "$with_ceres_solver"; then
    AC_MSG_ERROR([$with_ceres_solver is not a directory, please specify valid location of Ceres Solver])
fi
AC_SUBST([CERES_CPPFLAGS])
AC_SUBST([CERES_LIBS])

AC_ARG_WITH([sundials],
        [AS_HELP_STRING([--with-sundials],
                [Specify prefix of SUNDIALS])])
SUNDIALS_CFLAGS=
SUNDIALS_LIBS="-lsundials_arkode -lsundials_cvode -lsundials_cvodes -lsundials_ida -lsundials_kinsol -lsundials_nvecserial"
if test -d "$with_sundials"; then
    SUNDIALS_CFLAGS="-I$with_sundials/include"
    SUNDIALS_LIBS="-L$with_sundials/lib $SUNDIALS_LIBS"
elif test ! -z "$with_sundials"; then
    AC_MSG_ERROR([$with_sundials is not a directory, please specify valid location of SUNDIALS])
fi
AC_SUBST([SUNDIALS_CFLAGS])
AC_SUBST([SUNDIALS_LIBS])

AC_ARG_WITH([wxWidgets],
        [AS_HELP_STRING([--with-wxWidgets],
                [Specify prefix of wxWidgets])])
WXWIDGETS_CXXFLAGS=
WXWIDGETS_LIBS=
if test -d "$with_wxWidgets"; then
    WXWIDGETS_CXXFLAGS="`$with_wxWidgets/bin/wx-config --cxxflags`"
    WXWIDGETS_LIBS="`$with_wxWidgets/bin/wx-config --libs`"
elif test ! -z "$with_wxWidgets"; then
    AC_MSG_ERROR([$with_wxWidgets is not a directory, please specify valid location of wxWidgets])
fi
AC_SUBST([WXWIDGETS_CXXFLAGS])
AC_SUBST([WXWIDGETS_LIBS])

AM_PATH_XML2([2.7.6])

# Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h float.h stdint.h string.h sys/resource.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDC
AC_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_INT64_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UINT32_T

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
# The following 2 macros prevent cross-compilation with MinGW
#AC_FUNC_MALLOC
#AC_FUNC_REALLOC
AC_CHECK_FUNCS([floor gethostname gettimeofday memset mkstemp popen pow putenv socket sqrt strerror strstr])

AC_CONFIG_FILES([Makefile
                 src/Makefile
                 test/Makefile
                 test/base/Makefile
                 test/cas/Makefile
                 test/cellml/Makefile
                 test/compiler/Makefile
                 test/file/Makefile
                 test/flint/Makefile
                 test/gui/Makefile
                 test/job/Makefile
                 test/layout/Makefile
                 test/load/Makefile
                 test/mathml/Makefile
                 test/method/Makefile
                 test/phml/Makefile
                 test/phsp/Makefile
                 test/phz/Makefile
                 test/run/Makefile
                 test/runtime/Makefile
                 test/sbml/Makefile
                 test/sedml/Makefile
                 test/csv2isd/Makefile
                 test/isd2csv/Makefile
                 test/isdbswap/Makefile
                 test/isdcut/Makefile
                 test/isddiff/Makefile
                 test/isdhead/Makefile
                 test/isdls/Makefile
                 test/isdmerge/Makefile
                 test/isdrand/Makefile
                 test/isdsort/Makefile
                 test/isdstrip/Makefile
                 test/isdwc/Makefile])

AC_OUTPUT
