#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([Flint], [1.7.0], [flint@oist.jp])
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
      [WARNINGCFLAGS="-Wall -Wextra -Wcast-align -Wcast-qual -Wpointer-arith -Wmissing-prototypes -Wstrict-prototypes -Wnested-externs -Wwrite-strings"])
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
AC_ARG_ENABLE(thread,
      [AS_HELP_STRING([--enable-thread],
        [Enable thread support. Available values: posix, windows.
         Default: no])],
      [],
      [enable_thread=no])
AS_IF([test "x$enable_thread" = "xposix"],[AX_PTHREAD])
AS_IF([test "x$enable_thread" = "xposix"],[AC_DEFINE([USING_PTHREAD],[1],['Using POSIX Threads (pthreads)'])],
      [test "x$enable_thread" = "xwindows"],[AC_DEFINE([USING_WINTHREAD],[1],['Using Windows Threads'])],
      [test "x$enable_thread" != "xno"],[AC_MSG_ERROR([unknown type of thread: $enable_thread])])

BOOST_REQUIRE([1.61.0])
BOOST_FILESYSTEM
BOOST_HASH
BOOST_MATH
BOOST_PROGRAM_OPTIONS
BOOST_UUID
BOOST_XPRESSIVE
BOOST_TEST

# Boost.Interprocess uses pthread system calls in POSIX systems;
# see <http://www.boost.org/doc/libs/1_61_0/doc/html/interprocess.html>.
AC_SEARCH_LIBS([shm_open],[rt])

AC_ARG_ENABLE(release,
        [AS_HELP_STRING([--enable-release],
                [Enable the release (production) mode. Default: no])],
        [],
        [enable_release=no])
AS_IF([test "x$enable_release" != xno],[AC_DEFINE([NDEBUG],[1],[Defined as 1 if the release mode is enabled])])

AC_ARG_ENABLE(tcp,
        [AS_HELP_STRING([--enable-tcp],
                [Enable TCP support. Default: yes])],
        [],
        [enable_tcp=yes])
AS_IF([test "x$enable_tcp" != xno],[BOOST_ASIO])
AS_IF([test "x$enable_tcp" != xno],[AC_DEFINE([ENABLE_TCP],[1],[Define to 1 if TCP support is enabled])])
AM_CONDITIONAL([ENABLE_TCP],[test "x$enable_tcp" != xno])

PKG_CHECK_MODULES([CLIBSEDML],[clibsedml])
PKG_CHECK_MODULES([LIBSBML],[libsbml])
PKG_CHECK_MODULES([LIBZIP],[libzip >= 1.1.1])
PKG_CHECK_MODULES([SOSLIB],[libODES])
PKG_CHECK_MODULES([PROTOBUF],[protobuf])

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
AC_FUNC_FORK
# The following 2 macros prevent cross-compilation with MinGW
#AC_FUNC_MALLOC
#AC_FUNC_REALLOC
AC_CHECK_FUNCS([floor gethostname gettimeofday memset mkstemp pow putenv socket sqrt strerror strstr])

AC_CONFIG_FILES([Makefile
                 src/Makefile
                 test/Makefile
                 test/base/Makefile
                 test/cas/Makefile
                 test/cellml/Makefile
                 test/compiler/Makefile
                 test/file/Makefile
                 test/flint/Makefile
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
                 test/sys/Makefile
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
