dnl run `autoreconf -i` to generate a configure script. 
dnl Then run ./configure to generate a Makefile.
dnl Finally run make to generate the project.
AC_INIT([wisp], [0.5.9],
        [arne_bab@web.de])
# Check for programs I need for my build
AC_CANONICAL_TARGET
AC_ARG_VAR([guile], [How to call GNU Guile.])
AC_CHECK_TARGET_TOOL([guile], [guile], [no])
AS_IF([test "x$guile" = "xno"],
      [AC_MSG_ERROR([cannot find GNU Guile.])])
AC_ARG_VAR([python3], [How to call Python 3.])
AC_CHECK_TARGET_TOOL([python3], [python3], [no])
AS_IF([test "x$python3" = "xno"],
      [AC_MSG_ERROR([cannot find Python 3 which is required for bootstrapping.])])
# Run automake
# Use GNU style. Note that ChangeLog is created on every commit 
# by a commit hook in .hg/hgrc
#     [hooks]
#     post-commit = hg log --style changelog > ChangeLog
AM_INIT_AUTOMAKE([gnu])
AM_MAINTAINER_MODE([enable])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT