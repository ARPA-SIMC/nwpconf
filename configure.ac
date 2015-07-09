dnl Process this file with autoconf to produce a configure script.

AC_INIT(NWPconf, 1.0, [dcesari@arpa.emr.it])
dnl AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([configure.ac])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_MACRO_DIR([m4])



AC_ARG_ENABLE(doxydoc, AC_HELP_STRING([--disable-doxydoc],
  [disable generation of doxygen documentation]),,[enableval=yes])dnl default enable
AM_CONDITIONAL(DOXYDOC, test "x$enableval" = "xyes")


if test "x$prefix" = "xNONE"; then
AC_DEFINE_UNQUOTED(PREFIX, "$ac_default_prefix", [install prefix])
else
AC_DEFINE_UNQUOTED(PREFIX, "$prefix", [install prefix])
fi

pkgdocdir='${datadir}/doc/${PACKAGE}'
AC_SUBST(pkgdocdir)

AC_CONFIG_FILES([
Makefile
bin/Makefile
doc/Doxyfile
doc/Makefile
])
dnl examples/Makefile

AC_OUTPUT
