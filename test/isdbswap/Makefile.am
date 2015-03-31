EXECUTABLE = @top_builddir@/src/isdbswap

ISDRAND_EXECUTABLE = @top_builddir@/src/isdrand

TESTS = roundtrip.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo '$(MAKE) -f $< clean all check' >> $@
	chmod +x $@

roundtrip.mk: @top_srcdir@/test/isdbswap/roundtrip.mk.in
	m4 -DEXECUTABLE=$(EXECUTABLE) -DISDRAND_EXECUTABLE=$(ISDRAND_EXECUTABLE) $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.cmp *.isd

check_SCRIPTS = $(TESTS) $(TESTS:.sh=.mk)
