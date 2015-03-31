EXECUTABLE = @top_builddir@/src/isdwc

ISDRAND_EXECUTABLE = @top_builddir@/src/isdrand

TESTS = random.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo '$(MAKE) -f $< clean all check' >> $@
	chmod +x $@

random.mk: @top_srcdir@/test/isdwc/random.mk.in
	m4 -DEXECUTABLE=$(EXECUTABLE) -DISDRAND_EXECUTABLE=$(ISDRAND_EXECUTABLE) $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.txt *.isd

check_SCRIPTS = $(TESTS) $(TESTS:.sh=.mk)
