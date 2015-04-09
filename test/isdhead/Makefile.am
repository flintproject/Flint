EXECUTABLE = @top_builddir@/src/isdhead

ISDRAND_EXECUTABLE = @top_builddir@/src/isdrand

TESTS = random.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo '$(MAKE) -f $< -s clean all check' >> $@
	chmod +x $@

random.mk: @top_srcdir@/test/isdhead/random.mk.in
	m4 -DEXECUTABLE=$(EXECUTABLE) -DISDRAND_EXECUTABLE=$(ISDRAND_EXECUTABLE) $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.isd

check_SCRIPTS = $(TESTS) $(TESTS:.sh=.mk)
