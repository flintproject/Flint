EXECUTABLE = @top_builddir@/src/isdrand

TESTS = seed.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo '$(MAKE) -f $< clean all check' >> $@
	chmod +x $@

seed.mk: @top_srcdir@/test/isdrand/seed.mk.in
	m4 -DEXECUTABLE=$(EXECUTABLE) -DSKIP=28 $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.cmp *.isd

check_SCRIPTS = $(TESTS) $(TESTS:.sh=.mk)
