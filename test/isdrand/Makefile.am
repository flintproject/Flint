TESTS = seed.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s clean all check' >> $@
	chmod +x $@

seed.mk: @top_srcdir@/test/isdrand/seed.mk.in
	m4 -DSKIP=28 $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.cmp *.isd

check_SCRIPTS = $(TESTS) $(TESTS:.sh=.mk)
