TESTS = random.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s all check' >> $@
	chmod +x $@

random.mk: @top_srcdir@/test/isdls/random.mk.in
	m4 $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.txt *.isd

check_SCRIPTS = $(TESTS) $(TESTS:.sh=.mk)
