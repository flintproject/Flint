TESTS = roundtrip.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s all check' >> $@
	chmod +x $@

roundtrip.mk: @top_srcdir@/test/isdbswap/roundtrip.mk.in
	m4 $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.isd

check_SCRIPTS = $(TESTS) $(TESTS:.sh=.mk)
