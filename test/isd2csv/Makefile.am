TESTS = random.sh roundtrip.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s clean all check' >> $@
	chmod +x $@

random.mk: @top_srcdir@/test/isd2csv/random.mk.in
	m4 $< > $@

roundtrip.mk: @top_srcdir@/test/isd2csv/roundtrip.mk.in
	m4 $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.csv *.isd

check_SCRIPTS = $(TESTS) $(TESTS:.sh=.mk)
