DATADIR = @top_srcdir@/test/flint-combineall
MODELDIR = @top_srcdir@/test/models

TESTS = \
	hepatocyte_internal.isml.sh \
	hepatocyte_external.isml.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 \
		-DUUID=4d96c8de-d10a-48e2-a0e0-be9d74e58e78 \
		-DMODEL=$(MODELDIR)/$(@:.mk=) \
		-DNAME=$(DATADIR)/name.txt \
		-DVALUE=$(DATADIR)/value.txt \
		-DFUNCTION=$(DATADIR)/function.txt \
		-DODE=$(DATADIR)/ode.txt \
		-DOUTPUT=$(@:.mk=) \
		$< > $@

CLEANFILES = *.name *.value *.function *.ode *.sh *.mk *.db *.importdumpall *.sbmlall

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
