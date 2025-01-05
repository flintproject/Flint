DATADIR = @top_srcdir@/test/isddiff

CSV_TESTS = \
	no-units.sh \
	units.sh

TESTS = $(CSV_TESTS) \
	abstol.sh \
	nan.sh \
	reltol.sh \
	seed.sh \
	sign.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo '$(MAKE) -f $< -s all check' >> $@
	chmod +x $@

$(CSV_TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 -DDATADIR=$(DATADIR) -DTEST=$(@:.mk=) $< > $@

abstol.mk: $(DATADIR)/abstol.mk.in
	m4 -DDATADIR=$(DATADIR) $< > $@

nan.mk: $(DATADIR)/nan.mk.in
	m4 -DDATADIR=$(DATADIR) $< > $@

reltol.mk: $(DATADIR)/reltol.mk.in
	m4 -DDATADIR=$(DATADIR) $< > $@

seed.mk: $(DATADIR)/seed.mk.in
	m4 $< > $@

sign.mk: $(DATADIR)/sign.mk.in
	m4 -DDATADIR=$(DATADIR) $< > $@

CLEANFILES = $(TESTS) $(TESTS:.sh=.mk) *.isd

check_SCRIPTS = $(TESTS) $(TESTS:.sh=.mk)
