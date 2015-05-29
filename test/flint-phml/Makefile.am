DATADIR = @abs_top_srcdir@/test/flint-phml
MODELDIR = @abs_top_srcdir@/test/models

TESTS = \
	Izhikevich_2003_model.isml.sh \
	LR-pulse-189-164.isml.sh \
	LR1-ring_reentry_initiate.isml.sh \
	Rybak_2006_with_static_instance_and_multiple_input.isml.sh \
	bogus-td.phml.sh \
	delta_time.phml.sh \
	empty.phml.sh \
	exponential.isml.sh \
	gamma.isml.sh \
	graph-condition.phml.sh \
	graph-instance.phml.sh \
	graph-probability.phml.sh \
	hepatocyte_external.isml.sh \
	hepatocyte_internal.isml.sh \
	init-with-pq.isml.sh \
	luo_rudy_1991.isml.sh \
	max-min.isml.sh \
	mean.isml.sh \
	multiple.isml.sh \
	piecewise.phml.sh \
	poisson.isml.sh \
	ringed_Beeler_Reuter_1977_model_with_static_instance.isml.sh \
	ringed_Luo_Rudy_1991_model_with_instance.isml.sh \
	sine_curves.isml.sh \
	target.phml.sh \
	template-test_no-instance_no-edge.phml.sh \
	template-test_with-instace.phml.sh \
	time_units.isml.sh \
	timeseries.phml.sh \
	uniform.isml.sh

.mk.sh:
	echo '#!/bin/sh' > $@
	echo 'export PATH="@abs_top_builddir@/src:$$PATH"' >> $@
	echo 'install -d $(@:.sh=)' >> $@
	echo '$(MAKE) -C $(@:.sh=) -f ../$< -s clean all check' >> $@
	chmod +x $@

$(TESTS:.sh=.mk): $(DATADIR)/test.mk.in
	m4 \
		-DINPUT=$(MODELDIR)/$(@:.mk=) \
		-DMODULES=$(DATADIR)/$(@:.mk=.modules) \
		-DPQS=$(DATADIR)/$(@:.mk=.pqs) \
		-DIMPLS=$(DATADIR)/$(@:.mk=.impls) \
		-DEXTRAS=$(DATADIR)/$(@:.mk=.extras) \
		-DIVS=$(DATADIR)/$(@:.mk=.ivs) \
		-DBRIDGES=$(DATADIR)/$(@:.mk=.bridges) \
		-DIPORTS=$(DATADIR)/$(@:.mk=.iports) \
		-DOPORTS=$(DATADIR)/$(@:.mk=.oports) \
		-DEDGES=$(DATADIR)/$(@:.mk=.edges) \
		-DTPQS=$(DATADIR)/$(@:.mk=.tpqs) \
		-DTS=$(DATADIR)/$(@:.mk=.ts) \
		-DTSREF=$(DATADIR)/$(@:.mk=.tsref) \
		-DTREES=$(DATADIR)/$(@:.mk=.trees) \
		-DSCOPES=$(DATADIR)/$(@:.mk=.scopes) \
		-DJOURNALS=$(DATADIR)/$(@:.mk=.journals) \
		-DSPANS=$(DATADIR)/$(@:.mk=.spans) \
		-DREACHES=$(DATADIR)/$(@:.mk=.reaches) \
		$< > $@

CLEANFILES = *.sh *.mk

clean-local:
	-rm -rf $(TESTS:.sh=)

check_SCRIPTS = $(TESTS)

noinst_DATA = $(TESTS:.sh=.mk)
