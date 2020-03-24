# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
include ../deposit.mk
include ../version.mk
include config.mk

ARCH := $(shell uname -m)

RELEASE = 1

DEPS := \
    boost \
    ceres \
    clibsedml \
    czmq \
    libsbml \
    protobuf \
    soslib \
    sundials \
    wxwidgets \
    zeromq

boost_VERSION = $(BOOST_VERSION)
ceres_VERSION = $(CERES_VERSION)
clibsedml_VERSION = $(CLIBSEDML_VERSION)
czmq_VERSION = $(CZMQ_VERSION)
libsbml_VERSION = $(LIBSBML_VERSION)
protobuf_VERSION = $(PROTOBUF_VERSION)
soslib_VERSION = $(SOSLIB_VERSION)
sundials_VERSION = $(SUNDIALS_VERSION)
wxwidgets_VERSION = $(WXWIDGETS_VERSION)
zeromq_VERSION = $(ZEROMQ_VERSION)

boost_RELEASE = 1
ceres_RELEASE = 1
clibsedml_RELEASE = 1
czmq_RELEASE = 1
libsbml_RELEASE = 1
protobuf_RELEASE = 1
soslib_RELEASE = 1
sundials_RELEASE = 1
wxwidgets_RELEASE = 1
zeromq_RELEASE = 1

boost_DEPENDS =
ceres_DEPENDS =
clibsedml_DEPENDS =
czmq_DEPENDS = zeromq
libsbml_DEPENDS = 
protobuf_DEPENDS =
soslib_DEPENDS = libsbml sundials
sundials_DEPENDS =
wxwidgets_DEPENDS =
zeromq_DEPENDS =

flint_name = flint-$(1)
flint_basename = flint-$(1)-$($(1)_VERSION)
flint_fullname = flint-$(1)-$($(1)_VERSION)-$($(1)_RELEASE)
flint_packagename = flint-$(1)-$($(1)_VERSION)-$($(1)_RELEASE).$(DIST).$(ARCH).rpm
flint_installed = tmp/flint-$(1)-$($(1)_VERSION).installed
flint_tarball = var/flint-$(1)-$($(1)_VERSION).tar.gz

DEPNAMES := $(foreach d,$(DEPS),$(call flint_name,$(d)))
DEPPACKAGES := $(foreach d,$(DEPS),$(call flint_packagename,$(d)))
DEPINSTALLED := $(foreach d,$(DEPS),$(call flint_installed,$(d)))

RPMBUILD_DIR = ~/rpmbuild

FLINT_PACKAGE     = flint-$(VERSION)-$(RELEASE).$(DIST).$(ARCH).rpm

RPM_FILES := $(foreach p,$(DEPPACKAGES) $(FLINT_PACKAGE),rpm/$(p))
DIST_FILES := $(foreach p,$(DEPPACKAGES) $(FLINT_PACKAGE),Flint-$(VERSION)-$(DIST).$(ARCH)/$(p))

define flint_archive
$(call flint_tarball,$(1)): var/$(call flint_basename,$(1))
	tar -C var -czf $$@ $(call flint_basename,$(1))
endef

define flint_spec
flint-$(1).spec: flint-$(1).spec.in
	m4 \
		-D$(1)_VERSION=$($(1)_VERSION) \
		-D$(1)_RELEASE=$($(1)_RELEASE) \
		-DFLINT_INCLUDE_DIR=$(abspath ../include) \
		-DGTK_PACKAGE_NAME=$(GTK_PACKAGE_NAME) \
		-DCZMQ_CURRENT=$(CZMQ_CURRENT) \
		-DCZMQ_VERSION_INFO=$(CZMQ_VERSION_INFO) \
		-DPROTOBUF_CURRENT_MAJOR=$(PROTOBUF_CURRENT_MAJOR) \
		-DPROTOBUF_CURRENT_MINOR=$(PROTOBUF_CURRENT_MINOR) \
		-DZEROMQ_CURRENT=$(ZEROMQ_CURRENT) \
		-DZEROMQ_VERSION_INFO=$(ZEROMQ_VERSION_INFO) \
		$$< > $$@
endef

define flint_rpmbuild
$(RPMBUILD_DIR)/RPMS/$(ARCH)/$(call flint_packagename,$(1)): flint-$(1).spec $(call flint_tarball,$(1)) $(foreach d,$($(1)_DEPENDS),$(call flint_installed,$(d)))
	install $(call flint_tarball,$(1)) $(RPMBUILD_DIR)/SOURCES/
	rpmbuild -bb $$<
endef

define flint_srpm
$(call flint_fullname,$(1)).src.rpm: flint-$(1).spec $(call flint_tarball,$(1))
	mock --buildsrpm --spec flint-$(1).spec --sources $(call flint_tarball,$(1))
	install /var/lib/mock/epel-5-$(ARCH)/result/$$@ ./
endef

define flint_install
$(call flint_installed,$(1)): $(RPMBUILD_DIR)/RPMS/$(ARCH)/$(call flint_packagename,$(1)) | tmp
	sudo rpm -Uvh --force $$<
	touch $$@
endef

define flint_install_rpm
$(1): $(RPMBUILD_DIR)/RPMS/$(ARCH)/$(notdir $(1))
	install -D $$< $$@
endef

define flint_expand_tar_gz
var/$(1): src/$(2).tar.gz | var
	-rm -rf $$@
	-tar -C var -zxf $$<
	touch $$@
endef

define flint_expand_tar_bz2
var/$(1): src/$(2).tar.bz2 | var
	-rm -rf $$@
	tar -C var -jxf $$<
	touch $$@
endef

define flint_unzip
var/$(1): src/$(2).zip | var
	-rm -rf $$@
	(cd var && unzip -qq ../$$<)
	touch $$@
endef

define flint_setup_dir
var/$(call flint_basename,$(1)): var/$(2)
	-rm -rf $$@
	cp -pR $$< $$@
endef

.PHONY: all mostlyclean clean dist distclean maintainer-clean download

all: download $(RPM_FILES)

mostlyclean:
	-rm flint.spec var/flint-$(VERSION).tar.gz

clean:
	-rm -rf Flint-$(VERSION)-$(DIST).$(ARCH) rpm $(DEPINSTALLED) *.spec var/*.tar.gz var/*/

dist: Flint-$(VERSION)-$(DIST).$(ARCH).zip

distclean: clean
	-rm -f Flint-$(VERSION)-$(DIST).$(ARCH).zip

maintainer-clean: distclean
	-sudo rpm -e flint
	-sudo rpm -e $(DEPNAMES)

download: $(foreach lib,$(BOOST_UNDERSCORE).tar.bz2 clibsedml-$(CLIBSEDML_VERSION).tar.gz czmq-$(CZMQ_VERSION).tar.gz libSBML-$(LIBSBML_VERSION)-core-src.zip protobuf-cpp-$(PROTOBUF_VERSION).tar.gz SBML_odeSolver-$(SOSLIB_VERSION).tar.gz sundials-$(SUNDIALS_VERSION).tar.gz wxWidgets-$(WXWIDGETS_VERSION).tar.bz2 zeromq-$(ZEROMQ_VERSION).tar.gz,src/$(lib))

Flint-$(VERSION)-$(DIST).$(ARCH):
	install -d $@

Flint-$(VERSION)-$(DIST).$(ARCH)/README: data/README | Flint-$(VERSION)-$(DIST).$(ARCH)
	install -p -m 0644 $< $@

Flint-$(VERSION)-$(DIST).$(ARCH).zip: $(DIST_FILES) Flint-$(VERSION)-$(DIST).$(ARCH)/README
	zip -r $@ Flint-$(VERSION)-$(DIST).$(ARCH)

$(foreach d,$(DEPS),$(eval $(call flint_archive,$(d))))

$(foreach d,$(DEPS),$(eval $(call flint_spec,$(d))))

$(foreach d,$(DEPS),$(eval $(call flint_rpmbuild,$(d))))

$(foreach d,$(DEPS),$(eval $(call flint_srpm,$(d))))

$(foreach d,$(DEPS),$(eval $(call flint_install,$(d))))

$(foreach p,$(RPM_FILES) $(DIST_FILES),$(eval $(call flint_install_rpm,$(p))))

var tmp:
	install -d $@

var/flint-$(VERSION).tar.gz: | var
	(cd .. && git archive --format=tar --prefix=flint-$(VERSION)/ HEAD | gzip > $(CURDIR)/$@)

flint.spec: flint.spec.in
	m4 -DVERSION=$(VERSION) -DRELEASE=$(RELEASE) $< > $@

$(foreach p,$(FLINT_PACKAGE),$(RPMBUILD_DIR)/RPMS/$(ARCH)/$(p)): flint.spec var/flint-$(VERSION).tar.gz $(DEPINSTALLED)
	install var/flint-$(VERSION).tar.gz $(RPMBUILD_DIR)/SOURCES/
	rpmbuild -bb $<

$(eval $(call flint_expand_tar_bz2,$(BOOST_UNDERSCORE),$(BOOST_UNDERSCORE)))

$(eval $(call flint_setup_dir,boost,$(BOOST_UNDERSCORE)))

$(eval $(call flint_expand_tar_gz,ceres-solver-$(CERES_VERSION),ceres-solver-$(CERES_VERSION)))

$(eval $(call flint_setup_dir,ceres,ceres-solver-$(CERES_VERSION)))

$(eval $(call flint_expand_tar_gz,clibsedml-$(CLIBSEDML_VERSION),clibsedml-$(CLIBSEDML_VERSION)))

$(eval $(call flint_setup_dir,clibsedml,clibsedml-$(CLIBSEDML_VERSION)))

$(eval $(call flint_expand_tar_gz,czmq-$(CZMQ_VERSION),czmq-$(CZMQ_VERSION)))

$(eval $(call flint_setup_dir,czmq,czmq-$(CZMQ_VERSION)))

$(eval $(call flint_expand_tar_gz,sundials-$(SUNDIALS_VERSION),sundials-$(SUNDIALS_VERSION)))

$(eval $(call flint_setup_dir,sundials,sundials-$(SUNDIALS_VERSION)))

$(eval $(call flint_unzip,libsbml-$(LIBSBML_VERSION),libSBML-$(LIBSBML_VERSION)-core-src))

$(eval $(call flint_setup_dir,libsbml,libsbml-$(LIBSBML_VERSION)))

$(eval $(call flint_expand_tar_gz,protobuf-$(PROTOBUF_VERSION),protobuf-cpp-$(PROTOBUF_VERSION)))

$(eval $(call flint_setup_dir,protobuf,protobuf-$(PROTOBUF_VERSION)))

$(eval $(call flint_expand_tar_gz,SBML_odeSolver-$(SOSLIB_VERSION),SBML_odeSolver-$(SOSLIB_VERSION)))

$(eval $(call flint_setup_dir,soslib,SBML_odeSolver-$(SOSLIB_VERSION)))

var/wxWidgets-$(WXWIDGETS_VERSION): src/wxWidgets-$(WXWIDGETS_VERSION).tar.bz2 | var
	-rm -rf $@
	tar -C var -jxf $<
	touch $@

$(eval $(call flint_setup_dir,wxwidgets,wxWidgets-$(WXWIDGETS_VERSION)))

$(eval $(call flint_expand_tar_gz,zeromq-$(ZEROMQ_VERSION),zeromq-$(ZEROMQ_VERSION)))

$(eval $(call flint_setup_dir,zeromq,zeromq-$(ZEROMQ_VERSION)))

.DEFAULT_GOAL := all
