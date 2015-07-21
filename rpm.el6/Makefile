# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
include ../deposit.mk
include config.mk

ARCH := $(shell uname -m)

VERSION = 1.4.0
RELEASE = 1

DEPS := \
    boost \
    clibsedml \
    libsbml \
    libzip \
    protobuf \
    soslib \
    sundials

boost_VERSION = $(BOOST_VERSION)
clibsedml_VERSION = 0.1.0
libsbml_VERSION = $(LIBSBML_VERSION)
libzip_VERSION = $(LIBZIP_VERSION)
protobuf_VERSION = $(PROTOBUF_VERSION)
soslib_VERSION = 1.6.99
sundials_VERSION = $(SUNDIALS_VERSION)

boost_RELEASE = 1
clibsedml_RELEASE = 1
libsbml_RELEASE = 1
libzip_RELEASE = 1
protobuf_RELEASE = 1
soslib_RELEASE = 1
sundials_RELEASE = 1

boost_DEPENDS =
clibsedml_DEPENDS = 
libsbml_DEPENDS = 
libzip_DEPENDS = 
protobuf_DEPENDS =
soslib_DEPENDS = libsbml sundials
sundials_DEPENDS =

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
FLINT_GUI_PACKAGE = flint-gui-$(VERSION)-$(RELEASE).$(DIST).$(ARCH).rpm
TARGET_PACKAGES = $(FLINT_PACKAGE) $(FLINT_GUI_PACKAGE)
DIST_PACKAGES = $(FLINT_PACKAGE) $(FLINT_GUI_PACKAGE)

RPM_FILES := $(foreach p,$(DEPPACKAGES) $(TARGET_PACKAGES),rpm/$(p))
DIST_FILES := $(foreach p,$(DEPPACKAGES) $(DIST_PACKAGES),Flint_$(DIST).$(ARCH)/$(p))

define flint_archive
$(call flint_tarball,$(1)): var/$(call flint_basename,$(1))
	tar -C var -czf $$@ $(call flint_basename,$(1))
endef

define flint_spec
flint-$(1).spec: flint-$(1).spec.in
	m4 -D$(1)_VERSION=$($(1)_VERSION) -D$(1)_RELEASE=$($(1)_RELEASE) $$< > $$@
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
	-tar -C var -zxf $$<
	touch $$@
endef

define flint_expand_tar_bz2
var/$(1): src/$(2).tar.bz2 | var
	tar -C var -jxf $$<
	touch $$@
endef

define flint_unzip
var/$(1): src/$(2).zip | var
	(cd var && unzip -qq ../$$<)
	touch $$@
endef

define flint_setup_dir
var/$(call flint_basename,$(1)): var/$(2)
	-rm -rf $$@
	cp -pR $$< $$@
endef

.PHONY: all mostlyclean clean dist distclean maintainer-clean

all: $(RPM_FILES)

mostlyclean:
	-rm flint.spec var/flint-$(VERSION).tar.gz

clean:
	-rm -rf Flint_$(DIST).$(ARCH) rpm $(DEPINSTALLED) *.spec var/*.tar.gz var/*/

dist: Flint_$(DIST).$(ARCH).zip

distclean: clean
	-rm -f Flint_$(DIST).$(ARCH).zip

maintainer-clean: distclean
	-sudo rpm -e flint-gui
	-sudo rpm -e flint
	-sudo rpm -e $(DEPNAMES)

Flint_$(DIST).$(ARCH).zip: $(DIST_FILES)
	zip -r $@ Flint_$(DIST).$(ARCH)

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

$(foreach p,$(TARGET_PACKAGES),$(RPMBUILD_DIR)/RPMS/$(ARCH)/$(p)): flint.spec var/flint-$(VERSION).tar.gz $(DEPINSTALLED)
	install var/flint-$(VERSION).tar.gz $(RPMBUILD_DIR)/SOURCES/
	rpmbuild -bb $<

$(eval $(call flint_expand_tar_bz2,$(BOOST_UNDERSCORE),$(BOOST_UNDERSCORE)))

$(eval $(call flint_setup_dir,boost,$(BOOST_UNDERSCORE)))

$(eval $(call flint_expand_tar_gz,clibsedml-$(CLIBSEDML_COMMIT),clibsedml-$(CLIBSEDML_COMMIT)))

$(eval $(call flint_setup_dir,clibsedml,clibsedml-$(CLIBSEDML_COMMIT)))

$(eval $(call flint_expand_tar_gz,sundials-$(SUNDIALS_VERSION),sundials-$(SUNDIALS_VERSION)))

$(eval $(call flint_setup_dir,sundials,sundials-$(SUNDIALS_VERSION)))

$(eval $(call flint_unzip,libsbml-$(LIBSBML_VERSION),libSBML-$(LIBSBML_VERSION)-core-src))

$(eval $(call flint_setup_dir,libsbml,libsbml-$(LIBSBML_VERSION)))

$(eval $(call flint_expand_tar_gz,libzip-$(LIBZIP_VERSION),libzip-$(LIBZIP_VERSION)))

$(eval $(call flint_setup_dir,libzip,libzip-$(LIBZIP_VERSION)))

$(eval $(call flint_expand_tar_gz,protobuf-$(PROTOBUF_VERSION),protobuf-$(PROTOBUF_VERSION)))

$(eval $(call flint_setup_dir,protobuf,protobuf-$(PROTOBUF_VERSION)))

$(eval $(call flint_expand_tar_gz,SBML_odeSolver-$(SOSLIB_COMMIT),SBML_odeSolver-$(SOSLIB_COMMIT)))

$(eval $(call flint_setup_dir,soslib,SBML_odeSolver-$(SOSLIB_COMMIT)))

.DEFAULT_GOAL := all
