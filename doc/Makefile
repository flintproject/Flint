# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-
include ../version.mk

.PHONY: all clean

IMAGE_FILES := $(wildcard image/*.png)

VERSION_IMAGE_FILES := $(foreach f,$(IMAGE_FILES),$(VERSION)/$(f))

define copy_to_version
$(VERSION)/$(2): $(1) | $(VERSION)/image
	install -m 644 $$< $$@
endef

all: $(VERSION).zip

clean:
	-rm -rf $(VERSION).zip $(VERSION)
	-rm *.aux *.idx *.log *.out *.pdf *.toc

%.pdf: %.tex
	pdflatex $<
	pdflatex $<
	pdflatex $<

$(eval $(call copy_to_version,flint-user-guide.html,flint-$(VERSION)-user-guide.html))

$(eval $(call copy_to_version,flint-user-guide.pdf,flint-$(VERSION)-user-guide.pdf))

$(foreach f,$(IMAGE_FILES),$(eval $(call copy_to_version,$(f),$(f))))

$(VERSION).zip: $(VERSION)/flint-$(VERSION)-user-guide.pdf $(VERSION)/flint-$(VERSION)-user-guide.html $(VERSION_IMAGE_FILES)
	zip -FS -r $@ $(VERSION)

$(VERSION)/image: | $(VERSION)
	install -d $@

$(VERSION):
	install -d $@
