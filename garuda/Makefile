UUID_ON_MACOSX  = c87f46b3-e74d-4251-ab9f-1b99caac8e2c
UUID_ON_WINDOWS = 01a209fc-6a80-11e2-8738-1f7eda18fd79

define gadget_directory
$(1):
	install -d $$@

$(1)/icons: | $(1)
	install -d $$@

$(1)/snapshot: | $(1)
	install -d $$@

$(1)/icons/icon.png: icons/icon.png | $(1)/icons
	install $$< $$@

$(1)/snapshot/screenshot0.png: snapshot/screenshot0.png | $(1)/snapshot
	install $$< $$@

$(1)/snapshot/screenshot1.png: snapshot/screenshot1.png | $(1)/snapshot
	install $$< $$@

$(1).zip: $(1)/gadget_config.xml $(1)/icons/icon.png $(1)/snapshot/screenshot0.png $(1)/snapshot/screenshot1.png
	zip -r $$@ $(1)
endef

.PHONY: all clean

all: $(UUID_ON_MACOSX).zip $(UUID_ON_WINDOWS).zip

clean:
	rm -rf $(UUID_ON_MACOSX).zip $(UUID_ON_WINDOWS).zip $(UUID_ON_WINDOWS)/ $(UUID_ON_MACOSX)/

$(UUID_ON_MACOSX)/gadget_config.xml: gadget_config.xml.macosx | $(UUID_ON_MACOSX)
	install $< $@

$(UUID_ON_WINDOWS)/gadget_config.xml: gadget_config.xml.windows | $(UUID_ON_WINDOWS)
	install $< $@

$(foreach uuid,$(UUID_ON_MACOSX) $(UUID_ON_WINDOWS),$(eval $(call gadget_directory,$(uuid))))
