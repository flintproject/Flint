.PHONY: all clean

TARGET_FILES = header.h misc.h reduction.h

all: $(TARGET_FILES)

clean:
	-rm $(TARGET_FILES)

.c.h:
	xxd -i < $< > $@
