PREFIX ?= /usr

ifeq ($(shell uname),Darwin)
LIB_FEW := build/exec/px_app/libidris2_few.dylib
else
LIB_FEW := build/exec/px_app/libidris2_few.so
endif

.PHONY: px
px: $(LIB_FEW)
	idris2 --build

$(LIB_FEW): support/c/idris_few.c support/c/idris_few.h
	mkdir -p build/exec/px_app
	$(CC) -shared $< -o $@

.PHONY: install
install: px
	mkdir -p $(PREFIX)/bin
	cp -r build/exec/* $(PREFIX)/bin

.PHONY: clean
clean:
	rm -rf build/ result
