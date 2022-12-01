IDRIS2_CC = $(CC) -Isupport/c -Loutputs/out/lib -l$(LIB_FEW_NAME)
IDRIS2_CG ?= refc
PREFIX    ?= /usr

ifeq ($(shell uname),Darwin)
LIB_EXT = .dylib
else
LIB_EXT = .so
endif

LIB_FEW_NAME = idris2_few
LIB_FEW      = outputs/out/lib/lib$(LIB_FEW_NAME)$(LIB_EXT)
PX           = build/exec/px

$(PX): $(LIB_FEW) $(shell find src -type d -o -name '*.idr')
	IDRIS2_CC="$(IDRIS2_CC)" IDRIS2_CG="$(IDRIS2_CG)" idris2 --build

$(LIB_FEW): support/c/idris_few.o
	mkdir -p $$(dirname $(LIB_FEW))
	$(CC) -shared $< -o $@

# phony target to allow for use by the ipkg prebuild, this will result in the
# $(LIB_FEW) target getting evaluated again by idris2 --build, but that's ok
# since it will be up-to-date
.PHONY: lib-few
lib-few: $(LIB_FEW)

.PHONY: install
install: $(PX)
	mkdir -p $(PREFIX)/{bin,lib}
	cp build/exec/px $(PREFIX)/bin
	cp $(LIB_FEW) $(PREFIX)/lib

.PHONY: clean
clean:
	rm -rf build/ result
