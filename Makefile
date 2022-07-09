.PHONY: all px clean

all: px

px: build/exec/px_app/libidris2_few.so
	idris2 --build

build/exec/px_app/libidris2_few.so: support/c/idris_few.c support/c/idris_few.h
	mkdir -p build/exec/px_app
	$(CC) -shared $< -o $@

clean:
	rm -rf build/ result/
