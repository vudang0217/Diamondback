UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=elf64
  NOPIE=-nopie
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho64
  NOPIE=-Wl,-no_pie
endif
endif

PKGS=oUnit,extlib,unix,sexplib,str

BUILD=ocamlbuild -r -use-ocamlfind -pkg $(PKGS) $(OCAMLOPT)

main: main.ml compile.ml runner.ml parser.ml expr.ml typecheck.ml
	$(BUILD) main.native
	mv main.native main

test: compile.ml runner.ml test.ml parser.ml expr.ml typecheck.ml
	mkdir -p output
	$(BUILD) test.native
	mv test.native test

output/%.run: output/%.o main.c
	clang $(NOPIE) -g -mstackrealign -o $@ main.c $<

output/%.o: output/%.s
	nasm -f $(FORMAT) -o $@ $<

output/%.s: input/%.boa main
	mkdir -p output
	./main $< > $@

clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log
	rm -rf _build/
	rm -f main test
