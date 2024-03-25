cargs = -I build -g
files = lexer parser secd main
test_files ?= $(wildcard tests/*)

.PHONY: tests
tests: exec
	@for file in $(test_files); do \
		echo "----------------------------Testing with $$file-----------------------------------"; \
		./exec < $$file; \
	done

exec: $(foreach file, $(files), build/$(file).cmo)
	ocamlc -o $@ $^ $(cargs)

build/main.cmo: src/main.ml build/secd.cmo build/parser.cmo

build/%.cmo: src/%.ml
	@mkdir -p build
	ocamlc -c -o $@ $< $(cargs)

build/parser.cmo: src/parser.mly build/secd.cmo
	@mkdir -p build
	ocamlyacc -b build/parser src/parser.mly
	ocamlc -c -o $@ build/parser.mli $(cargs)
	ocamlc -c -o $@ build/parser.ml $(cargs)

build/lexer.cmo: src/lexer.mll build/parser.cmo
	@mkdir -p build
	ocamllex -o build/lexer.ml src/lexer.mll
	ocamlc -c -o $@ build/lexer.ml $(cargs)      

.PHONY: clean
clean:
	@rm -rf build
	@rm -f exec
