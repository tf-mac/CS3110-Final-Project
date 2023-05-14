build:
	dune build lib

code:
	-dune build
	code .
	! dune build --watch

run: build
	OCAMLRUNPARAM=b dune exec bin/main.exe

test: build
	OCAMLRUNPARAM=b dune exec test/main.exe

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh	