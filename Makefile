test:
	OCAMLRUNPARAM=b dune exec test/main.exe

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html