.PHONY: all build clean repl fmt deps

all: build

build:
	dune build

clean:
	dune clean

repl:
	dune utop

fmt:
	-dune build @fmt --auto-promote

deps:
	dune external-lib-deps --missing @@default
