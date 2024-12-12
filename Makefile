##
## EPITECH PROJECT, 2024
## B-FUN-400-BDX-4-1-wolfram-alea.chlodnik
## File description:
## Haskell Makefile
##

all:	build
	cp $$(stack path --local-install-root)/bin/wolfram-project-exe wolfram

build:
	stack build

test:
	stack test

clean:
	stack clean
	rm -f wolfram
