help:
	@cat Makefile

build:
	stack build

cabal-build-in-docker:
	docker build .

test:
	stack test

sdist:
	stack sdist

upload:
	stack upload

watch:
	stack build --fast --file-watch

watch-test:
	stack test --fast --file-watch

b: build
c: cabal-build-in-docker
t: test
w: watch
wt: watch-test

.PHONY: test
