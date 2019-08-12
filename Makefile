help:
	@cat Makefile

build:
	stack build

test:
	stack test

watch:
	stack build --fast --file-watch

watch-test:
	stack test --fast --file-watch

b: build
t: test
w: watch
wt: watch-test

.PHONY: test
