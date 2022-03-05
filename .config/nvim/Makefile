.PHONY: deps test

default: deps test

bootstrap:
	scripts/bootstrap.sh

deps:
	scripts/deps.sh 4o4 lua-reactivex d2b8244cc8bd6ade712f05f36204765037fa4ec5
	scripts/embed.sh lua-reactivex reactivex

test:
	SUFFIX="test/fnl/foo.fnl" scripts/test.sh
