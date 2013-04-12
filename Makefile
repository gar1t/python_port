compile: deps
	./rebar compile
	cd examples; make compile

quick:
	./rebar compile skip_deps=true

deps:
	./rebar get-deps

refresh-deps:
	./rebar delete-deps
	./rebar get-deps

clean:
	./rebar clean
	cd examples; make clean
	rm -f priv/py/erlport/*.pyc

.PHONY: test

DEPS=$(notdir $(wildcard deps/*))
SKIP_APP=$(shell echo $(DEPS) | tr ' ' ',')

TESTS=""
test:
ifeq ($(TESTS), "")
	./rebar -j1 eunit skip_deps=true skip_apps=$(SKIP_APP)
else
	./rebar -j1 eunit suite=$(TESTS) skip_deps=true skip_apps=$(SKIP_APP)
endif

.PHONY: doc
doc:
	./rebar doc
