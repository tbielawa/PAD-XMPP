#!/usr/bin/make -f
#Template Makefile especially suited for Erlang directories

SHELL = /bin/sh
OSTYPE := $(shell /usr/bin/uname -s)
INITMOD = test_tcp_client
INITFUNC = start
#INITARGS = []

# OS X?
ifeq ("$(findstring Darwin, $(OSTYPE))", "Darwin")
	SED = sed -E
	FIND = find -E
# Assume GNU
else
	SED = sed -r
	FIND = find -regextype posix-extended
endif

all:
	$(FIND) . -name "*.erl" -exec erlc '{}' \;

clean:
	$(FIND) . \( -regex "^[.]?(.+)\~$$" -o -regex "./[.]?#.*#" \) -delete
	$(FIND) . -name "*.beam" -delete

distclean: clean
	$(FIND) . -name *.dump -delete

run:
	erl -noshell -s $(INITMOD) $(INITFUNC) -pa $(CURDIR)

.PHONY : clean distclean all run
