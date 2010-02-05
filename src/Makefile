#!/usr/bin/make -f
#Template Makefile especially suited for Erlang directories

SHELL = /bin/sh
OSTYPE := $(shell /usr/bin/uname -s)
INITMOD = padxmpp
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

all: clean
	$(FIND) . -name "*.erl" -exec erlc '{}' \;

clean: dumpclean
	$(FIND) . \( -regex "^[.]?(.+)\~$$" -o -regex "./[.]?#.*#" \) -delete
	$(FIND) . -name "*.beam" -delete

dumpclean:
	$(FIND) . -name *.dump -delete

etags: 
	find . -name "*.[he]rl" -print | etags -

run:
	erl -noshell -s $(INITMOD) $(INITFUNC) -pa $(CURDIR) -boot start_sasl -name em

.PHONY : clean distclean all run
