#!/usr/bin/make -f
#Template Makefile especially suited for Erlang directories

SHELL = /bin/sh
OSTYPE := $(shell /usr/bin/uname -s)
INITMOD = MyModule
INITFUNC = init
INITARGS = []

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
	@echo "Nothing configured for all yet..."
	#find . -type f -name "*.erl" -exec erlc '{}' \;

clean:
	$(FIND) . \( -regex "^[.]?(.+)\~$$" -o -regex "./[.]?#.*#" \) -delete
	$(FIND) . -name "*.beam" -delete

distclean: clean
	$(FIND) . -name *.dump -delete

run:
	erl -noshell -s $(INITMOD) $(INITFUNC) $(INITARGS) -pa $(CURDIR)

.PHONY : clean distclean all run
