.PHONY: all clean cpp parser

all: cpp parser

cpp:
	$(MAKE) -C cpp

parser:
	$(MAKE) -C parser

clean:
	$(MAKE) -C parser clean
	$(MAKE) -C cpp clean
