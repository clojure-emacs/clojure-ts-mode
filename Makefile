.PHONY: clean compile lint indent test all
.DEFAULT_GOAL := all

clean:
	eldev clean

lint: clean
	eldev lint -c

# Checks that the sources are indented the way Emacs would indent them.
indent:
	eldev indent

# Checks for byte-compilation warnings.
compile: clean
	 eldev -dtT compile --warnings-as-errors

test: clean
	eldev -dtT -p test

all: clean compile lint indent test
