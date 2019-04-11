
SOURCES=$(wildcard *.erl)
BEAMS=$(patsubst %.erl, %.beam, $(SOURCES))

all: $(BEAMS)

%.beam: %.erl
	erlc $<

clean:
	rm $(BEAMS)

.PHONY: all clean
