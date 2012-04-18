SHELL := bash

FILENAME := Translation

.PHONY: all read aread clean


all: $(FILENAME).pdf

%.tex: %.lhs
	 lhs2TeX -o $@ $<

%.pdf: %.tex
	pdflatex $<
	pdflatex $<

read: $(FILENAME).pdf
	evince $< &

aread: $(FILENAME).pdf
	acroread $< &

clean:
	rm -rf $(FILENAME).{ps,ptb,tex,pdf,log,aux,out,dvi,bbl,blg}
