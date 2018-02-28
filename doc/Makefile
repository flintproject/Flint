.PHONY: all clean

all: flint-user-guide.pdf

clean:
	-rm *.aux *.idx *.log *.out *.pdf *.toc

%.pdf: %.tex publication.bib software.bib
	pdflatex $<
	bibtex $(basename $<)
	pdflatex $<
	pdflatex $<
