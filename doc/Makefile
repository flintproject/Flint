.PHONY: all clean

all: flint-user-guide.pdf

clean:
	-rm *.aux *.idx *.log *.out *.pdf *.toc

%.pdf: %.tex
	pdflatex $<
	pdflatex $< # run twice for building table of contents
