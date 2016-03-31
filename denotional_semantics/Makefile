all: presentation.pdf

%.pdf: %.tex
	pdflatex $(<)
	pdflatex $(<)

clean:
	-rm -fr *.aux
	-rm -fr *.log
	-rm -fr *.nav
	-rm -fr *.out
	-rm -fr *.snm
	-rm -fr *.toc
	-rm -fr *.pdf

.PHONY: clean all
