all: presentation.pdf

.PHONY: presentation.pdf

presentation.pdf:
	xelatex presentation.tex
	xelatex presentation.tex

clean:
	-rm -fr *.aux
	-rm -fr *.log
	-rm -fr *.nav
	-rm -fr *.out
	-rm -fr *.snm
	-rm -fr *.toc
	-rm -fr *.pdf
	-rm -fr *.vrb

.PHONY: clean all
