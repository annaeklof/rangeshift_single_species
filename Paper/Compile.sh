#!/bin/bash
pdflatex BayesExtinctions-3-MEE.tex
pdflatex BayesExtinctions-3-MEE.tex
bibtex BayesExtinctions-3-MEE
pdflatex BayesExtinctions-3-MEE.tex
pdflatex BayesExtinctions-3-MEE.tex
evince BayesExtinctions-3-MEE.pdf &

## Cleanup
rm *~
rm *.aux
rm *.dvi
rm *.log
rm *.nav
rm *.out
rm *.snm
rm *.toc

