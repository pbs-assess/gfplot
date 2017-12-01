TEXT = gf-synopsis
REFS = refs

all: $(TEXT).pdf

%.tex: %.Rnw $(REFS).bib
	Rscript -e "knitr::knit('$<')"

$(TEXT).pdf: $(TEXT).tex $(REFS).bib title-pages.tex methods.tex introduction.tex
	latexmk -pdf $(TEXT)

rtf: $(TEXT).pdf
	latex2rtf -f0 -M3 -E0 $(TEXT).tex

clean:
	latexmk -c
	rm -f $(TEXT).bbl
	rm -f $(TEXT).upa
	rm -f $(TEXT).pdf

continuous:
	while true; do make --silent; sleep 1; done
