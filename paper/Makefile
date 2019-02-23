
TGT=index
LATEX=pdflatex -shell-escape -output-directory=dist

default : src/index.tex src/body.tex src/references.bib
	@mkdir -p dist
	export TEXMFHOME=".:$(TEXMFHOME)" && \
	$(LATEX) src/$(TGT).tex 

dist/$(TGT).aux: default

bib: references.bib dist/$(TGT).aux
	bibtex dist/$(TGT).aux
	export TEXMFHOME=".:$(TEXMFHOME)" && \
	$(LATEX) src/$(TGT).tex

clean :
	cd dist && rm -rf * && cd ..
