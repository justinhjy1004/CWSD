TEX=pdflatex
BIBTEX=bibtex
SRC=jbes
OUT=../Paper.pdf

all: $(OUT)

$(OUT): $(SRC).tex
	$(TEX) $(SRC).tex
	$(BIBTEX) $(SRC)
	$(TEX) $(SRC).tex
	$(TEX) $(SRC).tex
	mv $(SRC).pdf $(OUT)

clean:
	rm -f $(SRC).aux $(SRC).log $(SRC).bbl $(SRC).blg $(SRC).out $(SRC).toc *.lof *.lot

