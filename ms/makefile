# Makefile for Insectes Sociaux review 2015  

all: output/msDraft.docx output/msDraft.pdf

output/msDraft.docx: msDraft.md supportFiles/InSoc_Review.bib  
	pandoc -H supportFiles/format.sty -V fontsize=12pt --bibliography supportFiles/InSoc_Review.bib --csl=supportFiles/insectes-sociaux.csl msDraft.md -o output/msDraft.docx

output/msDraft.pdf: msDraft.md supportFiles/InSoc_Review.bib  
	pandoc -H supportFiles/format.sty -V fontsize=12pt --bibliography supportFiles/InSoc_Review.bib --csl=supportFiles/insectes-sociaux.csl msDraft.md -o output/msDraft.pdf