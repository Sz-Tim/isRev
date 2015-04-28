# Makefile for Insectes Sociaux review 2015  

all: supportFiles/isRev.bib output/msDraft.docx output/msDraft.pdf

supportFiles/isRev.bib: ~/Dropbox/Thesis/Manuscripts/MendeleyBibTex/InSoc_Review.bib
	cp ~/Dropbox/Thesis/Manuscripts/MendeleyBibTex/InSoc_Review.bib supportFiles/isRev.bib

output/msDraft.docx: msDraft.md supportFiles/isRev.bib  
	pandoc -H supportFiles/format.sty -V fontsize=12pt --bibliography supportFiles/isRev.bib --csl=supportFiles/insectes-sociaux.csl --reference-docx=supportFiles/refTemplate.docx msDraft.md -o output/msDraft.docx

output/msDraft.pdf: msDraft.md supportFiles/isRev.bib  
	pandoc -H supportFiles/format.sty -V fontsize=12pt --bibliography supportFiles/isRev.bib --csl=supportFiles/insectes-sociaux.csl msDraft.md -o output/msDraft.pdf