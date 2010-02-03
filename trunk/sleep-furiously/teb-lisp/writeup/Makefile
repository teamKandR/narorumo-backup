# XPDF = /Applications/Preview.app/Contents/MacOS/Preview 
XPDF = xpdf

# xpdf: alexr-teb.pdf
#	$(XPDF) alexr-teb.pdf

alexr-teb.pdf: alexr-teb.tex alexr-teb.bbl
	pdflatex alexr-teb.tex
	pdflatex alexr-teb.tex

clean: 
	rm -f *.aux *.dvi *.log *.pdf  *.out *.blg
