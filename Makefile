.PHONY : test clean

ordering : ordering.hs util.hs
	ghc $< -main-is Ordering -o $@

slides : slides.hs util.hs
	ghc $< -main-is Slides -o $@

test : ordering slides papers.csv conflicts1.csv conflicts2.csv conflicts3.csv allpapers.csv pc.csv
	@echo "RUNNING SMALL REGRESSIONS..."
	./ordering papers.csv conflicts1.csv pc.csv >/dev/null
	./ordering papers.csv conflicts2.csv pc.csv >/dev/null
	@echo "DONE."

	@echo "RUNNING REAL TEST..."
	./ordering allpapers.csv conflicts3.csv pc.csv >schedule.csv.test
	@echo "CHECKING COMPLETENESS OF OUTPUT..."
	cut -f 1 -d , schedule.csv.test | sort -n > allpapers.csv.test
	diff -u allpapers.csv allpapers.csv.test
	@echo "GENERATING SLIDES..."
	./slides schedule.csv.test pc.csv > slides.tex
	pdflatex slides.tex
	open slides.pdf
	@echo "DONE."

clean :
	rm -f *.o *.hi ordering slides *~
	rm -f allpapers.csv.test schedule.csv.test
	rm -f *.nav *.snm *.out *.log *.aux *.toc slides.pdf slides.tex
