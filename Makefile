.PHONY : test clean

all : ordering slides

sample : ordering slides sample/papers.csv sample/conflicts.csv sample/pc.csv
	@echo "RUNNING SAMPLE..."
	(cd sample; ../ordering papers.csv conflicts.csv pc.csv >schedule.csv; diff -u schedule.expected schedule.csv; ../slides schedule.csv pc.csv > slides.tex; pdflatex slides.tex)
	@echo "DONE."

test : ordering slides papers.csv conflicts1.csv conflicts2.csv conflicts3.csv allpapers.csv pc.csv
	@echo "RUNNING SMALL REGRESSIONS..."
	./ordering papers.csv conflicts1.csv pc.csv >/dev/null
	./ordering papers.csv conflicts2.csv pc.csv >/dev/null
	./ordering allpapers.csv conflicts3.csv pc.csv >/dev/null
	@echo "DONE."

	@echo "RUNNING REAL TEST..."
	./ordering nonpcpapers.csv conflicts4.csv pc.csv >schedule.csv.test
	@echo "CHECKING COMPLETENESS OF OUTPUT..."
	cut -f 1 -d , schedule.csv.test | sort -n > nonpcpapers.csv.test
	diff -u nonpcpapers.csv nonpcpapers.csv.test
	@echo "GENERATING SLIDES..."
	./slides schedule.csv.test pc.csv > slides.tex
	pdflatex slides.tex
	open slides.pdf
	@echo "DONE."

ordering : ordering.hs util.hs
	ghc $< -main-is Ordering -o $@

slides : slides.hs util.hs
	ghc $< -main-is Slides -o $@

clean :
	rm -f *.o *.hi ordering slides *~
	rm -f nonpcpapers.csv.test schedule.csv.test allpapers.csv.test
	rm -f *.nav *.snm *.out *.log *.aux *.toc slides.pdf slides.tex
	(cd sample; rm -f schedule.csv *.nav *.snm *.out *.log *.aux *.toc slides.pdf slides.tex)

