.PHONY : all test clean

all : ordering slides

sample : ordering slides sample/papers.csv sample/conflicts.csv sample/pc.csv
	@echo "RUNNING SAMPLE..."
	(cd sample; \
	../ordering papers.csv conflicts.csv pc.csv >schedule.csv; \
	../check papers.csv schedule.csv; \
	diff -u schedule.expected schedule.csv; \
	../slides schedule.csv pc.csv > slides.tex; \
	pdflatex slides.tex)
	@echo "DONE."

ordering : ordering.hs util.hs
	ghc $< -main-is Ordering -o $@

slides : slides.hs util.hs
	ghc $< -main-is Slides -o $@

clean :
	rm -f *.o *.hi ordering slides *~
	(cd sample; rm -f schedule.csv *.nav *.snm *.out *.log *.aux *.toc slides.pdf slides.tex)

