.PHONY : test clean

ordering : ordering.hs
	ghc $< -main-is Ordering -o $@

test : ordering papers.csv conflicts1.csv conflicts2.csv pc.csv
	./ordering papers.csv conflicts1.csv pc.csv
	./ordering papers.csv conflicts2.csv pc.csv
	./ordering allpapers.csv conflicts3.csv pc.csv

clean :
	rm -f *.o *.hi ordering
