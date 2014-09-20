.PHONY : test clean

ordering : ordering.hs
	ghc $< -main-is Ordering -o $@

test : ordering papers.csv conflicts1.csv conflicts2.csv pc.csv
	@echo "RUNNING SMALL REGRESSIONS..."
	./ordering papers.csv conflicts1.csv pc.csv >/dev/null
	./ordering papers.csv conflicts2.csv pc.csv >/dev/null
	@echo "DONE."

	@echo "RUNNING REAL TEST..."
	./ordering allpapers.csv conflicts3.csv pc.csv >schedule.csv.test
	@echo "CHECKING COMPLETENESS OF OUTPUT..."
	cut -f 1 -d , schedule.csv.test | grep -v -e ^$$ | sort -n > allpapers.csv.test
	diff -u allpapers.csv allpapers.csv.test
	@echo "DONE."

clean :
	rm -f *.o *.hi ordering
	rm -f allpapers.csv.test schedule.csv.test
