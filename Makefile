.PHONY: clean all data


data:
	Rscript code/scripts/data_clean.R data/MERGED2014_15_PP.csv 



clean:
	rm data/clean_data.csv