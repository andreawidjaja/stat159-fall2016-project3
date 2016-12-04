.PHONY: clean all data


data: data_clean.R

data_clean.R:
	Rscript code/scripts/data_clean.R data/input_data/MERGED2014_15_PP.csv 


eda.txt: data_clean.R
	Rscript code/scripts/eda.R $^ data/input_data/MERGED2014_15_PP
clean:
	rm data/generated_data/*