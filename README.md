# stat159-fall2016-project3

Andrea Widjaja, Joseph Francia, Nicholas Saber, Priscilla Hartono

STAT 159 Project 3 - title of project

The structure of the project will be as follows:  

* stat159-fall2016-project3
	* README.md
	* Makefile
	* LICENSE
	* session-info.txt
	* .gitignore
	* code
		* functions
		* scripts
		* tests
	* data
	* images
	* report
		* report.pdf
		* report.Rmd
		* sections
	* slides

To reproduce, clone this repo, download the dataset MERGED2014_15_PP.csv, and run with make.

A list of Make commands for targets are below:

* all
  * produces entire report
* data
	* processes data and cleans it
* eda.txt
	* produces a text with exploratory data analysis results, must be run after make data 
* slides
	* produces slides.html in top level directory
* report
	* produces report.pdf in top-level directory
* app
	* runs shiny app, to access shiny app copy html address in terminal and view it on your browser of choice. When finished running the app press control and c to stop the app
* session
	* produces all relevant information regarding packages and the current session to stdout
* clean
	* cleans up all generated data from project


