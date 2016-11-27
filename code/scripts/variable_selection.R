#Reading in Data
args<-commandArgs(trailingOnly=TRUE)
data <- read.csv(args[1])
#setwd("/Users/josephfrancia/Desktop/Fall_2016/Stats159/stat159-fall2016-project3/code/scripts")
clean_data=read.csv("../../data/clean_data.csv")

response=which(colnames(clean_data)=="UGDS_BLACK")
clean_data[,setdiff(90:99, response)]=NULL #getting rid of all graduation rates, except for black graduation rates
reg=lm(UGDS_BLACK~., data=clean_data)
