#Reading in Data
args<-commandArgs(trailingOnly=TRUE)
data <- read.csv(args[1])
#clean_data=read.csv("../../data/clean_data.csv")


#Removing graduation rates that don't correspond to black peoples' graduation rates
useless_grad_rates=colnames(race_and_income)[-which(colnames(race_and_income)=="UGDS_BLACK")]
useless_grad_rates=useless_grad_rates[-1] #Getting rid of instutition name variable
relevant_cols=which(colnames(clean_data) %in% colnames(clean_data)[-which(colnames(clean_data) %in% useless_grad_rates)]) #columns we will be using for our analysis
clean_data=clean_data[,relevant_cols] 


