#Reading in Data
args<-commandArgs(trailingOnly=TRUE)
clean_data <- read.csv(args[1])
#argument should be scaled_data.csv
library(leaps)
library(glmnet)

#setwd("/Users/josephfrancia/Desktop/Fall_2016/Stats159/stat159-fall2016-project3/code/scripts")
#clean_data=read.csv("../../data/clean_data.csv")[,-1]
#scaled_data=read.csv("../../data/scaled_data.csv")[,-1]

#Data Cleaning
response=which(colnames(clean_data)=="UGDS_BLACK")
clean_data[,setdiff(97:106, response)]=NULL #getting rid of all graduation rates, except for black graduation rates

#Lasso for Variable Selection
lasso=cv.glmnet(as.matrix(clean_data[,-which(colnames(clean_data)=="UGDS_BLACK")]), clean_data[,which(colnames(clean_data)=="UGDS_BLACK")])
sparse_vector=glmnet(as.matrix(clean_data[,-which(colnames(clean_data)=="UGDS_BLACK")]), clean_data[,which(colnames(clean_data)=="UGDS_BLACK")], lambda=lasso$lambda.1se)
relevant=which(coef(sparse_vector)!=0)[-1] #Extracting the nonzero values of our beta vector (aka, the relevant variables)... also, i removed the beta coefficient corresponding to the intercept
lasso_var=data.frame(row.names(coef(sparse_vector))[relevant], coef(sparse_vector)[relevant]) #Printing out the names of our relevant variables!

#BIC With Forward Selection for Variable Selection
model=summary(regsubsets(as.matrix(clean_data[,-which(colnames(clean_data)=="UGDS_BLACK")]), clean_data[,which(colnames(clean_data)=="UGDS_BLACK")], method="forward", nvmax=ncol(clean_data)))
bic_var=model$which[which.min(model$bic),]
relevant=which(bic_var==TRUE)[-1]
bic_var=colnames(model$which)[relevant]
lm_obj=lm.fit(as.matrix(clean_data[,-which(colnames(clean_data)=="UGDS_BLACK")]), clean_data[,which(colnames(clean_data)=="UGDS_BLACK")])
bic_var=lm_obj$coefficients[-1]



