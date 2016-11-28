#Reading in Data
args<-commandArgs(trailingOnly=TRUE)
data <- read.csv(args[1])
library(leaps)
library(glmnet)

#setwd("/Users/josephfrancia/Desktop/Fall_2016/Stats159/stat159-fall2016-project3/code/scripts")
#clean_data=read.csv("../../data/clean_data.csv")[,-1]

#Data Cleaning
response=which(colnames(clean_data)=="UGDS_BLACK")
clean_data[,setdiff(97:106, response)]=NULL #getting rid of all graduation rates, except for black graduation rates

#Lasso for Variable Selection
lasso=cv.glmnet(as.matrix(clean_data[,-which(colnames(clean_data)=="UGDS_BLACK")]), clean_data$UGDS_BLACK)
sparse_vector=glmnet(as.matrix(clean_data[,-which(colnames(clean_data)=="UGDS_BLACK")]), clean_data$UGDS_BLACK, lambda=lasso$lambda.1se)
relevant=which(coef(sparse_vector)!=0)[-1] #Extracting the nonzero values of our beta vector (aka, the relevant variables)... also, i removed the beta coefficient corresponding to the intercept
lasso_var=row.names(coef(sparse_vector))[relevant] #Printing out the names of our relevant variables!

#BIC With Forward Selection for Variable Selection
model=summary(regsubsets(UGDS_BLACK~.,data=clean_data, method="forward", nvmax=ncol(clean_data)))
bic_var=model$which[which.min(model$bic),]
relevant=which(bic_var==TRUE)[-1]
bic_var=colnames(model$which)[relevant]

#Nick, you can do the forward selection stuff with p-values. you'll have to make a function that does that since there's no built in R thing that does that



#### MAKING FUNCIONS THAT DOES ALL OF THE ABOVE WORK... WE WANT TO EVENTUALLY MAKE A NICE SHINY APP WITH THESE FUNCTIONS###### 

choosing_response=function(character){
  response=which(colnames(clean_data)==character)
  clean_data[,setdiff(97:106, response)]=NULL #getting rid of all graduation rates, except for black graduation rates
  return(clean_data)
}


#For AJ: figure out how to output coefficients, along with the variable names

lasso_select=function(dataframe, character){
  lasso=cv.glmnet(as.matrix(dataframe[,-which(colnames(dataframe)==character)]), dataframe[,which(colnames(dataframe)==character)])
  sparse_vector=glmnet(as.matrix(dataframe[,-which(colnames(dataframe)==character)]), dataframe[,which(colnames(dataframe)==character)], lambda=lasso$lambda.1se)
  relevant=which(coef(sparse_vector)!=0)[-1] #Extracting the nonzero values of our beta vector (aka, the relevant variables)... also, i removed the beta coefficient corresponding to the intercept
  lasso_var=row.names(coef(sparse_vector))[relevant] #Printing out the names of our relevant variables!
  return(lasso_var)
}

bic_select=function(dataframe, character){
  model=summary(regsubsets(dataframe[,-which(colnames(dataframe)==character)],dataframe[,which(colnames(dataframe)==character)], method="forward", nvmax=15))
  bic_var=model$which[which.min(model$bic),]
  relevant=which(bic_var==TRUE)[-1]
  bic_var=colnames(model$which)[relevant]
  return(bic_var)
}
