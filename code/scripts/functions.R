library(leaps)
library(glmnet)

#setwd("/Users/josephfrancia/Desktop/Fall_2016/Stats159/stat159-fall2016-project3/code/scripts")
#clean_data=read.csv("../../data/clean_data.csv")[,-1]


choosing_response=function(character){
  response=which(colnames(clean_data)==character)
  clean_data[,setdiff(97:106, response)]=NULL #getting rid of all graduation rates, except for black graduation rates
  return(clean_data)
}


lasso_select=function(dataframe, character){
  lasso=cv.glmnet(as.matrix(dataframe[,-which(colnames(dataframe)==character)]), dataframe[,which(colnames(dataframe)==character)])
  sparse_vector=glmnet(as.matrix(dataframe[,-which(colnames(dataframe)==character)]), dataframe[,which(colnames(dataframe)==character)], lambda=lasso$lambda.1se)
  relevant=which(coef(sparse_vector)!=0)[-1] #Extracting the nonzero values of our beta vector (aka, the relevant variables)... also, i removed the beta coefficient corresponding to the intercept
  lasso_var=data.frame(row.names(coef(sparse_vector))[relevant], coef(sparse_vector)[relevant]) #Printing out the names of our relevant variables!
  return(lasso_var)
}

bic_select=function(dataframe, character){
  model=summary(regsubsets(dataframe[,-which(colnames(dataframe)==character)],dataframe[,which(colnames(dataframe)==character)], method="forward", nvmax=15))
  bic_var=model$which[which.min(model$bic),]
  relevant=which(bic_var==TRUE)[-1]
  bic_var=colnames(model$which)[relevant]
  lm_obj=lm(character~., data=clean_data[,c(which(colnames(clean_data) %in% bic_var), which(colnames(clean_data)==character))])
  bic_var=lm_obj$coefficients[-1]
  return(bic_var)
}
