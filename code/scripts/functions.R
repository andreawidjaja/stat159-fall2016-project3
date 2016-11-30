library(leaps)
library(glmnet)

#setwd("/Users/josephfrancia/Desktop/Fall_2016/Stats159/stat159-fall2016-project3/code/scripts")
#clean_data=read.csv("../../data/clean_data.csv")[,-1]


choosing_response=function(dataframe, character){
  response=which(colnames(dataframe)==character)
  dataframe[,setdiff(95:101, response)]=NULL #getting rid of all graduation rates, except for black graduation rates
  dataframe= subset(dataframe, select=c(setdiff(1:ncol(dataframe),response), response))
  return(dataframe)
}


lasso_select=function(dataframe, character, n){
  lasso=cv.glmnet(as.matrix(dataframe[,-which(colnames(dataframe)==character)]), dataframe[,which(colnames(dataframe)==character)])
  sparse_vector=glmnet(as.matrix(dataframe[,-which(colnames(dataframe)==character)]), dataframe[,which(colnames(dataframe)==character)], lambda=lasso$lambda.1se)
  relevant=which(coef(sparse_vector)!=0)[-1] #Extracting the nonzero values of our beta vector (aka, the relevant variables)... also, i removed the beta coefficient corresponding to the intercept
  lasso_var=data.frame(row.names(coef(sparse_vector))[relevant], coef(sparse_vector)[relevant]) #Printing out the names of our relevant variables!
  colnames(lasso_var)=c("Relevant_Variables", "Beta_Coefficients")
  ordered=order(abs(lasso_var$Beta_Coefficients), decreasing=TRUE)
  lasso_var=lasso_var[ordered,]
  return(lasso_var[1:n,])
}

bic_select=function(dataframe, character, n){
  model=summary(regsubsets(as.matrix(dataframe[,-which(colnames(dataframe)==character)]), dataframe[,which(colnames(dataframe)==character)], method="forward", nvmax=15))
  bic_var=model$which[which.min(model$bic),]
  relevant=which(bic_var==TRUE)[-1]
  lm_obj=lm.fit(as.matrix(dataframe[, relevant-1]), dataframe[,which(colnames(dataframe)==character)])
  bic_var=data.frame(rownames(data.frame(lm_obj$coefficients)),lm_obj$coefficients)
  rownames(bic_var)=NULL
  colnames(bic_var)=c("Relevant_Variables","Beta_Coefficients")
  ordered=order(abs(bic_var$Beta_Coefficients), decreasing=TRUE)
  bic_var=bic_var[ordered,]
  return(bic_var[1:n,])
}

