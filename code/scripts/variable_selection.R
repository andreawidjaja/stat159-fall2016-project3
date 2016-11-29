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




#Nick, you can do the forward selection stuff with p-values. you'll have to make a function that does that since there's no built in R thing that does that
forward_p<-function(p,data)
  #data argumented must formatted such that the last column is the response variable
  #p must be the number of predictor variables to use for the model, ie after addidng p variables to our model
  #we stop adding predictors to our model, p must be >= 1
  
  
  #break data into response and predictors, 
  predictors<-data[,-c(length(colnames(data)))]
response<-data[,length(colnames(data))]

#resultant list of all models
models<-vector(length=p)

for(i in seq(1:p)) {
  temp1<-vector(length=(length(colnames(predictors))))
  
  for (j in seq(1:(length(colnames(predictors))))){
    #Initial loop to fill in temp1
    if (!models[1]){
      #Store t-value of regression of response variable ran on just the j'th feature of predictors
      temp1[j]<-summary(lm.fit(x=predictors[,j],y=response))$coefficients[i+1,4]
    } 
    else {
      #Skip if predictor is already in our model so far
      if (j %in% models) {
        #assign value of two so that which.min call won't mistake and double count predictors
        temp1[j]<-2
        next
      }
      else {
        #Store t-value of newly added predictor variable to regression model     
        temp1[j]<-summary(lm.fit(x=predictors[,c((models[models != 0]),j)],y=response))$coefficients[i+1,4]
      }
    }
  }
  #store the predictor whose t-value is the lowest when added to the model
  models[i]<-which.min(temp1)
}
