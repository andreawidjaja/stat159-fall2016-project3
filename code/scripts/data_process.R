#Read data sets
args <- commandArgs(trailingOnly=TRUE)
clean_data <- read.csv(args[1])


#Replace NA's with column means
for(i in 1:ncol(clean_data)){
  clean_data[is.na(clean_data[,i]),i]=mean(clean_data[,i], na.rm=TRUE)
}

clean_data$REGION=as.factor(clean_data$REGION)
options(na.action='na.pass')
temp_data=model.matrix(~., clean_data)
scaled_data=as.data.frame(temp_data)[,-c(1,2)]

#scaling non-categorical variables
for (i in c(10:ncol(scaled_data))){
  scaled_data[,i]=scale(scaled_data[,i])
}

#export scaled data
write.csv(scaled_data, file = 'data/generated_data/scaled_data.csv')
