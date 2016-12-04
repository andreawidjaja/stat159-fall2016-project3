#Read data sets
clean_data <- read.csv('../../data/generated_data/clean_data.csv')

#converting categorical columns into multiple binary columns
clean_data$REGION=as.factor(clean_data$REGION)
options(na.action='na.pass')
temp_data=model.matrix(~., clean_data)
scaled_data=as.data.frame(temp_data)[,-c(1,2)]

#scaling non-categorical variables
for (i in c(10:ncol(scaled_data))){
  scaled_data[,i]=scale(scaled_data[,i])
}

#export scaled data
write.csv(scaled_data, file = '../../data/generated_data/scaled_data.csv')
