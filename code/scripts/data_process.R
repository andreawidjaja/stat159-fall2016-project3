#Read data sets
clean_data <- read.csv('../../data/clean_data.csv')

#converting categorical columns into multiple binary columns
temp_data=model.matrix(~., clean_data)
scaled_data=as.data.frame(temp_data)[,-1]

#scaling non-categorical variables
for (i in c(10:ncol(scaled_data))){
  scaled_data[,i]=scale(scaled_data[,i])
}

#export scaled data
write.csv(scaled_data, file = '../../data/scaled_data.csv')