#setwd("/Users/josephfrancia/Desktop/Fall_2016/Stats159/stat159-fall2016-project3/code/scripts")
#data=read.csv("../../data/MERGED2014_15_PP.csv")
#clean_data=read.csv("../../data/clean_data.csv")
library(dplyr)

#read data set
args <- commandArgs(trailingOnly=TRUE)
data <- read.csv(args[1])

#data cleaning
selected_data <- data %>%
  filter(PREDDEG == 3) %>% #Degree awarded predominantly bachelor
  filter(MAIN == 1) %>% #Main campus
  filter(CONTROL == 1) #Public schools

#variables we are going to use
x_variables <- selected_data %>%
  select(UNITID, PREDDEG, MAIN, CONTROL, INSTNM, CITY, ZIP, ST_FIPS, REGION, LOCALE2, ADM_RATE, SATVR25, SATVR75, SATMT25, SATMT75, SATWR25,
         SATWR75, SATVRMID, SATMTMID, SATWRMID, ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75, ACTCMMID,
         ACTENMID, ACTMTMID, ACTWRMID, SAT_AVG, PCIP01, PCIP03, PCIP04, PCIP05, PCIP09, PCIP10, PCIP11, PCIP12, PCIP13,
         PCIP14, PCIP15, PCIP16, PCIP19, PCIP22, PCIP23, PCIP24, PCIP25, PCIP26, PCIP27, PCIP29, PCIP30, PCIP31, PCIP38, PCIP39,
         PCIP40, PCIP41, PCIP42, PCIP43, PCIP44, PCIP45, PCIP46, PCIP47, PCIP48, PCIP49, PCIP50, PCIP51, PCIP52, PCIP54, NPT4_PUB,
         NPT41_PUB, NPT42_PUB, NPT43_PUB, NPT44_PUB, NPT45_PUB, NUM4_PUB, NUM41_PUB, NUM42_PUB, NUM43_PUB, NUM44_PUB, NUM45_PUB,
         COSTT4_A, TUITIONFEE_IN, TUITIONFEE_OUT, AVGFACSAL, PFTFAC, OVERALL_YR4_N, LO_INC_YR4_N, MD_INC_YR4_N, HI_INC_YR4_N, DEP_YR4_N,
         IND_YR4_N, FEMALE_YR4_N, MALE_YR4_N, FIRSTGEN_YR4_N, NOT1STGEN_YR4_N, OVERALL_YR6_N, LO_INC_YR6_N, MD_INC_YR6_N, HI_INC_YR6_N,
         DEP_YR6_N, IND_YR6_N, FEMALE_YR6_N, MALE_YR6_N, FIRSTGEN_YR6_N, NOT1STGEN_YR6_N, AGE_ENTRY, AGEGE24, FEMALE, MARRIED, DEPENDENT,
         VETERAN, FIRST_GEN)

y_variables <- selected_data %>%
  select(INSTNM, C150_4, C150_4_HISP, C150_4_AIAN, C150_4_WHITE, C150_4_BLACK, C150_4_ASIAN, C150_4_NHPI, COMP_ORIG_YR6_RT, COMP_4YR_TRANS_YR6_RT, LO_INC_COMP_ORIG_YR6_RT, LO_INC_COMP_4YR_TRANS_YR6_RT, MD_INC_COMP_ORIG_YR6_RT,
         MD_INC_COMP_4YR_TRANS_YR6_RT, HI_INC_COMP_ORIG_YR6_RT, HI_INC_COMP_4YR_TRANS_YR6_RT, DEP_COMP_ORIG_YR6_RT, DEP_COMP_4YR_TRANS_YR6_RT,
         IND_COMP_ORIG_YR6_RT, IND_COMP_4YR_TRANS_YR6_RT, FEMALE_COMP_ORIG_YR6_RT, FEMALE_COMP_4YR_TRANS_YR6_RT, MALE_COMP_ORIG_YR6_RT,
         MALE_COMP_4YR_TRANS_YR6_RT, FIRSTGEN_COMP_ORIG_YR6_RT, FIRSTGEN_COMP_4YR_TRANS_YR6_RT, NOT1STGEN_COMP_ORIG_YR6_RT,
         NOT1STGEN_COMP_4YR_TRANS_YR6_RT)

race_and_income <- selected_data %>%
  select(INSTNM, UGDS, UG, UGDS_WHITE, UGDS_BLACK, UGDS_HISP, UGDS_ASIAN, UGDS_AIAN, UGDS_NHPI, UGDS_2MOR, UGDS_NRA, UGDS_UNKN, UGDS_WHITENH,
         UGDS_BLACKNH, UGDS_API, UGDS_AIANOLD, UGDS_HISPOLD, UG_NRA, UG_UNKN, UG_WHITENH, UG_BLACKNH, UG_API, UG_AIANOLD, UG_HISPOLD,
         PPTUG_EF, PPTUG_EF2, CURROPER, NPT4_048_PUB, NPT4_3075_PUB, NPT4_75UP_PUB)

merge_1 <- merge(x_variables, y_variables, by = "INSTNM")
clean_data <- merge(merge_1, race_and_income, by = "INSTNM")

#checking to see variable types
v=vector()
for(i in 1:ncol(clean_data)){
  v[i]=class(clean_data[,i])
}

#converting types of variables that don't make any sense (and getting rid of some variables)


#Converting types of variables that don't make any sense (and getting rid of some variables)
clean_data$PREDDEG=as.factor(clean_data$PREDDEG) #PREDDEG tells us the predominant type of degree given in a univ
clean_data$CITY=NULL  #Don't need city variable in my opinion
clean_data$ZIP=NULL #Don't need zip code variable in my opinion
clean_data$ST_FIPS= as.factor(clean_data$ST_FIPS) #State should not be an integer variable
clean_data$ST_FIPS=NULL #Don't need state variable when we have a region variable
clean_data$REGION= as.factor(clean_data$REGION) #Region should not be an integer variable
clean_data$LOCALE2=NULL #Too many NULL values here
clean_data$ADM_RATE=as.numeric(clean_data$ADM_RATE) #Admission rates shouldn't be a factor
clean_data[,13:85]=apply(clean_data[,13:85], 2, as.numeric) #Converting all sat/act scores and percentage of people in certain degrees into numeric variables
clean_data$LOCALE2=NULL # Too many NULL values here
clean_data$LOCALE2=NULL # Too many NULL values here


clean_data$LOCALE2=NULL # Too many NULL values here 
>>>>>>> variable_selection
clean_data$ADM_RATE=as.numeric(as.character(clean_data$ADM_RATE)) #admission rates shouldnt be a factor
clean_data$ADM_RATE_ALL=NULL #no need for two admission rates... 99% correlation between above adm rate
clean_data[,13:85]=apply(clean_data[,13:85], 2, as.numeric) #converting all sat/act scores and percentage of people in certain degrees into numeric variables
clean_data[,c(86:105,107,114:133,135,145:156,158)]=NULL #Removing variables with several Null values
clean_data[,86:ncol(clean_data)]=apply(clean_data[,86:ncol(clean_data)], 2, as.numeric) #Converting all these factors into numeric variables
clean_data$CURROPER=NULL #Takes on only 1 variable
clean_data[,7:85]=apply(clean_data[,7:85], 2, as.numeric) #converting all sat/act scores and percentage of people in certain degrees into numeric variables
clean_data[,c(86:105,107,120:139,141,151:162,164)]=NULL #Removing variables with several Null values
clean_data[,86:ncol(clean_data)]=apply(clean_data[,86:ncol(clean_data)], 2, as.numeric) #converting all these factors  into numeric variables
clean_data$CURROPER=NULL #Takes on only 1 variable

#getting rid of 4 more useless variables
clean_data$INSTNM=NULL
clean_data$UNITID=NULL
clean_data$MAIN=NULL #all of our schools are main campuses
clean_data$CONTROL=NULL # all of our schools are public
clean_data$PREDDEG=NULL # all values are 3, so useless variable
clean_data[,3:7]=apply(clean_data[,3:7] , 2, as.numeric)

#dealing with NA values... lets replace each NA value with the column average

for(i in 1:ncol(clean_data)){
  clean_data[is.na(clean_data[,i]),i]=mean(clean_data[,i], na.rm=TRUE)
}


#converting categorical columns into multiple binary columns
clean_data=model.matrix(~., clean_data)
clean_data=as.data.frame(clean_data)[,-1]

#scaling non-categorical variables
for (i in c(10:ncol(clean_data))){
  clean_data[,i]=scale(clean_data[,i])
}

#clean race and income and y and x variables
write.csv(clean_data, file = '../../data/clean_data.csv')
write.csv(race_and_income, file='../../data/race_and_income.csv')
write.csv(y_variables, file='../../data/y_variables.csv')
write.csv(x_variables, file='../../data/x_variables.csv')

