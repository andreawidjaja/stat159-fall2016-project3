library(dplyr)

#Read data sets
clean_data <- read.csv('../../data/clean_data.csv')
data <- read.csv('../../data/MERGED2014_15_PP.csv')

#Selecting data
selected_data <- data %>%
  filter(PREDDEG == 3) %>% #Degree awarded predominantly bachelor
  filter(MAIN == 1) %>% #Main campus
  filter(CONTROL == 1) #Public schools

#Quantitative variables
quantitative <- clean_data %>%
  select(PREDDEG, REGION, ADM_RATE, SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
         SATVRMID, SATMTMID, SATWRMID, ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75,
         ACTWR25, ACTWR75, ACTCMMID, ACTENMID, ACTMTMID, ACTWRMID, SAT_AVG, PCIP01, PCIP03,
         PCIP04, PCIP05, PCIP09, PCIP10, PCIP11, PCIP12, PCIP13, PCIP14, PCIP15, PCIP16,
         PCIP19, PCIP22, PCIP23, PCIP24, PCIP25, PCIP26, PCIP27, PCIP29, PCIP30, PCIP31,
         PCIP38, PCIP39, PCIP40, PCIP41, PCIP42, PCIP43, PCIP44, PCIP45, PCIP46, PCIP47,
         PCIP48, PCIP49, PCIP50, PCIP51, PCIP52, PCIP54, NPT4_PUB, NPT41_PUB, NPT42_PUB,
         NPT43_PUB, NPT44_PUB, NPT45_PUB, NUM4_PUB, NUM41_PUB, NUM42_PUB, NUM43_PUB,
         NUM44_PUB, NUM45_PUB, COSTT4_A, TUITIONFEE_IN, TUITIONFEE_OUT, AVGFACSAL, PFTFAC,
         AGE_ENTRY, FEMALE, MARRIED, DEPENDENT, VETERAN, FIRST_GEN, C150_4, UGDS, UGDS_WHITE,
         UGDS_BLACK, UGDS_HISP, UGDS_ASIAN, UGDS_AIAN, UGDS_NHPI, UGDS_2MOR, UGDS_NRA, UGDS_UNKN,
         PPTUG_EF, NPT4_048_PUB, NPT4_3075_PUB, NPT4_75UP_PUB)

#Qualitative variables
qualitative <- clean_data %>%
  select()

#Summary statistics of quantitative variables
#min, 1st quartile, median, mean, 3rd quartile, max
quantitative_summary <- summary(quantitative)
#range
quantitative_range <- apply(quantitative, 2, FUN=range)
#IQR
quantitative_IQR <- apply(quantitative, 2, FUN=IQR)
#standard deviation
quantitative_SD <- apply(quantitative, 2, FUN=sd)
#histogram of average ACT scores
png("../../images/histogram_ACT_avg.png")
hist(clean_data$ACTCMMID)
dev.off()
#histogram of average SAT scores
png("../../images/histogram_SAT_avg.png")
hist(clean_data$SAT_AVG)
dev.off()
#histogram of net price of institution
png("../../images/histogram_net_price.png")
hist(clean_data$NPT4_PUB)
dev.off()
#histogram of average cost per academic year
png("../../images/histogram_net_price.png")
hist(clean_data$COSTT4_A)
dev.off()
#histogram of in-state tuition and fees
png("../../images/histogram_net_price.png")
hist(clean_data$TUITIONFEE_IN)
dev.off()
#histogram of out-of-state tuition and fees
png("../../images/histogram_net_price.png")
hist(clean_data$TUITIONFEE_OUT)
dev.off()
#histogram of average faculty salary
png("../../images/histogram_net_price.png")
hist(clean_data$AVGFACSAL)
dev.off()

#Summary statistics of qualitative variables
png("../../images/histogram_region.png")
hist(selected_data$REGION)
dev.off()

#Correlation Matrix
matrix <- cor(clean_data)
save(matrix, file = "../../data/correlation_matrix.RData")
