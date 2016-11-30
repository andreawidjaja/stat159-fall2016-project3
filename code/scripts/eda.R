library(dplyr)
library(ggplot2)

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
#histogram of percentage of degrees of different majors
pairs_majors = clean_data[, c("PCIP01", "PCIP03", "PCIP04", "PCIP05", "PCIP09", "PCIP10",
                              "PCIP11", "PCIP12", "PCIP13", "PCIP14", "PCIP15", "PCIP16",
                              "PCIP19", "PCIP22", "PCIP23", "PCIP24", "PCIP25", "PCIP26",
                              "PCIP27", "PCIP29", "PCIP30", "PCIP31", "PCIP38", "PCIP39",
                              "PCIP40", "PCIP41", "PCIP42", "PCIP43", "PCIP44", "PCIP45",
                              "PCIP46", "PCIP47", "PCIP48", "PCIP49", "PCIP50", "PCIP51",
                              "PCIP52", "PCIP54")]
png("../../images/histogram_majors.png")
attach(pairs_majors)
graph_majors <- par(mfrow = c(8, 5))
hist(PCIP01, main = "Histogram of % of Degrees in Agriculture, Agriculture Operations, and Related Sciences")
hist(PCIP03, main = "Histogram of % of Degrees in Natural Resources and Conservation")
hist(PCIP04, main = "Histogram of % of Degrees in Architecture and Related Services")
hist(PCIP05, main = "Histogram of % of Degrees in Area, Ethnic, Cultural, Gender, and Group Studies")
hist(PCIP09, main = "Histogram of % of Degrees in Communication, Journalism, and Related Programs")
hist(PCIP10, main = "Histogram of % of Degrees in Communications Technologies/Technicians and Support Services")
hist(PCIP11, main = "Histogram of % of Degrees in Computer and Information Sciences and Support Services")
hist(PCIP12, main = "Histogram of % of Degrees in Personal and Culinary Services")
hist(PCIP13, main = "Histogram of % of Degrees in Education")
hist(PCIP14, main = "Histogram of % of Degrees in Engineering")
hist(PCIP15, main = "Histogram of % of Degrees in Engineering Technologies and Engineering-Related Fields")
hist(PCIP16, main = "Histogram of % of Degrees in Foreign Languages, Literatures, and Linguistics")
hist(PCIP19, main = "Histogram of % of Degrees in Family and Consumer Sciences/Human Sciences")
hist(PCIP22, main = "Histogram of % of Degrees in Legal Professions and Studies")
hist(PCIP23, main = "Histogram of % of Degrees in English Language and Literature/Letters")
hist(PCIP24, main = "Histogram of % of Degrees in Liberal Arts and Sciences, General Studies and Humanities")
hist(PCIP25, main = "Histogram of % of Degrees in Library Science")
hist(PCIP26, main = "Histogram of % of Degrees in Biological and Biomedical Sciences")
hist(PCIP27, main = "Histogram of % of Degrees in Mathematics and Statistics")
hist(PCIP29, main = "Histogram of % of Degrees in Military Technologies and Applied Sciences")
hist(PCIP30, main = "Histogram of % of Degrees in Multi/Interdisciplinary Studies")
hist(PCIP31, main = "Histogram of % of Degrees in Parks, Recreation, Leisure, and Fitness Studies")
hist(PCIP38, main = "Histogram of % of Degrees in Philosophy and Religious Studies")
hist(PCIP39, main = "Histogram of % of Degrees in Theology and Religious Vocations")
hist(PCIP40, main = "Histogram of % of Degrees in Physical Sciences")
hist(PCIP41, main = "Histogram of % of Degrees in Science Technologies/Technicians")
hist(PCIP42, main = "Histogram of % of Degrees in Psychology")
hist(PCIP43, main = "Histogram of % of Degrees in Homeland Security, Law Enforcement, Firefighting and Related Protective Services")
hist(PCIP44, main = "Histogram of % of Degrees in Public Administration and Social Service Professions")
hist(PCIP45, main = "Histogram of % of Degrees in Social Sciences")
hist(PCIP46, main = "Histogram of % of Degrees in Construction Trades")
hist(PCIP47, main = "Histogram of % of Degrees in Mechanic and Repair Technologies/Technicians")
hist(PCIP48, main = "Histogram of % of Degrees in Precision Production")
hist(PCIP49, main = "Histogram of % of Degrees in Transportation and Materials Moving")
hist(PCIP50, main = "Histogram of % of Degrees in Visual and Performing Arts")
hist(PCIP51, main = "Histogram of % of Degrees in Health Professions and Related Programs")
hist(PCIP52, main = "Histogram of % of Degrees in Business, Management, Marketing, and Related Support Services")
hist(PCIP54, main = "Histogram of % of Degrees in History")
par(graph_majors)
dev.off()
#histogram of enrollment by race
pairs_race_enrollment = clean_data[, c("UGDS_WHITE", "UGDS_BLACK", "UGDS_HISP", "UGDS_ASIAN", "UGDS_AIAN", "UGDS_NHPI")]
png("../../images/histogram_race_enrollment.png")
attach(pairs_race_enrollment)
graph_race_enrollment <- par(mfrow = c(3, 2))
hist(UGDS_WHITE, main = "Histogram of Share of Enrollment who are White")
hist(UGDS_BLACK, main = "Histogram of Share of Enrollment who are Black")
hist(UGDS_HISP, main = "Histogram of Share of Enrollment who are Hispanic")
hist(UGDS_ASIAN, main = "Histogram of Share of Enrollment who are Asian")
hist(UGDS_AIAN, main = "Histogram of Share of Enrollment who are American Indian/Alaska Native")
hist(UGDS_NHPI, main = "Histogram of Share of Enrollment who are Hawaiian/Pacific Islander")
par(graph_race_enrollment)
dev.off()
#histogram of net price of institution
png("../../images/histogram_net_price.png")
hist(clean_data$NPT4_PUB)
dev.off()
#histogram of average cost per academic year
png("../../images/histogram_net_price.png")
hist(clean_data$COSTT4_A)
dev.off()
#histogram of average faculty salary
png("../../images/histogram_faculty_salary.png")
hist(clean_data$AVGFACSAL)
dev.off()
#histogram of completion rate
png("../../images/histogram_completion.png")
hist(clean_data$C150_4)
dev.off()
#histogram of completion rate by income level
pairs_race_enrollment = clean_data[, c("COMP_ORIG_YR6_RT", "LO_INC_COMP_ORIG_YR6_RT", "LO_INC_COMP_4YR_TRANS_YR6_RT", "MD_INC_COMP_ORIG_YR6_RT",
                                      "HI_INC_COMP_ORIG_YR6_RT")]
png("../../images/histogram_income_graduation.png")
attach(pairs_income_graduation)
graph_race_enrollment <- par(mfrow = c(2, 2))
hist(COMP_ORIG_YR6_RT, main = "Histogram of % who completed in 6 years")
hist(LO_INC_COMP_ORIG_YR6_RT, main = "Histogram of % who completed in 6 years from low-income families")
hist(LO_INC_COMP_4YR_TRANS_YR6_RT, main = "Histogram of % who completed in 6 years from medium-income families")
hist(HI_INC_COMP_ORIG_YR6_RT, main = "Histogram of % who completed in 6 years from high-income families")
par(graph_income_graduation)
dev.off()
#histogram of completion rate by race (black, white, hisp, asian, native american, nhpi)
FILL THIS IN


#Summary statistics of qualitative variables
png("../../images/histogram_region.png")
hist(selected_data$REGION)
dev.off()

#Correlation Matrix
matrix <- cor(clean_data)
save(matrix, file = "../../data/correlation_matrix.RData")

#Generate eda.txt for quantitative variables
sink("../../data/eda.txt")
#Summary statistics of quantitative variables
cat("Summary Statistics of Quantitative Variables\n")
cat("\n\n")
#min, 1st quartile, median, mean, 3rd quartile, max
cat("Min, 1st Quartile, Median, Mean, 3rd Quartile, Max of quantitative variables\n")
print(quantitative_summary)
cat("\n\n")
#range
cat("Range of quantitative variables\n")
print(quantitative_range)
cat("\n\n")
#IQR
cat("IQR of quantitative variables\n")
print(quantitative_IQR)
cat("\n\n")
#standard deviation
cat("Standard Deviation of quantitative variables\n")
print(quantitative_SD)
cat("\n\n")
sink()

