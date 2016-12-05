library(dplyr)
library(ggplot2)

#Read data sets
args <- commandArgs(trailingOnly=TRUE)
clean_data <- read.csv(args[1])
data <- read.csv(args[2])

#Selecting data
selected_data <- data %>%
  filter(PREDDEG == 3) %>% #Degree awarded predominantly bachelor
  filter(MAIN == 1) %>% #Main campus
  filter(CONTROL == 1) #Public schools

#Quantitative variables
quantitative <- clean_data %>%
  select(ADM_RATE, SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75,
                SATVRMID, SATMTMID, SATWRMID, ACTCM25, ACTCM75, ACTEN25, ACTEN75, ACTMT25, ACTMT75,
                ACTWR25, ACTWR75, ACTCMMID, ACTENMID, ACTMTMID, ACTWRMID, SAT_AVG, PCIP01, PCIP03,
                PCIP04, PCIP05, PCIP09, PCIP10, PCIP11, PCIP12, PCIP13, PCIP14, PCIP15, PCIP16,
                PCIP19, PCIP22, PCIP23, PCIP24, PCIP25, PCIP26, PCIP27, PCIP29, PCIP30, PCIP31,
                PCIP38, PCIP39, PCIP40, PCIP41, PCIP42, PCIP43, PCIP44, PCIP45, PCIP46, PCIP47,
                PCIP48, PCIP49, PCIP50, PCIP51, PCIP52, PCIP54, NPT4_PUB, NPT41_PUB, NPT42_PUB,
                NPT43_PUB, NPT44_PUB, NPT45_PUB, NUM4_PUB, NUM41_PUB, NUM42_PUB, NUM43_PUB,
                NUM44_PUB, NUM45_PUB, COSTT4_A, TUITIONFEE_IN, TUITIONFEE_OUT, AVGFACSAL, PFTFAC, C150_4, C150_4_HISP,
                C150_4_AIAN, C150_4_WHITE, C150_4_BLACK, C150_4_ASIAN, C150_4_NHPI, UGDS_WHITE,
                UGDS_BLACK, UGDS_HISP, UGDS_ASIAN, UGDS_AIAN, UGDS_NHPI)

#ADM_RATE: Admission rate
#SATVR25: 25th percentile of SAT scores at the institution (critical reading)
#SATVR75: 75th percentile of SAT scores at the institution (critical reading)
#SATMT25: 25th percentile of SAT scores at the institution (math)
#SATMT75: 75th percentile of SAT scores at the institution (math)
#SATWR25: 25th percentile of SAT scores at the institution (writing)
#SATWR75: 75th percentile of SAT scores at the institution (writing)
#SATVRMID: Midpoint of SAT scores at the institution (critical reading)
#SATMTMID: Midpoint of SAT scores at the institution (math)
#SATWRMID: Midpoint of SAT scores at the institution (writing)
#ACTCM25: 25th percentile of the ACT cumulative score
#ACTCM75: 75th percentile of the ACT cumulative score
#ACTEN25: 25th percentile of the ACT English score
#ACTEN75: 75th percentile of the ACT English score
#ACTMT25: 25th percentile of the ACT math score
#ACTMT75: 75th percentile of the ACT math score
#ACTWR25: 25th percentile of the ACT writing score
#ACTWR75: 75th percentile of the ACT writing score
#ACTCMMID: Midpoint of the ACT cumulative score
#ACTENMID: Midpoint of the ACT English score
#ACTMTMID: Midpoint of the ACT math score
#ACTWRMID: Midpoint of the ACT writing score
#SAT_AVG: Average SAT equivalent score of students admitted
#PCIP01: Percentage of degrees awarded in Agriculture, Agriculture Operations, And Related Sciences.
#PCIP03: Percentage of degrees awarded in Natural Resources And Conservation.
#PCIP04: Percentage of degrees awarded in Architecture And Related Services.
#PCIP05: Percentage of degrees awarded in Area, Ethnic, Cultural, Gender, And Group Studies.
#PCIP09: Percentage of degrees awarded in Communication, Journalism, And Related Programs.
#PCIP10: Percentage of degrees awarded in Communications Technologies/Technicians And Support Services.
#PCIP11: Percentage of degrees awarded in Computer And Information Sciences And Support Services.
#PCIP12:Percentage of degrees awarded in Personal And Culinary Services.
#PCIP13:Percentage of degrees awarded in Education.
#PCIP14:Percentage of degrees awarded in Engineering.
#PCIP15: Percentage of degrees awarded in Engineering Technologies And Engineering-Related Fields.
#PCIP16: Percentage of degrees awarded in Foreign Languages, Literatures, And Linguistics.
#PCIP19: Percentage of degrees awarded in Family And Consumer Sciences/Human Sciences.
#PCIP22: Percentage of degrees awarded in Legal Professions And Studies.
#PCIP23: Percentage of degrees awarded in English Language And Literature/Letters.
#PCIP24: Percentage of degrees awarded in Liberal Arts And Sciences, General Studies And Humanities.
#PCIP25: Percentage of degrees awarded in Library Science.
#PCIP26: Percentage of degrees awarded in Biological And Biomedical Sciences.
#PCIP27: Percentage of degrees awarded in Mathematics And Statistics.
#PCIP29: Percentage of degrees awarded in Military Technologies And Applied Sciences.
#PCIP30: Percentage of degrees awarded in Multi/Interdisciplinary Studies.
#PCIP31: Percentage of degrees awarded in Parks, Recreation, Leisure, And Fitness Studies.
#PCIP38: Percentage of degrees awarded in Philosophy And Religious Studies.
#PCIP39: Percentage of degrees awarded in Theology And Religious Vocations.
#PCIP40: Percentage of degrees awarded in Physical Sciences.
#PCIP41: Percentage of degrees awarded in Science Technologies/Technicians.
#PCIP42: Percentage of degrees awarded in Psychology.
#PCIP43: Percentage of degrees awarded in Homeland Security, Law Enforcement, Firefighting And Related Protective Services.
#PCIP44: Percentage of degrees awarded in Public Administration And Social Service Professions.
#PCIP45: Percentage of degrees awarded in Social Sciences.
#PCIP46: Percentage of degrees awarded in Construction Trades.
#PCIP47: Percentage of degrees awarded in Mechanic And Repair Technologies/Technicians.
#PCIP48: Percentage of degrees awarded in Precision Production.
#PCIP49: Percentage of degrees awarded in Transportation And Materials Moving.
#PCIP50: Percentage of degrees awarded in Visual And Performing Arts.
#PCIP51: Percentage of degrees awarded in Health Professions And Related Programs.
#PCIP52: Percentage of degrees awarded in Business, Management, Marketing, And Related Support Services.
#PCIP54: Percentage of degrees awarded in History.
#NPT4_PUB: Average net price for Title IV institutions (public institutions)
#NPT41_PUB: Average net price for $0-$30,000 family income (public institutions)
#NPT42_PUB: Average net price for $30,001-$48,000 family income (public institutions)
#NPT43_PUB: Average net price for $48,001-$75,000 family income (public institutions)
#NPT44_PUB: Average net price for $75,001-$110,000 family income (public institutions)
#NPT45_PUB: Average net price for $110,000+ family income (public institutions)
#NUM4_PUB: Number of Title IV students (public institutions)
#NUM41_PUB: Number of Title IV students, $0-$30,000 family income (public institutions)
#NUM42_PUB: Number of Title IV students, $30,001-$48,000 family income (public institutions)
#NUM43_PUB: Number of Title IV students, $48,001-$75,000 family income (public institutions)
#NUM44_PUB: Number of Title IV students, $75,001-$110,000 family income (public institutions)
#NUM45_PUB: Number of Title IV students, $110,000+ family income (public institutions)
#COSTT4_A: Average cost of attendance (academic year institutions)
#TUITIONFEE_IN: In-state tuition and fees
#TUITIONFEE_OUT: Out-of-state tuition and fees
#AVGFACSAL: Average faculty salary
#PFTFAC: Proportion of faculty that is full-time
#AGE_ENTRY: Average age of entry, via SSA data
#FEMALE: Share of female students, via SSA data
#MARRIED: Share of married students
#DEPENDENT: Share of dependent students
#VETERAN: Share of veteran students
#FIRST_GEN: Share of first-generation students
#C150_4: Completion rate for first-time, full-time students at four-year institutions (150% of expected time to completion/6 years)
#UGDS: Enrollment of undergraduate certificate/degree-seeking students
#UGDS_WHITE: Total share of enrollment of undergraduate degree-seeking students who are white
#UGDS_BLACK: Total share of enrollment of undergraduate degree-seeking students who are black
#UGDS_HISP: Total share of enrollment of undergraduate degree-seeking students who are Hispanic
#UGDS_ASIAN: Total share of enrollment of undergraduate degree-seeking students who are Asian
#UGDS_AIAN: Total share of enrollment of undergraduate degree-seeking students who are American Indian/Alaska Native
#UGDS_NHPI: Total share of enrollment of undergraduate degree-seeking students who are Native Hawaiian/Pacific Islander
#UGDS_2MOR: Total share of enrollment of undergraduate degree-seeking students who are two or more races
#UGDS_NRA: Total share of enrollment of undergraduate degree-seeking students who are non-resident aliens
#UGDS_UNKN: Total share of enrollment of undergraduate degree-seeking students whose race is unknown
#PPTUG_EF: Share of undergraduate, degree-/certificate-seeking students who are part-time
#NPT4_048_PUB: Average net price for $0-$48,000 family income (public institutions)
#NPT4_3075_PUB: Average net price for $30,001-$75,000 family income (public institutions)
#NPT4_75UP_PUB: Average net price for $75,000+ family income (public institutions)
#COMP_ORIG_YR6_RT: Percent completed within 6 years at original institution
#LO_INC_COMP_ORIG_YR6_RT: Percent of low-income (less than $30,000 in nominal family income) students who completed within 6 years at original institution
#MD_INC_COMP_ORIG_YR6_RT: Percent of middle-income (between $30,000 and $75,000 in nominal family income) students who completed within 6 years at original institution
#HI_INC_COMP_ORIG_YR6_RT: Percent of high-income (above $75,000 in nominal family income) students who completed within 6 years at original institution



#Qualitative variables
qualitative <- selected_data %>%
  select(REGION)

#REGION
#0	U.S. Service Schools
#1	New England (CT, ME, MA, NH, RI, VT)
#2	Mid East (DE, DC, MD, NJ, NY, PA)
#3	Great Lakes (IL, IN, MI, OH, WI)
#4	Plains (IA, KS, MN, MO, NE, ND, SD)
#5	Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV)
#6	Southwest (AZ, NM, OK, TX)
#7	Rocky Mountains (CO, ID, MT, UT, WY)
#8	Far West (AK, CA, HI, NV, OR, WA)
#9	Outlying Areas (AS, FM, GU, MH, MP, PR, PW, VI)
#===========================================================================================================================

#Summary statistics of quantitative variables

#min, 1st quartile, median, mean, 3rd quartile, max
quantitative_summary <- summary(quantitative)
#range
quantitative_range <- apply(quantitative, 2, FUN=range)
#IQR
f_iqr<-function(x){IQR(x,na.rm=TRUE)}
quantitative_IQR <- apply(quantitative, 2, FUN=f_iqr)
#standard deviation
quantitative_SD <- apply(quantitative, 2, FUN=sd)

#---------------------------------------------------------------------------------------------------------------------------
#Histograms

#histogram of average ACT scores
png("images/histogram_ACT_avg.png")
hist(clean_data$ACTCMMID, main = "Histogram of Average of ACT Scores", xlab = "ACT average")
dev.off()

#histogram of average SAT scores
png("images/histogram_SAT_avg.png")
hist(clean_data$SAT_AVG, main = "Histogram of Average of SAT Scores", xlab = "SAT average")
dev.off()

#histogram of percentage of degrees of different majors
pairs_majors1 = clean_data[, c("PCIP01", "PCIP03", "PCIP04", "PCIP05", "PCIP09", "PCIP10", "PCIP11", "PCIP12", "PCIP13")]
png("images/histogram_majors1.png")
attach(pairs_majors1)
graph_majors1 <- par(mfrow = c(3, 3))
hist(PCIP01, xlab = "Agriculture, Agriculture Operations, and Related Sciences", main = "% of Degrees")
hist(PCIP03, xlab = "Natural Resources and Conservation", main = "% of Degrees")
hist(PCIP04, xlab = "Architecture and Related Services", main = "% of Degrees")
hist(PCIP05, xlab = "Area, Ethnic, Cultural, Gender, and Group Studies", main = "% of Degrees")
hist(PCIP09, xlab = "Communication, Journalism, and Related Programs", main = "% of Degrees")
hist(PCIP10, xlab = "Communications Technologies/Technicians and Support Services", main = "% of Degrees")
hist(PCIP11, xlab = "Computer and Information Sciences and Support Services", main = "% of Degrees")
hist(PCIP12, xlab = "Personal and Culinary Services", main = "% of Degrees")
hist(PCIP13, xlab = "Education", main = "% of Degrees")
par(graph_majors1)
dev.off()

pairs_majors2 = clean_data[, c("PCIP14", "PCIP15", "PCIP16", "PCIP19", "PCIP22", "PCIP23", "PCIP24", "PCIP25", "PCIP26")]
png("images/histogram_majors2.png")
attach(pairs_majors2)
graph_majors2 <- par(mfrow = c(3, 3))
hist(PCIP14, xlab = "Engineering", main = "% of Degrees")
hist(PCIP15, xlab = "Engineering Technologies and Engineering-Related Fields", main = "% of Degrees")
hist(PCIP16, xlab = "Foreign Languages, Literatures, and Linguistics", main = "% of Degrees")
hist(PCIP19, xlab = "Family and Consumer Sciences/Human Sciences", main = "% of Degrees")
hist(PCIP22, xlab = "Legal Professions and Studies", main = "% of Degrees")
hist(PCIP23, xlab = "English Language and Literature/Letters", main = "% of Degrees")
hist(PCIP24, xlab = "Liberal Arts and Sciences, General Studies and Humanities", main = "% of Degrees")
hist(PCIP25, xlab = "Library Science", main = "% of Degrees")
hist(PCIP26, xlab = "Biological and Biomedical Sciences", main = "% of Degrees")
par(graph_majors2)
dev.off()

pairs_majors3 = clean_data[, c("PCIP27", "PCIP29", "PCIP30", "PCIP31", "PCIP38", "PCIP39", "PCIP40", "PCIP41", "PCIP42")]
png("images/histogram_majors3.png")
attach(pairs_majors3)
graph_majors3 <- par(mfrow = c(3, 3))
hist(PCIP27, xlab = "Mathematics and Statistics", main = "% of Degrees")
hist(PCIP29, xlab = "Military Technologies and Applied Sciences", main = "% of Degrees")
hist(PCIP30, xlab = "Multi/Interdisciplinary Studies", main = "% of Degrees")
hist(PCIP31, xlab = "Parks, Recreation, Leisure, and Fitness Studies", main = "% of Degrees")
hist(PCIP38, xlab = "Philosophy and Religious Studies", main = "% of Degrees")
hist(PCIP39, xlab = "Theology and Religious Vocations", main = "% of Degrees")
hist(PCIP40, xlab = "Physical Sciences", main = "% of Degrees")
hist(PCIP41, xlab = "Science Technologies/Technicians", main = "% of Degrees")
hist(PCIP42, xlab = "Psychology", main = "% of Degrees")
par(graph_majors3)
dev.off()

pairs_majors4 = clean_data[, c("PCIP43", "PCIP44", "PCIP45", "PCIP46", "PCIP47", "PCIP48", "PCIP49", "PCIP50", "PCIP51", "PCIP52", "PCIP54")]
png("images/histogram_majors4.png")
attach(pairs_majors4)
graph_majors4 <- par(mfrow = c(4, 3))
hist(PCIP43, xlab = "Homeland Security, Law Enforcement, Firefighting and Related Protective Services", main = "% of Degrees")
hist(PCIP44, xlab = "Public Administration and Social Service Professions", main = "% of Degrees")
hist(PCIP45, xlab = "Social Sciences", main = "% of Degrees")
hist(PCIP46, xlab = "Construction Trades", main = "% of Degrees")
hist(PCIP47, xlab = "Mechanic and Repair Technologies/Technicians", main = "% of Degrees")
hist(PCIP48, xlab = "Precision Production", main = "% of Degrees")
hist(PCIP49, xlab = "Transportation and Materials Moving", main = "% of Degrees")
hist(PCIP50, xlab = "Visual and Performing Arts", main = "% of Degrees")
hist(PCIP51, xlab = "Health Professions and Related Programs", main = "% of Degrees")
hist(PCIP52, xlab = "Business, Management, Marketing, and Related Support Services", main = "% of Degrees")
hist(PCIP54, xlab = "History")
par(graph_majors4)
dev.off()

#histogram of enrollment by race
pairs_race_enrollment = clean_data[, c("UGDS_WHITE", "UGDS_BLACK", "UGDS_HISP", "UGDS_ASIAN", "UGDS_AIAN", "UGDS_NHPI")]
png("images/histogram_race_enrollment.png")
attach(pairs_race_enrollment)
graph_race_enrollment <- par(mfrow = c(3, 2))
hist(UGDS_WHITE, xlab = "White", main = "Histogram of Share of Enrollment")
hist(UGDS_BLACK, xlab = "Black", main = "Histogram of Share of Enrollment")
hist(UGDS_HISP, xlab = "Hispanic", main = "Histogram of Share of Enrollment")
hist(UGDS_ASIAN, xlab = "Asian", main = "Histogram of Share of Enrollment")
hist(UGDS_AIAN, xlab = "American Indian/Alaska Native", main = "Histogram of Share of Enrollment")
hist(UGDS_NHPI, xlab = "Hawaiian/Pacific Islander", main = "Histogram of Share of Enrollment")
par(graph_race_enrollment)
dev.off()

#histogram of average cost per academic year
png("images/histogram_net_price.png")
hist(clean_data$COSTT4_A, main = "Histogram of Average Cost per Academic Year", xlab = "Average Cost per Academic Year")
dev.off()

#histogram of completion rate
png("images/histogram_completion.png")
hist(clean_data$C150_4, main = "Histogram of Completion Rate", xlab = "Completion Rate")
dev.off()

#histogram of completion rate by race (black, white, hisp, asian, native american, nhpi)
pairs_race_completion = clean_data[, c("C150_4_WHITE", "C150_4_BLACK", "C150_4_HISP", "C150_4_AIAN", "C150_4_ASIAN", "C150_4_NHPI")]
png("images/histogram_race_completion")
attach(pairs_race_completion)
graph_race_completion <- par(mfrow = c(3, 2))
hist(C150_4_WHITE, xlab = "White", main = "Histogram of Completion Rate in 6 Years")
hist(C150_4_BLACK, xlab = "Black", main = "Histogram of Completion Rate in 6 Years")
hist(C150_4_HISP, xlab = "Hispanic", main = "Histogram of Completion Rate in 6 Years")
hist(C150_4_AIAN, xlab = "American Indian/Alaska Native", main = "Histogram of Completion Rate in 6 Years")
hist(C150_4_ASIAN, xlab = "Asian", main = "Histogram of Completion Rate in 6 Years")
hist(C150_4_NHPI, xlab = "Hawaiian/Pacific Islander", main = "Histogram of Completion Rate in 6 Years")
par(graph_race_completion)
dev.off()

#---------------------------------------------------------------------------------------------------------------------------
#Summary statistics of qualitative variables
summary_qual <- summary(qualitative)

#Table of Relative Frequency of Qualitative Variables
freq_qual <- table(clean_data$REGION)
#---------------------------------------------------------------------------------------------------------------------------
frequency=as.numeric(table(clean_data$REGION))
proportions=frequency/sum(frequency)
rownames=names(table(clean_data$REGION))
df=data.frame(proportions, frequency)
rownames(df)=rownames

png("images/conditional_boxplot_region.png")
plot(table(clean_data$REGION))
dev.off()
#Barchart for Region qualitative variable
png("images/barchart_region.png")
barplot(table(clean_data$REGION), main="Barchart of Region")
dev.off
#Conditional Boxplots for Region qualitative vairables
png("images/conditional_boxplot_region.png")
boxplot(C150_4 ~ REGION, data=clean_data, main="Conditional Boxplot of Region")
dev.off
#Anova's between Completion rate and all qualitative variables
anova <- aov(C150_4 ~ REGION, clean_data)
#===========================================================================================================================
#Correlation Matrix
matrix <- cor(clean_data)
save(matrix, file = "data/generated_data/correlation_matrix.RData")

#Anova's between Completion rate and all qualitative variables
anova <- aov(C150_4 ~ REGION, clean_data)
#===========================================================================================================================
#Generate eda.txt for quantitative variables
sink("data/generated_data/eda.txt")
#Summary statistics of quantitative variables
cat("Summary Statistics of Quantitative Variables\n")
cat("\n\n")
#min, 1st quartile, median, mean, 3rd quartile, max
cat("Min, 1st Quartile, Median, Mean, 3rd Quartile, Max of quantitative variables\n")
print(quantitative_summary)
cat("\n\n")
#range
cat("Range of Quantitative Variables\n")
print(quantitative_range)
cat("\n\n")
#IQR
cat("IQR of Quantitative Variables\n")
print(quantitative_IQR)
cat("\n\n")
#standard deviation
cat("Standard Deviation of Quantitative Variables\n")
print(quantitative_SD)
cat("\n\n")

#Summary statistics of Qualitative variables
cat("Summary Statistics of Qualitative Variables\n")
print(summary_qual)
cat("\n\n")
#Table of frequency
cat("Table of Frequency of qualitative variables\n")
print(freq_qual)
cat("\n\n")

cat("Additional data\n")
cat("\n\n")
#Matrix of Correlation
cat("Matrix of Correlation\n")
print(matrix)
cat("\n\n")
#Anova between Completion Rate and qualitative varible
cat("Anova's between Completion Rate and qualitative variables\n")
print(anova)
cat("\n\n")
sink()

