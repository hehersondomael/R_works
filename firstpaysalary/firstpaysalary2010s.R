
'
install.packages("dplyr")
install.packages("gdata")
install.packages("plyr")
install.packages("xlsx")

library(dplyr)
library(gdata)
library(plyr)
library(xlsx)
'

'
# Read xlsx file
my_data <- read.xlsx("firstpaysurvey.xlsx", 1, sheetName = "Sheet1")
# View(my_data)
'

# years <- data.frame(years=unique(my_data %>% pull(startYear)))
# View(years[order(years),])
layout(matrix(c(1,2), 2, 1, byrow = TRUE))

year2010 <- filter(my_data, startYear==2010)
year2011 <- filter(my_data, startYear==2011)
year2012 <- filter(my_data, startYear==2012)
year2013 <- filter(my_data, startYear==2013)
year2014 <- filter(my_data, startYear==2014)
year2015 <- filter(my_data, startYear==2015)
year2016 <- filter(my_data, startYear==2016)
year2017 <- filter(my_data, startYear==2017)
year2018 <- filter(my_data, startYear==2018)
year2019 <- filter(my_data, startYear==2019)

year2010mean <- mean(year2010$monthlySalary[!is.na(year2010$monthlySalary)])
year2011mean <- mean(year2011$monthlySalary[!is.na(year2011$monthlySalary)])
year2012mean <- mean(year2012$monthlySalary[!is.na(year2012$monthlySalary)])
year2013mean <- mean(year2013$monthlySalary[!is.na(year2013$monthlySalary)])
year2014mean <- mean(year2014$monthlySalary[!is.na(year2014$monthlySalary)])
year2015mean <- mean(year2015$monthlySalary[!is.na(year2015$monthlySalary)])
year2016mean <- mean(year2016$monthlySalary[!is.na(year2016$monthlySalary)])
year2017mean <- mean(year2017$monthlySalary[!is.na(year2017$monthlySalary)])
year2018mean <- mean(year2018$monthlySalary[!is.na(year2018$monthlySalary)])
year2019mean <- mean(year2019$monthlySalary[!is.na(year2019$monthlySalary)])

# Create the data for the chart.
yearMean <- c(year2010mean,year2011mean,year2012mean,year2013mean,
              year2014mean,year2015mean,year2016mean,year2017mean,
              year2018mean,year2019mean)
years <- c("2010", "2011", "2012", "2013", "2014", "2015",
           "2016", "2017", "2018", "2019")

yearMeanChart <- plot(years, yearMean, type="o", xlab="Year (2010s)",
     main="Average First Pay Salary through the Years", ylab="Amount (in PHP)",
     ylim=c(15000,25000), xaxt="n")
axis(1, xaxp=c(2010, 2020, 10), las=1)

text(years, yearMean+799,
     labels=format(round(yearMean, 2),nsmall = 2), cex=0.6)


# gender <- data.frame(years=unique(my_data %>% pull(gender)))
# View(gender[order(gender),])

maleEntries <- filter(my_data, (gender=="Male" | gender=="Heterosexual Male" |
  gender=="Male" | gender=="m" | gender=="M" | gender=="Make" | 
  gender=="male" | gender=="MALE" | gender=="Male " | gender=="Males" | 
  gender=="Man" | gender=="Mqle"))

year2010Males <- filter(maleEntries, startYear==2010)
year2011Males <- filter(maleEntries, startYear==2011)
year2012Males <- filter(maleEntries, startYear==2012)
year2013Males <- filter(maleEntries, startYear==2013)
year2014Males <- filter(maleEntries, startYear==2014)
year2015Males <- filter(maleEntries, startYear==2015)
year2016Males <- filter(maleEntries, startYear==2016)
year2017Males <- filter(maleEntries, startYear==2017)
year2018Males <- filter(maleEntries, startYear==2018)
year2019Males <- filter(maleEntries, startYear==2019)

year2010MalesMean <- mean(year2010Males$monthlySalary[!is.na(year2010Males$monthlySalary)])
year2011MalesMean <- mean(year2011Males$monthlySalary[!is.na(year2011Males$monthlySalary)])
year2012MalesMean <- mean(year2012Males$monthlySalary[!is.na(year2012Males$monthlySalary)])
year2013MalesMean <- mean(year2013Males$monthlySalary[!is.na(year2013Males$monthlySalary)])
year2014MalesMean <- mean(year2014Males$monthlySalary[!is.na(year2014Males$monthlySalary)])
year2015MalesMean <- mean(year2015Males$monthlySalary[!is.na(year2015Males$monthlySalary)])
year2016MalesMean <- mean(year2016Males$monthlySalary[!is.na(year2016Males$monthlySalary)])
year2017MalesMean <- mean(year2017Males$monthlySalary[!is.na(year2017Males$monthlySalary)])
year2018MalesMean <- mean(year2018Males$monthlySalary[!is.na(year2018Males$monthlySalary)])
year2019MalesMean <- mean(year2019Males$monthlySalary[!is.na(year2019Males$monthlySalary)])

yearMalesMean <- c(year2010MalesMean,year2011MalesMean,year2012MalesMean,
              year2013MalesMean,year2014MalesMean,year2015MalesMean,
              year2016MalesMean,year2017MalesMean,year2018MalesMean,
              year2019MalesMean)
years <- c("2010", "2011", "2012", "2013", "2014", "2015",
           "2016", "2017", "2018", "2019")
femaleEntries <- filter(my_data, (gender=="*Sex = Female" |
                                    gender=="Babae" |
                                    gender=="Biological Female" |
                                    gender=="F" | gender=="Femae" |
                                    gender=="Femail" | 
                                    gender=="Femaile" | gender=="Femal" |
                                    gender=="female" | gender=="Female" | 
                                    gender=="FEMALE" | gender=="Female " |
                                    gender=="FEMALE " | gender=="Femalr" |
                                    gender=="Femalw" | gender=="feme" |
                                    gender=="Feme" | gender=="Frmale" |
                                    gender=="Heterosexual Female" | 
                                    gender=="Woman" | gender=="Women"))

year2010Females <- filter(femaleEntries, startYear==2010)
year2011Females <- filter(femaleEntries, startYear==2011)
year2012Females <- filter(femaleEntries, startYear==2012)
year2013Females <- filter(femaleEntries, startYear==2013)
year2014Females <- filter(femaleEntries, startYear==2014)
year2015Females <- filter(femaleEntries, startYear==2015)
year2016Females <- filter(femaleEntries, startYear==2016)
year2017Females <- filter(femaleEntries, startYear==2017)
year2018Females <- filter(femaleEntries, startYear==2018)
year2019Females <- filter(femaleEntries, startYear==2019)

year2010FemalesMean <- mean(year2010Females$monthlySalary[!is.na(year2010Females$monthlySalary)])
year2011FemalesMean <- mean(year2011Females$monthlySalary[!is.na(year2011Females$monthlySalary)])
year2012FemalesMean <- mean(year2012Females$monthlySalary[!is.na(year2012Females$monthlySalary)])
year2013FemalesMean <- mean(year2013Females$monthlySalary[!is.na(year2013Females$monthlySalary)])
year2014FemalesMean <- mean(year2014Females$monthlySalary[!is.na(year2014Females$monthlySalary)])
year2015FemalesMean <- mean(year2015Females$monthlySalary[!is.na(year2015Females$monthlySalary)])
year2016FemalesMean <- mean(year2016Females$monthlySalary[!is.na(year2016Females$monthlySalary)])
year2017FemalesMean <- mean(year2017Females$monthlySalary[!is.na(year2017Females$monthlySalary)])
year2018FemalesMean <- mean(year2018Females$monthlySalary[!is.na(year2018Females$monthlySalary)])
year2019FemalesMean <- mean(year2019Females$monthlySalary[!is.na(year2019Females$monthlySalary)])

yearFemalesMean <- c(year2010FemalesMean,year2011FemalesMean,year2012FemalesMean,
                     year2013FemalesMean,year2014FemalesMean,year2015FemalesMean,
                     year2016FemalesMean,year2017FemalesMean,year2018FemalesMean,
                     year2019FemalesMean)
years <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016",
           "2017", "2018", "2019")

yearMeanChart <- plot(years, yearMalesMean, type="o", xlab="Year (2010s)",
                      lwd=1.5, main="Average First Pay Salary through the Years\nMale vs. Female", ylab="Amount (in PHP)",
                      ylim=c(10000,35000), xaxt="n")
axis(1, xaxp=c(2010, 2020, 10), las=1)
lines(years, yearFemalesMean, lwd=1.5, col="red", type="o")

text(years, yearMalesMean+4000,
     labels=format(round(yearMalesMean, 2),nsmall = 2), cex=0.6)
text(years, yearMalesMean+2500,
     labels=format(round(yearFemalesMean, 2),nsmall = 2), cex=0.6,
     col="red")