# System library: base, datasets, graphics, grDevices, methods, stats, utils
'
install.packages("dplyr")
install.packages("gdata")
install.packages("plyr")
install.packages("xlsx")
'
'
library(dplyr)
library(gdata)
library(plyr)
library(xlsx)
'

# Read xlsx file
# my_data <- read.xlsx("firstpaysurvey.xlsx", 1, sheetName = "Sheet1")

# listOfSchools <- data.frame(schools=unique(my_data %>% pull(school)))
# View(listOfSchools)

AdU <- filter(my_data, (school=="Adamson" |
                          school=="adamson university" |
                          school=="Adamson University " |
                          school=="Adamson"))
# View(AdU)
ADMU <- filter(my_data, (school=="Ateneo de Manila University" |
                           school=="Ateneo de Manila University " |
                           school=="Ateneo " |
                           school=="Ateneo" |
                           school=="Ateneo de Manila" |
                           school=="Ateneo de Manila " |
                           school=="Ateneo De Manila University" |
                           school=="Ateneo de Manila University (BS)/ EU-based University (MS)" |
                           school=="Ateneo De Manila" |
                           school=="Ateneo de manila university" |
                           school=="ADMU" |
                           school=="AdMU" |
                           school=="Admu" |
                           school=="Admu "))
#View(ADMU)
DLSU <- filter(my_data, (school=="De La Salle University Manila" |
                           school=="De La Salle University " |
                           school=="De La Salle University" |
                           school=="De La Salle University-Manila" |
                           school=="de la salle university - manila" |
                           school=="De La Salle University - Manila" |
                           school=="De La Salle University - manila" |
                           school=="La Salle" |
                           school=="DLSU" |
                           school=="Dlsu" |
                           school=="DLSU-M" |
                           school=="DLSU - Manila" |
                           school=="DLSU Manila" |
                           school=="DLSU-Manila" |
                           school=="DLSU - M" |
                           school=="DLSU MANILA"))
#View(DLSU)
FEU <- filter(my_data, (school=="Far Eastern University" |
                           school=="Far Eastern University - Makati" |
                           school=="Far Eastern University Manila" |
                           school=="Far Eastern University, Manila" |
                           school=="FAR EASTERN UNIVERSITY - MANILA" |
                           school=="Far Eastern University - Manila" |
                           school=="Far Eastern University " |
                           school=="feu" |
                           school=="FEU " |
                           school=="FEU" |
                           school=="FEU - Manila" |
                           school=="FEU Manila" |
                           school=="FEU MANILA"))
# View(FEU)
NU <- filter(my_data, (school=="National University" |
                          school=="National University Manila"))
# View(NU)
UE <- filter(my_data, (school=="UE" |
                          school=="UE Manila" |
                          school=="UE Caloocan" |
                          school=="University of the East-Caloocan" |
                          school=="University of the East-Manila " |
                          school=="University of the East" |
                          school=="University of the East- Manila" |
                          school=="University of the East - Manila" |
                          school=="University of the East-Manila"))
# View(UE)
UP <- filter(my_data, (school=="UP Diliman" | 
                             school=="University of the Philippines - Diliman" |
                             school=="University of the Philippines Diliman" |
                             school=="UP diliman" |
                             school=="University of the Philippines, Diliman" |
                             school=="Up Diliman" |
                             school=="UP-Diliman" |
                             school=="Univeristy of the Philippines Diliman" |
                             school=="UP Diliman " |
                             school=="Up diliman" |
                             school=="University of the Philippines Diliman " |
                             school=="University of Philippines Diliman" |
                             school=="University of the Philippines- Diliman" |
                             school=="University of the Philippines (Diliman)" |
                             school=="UP DILIMAN" |
                             school=="University of the Philiippines Diliman" |
                             school=="UPD" |
                             school=="UP" |
                             school=="UP " |
                             school=="Up diliman" |
                             school=="UP Undergrad at the time" |
                             school=="UP Diliiman" |
                             school=="University of the Philippines " |
                             school=="University of the Philippine" |
                             school=="University of the Philippines" |
                             school=="University of The Philippines"))
# View(UP)
UST <- filter(my_data, (school=="University of Santo Tomas" | 
                          school=="University of Sto. Tomas" |
                          school=="University of Santo Tomas " |
                          school=="University of Sto Tomas" |
                          school=="University of Santo Tomas, Manila" |
                          school=="University of Santo Tomas Manila" |
                          school=="UST" |
                          school=="ust" |
                          school=="uste" |
                          school=="Ust" |
                          school=="Ust " |
                          school=="UST "))
# View(UST)
AdUmean <- mean(AdU$monthlySalary[!is.na(AdU$monthlySalary)])
ADMUmean <- mean(ADMU$monthlySalary[!is.na(ADMU$monthlySalary)])
DLSUmean <- mean(DLSU$monthlySalary[!is.na(DLSU$monthlySalary)])
FEUmean <- mean(FEU$monthlySalary[!is.na(FEU$monthlySalary)])
NUmean <- mean(NU$monthlySalary[!is.na(NU$monthlySalary)])
UEmean <- mean(UE$monthlySalary[!is.na(UE$monthlySalary)])
UPmean <- mean(UP$monthlySalary[!is.na(UP$monthlySalary)])
USTmean <- mean(UST$monthlySalary[!is.na(UST$monthlySalary)])

UAAPaverage <- c(AdUmean, ADMUmean, DLSUmean, FEUmean,
              NUmean, UEmean, UPmean, USTmean)
UAAPschools <- c("AdU", "ADMU", "DLSU", "FEU",
                 "NU", "UE", "UP", "UST")
UAAPchart <- barplot(UAAPaverage,
        main="Average first pay monthly salary\nof UAAP school grads (in PHP)",
        names.arg=UAAPschools, xlab="UAAP schools", ylim=c(0,30000),
        cex.names=0.9, cex.axis=0.9, beside=TRUE,
        col=c("#e37222", "#07889b", "#66b9bf","#eeaa7b",
              "#e37222", "#07889b", "#66b9bf","#eeaa7b"),
        las=1, horiz=FALSE)
abline(h=0)
text(UAAPchart, UAAPaverage+1000, labels = format(round(UAAPaverage, 2), nsmall = 2),cex=0.9)