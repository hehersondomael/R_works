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
# my_data <- read.xlsx("firstpaysurvey.xlsx",1)
# View(my_data)
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

'
# View unique entries
listOfSchools <- data.frame(schools=unique(my_data %>% pull(school)))
View(listOfSchools)
'

# 1. NEGOTIATED PERCENTAGE (PLM)
PLMayers <- filter(my_data, (school=="PLM" | 
                             school=="Pamantasan ng Lungsod ng Maynila" |
                             school=="Pamantasan ng Lungsod ng maynila" |
                             school=="Pamantasan ng Lungsod ng Maynila " |
                             school=="PAMANTASAN NG LUNGSOD NG MAYNILA"))
counts <- table(PLMayers$negotiated, useNA="always")
counts <- counts[c(2,1)]
barplot(counts, main="How many negotiated among PLM grads?",
        xlab="Response", ylim=c(0,30), las=2, horiz=FALSE,
        col=c("#dcd0c0", "#373737"))

# 2. NEGOTIATED PERCENTAGE (TIP)
TIPians <- filter(my_data, (school=="Technological Institute of the Philippines - Manila" | 
                             school=="Technological Institute of the Philippines" |
                             school=="TIP Manila" |
                             school=="TIP-MNL" |
                             school=="TIP - Manila"))
counts <- table(TIPians$negotiated, useNA="always")
counts <- counts[c(2,1)]
barplot(counts, main="How many negotiated among TIP grads?",
        xlab="Response", ylim=c(0,30), las=2, horiz=FALSE,
        col=c("#c0b283", "#373737"))


# 3. GENDER PERCENTAGE OF UP GRADS WHO NEGOTIATED THEIR SALARY
'
listOfSchools <- data.frame(schools=unique(my_data %>% pull(school)))
undisclosedUPGrad_1 <-
  nrow(
    subset(
      my_data %>% filter(my_data$school %in%
                         listOfSchools[grep("^UP", listOfSchools$schools), ]),
      is.na(negotiated), ))
yesUPGrad_1 <-
  nrow(
    subset(
      my_data %>% filter(my_data$school %in% 
                         listOfSchools[grep("^UP", listOfSchools$schools),]),
      negotiated=="Yes", ))
noUPGrad_1 <-
  nrow(
    subset(my_data %>% filter(my_data$school %in%
                              listOfSchools[grep("^UP", listOfSchools$schools),]),
    negotiated=="No", ))
'
UPGrad <- filter(my_data, (school=="UP Diliman" | 
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
                             school=="UP Diliman (didn't finish)" |
                             school=="University of the Philippines (Diliman)" |
                             school=="UP DILIMAN" |
                             school=="University of the Philiippines Diliman" |
                             school=="(deferred) University of the Philippines - Diliman" |
                             school=="UPD"))
negotiatedUPGrad <-filter(UPGrad, negotiated=="Yes")
negotiatedUPGradMale <- filter(negotiatedUPGrad, (gender=="Male" |
                                                    gender=="M"))
negotiatedUPGradFemale <- filter(negotiatedUPGrad, (gender=="Female" |
                                                    gender=="Female " |
                                                    gender=="F"))
negotiatedUPGradBinary <- nrow(negotiatedUPGradMale)+nrow(negotiatedUPGradFemale)
negotiatedUPGradOther <- nrow(negotiatedUPGrad)-negotiatedUPGradBinary
slices <- c(nrow(negotiatedUPGradFemale),
            nrow(negotiatedUPGradMale),negotiatedUPGradOther)
lbls <- c("Female", "Male","Other")
pct <- paste("(", round(slices/sum(slices)*100, digits=1), "%)", sep="")
lbls <- paste(lbls, ": ", slices, " ", pct, sep="")
pie(slices, labels = lbls, radius=1, col=c("#c0b283", "#dcd0c0", "#373737"),
    main="How genders fared among UPD grads\nwho negotiated their first pay salary?"
    ,init.angle=90)

# NEGOTIATED PERCENTAGE (ALL)
yes <- nrow(filter(my_data, negotiated=="Yes"))
no <- nrow(filter(my_data, negotiated=="No"))
undisclosed <- nrow(my_data)-(yes+no)
slices <- c(yes, no, undisclosed)
lbls <- c("Yes", "No", "Undisclosed")
pct <- paste("(", round(slices/sum(slices)*100, digits=1), "%)", sep="")
lbls <- paste(lbls, ": ", slices, " ", pct, sep="")
pie(slices, labels = lbls, radius=1,
    col=c("#c0b283", "#dcd0c0", "#373737"),
    main="How many negotiated their first salary?\n(Overall) ")