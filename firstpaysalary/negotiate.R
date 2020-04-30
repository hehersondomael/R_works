# User library: dplyr, gdata, plyr, xlsx
# System library: base, datasets, graphics, grDevices, methods, stats, utils

# Read xlsx file
my_data <- read.xlsx("firstpaysurvey.xlsx",1)

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))


# NEGOTIATED PERCENTAGE (PLM)
PLMayers <- filter(my_data, (school=="PLM" | school=="Pamantasan ng Lungsod ng Maynila"))
counts <- table(PLMayers$negotiated)
barplot(counts, main="Who negotiated among PLM grads?",
        xlab="Response", las=2, horiz=TRUE, col=c("#dcd0c0", "#373737"))


# NEGOTIATED PERCENTAGE (ALL)
yes <- as.integer(count(filter(my_data, negotiated=="Yes")))
no <- as.integer(count(filter(my_data, negotiated=="No")))
undisclosed <- as.integer(nrow(my_data))-(yes+no)

slices <- c(yes, no, undisclosed)

lbls <- c("Yes", "No", "Undisclosed")
pct <- paste("(", round(slices/sum(slices)*100, digits=1), "%)", sep="")
lbls <- paste(lbls, ": ", slices, " ", pct, sep="")

pie(slices, labels = lbls, radius=1, col=c("#c0b283", "#dcd0c0", "#373737"), main="Did you negotiate your first salary?")


# NEGOTIATED PERCENTAGE (UP)
UPGrads_1 <- data.frame(schools=unique(my_data %>% pull(school)))
undisclosedUPGrad_1 <- as.integer(count(subset(my_data %>% filter(my_data$school %in% UPGrads_1[grep("^UP", UPGrads_1$schools),]),
                                               is.na(negotiated), )))
yesUPGrad_1 <- as.integer(count(subset(my_data %>% filter(my_data$school %in% UPGrads_1[grep("^UP", UPGrads_1$schools),]),
                                       negotiated=="Yes", )))
noUPGrad_1 <- as.integer(count(subset(my_data %>% filter(my_data$school %in% UPGrads_1[grep("^UP", UPGrads_1$schools),]),
                                      negotiated=="No",)))

UPGrads_2 <- data.frame(schools=unique(my_data %>% pull(school)))
undisclosedUPGrad_2 <- as.integer(count(subset(my_data %>% filter(my_data$school %in% UPGrads_2[grep("^University of the P", UPGrads_2$schools),]),
                                               is.na(negotiated), select=c(negotiated))))
yesUPGrad_2 <- as.integer(count(subset(my_data %>% filter(my_data$school %in% UPGrads_2[grep("^University of the P", UPGrads_2$schools),]),
                                       negotiated=="Yes", select=c(negotiated))))
noUPGrad_2 <- as.integer(count(subset(my_data %>% filter(my_data$school %in% UPGrads_2[grep("^University of the P", UPGrads_2$schools),]),
                                      negotiated=="No", select=c(negotiated))))

undisclosedUPGrad <- undisclosedUPGrad_1 + undisclosedUPGrad_2
yesUPGrad <- yesUPGrad_1 + yesUPGrad_2
noUPGrad <- noUPGrad_1 + noUPGrad_2
slices <- c(yesUPGrad, noUPGrad, undisclosedUPGrad)

lbls <- c("Yes", "No", "Undisclosed")
pct <- paste("(", round(slices/sum(slices)*100, digits=1), "%)", sep="")
lbls <- paste(lbls, ": ", slices, " ", pct, sep="")

pie(slices, labels = lbls, radius=1, col=c("#c0b283", "#dcd0c0", "#373737"), main="Who negotiated among UP grads?")