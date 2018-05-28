

rm(list=ls())
library(languageR)
library(lme4)
library(lmerTest)
library(psych)
library(stringr)


####
#### Read in the dataset
####

ibex_raw_l1 <- read.delim("list1.txt",header=TRUE)
ibex_raw_l2 <- read.delim("list2.txt",header=TRUE)
ibex_raw_l3 <- read.delim("list3.txt",header=TRUE)
ibex_raw_l4 <- read.delim("list4.txt",header=TRUE)

ibex_raw <- rbind(ibex_raw_l1,ibex_raw_l2,ibex_raw_l3,ibex_raw_l4)
rm(ibex_raw_l1,ibex_raw_l2,ibex_raw_l3,ibex_raw_l4)

ibex_raw$Type <- factor(ibex_raw$Type)
ibex_raw$ParticipantIP <- factor(ibex_raw$ParticipantIP)
ibex_raw$Controller <- factor(ibex_raw$Controller)
ibex_raw$WorkerID <- factor(as.numeric(ibex_raw$WorkerID))
ibex_raw$WordNumber <- factor(ibex_raw$WordNumber)
ibex_raw$Item <- factor(ibex_raw$Item)
ibex_raw$TimeReceived <- NULL
ibex_raw$Group <- NULL
ibex_raw$Sentence <- NULL
ibex_raw$Newline <- NULL

summary(ibex_raw)

spr_raw <- subset(ibex_raw, substr(ibex_raw$Type,0,5) == "block" )
spr_raw$ItemNumber <- factor(gsub("block","",gsub("-","",regmatches(spr_raw$Type,regexpr("^block[[:digit:]]+\\-",spr_raw$Type,perl=TRUE)))))
spr_raw$Condition <- factor(gsub("-","",regmatches(spr_raw$Type,regexpr("\\-[a-zA-Z]+\\-",spr_raw$Type,perl=TRUE))))
spr_raw$List <- factor(gsub("-","",regmatches(spr_raw$Type,regexpr("\\-[a-zA-Z1-4]+$",spr_raw$Type,perl=TRUE))))
spr_raw$Type <- NULL

head(spr_raw)




###########################################################
####
####  Find comprehension question accuracy rates
####
###########################################################


WorkerIDList <- unique(spr_raw$WorkerID)
ratio <- c()
i <- 1

for (id in WorkerIDList) {
ratio[i] <- nrow(subset(spr_raw, spr_raw$WorkerID == id &  
                                 spr_raw$Controller == "Question" & 
                                 spr_raw$ReadingTime == "1")) / 12
    i = i+1
}

accuracy <- data.frame(ID = WorkerIDList, Ratio = ratio)
accuracy <- accuracy[order(accuracy$Ratio), ]
accuracy


###########################################################
####
####  Toss out bad participants below 75% cut-off for comprehension
####
###########################################################


if (FALSE) {
spr_raw <- spr_raw[which(spr_raw$WorkerID  != "bob" &
                         spr_raw$WorkerID != "mia" ), ]
 }
 

###########################################################
####
####  Remove fillers
####
###########################################################

spr_clean <- spr_raw[which( substr(spr_raw$Condition,0,6) != "filler"),]
spr_clean$Condition <- factor(spr_clean$Condition)
spr_clean$Controller <- NULL
spr_clean$ParticipantIP <- NULL
spr_clean$Element <- NULL

table(spr_clean$Condition)
table(spr_clean$List)
 
# See data set 
# spr_clean[order(spr_clean$WorkerID, spr_clean$Item),]

###########################################################
####
####  Toss out bad items
####
###########################################################


####
#### Set min/max for removing duds
####

## Conservative Dud Removal

dudMin6 <- 100
dudMax6 <- 1500


####
####  Remove duds
####

spr_clean <- spr_clean[which(spr_clean$ReadingTime > dudMin6 & spr_clean$ReadingTime < dudMax6), ]


####
####
#### Squash all region data that is more than 2.5 standard deviations away from the mean
#### 
####

totalReplacements <- 0


for(subj in unique(spr_clean$WorkerID)) {
tempMean <- mean(spr_clean[(spr_clean$WorkerID == subj), ]$ReadingTime)
tempSD <- sd(spr_clean[(spr_clean$WorkerID == subj), ]$ReadingTime)

tempReplacements <- length(spr_clean[which(spr_clean$WorkerID == subj & 
                                           spr_clean$ReadingTime > tempMean + 2.5*tempSD), ]$ReadingTime)
totalReplacements <- totalReplacements + tempReplacements

if (tempReplacements > 0) {
spr_clean[which(spr_clean$WorkerID == subj & 
                spr_clean$ReadingTime > tempMean + 2.5*tempSD),]$ReadingTime <- tempMean + 2.5*tempSD
}

tempReplacements <- length(spr_clean[which(spr_clean$WorkerID == subj & 
                                           spr_clean$ReadingTime < tempMean - 2.5*tempSD), ]$ReadingTime)
totalReplacements <- totalReplacements + tempReplacements

if (tempReplacements > 0) {
spr_clean[which(spr_clean$WorkerID == subj & 
                spr_clean$ReadingTime < tempMean - 2.5*tempSD),]$ReadingTime <- tempMean - 2.5*tempSD
}

}


totalReplacements

summary(spr_clean)


###########################################################
####
####  Add region lengths
####
###########################################################

spr_clean$Length <- str_length(spr_clean$Word)

###########################################################
####
#### LMER for Length Residualization
####
###########################################################

ls01 <- spr_clean[which(spr_clean$WorkerID == "1"), ]
ls02 <- spr_clean[which(spr_clean$WorkerID == "2"), ]
ls03 <- spr_clean[which(spr_clean$WorkerID == "3"), ]

if (FALSE) {
ls04 <- spr_clean[which(spr_clean$WorkerID == "4"), ]
ls05 <- spr_clean[which(spr_clean$WorkerID == "5"), ]
ls06 <- spr_clean[which(spr_clean$WorkerID == "6"), ]
ls07 <- spr_clean[which(spr_clean$WorkerID == "7"), ]
ls08 <- spr_clean[which(spr_clean$WorkerID == "8"), ]
ls09 <- spr_clean[which(spr_clean$WorkerID == "9"), ]
ls10 <- spr_clean[which(spr_clean$WorkerID == "10"), ]
ls11 <- spr_clean[which(spr_clean$WorkerID == "11"), ]
ls12 <- spr_clean[which(spr_clean$WorkerID == "12"), ]
ls13 <- spr_clean[which(spr_clean$WorkerID == "13"), ]
ls14 <- spr_clean[which(spr_clean$WorkerID == "14"), ]
ls15 <- spr_clean[which(spr_clean$WorkerID == "15"), ]
ls16 <- spr_clean[which(spr_clean$WorkerID == "16"), ]
ls17 <- spr_clean[which(spr_clean$WorkerID == "17"), ]
ls18 <- spr_clean[which(spr_clean$WorkerID == "18"), ]
ls19 <- spr_clean[which(spr_clean$WorkerID == "19"), ]
ls20 <- spr_clean[which(spr_clean$WorkerID == "20"), ]
ls21 <- spr_clean[which(spr_clean$WorkerID == "21"), ]
ls22 <- spr_clean[which(spr_clean$WorkerID == "22"), ]
ls23 <- spr_clean[which(spr_clean$WorkerID == "23"), ]
ls24 <- spr_clean[which(spr_clean$WorkerID == "24"), ]
ls25 <- spr_clean[which(spr_clean$WorkerID == "25"), ]
ls26 <- spr_clean[which(spr_clean$WorkerID == "26"), ]
ls27 <- spr_clean[which(spr_clean$WorkerID == "27"), ]
ls28 <- spr_clean[which(spr_clean$WorkerID == "28"), ]
ls29 <- spr_clean[which(spr_clean$WorkerID == "29"), ]
ls30 <- spr_clean[which(spr_clean$WorkerID == "30"), ]
ls31 <- spr_clean[which(spr_clean$WorkerID == "31"), ]
ls32 <- spr_clean[which(spr_clean$WorkerID == "32"), ]
ls33 <- spr_clean[which(spr_clean$WorkerID == "33"), ]
ls34 <- spr_clean[which(spr_clean$WorkerID == "34"), ]
ls35 <- spr_clean[which(spr_clean$WorkerID == "35"), ]
ls36 <- spr_clean[which(spr_clean$WorkerID == "36"), ]
ls37 <- spr_clean[which(spr_clean$WorkerID == "37"), ]
ls38 <- spr_clean[which(spr_clean$WorkerID == "38"), ]
ls39 <- spr_clean[which(spr_clean$WorkerID == "39"), ]
ls40 <- spr_clean[which(spr_clean$WorkerID == "40"), ]
}


###########################################################
####
#### Add coefficients for region length per participant
####
###########################################################

spr_clean["LBeta"] <- NA



attach(ls01)
lm <- lm(ReadingTime ~ Length)
detach(ls01)

spr_clean[which(spr_clean$WorkerID == "1"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls02)
lm <- lm(ReadingTime ~ Length)
detach(ls02)

spr_clean[which(spr_clean$WorkerID == "2"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls03)
lm <- lm(ReadingTime ~ Length)
detach(ls03)

spr_clean[which(spr_clean$WorkerID == "3"),]$LBeta <- summary(lm)$coefficients[2]

if (FALSE) {
attach(ls04)
lm <- lm(ReadingTime ~ Length)
detach(ls04)

spr_clean[which(spr_clean$WorkerID == "4"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls05)
lm <- lm(ReadingTime ~ Length)
detach(ls05)

spr_clean[which(spr_clean$WorkerID == "5"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls06)
lm <- lm(ReadingTime ~ Length)
detach(ls06)

spr_clean[which(spr_clean$WorkerID == "6"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls07)
lm <- lm(ReadingTime ~ Length)
detach(ls07)

spr_clean[which(spr_clean$WorkerID == "7"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls08)
lm <- lm(ReadingTime ~ Length)
detach(ls08)

spr_clean[which(spr_clean$WorkerID == "8"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls09)
lm <- lm(ReadingTime ~ Length)
detach(ls09)

spr_clean[which(spr_clean$WorkerID == "9"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls10)
lm <- lm(ReadingTime ~ Length)
detach(ls10)

spr_clean[which(spr_clean$WorkerID == "10"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls11)
lm <- lm(ReadingTime ~ Length)
detach(ls11)

spr_clean[which(spr_clean$WorkerID == "11"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls12)
lm <- lm(ReadingTime ~ Length)
detach(ls12)

spr_clean[which(spr_clean$WorkerID == "12"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls13)
lm <- lm(ReadingTime ~ Length)
detach(ls13)

spr_clean[which(spr_clean$WorkerID == "13"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls14)
lm <- lm(ReadingTime ~ Length)
detach(ls14)

spr_clean[which(spr_clean$WorkerID == "14"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls15)
lm <- lm(ReadingTime ~ Length)
detach(ls15)

spr_clean[which(spr_clean$WorkerID == "15"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls16)
lm <- lm(ReadingTime ~ Length)
detach(ls16)

spr_clean[which(spr_clean$WorkerID == "16"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls17)
lm <- lm(ReadingTime ~ Length)
detach(ls17)

spr_clean[which(spr_clean$WorkerID == "17"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls18)
lm <- lm(ReadingTime ~ Length)
detach(ls18)

spr_clean[which(spr_clean$WorkerID == "18"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls19)
lm <- lm(ReadingTime ~ Length)
detach(ls19)

spr_clean[which(spr_clean$WorkerID == "19"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls20)
lm <- lm(ReadingTime ~ Length)
detach(ls20)

spr_clean[which(spr_clean$WorkerID == "20"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls21)
lm <- lm(ReadingTime ~ Length)
detach(ls21)

spr_clean[which(spr_clean$WorkerID == "21"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls22)
lm <- lm(ReadingTime ~ Length)
detach(ls22)

spr_clean[which(spr_clean$WorkerID == "22"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls23)
lm <- lm(ReadingTime ~ Length)
detach(ls23)

spr_clean[which(spr_clean$WorkerID == "23"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls24)
lm <- lm(ReadingTime ~ Length)
detach(ls24)

spr_clean[which(spr_clean$WorkerID == "24"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls25)
lm <- lm(ReadingTime ~ Length)
detach(ls25)

spr_clean[which(spr_clean$WorkerID == "25"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls26)
lm <- lm(ReadingTime ~ Length)
detach(ls26)

spr_clean[which(spr_clean$WorkerID == "26"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls27)
lm <- lm(ReadingTime ~ Length)
detach(ls27)

spr_clean[which(spr_clean$WorkerID == "27"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls28)
lm <- lm(ReadingTime ~ Length)
detach(ls28)

spr_clean[which(spr_clean$WorkerID == "28"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls29)
lm <- lm(ReadingTime ~ Length)
detach(ls29)

spr_clean[which(spr_clean$WorkerID == "29"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls30)
lm <- lm(ReadingTime ~ Length)
detach(ls30)

spr_clean[which(spr_clean$WorkerID == "30"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls31)
lm <- lm(ReadingTime ~ Length)
detach(ls31)

spr_clean[which(spr_clean$WorkerID == "31"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls32)
lm <- lm(ReadingTime ~ Length)
detach(ls32)

spr_clean[which(spr_clean$WorkerID == "32"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls33)
lm <- lm(ReadingTime ~ Length)
detach(ls33)

spr_clean[which(spr_clean$WorkerID == "33"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls34)
lm <- lm(ReadingTime ~ Length)
detach(ls34)

spr_clean[which(spr_clean$WorkerID == "34"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls35)
lm <- lm(ReadingTime ~ Length)
detach(ls35)

spr_clean[which(spr_clean$WorkerID == "35"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls36)
lm <- lm(ReadingTime ~ Length)
detach(ls36)

spr_clean[which(spr_clean$WorkerID == "36"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls37)
lm <- lm(ReadingTime ~ Length)
detach(ls37)

spr_clean[which(spr_clean$WorkerID == "37"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls38)
lm <- lm(ReadingTime ~ Length)
detach(ls38)

spr_clean[which(spr_clean$WorkerID == "38"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls39)
lm <- lm(ReadingTime ~ Length)
detach(ls39)

spr_clean[which(spr_clean$WorkerID == "39"),]$LBeta <- summary(lm)$coefficients[2]

attach(ls40)
lm <- lm(ReadingTime ~ Length)
detach(ls40)

spr_clean[which(spr_clean$WorkerID == "40"),]$LBeta <- summary(lm)$coefficients[2]
}


###########################################################
####
####  Divide by regions
####
###########################################################


colnames(spr_clean)[colnames(spr_clean)=="WordNumber"] <- "Region"

reg1 <- spr_clean[which(spr_clean$Region == "1"), ]
reg2 <- spr_clean[which(spr_clean$Region == "2"), ]
reg3 <- spr_clean[which(spr_clean$Region == "3"), ]
reg4 <- spr_clean[which(spr_clean$Region == "4"), ]
reg5 <- spr_clean[which(spr_clean$Region == "5"), ]
reg6 <- spr_clean[which(spr_clean$Region == "6"), ]
reg7 <- spr_clean[which(spr_clean$Region == "7"), ]



###########################################################
###########################################################
####
####  Region  1
####
###########################################################
###########################################################



###########################################################
####
####  Raw descriptives
####
###########################################################

mean(reg1[which(reg1$Condition == "LL"), ]$ReadingTime)
sd(reg1[which(reg1$Condition == "LL"), ]$ReadingTime)

mean(reg1[which(reg1$Condition == "LH"), ]$ReadingTime)
sd(reg1[which(reg1$Condition == "LH"), ]$ReadingTime)

mean(reg1[which(reg1$Condition == "HL"), ]$ReadingTime)
sd(reg1[which(reg1$Condition == "HL"), ]$ReadingTime)

mean(reg1[which(reg1$Condition == "HH"), ]$ReadingTime)
sd(reg1[which(reg1$Condition == "HH"), ]$ReadingTime)



###########################################################
####
####  Length residual correction
####
###########################################################


reg1$ResRT <- reg1$ReadingTime - (reg1$Length * reg1$LBeta) ## per participant calculation



###########################################################
####
####  Residualized descriptives
####
###########################################################


mean.LL.reg1 <- mean(reg1[which(reg1$Condition == "LL"), ]$ResRT)
mean.LL.reg1
sd.LL.reg1 <- sd(reg1[which(reg1$Condition == "LL"), ]$ResRT)
sd.LL.reg1

mean.LH.reg1 <- mean(reg1[which(reg1$Condition == "LH"), ]$ResRT)
mean.LH.reg1
sd.LH.reg1 <- sd(reg1[which(reg1$Condition == "LH"), ]$ResRT)
sd.LH.reg1

mean.HL.reg1 <- mean(reg1[which(reg1$Condition == "HL"), ]$ResRT)
mean.HL.reg1
sd.HL.reg1 <- sd(reg1[which(reg1$Condition == "HL"), ]$ResRT)
sd.HL.reg1

mean.HH.reg1 <- mean(reg1[which(reg1$Condition == "HH"), ]$ResRT)
mean.HH.reg1
sd.HH.reg1 <- sd(reg1[which(reg1$Condition == "HH"), ]$ResRT)
sd.HH.reg1



###########################################################
####
####  95% Confidence Interval Calculation
####
###########################################################


###
### LL
###

length.LL.reg1 <- length(reg1[which(reg1$Condition == "LL"), ]$ResRT)
length.LL.reg1 

error.LL.reg1 <- qt(0.975,df=length.LL.reg1-1)*sd.LL.reg1/sqrt(length.LL.reg1)
error.LL.reg1 

left.LL.reg1 <- mean.LL.reg1 - error.LL.reg1
left.LL.reg1

right.LL.reg1 <- mean.LL.reg1 + error.LL.reg1
right.LL.reg1 


###
### LH
###

length.LH.reg1 <- length(reg1[which(reg1$Condition == "LH"), ]$ResRT)
length.LH.reg1 

error.LH.reg1 <- qt(0.975,df=length.LH.reg1-1)*sd.LH.reg1/sqrt(length.LH.reg1)
error.LH.reg1 

left.LH.reg1 <- mean.LH.reg1 - error.LH.reg1
left.LH.reg1

right.LH.reg1 <- mean.LH.reg1 + error.LH.reg1
right.LH.reg1 


###
### HL
###

length.HL.reg1 <- length(reg1[which(reg1$Condition == "HL"), ]$ResRT)
length.HL.reg1 

error.HL.reg1 <- qt(0.975,df=length.HL.reg1-1)*sd.HL.reg1/sqrt(length.HL.reg1)
error.HL.reg1 

left.HL.reg1 <- mean.HL.reg1 - error.HL.reg1
left.HL.reg1

right.HL.reg1 <- mean.HL.reg1 + error.HL.reg1
right.HL.reg1 


###
### HH
###

length.HH.reg1 <- length(reg1[which(reg1$Condition == "HH"), ]$ResRT)
length.HH.reg1 

error.HH.reg1 <- qt(0.975,df=length.HH.reg1-1)*sd.HH.reg1/sqrt(length.HH.reg1)
error.HH.reg1 

left.HH.reg1 <- mean.HH.reg1 - error.HH.reg1
left.HH.reg1

right.HH.reg1 <- mean.HH.reg1 + error.HH.reg1
right.HH.reg1 





###########################################################
####
####  LMER
####
###########################################################



attach(reg1)
lmer.reg1 <- lmer(ResRT ~ Condition + (1|WorkerID) + (1|Item) + (1|List), data=reg1)
summary(lmer.reg1)
detach(reg1)


#####
#####
##### End LMER
#####
#####




###########################################################
###########################################################
####
####  Region  2
####
###########################################################
###########################################################




###########################################################
####
####  Raw descriptives
####
###########################################################

mean(reg2[which(reg2$Condition == "LL"), ]$ReadingTime)
sd(reg2[which(reg2$Condition == "LL"), ]$ReadingTime)

mean(reg2[which(reg2$Condition == "LH"), ]$ReadingTime)
sd(reg2[which(reg2$Condition == "LH"), ]$ReadingTime)

mean(reg2[which(reg2$Condition == "HL"), ]$ReadingTime)
sd(reg2[which(reg2$Condition == "HL"), ]$ReadingTime)

mean(reg2[which(reg2$Condition == "HH"), ]$ReadingTime)
sd(reg2[which(reg2$Condition == "HH"), ]$ReadingTime)



###########################################################
####
####  Length residual correction
####
###########################################################


reg2$ResRT <- reg2$ReadingTime - (reg2$Length * reg2$LBeta) ## per participant calculation



###########################################################
####
####  Residualized descriptives
####
###########################################################


mean.LL.reg2 <- mean(reg2[which(reg2$Condition == "LL"), ]$ResRT)
mean.LL.reg2
sd.LL.reg2 <- sd(reg2[which(reg2$Condition == "LL"), ]$ResRT)
sd.LL.reg2

mean.LH.reg2 <- mean(reg2[which(reg2$Condition == "LH"), ]$ResRT)
mean.LH.reg2
sd.LH.reg2 <- sd(reg2[which(reg2$Condition == "LH"), ]$ResRT)
sd.LH.reg2

mean.HL.reg2 <- mean(reg2[which(reg2$Condition == "HL"), ]$ResRT)
mean.HL.reg2
sd.HL.reg2 <- sd(reg2[which(reg2$Condition == "HL"), ]$ResRT)
sd.HL.reg2

mean.HH.reg2 <- mean(reg2[which(reg2$Condition == "HH"), ]$ResRT)
mean.HH.reg2
sd.HH.reg2 <- sd(reg2[which(reg2$Condition == "HH"), ]$ResRT)
sd.HH.reg2



###########################################################
####
####  95% Confidence Interval Calculation
####
###########################################################


###
### LL
###

length.LL.reg2 <- length(reg2[which(reg2$Condition == "LL"), ]$ResRT)
length.LL.reg2 

error.LL.reg2 <- qt(0.975,df=length.LL.reg2-1)*sd.LL.reg2/sqrt(length.LL.reg2)
error.LL.reg2 

left.LL.reg2 <- mean.LL.reg2 - error.LL.reg2
left.LL.reg2

right.LL.reg2 <- mean.LL.reg2 + error.LL.reg2
right.LL.reg2 


###
### LH
###

length.LH.reg2 <- length(reg2[which(reg2$Condition == "LH"), ]$ResRT)
length.LH.reg2 

error.LH.reg2 <- qt(0.975,df=length.LH.reg2-1)*sd.LH.reg2/sqrt(length.LH.reg2)
error.LH.reg2 

left.LH.reg2 <- mean.LH.reg2 - error.LH.reg2
left.LH.reg2

right.LH.reg2 <- mean.LH.reg2 + error.LH.reg2
right.LH.reg2 


###
### HL
###

length.HL.reg2 <- length(reg2[which(reg2$Condition == "HL"), ]$ResRT)
length.HL.reg2 

error.HL.reg2 <- qt(0.975,df=length.HL.reg2-1)*sd.HL.reg2/sqrt(length.HL.reg2)
error.HL.reg2 

left.HL.reg2 <- mean.HL.reg2 - error.HL.reg2
left.HL.reg2

right.HL.reg2 <- mean.HL.reg2 + error.HL.reg2
right.HL.reg2 


###
### HH
###

length.HH.reg2 <- length(reg2[which(reg2$Condition == "HH"), ]$ResRT)
length.HH.reg2 

error.HH.reg2 <- qt(0.975,df=length.HH.reg2-1)*sd.HH.reg2/sqrt(length.HH.reg2)
error.HH.reg2 

left.HH.reg2 <- mean.HH.reg2 - error.HH.reg2
left.HH.reg2

right.HH.reg2 <- mean.HH.reg2 + error.HH.reg2
right.HH.reg2 





###########################################################
####
####  LMER
####
###########################################################



attach(reg2)
lmer.reg2 <- lmer(ResRT ~ Condition + (1|WorkerID) + (1|Item) + (1|List), data=reg2)
summary(lmer.reg2)
detach(reg2)


#####
#####
##### End LMER
#####
#####


###########################################################
###########################################################
####
####  Region  3
####
###########################################################
###########################################################



###########################################################
####
####  Raw descriptives
####
###########################################################

mean(reg3[which(reg3$Condition == "LL"), ]$ReadingTime)
sd(reg3[which(reg3$Condition == "LL"), ]$ReadingTime)

mean(reg3[which(reg3$Condition == "LH"), ]$ReadingTime)
sd(reg3[which(reg3$Condition == "LH"), ]$ReadingTime)

mean(reg3[which(reg3$Condition == "HL"), ]$ReadingTime)
sd(reg3[which(reg3$Condition == "HL"), ]$ReadingTime)

mean(reg3[which(reg3$Condition == "HH"), ]$ReadingTime)
sd(reg3[which(reg3$Condition == "HH"), ]$ReadingTime)



###########################################################
####
####  Length residual correction
####
###########################################################


reg3$ResRT <- reg3$ReadingTime - (reg3$Length * reg3$LBeta) ## per participant calculation



###########################################################
####
####  Residualized descriptives
####
###########################################################


mean.LL.reg3 <- mean(reg3[which(reg3$Condition == "LL"), ]$ResRT)
mean.LL.reg3
sd.LL.reg3 <- sd(reg3[which(reg3$Condition == "LL"), ]$ResRT)
sd.LL.reg3

mean.LH.reg3 <- mean(reg3[which(reg3$Condition == "LH"), ]$ResRT)
mean.LH.reg3
sd.LH.reg3 <- sd(reg3[which(reg3$Condition == "LH"), ]$ResRT)
sd.LH.reg3

mean.HL.reg3 <- mean(reg3[which(reg3$Condition == "HL"), ]$ResRT)
mean.HL.reg3
sd.HL.reg3 <- sd(reg3[which(reg3$Condition == "HL"), ]$ResRT)
sd.HL.reg3

mean.HH.reg3 <- mean(reg3[which(reg3$Condition == "HH"), ]$ResRT)
mean.HH.reg3
sd.HH.reg3 <- sd(reg3[which(reg3$Condition == "HH"), ]$ResRT)
sd.HH.reg3



###########################################################
####
####  95% Confidence Interval Calculation
####
###########################################################


###
### LL
###

length.LL.reg3 <- length(reg3[which(reg3$Condition == "LL"), ]$ResRT)
length.LL.reg3 

error.LL.reg3 <- qt(0.975,df=length.LL.reg3-1)*sd.LL.reg3/sqrt(length.LL.reg3)
error.LL.reg3 

left.LL.reg3 <- mean.LL.reg3 - error.LL.reg3
left.LL.reg3

right.LL.reg3 <- mean.LL.reg3 + error.LL.reg3
right.LL.reg3 


###
### LH
###

length.LH.reg3 <- length(reg3[which(reg3$Condition == "LH"), ]$ResRT)
length.LH.reg3 

error.LH.reg3 <- qt(0.975,df=length.LH.reg3-1)*sd.LH.reg3/sqrt(length.LH.reg3)
error.LH.reg3 

left.LH.reg3 <- mean.LH.reg3 - error.LH.reg3
left.LH.reg3

right.LH.reg3 <- mean.LH.reg3 + error.LH.reg3
right.LH.reg3 


###
### HL
###

length.HL.reg3 <- length(reg3[which(reg3$Condition == "HL"), ]$ResRT)
length.HL.reg3 

error.HL.reg3 <- qt(0.975,df=length.HL.reg3-1)*sd.HL.reg3/sqrt(length.HL.reg3)
error.HL.reg3 

left.HL.reg3 <- mean.HL.reg3 - error.HL.reg3
left.HL.reg3

right.HL.reg3 <- mean.HL.reg3 + error.HL.reg3
right.HL.reg3 


###
### HH
###

length.HH.reg3 <- length(reg3[which(reg3$Condition == "HH"), ]$ResRT)
length.HH.reg3 

error.HH.reg3 <- qt(0.975,df=length.HH.reg3-1)*sd.HH.reg3/sqrt(length.HH.reg3)
error.HH.reg3 

left.HH.reg3 <- mean.HH.reg3 - error.HH.reg3
left.HH.reg3

right.HH.reg3 <- mean.HH.reg3 + error.HH.reg3
right.HH.reg3 





###########################################################
####
####  LMER
####
###########################################################



attach(reg3)
lmer.reg3 <- lmer(ResRT ~ Condition + (1|WorkerID) + (1|Item) + (1|List), data=reg3)
summary(lmer.reg3)
detach(reg3)


#####
#####
##### End LMER
#####
#####



###########################################################
###########################################################
####
####  Region  4
####
###########################################################
###########################################################



###########################################################
####
####  Raw descriptives
####
###########################################################

mean(reg4[which(reg4$Condition == "LL"), ]$ReadingTime)
sd(reg4[which(reg4$Condition == "LL"), ]$ReadingTime)

mean(reg4[which(reg4$Condition == "LH"), ]$ReadingTime)
sd(reg4[which(reg4$Condition == "LH"), ]$ReadingTime)

mean(reg4[which(reg4$Condition == "HL"), ]$ReadingTime)
sd(reg4[which(reg4$Condition == "HL"), ]$ReadingTime)

mean(reg4[which(reg4$Condition == "HH"), ]$ReadingTime)
sd(reg4[which(reg4$Condition == "HH"), ]$ReadingTime)



###########################################################
####
####  Length residual correction
####
###########################################################


reg4$ResRT <- reg4$ReadingTime - (reg4$Length * reg4$LBeta) ## per participant calculation



###########################################################
####
####  Residualized descriptives
####
###########################################################


mean.LL.reg4 <- mean(reg4[which(reg4$Condition == "LL"), ]$ResRT)
mean.LL.reg4
sd.LL.reg4 <- sd(reg4[which(reg4$Condition == "LL"), ]$ResRT)
sd.LL.reg4

mean.LH.reg4 <- mean(reg4[which(reg4$Condition == "LH"), ]$ResRT)
mean.LH.reg4
sd.LH.reg4 <- sd(reg4[which(reg4$Condition == "LH"), ]$ResRT)
sd.LH.reg4

mean.HL.reg4 <- mean(reg4[which(reg4$Condition == "HL"), ]$ResRT)
mean.HL.reg4
sd.HL.reg4 <- sd(reg4[which(reg4$Condition == "HL"), ]$ResRT)
sd.HL.reg4

mean.HH.reg4 <- mean(reg4[which(reg4$Condition == "HH"), ]$ResRT)
mean.HH.reg4
sd.HH.reg4 <- sd(reg4[which(reg4$Condition == "HH"), ]$ResRT)
sd.HH.reg4



###########################################################
####
####  95% Confidence Interval Calculation
####
###########################################################


###
### LL
###

length.LL.reg4 <- length(reg4[which(reg4$Condition == "LL"), ]$ResRT)
length.LL.reg4 

error.LL.reg4 <- qt(0.975,df=length.LL.reg4-1)*sd.LL.reg4/sqrt(length.LL.reg4)
error.LL.reg4 

left.LL.reg4 <- mean.LL.reg4 - error.LL.reg4
left.LL.reg4

right.LL.reg4 <- mean.LL.reg4 + error.LL.reg4
right.LL.reg4 


###
### LH
###

length.LH.reg4 <- length(reg4[which(reg4$Condition == "LH"), ]$ResRT)
length.LH.reg4 

error.LH.reg4 <- qt(0.975,df=length.LH.reg4-1)*sd.LH.reg4/sqrt(length.LH.reg4)
error.LH.reg4 

left.LH.reg4 <- mean.LH.reg4 - error.LH.reg4
left.LH.reg4

right.LH.reg4 <- mean.LH.reg4 + error.LH.reg4
right.LH.reg4 


###
### HL
###

length.HL.reg4 <- length(reg4[which(reg4$Condition == "HL"), ]$ResRT)
length.HL.reg4 

error.HL.reg4 <- qt(0.975,df=length.HL.reg4-1)*sd.HL.reg4/sqrt(length.HL.reg4)
error.HL.reg4 

left.HL.reg4 <- mean.HL.reg4 - error.HL.reg4
left.HL.reg4

right.HL.reg4 <- mean.HL.reg4 + error.HL.reg4
right.HL.reg4 


###
### HH
###

length.HH.reg4 <- length(reg4[which(reg4$Condition == "HH"), ]$ResRT)
length.HH.reg4 

error.HH.reg4 <- qt(0.975,df=length.HH.reg4-1)*sd.HH.reg4/sqrt(length.HH.reg4)
error.HH.reg4 

left.HH.reg4 <- mean.HH.reg4 - error.HH.reg4
left.HH.reg4

right.HH.reg4 <- mean.HH.reg4 + error.HH.reg4
right.HH.reg4 





###########################################################
####
####  LMER
####
###########################################################



attach(reg4)
lmer.reg4 <- lmer(ResRT ~ Condition + (1|WorkerID) + (1|Item) + (1|List), data=reg4)
summary(lmer.reg4)
detach(reg4)


#####
#####
##### End LMER
#####
#####



###########################################################
###########################################################
####
####  Region  5
####
###########################################################
###########################################################



###########################################################
####
####  Raw descriptives
####
###########################################################

mean(reg5[which(reg5$Condition == "LL"), ]$ReadingTime)
sd(reg5[which(reg5$Condition == "LL"), ]$ReadingTime)

mean(reg5[which(reg5$Condition == "LH"), ]$ReadingTime)
sd(reg5[which(reg5$Condition == "LH"), ]$ReadingTime)

mean(reg5[which(reg5$Condition == "HL"), ]$ReadingTime)
sd(reg5[which(reg5$Condition == "HL"), ]$ReadingTime)

mean(reg5[which(reg5$Condition == "HH"), ]$ReadingTime)
sd(reg5[which(reg5$Condition == "HH"), ]$ReadingTime)



###########################################################
####
####  Length residual correction
####
###########################################################


reg5$ResRT <- reg5$ReadingTime - (reg5$Length * reg5$LBeta) ## per participant calculation



###########################################################
####
####  Residualized descriptives
####
###########################################################


mean.LL.reg5 <- mean(reg5[which(reg5$Condition == "LL"), ]$ResRT)
mean.LL.reg5
sd.LL.reg5 <- sd(reg5[which(reg5$Condition == "LL"), ]$ResRT)
sd.LL.reg5

mean.LH.reg5 <- mean(reg5[which(reg5$Condition == "LH"), ]$ResRT)
mean.LH.reg5
sd.LH.reg5 <- sd(reg5[which(reg5$Condition == "LH"), ]$ResRT)
sd.LH.reg5

mean.HL.reg5 <- mean(reg5[which(reg5$Condition == "HL"), ]$ResRT)
mean.HL.reg5
sd.HL.reg5 <- sd(reg5[which(reg5$Condition == "HL"), ]$ResRT)
sd.HL.reg5

mean.HH.reg5 <- mean(reg5[which(reg5$Condition == "HH"), ]$ResRT)
mean.HH.reg5
sd.HH.reg5 <- sd(reg5[which(reg5$Condition == "HH"), ]$ResRT)
sd.HH.reg5



###########################################################
####
####  95% Confidence Interval Calculation
####
###########################################################


###
### LL
###

length.LL.reg5 <- length(reg5[which(reg5$Condition == "LL"), ]$ResRT)
length.LL.reg5 

error.LL.reg5 <- qt(0.975,df=length.LL.reg5-1)*sd.LL.reg5/sqrt(length.LL.reg5)
error.LL.reg5 

left.LL.reg5 <- mean.LL.reg5 - error.LL.reg5
left.LL.reg5

right.LL.reg5 <- mean.LL.reg5 + error.LL.reg5
right.LL.reg5 


###
### LH
###

length.LH.reg5 <- length(reg5[which(reg5$Condition == "LH"), ]$ResRT)
length.LH.reg5 

error.LH.reg5 <- qt(0.975,df=length.LH.reg5-1)*sd.LH.reg5/sqrt(length.LH.reg5)
error.LH.reg5 

left.LH.reg5 <- mean.LH.reg5 - error.LH.reg5
left.LH.reg5

right.LH.reg5 <- mean.LH.reg5 + error.LH.reg5
right.LH.reg5 


###
### HL
###

length.HL.reg5 <- length(reg5[which(reg5$Condition == "HL"), ]$ResRT)
length.HL.reg5 

error.HL.reg5 <- qt(0.975,df=length.HL.reg5-1)*sd.HL.reg5/sqrt(length.HL.reg5)
error.HL.reg5 

left.HL.reg5 <- mean.HL.reg5 - error.HL.reg5
left.HL.reg5

right.HL.reg5 <- mean.HL.reg5 + error.HL.reg5
right.HL.reg5 


###
### HH
###

length.HH.reg5 <- length(reg5[which(reg5$Condition == "HH"), ]$ResRT)
length.HH.reg5 

error.HH.reg5 <- qt(0.975,df=length.HH.reg5-1)*sd.HH.reg5/sqrt(length.HH.reg5)
error.HH.reg5 

left.HH.reg5 <- mean.HH.reg5 - error.HH.reg5
left.HH.reg5

right.HH.reg5 <- mean.HH.reg5 + error.HH.reg5
right.HH.reg5 





###########################################################
####
####  LMER
####
###########################################################



attach(reg5)
lmer.reg5 <- lmer(ResRT ~ Condition + (1|WorkerID) + (1|Item) + (1|List), data=reg5)
summary(lmer.reg5)
detach(reg5)


#####
#####
##### End LMER
#####
#####



###########################################################
###########################################################
####
####  Region  6
####
###########################################################
###########################################################



###########################################################
####
####  Raw descriptives
####
###########################################################

mean(reg6[which(reg6$Condition == "LL"), ]$ReadingTime)
sd(reg6[which(reg6$Condition == "LL"), ]$ReadingTime)

mean(reg6[which(reg6$Condition == "LH"), ]$ReadingTime)
sd(reg6[which(reg6$Condition == "LH"), ]$ReadingTime)

mean(reg6[which(reg6$Condition == "HL"), ]$ReadingTime)
sd(reg6[which(reg6$Condition == "HL"), ]$ReadingTime)

mean(reg6[which(reg6$Condition == "HH"), ]$ReadingTime)
sd(reg6[which(reg6$Condition == "HH"), ]$ReadingTime)



###########################################################
####
####  Length residual correction
####
###########################################################


reg6$ResRT <- reg6$ReadingTime - (reg6$Length * reg6$LBeta) ## per participant calculation



###########################################################
####
####  Residualized descriptives
####
###########################################################


mean.LL.reg6 <- mean(reg6[which(reg6$Condition == "LL"), ]$ResRT)
mean.LL.reg6
sd.LL.reg6 <- sd(reg6[which(reg6$Condition == "LL"), ]$ResRT)
sd.LL.reg6

mean.LH.reg6 <- mean(reg6[which(reg6$Condition == "LH"), ]$ResRT)
mean.LH.reg6
sd.LH.reg6 <- sd(reg6[which(reg6$Condition == "LH"), ]$ResRT)
sd.LH.reg6

mean.HL.reg6 <- mean(reg6[which(reg6$Condition == "HL"), ]$ResRT)
mean.HL.reg6
sd.HL.reg6 <- sd(reg6[which(reg6$Condition == "HL"), ]$ResRT)
sd.HL.reg6

mean.HH.reg6 <- mean(reg6[which(reg6$Condition == "HH"), ]$ResRT)
mean.HH.reg6
sd.HH.reg6 <- sd(reg6[which(reg6$Condition == "HH"), ]$ResRT)
sd.HH.reg6



###########################################################
####
####  95% Confidence Interval Calculation
####
###########################################################


###
### LL
###

length.LL.reg6 <- length(reg6[which(reg6$Condition == "LL"), ]$ResRT)
length.LL.reg6 

error.LL.reg6 <- qt(0.975,df=length.LL.reg6-1)*sd.LL.reg6/sqrt(length.LL.reg6)
error.LL.reg6 

left.LL.reg6 <- mean.LL.reg6 - error.LL.reg6
left.LL.reg6

right.LL.reg6 <- mean.LL.reg6 + error.LL.reg6
right.LL.reg6 


###
### LH
###

length.LH.reg6 <- length(reg6[which(reg6$Condition == "LH"), ]$ResRT)
length.LH.reg6 

error.LH.reg6 <- qt(0.975,df=length.LH.reg6-1)*sd.LH.reg6/sqrt(length.LH.reg6)
error.LH.reg6 

left.LH.reg6 <- mean.LH.reg6 - error.LH.reg6
left.LH.reg6

right.LH.reg6 <- mean.LH.reg6 + error.LH.reg6
right.LH.reg6 


###
### HL
###

length.HL.reg6 <- length(reg6[which(reg6$Condition == "HL"), ]$ResRT)
length.HL.reg6 

error.HL.reg6 <- qt(0.975,df=length.HL.reg6-1)*sd.HL.reg6/sqrt(length.HL.reg6)
error.HL.reg6 

left.HL.reg6 <- mean.HL.reg6 - error.HL.reg6
left.HL.reg6

right.HL.reg6 <- mean.HL.reg6 + error.HL.reg6
right.HL.reg6 


###
### HH
###

length.HH.reg6 <- length(reg6[which(reg6$Condition == "HH"), ]$ResRT)
length.HH.reg6 

error.HH.reg6 <- qt(0.975,df=length.HH.reg6-1)*sd.HH.reg6/sqrt(length.HH.reg6)
error.HH.reg6 

left.HH.reg6 <- mean.HH.reg6 - error.HH.reg6
left.HH.reg6

right.HH.reg6 <- mean.HH.reg6 + error.HH.reg6
right.HH.reg6 





###########################################################
####
####  LMER
####
###########################################################



attach(reg6)
lmer.reg6 <- lmer(ResRT ~ Condition + (1|WorkerID) + (1|Item) + (1|List), data=reg6)
summary(lmer.reg6)
detach(reg6)


#####
#####
##### End LMER
#####
#####



###########################################################
###########################################################
####
####  Region  7
####
###########################################################
###########################################################



###########################################################
####
####  Raw descriptives
####
###########################################################

mean(reg7[which(reg7$Condition == "LL"), ]$ReadingTime)
sd(reg7[which(reg7$Condition == "LL"), ]$ReadingTime)

mean(reg7[which(reg7$Condition == "LH"), ]$ReadingTime)
sd(reg7[which(reg7$Condition == "LH"), ]$ReadingTime)

mean(reg7[which(reg7$Condition == "HL"), ]$ReadingTime)
sd(reg7[which(reg7$Condition == "HL"), ]$ReadingTime)

mean(reg7[which(reg7$Condition == "HH"), ]$ReadingTime)
sd(reg7[which(reg7$Condition == "HH"), ]$ReadingTime)



###########################################################
####
####  Length residual correction
####
###########################################################


reg7$ResRT <- reg7$ReadingTime - (reg7$Length * reg7$LBeta) ## per participant calculation



###########################################################
####
####  Residualized descriptives
####
###########################################################


mean.LL.reg7 <- mean(reg7[which(reg7$Condition == "LL"), ]$ResRT)
mean.LL.reg7
sd.LL.reg7 <- sd(reg7[which(reg7$Condition == "LL"), ]$ResRT)
sd.LL.reg7

mean.LH.reg7 <- mean(reg7[which(reg7$Condition == "LH"), ]$ResRT)
mean.LH.reg7
sd.LH.reg7 <- sd(reg7[which(reg7$Condition == "LH"), ]$ResRT)
sd.LH.reg7

mean.HL.reg7 <- mean(reg7[which(reg7$Condition == "HL"), ]$ResRT)
mean.HL.reg7
sd.HL.reg7 <- sd(reg7[which(reg7$Condition == "HL"), ]$ResRT)
sd.HL.reg7

mean.HH.reg7 <- mean(reg7[which(reg7$Condition == "HH"), ]$ResRT)
mean.HH.reg7
sd.HH.reg7 <- sd(reg7[which(reg7$Condition == "HH"), ]$ResRT)
sd.HH.reg7



###########################################################
####
####  95% Confidence Interval Calculation
####
###########################################################


###
### LL
###

length.LL.reg7 <- length(reg7[which(reg7$Condition == "LL"), ]$ResRT)
length.LL.reg7 

error.LL.reg7 <- qt(0.975,df=length.LL.reg7-1)*sd.LL.reg7/sqrt(length.LL.reg7)
error.LL.reg7 

left.LL.reg7 <- mean.LL.reg7 - error.LL.reg7
left.LL.reg7

right.LL.reg7 <- mean.LL.reg7 + error.LL.reg7
right.LL.reg7 


###
### LH
###

length.LH.reg7 <- length(reg7[which(reg7$Condition == "LH"), ]$ResRT)
length.LH.reg7 

error.LH.reg7 <- qt(0.975,df=length.LH.reg7-1)*sd.LH.reg7/sqrt(length.LH.reg7)
error.LH.reg7 

left.LH.reg7 <- mean.LH.reg7 - error.LH.reg7
left.LH.reg7

right.LH.reg7 <- mean.LH.reg7 + error.LH.reg7
right.LH.reg7 


###
### HL
###

length.HL.reg7 <- length(reg7[which(reg7$Condition == "HL"), ]$ResRT)
length.HL.reg7 

error.HL.reg7 <- qt(0.975,df=length.HL.reg7-1)*sd.HL.reg7/sqrt(length.HL.reg7)
error.HL.reg7 

left.HL.reg7 <- mean.HL.reg7 - error.HL.reg7
left.HL.reg7

right.HL.reg7 <- mean.HL.reg7 + error.HL.reg7
right.HL.reg7 


###
### HH
###

length.HH.reg7 <- length(reg7[which(reg7$Condition == "HH"), ]$ResRT)
length.HH.reg7 

error.HH.reg7 <- qt(0.975,df=length.HH.reg7-1)*sd.HH.reg7/sqrt(length.HH.reg7)
error.HH.reg7 

left.HH.reg7 <- mean.HH.reg7 - error.HH.reg7
left.HH.reg7

right.HH.reg7 <- mean.HH.reg7 + error.HH.reg7
right.HH.reg7 





###########################################################
####
####  LMER
####
###########################################################



attach(reg7)
lmer.reg7 <- lmer(ResRT ~ Condition + (1|WorkerID) + (1|Item) + (1|List), data=reg7)
summary(lmer.reg7)
detach(reg7)


#####
#####
##### End LMER
#####
#####






sink("graph.tex")
cat("\\documentclass[11pt]{article}
\\usepackage{setspace}
\\usepackage{fullpage}
\\usepackage{pslatex}
\\usepackage{pgfplots}
\\pgfplotsset{
	compat=1.4}
\\usepackage{tikz}
\\usetikzlibrary{shapes,arrows}
\\let\\word=\\textit
\\begin{document}
\\begin{figure}[htb]
\\centering
\\begin{tikzpicture}
\\begin{axis}[
	width=15cm, height=12cm,
	legend style={at={(0.85,1.02)},
	anchor=north,legend columns=-1},
	ylabel={Residual reading time (msec)},
	xlabel={Region},
	symbolic x coords={1, 2, 3, 4, 5, 6, 7},
    	xtick=data,
]
\\addplot[
		mark=square,
		error bars/.cd,
		error bar style={color=black},
		y dir=both,
		y explicit
] 
	coordinates {
 			   (1,")
   cat(mean.LL.reg1)
   cat(") +- (")
   cat(left.LL.reg1)
   cat(",")
   cat(right.LL.reg1)
   cat(")
 			   (2,439) +- (21,21)
 			   (3,360) +- (22,22)
 			   (4,431) +- (25,25)
 			   (5,398) +- (21,21)
 			   (6,378) +- (24,24)
 			   (7,411) +- (23,23)
};
\\addplot[
		mark=square,
		dashed,
		error bars/.cd,
		error bar style={color=black},
		y dir=both,
		y explicit
] 
	coordinates {
 			   (1,442) +- (25,25)
 			   (2,443) +- (20,20)
 			   (3,378) +- (23,23)
 			   (4,450) +- (26,26)
 			   (5,412) +- (23,23)
 			   (6,390) +- (24,24)
 			   (7,437) +- (26,26)
};
\\legend{LL,LH,HL,HH}
\\end{axis}
\\end{tikzpicture}
\\caption{Mean residual reading times for all sentence regions}
\\end{figure}
\\end{document}")
sink()

