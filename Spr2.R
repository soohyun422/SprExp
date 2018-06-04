rm(list=ls())
library(languageR)
library(lme4)
library(lmerTest)
library(psych)
library(stringr)


####
#### Read dataset
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
####  Remove bad data
####
###########################################################


spr_raw<- subset(spr_raw,substr(spr_raw$Type,10,15)!="List1") 
#spr_raw<- subset(spr_raw,substr(spr_raw$Type,0,15)!="block2-HL-List1")


###########################################################
####
####  Toss out bad participants below 75% cut-off for comprehension
####
###########################################################



spr_raw$Type <- Null
spr_raw <- spr_raw[which(spr_raw$WorkerID  != "A3963FCT1PKKOH" &
                           spr_raw$WorkerID != "A1PUHCEBSOWETV" &
                           spr_raw$WorkerID != "A3E9A9O3CLMEW" ), ]


spr_raw$WorkerID <- factor(spr_raw$WorkerID) 
summary(spr_raw$WorkerID)


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
####  Add PatientHood
####
###########################################################

spr_clean["Patienthood"] <- NA
spr_clean[which(spr_clean$Condition=="HH"|spr_clean$Condition=="HL"),]$Patienthood <- 'high'
spr_clean[which(spr_clean$Condition=="LH"|spr_clean$Condition=="LL"),]$Patienthood <- 'low'


###########################################################
####
####  Add Bigram Frequency
####
###########################################################

spr_clean["BigramF"] <- NA
spr_clean[which(spr_clean$Condition=="HH"|spr_clean$Condition=="LH"),]$BigramF <- 'high'
spr_clean[which(spr_clean$Condition=="HL"|spr_clean$Condition=="LL"),]$BigramF <- 'low'




###########################################################
####
#### LMER for Length Residualization
####
###########################################################

spr_clean$WorkerID <- factor(as.numeric(spr_clean$WorkerID))

ls01 <- spr_clean[which(spr_clean$WorkerID == "1"), ]
ls02 <- spr_clean[which(spr_clean$WorkerID == "2"), ]
ls03 <- spr_clean[which(spr_clean$WorkerID == "3"), ]
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
ls41 <- spr_clean[which(spr_clean$WorkerID == "41"), ]


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

attach(ls41)
lm <- lm(ReadingTime ~ Length)
detach(ls41)

spr_clean[which(spr_clean$WorkerID == "41"),]$LBeta <- summary(lm)$coefficients[2]


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

mean(reg1[which(reg1$Patienthood == "low"), ]$ReadingTime)
sd(reg1[which(reg1$Patienthood == "low"), ]$ReadingTime)

mean(reg1[which(reg1$Patienthood == "high"), ]$ReadingTime)
sd(reg1[which(reg1$Patienthood == "high"), ]$ReadingTime)

mean(reg1[which(reg1$BigramF == "low"), ]$ReadingTime)
sd(reg1[which(reg1$BigramF == "low"), ]$ReadingTime)

mean(reg1[which(reg1$BigramF == "high"), ]$ReadingTime)
sd(reg1[which(reg1$BigramF == "high"), ]$ReadingTime)



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


mean.PHigh.reg1 <- mean(reg1[which(reg1$Patienthood == "high"), ]$ResRT)
mean.PHigh.reg1
sd.PHigh.reg1 <- sd(reg1[which(reg1$Patienthood == "high"), ]$ResRT)
sd.PHigh.reg1

mean.PLow.reg1 <- mean(reg1[which(reg1$Patienthood== "low"), ]$ResRT)
mean.PLow.reg1
sd.PLow.reg1 <- sd(reg1[which(reg1$Patienthood== "low"), ]$ResRT)
sd.PLow.reg1

mean.FHigh.reg1 <- mean(reg1[which(reg1$BigramF == "high"), ]$ResRT)
mean.FHigh.reg1
sd.FHigh.reg1 <- sd(reg1[which(reg1$BigramF == "high"), ]$ResRT)
sd.FHigh.reg1

mean.FLow.reg1 <- mean(reg1[which(reg1$BigramF== "low"), ]$ResRT)
mean.FLow.reg1
sd.FLow.reg1 <- sd(reg1[which(reg1$BigramF == "low"), ]$ResRT)
sd.FLow.reg1



###########################################################
####
####  95% Confidence Interval Calculation
####
###########################################################


###
### Patienthood High
###

length.PHigh.reg1 <- length(reg1[which(reg1$Patienthood == "high"), ]$ResRT)
length.PHigh.reg1 

error.PHigh.reg1 <- qt(0.975,df=length.PHigh.reg1-1)*sd.PHigh.reg1/sqrt(length.PHigh.reg1)
error.PHigh.reg1 

left.PHigh.reg1 <- mean.PHigh.reg1 - error.PHigh.reg1
left.PHigh.reg1

right.PHigh.reg1 <- mean.PHigh.reg1 + error.PHigh.reg1
right.PHigh.reg1 


###
### Patienthood Low
###

length.PLow.reg1 <- length(reg1[which(reg1$Patienthood == "low"), ]$ResRT)
length.PLow.reg1 

error.PLow.reg1 <- qt(0.975,df=length.PLow.reg1-1)*sd.PLow.reg1/sqrt(length.PLow.reg1)
error.PLow.reg1 

left.PLow.reg1 <- mean.PLow.reg1 - error.PLow.reg1
left.PLow.reg1

right.PLow.reg1 <- mean.PLow.reg1 + error.PLow.reg1
right.PLow.reg1 


###
### Bigram Frequency High
###

length.FHigh.reg1 <- length(reg1[which(reg1$BigramF == "high"), ]$ResRT)
length.FHigh.reg1 

error.FHigh.reg1 <- qt(0.975,df=length.FHigh.reg1-1)*sd.FHigh.reg1/sqrt(length.FHigh.reg1)
error.FHigh.reg1 

left.FHigh.reg1 <- mean.FHigh.reg1 - error.FHigh.reg1
left.FHigh.reg1

right.FHigh.reg1 <- mean.FHigh.reg1 + error.FHigh.reg1
right.FHigh.reg1 


###
### Bigram Frequency Low
###

length.FLow.reg1 <- length(reg1[which(reg1$BigramF == "low"), ]$ResRT)
length.FLow.reg1 

error.FLow.reg1 <- qt(0.975,df=length.FLow.reg1-1)*sd.FLow.reg1/sqrt(length.FLow.reg1)
error.FLow.reg1 

left.FLow.reg1 <- mean.FLow.reg1 - error.FLow.reg1
left.FLow.reg1

right.FLow.reg1 <- mean.FLow.reg1 + error.FLow.reg1
right.FLow.reg1 





###########################################################
####
####  LMER
####
###########################################################



attach(reg1)
lmer.reg1.Patienthood <- lmer(ResRT ~ Patienthood + (1|WorkerID) + (1|Item) + (1|List), data=reg1)
summary(lmer.reg1.Patienthood)
lmer.reg1.Frequency <- lmer(ResRT ~ BigramF + (1|WorkerID) + (1|Item) + (1|List), data=reg1)
summary(lmer.reg1.Frequency)
lmer.reg1.Interaction <- lmer(ResRT ~ Patienthood*BigramF + (1|WorkerID) + (1|Item) + (1|List), data=reg1)
summary(lmer.reg1.Interaction)
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

mean(reg2[which(reg2$Patienthood == "low"), ]$ReadingTime)
sd(reg2[which(reg2$Patienthood == "low"), ]$ReadingTime)

mean(reg2[which(reg2$Patienthood == "high"), ]$ReadingTime)
sd(reg2[which(reg2$Patienthood == "high"), ]$ReadingTime)

mean(reg2[which(reg2$BigramF == "low"), ]$ReadingTime)
sd(reg2[which(reg2$BigramF == "low"), ]$ReadingTime)

mean(reg2[which(reg2$BigramF == "high"), ]$ReadingTime)
sd(reg2[which(reg2$BigramF == "high"), ]$ReadingTime)



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


mean.PHigh.reg2 <- mean(reg2[which(reg2$Patienthood == "high"), ]$ResRT)
mean.PHigh.reg2
sd.PHigh.reg2 <- sd(reg2[which(reg2$Patienthood == "high"), ]$ResRT)
sd.PHigh.reg2

mean.PLow.reg2 <- mean(reg2[which(reg2$Patienthood== "low"), ]$ResRT)
mean.PLow.reg2
sd.PLow.reg2 <- sd(reg2[which(reg2$Patienthood== "low"), ]$ResRT)
sd.PLow.reg2

mean.FHigh.reg2 <- mean(reg2[which(reg2$BigramF == "high"), ]$ResRT)
mean.FHigh.reg2
sd.FHigh.reg2 <- sd(reg2[which(reg2$BigramF == "high"), ]$ResRT)
sd.FHigh.reg2

mean.FLow.reg2 <- mean(reg2[which(reg2$BigramF== "low"), ]$ResRT)
mean.FLow.reg2
sd.FLow.reg2 <- sd(reg2[which(reg2$BigramF == "low"), ]$ResRT)
sd.FLow.reg2



###########################################################
####
####  95% Confidence Interval Calculation
####
###########################################################


###
### Patienthood High
###

length.PHigh.reg2 <- length(reg2[which(reg2$Patienthood == "high"), ]$ResRT)
length.PHigh.reg2 

error.PHigh.reg2 <- qt(0.975,df=length.PHigh.reg2-1)*sd.PHigh.reg2/sqrt(length.PHigh.reg2)
error.PHigh.reg2 

left.PHigh.reg2 <- mean.PHigh.reg2 - error.PHigh.reg2
left.PHigh.reg2

right.PHigh.reg2 <- mean.PHigh.reg2 + error.PHigh.reg2
right.PHigh.reg2 


###
### Patienthood Low
###

length.PLow.reg2 <- length(reg2[which(reg2$Patienthood == "low"), ]$ResRT)
length.PLow.reg2 

error.PLow.reg2 <- qt(0.975,df=length.PLow.reg2-1)*sd.PLow.reg2/sqrt(length.PLow.reg2)
error.PLow.reg2 

left.PLow.reg2 <- mean.PLow.reg2 - error.PLow.reg2
left.PLow.reg2

right.PLow.reg2 <- mean.PLow.reg2 + error.PLow.reg2
right.PLow.reg2 


###
### Bigram Frequency High
###

length.FHigh.reg2 <- length(reg2[which(reg2$BigramF == "high"), ]$ResRT)
length.FHigh.reg2 

error.FHigh.reg2 <- qt(0.975,df=length.FHigh.reg2-1)*sd.FHigh.reg2/sqrt(length.FHigh.reg2)
error.FHigh.reg2 

left.FHigh.reg2 <- mean.FHigh.reg2 - error.FHigh.reg2
left.FHigh.reg2

right.FHigh.reg2 <- mean.FHigh.reg2 + error.FHigh.reg2
right.FHigh.reg2 


###
### Bigram Frequency Low
###

length.FLow.reg2 <- length(reg2[which(reg2$BigramF == "low"), ]$ResRT)
length.FLow.reg2 

error.FLow.reg2 <- qt(0.975,df=length.FLow.reg2-1)*sd.FLow.reg2/sqrt(length.FLow.reg2)
error.FLow.reg2 

left.FLow.reg2 <- mean.FLow.reg2 - error.FLow.reg2
left.FLow.reg2

right.FLow.reg2 <- mean.FLow.reg2 + error.FLow.reg2
right.FLow.reg2 





###########################################################
####
####  LMER
####
###########################################################



attach(reg2)
lmer.reg2.Patienthood <- lmer(ResRT ~ Patienthood + (1|WorkerID) + (1|Item) + (1|List), data=reg2)
summary(lmer.reg2.Patienthood)
lmer.reg2.Frequency <- lmer(ResRT ~ BigramF + (1|WorkerID) + (1|Item) + (1|List), data=reg2)
summary(lmer.reg2.Frequency)
lmer.reg2.Interaction <- lmer(ResRT ~ Patienthood*BigramF + (1|WorkerID) + (1|Item) + (1|List), data=reg2)
summary(lmer.reg2.Interaction)
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

mean(reg3[which(reg3$Patienthood == "low"), ]$ReadingTime)
sd(reg3[which(reg3$Patienthood == "low"), ]$ReadingTime)

mean(reg3[which(reg3$Patienthood == "high"), ]$ReadingTime)
sd(reg3[which(reg3$Patienthood == "high"), ]$ReadingTime)

mean(reg3[which(reg3$BigramF == "low"), ]$ReadingTime)
sd(reg3[which(reg3$BigramF == "low"), ]$ReadingTime)

mean(reg3[which(reg3$BigramF == "high"), ]$ReadingTime)
sd(reg3[which(reg3$BigramF == "high"), ]$ReadingTime)



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


mean.PHigh.reg3 <- mean(reg3[which(reg3$Patienthood == "high"), ]$ResRT)
mean.PHigh.reg3
sd.PHigh.reg3 <- sd(reg3[which(reg3$Patienthood == "high"), ]$ResRT)
sd.PHigh.reg3

mean.PLow.reg3 <- mean(reg3[which(reg3$Patienthood== "low"), ]$ResRT)
mean.PLow.reg3
sd.PLow.reg3 <- sd(reg3[which(reg3$Patienthood== "low"), ]$ResRT)
sd.PLow.reg3

mean.FHigh.reg3 <- mean(reg3[which(reg3$BigramF == "high"), ]$ResRT)
mean.FHigh.reg3
sd.FHigh.reg3 <- sd(reg3[which(reg3$BigramF == "high"), ]$ResRT)
sd.FHigh.reg3

mean.FLow.reg3 <- mean(reg3[which(reg3$BigramF== "low"), ]$ResRT)
mean.FLow.reg3
sd.FLow.reg3 <- sd(reg3[which(reg3$BigramF == "low"), ]$ResRT)
sd.FLow.reg3



###########################################################
####
####  95% Confidence Interval Calculation
####
###########################################################


###
### Patienthood High
###

length.PHigh.reg3 <- length(reg3[which(reg3$Patienthood == "high"), ]$ResRT)
length.PHigh.reg3 

error.PHigh.reg3 <- qt(0.975,df=length.PHigh.reg3-1)*sd.PHigh.reg3/sqrt(length.PHigh.reg3)
error.PHigh.reg3 

left.PHigh.reg3 <- mean.PHigh.reg3 - error.PHigh.reg3
left.PHigh.reg3

right.PHigh.reg3 <- mean.PHigh.reg3 + error.PHigh.reg3
right.PHigh.reg3 


###
### Patienthood Low
###

length.PLow.reg3 <- length(reg3[which(reg3$Patienthood == "low"), ]$ResRT)
length.PLow.reg3 

error.PLow.reg3 <- qt(0.975,df=length.PLow.reg3-1)*sd.PLow.reg3/sqrt(length.PLow.reg3)
error.PLow.reg3 

left.PLow.reg3 <- mean.PLow.reg3 - error.PLow.reg3
left.PLow.reg3

right.PLow.reg3 <- mean.PLow.reg3 + error.PLow.reg3
right.PLow.reg3 


###
### Bigram Frequency High
###

length.FHigh.reg3 <- length(reg3[which(reg3$BigramF == "high"), ]$ResRT)
length.FHigh.reg3 

error.FHigh.reg3 <- qt(0.975,df=length.FHigh.reg3-1)*sd.FHigh.reg3/sqrt(length.FHigh.reg3)
error.FHigh.reg3 

left.FHigh.reg3 <- mean.FHigh.reg3 - error.FHigh.reg3
left.FHigh.reg3

right.FHigh.reg3 <- mean.FHigh.reg3 + error.FHigh.reg3
right.FHigh.reg3 


###
### Bigram Frequency Low
###

length.FLow.reg3 <- length(reg3[which(reg3$BigramF == "low"), ]$ResRT)
length.FLow.reg3 

error.FLow.reg3 <- qt(0.975,df=length.FLow.reg3-1)*sd.FLow.reg3/sqrt(length.FLow.reg3)
error.FLow.reg3 

left.FLow.reg3 <- mean.FLow.reg3 - error.FLow.reg3
left.FLow.reg3

right.FLow.reg3 <- mean.FLow.reg3 + error.FLow.reg3
right.FLow.reg3 





###########################################################
####
####  LMER
####
###########################################################



attach(reg3)
lmer.reg3.Patienthood <- lmer(ResRT ~ Patienthood + (1|WorkerID) + (1|Item) + (1|List), data=reg3)
summary(lmer.reg3.Patienthood)
lmer.reg3.Frequency <- lmer(ResRT ~ BigramF + (1|WorkerID) + (1|Item) + (1|List), data=reg3)
summary(lmer.reg3.Frequency)
lmer.reg3.Interaction <- lmer(ResRT ~ Patienthood*BigramF + (1|WorkerID) + (1|Item) + (1|List), data=reg3)
summary(lmer.reg3.Interaction)
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

mean(reg4[which(reg4$Patienthood == "low"), ]$ReadingTime)
sd(reg4[which(reg4$Patienthood == "low"), ]$ReadingTime)

mean(reg4[which(reg4$Patienthood == "high"), ]$ReadingTime)
sd(reg4[which(reg4$Patienthood == "high"), ]$ReadingTime)

mean(reg4[which(reg4$BigramF == "low"), ]$ReadingTime)
sd(reg4[which(reg4$BigramF == "low"), ]$ReadingTime)

mean(reg4[which(reg4$BigramF == "high"), ]$ReadingTime)
sd(reg4[which(reg4$BigramF == "high"), ]$ReadingTime)



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


mean.PHigh.reg4 <- mean(reg4[which(reg4$Patienthood == "high"), ]$ResRT)
mean.PHigh.reg4
sd.PHigh.reg4 <- sd(reg4[which(reg4$Patienthood == "high"), ]$ResRT)
sd.PHigh.reg4

mean.PLow.reg4 <- mean(reg4[which(reg4$Patienthood== "low"), ]$ResRT)
mean.PLow.reg4
sd.PLow.reg4 <- sd(reg4[which(reg4$Patienthood== "low"), ]$ResRT)
sd.PLow.reg4

mean.FHigh.reg4 <- mean(reg4[which(reg4$BigramF == "high"), ]$ResRT)
mean.FHigh.reg4
sd.FHigh.reg4 <- sd(reg4[which(reg4$BigramF == "high"), ]$ResRT)
sd.FHigh.reg4

mean.FLow.reg4 <- mean(reg4[which(reg4$BigramF== "low"), ]$ResRT)
mean.FLow.reg4
sd.FLow.reg4 <- sd(reg4[which(reg4$BigramF == "low"), ]$ResRT)
sd.FLow.reg4



###########################################################
####
####  95% Confidence Interval Calculation
####
###########################################################


###
### Patienthood High
###

length.PHigh.reg4 <- length(reg4[which(reg4$Patienthood == "high"), ]$ResRT)
length.PHigh.reg4 

error.PHigh.reg4 <- qt(0.975,df=length.PHigh.reg4-1)*sd.PHigh.reg4/sqrt(length.PHigh.reg4)
error.PHigh.reg4 

left.PHigh.reg4 <- mean.PHigh.reg4 - error.PHigh.reg4
left.PHigh.reg4

right.PHigh.reg4 <- mean.PHigh.reg4 + error.PHigh.reg4
right.PHigh.reg4 


###
### Patienthood Low
###

length.PLow.reg4 <- length(reg4[which(reg4$Patienthood == "low"), ]$ResRT)
length.PLow.reg4 

error.PLow.reg4 <- qt(0.975,df=length.PLow.reg4-1)*sd.PLow.reg4/sqrt(length.PLow.reg4)
error.PLow.reg4 

left.PLow.reg4 <- mean.PLow.reg4 - error.PLow.reg4
left.PLow.reg4

right.PLow.reg4 <- mean.PLow.reg4 + error.PLow.reg4
right.PLow.reg4 


###
### Bigram Frequency High
###

length.FHigh.reg4 <- length(reg4[which(reg4$BigramF == "high"), ]$ResRT)
length.FHigh.reg4 

error.FHigh.reg4 <- qt(0.975,df=length.FHigh.reg4-1)*sd.FHigh.reg4/sqrt(length.FHigh.reg4)
error.FHigh.reg4 

left.FHigh.reg4 <- mean.FHigh.reg4 - error.FHigh.reg4
left.FHigh.reg4

right.FHigh.reg4 <- mean.FHigh.reg4 + error.FHigh.reg4
right.FHigh.reg4 


###
### Bigram Frequency Low
###

length.FLow.reg4 <- length(reg4[which(reg4$BigramF == "low"), ]$ResRT)
length.FLow.reg4 

error.FLow.reg4 <- qt(0.975,df=length.FLow.reg4-1)*sd.FLow.reg4/sqrt(length.FLow.reg4)
error.FLow.reg4 

left.FLow.reg4 <- mean.FLow.reg4 - error.FLow.reg4
left.FLow.reg4

right.FLow.reg4 <- mean.FLow.reg4 + error.FLow.reg4
right.FLow.reg4 





###########################################################
####
####  LMER
####
###########################################################



attach(reg4)
lmer.reg4.Patienthood <- lmer(ResRT ~ Patienthood + (1|WorkerID) + (1|Item) + (1|List), data=reg4)
summary(lmer.reg4.Patienthood)
lmer.reg4.Frequency <- lmer(ResRT ~ BigramF + (1|WorkerID) + (1|Item) + (1|List), data=reg4)
summary(lmer.reg4.Frequency)
lmer.reg4.Interaction <- lmer(ResRT ~ Patienthood*BigramF + (1|WorkerID) + (1|Item) + (1|List), data=reg4)
summary(lmer.reg4.Interaction)
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

mean(reg5[which(reg5$Patienthood == "low"), ]$ReadingTime)
sd(reg5[which(reg5$Patienthood == "low"), ]$ReadingTime)

mean(reg5[which(reg5$Patienthood == "high"), ]$ReadingTime)
sd(reg5[which(reg5$Patienthood == "high"), ]$ReadingTime)

mean(reg5[which(reg5$BigramF == "low"), ]$ReadingTime)
sd(reg5[which(reg5$BigramF == "low"), ]$ReadingTime)

mean(reg5[which(reg5$BigramF == "high"), ]$ReadingTime)
sd(reg5[which(reg5$BigramF == "high"), ]$ReadingTime)



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


mean.PHigh.reg5 <- mean(reg5[which(reg5$Patienthood == "high"), ]$ResRT)
mean.PHigh.reg5
sd.PHigh.reg5 <- sd(reg5[which(reg5$Patienthood == "high"), ]$ResRT)
sd.PHigh.reg5

mean.PLow.reg5 <- mean(reg5[which(reg5$Patienthood== "low"), ]$ResRT)
mean.PLow.reg5
sd.PLow.reg5 <- sd(reg5[which(reg5$Patienthood== "low"), ]$ResRT)
sd.PLow.reg5

mean.FHigh.reg5 <- mean(reg5[which(reg5$BigramF == "high"), ]$ResRT)
mean.FHigh.reg5
sd.FHigh.reg5 <- sd(reg5[which(reg5$BigramF == "high"), ]$ResRT)
sd.FHigh.reg5

mean.FLow.reg5 <- mean(reg5[which(reg5$BigramF== "low"), ]$ResRT)
mean.FLow.reg5
sd.FLow.reg5 <- sd(reg5[which(reg5$BigramF == "low"), ]$ResRT)
sd.FLow.reg5



###########################################################
####
####  95% Confidence Interval Calculation
####
###########################################################


###
### Patienthood High
###

length.PHigh.reg5 <- length(reg5[which(reg5$Patienthood == "high"), ]$ResRT)
length.PHigh.reg5 

error.PHigh.reg5 <- qt(0.975,df=length.PHigh.reg5-1)*sd.PHigh.reg5/sqrt(length.PHigh.reg5)
error.PHigh.reg5 

left.PHigh.reg5 <- mean.PHigh.reg5 - error.PHigh.reg5
left.PHigh.reg5

right.PHigh.reg5 <- mean.PHigh.reg5 + error.PHigh.reg5
right.PHigh.reg5 


###
### Patienthood Low
###

length.PLow.reg5 <- length(reg5[which(reg5$Patienthood == "low"), ]$ResRT)
length.PLow.reg5 

error.PLow.reg5 <- qt(0.975,df=length.PLow.reg5-1)*sd.PLow.reg5/sqrt(length.PLow.reg5)
error.PLow.reg5 

left.PLow.reg5 <- mean.PLow.reg5 - error.PLow.reg5
left.PLow.reg5

right.PLow.reg5 <- mean.PLow.reg5 + error.PLow.reg5
right.PLow.reg5 


###
### Bigram Frequency High
###

length.FHigh.reg5 <- length(reg5[which(reg5$BigramF == "high"), ]$ResRT)
length.FHigh.reg5 

error.FHigh.reg5 <- qt(0.975,df=length.FHigh.reg5-1)*sd.FHigh.reg5/sqrt(length.FHigh.reg5)
error.FHigh.reg5 

left.FHigh.reg5 <- mean.FHigh.reg5 - error.FHigh.reg5
left.FHigh.reg5

right.FHigh.reg5 <- mean.FHigh.reg5 + error.FHigh.reg5
right.FHigh.reg5 


###
### Bigram Frequency Low
###

length.FLow.reg5 <- length(reg5[which(reg5$BigramF == "low"), ]$ResRT)
length.FLow.reg5 

error.FLow.reg5 <- qt(0.975,df=length.FLow.reg5-1)*sd.FLow.reg5/sqrt(length.FLow.reg5)
error.FLow.reg5 

left.FLow.reg5 <- mean.FLow.reg5 - error.FLow.reg5
left.FLow.reg5

right.FLow.reg5 <- mean.FLow.reg5 + error.FLow.reg5
right.FLow.reg5 





###########################################################
####
####  LMER
####
###########################################################



attach(reg5)
lmer.reg5.Patienthood <- lmer(ResRT ~ Patienthood + (1|WorkerID) + (1|Item) + (1|List), data=reg5)
summary(lmer.reg5.Patienthood)
lmer.reg5.Frequency <- lmer(ResRT ~ BigramF + (1|WorkerID) + (1|Item) + (1|List), data=reg5)
summary(lmer.reg5.Frequency)
lmer.reg5.Interaction <- lmer(ResRT ~ Patienthood*BigramF + (1|WorkerID) + (1|Item) + (1|List), data=reg5)
summary(lmer.reg5.Interaction)
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

mean(reg6[which(reg6$Patienthood == "low"), ]$ReadingTime)
sd(reg6[which(reg6$Patienthood == "low"), ]$ReadingTime)

mean(reg6[which(reg6$Patienthood == "high"), ]$ReadingTime)
sd(reg6[which(reg6$Patienthood == "high"), ]$ReadingTime)

mean(reg6[which(reg6$BigramF == "low"), ]$ReadingTime)
sd(reg6[which(reg6$BigramF == "low"), ]$ReadingTime)

mean(reg6[which(reg6$BigramF == "high"), ]$ReadingTime)
sd(reg6[which(reg6$BigramF == "high"), ]$ReadingTime)



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


mean.PHigh.reg6 <- mean(reg6[which(reg6$Patienthood == "high"), ]$ResRT)
mean.PHigh.reg6
sd.PHigh.reg6 <- sd(reg6[which(reg6$Patienthood == "high"), ]$ResRT)
sd.PHigh.reg6

mean.PLow.reg6 <- mean(reg6[which(reg6$Patienthood== "low"), ]$ResRT)
mean.PLow.reg6
sd.PLow.reg6 <- sd(reg6[which(reg6$Patienthood== "low"), ]$ResRT)
sd.PLow.reg6

mean.FHigh.reg6 <- mean(reg6[which(reg6$BigramF == "high"), ]$ResRT)
mean.FHigh.reg6
sd.FHigh.reg6 <- sd(reg6[which(reg6$BigramF == "high"), ]$ResRT)
sd.FHigh.reg6

mean.FLow.reg6 <- mean(reg6[which(reg6$BigramF== "low"), ]$ResRT)
mean.FLow.reg6
sd.FLow.reg6 <- sd(reg6[which(reg6$BigramF == "low"), ]$ResRT)
sd.FLow.reg6



###########################################################
####
####  95% Confidence Interval Calculation
####
###########################################################


###
### Patienthood High
###

length.PHigh.reg6 <- length(reg6[which(reg6$Patienthood == "high"), ]$ResRT)
length.PHigh.reg6 

error.PHigh.reg6 <- qt(0.975,df=length.PHigh.reg6-1)*sd.PHigh.reg6/sqrt(length.PHigh.reg6)
error.PHigh.reg6 

left.PHigh.reg6 <- mean.PHigh.reg6 - error.PHigh.reg6
left.PHigh.reg6

right.PHigh.reg6 <- mean.PHigh.reg6 + error.PHigh.reg6
right.PHigh.reg6 


###
### Patienthood Low
###

length.PLow.reg6 <- length(reg6[which(reg6$Patienthood == "low"), ]$ResRT)
length.PLow.reg6 

error.PLow.reg6 <- qt(0.975,df=length.PLow.reg6-1)*sd.PLow.reg6/sqrt(length.PLow.reg6)
error.PLow.reg6 

left.PLow.reg6 <- mean.PLow.reg6 - error.PLow.reg6
left.PLow.reg6

right.PLow.reg6 <- mean.PLow.reg6 + error.PLow.reg6
right.PLow.reg6 


###
### Bigram Frequency High
###

length.FHigh.reg6 <- length(reg6[which(reg6$BigramF == "high"), ]$ResRT)
length.FHigh.reg6 

error.FHigh.reg6 <- qt(0.975,df=length.FHigh.reg6-1)*sd.FHigh.reg6/sqrt(length.FHigh.reg6)
error.FHigh.reg6 

left.FHigh.reg6 <- mean.FHigh.reg6 - error.FHigh.reg6
left.FHigh.reg6

right.FHigh.reg6 <- mean.FHigh.reg6 + error.FHigh.reg6
right.FHigh.reg6 


###
### Bigram Frequency Low
###

length.FLow.reg6 <- length(reg6[which(reg6$BigramF == "low"), ]$ResRT)
length.FLow.reg6 

error.FLow.reg6 <- qt(0.975,df=length.FLow.reg6-1)*sd.FLow.reg6/sqrt(length.FLow.reg6)
error.FLow.reg6 

left.FLow.reg6 <- mean.FLow.reg6 - error.FLow.reg6
left.FLow.reg6

right.FLow.reg6 <- mean.FLow.reg6 + error.FLow.reg6
right.FLow.reg6 





###########################################################
####
####  LMER
####
###########################################################



attach(reg6)
lmer.reg6.Patienthood <- lmer(ResRT ~ Patienthood + (1|WorkerID) + (1|Item) + (1|List), data=reg6)
summary(lmer.reg6.Patienthood)
lmer.reg6.Frequency <- lmer(ResRT ~ BigramF + (1|WorkerID) + (1|Item) + (1|List), data=reg6)
summary(lmer.reg6.Frequency)
lmer.reg6.Interaction <- lmer(ResRT ~ Patienthood*BigramF + (1|WorkerID) + (1|Item) + (1|List), data=reg6)
summary(lmer.reg6.Interaction)
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

mean(reg7[which(reg7$Patienthood == "low"), ]$ReadingTime)
sd(reg7[which(reg7$Patienthood == "low"), ]$ReadingTime)

mean(reg7[which(reg7$Patienthood == "high"), ]$ReadingTime)
sd(reg7[which(reg7$Patienthood == "high"), ]$ReadingTime)

mean(reg7[which(reg7$BigramF == "low"), ]$ReadingTime)
sd(reg7[which(reg7$BigramF == "low"), ]$ReadingTime)

mean(reg7[which(reg7$BigramF == "high"), ]$ReadingTime)
sd(reg7[which(reg7$BigramF == "high"), ]$ReadingTime)



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


mean.PHigh.reg7 <- mean(reg7[which(reg7$Patienthood == "high"), ]$ResRT)
mean.PHigh.reg7
sd.PHigh.reg7 <- sd(reg7[which(reg7$Patienthood == "high"), ]$ResRT)
sd.PHigh.reg7

mean.PLow.reg7 <- mean(reg7[which(reg7$Patienthood== "low"), ]$ResRT)
mean.PLow.reg7
sd.PLow.reg7 <- sd(reg7[which(reg7$Patienthood== "low"), ]$ResRT)
sd.PLow.reg7

mean.FHigh.reg7 <- mean(reg7[which(reg7$BigramF == "high"), ]$ResRT)
mean.FHigh.reg7
sd.FHigh.reg7 <- sd(reg7[which(reg7$BigramF == "high"), ]$ResRT)
sd.FHigh.reg7

mean.FLow.reg7 <- mean(reg7[which(reg7$BigramF== "low"), ]$ResRT)
mean.FLow.reg7
sd.FLow.reg7 <- sd(reg7[which(reg7$BigramF == "low"), ]$ResRT)
sd.FLow.reg7



###########################################################
####
####  95% Confidence Interval Calculation
####
###########################################################


###
### Patienthood High
###

length.PHigh.reg7 <- length(reg7[which(reg7$Patienthood == "high"), ]$ResRT)
length.PHigh.reg7 

error.PHigh.reg7 <- qt(0.975,df=length.PHigh.reg7-1)*sd.PHigh.reg7/sqrt(length.PHigh.reg7)
error.PHigh.reg7 

left.PHigh.reg7 <- mean.PHigh.reg7 - error.PHigh.reg7
left.PHigh.reg7

right.PHigh.reg7 <- mean.PHigh.reg7 + error.PHigh.reg7
right.PHigh.reg7 


###
### Patienthood Low
###

length.PLow.reg7 <- length(reg7[which(reg7$Patienthood == "low"), ]$ResRT)
length.PLow.reg7 

error.PLow.reg7 <- qt(0.975,df=length.PLow.reg7-1)*sd.PLow.reg7/sqrt(length.PLow.reg7)
error.PLow.reg7 

left.PLow.reg7 <- mean.PLow.reg7 - error.PLow.reg7
left.PLow.reg7

right.PLow.reg7 <- mean.PLow.reg7 + error.PLow.reg7
right.PLow.reg7 


###
### Bigram Frequency High
###

length.FHigh.reg7 <- length(reg7[which(reg7$BigramF == "high"), ]$ResRT)
length.FHigh.reg7 

error.FHigh.reg7 <- qt(0.975,df=length.FHigh.reg7-1)*sd.FHigh.reg7/sqrt(length.FHigh.reg7)
error.FHigh.reg7 

left.FHigh.reg7 <- mean.FHigh.reg7 - error.FHigh.reg7
left.FHigh.reg7

right.FHigh.reg7 <- mean.FHigh.reg7 + error.FHigh.reg7
right.FHigh.reg7 


###
### Bigram Frequency Low
###

length.FLow.reg7 <- length(reg7[which(reg7$BigramF == "low"), ]$ResRT)
length.FLow.reg7 

error.FLow.reg7 <- qt(0.975,df=length.FLow.reg7-1)*sd.FLow.reg7/sqrt(length.FLow.reg7)
error.FLow.reg7 

left.FLow.reg7 <- mean.FLow.reg7 - error.FLow.reg7
left.FLow.reg7

right.FLow.reg7 <- mean.FLow.reg7 + error.FLow.reg7
right.FLow.reg7 





###########################################################
####
####  LMER
####
###########################################################



attach(reg7)
lmer.reg7.Patienthood <- lmer(ResRT ~ Patienthood + (1|WorkerID) + (1|Item) + (1|List), data=reg7)
summary(lmer.reg7.Patienthood)
lmer.reg7.Frequency <- lmer(ResRT ~ BigramF + (1|WorkerID) + (1|Item) + (1|List), data=reg7)
summary(lmer.reg7.Frequency)
lmer.reg7.Interaction <- lmer(ResRT ~ Patienthood*BigramF + (1|WorkerID) + (1|Item) + (1|List), data=reg7)
summary(lmer.reg7.Interaction)
detach(reg7)


#####
#####
##### End LMER
#####
#####

