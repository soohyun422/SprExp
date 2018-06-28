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



spr_raw <- subset(ibex_raw, substr(ibex_raw$Type,0,5) == "block" )
spr_raw$ItemNumber <- factor(gsub("block","",gsub("-","",regmatches(spr_raw$Type,regexpr("^block[[:digit:]]+\\-",spr_raw$Type,perl=TRUE)))))
spr_raw$Condition <- factor(gsub("-","",regmatches(spr_raw$Type,regexpr("\\-[a-zA-Z]+\\-",spr_raw$Type,perl=TRUE))))
spr_raw$List <- factor(gsub("-","",regmatches(spr_raw$Type,regexpr("\\-[a-zA-Z1-4]+$",spr_raw$Type,perl=TRUE))))





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


###########################################################
####
####  Add Presentation Order
####
###########################################################


spr_clean$Order <-NA


for (participant in unique(spr_clean$WorkerID)){
  participant_clean <- spr_clean[which(spr_clean$WorkerID==participant),]
  order = 1
  for(item in unique(participant_clean$ItemNumber)){
    spr_clean[which(spr_clean$WorkerID==participant & spr_clean$ItemNumber==item),]$Order <- order
    order = order+1
  }
}

table(spr_clean$Order)



###########################################################
####
####  Remove bad data
####
###########################################################


#spr_clean<- spr_clean[which(spr_raw$List!="List1"),] 
spr_clean<- spr_clean[which(spr_clean$Type!="block2-HL-List1"),]


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
####  Length residual correction
####
###########################################################


reg1$ResRT <- reg1$ReadingTime - (reg1$Length * reg1$LBeta) ## per participant calculation



###########################################################
####
####  LMER
####
###########################################################



attach(reg1)
lmer.reg1.Order <- lmer(ResRT ~ Order + (1|WorkerID) + (1|Item) + (1|List), data=reg1)
summary(lmer.reg1.Order)

lmer.reg1.HL.Order <-lmer(ResRT ~Order +(1|WorkerID) + (1|Item) + (1|List), data=reg1[which(reg3$Condition=="HL"),] ) 
summary(lmer.reg1.HL.Order)
detach(reg1)

###########################################################
###########################################################
####
####  Region  2
####
###########################################################
###########################################################



###########################################################
####
####  Length residual correction
####
###########################################################


reg2$ResRT <- reg2$ReadingTime - (reg2$Length * reg2$LBeta) ## per participant calculation


###########################################################
####
####  LMER
####
###########################################################



attach(reg2)
lmer.reg2.Order<- lmer(ResRT ~ Order + (1|WorkerID) + (1|Item) + (1|List), data=reg2)
summary(lmer.reg2.Order)

lmer.reg2.HL.Order <-lmer(ResRT ~Order +(1|WorkerID) + (1|Item) + (1|List), data=reg2[which(reg2$Condition=="HL"),] ) 
summary(lmer.reg2.HL.Order)
detach(reg2)


###########################################################
###########################################################
####
####  Region  3
####
###########################################################
###########################################################



###########################################################
####
####  Length residual correction
####
###########################################################


reg3$ResRT <- reg3$ReadingTime - (reg3$Length * reg3$LBeta) ## per participant calculation



###########################################################
####
####  LMER
####
###########################################################



attach(reg3)
lmer.reg3.Order <- lmer(ResRT ~ Order + (1|WorkerID) + (1|Item) + (1|List), data=reg3)
summary(lmer.reg3.Order)

lmer.reg3.HL.Order <-lmer(ResRT ~Order +(1|WorkerID) + (1|Item) + (1|List), data=reg3[which(reg3$Condition=="HL"),] ) 
summary(lmer.reg3.HL.Order)
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
####  Length residual correction
####
###########################################################


reg4$ResRT <- reg4$ReadingTime - (reg4$Length * reg4$LBeta) ## per participant calculation




###########################################################
####
####  LMER
####
###########################################################



attach(reg4)
lmer.reg4.Order <- lmer(ResRT ~ Order + (1|WorkerID) + (1|Item) + (1|List), data=reg4)
summary(lmer.reg4.Order)

lmer.reg4.HL.Order <-lmer(ResRT ~Order +(1|WorkerID) + (1|Item) + (1|List), data=reg4[which(reg4$Condition=="HL"),] ) 
summary(lmer.reg4.HL.Order)
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
####  Length residual correction
####
###########################################################


reg5$ResRT <- reg5$ReadingTime - (reg5$Length * reg5$LBeta) ## per participant calculation



###########################################################
####
####  LMER
####
###########################################################



attach(reg5)
lmer.reg5.Order <- lmer(ResRT ~ Order+ (1|WorkerID) + (1|Item) + (1|List), data=reg5)
summary(lmer.reg5.Order)

lmer.reg5.HL.Order <-lmer(ResRT ~Order +(1|WorkerID) + (1|Item) + (1|List), data=reg5[which(reg5$Condition=="HL"),] ) 
summary(lmer.reg5.HL.Order)
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
####  Length residual correction
####
###########################################################


reg6$ResRT <- reg6$ReadingTime - (reg6$Length * reg6$LBeta) ## per participant calculation


###########################################################
####
####  LMER
####
###########################################################



attach(reg6)
lmer.reg6.Order <- lmer(ResRT ~ Order + (1|WorkerID) + (1|Item) + (1|List), data=reg6)
summary(lmer.reg6.Order)

lmer.reg6.HL.Order <-lmer(ResRT ~Order +(1|WorkerID) + (1|Item) + (1|List), data=reg6[which(reg6$Condition=="HL"),] ) 
summary(lmer.reg6.HL.Order)
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
####  Length residual correction
####
###########################################################


reg7$ResRT <- reg7$ReadingTime - (reg7$Length * reg7$LBeta) ## per participant calculation




###########################################################
####
####  LMER
####
###########################################################



attach(reg7)
lmer.reg7.Order <- lmer(ResRT ~ Order + (1|WorkerID) + (1|Item) + (1|List), data=reg7)
summary(lmer.reg7.Order)

lmer.reg7.HL.Order <-lmer(ResRT ~Order +(1|WorkerID) + (1|Item) + (1|List), data=reg7[which(reg7$Condition=="HL"),] ) 
summary(lmer.reg7.HL.Order)
detach(reg7)


#####
#####
##### End LMER
#####
#####
