rm(list=ls())

####
#### Read dataset
####
data<-read.xlsx("Datasheet.xlsx",1,header=TRUE)
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



###########################################################
####
####  Add PatientHood, Bigram Frequency, Word Frequency from Datasheet
####
###########################################################

spr_raw["Patienthood"] <- NA
spr_raw["BigramF"] <- NA
spr_raw["W_Freq"] <- NA

for(type in unique(data$Type)){
  
  spr_raw[which(spr_raw$Type==type),]$Patienthood<-data[which(data$Type==type),]$PatientHood
  
  spr_raw[which(spr_raw$Type==type),]$BigramF<-(data[which(data$Type==type),]$BigramF)
  
  spr_raw[which(spr_raw$Type==type),]$W_Freq<-log(data[which(data$Type==type),]$W_Freq)
  
}


head(spr_raw)


###########################################################
####
####  Remove bad data
####
###########################################################


#spr_raw<- spr_raw[which(spr_raw$List!="List1"),]
spr_raw<- spr_raw[which(spr_raw$Type!="block2-HL-List1"),]



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
####  High Patienthood Statistics 
###########################################################
mean(spr_clean[which(spr_clean$Condition=="HH"|spr_clean$Condition=="HL"),]$Patienthood)
sd(spr_clean[which(spr_clean$Condition=="HH"|spr_clean$Condition=="HL"),]$Patienthood)



###########################################################
####  Low Patienthood Statistics 
###########################################################
mean(spr_clean[which(spr_clean$Condition=="LH"|spr_clean$Condition=="LL"),]$Patienthood)
sd(spr_clean[which(spr_clean$Condition=="LH"|spr_clean$Condition=="LL"),]$Patienthood)



###########################################################
####  High Dependency Frequency Statistics 
###########################################################
mean(spr_clean[which(spr_clean$Condition=="HH"|spr_clean$Condition=="LH"),]$BigramF)
sd(spr_clean[which(spr_clean$Condition=="HH"|spr_clean$Condition=="LH"),]$BigramF)


###########################################################
####  Low Dependency Frequency Statistics 
###########################################################
mean(spr_clean[which(spr_clean$Condition=="HL"|spr_clean$Condition=="LL"),]$BigramF)
sd(spr_clean[which(spr_clean$Condition=="HL"|spr_clean$Condition=="LL"),]$BigramF)