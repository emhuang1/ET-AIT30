##6/3/25

##Calculate summary statistics for NDY, NDY-Pre, NDY-Post, FDY, LDY, and MPD
##across all subjects and by imbalance group

rm(list=ls())

############################################################################
##Libraries##
############################################################################

library(lubridate)
library(dplyr)

############################################################################
##Inputs##
############################################################################

dayMin <- -56 ##8 weeks before FUS
dayMax <- 56 ##8 weeks after FUS

nAI.thresh <- 90 #minimum number of AI minutes per AI-T30 day

#Directory with the day-level AI-T30 results
inputDir <- "~/Dropbox/WakeForest/research/CNOC/output/AI_Calc_tri_sec/output/base_results"

#Output Directory for processed day-level AI-T30 results
outputDir <- "~/Dropbox/research/github/ET_tri_sec/output/processedDF"

#Directory for plots
plotDir <- "~/Dropbox/research/github/ET_tri_sec/output/figs"

############################################################################
##Read in clinic data##
############################################################################

#Read in clinic data set
clinic_data <- read.csv("~/Documents/datasets/CNOC/et/clinic_data.csv")

head(clinic_data)
tail(clinic_data)

clinic_data <- clinic_data[clinic_data$Beiwe.ID != "",]
nrow(clinic_data)

##Retrieve variables of interest
clinic_data <- data.frame(Beiwe.ID = clinic_data$Beiwe.ID,
                          Procedure.date = clinic_data$Procedure.date, 
                          Age = clinic_data$Age,
                          Sex = clinic_data$Sex..0.Male..1.Female.,
                          Imbalance = clinic_data$Post.op.day.1.imbalance..1...No..1...Mild..2...Yes.)

##Number of patients
npatients <- nrow(clinic_data)

############################################################################
##Read in AI-T30 results##
############################################################################

setwd(inputDir)

#Get AI-T30 values for each patient, save the results in all_pat_data

all_pat_data <- list()

for (i in 1:npatients){
  #Focus on one patient at a time 
  beiwe.id <- clinic_data$Beiwe.ID[i] 
  print(beiwe.id)
  
  #Read in AI-T30 values
  pat_data <- read.csv(paste(beiwe.id,"_AI_dayLevel_cutoff20_minBB.csv",sep=""))
  
  #Remove dates without AI-T30 values
  pat_data <- pat_data[!is.na(pat_data$AI.T30),]
  print(nrow(pat_data))
  
  if (nrow(pat_data) == 0){
    print("No AI-T30 values") 
  } else{
    #Get FUS date
    procedure_date <- clinic_data$Procedure.date[clinic_data$Beiwe.ID == beiwe.id]
    print(procedure_date)
    
    #Calculate time relative to FUS
    x <- interval(ymd(procedure_date), ymd(pat_data$date))
    pat_data$days_since_FUS <- x / days(1)
    test1 <- ymd(pat_data$date)-ymd(procedure_date)
    check1 <- sum(pat_data$days_since_FUS - test1 != 0)
    if (check1 != 0){
      stop("Error in time difference code")
    }
    
    #Plot day vs AI-T30
    plot(pat_data$days_since_FUS, pat_data$AI.T30,
         xlab = "Day", ylab = "AI-T30", pch = 16, type = "p", main = beiwe.id)
    abline(v = 0, col = "red")
    
    #Imbalance
    pat_imbalance <- clinic_data$Imbalance[clinic_data$Beiwe.ID == beiwe.id]
    
    #Save (id, day, nmin, AI-T30, imbalance) into all_pat_data
    all_pat_data[[i]] <- data.frame(id = beiwe.id, 
                                    days_since_FUS = pat_data$days_since_FUS, 
                                    nmin = pat_data$nmin,
                                    AI.T30 = pat_data$AI.T30,
                                    imbalance = pat_imbalance)
    
    rm(beiwe.id, pat_data, procedure_date, x, pat_imbalance, check1, test1)
  }
}

#Put all results into a data frame instead of a list
all_pat_data <- do.call(rbind, all_pat_data)
data_plot <- all_pat_data

############################################################################
##Formatting imbalance variable##
############################################################################

#Make imbalance a factor variable
data_plot$imbalance <- as.factor(data_plot$imbalance)

#Add an A, B, C version of imbalance
data_plot$imbalanceABC <- NA
data_plot$imbalanceABC[data_plot$imbalance == 0] <- "A"
data_plot$imbalanceABC[data_plot$imbalance == 1] <- "B"
data_plot$imbalanceABC[data_plot$imbalance == 2] <- "C"
data_plot$imbalanceABC <- as.factor(data_plot$imbalanceABC)

table(data_plot$imbalance, data_plot$imbalanceABC, useNA = "ifany")

############################################################################
##Getting patient id's##
############################################################################

#Separate by imbalance group
patients0 <- clinic_data$Beiwe.ID[clinic_data$Imbalance==0]
patients1 <- clinic_data$Beiwe.ID[clinic_data$Imbalance==1]
patients2 <- clinic_data$Beiwe.ID[clinic_data$Imbalance==2]

############################################################################
##Renaming of patient id's##
############################################################################

map0 <- data.frame(beiwe.id = patients0, paper.id = NA)
map0$paper.id <- paste("A", 1:length(patients0), sep = "")

map1 <- data.frame(beiwe.id = patients1, paper.id = NA)
map1$paper.id <- paste("B", 1:length(patients1), sep = "")

map2 <- data.frame(beiwe.id = patients2, paper.id = NA)
map2$paper.id <- paste("C", 1:length(patients2), sep = "")

map <- rbind(map0, map1, map2)

##Add shorter ID labels for paper
data_plot$paper.id <- NA 
for (i in 1:nrow(data_plot)){
  data_plot$paper.id[i] <- map$paper.id[data_plot$id[i]==map$beiwe.id]
}
data_plot$paper.id <- factor(data_plot$paper.id, 
                             levels = c(map0$paper.id,map1$paper.id,map2$paper.id))

############################################################################
##Save data_plot output for subsequent steps##
############################################################################

write.csv(data_plot, 
          file = paste(outputDir,"aiT30.csv",sep = "/"), 
          row.names = FALSE)

############################################################################
##Exclude data outside the time range and with too few AI minutes##
############################################################################

#Narrow data to time range
hist(data_plot$days_since_FUS)
data_plot <- data_plot[data_plot$days_since_FUS >= dayMin & data_plot$days_since_FUS <= dayMax,]
hist(data_plot$days_since_FUS)
summary(data_plot$days_since_FUS)

#Narrow down to enough AI minutes
data_plot <- data_plot[data_plot$nmin >= nAI.thresh,]
summary(data_plot$nmin)



############################################################################
##Get accelerometer data quality statistics##
############################################################################

ndays_AIT30 <- data.frame(id = clinic_data$Beiwe.ID,
                          Imbalance = clinic_data$Imbalance,
                          ndays = NA,
                          ndays.beforeFUS = NA,
                          ndays.afterFUS = NA,
                          AImin.perDay = NA,
                          firstDay = NA,
                          lastDay = NA,
                          paper.id = NA)

#Change imbalance to A, B, C
table(ndays_AIT30$Imbalance)
ndays_AIT30$Imbalance[ndays_AIT30$Imbalance == 0] <- "A"
ndays_AIT30$Imbalance[ndays_AIT30$Imbalance == 1] <- "B"
ndays_AIT30$Imbalance[ndays_AIT30$Imbalance == 2] <- "C"
table(ndays_AIT30$Imbalance)

#Turn Imbalance into a factor variable
ndays_AIT30$Imbalance <- as.factor(ndays_AIT30$Imbalance)
ndays_AIT30$Imbalance

#Number of rows
n <- nrow(ndays_AIT30)

for (i in 1:n){
  #Focus on one patient
  patient <- ndays_AIT30$id[i]
  print(patient)

  #Get data frame for patient
  pat_data <- data_plot[data_plot$id == patient,]
  
  if (nrow(pat_data)==0){
    #Patient with no days with AI-T30
    ndays_AIT30$ndays[i] <- nrow(pat_data)
    ndays_AIT30$ndays.beforeFUS[i] <- 0
    ndays_AIT30$ndays.afterFUS[i] <- 0
    ndays_AIT30$AImin.perDay[i] <- NA
    ndays_AIT30$firstDay[i] <- NA
    ndays_AIT30$lastDay[i] <- NA
    ndays_AIT30$paper.id[i] <- map$paper.id[map$beiwe.id==patient]
  } else{
    #Patient with some days with AI-T30
    
    #number of days
    ndays_AIT30$ndays[i] <- nrow(pat_data) 
    
    #number of days before day 0
    ndays_AIT30$ndays.beforeFUS[i] <- sum(pat_data$days_since_FUS < 0)
    
    #number of days on or after day 0
    ndays_AIT30$ndays.afterFUS[i] <- sum(pat_data$days_since_FUS >= 0)
    
    #avg number of AI minutes per AI-T30 day
    ndays_AIT30$AImin.perDay[i] <- mean(pat_data$nmin)
    
    #first day
    ndays_AIT30$firstDay[i] <- min(pat_data$days_since_FUS)
    
    #last day
    ndays_AIT30$lastDay[i] <- max(pat_data$days_since_FUS)
    
    #paper.id
    ndays_AIT30$paper.id[i] <- map$paper.id[map$beiwe.id==patient]
  }
}

ndays_AIT30$ndays.beforeFUS + ndays_AIT30$ndays.afterFUS == ndays_AIT30$ndays


#summary statistics are calculated for those with NDY > 0
ndays_AIT30 <- ndays_AIT30[ndays_AIT30$ndays > 0,] 
nrow(ndays_AIT30)

#######################################################################
#Summary statistics for NDY, NDY-Pre, NDY-Post, MPD
#######################################################################

##All patients

hist(ndays_AIT30$ndays)

summary(ndays_AIT30$ndays.beforeFUS)
summary(ndays_AIT30$ndays.afterFUS)
summary(ndays_AIT30$ndays)
sd(ndays_AIT30$ndays.beforeFUS)
sd(ndays_AIT30$ndays.afterFUS)
sd(ndays_AIT30$ndays)

summary(ndays_AIT30$AImin.perDay)
sd(ndays_AIT30$AImin.perDay, na.rm = TRUE)

#Imbalance group A

temp0 <- ndays_AIT30[ndays_AIT30$Imbalance == "A",]
nrow(temp0)

hist(temp0$ndays)
summary(temp0$ndays.beforeFUS)
summary(temp0$ndays.afterFUS)
summary(temp0$ndays)

sd(temp0$ndays.beforeFUS)
sd(temp0$ndays.afterFUS)
sd(temp0$ndays)

summary(temp0$AImin.perDay)
sd(temp0$AImin.perDay)

#Imbalance group B

temp1 <- ndays_AIT30[ndays_AIT30$Imbalance == "B",]
nrow(temp1)

hist(temp1$ndays)
summary(temp1$ndays.beforeFUS)
summary(temp1$ndays.afterFUS)
summary(temp1$ndays)

sd(temp1$ndays.beforeFUS)
sd(temp1$ndays.afterFUS)
sd(temp1$ndays)

summary(temp1$AImin.perDay)
sd(temp1$AImin.perDay, na.rm = TRUE)

#Imbalance group C

temp2 <- ndays_AIT30[ndays_AIT30$Imbalance == "C",]
nrow(temp2)

hist(temp2$ndays)

summary(temp2$ndays.beforeFUS)
summary(temp2$ndays.afterFUS)
summary(temp2$ndays)

sd(temp2$ndays.beforeFUS)
sd(temp2$ndays.afterFUS)
sd(temp2$ndays)

summary(temp2$AImin.perDay)
sd(temp2$AImin.perDay, na.rm = TRUE)

#######################################################################
#Summary statistics for FDY
#######################################################################

#All patients

summary(ndays_AIT30$firstDay)

mean(ndays_AIT30$firstDay, na.rm = TRUE)
sd(ndays_AIT30$firstDay, na.rm = TRUE)

table(ndays_AIT30$firstDay, useNA = "ifany")

sum(ndays_AIT30$firstDay < -28, na.rm = TRUE)
mean(ndays_AIT30$firstDay < -28, na.rm = TRUE)

#Imbalance group A

summary(temp0$firstDay)

mean(temp0$firstDay)
sd(temp0$firstDay)

table(temp0$firstDay, useNA = "ifany")

#Imbalance group B

summary(temp1$firstDay)

mean(temp1$firstDay, na.rm = TRUE)
sd(temp1$firstDay, na.rm = TRUE)

table(temp1$firstDay, useNA = "ifany")

#Imbalance group C

summary(temp2$firstDay)

mean(temp2$firstDay, na.rm = TRUE)
sd(temp2$firstDay, na.rm = TRUE)

table(temp2$firstDay, useNA = "ifany")

#######################################################################
#Summary statistics for LDY
#######################################################################

#All patients
summary(ndays_AIT30$lastDay)
mean(ndays_AIT30$lastDay)
sd(ndays_AIT30$lastDay)

mean(ndays_AIT30$lastDay > 28)
sum(ndays_AIT30$lastDay > 28)

#Imbalance group A
summary(temp0$lastDay)
mean(temp0$lastDay)
sd(temp0$lastDay)

#Imbalance group B
summary(temp1$lastDay)
mean(temp1$lastDay, na.rm = TRUE)
sd(temp1$lastDay, na.rm = TRUE)

#Imbalance group C
summary(temp2$lastDay)
mean(temp2$lastDay, na.rm = TRUE)
sd(temp2$lastDay, na.rm = TRUE)


#######################################################################
#Dividing patients into groups based on amount of AI-T30 collection
#######################################################################

lowColl <- ndays_AIT30$paper.id[ndays_AIT30$ndays.afterFUS < 12]
lowColl

highColl <- ndays_AIT30$paper.id[ndays_AIT30$ndays.afterFUS > 28]
highColl

medColl <- ndays_AIT30$paper.id[ndays_AIT30$ndays.afterFUS <= 28 & ndays_AIT30$ndays.afterFUS >= 12]
medColl

