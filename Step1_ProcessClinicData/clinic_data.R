##5/29/25

##Calculate descriptive statistics for age and sex across all subjects and by imbalance group

rm(list=ls())

############################################################################################
##Libraries##
############################################################################################

library(lubridate)

############################################################################################
##Read in clinic data##
############################################################################################

clinic_data <- read.csv("~/Documents/datasets/CNOC/et/clinic_data.csv")

head(clinic_data)
tail(clinic_data)

clinic_data$Beiwe.ID
clinic_data <- clinic_data[clinic_data$Beiwe.ID != "",]

##Retrieve variables of interest
data <- data.frame(Beiwe.ID = clinic_data$Beiwe.ID, 
                   DOB = clinic_data$DOB, 
                   Procedure.date = clinic_data$Procedure.date, 
                   Age = clinic_data$Age,
                   Sex = clinic_data$Sex..0.Male..1.Female.,
                   Imbalance = clinic_data$Post.op.day.1.imbalance..1...No..1...Mild..2...Yes.)

##Sample size
nrow(data)

############################################################################################
##Calculate Age at time of FUS##
############################################################################################

x <- interval(ymd(data$DOB),ymd(data$Procedure.date))
data$Age.FUS <-  x / days(1)
data$Age.FUS <- data$Age.FUS/365.25 #365.25 days per year

############################################################################################
##Separate data frames##
############################################################################################

#Create separate data frames for imbalance groups
imbalance0 <- data[data$Imbalance == 0,]
imbalance1 <- data[data$Imbalance == 1,]
imbalance2 <- data[data$Imbalance == 2,]

############################################################################################
##Calculate percentages and counts for imbalance group##
############################################################################################

sum(is.na(data$Imbalance))
table(data$Imbalance, useNA = "ifany")
table(data$Imbalance, useNA = "ifany")/nrow(data)

############################################################################################
##Calculate percentages and counts for sex##
############################################################################################

sum(is.na(data$Sex))

table(data$Sex, useNA = "ifany")
table(data$Sex, useNA = "ifany")/nrow(data)

table(imbalance0$Sex, useNA = "ifany")
table(imbalance0$Sex, useNA = "ifany")/nrow(imbalance0)

table(imbalance1$Sex, useNA = "ifany")
table(imbalance1$Sex, useNA = "ifany")/nrow(imbalance1)

table(imbalance2$Sex, useNA = "ifany")
table(imbalance2$Sex, useNA = "ifany")/nrow(imbalance2)

############################################################################################
##Calculate age statistics##
############################################################################################

#Mean and standard deviation

mean(data$Age.FUS)
sd(data$Age.FUS)

mean(imbalance0$Age.FUS)
sd(imbalance0$Age.FUS)

mean(imbalance1$Age.FUS)
sd(imbalance1$Age.FUS)

mean(imbalance2$Age.FUS)
sd(imbalance2$Age.FUS)

#Plots of age distribution
hist(data$Age.FUS, breaks = 10)
boxplot(Age.FUS ~ Imbalance, data = data)

#Five-number summary for age
summary(data$Age.FUS)
summary(imbalance0$Age.FUS)
summary(imbalance1$Age.FUS)
summary(imbalance2$Age.FUS)


############################################################################################
##Read in operating system (OS) data##
############################################################################################

#Record the type of phone operating system each patient had and 
#save the results in data. The goal is to find out whether the phone used
#Android or iOS

#We record device id, device os, os_version, and manufacturer

#If a patient had multiple os (e.g., 16.2 & 16.3.1), the first one is recorded

#If a patient had multiple manufactuers ("iPhone14" "iPhone13"), the first one is recorded

OS_path <- "~/Documents/datasets/CNOC/et/OS"

data$device_id <- NA
data$device_os <- NA
data$os_version <- NA
data$manufacturer <- NA

for (i in 1:nrow(data)){
  id <- data$Beiwe.ID[i]
  print(id)
  
  file_names <- list.files(paste(OS_path,id,"identifiers",sep = "/"))
  
  pat_device_id <- rep(NA, length(file_names))
  pat_device_os <- rep(NA, length(file_names))
  pat_os_version <- rep(NA, length(file_names))
  pat_manufacturer <- rep(NA, length(file_names))
  
  for (j in 1:length(file_names)){
    #print(file_names[j])
    os_file <- read.csv(paste(OS_path,id,"identifiers", file_names[j], sep = "/"))
    pat_device_id[j] <- os_file$device_id
    pat_device_os[j] <- os_file$device_os
    pat_os_version[j] <- os_file$os_version
    pat_manufacturer[j] <- os_file$manufacturer
    rm(os_file)
  }
  
  pat_device_id <- unique(pat_device_id)
  pat_device_os <- unique(pat_device_os)
  pat_os_version <- unique(pat_os_version)
  pat_manufacturer <- unique(pat_manufacturer)
  
  if (length(pat_device_id)>1){
    stop("Wrong length")
  }
  
  if (length(pat_device_os)>1){
    warning("Multiple OS")
    print(pat_device_os)
    pat_device_os <- pat_device_os[1]
  }
  
  if (length(pat_os_version)>1){
    stop("Wrong length")
  }
  
  if (length(pat_manufacturer)>1){
    warning("Multiple manufacturers")
    print(pat_manufacturer)
    pat_manufacturer <- pat_manufacturer[1]
  }
  
  data$device_id[i] <- pat_device_id
  data$device_os[i] <- pat_device_os
  data$os_version[i] <- pat_os_version
  data$manufacturer[i] <- pat_manufacturer
  
  rm(id, file_names)
}

data$device_id
data$device_os
