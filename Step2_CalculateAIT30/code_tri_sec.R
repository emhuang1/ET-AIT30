## This code computes the second-level AI, then the minute-level AI, and lastly the
## day-level AI-T30. Note that the AI-T30 that is produced by this script is unscaled 
## (i.e., it has not been multiplied by 60)

rm(list=ls())

#########################################################################################################
##Libraries
#########################################################################################################

library(lubridate)
library(dplyr)

#########################################################################################################
##Directories and Subjects
#########################################################################################################

input_directory <- "/deac/sta/huangGrp/cnoc/et_1"

output_directory <- "/deac/sta/huangGrp/etAnalysis/AI_Calc_tri_sec/output/"

#Patients
patients <- list.files(input_directory)

#########################################################################################################
##Functions
#########################################################################################################

AIT30_calculate <- function(results.day, results.min){
  results.day$AI.T30 <- NA #average of top 30 AI's
  results.day$AI.T5 <- NA #average of top 5 AI's
  results.day$varAI.T30 <- NA #variance of top 30 AI's
  results.day$varAI.T5 <- NA #variance of top 5 AI's
  ndates <- nrow(results.day)
  
  for (i in 1:ndates){
    d <- results.day$date[i]
    sub_data <- results.min[results.min$date == d,]
    temp <- sort(sub_data$AI, decreasing = TRUE)
    if (length(temp)>=30){
      results.day$AI.T30[i] <- mean(temp[1:30])
      results.day$AI.T5[i] <- mean(temp[1:5])
      results.day$varAI.T30[i] <- var(temp[1:30])  
      results.day$varAI.T5[i] <- var(temp[1:5])
    } 
  }
  
  return(results.day)
}

#########################################################################################################
##AI calculations
#########################################################################################################

for (patient in patients){
  print(patient)
  
  files <- list.files(paste(input_directory,patient,"accelerometer",sep="/"))
  nfiles <- length(files)
  
  #########################################################################################################
  ##Calculating AI (minute-level)
  #########################################################################################################
  
  output_list <- list()
  #test_list <- list()
  
  for (i in 1:nfiles){
    #print(i)
    
    #Read in data
    data <- read.csv(paste(input_directory,patient,"accelerometer", files[i],sep="/"))
    
    ##Create new variable for EST time
    data$EST.time <- ymd_hms(data$UTC.time, tz = "UTC")
    data$EST.time <- with_tz(data$EST.time, tzone = "America/New_York")
    
    ##Get date, hour, minute, second
    data$date <- date(data$EST.time)
    data$hour <- hour(data$EST.time)
    data$minute <- minute(data$EST.time)
    data$exact_second <- second(data$EST.time)
    data$second <- floor(data$exact_second)
    
    ##We will calculate AI at the second-level
    ##var_x, var_y, var_z: variance for x, y, and z measurements for the given second
    ##samples_in_sec: number of recordings within the second
    temp <- data %>% group_by(date, hour, minute, second) %>% 
      summarise(var_x = var(x), var_y = var(y), var_z = var(z), samples_in_sec = n(),
                .groups = "drop")
    temp <- as.data.frame(temp)
    temp$AIsec <- 1/3*(temp$var_x+temp$var_y+temp$var_z)
    temp$AIsec[temp$AIsec < 0] <- 0 
    temp$AIsec <- sqrt(temp$AIsec)
    
    ##Retain rows with at least 10 samples
    temp <- temp[temp$samples_in_sec >= 10,]
    #hist(temp$samples_in_sec)
    
    ##Average second-level AI's within the minute (AI)
    ##Count the number of recordings within the minute (samples)
    temp2 <- temp %>% group_by(date, hour, minute) %>%
      summarise(AI = mean(AIsec), samples = sum(samples_in_sec), .groups = "drop")
    temp2 <- as.data.frame(temp2)
    
    output_list[[i]] <- temp2
    
    
  }
  
  results.min <- do.call(rbind, output_list)
  rm(output_list)
  
  
  ##Save results.min
  write.csv(results.min, file = paste(output_directory, "base_results/", patient,"_AI_minuteLevel_minBB.csv",sep = ""), 
            row.names = FALSE)
  
  #Plot the number of samples on which AI is based
  pdf(file = paste(output_directory, "plots/", patient, "_samples.pdf", sep = ""))
  hist(results.min$samples, breaks = 50, xlab = "Number of samples",
       main = paste("Histogram for Subject ", patient, sep = ""))
  dev.off()
  
  #Drop rows with fewer than 20 samples
  results20.min <- results.min[results.min$samples >= 20,]
  
  
  rm(results.min)

  #########################################################################################################
  ##Calculating AI (daily-level)
  #########################################################################################################
  
  
  results20.day <- results20.min %>% group_by(date) %>% 
    summarise(AI = sum(AI), nmin = n(),
              .groups = "drop") 
  results20.day <- as.data.frame(results20.day)
  
  
  
  
  #########################################################################################################
  ##Calculating AI-T30 and AI-T5 (daily-level)
  #########################################################################################################
  
  results20.day <- AIT30_calculate(results20.day, results20.min)
  
  #########################################################################################################
  ##Save output (minute- and day-level)
  #########################################################################################################
  
  #Save output
  write.csv(results20.day, file = paste(output_directory, "base_results/", patient,"_AI_dayLevel_cutoff20_minBB.csv",sep = ""), 
            row.names = FALSE)
  
  
  #Clear workspace
  rm(results20.day,results20.min,files,i,nfiles,temp,temp2,data)
}
