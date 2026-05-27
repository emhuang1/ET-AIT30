#6/16/25

#Plot AI minutes over the course of each day in the window of interest,
#separately for each patient

rm(list=ls())

############################################################################
##Libraries##
############################################################################

library(lubridate)
library(ggplot2)

############################################################################
##Inputs##
############################################################################

dayMin <- -56 ##8 weeks before FUS
dayMax <- 56 ##8 weeks after FUS

#At least 20 accelerometer samples per AI minute
minSamples <- 20 

#At least 90 AI minutes per AI-T30 day
nmin.thresh <- 90

#Directory with the minute-level AI results
inputDir <- "~/Dropbox/WakeForest/research/CNOC/output/AI_Calc_tri_sec/output/base_results"

#Directory for plots
plotDir <- "~/Dropbox/research/github/ET_tri_sec/output/figs"

############################################################################
##Read in clinic data##
############################################################################

clinic_data <- read.csv("~/Documents/datasets/CNOC/et/clinic_data.csv")

head(clinic_data)
tail(clinic_data)

clinic_data$Beiwe.ID
clinic_data <- clinic_data[clinic_data$Beiwe.ID != "",]
nrow(clinic_data)

##Retrieve variables of interest
clinic_data <- data.frame(Beiwe.ID = clinic_data$Beiwe.ID, 
                          Procedure.date = clinic_data$Procedure.date, 
                          Age = clinic_data$Age,
                          Sex = clinic_data$Sex..0.Male..1.Female.,
                          Imbalance = clinic_data$Post.op.day.1.imbalance..1...No..1...Mild..2...Yes.)

npatients <- nrow(clinic_data)

############################################################################
##Read in AI results##
############################################################################

setwd(inputDir)

#Get AI values for each patient, save the results in all_pat_data

all_pat_data <- list()

for (i in 1:npatients){
  #Focus on one patient at a time 
  beiwe.id <- clinic_data$Beiwe.ID[i] 
  print(beiwe.id)
  
  #Read in AI values
  pat_data <- read.csv(paste(beiwe.id,"_AI_minuteLevel_minBB.csv",sep=""))
  
  if (nrow(pat_data) == 0){
    print("No AI values") 
  } else{
    #Read in daily results
    daily_data <- read.csv(paste(beiwe.id,"_AI_dayLevel_cutoff20_minBB.csv",sep=""))
    
    #Check which dates are AI-T30 days
    aiT30days <- daily_data$date[daily_data$nmin >= nmin.thresh]
    
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
    
    #Calculate time of day (this is the hour of the day)
    pat_data$time_of_day <- (pat_data$hour * 60 + pat_data$minute)/60 
    
    #Imbalance
    pat_imbalance <- clinic_data$Imbalance[clinic_data$Beiwe.ID == beiwe.id]
    
    #Is the minute coming from an AI-T30 day?
    aiT30day.indicator <- pat_data$date %in% aiT30days
    
    #Save (id, day, time of day, AI, imbalance) into all_pat_data
    all_pat_data[[i]] <- data.frame(id = beiwe.id, 
                                    days_since_FUS = pat_data$days_since_FUS, 
                                    time_of_day = pat_data$time_of_day,
                                    AI = pat_data$AI,
                                    samples = pat_data$samples,
                                    imbalance = pat_imbalance,
                                    aiT30day = aiT30day.indicator)
    
    rm(beiwe.id, pat_data, procedure_date, x, pat_imbalance, 
       check1, test1, daily_data, aiT30days, aiT30day.indicator)
  }
  
}

#Put all results into a data frame instead of a list
all_pat_data <- do.call(rbind, all_pat_data)

#Make imbalance a factor variable
all_pat_data$imbalance <- as.factor(all_pat_data$imbalance)

############################################################################
##Exclusions##
############################################################################

#Subset days in the time window of interest
all_pat_data <- all_pat_data[all_pat_data$days_since_FUS >= dayMin & all_pat_data$days_since_FUS <= dayMax,]

#Remove minutes with less than minSamples
all_pat_data <- all_pat_data[all_pat_data$samples >= minSamples,]

############################################################################
##Subject labels##
############################################################################

#Separate by imbalance group
patients0 <- clinic_data$Beiwe.ID[clinic_data$Imbalance==0]
patients1 <- clinic_data$Beiwe.ID[clinic_data$Imbalance==1]
patients2 <- clinic_data$Beiwe.ID[clinic_data$Imbalance==2]

map0 <- data.frame(beiwe.id = patients0, paper.id = NA)
map0$paper.id <- paste("A", 1:length(patients0), sep = "")

map1 <- data.frame(beiwe.id = patients1, paper.id = NA)
map1$paper.id <- paste("B", 1:length(patients1), sep = "")

map2 <- data.frame(beiwe.id = patients2, paper.id = NA)
map2$paper.id <- paste("C", 1:length(patients2), sep = "")

map <- rbind(map0, map1, map2)

##Add shorter ID labels for paper
all_pat_data$paper.id <- NA 
for (i in 1:nrow(map)){
  pat <- map$beiwe.id[i]
  print(pat)
  indices <- which(all_pat_data$id == pat) 
  all_pat_data$paper.id[indices] <- map$paper.id[i]
}
all_pat_data$paper.id <- factor(all_pat_data$paper.id, levels = c(map0$paper.id,map1$paper.id,map2$paper.id))
all_pat_data$aiT30day <- factor(all_pat_data$aiT30day, levels = c(TRUE, FALSE))

############################################################################
##Plot AI minute distributions##
############################################################################

#Plot each patient's AI minutes over time

ggplot(all_pat_data, aes(x=days_since_FUS, y = time_of_day, color = aiT30day)) + 
  geom_point(size = 0.05) + 
  facet_wrap(~paper.id)+
  scale_color_manual(values = c("FALSE" = "grey", "TRUE" = "black"))+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),   
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  ) +
  geom_vline(xintercept = 0, color = "red") +
  labs(x = "Days since FUS", y="Hour of Day", color = "AI-T30 Day?")+
  scale_y_continuous(breaks = c(0, 6, 12, 18, 24))+
  scale_x_continuous(breaks = seq(from = -56, to = 56, by = 14)) +
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(paste(plotDir, "all_AIminutes_90thresh.pdf",sep = "/"), device = "pdf", width = 8.5, height = 8)
