##8/2/25

##Plot AI-T30 time series for all patients
##Plot LOESS curve for patients with extensive AI-T30 collection

rm(list=ls()) 

############################################################################
##Libraries##
############################################################################

library(ggplot2)
library(gtools)

############################################################################
##Filepaths##
############################################################################

#Processed day-level AI-T30 results
aiT30_filepath <- "~/Dropbox/research/github/ET_tri_sec/output/processedDF"

#Directory for plots
plotDir <- "~/Dropbox/research/github/ET_tri_sec/output/figs"

############################################################################
##Inputs##
############################################################################

dayMin <- -56 ##8 weeks before FUS 
dayMax <- 56 ##8 weeks after FUS


############################################################################
##Read in AI-T30 output##
############################################################################

#Read in AI-T30 output from aiT30_filepath
aiT30 <- read.csv(paste(aiT30_filepath,"aiT30.csv",sep = "/"))

#Multiply AI-T30 by a factor of 60
aiT30$AI.T30 <- aiT30$AI.T30 * 60

############################################################################
##Restrictions##
############################################################################

#Narrow down to desired time frame using dayMin and dayMax
aiT30 <- aiT30[aiT30$days_since_FUS >= dayMin & aiT30$days_since_FUS <= dayMax,]
summary(aiT30$days_since_FUS)

#Highest observed AI-T30
maxAIT30 <- max(aiT30$AI.T30) #for plotting ranges

#Set a restriction that nmin >= nmin_threshold
nmin_threshold <- 90
summary(aiT30$nmin)
aiT30 <- aiT30[aiT30$nmin >= nmin_threshold,]
summary(aiT30$nmin)

#Vector of all patients in data frame
all_patients <- unique(aiT30$paper.id)
all_patients <- mixedsort(all_patients) #put patients in order using gtools package


############################################################################
##AI-T30 time series of all patients##
############################################################################

ggplot(aiT30, aes(x = days_since_FUS, y = AI.T30, color = nmin)) +  
  geom_point(size = 0.5) +    
  facet_wrap(~ paper.id, ncol = 5) +    
  theme_bw()+
  labs(x = "Days since FUS", y = "AI-T30") + 
  geom_vline(xintercept = 0, color = "black") +
  scale_color_gradientn(
    name = "Number of AI Minutes",
    colours = c("#feb24c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026"),
    values = scales::rescale(c(90, 360, 720, 1080, 1440)),
    limits = c(90, 1440),
    breaks = c(90, 360, 720, 1080, 1440)
  ) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  scale_x_continuous(breaks = seq(from = -56, to = 56, by = 14)) 

############################################################################
##Patients with extensive AI-T30 data collection##
############################################################################

#Patients with extensive AI-T30 collection
patientsHQ <- c("A3", "A6", "A10", "A13", 
                "B15", "C2", "C3", "C4")

#Data frame with those patients' data
HQdata <- aiT30[aiT30$paper.id %in% patientsHQ,]
HQdata$paper.id <- factor(HQdata$paper.id, levels = patientsHQ)


############################################################################
##LOESS for each patient with extensive AI-T30 data collection##
############################################################################

par(mfrow = c(3,3))

#LOESS separately for each patient 
for (patient in patientsHQ){
  #Patient name
  print(patient)
  
  #Subset patient's AI-T30 results 
  patient_data <- aiT30[aiT30$paper.id == patient,]
  
  #Plot patient's AI-T30 results 
  plot(patient_data$days_since_FUS, patient_data$AI.T30,
       main = paste("Patient", patient, sep = " "),
       xlab = "Days since FUS",
       ylab = "AI-T30 (g)", 
       xlim = c(dayMin, dayMax),
       ylim = c(0, maxAIT30),
       pch = 20,
       col = "grey")
  abline(v = 0, col = "black")
  
  #Fit LOESS curve, with different spans
  lo1 <- loess(AI.T30 ~ days_since_FUS, span = 0.75, family = "symmetric", data=patient_data)
  lines(patient_data$days_since_FUS,lo1$fitted,col="blue",lwd=1.5)
  
}

#Make the LOESS plots in ggplot

setwd(plotDir)

# Note: adjust y-axis limit based on maxAIT30

pdf(paste(plotDir, "loess_HQ.pdf", sep = "/"), width = 8, height = 5)
ggplot(HQdata, aes(x = days_since_FUS, y = AI.T30, color = nmin)) +
  geom_point(size = 0.5) +
  facet_wrap(~ paper.id, ncol = 4) +
  theme_bw()+
  labs(title = "Extensive AI-T30 Collection Post-MRgFUS",
       x = "Days since FUS", y = "AI-T30") +
  geom_vline(xintercept = 0, color = "black") +
  scale_color_gradientn(
    name = "Number of AI Minutes",
    colours = c("#feb24c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026"),
    values = scales::rescale(c(90, 360, 720, 1080, 1440)),
    limits = c(90, 1440),
    breaks = c(90, 360, 720, 1080, 1440)
  ) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_x_continuous(breaks = seq(from = -56, to = 56, by = 14)) +
  scale_y_continuous(breaks = seq(from = 0, to = 40, by = 10)) +
  geom_smooth(aes(group = paper.id), method = "loess", 
              method.args = list(family = "symmetric"),
              se = FALSE,
              formula = y ~ x,
              color = "blue", linetype = "solid", 
              span = 0.75, linewidth = 0.75)
dev.off()

############################################################################
##AI-T30 time series for the other patients##
############################################################################

other_patients1 <- c("A5","A12","B1","B3","B5","B6","B10","B16","B17","C7")
other_patients2 <- c("A4","A8","A9","A11","B2","B7","B9","B11","B13","B18")
length(other_patients1)
length(other_patients2)                                      

#Create data frame with patients who have moderate collection
modD <- aiT30[aiT30$paper.id %in% other_patients1,]
modD$paper.id <- factor(modD$paper.id,
                        levels = other_patients1)
unique(modD$paper.id)

#Create data frame with patients who have low collection
lowD <- aiT30[aiT30$paper.id %in% other_patients2,]
lowD$paper.id <- factor(lowD$paper.id,
                        levels = other_patients2)
unique(lowD$paper.id)                                        

#Plot time series for moderate collection
pdf(paste(plotDir, "AIT30_mod.pdf", sep = "/"), width = 7, height = 6)
ggplot(modD, aes(x = days_since_FUS, y = AI.T30, color = nmin)) +  
  geom_point(size = 0.5) +
  facet_wrap(~ paper.id, ncol = 4) +    
  theme_bw()+
  labs(x = "Days since FUS", y = "AI-T30") + 
  geom_vline(xintercept = 0, color = "black") +
  scale_color_gradientn(
    name = "Number of AI Minutes",
    colours = c("#feb24c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026"),
    values = scales::rescale(c(90, 360, 720, 1080, 1440)),
    limits = c(90, 1440),
    breaks = c(90, 360, 720, 1080, 1440)
  ) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
  scale_x_continuous(breaks = seq(from = -56, to = 56, by = 14)) +
  scale_y_continuous(breaks = seq(from = 0, to = 40, by = 10)) 
dev.off()

#Plot time series for low collection
pdf(paste(plotDir,"AIT30_low.pdf",sep="/"), width = 7, height = 6)
ggplot(lowD, aes(x = days_since_FUS, y = AI.T30, color = nmin)) +  
  geom_point(size = 0.5) +
  facet_wrap(~ paper.id, ncol = 4) +    
  theme_bw()+
  labs(x = "Days since FUS", y = "AI-T30") + 
  geom_vline(xintercept = 0, color = "black") +
  scale_color_gradientn(
    name = "Number of AI Minutes",
    colours = c("#feb24c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026"),
    values = scales::rescale(c(90, 360, 720, 1080, 1440)),
    limits = c(90, 1440),
    breaks = c(90, 360, 720, 1080, 1440)
  ) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
  scale_x_continuous(breaks = seq(from = -56, to = 56, by = 14)) +
  scale_y_continuous(breaks = seq(from = 0, to = 40, by = 10)) 
dev.off()

