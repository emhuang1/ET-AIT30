##1/26/26

##LMM to examine effect of time on AI-T30

rm(list=ls()) 

############################################################################
##Libraries##
############################################################################

library(gtools) #for mixedsort
library(lme4) #for lmer
library(lmerTest) #gives more detailed summary output


############################################################################
##Filepaths##
############################################################################

aiT30_filepath <- "~/Dropbox/research/github/ET_tri_sec/output/processedDF"

############################################################################
##Read in AI-T30 output##
############################################################################

#Read in AI-T30 output from aiT30_filepath
aiT30 <- read.csv(paste(aiT30_filepath,"aiT30.csv",sep = "/"))

#Multiply AI-T30 by a factor of 60
aiT30$AI.T30 <- aiT30$AI.T30 * 60

############################################################################
##Exclusions##
############################################################################

dayMin <- 0 ##the day of FUS 
dayMax <- 56 ##8 weeks after FUS

#Narrow down to desired time frame using dayMin and dayMax
aiT30 <- aiT30[aiT30$days_since_FUS >= dayMin & aiT30$days_since_FUS <= dayMax,]
summary(aiT30$days_since_FUS)
nrow(aiT30)

#At least nmin.thresh per day
nmin.thresh <- 90
aiT30 <- aiT30[aiT30$nmin >= nmin.thresh,]
summary(aiT30$nmin)

nrow(aiT30)

############################################################################
##Patients##
############################################################################

#Vector of all patients in data frame
all_patients <- unique(aiT30$paper.id)
all_patients <- mixedsort(all_patients) #uses gtools package
length(all_patients)

############################################################################
##Adjust certain variables to factors##
############################################################################

#Turn paper.id to a factor
aiT30$paper.id <- factor(aiT30$paper.id, 
                         levels = all_patients) 

#Turn imbalanceABC to a factor
aiT30$imbalanceABC <- factor(aiT30$imbalanceABC)

############################################################################
##LMM##
############################################################################

##################################################################
#Fitting a model with random intercept
##################################################################

mod <- lmer(AI.T30 ~ days_since_FUS + (1| paper.id), 
            data = aiT30)

summary(mod)

confint(mod)


