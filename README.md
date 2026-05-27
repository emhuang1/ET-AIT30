# ET-AIT30

This Github repository corresponds to the following manuscript: 
“Using Smartphone Accelerometer Data to Measure Recovery of Essential Tremor Patients after MRI-guided Focused Ultrasound Thalamotomy ” by Huang et al.

The R code performs processing and analysis of the smartphone accelerometer data from the ET-MRgFUS study. The code is organized into six steps, outlined below.
 
Step 1: Process Clinic Data
“clinic_data.R”: Calculate sample sizes and the descriptive statistics for age and sex across all study participants and by imbalance group.

Step 2: Calculate AI-T30
“code_tri_sec.R”: For each patient, compute the second-level AI using his/her raw tri-axial smartphone accelerometer data, then uses those results to determine the minute-level AI and the day-level AI-T30. Note that the AI-T30 that is produced by this script is unscaled (i.e., it has not yet been multiplied by 60).

Step 3: AI Minute 
“ai_plots.R”: Plot occurrence of AI minutes over the course of each day throughout the window of interest (i.e., Day -56 through Day 56), separately for each participant.

Step 4: Data Quality
“dataQuality.R”: Calculate summary statistics for NDY, NDY-Pre, NDY-Post, FDY, LDY, and MPD across all participants and by imbalance group.

Step 5: AI-T30 Time Series
“aiT30_timeSeries.R”: Plot the AI-T30 time series for all participants, and calculate and plot the LOESS curve for participants with extensive AI-T30 collection post-MRgFUS.

Step 6: LMM
“aiT30_lmm.R”: Fit linear mixed model to examine effect of time on AI-T30.
