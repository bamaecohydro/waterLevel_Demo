#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: waterLevel 
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 6/3/2020
#Purpose: Re-analysis of water level data downloaded March 2019
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Table of Contents~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Organize workspace
# Step 2: Field Worksheet
# Step 3: Baro Data
# Step 4: Water Depth
# Step 5: QAQC
# Step 6: Print

#Log Notes~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#AL-D has a weird shift. Was their a big storm. If not, then the shift is off...

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Setup workspace-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Clear workspace 
remove(list=ls())

#Gather libraries of interest
library(xts)
library(dygraphs)
library(lubridate)
library(tidyverse)

#Read custom R functions
source("R/download_fun.R")
source("R/check_fun.R")
source("R/dygraph_ts_fun.R")
source("R/dygraph_QAQC_fun.R")

#Define data directory
data_dir<-"data//"

#list pt and baor file locations
pt_files<-list.files(paste0(data_dir), full.names =  TRUE) %>% 
  as_tibble() %>% 
  filter(!str_detect(value, 'offset')) %>% 
  filter(!str_detect(value, 'well_log.csv')) %>% 
  as_vector()
baro_files<-pt_files[str_detect(pt_files, "Baro")]
field_files<-paste0(data_dir, 'well_log.csv')
offset<-read_csv(paste0(data_dir, 'offset.csv'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Field Worksheet--------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download Field Worksheet
field_log<-read_csv(field_files)

#Check to make sure pt files match field logs
check_fun(pt_files,field_log)
#If final 

#create df of site name, sonde_id, and measured offset
field_log<-field_log %>% 
  select(Site_Name, Sonde_ID, Relative_Water_Level_m)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Barometric Pressure Data----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Gather baro data
baro<-download_fun(baro_files) %>% 
  select(-download_date) %>% 
  left_join(., field_log) %>% 
  drop_na()
  
#Plot to check for any issues
baro %>% select(Timestamp, pressureAbsolute) %>%  dygraph_ts_fun()
  
#create interp functions
baro_fun<-approxfun(baro$Timestamp, baro$pressureAbsolute)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: WaterDepth Data-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Gather PT data
df<-lapply(pt_files, download_fun) %>% bind_rows()

#Joint to df
df<-df %>% left_join(., field_log) 

#Assign baro pressure
df<-df %>% 
  #Interpolate barometric pressure from loggers
  mutate(pressureBaro = baro_fun(Timestamp)) %>% 
  #Clean up
  select(Site_Name, Timestamp, pressureAbsolute, pressureBaro)

#Estimate waterHeight
df<-df %>% 
  mutate(pressureGauge = pressureAbsolute-pressureBaro, 
         waterHeight   = pressureGauge/9.81)

#Joint to df
df<-df %>% left_join(., offset) 

#Estimate waterdepth
df<-df %>% mutate(waterDepth = waterHeight + offset)

#Subset to waterDepth (after inspection!)
df<-df %>% select(Timestamp, Site_Name, waterDepth) 

#Add prcessing level
df$processing_level<-"raw"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 5: QAQC------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#list site names
field_log %>% select(Site_Name) %>% arrange(Site_Name) %>% pull()

# #5.1 BN-A-----------------------------------------------------------------------
# #Organize data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site<-"BN-A"
waterDepth <- df %>% filter(Site_Name==site)

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add offset to match historic
updated<-waterDepth %>%
  mutate(waterDepth = waterDepth - 0.098)

#remove weird periods
updated<-updated %>%
  filter(waterDepth > -1.5)

#Add rolling average
updated<-updated %>%
  mutate(waterDepth = rollmean(waterDepth, k=5, fill=NA))

#interpolate between weird periods
interpfun<-approxfun(updated$Timestamp, updated$waterDepth)
updated<-tibble(
  Timestamp = waterDepth$Timestamp,
  waterDepth = interpfun(waterDepth$Timestamp))

#Inspect output
dygraph_QAQC_fun(waterDepth, historic, updated)

#print~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add info to 'updated' dataframe
updated$Site_Name = site
updated$processing_level = 'processed'

#Append master dataframe
df<-bind_rows(df, updated)

# #5.2 BN-B-----------------------------------------------------------------------

# .... edit as you see fit


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 6: Export-----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df<-df %>%
  select(Timestamp, Site_Name, waterDepth, processing_level) %>%
  filter(processing_level == 'processed') %>%
  select(-processing_level)
write_csv(df, paste0(data_dir,"export.csv"))

