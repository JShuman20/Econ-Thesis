#----------------------------------------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------------------------------#
#The Purpose of This Script is to Merge and Clean the Individual Storm Datasets Downloaded from NOAA-------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------------------------------#

#--------------------------Loading Required Libraries----------------#
library(tidyverse)
library(readxl)
library(writexl)
library(rvest)
library(xml2)
library(data.table)
library(zoo)
library(purrr)

#-----------------------Datasets with various geographic codes-------------------------#
STATE = read_xlsx("~/Google Drive/DATA/ECON/STATES.xlsx") 

STATE_REGION = read_xlsx("~/Google Drive/DATA/ECON/STATES_REGIONS.xlsx") %>%
  rename(STATE = `State Code`) %>%
  left_join(STATE, by = "STATE") %>%
  rename(STATE_CODE = STATE,
         STATE = `State Name`) %>%
  mutate(STATE = toupper(STATE))


#------------------------Processing NOAA Files-------------------------------------#
#List of All Files in Directory
STORM_FILES  = list.files("~/Google Drive/DATA/ECON/RAW/NOAA_Storms/")
#Reading in All Data Files
DAT = map_dfr(.x = STORM_FILES,
              .f = ~ fread(str_c("~/Google Drive/DATA/ECON/RAW/NOAA_Storms/",.x)) %>%
                mutate(DAMAGE_CROPS = as.character(DAMAGE_CROPS)))
#Writing a File With all Merged, but no cleaning
write_csv(DAT,"~/Google Drive/DATA/ECON/CLEAN/NOAA/01_NOAA_MERGED.csv")


#-----------------------------Cleaning--------------------------------------------#
DAT_CLEAN = DAT %>%
  dplyr::select(-c(ends_with("NARRATIVE"))) %>%
  mutate(EVENT_TYPE = toupper(EVENT_TYPE),
         DAMAGE_PROPERTY = as.character(DAMAGE_PROPERTY),
         DAMAGE_PROPERTY = case_when(
             str_detect(DAMAGE_PROPERTY, "M") ~ as.numeric(str_extract(DAMAGE_PROPERTY, pattern = "[[:digit:]]{0,4}[.]{0,1}[[:digit:]]{0,4}"))*1000000,
             str_detect(DAMAGE_PROPERTY, "B") ~ as.numeric(str_extract(DAMAGE_PROPERTY, pattern = "[[:digit:]]{0,4}[.]{0,1}[[:digit:]]{0,4}"))*1000000000,
             str_detect(DAMAGE_PROPERTY, "K") ~ as.numeric(str_extract(DAMAGE_PROPERTY, pattern = "[[:digit:]]{0,4}[.]{0,1}[[:digit:]]{0,4}"))*1000,
             str_detect(DAMAGE_PROPERTY, "0") ~ as.numeric(str_extract(DAMAGE_PROPERTY, pattern = "[[:digit:]]{0,4}[.]{0,1}[[:digit:]]{0,4}"))*0),
         DAMAGE_PROPERTY = as.numeric(DAMAGE_PROPERTY), #Numeric Variables for Property Damage
         BEGIN_YEARMONTH = as.character(BEGIN_YEARMONTH),
         YEAR = str_sub(BEGIN_YEARMONTH, 1,4),
         MONTH = str_sub(BEGIN_YEARMONTH,5,6),
         BEGIN_DATE = as.Date(str_c(str_sub(BEGIN_YEARMONTH,1,4), str_sub(BEGIN_YEARMONTH,5,6),BEGIN_DAY, sep = "-")),
         BEGIN_DATE_YM = (str_c(str_sub(BEGIN_YEARMONTH,1,4), str_sub(BEGIN_YEARMONTH,5,6), sep = "-")),
         BEGIN_DATE_YM = as.yearmon(BEGIN_DATE_YM),
         END_DATE = as.Date(str_c(str_sub(END_YEARMONTH,1,4), str_sub(END_YEARMONTH,5,6), END_DAY, sep = "-")),
         MID_DATE = BEGIN_DATE + floor(as.numeric(END_DATE - BEGIN_DATE)/2)) #All Versions of Time Variables (Was not clear which I would use initially)

#Filtering to US States Only
DAT_CLEAN = DAT_CLEAN %>%
  left_join(STATE_REGION, by = "STATE") %>%
  filter(!is.na(Region))

#Adding Variables For Placebo ("Cold") Group
PLACEBO_EVENTS = c("WINTER STORM", "HEAVY SNOW","WINTER WEATHER","COLD/WIND CHILL",
                   "BLIZZARD", "EXTREME COLD/WIND CHILL","FROST/FREEZE","ICE STORM",
                   "LAKE-EFFECT SNOW","FREEZING FOG","HAIL FLOODING", "HAIL/ICY ROADS", "HIGH SNOW")
DAT_CLEAN = DAT_CLEAN %>%
  dplyr::select(c(1:16,18,20:26,28,30,33,44:58)) %>%
  mutate(PLACEBO_EVENT = ifelse(EVENT_TYPE %in% PLACEBO_EVENTS, 1,0))
  

#------------------------Writing Cleaned File-------------------------------------#
write_csv(DAT_CLEAN, "~/Google Drive/DATA/ECON/CLEAN/NOAA/02_NOAA_CLEANED.csv")

