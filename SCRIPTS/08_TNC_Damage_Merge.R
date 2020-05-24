#--------------------------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------------------------------------#
#This Script Computes Lagged Storm Exposure Variables For The Weekly TNC Panel Dataset. NOTE: This script uses parallelization to decrease
#computation time. I will note where this happens in case the user wants to decrease the number of cores. It might be preferable to run this
#script on a desktop or on the computer cluster---------------------
#--------------------------------------------------------------------------------------------------------------------------------------------#


#-----------------------Loading Libraries----------------#
library(tidyverse)
library(purrr)
library(furrr)
library(parallel)
library(readxl)
library(data.table)
library(lubridate)


#-----------------------------------Re-Define Function For the TNC Data Set Format----------------------------#

Compute_Lag_V2 = function(DSET, DAT, LAG_NEAR = 0, LAG_FAR, PLACE, TIME = "BEGIN"){
  DAT_FAR  = DAT - LAG_FAR
  DAT_NEAR = DAT - LAG_NEAR
  if(TIME == "BEGIN"){
    RES = sum(DSET$DAMAGE_PROPERTY[which(DSET$BEGIN_DATE >= DAT_FAR  & 
                                           DSET$BEGIN_DATE <= DAT_NEAR & 
                                           DSET$STATE_CODE == PLACE)], na.rm = TRUE)
  }
  else if (TIME == "END"){
    RES = sum(DSET$DAMAGE_PROPERTY[which(DSET$END_DATE >= DAT_FAR    & 
                                           DSET$END_DATE <= DAT_NEAR   & 
                                           DSET$STATE_CODE == PLACE)], na.rm = TRUE)
  }
  else if (TIME == "MID"){
    RES = sum(DSET$DAMAGE_PROPERTY[which(DSET$MID_DATE >= DAT_FAR      & 
                                           DSET$MID_DATE <= DAT_NEAR     & 
                                           DSET$STATE_CODE == PLACE)],   na.rm = TRUE)
  }
  return(RES)
}
#-------------------------------------Read in NOAA Data---------------------------------#
NOAA = fread("~/Google Drive/DATA/ECON/CLEAN/NOAA/02_NOAA_CLEANED.csv") %>%
  mutate(END_DATE = as.Date(END_DATE),
         BEGIN_DATE = as.Date(BEGIN_DATE))
#Filter to Relevant
NOAA_RELEVANT = NOAA %>%
  filter(YEAR > 2010 & DAMAGE_PROPERTY > 0) 
#------------------------------------Read in TNC state x week panel---------------------#
TNC_CLEAN_WEEK = read.csv("~/Google Drive/DATA/ECON/CLEAN/TNC_CLEAN_Week.csv")   %>%
  mutate(WEEK_LOW = as.Date(WEEK_LOW))

#---------------Defining Looping Arguments----------#
args = list(
  L1 = c(0,15,30,45,60,90,120),
  L2 = c(0,15,30,45,60,90,120)
)
crossedArgs = cross_df(args) %>%
  filter(L2 - L1 >0)


#------------------------------------------------Calculate Lagged Variables For All Storm Types--------------------------------------#
X = map2_dfc(.x = crossedArgs$L1, .y = crossedArgs$L2, 
             .f = ~ data.frame(as.numeric(unlist(mclapply(1:nrow(TNC_CLEAN_WEEK), 
                                                          function(x) Compute_Lag_V2(DSET = NOAA_RELEVANT, 
                                                                                     DAT = TNC_CLEAN_WEEK$WEEK_LOW[x], 
                                                                                     LAG_NEAR = .x, LAG_FAR = .y, 
                                                                                     PLACE = TNC_CLEAN_WEEK$state[x], TIME = "END"),
                                                          mc.cores = 6))))) #USE TO ADJUST NUMBER OF CORES
#Defining List of Variable Names
names = map2_chr(.x = crossedArgs$L1, .y = crossedArgs$L2,
                 ~ str_c("Lag",as.character(.x),as.character(.y),sep = "_"))
#Assigning Variable Names
colnames(X) = names

#Appending Columns to TNC Panel
TNC_CLEAN_WEEK = bind_cols(TNC_CLEAN_WEEK, X)

#-------------------------------------------Calculating Lagged Variables For "Cold" Storms-------------------------------------------#
#Filter NOAA Data to relevant storms
NOAA_RELEVANT = NOAA_RELEVANT %>%
  filter(PLACEBO_EVENT == 1)
#Perform Calculation
X_Placebo = map2_dfc(.x = crossedArgs$L1, .y = crossedArgs$L2, 
                     .f = ~ data.frame(as.numeric(unlist(mclapply(1:nrow(TNC_CLEAN_WEEK), 
                                                                  function(x) Compute_Lag_V2(DSET = NOAA_RELEVANT,
                                                                                             DAT = TNC_CLEAN_WEEK$WEEK_LOW[x], 
                                                                                             LAG_NEAR = .x, LAG_FAR = .y, 
                                                                                             PLACE = TNC_CLEAN_WEEK$state[x], TIME = "END"),
                                                                  mc.cores = 6))))) #USE TO ADJUST NUMBER OF CORES
#Define Variable Names
names = map2_chr(.x = crossedArgs$L1, .y = crossedArgs$L2,
                 ~ str_c("Lag_Placebo",as.character(.x),as.character(.y),sep = "_"))
#Assign Variable Names
colnames(X_Placebo) = names
#Append "Cold" Lagged Variables
TNC_CLEAN_WEEK = bind_cols(TNC_CLEAN_WEEK, X_Placebo)


#----------------------------------------Creating "Not Cold" Lagged Variables---------------------------------#
#Calculate By Subtracting Cold From Total
X = as.matrix(X)
X_Placebo = as.matrix(X_Placebo)
X_Not_Placebo = matrix(nrow = nrow(X),ncol = ncol(X))
for(i in 1:nrow(X)){
  for(j in 1:ncol(X)){
    X_Not_Placebo[i,j] = X[i,j] - X_Placebo[i,j]
  }
}
#Creating Variable Names
colnames(X_Not_Placebo) = str_replace(colnames(X_Placebo),pattern = "Placebo", replacement = "Not_Placebo")
#Assigning Column Names
X_Not_Placebo = as.data.frame(X_Not_Placebo)
#Appending "Non-Cold" Columns
TNC_CLEAN_WEEK = bind_cols(TNC_CLEAN_WEEK,X_Not_Placebo)

#Writing Merged (Lagged) DataSet
write.csv(TNC_CLEAN_WEEK,"~/Google Drive/DATA/ECON/CLEAN/TNC_Merged_Week.csv")
