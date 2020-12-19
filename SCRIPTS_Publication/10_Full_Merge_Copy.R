#---------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------#
#This Script Adds Lagged TNC Donations to the Senate and House Vote Panel DataSets. NOTE: This script uses parallelization to 
#decrease computation time. Adjust #cores as is necessary
#---------------------------------------------------------------------------------------------------------------------------------#


#----------------Loading Required Libraries---------------------#
library(tidyverse)
library(useful)
library(parallel)
library(data.table)
library(lubridate)
library(dplyr)
library(stargazer)


#---------------------------------Importing Lagged Storm Damaged Data Sets In House and Senate-------------------------------------#
SENATE_DAT = fread("~/Google Drive/DATA/ECON/CLEAN/SENATE_LAGGED.csv")
HOUSE_DAT = fread("~/Google Drive/DATA/ECON/CLEAN/HOUSE_LAGGED.csv")

#-------------------------Creating Table of Means for Whole Timeframe-----------------------------#
library(dplyr)
#For Senate
SEN_DAT_ToM = SENATE_DAT %>%
  dplyr::select(POS_VOTE, Lag_0_30_END) %>%
  rename(Votes_Senate = POS_VOTE, Damage_Senate =Lag_0_30_END) %>%
  mutate(Damage_Senate = Damage_Senate/1000000) %>%
  filter(Votes_Senate != 'NA') %>%
  as.matrix()
#For House
H_DAT_ToM = HOUSE_DAT %>%
  dplyr::select(POS_VOTE, Lag_0_30_END) %>%
  rename(Votes_House = POS_VOTE, Damage_House = Lag_0_30_END) %>%
  mutate(Damage_House = Damage_House/1000000) %>%
  filter(Votes_House != 'NA') %>%
  as.matrix()
#Combining into one file
ALL_Years = matrix(nrow = nrow(H_DAT_ToM), ncol = 4)
head(ALL_Years)
ALL_Years[1:nrow(H_DAT_ToM),1:2] = H_DAT_ToM
ALL_Years[1:(nrow(SEN_DAT_ToM)),3:4] = SEN_DAT_ToM
colnames(ALL_Years) = c("Vote_House","Damage_House","Vote_Sen","Damage_Sen")
ALL_Years = as.data.frame(ALL_Years) 
#Outputting Results
ALL_Years = stargazer(ALL_Years, style = "aer",type = "latex",
                     summary.stat = c("n", "mean","min","max","sd"),
                     column.sep.width = "1",
                     title  = "Summary Statistics -- All LCV Years",
                     covariate.labels = c("House Vote", "30-Day Damage (House)", "Senate Vote", "30-Day Damage (Senate)"),
                     out = "~/Desktop/ECON Thesis/OUTPUT/DATA_Section/Table_of_Means_ALL.tex")

#-------------------------------------------------Performing Calculations-------------------------------------#
#Restricting Senate Data to Time Frame For Which I have TNC Data to Lag
SENATE_DAT_ALL = SENATE_DAT %>%
  dplyr::select(-contains("BEGIN")) %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(DATE > (min(TNC_CLEAN_DAY$GIFT_DATE) + 60) & DATE < (max(TNC_CLEAN_DAY$GIFT_DATE) - 60))  

#Import the TNC_Clean DataSet
TNC_CLEAN_DAY = read.csv("~/Google Drive/DATA/ECON/CLEAN/TNC_CLEAN_DAY.csv") %>%
  dplyr::select(GIFT_DATE,state,COUNT,AMT) %>%
  mutate(GIFT_DATE = as.Date(GIFT_DATE))

#-------------------Function to Construct Lagged Donations------------------------#
Compute_Lag_V3 = function(DSET, DAT, LAG_NEAR = 0, LAG_FAR, PLACE){
  DAT_FAR  = DAT - LAG_FAR
  DAT_NEAR = DAT - LAG_NEAR
  RES = sum(DSET$COUNT[which(DSET$GIFT_DATE >= DAT_FAR      & 
                             DSET$GIFT_DATE <= DAT_NEAR     & 
                             DSET$state == PLACE)],   na.rm = TRUE)
  return(RES)
}

#Defining Argument List
args = list(
  L1 = c(0,15,30,45,60),
  L2 = c(0,15,30,45,60)
)
crossedArgs = cross_df(args) %>%
  filter(L2 - L1 >0)

#Performing Calculation for Senate
X = map2_dfc(.x = crossedArgs$L1, .y = crossedArgs$L2, 
             .f = ~ data.frame(as.numeric(unlist(mclapply(1:nrow(SENATE_DAT_ALL), 
                                                          function(x) Compute_Lag_V3(DSET = TNC_CLEAN_DAY, 
                                                                                     DAT = SENATE_DAT_ALL$DATE[x], 
                                                                                     LAG_NEAR = .x, LAG_FAR = .y, 
                                                                                     PLACE = SENATE_DAT_ALL$State[x]), 
                                                          mc.cores = 6))))) #Use this to adjust number of cores
#Variable Names
names = map2_chr(.x = crossedArgs$L1, .y = crossedArgs$L2,
                 ~ str_c("TNC_Lag",as.character(.x),as.character(.y),sep = "_"))
#Assigning Variable Names
colnames(X) = names

#Appending Lagged Donations to Senate Data
SENATE_DAT_ALL = bind_cols(SENATE_DAT_ALL, X)
#Writing Fully Lagged DataSet
write.csv(SENATE_DAT_ALL, file = "~/Google Drive/DATA/ECON/CLEAN/FULL_LAGGED/Sen_All_Lags.csv")


#-------------------------------------------------Analogous Process for the House-----------------------------------#
#Filter To Relevant Time Frame
HOUSE_DAT_ALL = HOUSE_DAT %>%
  dplyr::select(-contains("BEGIN")) %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(DATE > (min(TNC_CLEAN_DAY$GIFT_DATE) + 60) & DATE < (max(TNC_CLEAN_DAY$GIFT_DATE) - 60))  #Need to Filter to Years that Have TNC_DAT

#Performing Calculation
X = map2_dfc(.x = crossedArgs$L1, .y = crossedArgs$L2, 
             .f = ~ data.frame(as.numeric(unlist(mclapply(1:nrow(HOUSE_DAT_ALL), 
                                                          function(x) Compute_Lag_V3(DSET = TNC_CLEAN_DAY, 
                                                                                     DAT = HOUSE_DAT_ALL$DATE[x], 
                                                                                     LAG_NEAR = .x, LAG_FAR = .y, 
                                                                                     PLACE = HOUSE_DAT_ALL$CODE[x]), 
                                                          mc.cores = 6))))) #Adjust cores as necessary
#Variable Names
names = map2_chr(.x = crossedArgs$L1, .y = crossedArgs$L2,
                 ~ str_c("TNC_Lag",as.character(.x),as.character(.y),sep = "_"))
#Assigning Variable Names
colnames(X) = names
#Appending Lagged Donations
HOUSE_DAT_ALL = bind_cols(HOUSE_DAT_ALL, X)
#Writing Full Result to New File
write.csv(HOUSE_DAT_ALL, file = "~/Google Drive/DATA/ECON/CLEAN/FULL_LAGGED/House_All_Lags.csv")



