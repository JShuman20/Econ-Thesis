#--------------------------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------------------------------------#
#The Script Constructs a Series of Lagged Storm Damage Variables For Each LCV Vote Date. NOTE: This script uses parallelization to decrease
#computation time. I will note when this occurs in case the user wants to decrease the number of cores uses for this process. If possible,
#it may be preferable to run this script on the computer cluster or some a desktop with a bunch of processors
#--------------------------------------------------------------------------------------------------------------------------------------------#

#-------------------------Loading Libraries-----------------------#
library(tidyverse)
library(purrr)
library(furrr)
library(parallel)


#----------------------------Read in Cleaned Senate Data and NOAA Data----------------------------------#
ALL_LCV_SENATE_MERGE = read.csv("~/Google Drive/DATA/ECON/CLEAN/LCV/SENATE.csv")
DAT_CLEAN = read.csv("~/Google Drive/DATA/ECON/CLEAN/NOAA/02_NOAA_CLEANED.csv") %>%
     mutate(BEGIN_DATE = as.Date(BEGIN_DATE),
            END_DATE   = as.Date(END_DATE),
            MID_DATE   = as.Date(MID_DATE)) #Convert to Date Format

#----------------For Quicker Computation -- Extract Unique Votes, Merge Lagged Damages, and then group back------------------------#
#Unique Observations Will Only Occur By State x Vote Date
Distinct_Senate_Votes = ALL_LCV_SENATE_MERGE %>%
  distinct(DATE, Policy, State) %>% # Add Policy in case more than 1 vote on given day
  mutate(DATE = as.Date(DATE))
#----------------------Filtering to Nonzero rows of Disaster data for faster processing------------------------#
DAT_CLEAN_NOZEROS = DAT_CLEAN %>% 
  filter(DAM_PROP_2000 > 0) 
#--------------------Filtering Data For Only "Cold" Events----------------------------------#
DAT_CLEAN_PLACEBO = filter(DAT_CLEAN_NOZEROS, PLACEBO_EVENT == 1)

#--------------------------------------Function to sum damages over a time lag prior to a specified date-----------------------#
#Note: This Function allows the user to consder the start, end, or mid-point of a storm entry
Compute_Lag_V2 = function(DSET, DAT, LAG_NEAR = 0, LAG_FAR, PLACE, TIME = "BEGIN"){
  DAT_FAR  = DAT - LAG_FAR
  DAT_NEAR = DAT - LAG_NEAR
  if(TIME == "BEGIN"){
    RES = sum(DSET$DAM_PROP_2000[which(DSET$BEGIN_DATE >= DAT_FAR  & 
                                         DSET$BEGIN_DATE <= DAT_NEAR & 
                                         DSET$STATE_CODE == PLACE)], na.rm = TRUE)
  }
  else if (TIME == "END"){
    RES = sum(DSET$DAM_PROP_2000[which(DSET$END_DATE >= DAT_FAR    & 
                                         DSET$END_DATE <= DAT_NEAR   & 
                                         DSET$STATE_CODE == PLACE)], na.rm = TRUE)
  }
  else if (TIME == "MID"){
    RES = sum(DSET$DAM_PROP_2000[which(DSET$MID_DATE >= DAT_FAR      & 
                                         DSET$MID_DATE <= DAT_NEAR     & 
                                         DSET$STATE_CODE == PLACE)],   na.rm = TRUE)
  }
  return(RES)
}

#-------------------Defining Arguments to Loop Over to Construct Different Lag Variables-------------------#
args = list(
  L1 = c(0,15,30,45,60,90),
  L2 = c(0,15,30,45,60,90)
)
crossedArgs = cross_df(args) %>%
  filter(L2 - L1  >0)


#---------------------------------Creating Lagged Damage Variables---------------------------------------------------#
#Defining List of Column Names
names = map2_chr(.x = crossedArgs$L1, .y = crossedArgs$L2,
                 .f = ~ str_c("Lag",as.character(.x),as.character(.y),"END",sep = "_"))


#Performing the Calculations -- THIS TAKES A LITTLE WHILE DEPENDING ON COMPUTER
RES = map2_dfc(.x = crossedArgs$L1, .y = crossedArgs$L2,
              .f = ~ data.frame(as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                                             function(x) Compute_Lag_V2(DSET = DAT_CLEAN_NOZEROS, 
                                                                        DAT = Distinct_Senate_Votes$DATE[x], 
                                                                        LAG_NEAR = .x, LAG_FAR = .y, 
                                                                        PLACE = Distinct_Senate_Votes$State[x], TIME = "END"),
                                             mc.cores = 5)))))  #CHANGE THIS ARGUMENT TO USE FEWER CORES
#Assigning the Corresponding Column Names
colnames(RES) = names

#-----------------------------Creating Lagged Damage Variables For "Cold" Events--------------------------------------#
#Defining List of Column Names
names = map2_chr(.x = crossedArgs$L1, .y = crossedArgs$L2,
                 .f = ~ str_c("Lag",as.character(.x),as.character(.y),"END","PLACEBO" ,sep = "_"))
#Performing Calculation --- THIS TAKES A LITTLE WHILE
RES_PLACEBO = map2_dfc(.x = crossedArgs$L1, .y = crossedArgs$L2, 
                       .f = ~ data.frame(as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes),
                                             function(x) Compute_Lag_V2(DSET = DAT_CLEAN_PLACEBO, 
                                                                        DAT = Distinct_Senate_Votes$DATE[x], 
                                                                        LAG_NEAR = .x, LAG_FAR = .y,
                                                                        PLACE = Distinct_Senate_Votes$State[x], TIME = "END"), 
                                             mc.cores = 5))))) #CHANGE THIS ARGUMENT TO USE FEWER CORES
#Assigning the Corresponding Column Names
colnames(RES_PLACEBO) = names
#-------------------------------Creating Lagged Damage Variables For "Non-Cold" Events--------------------------------#
#Not-Placebo is Completely Determined (Difference between full and "cold" results)
#Creating Names
names = map2_chr(.x = crossedArgs$L1, .y = crossedArgs$L2,
                 .f = ~ str_c("Lag",as.character(.x),as.character(.y),"END","NOT_PLACEBO" ,sep = "_"))
#Subtract Cold From Full Variables
RES_NOT_PLACEBO = map_dfc(.x = 1:nrow(crossedArgs), .f = ~  RES[,.x] - RES_PLACEBO[,.x])
#Assign Column Names
colnames(RES_NOT_PLACEBO) = names 

#--------------------------------Binding this DataFrame to Distinct Senate Votes--------------------------------------#
Distinct_Senate_Votes = Distinct_Senate_Votes %>%
   bind_cols(RES,RES_PLACEBO,RES_NOT_PLACEBO) %>%
   mutate(DATE = as.factor(DATE))



#Merge Back Onto Full Senate Dataset
ALL_LCV_SENATE_MERGE = ALL_LCV_SENATE_MERGE %>%
  left_join(Distinct_Senate_Votes, by = c("DATE", "Policy", "State"))

#Writing Merged (Lagged) DataSet
write.csv(ALL_LCV_SENATE_MERGE, "~/Google Drive/DATA/ECON/CLEAN/SENATE_LAGGED.csv")


#-------------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------The Code Below Implements the Same Process For the House of Representatives-------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------------------#

#---------------------------------Now Doing Same Merges for the House of Representatives-----------------------------#
ALL_LCV_HOUSE_MERGE = read.csv("~/Google Drive/DATA/ECON/CLEAN/LCV/HOUSE.csv")
#Finding Distinct State x Date Observations
Distinct_HOUSE_Votes = ALL_LCV_HOUSE_MERGE %>%
  distinct(DATE, Policy, CODE) %>%
  mutate(DATE = as.Date(DATE))

#Defining Argument List
args = list(
  L1 = c(0,15,30,45,60,90),
  L2 = c(0,15,30,45,60,90)
)
crossedArgs = cross_df(args) %>%
  filter(L2 - L1 >0)

#-----------------------------------------Calculating Lagged Damage Variables for All Storm Types--------------------------------#
#Defining Column Names
names = map2_chr(.x = crossedArgs$L1, .y = crossedArgs$L2,
                 .f = ~ str_c("Lag",as.character(.x),as.character(.y),"END",sep = "_"))
#Performing Calculations -- THIS TAKES A WHILE!!!
RES = map2_dfc(.x = crossedArgs$L1, .y = crossedArgs$L2,
              .f = ~ data.frame(as.numeric(unlist(mclapply(1:nrow(Distinct_HOUSE_Votes),
                                             function(x) Compute_Lag_V2(DSET = DAT_CLEAN_NOZEROS, 
                                                                        DAT = Distinct_HOUSE_Votes$DATE[x], 
                                                                        LAG_NEAR = .x, LAG_FAR = .y,
                                                                       PLACE = Distinct_HOUSE_Votes$CODE[x], TIME = "END"),
                                             mc.cores = 6))))) #USE THIS TO CHANGE NUMBER OF CORES
#Assigning Column Names
colnames(RES) = names

#----------------------------------------Calculating Lagged Damage Variables For "Cold" Storm Type--------------------------------#
#Defining Column Names
names = map2_chr(.x = crossedArgs$L1, .y = crossedArgs$L2,
                 .f = ~ str_c("Lag",as.character(.x),as.character(.y),"END","PLACEBO", sep = "_"))
#Performing Calculation --- THIS TAKES A WHILE
RES_PLACEBO  = map2_dfc(.x = crossedArgs$L1, .y = crossedArgs$L2, 
              .f = ~ data.frame(as.numeric(unlist(mclapply(1:nrow(Distinct_HOUSE_Votes),
                                             function(x) Compute_Lag_V2(DSET = DAT_CLEAN_PLACEBO, 
                                                                        DAT = Distinct_HOUSE_Votes$DATE[x], 
                                                                        LAG_NEAR = ..1, LAG_FAR = ..2,
                                                                       PLACE = Distinct_HOUSE_Votes$CODE[x], TIME = "END"), 
                                             mc.cores = 6))))) #USE TO CHANGE NUMBER OF CORES
#Assigning Column Names
colnames(RES_PLACEBO) = names

#----------------------------------Calculating Lagged Damage Variables For "Non-Cold"-------------------------#
#Defining Variable Names
names = map2_chr(.x = crossedArgs$L1, .y = crossedArgs$L2, 
                 ~ str_c("Lag",as.character(.x),as.character(.y),"END","NOT_PLACEBO" ,sep = "_"))
#Defined By Subtracting Cold From Total Lagged Variables
RES_NOT_PLACEBO = map_dfc(.x = 1:nrow(crossedArgs), .f = ~  RES[,.x] - RES_PLACEBO[,.x])
#Assigning Column Names
colnames(RES_NOT_PLACEBO) = names

#--------------------------------------------------Merging Lags Back Onto House Voting Data----------------------#
Distinct_HOUSE_Votes = Distinct_HOUSE_Votes %>%
   bind_cols(RES,RES_PLACEBO,RES_NOT_PLACEBO) %>%
   mutate(DATE = as.factor(DATE))

ALL_LCV_HOUSE_MERGE = ALL_LCV_HOUSE_MERGE %>%
  left_join(Distinct_HOUSE_Votes, by = c("DATE", "Policy", "CODE"))

#Writing Merged (Lagged) Data File
write.csv(ALL_LCV_HOUSE_MERGE, "~/Google Drive/DATA/ECON/CLEAN/HOUSE_LAGGED.csv")




#Adding a Robustness Check -- Does National Damage Predict the Prob of a vote?
library(lubridate)
DAT_init = as_date("1990-01-01")
ALL_DATES = map_dfr(.x = 0:12000, .f = ~ data.frame(DATE = DAT_init + days(.x))) %>% filter(DATE < "2020-01-01")
max(ALL_DATES$DATE)
#For House
Distinct_HOUSE_Votes = distinct(ALL_LCV_HOUSE_MERGE, DATE) %>% mutate(DATE = as_date(DATE))
Distinct_SENATE_Votes = distinct(ALL_LCV_SENATE_MERGE, DATE) %>% mutate(DATE = as_date(DATE))
#Create National Series of Dates
Test_Dates_House = ALL_DATES %>%
  mutate(VOTE_OCCURED_House  = ifelse(DATE %in% Distinct_HOUSE_Votes$DATE, 1,0),
         VOTE_OCCURED_Senate = ifelse(DATE %in% Distinct_SENATE_Votes$DATE, 1,0),
         DATE = floor_date(DATE, unit = "week")) %>%
  group_by(DATE) %>%
  summarize(VOTES_HOUSE  = sum(VOTE_OCCURED_House),
            VOTES_SENATE = sum(VOTE_OCCURED_Senate)) %>%
  ungroup() %>%
  mutate(OCCURED_HOUSE  = ifelse(VOTES_HOUSE>0,1,0),
         OCCURED_SENATE = ifelse(VOTES_SENATE>0,1,0))

head(Test_Dates_House)
nrow(Test_Dates_House)

DAM_LAGGED = unlist(mclapply(X = 1:nrow(Test_Dates_House), FUN = function(X){
  sum(DAT_CLEAN_NOZEROS$DAMAGE_PROPERTY[which(DAT_CLEAN_NOZEROS$END_DATE<Test_Dates_House$DATE[X] & 
                                                   DAT_CLEAN_NOZEROS$END_DATE >= (Test_Dates_House$DATE[X]- days(30)))])
  
}, mc.cores = 4))

Test_Dates_House = Test_Dates_House %>%
  mutate(National_Dam_30 = DAM_LAGGED,
         Log_Dam = log(National_Dam_30 +1),
         YEAR = year(DATE),
         MON = month(DATE))
head(Test_Dates_House)

Test_Dates_House = Test_Dates_House %>%
  mutate(MONTH = as.character(MON),
         DAM_BIN = case_when(
           National_Dam_30 <= quantile(National_Dam_30,0.25) ~ "Bottom",
           National_Dam_30 > quantile(National_Dam_30,0.25) & National_Dam_30 <= quantile(National_Dam_30,0.75) ~ "Mid",
           National_Dam_30 > quantile(National_Dam_30,0.75) ~ "High"
         ),
         SEASON = case_when(
           MONTH %in% 1:3 ~ "Winter",
           MONTH %in% 4:6 ~ "Spring",
           MONTH %in% 7:9 ~ "Summer",
           MONTH %in% 10:12 ~ "Winter"
         ))

head(Test_Dates_House)
summary(glm(OCCURED_SENATE ~ DAM_BIN, data = Test_Dates_House))


