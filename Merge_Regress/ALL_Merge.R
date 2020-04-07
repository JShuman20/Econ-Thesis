library(tidyverse)
library(useful)
library(parallel)
library(data.table)
library(lubridate)
library(dplyr)
library(stargazer)


#First, Import Storm on Vote Data
SENATE_DAT = fread("~/Google Drive/DATA/ECON/CLEAN/SENATE_LAGGED.csv")
HOUSE_DAT = fread("~/Google Drive/DATA/ECON/CLEAN/HOUSE_LAGGED.csv")

#Creating Table of Means for Whole Timeframe
library(dplyr)
SEN_DAT_ToM = SENATE_DAT %>%
  dplyr::select(POS_VOTE, Lag_0_30_END) %>%
  rename(Votes_Senate = POS_VOTE, Damage_Senate =Lag_0_30_END) %>%
  filter(Votes_Senate != 'NA') %>%
  as.matrix()
H_DAT_ToM = HOUSE_DAT %>%
  dplyr::select(POS_VOTE, Lag_0_30_END) %>%
  rename(Votes_House = POS_VOTE, Damage_House =Lag_0_30_END) %>%
  filter(Votes_House != 'NA') %>%
  as.matrix()
ALL_Years = matrix(nrow = nrow(H_DAT_ToM), ncol = 4)
ALL_Years[,1:2] = H_DAT_ToM
ALL_Years[1:nrow(SEN_DAT_ToM),3:4] = SEN_DAT_ToM
colnames(ALL_Years) = c("Vote_House","Damage_House","Vote_Sen","Damage_Sen")
ALL_Years = as.data.frame(ALL_Years) 
ALL_Years = stargazer(ALL_Years, style = "aer",type = "latex",
                     summary.stat = c("n", "mean","min","max","sd"),
                     column.sep.width = "1",
                     title  = "Summary Statistics -- All LCV Years",
                     covariate.labels = c("House Vote", "30-Day Damage (House)", "Senate Vote", "30-Day Damage (Senate)"),
                     out = "~/Desktop/ECON Thesis/OUTPUT/DATA_Section/Table_of_Means_ALL.tex")


SENATE_DAT_ALL = SENATE_DAT %>%
  dplyr::select(-contains("BEGIN")) %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(DATE > (min(TNC_CLEAN_DAY$GIFT_DATE) + 60) & DATE < (max(TNC_CLEAN_DAY$GIFT_DATE) - 60))  #Need to Filter to Years that Have TNC_DAT


#Now Import the TNC_Clean DataSet
TNC_CLEAN_DAY = read.csv("~/Google Drive/DATA/ECON/CLEAN/TNC_Merged_Day.csv") %>%
  select(GIFT_DATE,state,COUNT,AMT) %>%
  mutate(GIFT_DATE = as.Date(GIFT_DATE))

Compute_Lag_V3 = function(DSET, DAT, LAG_NEAR = 0, LAG_FAR, PLACE){
  DAT_FAR  = DAT - LAG_FAR
  DAT_NEAR = DAT - LAG_NEAR
  RES = sum(DSET$COUNT[which(DSET$GIFT_DATE >= DAT_FAR      & 
                             DSET$GIFT_DATE <= DAT_NEAR     & 
                             DSET$state == PLACE)],   na.rm = TRUE)
  return(RES)
}


args = list(
  L1 = c(0,15,30,45,60),
  L2 = c(0,15,30,45,60)
)
crossedArgs = cross_df(args) %>%
  filter(L2 - L1 >0)

#Code Set Up for Lags
X = map2_dfc(.x = crossedArgs$L1, .y = crossedArgs$L2, 
             .f = ~ data.frame(as.numeric(unlist(mclapply(1:nrow(SENATE_DAT_ALL), 
                                                          function(x) Compute_Lag_V3(DSET = TNC_CLEAN_DAY, DAT = SENATE_DAT_ALL$DATE[x], LAG_NEAR = .x, LAG_FAR = .y, 
                                                                                     PLACE = SENATE_DAT_ALL$State[x]), mc.cores = 6)))))
names = map2_chr(.x = crossedArgs$L1, .y = crossedArgs$L2,
                 ~ str_c("TNC_Lag",as.character(.x),as.character(.y),sep = "_"))
colnames(X) = names

SENATE_DAT_ALL = bind_cols(SENATE_DAT_ALL, X)


HOUSE_DAT_ALL = HOUSE_DAT %>%
  dplyr::select(-contains("BEGIN")) %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(DATE > (min(TNC_CLEAN_DAY$GIFT_DATE) + 60) & DATE < (max(TNC_CLEAN_DAY$GIFT_DATE) - 60))  #Need to Filter to Years that Have TNC_DAT

X = map2_dfc(.x = crossedArgs$L1, .y = crossedArgs$L2, 
             .f = ~ data.frame(as.numeric(unlist(mclapply(1:nrow(HOUSE_DAT_ALL), 
                                                          function(x) Compute_Lag_V3(DSET = TNC_CLEAN_DAY, DAT = HOUSE_DAT_ALL$DATE[x], LAG_NEAR = .x, LAG_FAR = .y, 
                                                                                     PLACE = HOUSE_DAT_ALL$CODE[x]), mc.cores = 6)))))
names = map2_chr(.x = crossedArgs$L1, .y = crossedArgs$L2,
                 ~ str_c("TNC_Lag",as.character(.x),as.character(.y),sep = "_"))
colnames(X) = names
HOUSE_DAT_ALL = bind_cols(HOUSE_DAT_ALL, X)
glimpse(HOUSE_DAT_ALL)


#Last Panel For Table of Means
H_ToM = HOUSE_DAT_ALL %>%
  dplyr::select(POS_VOTE, TNC_Lag_0_30, Lag_0_30_END) %>%
  filter(!is.na(POS_VOTE)) %>%
  as.matrix()
S_ToM = SENATE_DAT_ALL %>%
  dplyr::select(POS_VOTE, TNC_Lag_0_30, Lag_0_30_END) %>%
  filter(!is.na(POS_VOTE)) %>%
  as.matrix()

ALL = matrix(nrow = nrow(H_ToM), ncol = 6)
ALL[,1:3] = H_ToM
ALL[1:nrow(S_ToM),4:6] = S_ToM

ALL = as.data.frame(ALL) %>%
  stargazer(., type = "latex",style = "aer", 
            title = "Summary Stats for Restricted Date Range",
            summary.stat = c("n", "mean","min","max","sd"),
            covariate.labels = c("House Vote", "30-Day Lagged TNC (House)", "30-Day Lagged Damage House","Senate Vote", "30-Day Lagged TNC (Senate)", "30-Day Lagged Damage (Senate)"),
            out = "~/Desktop/ECON Thesis/OUTPUT/DATA_Section/Table_of_Means.tex")

SENATE_DAT_ALL = SENATE_DAT_ALL %>%
  mutate_at(vars(contains("Lag_")), ~ log(.+1)) %>%
  mutate_at(vars(contains("Lag_")), .funs = list(BIN = ~ifelse(.>0,1,0))) %>%
  mutate(BIN_30_Mil = ifelse( (Lag_0_30_END > 0 & Lag_0_30_END < log(1000000)),1,0),
         BIN_30_G_MIL = ifelse(Lag_0_30_END > log(1000000),1,0),
         BIN_30_FiftyMil = ifelse( (Lag_0_30_END > log(1000000) & Lag_0_30_END < log(50000000)), 1,0),
         BIN_30_G_FiftyMil = ifelse(Lag_0_30_END >log(50000000),1,0),
         BIN_30_FHMil = ifelse( (Lag_0_30_END > log(50000000) & Lag_0_30_END < log(400000000)), 1,0),
         BIN_30_Huge = ifelse(Lag_0_30_END > log(400000000), 1,0)) %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(SEASON = case_when(
    MON_NUM %in% c(12,1,2,3) ~ "WINTER",
    MON_NUM %in% c(4,5,6) ~ "SPRING",
    MON_NUM %in% c(7,8,9) ~ "SUMMER",
    MON_NUM %in% c(10,11,12) ~ "FALL")) %>%
  mutate(PANEL_VAR = 100000*Year + ROLL_CALL)  %>%
  mutate(REP = ifelse(Party == "R",1,0))


HOUSE_DAT_ALL = HOUSE_DAT_ALL %>%
  mutate_at(vars(starts_with("Lag_")), ~ log(.+1)) %>%
  mutate_at(vars(contains("Lag_")), .funs = list(BIN = ~ifelse(.>0,1,0))) %>%
  mutate(BIN_30_Mil = ifelse( (Lag_0_30_END > 0 & Lag_0_30_END < log(1000000)),1,0),
         BIN_30_G_MIL = ifelse(Lag_0_30_END > log(1000000),1,0),
         BIN_30_FiftyMil = ifelse( (Lag_0_30_END > log(1000000) & Lag_0_30_END < log(50000000)), 1,0),
         BIN_30_G_FiftyMil = ifelse(Lag_0_30_END >log(50000000),1,0),
         BIN_30_FHMil = ifelse( (Lag_0_30_END > log(50000000) & Lag_0_30_END < log(400000000)), 1,0),
         BIN_30_Huge = ifelse(Lag_0_30_END > log(400000000), 1,0)) %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(SEASON = case_when(
    MON_NUM %in% c(12,1,2,3) ~ "WINTER",
    MON_NUM %in% c(4,5,6) ~ "SPRING",
    MON_NUM %in% c(7,8,9) ~ "SUMMER",
    MON_NUM %in% c(10,11,12) ~ "FALL")) %>%
  mutate(PANEL_VAR = 100000*Year + ROLL_CALL) %>%
  mutate(Member.of.Congress = case_when(
    Member.of.Congress == "Rogers, Mike"  & State == "AL" ~ "Rogers, Mike_AL",
    Member.of.Congress == "Brown, George" & State == "CO" ~ "Brown, George_CO",
    TRUE ~ Member.of.Congress)) %>% #This is an Extra Catch -- didnt seem to get caught the first time
  mutate(REP = ifelse(Party == "R",1,0))




#-------------------------------------------Now Try Running These Regressions-------------------------------#

T1 = plm(POS_VOTE ~ Lag_0_30_END + TNC_Lag_0_30+ factor(Year) + factor(MON) , data = HOUSE_DAT_ALL, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
summary(T1)
