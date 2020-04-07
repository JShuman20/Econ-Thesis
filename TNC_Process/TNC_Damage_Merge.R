library(tidyverse)
library(purrr)
library(furrr)
library(parallel)
library(readxl)
library(data.table)
library(lubridate)
library(USAboundaries)
library(ggmap)
library(sp)
library(sf)
library(zoo)
library(plm)
library(sandwich)
library(lmtest)
library(readxl)
library(stargazer)

#Read in Data Sets

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
#Read in NOAA Data
NOAA = fread("~/Google Drive/DATA/ECON/CLEAN/NOAA/02_NOAA_CLEANED.csv") %>%
  mutate(END_DATE = as.Date(END_DATE),
         BEGIN_DATE = as.Date(BEGIN_DATE))
#Filter to Relevant
NOAA_RELEVANT = NOAA %>%
  filter(YEAR > 2010 & DAMAGE_PROPERTY > 0) 

TNC_CLEAN_WEEK = read.csv("~/Google Drive/DATA/ECON/CLEAN/TNC_CLEAN_Week.csv")   %>%
  mutate(WEEK_LOW = as.Date(WEEK_LOW))

#Defining Looping Arguments
args = list(
  L1 = c(0,15,30,45,60,90),
  L2 = c(0,15,30,45,60,90)
)
crossedArgs = cross_df(args) %>%
  filter(L2 - L1 >0)


#Code Set Up for Lags
X = map2_dfc(.x = crossedArgs$L1, .y = crossedArgs$L2, 
             .f = ~ data.frame(as.numeric(unlist(mclapply(1:nrow(TNC_CLEAN_WEEK), 
                                                          function(x) Compute_Lag_V2(DSET = NOAA_RELEVANT, DAT = TNC_CLEAN_WEEK$WEEK_LOW[x], LAG_NEAR = .x, LAG_FAR = .y, 
                                                                                     PLACE = TNC_CLEAN_WEEK$state[x], TIME = "END"), mc.cores = 6)))))
names = map2_chr(.x = crossedArgs$L1, .y = crossedArgs$L2,
                 ~ str_c("Lag",as.character(.x),as.character(.y),sep = "_"))
colnames(X) = names

TNC_CLEAN_WEEK = bind_cols(TNC_CLEAN_WEEK, X)

#Doing Same Thing for Placebo Events
NOAA_RELEVANT = NOAA_RELEVANT %>%
  filter(PLACEBO_EVENT == 1)

X_Placebo = map2_dfc(.x = crossedArgs$L1, .y = crossedArgs$L2, 
                     .f = ~ data.frame(as.numeric(unlist(mclapply(1:nrow(TNC_CLEAN_WEEK), 
                                                                  function(x) Compute_Lag_V2(DSET = NOAA_RELEVANT, DAT = TNC_CLEAN_WEEK$WEEK_LOW[x], LAG_NEAR = .x, LAG_FAR = .y, 
                                                                                             PLACE = TNC_CLEAN_WEEK$state[x], TIME = "END"), mc.cores = 6)))))

names = map2_chr(.x = crossedArgs$L1, .y = crossedArgs$L2,
                 ~ str_c("Lag_Placebo",as.character(.x),as.character(.y),sep = "_"))


colnames(X_Placebo) = names
glimpse(X_Placebo)

TNC_CLEAN_WEEK = bind_cols(TNC_CLEAN_WEEK, X_Placebo)

glimpse(TNC_CLEAN_WEEK)




#Creating Not Placebo
X = as.matrix(X)
X_Placebo = as.matrix(X_Placebo)
X_Not_Placebo = matrix(nrow = nrow(X),ncol = ncol(X))
for(i in 1:nrow(X)){
  for(j in 1:ncol(X)){
    X_Not_Placebo[i,j] = X[i,j] - X_Placebo[i,j]
  }
}
colnames(X_Not_Placebo) = str_replace(colnames(X_Placebo),pattern = "Placebo", replacement = "Not_Placebo")
X_Not_Placebo = as.data.frame(X_Not_Placebo)

TNC_CLEAN_WEEK = bind_cols(TNC_CLEAN_WEEK,X_Not_Placebo)

write.csv(TNC_CLEAN_WEEK,"~/Google Drive/DATA/ECON/CLEAN/TNC_Merged_Week.csv")


#Table of Means for TNC Regressions
TNC_CLEAN_WEEK = read.csv("~/Google Drive/DATA/ECON/CLEAN/TNC_Merged_Week.csv")
TNC_ToM = TNC_CLEAN_WEEK %>%
  ungroup() %>%
  dplyr::select(COUNT,Lag_0_30) %>%
  as.data.frame()
TNC_ToM = stargazer(TNC_ToM, style = "aer", type = "latex",
                    summary.stat = c("n", "mean","min","max","sd"),
                    column.sep.width = "1",
                    title = "Summary Statistics for TNC Donations Data",
                    covariate.labels = c("Donation Count", "30-Day Lagged Damages"))
cat(paste(TNC_ToM, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/DATA_Section/Table_of_Means_TNC.tex", append = TRUE)

#------------------------------------------------Analyzing Results-----------------------------------#
#---------------------------------------------------------------------------------------------------#
