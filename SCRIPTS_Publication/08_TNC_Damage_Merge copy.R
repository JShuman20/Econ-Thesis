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


#-------------------------------------Read in NOAA Data---------------------------------#
NOAA = fread("~/Google Drive/DATA/ECON/CLEAN/NOAA/02_NOAA_CLEANED.csv") %>%
  mutate(END_DATE = as.Date(END_DATE),
         BEGIN_DATE = as.Date(BEGIN_DATE))
#Filter to Relevant
NOAA_RELEVANT = NOAA %>%
  filter(YEAR >= 2010 & DAMAGE_PROPERTY > 0) 

#----------------------------------Trying Day-By-Day For Donations---------------------------#
TNC_DAY = read.csv("~/Google Drive/DATA/ECON/CLEAN/TNC_CLEAN_DAY.csv") %>%
  mutate(GIFT_DATE = as.Date(GIFT_DATE))

#---------Doing Merge------------#

day_level_args = data.frame(
  Lag_Near = c(0,0,0,0,15,30,60),
  Lag_Far = c(1,7,15,30,30,60,90)
)

DT_Args = map_dfr(.x = unique(TNC_DAY$GIFT_DATE), .f = ~ day_level_args %>% mutate(Date = as.Date(.x)))
DT_Args = bind_rows((DT_Args %>% mutate(Placebo = T)),
                    (DT_Args %>% mutate(Placebo = F)))


NOAA_RELEVANT_DT = setDT(NOAA_RELEVANT)
Rel_State_Names = data.table(STATE = unique(NOAA_RELEVANT$STATE))

library(furrr)
options("future.fork.enable" = TRUE)
future::plan(multicore(workers = 4))
ALL_DAM = future_pmap_dfr(list(DT_Args$Lag_Near,
                               DT_Args$Lag_Far,
                               DT_Args$Date,
                               DT_Args$Placebo),
                  .f = ~ {
                          NAME = ifelse(..4 == T,
                                        as.character(str_c('Lag',as.character(..1), as.character(..2), sep = '_')),
                                        as.character(str_c('Placebo_Lag',as.character(..1), as.character(..2), sep = '_')))
                          if(..4 == T){
                            RES =  NOAA_RELEVANT_DT[END_DATE >=(..3 - days(..2))  & END_DATE <= (..3 - days(..1)),
                                                    list('SUM' =sum(DAM_PROP_2000)), by = STATE]
                          }
                          else{
                            RES = NOAA_RELEVANT_DT[END_DATE >=(..3 - days(..2))  & END_DATE <= (..3 - days(..1)) & PLACEBO_EVENT == 1,
                                                   list('SUM' =sum(DAM_PROP_2000)), by = STATE]
                          }
                          RES = RES[Rel_State_Names,on = 'STATE'][is.na(SUM),SUM:=0]
                          RES$GIFT_DATE = ..3
                          RES$Lag = NAME
                          return(RES)
                        }) %>%
  data.frame() %>%
  pivot_wider(id_cols = c('STATE','GIFT_DATE'), names_from = 'Lag', values_from = 'SUM')



#Add Non-Placebo Columns
NAMES = map2_chr(day_level_args$Lag_Near,day_level_args$Lag_Far,.f =  ~ str_c('Lag',.x,.y,sep = "_"))
Not_Placebo = map_dfc(.x = NAMES, .f = ~{
                Placebo_Name = str_c('Placebo',.x,sep='_')
                Not_Placebo = str_c('Not_Placebo',.x,sep = '_')
                RES = data.frame(ALL_DAM[,.x] - ALL_DAM[,Placebo_Name])
                colnames(RES) = Not_Placebo
                return(RES)
              })
ALL_DAM = ALL_DAM %>% bind_cols(Not_Placebo)
head(ALL_DAM)


ALL_DAM_Sub = ALL_DAM %>% filter(GIFT_DATE > date('2010-12-31'))



#Merge on State Codes
STATES = readxl::read_xlsx("~/Google Drive/DATA/ECON/STATES_REGIONS.xlsx") %>%
  dplyr::select(1:2) %>%
  rename(STATE = 'State Name', 'state' = 'State Code')

TNC_DAY = TNC_DAY %>%
  left_join(STATES, by = 'state') %>%
  mutate(STATE = toupper(STATE)) %>%
  left_join(ALL_DAM, by = c('STATE','GIFT_DATE')) %>%
  mutate(across(.cols = c(contains('Lag')), .fns = ~ ifelse(.x>0,1,0),.names = '{col}_Bin')) %>%
  mutate(YEAR = lubridate::year(GIFT_DATE),
         MONTH = lubridate::month(GIFT_DATE),
         DAY_OF_WEEK = wday(GIFT_DATE)) %>%
  filter(YEAR %in% 2011:2017)

POPS = read.csv("~/Google Drive/DATA/ECON/STATE_POP.csv") %>%
  dplyr::select(YEAR, POP,CODE) %>%
  rename(state = CODE)


TNC_DAY = TNC_DAY %>%
  left_join(POPS, by = c("YEAR", "state")) %>%
  mutate(COUNT_PER_MIL = COUNT/(POP/1000000))

TNC_DAY = TNC_DAY %>%
  mutate(BIN_FIVE = cut(Lag_0_30, breaks = c(-Inf,0,quantile(Lag_0_30[which(Lag_0_30>0)],c(1/4,1/2,3/4)),Inf),
                         labels = c("Zero", "Q1", "Q2","Q3", "Q4")),
          BIN1 = ifelse(BIN_FIVE == "Q1",1,0),
          BIN2 = ifelse(BIN_FIVE  == "Q2", 1,0),
          BIN3 = ifelse(BIN_FIVE == "Q3",1,0),
          BIN4 = ifelse(BIN_FIVE == "Q4", 1,0))

head(TNC_DAY)

library(fixest)

feols(COUNT_PER_MIL ~ Lag_0_30_Bin + Lag_30_60_Bin + Lag_60_90_Bin | state + YEAR + MONTH + DAY_OF_WEEK , data = TNC_DAY)
feols(COUNT_PER_MIL ~ BIN1 + BIN2 + BIN3 + BIN4 | state + YEAR + MONTH + DAY_OF_WEEK, data = TNC_DAY)
feols(COUNT_PER_MIL ~ Placebo_Lag_0_30_Bin | state + YEAR + MONTH + DAY_OF_WEEK , data = subset(TNC_DAY,MONTH != 12))

colnames(TNC_DAY)


#Writing Result to Excel
writexl::write_xlsx(TNC_DAY, "~/Google Drive/DATA/ECON/CLEAN/TNC_CLEAN_DAY_MERGED.csv")



table(is.na(TNC_DAY$Lag_0_15))








#--------------------------------------------------------Assigning Relevant Columns for  Day-Level---------------------------------------#
day_level_args = data.frame(
  Lag_Near = c(0,0,0,0,15,30,60),
  Lag_Far = c(1,7,15,30,30,60,90)
)

day_level_vals = map2_dfc(.x = day_level_args$Lag_Near,.y = day_level_args$Lag_Far,
                          .f =  ~ data.frame(unlist(mclapply(1:10000,
                                                             function(X) sum(NOAA_RELEVANT$DAM_PROP_2000[which(NOAA_RELEVANT$STATE_CODE== TNC_DAY$state[X] &
                                                                                                               NOAA_RELEVANT$END_DATE >= (TNC_DAY$GIFT_DATE[X] - .y) &
                                                                                                               NOAA_RELEVANT$END_DATE <= (TNC_DAY$GIFT_DATE[X] - .x))]),
                                                             mc.cores = 6))))
   
colnames(day_level_vals) = map2_chr(day_level_args$Lag_Near,day_level_args$Lag_Far, ~ str_c("Lag_",.x,"_",.y))
head(day_level_vals)

compare = day_level_vals %>%
  dplyr::select(Lag_0_1,Lag_0_7,Lag_0_30) %>%
  rename_all(toupper) %>%
  bind_cols((TNC_DAY %>%
  dplyr::select(Lag_0_1, Lag_0_7,Lag_0_30) %>%
  head(10000)))

compare %>% mutate(DIFF1 = LAG_0_1 - Lag_0_1,
                   DIFF2 = LAG_0_7 - Lag_0_7,
                   DIFF3 = LAG_0_30 - Lag_0_30) 
#----------------Day-Level Placebo Vals
NOAA_RELEVANT = NOAA_RELEVANT %>% filter(PLACEBO_EVENT == 1)

placebo_day_level_vals = map2_dfc(.x = day_level_args$Lag_Near,.y = day_level_args$Lag_Far,
                          .f =  ~ data.frame(unlist(mclapply(1:nrow(TNC_DAY), 
                                                             function(X) sum(NOAA_RELEVANT$DAM_PROP_2000[which(NOAA_RELEVANT$STATE_CODE == TNC_DAY$state[X] &
                                                                                                               NOAA_RELEVANT$END_DATE >= (TNC_DAY$GIFT_DATE[X] - .y) &
                                                                                                               NOAA_RELEVANT$END_DATE <= (TNC_DAY$GIFT_DATE[X] - .x))]),
                                                             mc.cores = 6))))

colnames(placebo_day_level_vals) = map2_chr(day_level_args$Lag_Near,day_level_args$Lag_Far, ~ str_c("Placebo_Lag_",.x,"_",.y))


not_placebo_day_level_vals = map_dfc(.x = 1:ncol(day_level_vals),
                                     .f = ~ day_level_vals[,.x] - placebo_day_level_vals[,.x])

colnames(not_placebo_day_level_vals) = map2_chr(day_level_args$Lag_Near,day_level_args$Lag_Far, ~ str_c("Not_Placebo_Lag_",.x,"_",.y))

TNC_DAY = TNC_DAY %>%
  bind_cols(day_level_vals) %>%
  bind_cols(placebo_day_level_vals) %>%
  bind_cols(not_placebo_day_level_vals) %>%
  mutate(across(.cols = c(contains('Lag')), .fns = ~ ifelse(.x>0,1,0),.names = '{col}_Bin')) %>%
  mutate(YEAR = lubridate::year(GIFT_DATE),
         MONTH = lubridate::month(GIFT_DATE),
         DAY_OF_WEEK = wday(GIFT_DATE)) %>%
  filter(YEAR %in% 2011:2017)

POPS = read.csv("~/Google Drive/DATA/ECON/STATE_POP.csv") %>%
  dplyr::select(YEAR, POP,CODE) %>%
  rename(state = CODE)

TNC_DAY = TNC_DAY %>%
  left_join(POPS, by = c("YEAR", "state")) %>%
  mutate(COUNT_PER_MIL = COUNT/(POP/1000000))

#Writing Result to Excel
writexl::write_xlsx(TNC_DAY, "~/Google Drive/DATA/ECON/CLEAN/TNC_CLEAN_DAY_MERGED.csv")


Test_Old_Vals = readxl::read_xlsx("~/Google Drive/DATA/ECON/CLEAN/TNC_CLEAN_DAY_MERGED.csv")

head(Test_Old_Vals)


feols(COUNT_PER_MIL ~ Lag_0_30_Bin | state + YEAR + MONTH + DAYOFWEEK, data = Test_Old_Vals)



head(TNC_DAY)


Test_Diffs = Test_Old_Vals %>%
  select(state, GIFT_DATE, Lag_0_30_Bin, Lag_0_30) %>%
  rename_all(toupper) %>%
  bind_cols((TNC_DAY %>% select(Lag_0_30_Bin, Lag_0_30))) %>%
  filter(LAG_0_30_BIN != Lag_0_30_Bin)

Test_Diffs %>% filter(LAG_0_30_BIN == 0)

unique(Test_Diffs$GIFT_DATE)

min(NOAA_RELEVANT_DT$END_DATE)


NOAA_RELEVANT %>% filter(END_DATE <= as.Date('2011-01-01') & END_DATE >= as.Date('2010-12-01') & STATE_CODE =='AR')
head(NOAA_RELEVANT)


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
X

#Appending Columns to TNC Panel
TNC_CLEAN_WEEK = bind_cols(TNC_CLEAN_WEEK, X)
TNC_CLEAN_WEEK

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
X_Placebo
#Append "Cold" Lagged Variables
TNC_CLEAN_WEEK = bind_cols(TNC_CLEAN_WEEK, X_Placebo)

glimpse(TNC_CLEAN_WEEK)
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
X_Not_Placebo
#Appending "Non-Cold" Columns
TNC_CLEAN_WEEK = bind_cols(TNC_CLEAN_WEEK,X_Not_Placebo)

glimpse(TNC_CLEAN_WEEK)
TNC_CLEAN_WEEK = TNC_CLEAN_WEEK %>%
  mutate(WEEK_LOW = as.Date(WEEK_LOW),
         LAG_7 = WEEK_LOW - days(7)) %>%
  rowwise() %>%
  mutate(Dam_Lag_7 = sum(NOAA_RELEVANT$DAM_PROP_2000[which(NOAA_RELEVANT$STATE_CODE==state &
                                                             NOAA_RELEVANT$END_DATE >= LAG_7 &
                                                             NOAA_RELEVANT$END_DATE < WEEK_LOW)])) %>%
  ungroup() 


#Writing Merged (Lagged) DataSet
write.csv(TNC_CLEAN_WEEK,"~/Google Drive/DATA/ECON/CLEAN/TNC_Merged_Week.csv")




write.csv(TNC_CLEAN_WEEK,"~/Google Drive/DATA/ECON/CLEAN/TNC_Merged_Week.csv")

     