



library(tidyverse)
library(parallel)

#Reading in House Voting Data
ALL_LCV_HOUSE_MERGE = read.csv("~/Google Drive/DATA/ECON/CLEAN/LCV/HOUSE.csv") %>%
  mutate(District = as.character(District)) %>%
  mutate(District = ifelse(District == "SD-01", "SD-AL", District)) %>%
  mutate(DATE = as.Date(DATE))
#Extracting Distinct Events to Make Merge Easier
Distinct_HOUSE_Votes = ALL_LCV_HOUSE_MERGE %>%
  distinct(District, DATE, CODE)


#Sotrm Data Merged by Congressional District
Storms = read_rds("~/Desktop/ECON Thesis/ALL_MERGED.rds")

Storms = Storms %>%
  mutate(DISTRICT = as.character(DISTRICT),
         CD = ifelse(str_length(DISTRICT) == 1, str_c(STATE_CODE, "-0", DISTRICT), str_c(STATE_CODE, "-", DISTRICT)),
         CD_1 = case_when(
           (STATE == "MONTANA" & END_DATE > as.Date("1996-07-25")) ~ "MT-AL",
           (STATE %in% c("MONTANA", "NORTH DAKOTA", "SOUTH DAKOTA", "VERMONT",
                        "WYOMING", "ALASKA", "DELAWARE") & str_detect(CD, "00")) ~ str_c(STATE_CODE, "-AL"),
           
           TRUE ~ CD))
head(Storms)


#Record of Storms that are not matched by district
NOAA = read.csv("~/Google Drive/DATA/ECON/CLEAN/NOAA/02_NOAA_CLEANED.csv")
NOAA_RELEVANT = NOAA %>% filter(DAMAGE_PROPERTY > 0) %>%
  mutate(END_DATE = as.Date(END_DATE))

NOAA_RELEVANT_Excluded = NOAA_RELEVANT %>%
  filter(!EVENT_ID %in% unique(Storms$EVENT_ID))
head(NOAA_RELEVANT_Excluded)


#Writing Function to Compute Lags By Either State or Disitrct
Compute_Lag_V2 = function(DSET, DAT, LAG_NEAR = 0, LAG_FAR, PLACE, UNIT = "STATE"){
  DAT_FAR  = DAT - LAG_FAR
  DAT_NEAR = DAT - LAG_NEAR
 if(UNIT == "STATE"){
   RES = sum(DSET$DAMAGE_PROPERTY[which(DSET$END_DATE >= DAT_FAR    & 
                                          DSET$END_DATE <= DAT_NEAR   & 
                                          DSET$STATE_CODE == PLACE)], na.rm = TRUE) 
 }
 else{
   RES = sum(DSET$DAMAGE_PROPERTY[which(DSET$END_DATE >= DAT_FAR    & 
                                          DSET$END_DATE <= DAT_NEAR   & 
                                          DSET$CD_1 == PLACE)], na.rm = TRUE) 
 }
  return(RES)
}

#Computing 30-Day Lagged Values
VALS_District = unlist(mclapply(1:nrow(Distinct_HOUSE_Votes),
                                function(x) Compute_Lag_V2(DSET = Storms, 
                                                           DAT = Distinct_HOUSE_Votes$DATE[x], 
                                                           LAG_NEAR = 0, 
                                                           LAG_FAR = 30,
                                                           PLACE = Distinct_HOUSE_Votes$District[x], 
                                                           UNIT = "DISTRICT"), mc.cores = 4))

VALS_District_Sup = unlist(mclapply(1:nrow(Distinct_HOUSE_Votes),
                                    function(x) Compute_Lag_V2(DSET = NOAA_RELEVANT_Excluded, 
                                                               DAT = Distinct_HOUSE_Votes$DATE[x], 
                                                               LAG_NEAR = 0, 
                                                               LAG_FAR = 30,
                                                               PLACE = Distinct_HOUSE_Votes$CODE[x], 
                                                               UNIT = "STATE"), mc.cores = 4))

#Computing 30-Day Placebo Lagged Values
VALS_District_Placebo = unlist(mclapply(1:nrow(Distinct_HOUSE_Votes),
                                function(x) Compute_Lag_V2(DSET = Storms[which(Storms$PLACEBO_EVENT==1),], 
                                                           DAT = Distinct_HOUSE_Votes$DATE[x], 
                                                           LAG_NEAR = 0, 
                                                           LAG_FAR = 30,
                                                           PLACE = Distinct_HOUSE_Votes$District[x], 
                                                           UNIT = "DISTRICT"),mc.cores = 4))
VALS_District_Placebo_Sup = unlist(mclapply(1:nrow(Distinct_HOUSE_Votes),
                               function(x) Compute_Lag_V2(DSET = NOAA_RELEVANT_Excluded[which(NOAA_RELEVANT_Excluded$PLACEBO_EVENT==1),], 
                                                          DAT = Distinct_HOUSE_Votes$DATE[x], 
                                                          LAG_NEAR = 0, 
                                                          LAG_FAR = 30,
                                                          PLACE = Distinct_HOUSE_Votes$CODE[x], 
                                                          UNIT = "STATE"), mc.cores = 4))

#Adding Together
VALS_District = VALS_District + VALS_District_Sup
VALS_District_Placebo = VALS_District_Placebo + VALS_District_Placebo_Sup
VALS_District_NotPlacebo = VALS_District - VALS_District_Placebo

table(VALS_District > 0)
table(VALS_Total_District>0)
#Adding onto distinct votes dataset

Distinct_HOUSE_Votes$Damage_All = VALS_District
Distinct_HOUSE_Votes$Damage_Placebo = VALS_District_Placebo
Distinct_HOUSE_Votes$Damage_Not_Placebo = VALS_District_NotPlacebo




ALL_LCV_HOUSE_MERGE = ALL_LCV_HOUSE_MERGE %>%
  left_join(Distinct_HOUSE_Votes, by = c("District", "CODE", "DATE"))

table(Distinct_HOUSE_Votes$Damage_All>0)
head(ALL_LCV_HOUSE_MERGE)

ALL_LCV_HOUSE_MERGE = ALL_LCV_HOUSE_MERGE %>%
  mutate(POS_VOTE = case_when(
    Vote == "+" ~ 1,
    Vote == "-" ~ 0),
    POS_All = ifelse(Damage_All.y > 0 ,1,0),
    POS_Placebo = ifelse(Damage_Placebo>0,1,0),
    POS_NotPlacebo = ifelse(Damage_Not_Placebo>0,1,0),
    SEASON = case_when(
      MON_NUM %in% c(1,2,3) ~ "WINTER",
      MON_NUM %in% c(4,5,6) ~ "SPRING",
      MON_NUM %in% c(7,8,9) ~ "SUMMER",
      MON_NUM %in% c(10,11,12) ~ "FALL")) %>% #Season fixed effect
  mutate(PANEL_VAR = 100000*Year + ROLL_CALL)  %>%  #Panel time variables
  mutate(REP = ifelse(Party == "R",1,0)) #Indicato

Damage

table(ALL_LCV_HOUSE_MERGE$POS == 0)

ALL_LCV_HOUSE_MERGE = ALL_LCV_HOUSE_MERGE %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(SEASON = ifelse(Policy == "Air.Right.to.Know" & Year == 2000, "SPRING", SEASON)) %>%
  mutate(TOO_LATE = ifelse(DATE >= "2019-01-01", 1,0))




ALL_LCV_HOUSE_MERGE %>%
  filter(State == "California")

length(VALS)

library(fixest)

feols(POS_VOTE ~ POS_Placebo*REP - REP  | Member.of.Congress  +  Year +SEASON  , data = subset(ALL_LCV_HOUSE_MERGE, TOO_LATE == 0))
feols(POS_VOTE ~ POS_All*REP - REP  | Member.of.Congress  + Year + SEASON  , data = ALL_LCV_HOUSE_MERGE)
                                                                                          
library(stargazer)
stargazer(Test, omit = "factor*")


head(ALL_LCV_HOUSE_MERGE)
ALL_LCV_HOUSE_MERGE[which(ALL_LCV_HOUSE_MERGE$State=="Montana"),]

table(ALL_LCV_HOUSE_MERGE$VALS>0)

test = Distinct_HOUSE_Votes %>%
  filter(str_detect(District, "SD"))
table(test$District)
table(str_length(Storms$DISTRICT))

table(ALL_LCV_HOUSE_MERGE$REP)

library(plm)
plm(POS_VOTE ~ POS*REP + factor(Year) + factor(SEASON), data = ALL_LCV_HOUSE_MERGE, 
    model = "within", index = c("Member.of.Congress","PANEL_VAR"))

head(Storms)

#Just Trying 1
Test = Distinct_HOUSE_Votes %>%
  filter(str_detect(District, "ND|VT|AK|SD|WY|MT|DE"))  %>%
  filter(!str_detect(District, "AL")) %>%
  filter(str_detect(District, "SD"))
Test
Test = Distinct_HOUSE_Votes %>%
  filter(str_detect(District, "MT"))
table(Test$District)



#Defining Argument List
args = list(
  L1 = c(0,15,30,45,60,90),
  L2 = c(0,15,30,45,60,90),
  TIME = c("BEGIN","END")
)
crossedArgs = cross_df(args) %>%
  filter(L2 - L1 >0)

#-----------------------------------------Calculating Lagged Damage Variables for All Storm Types--------------------------------#
#Defining Column Names
names = pmap_chr(list(..1 = crossedArgs$L1, ..2 = crossedArgs$L2, ..3 = crossedArgs$TIME),
                 ~ str_c("Lag",as.character(..1),as.character(..2),..3,sep = "_"))
#Performing Calculations -- THIS TAKES A WHILE!!!
RES = pmap_dfc(list(..1 = crossedArgs$L1, ..2 = crossedArgs$L2, ..3 = crossedArgs$TIME),
               .f = ~ data.frame(as.numeric(unlist(mclapply(1:nrow(Distinct_HOUSE_Votes),
                                                            function(x) Compute_Lag_V2(DSET = DAT_CLEAN_NOZEROS, 
                                                                                       DAT = Distinct_HOUSE_Votes$DATE[x], 
                                                                                       LAG_NEAR = ..1, LAG_FAR = ..2,
                                                                                       PLACE = Distinct_HOUSE_Votes$CODE[x], TIME = ..3),
                                                            mc.cores = 8))))) #USE THIS TO CHANGE NUMBER OF CORES
#Assigning Column Names
colnames(RES) = names

#----------------------------------------Calculating Lagged Damage Variables For "Cold" Storm Type--------------------------------#
#Defining Column Names
names = pmap_chr(list(..1 = crossedArgs$L1, ..2 = crossedArgs$L2, ..3 = crossedArgs$TIME),
                 ~ str_c("Lag",as.character(..1),as.character(..2),..3,"PLACEBO", sep = "_"))
#Performing Calculation --- THIS TAKES A WHILE
RES_PLACEBO  = pmap_dfc(list(..1 = crossedArgs$L1, ..2 = crossedArgs$L2, ..3 = crossedArgs$TIME),
                        .f = ~ data.frame(as.numeric(unlist(mclapply(1:nrow(Distinct_HOUSE_Votes),
                                                                     function(x) Compute_Lag_V2(DSET = DAT_CLEAN_PLACEBO, 
                                                                                                DAT = Distinct_HOUSE_Votes$DATE[x], 
                                                                                                LAG_NEAR = ..1, LAG_FAR = ..2,
                                                                                                PLACE = Distinct_HOUSE_Votes$CODE[x], TIME = ..3), 
                                                                     mc.cores = 8))))) #USE TO CHANGE NUMBER OF CORES
#Assigning Column Names
colnames(RES_PLACEBO) = names

#----------------------------------Calculating Lagged Damage Variables For "Non-Cold"-------------------------#
#Defining Variable Names
names = pmap_chr(list(..1 = crossedArgs$L1, ..2 = crossedArgs$L2, ..3 = crossedArgs$TIME),
                 ~ str_c("Lag",as.character(..1),as.character(..2),..3,"NOT_PLACEBO" ,sep = "_"))
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

House_Lagged = read.csv("~/Google Drive/DATA/ECON/CLEAN/HOUSE_LAGGED.csv")
head(House_Lagged)

