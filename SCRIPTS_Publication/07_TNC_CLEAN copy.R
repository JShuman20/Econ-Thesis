#-------------------------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------------------#
#This Script cleans the large TNC dataset and creates state panels by week and day. Summary Figures are also computed-----------------------------#
#-------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------Loading Required Libraries--------------------------#

library(tidyverse)
library(purrr)
library(parallel)
library(readxl)
library(data.table)
library(lubridate)
library(USAboundaries)
library(ggmap)
library(sp)
library(sf)
library(stargazer)

#------------------------------------Reading in Raw Data Files---------------------------------------#
TNC = fread("~/Google Drive/DATA/ECON/RAW/TNC_RAW.csv") #Faster import
STATES = read_xlsx("~/Google Drive/DATA/ECON/STATES_REGIONS.xlsx") %>%
  rename(CODE = `State Code`)

head(TNC_CLEAN)
glimpse(TNC)
#Number of Observations
nrow(TNC)
colnames(TNC_CLEAN)

#-------------------------------Preliminary Cleaning------------------------------------------------#
TNC_CLEAN = TNC %>%
  filter(str_length(state) > 1) %>%
  mutate(state = toupper(state)) %>%
  filter(state %in% c(STATES$CODE)) %>%  #Filtering to 50 States
  filter(donor_type == "Individual")  #Remove Non-Individual Donors

#Intermediate Stat: How Many Did this remove?
nrow(TNC) - nrow(TNC_CLEAN)
#------------------------------------Majority of Cleaning--------------------------------------------#
TNC_CLEAN = TNC_CLEAN %>%
  mutate(gift_year = str_sub(gift_date,6,10),
         gift_mon  = str_sub(gift_date, 3,5),
         gift_mon_int = case_when(
           gift_mon == "JAN" ~ 1,
           gift_mon == "FEB" ~ 2,
           gift_mon == "MAR" ~ 3,
           gift_mon == "APR" ~ 4,
           gift_mon == "MAY" ~ 5,
           gift_mon == "JUN" ~ 6,
           gift_mon == "JUL" ~ 7,
           gift_mon == "AUG" ~ 8,
           gift_mon == "SEP" ~ 9,
           gift_mon == "OCT" ~ 10,
           gift_mon == "NOV" ~ 11,
           gift_mon == "DEC" ~ 12),
         gift_day  = str_sub(gift_date,1,2),
         GIFT_DATE = as.Date(str_c(gift_year,gift_mon_int,gift_day,sep = "-"))) %>% #Adding Time Variables
  arrange(member_id,GIFT_DATE) %>%  #Sorting by ID and time
  mutate(DIFF_ID   = c(1,diff(member_id, lag = 1)),
         DIFF_AMT  = c(1,diff(gift_amount, lag = 1)),
         DIFF_DATE = c(1,diff(GIFT_DATE,lag = 1))) %>%  #Creating Variables to Detect Monthly Donations
  mutate(DROP = ifelse(DIFF_ID == 0 & DIFF_AMT == 0 & DIFF_DATE %in% c(28:32),1,0)) %>% # Only Exclude Monthly
  filter(DROP == 0)  #Dropping Recurring Monthly Donations

#How Many Results Does this Leave?
nrow(TNC_CLEAN)
#-----------------------------------------------More Processing--------------------------------------#
TNC_CLEAN = TNC_CLEAN %>%
  mutate(GIFT_DATE = case_when(
              giving_channel == "Mail" ~ GIFT_DATE - 3, 
              TRUE ~ GIFT_DATE),   #Lagging Date By 3 For Mail Donations
         WEEK_LOW = floor_date(GIFT_DATE, unit = "week"))  #Collapsing All Donations to Beginning of the Week

#Removing Strange Individual IDs
Weird_IDS = TNC_CLEAN %>%
  group_by(member_id) %>%
  summarize(Count = n()) %>%
  filter(Count > 500) %>%
  pull(member_id)
TNC_CLEAN = TNC_CLEAN %>%
  filter(!member_id %in% Weird_IDS) 
#How Many Weird IDS?
5848290 - nrow(TNC_CLEAN)

#---------------------------------------------------------Constructing State x Week Panel-----------------------#
TNC_CLEAN_WEEK = TNC_CLEAN %>%
  group_by(WEEK_LOW, state) %>%
  summarize(COUNT = n(),
            AMT = sum(gift_amount))
#Imputing Any Missing state x week observations as zero (I don't think there are any for week-level)
MISSINGS = TNC_CLEAN_WEEK %>%
  group_by(WEEK_LOW) %>%
  summarize(COUNT = n()) %>%
  filter(COUNT < 50) %>%
  pull(WEEK_LOW)    #Extracting List of Weeks That Don't Have 50 Observations
#Adding Zero Rows
Zeros = map_dfr(.x = 1:length(MISSINGS),
                .f =  ~ data.frame(
                  state = setdiff(STATES$CODE, TNC_CLEAN_WEEK %>%
                                    filter(WEEK_LOW == MISSINGS[.x]) %>%
                                    pull(state)),
                  WEEK_LOW = MISSINGS[.x],
                  COUNT = 0,
                  AMT = 0))   #Constructing Rows With Zeros
TNC_CLEAN_WEEK = bind_rows(TNC_CLEAN_WEEK, Zeros) #Binding to State x Week Dataset

#--------------------------------------Remove Seemingly-Partial Observations at Ends of DataSet-------------------------#
TNC_CLEAN_WEEK = TNC_CLEAN_WEEK %>%
  filter(year(WEEK_LOW) %in% 2011:2017) %>%
  filter(WEEK_LOW != "2017-12-03")  #It looks like the data ends is problematic in this one week

#--------------------------------------------------Constructing State x Day Panel------------------------------------#
TNC_CLEAN_DAY = TNC_CLEAN %>%
  group_by(GIFT_DATE, state) %>%
  summarize(COUNT = n(),
            AMT = sum(gift_amount))
MISSINGS = TNC_CLEAN_DAY %>%
  group_by(GIFT_DATE) %>% summarize(COUNT = n()) %>%
  filter(COUNT < 50) %>%
  pull(GIFT_DATE)
#Adding Zero Rows
Zeros = map_dfr(.x = 1:length(MISSINGS),
                .f =  ~ data.frame(
                  state = setdiff(STATES$CODE, TNC_CLEAN_DAY %>%
                                    filter(GIFT_DATE == MISSINGS[.x]) %>%
                                    pull(state)),
                  GIFT_DATE = MISSINGS[.x],
                  COUNT = 0,
                  AMT = 0))

TNC_CLEAN_DAY = bind_rows(TNC_CLEAN_DAY,Zeros)
write.csv("~/Google Drive/DATA/ECON/CLEAN/TNC_CLEAN_DAY.csv")

#-------------------------------------Merging With Census Population Estimates-----------------------------------------#
#Reading and Formatting Census Data for Valid Merge
Census = read.csv("~/Google Drive/DATA/ECON/nst-est2019-popchg2010_2019.csv") %>%
  dplyr::select(NAME, starts_with("POPESTIMATE")) %>%
  filter(NAME %in% STATES$`State Name`) %>%
  rename_all(~str_sub(.,-4)) %>%
  pivot_longer(cols = 2:11) %>%
  rename(`State Name` = NAME,YEAR = name, POP = value) %>%
  mutate(YEAR = as.numeric(YEAR),
         `State Name` = as.character(`State Name`)) %>%
  left_join(STATES) 
#Writing Cleaned Version
write.csv(Census, "~/Google Drive/DATA/ECON/STATE_POP.csv")
Census = Census %>%
  select(YEAR, CODE, POP) %>%
  rename(state = CODE)

#Adding Census Data to TNC Panel and Calculating Donations Per Million Residents
TNC_CLEAN_WEEK = TNC_CLEAN_WEEK %>%
  mutate(YEAR = year(WEEK_LOW)) %>%
  left_join(Census, by = c("YEAR", "state"))%>%
  mutate(COUNT_PER_POP = COUNT/POP,
         COUNT_PER_MIL = COUNT_PER_POP*1000000)
         

write.csv(TNC_CLEAN_WEEK,"~/Google Drive/DATA/ECON/CLEAN/TNC_CLEAN_Week.csv")    



#--------------------------------------------Summary Statistics--------------------------------------#
#Dimension of DataSet
nrow(TNC_CLEAN_WEEK)/50
#Distribution of Gift Amounts
summary(TNC_CLEAN$gift_amount)
#Minimum Value
max(TNC_CLEAN_WEEK$COUNT)
#From Where?
TNC_CLEAN_WEEK$state[which.max(TNC_CLEAN_WEEK$COUNT_PER_MIL)]
#When?
TNC_CLEAN_WEEK$WEEK_LOW[which.max(TNC_CLEAN_WEEK$COUNT_PER_MIL)]

plot_state
#--------------------------------------------Creating Map-------------------------------------------#
#Import Plot of United States
STATE_GIS = us_states(resolution = "low")
#Collapsing into single observation per state
Merge_TNC = TNC_CLEAN_WEEK %>%
  group_by(state) %>%
  summarize(Count_Per_Week  = mean(COUNT),
            Count_Per_Week_Mil = mean(COUNT_PER_MIL)) %>%
  rename(state_abbr = state)

#See Which States have highest and lowest averages
Merge_TNC %>% arrange(desc(Count_Per_Week))
Merge_TNC %>% arrange(Count_Per_Week)
#See Which States have the highest and lowest averages by population
Merge_TNC %>% arrange(desc(Count_Per_Week_Mil))
Merge_TNC %>% arrange(Count_Per_Week_Mil)

#Joining Donation Counts Onto Map Data
STATEPLOT = STATE_GIS %>%
  left_join(Merge_TNC, by = "state_abbr") %>%
  filter(!state_name %in% c("Hawaii", "Alaska", "Puerto Rico")) 
#Creating Map
ggplot() +
  geom_sf() +
  geom_sf(data = STATEPLOT, aes(fill = Count_Per_Week)) +
  scale_fill_gradient(low = "orange", high = "blue", name = "Count Per Week") +
  theme_minimal() +
  ggtitle("Average Weekly Count of TNC Donations Per Million Residents By State")
#Creating a Map with Scaled Donations Per Million People
ggplot() +
  geom_sf() +
  geom_sf(data = STATEPLOT, aes(fill = Count_Per_Week_Mil)) +
  scale_fill_gradient(low = "orange", high = "blue", name = "Count Per Week") +
  theme_minimal() +
  ggtitle("Average Weekly Count of TNC Donations Per Million Residents By State")


#------------------------------Histograms of Donation Counts For 6 Extreme States-------------------------------#
TNC_CLEAN_WEEK %>%
  filter(state %in% c("ND","SD","WY", "FL", "NY","CA")) %>%
  ggplot() + 
  geom_histogram(aes(x = COUNT), fill = "black", bins = 20) +
  theme_bw() +
  facet_grid(~state, scales = "free_x") +
  xlab("Weekly Donations") + ylab("Count") +
  ggtitle("Distribution of Weekly Donation Counts by State")

#Histograms for Extreme States Identified by Scaled Donations
TNC_CLEAN_WEEK %>%
  filter(state %in% c("MS","LA","TX", "VT", "OR", "ME")) %>%
  ggplot() + 
  geom_histogram(aes(x = COUNT_PER_MIL), fill = "black", bins = 20) +
  theme_bw() +
  facet_grid(~state, scales = "free_x") +
  xlab("Weekly Donations") + ylab("Count") +
  ggtitle("Distribution of Weekly Donation Counts Per Million by State")



#--------------------------------------------------Time Trend by Median Split-----------------------------------------------------#
Above.Med = Merge_TNC$state_abbr[which(Merge_TNC$Count_Per_Week > median(Merge_TNC$Count_Per_Week))] 
#Trends by Week
TNC_CLEAN_WEEK %>%
  mutate(MON = as.yearmon(as.character(WEEK_LOW)),
         Above.Median = as.factor(ifelse(state %in% Above.Med, 1,0))) %>%
  group_by(WEEK_LOW, Above.Median) %>%
  summarize(COUNT = sum(COUNT)) %>%
  ggplot() +
  geom_line(aes(x = WEEK_LOW,y = log10(COUNT),group = Above.Median, col = Above.Median)) +
  #geom_vline(xintercept = as.Date(str_c(as.character(2011:2017), "-12-15")),col = "forestgreen",lty = 2) + #Optional Aesthetic
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_date(breaks = as.Date(str_c(as.character(2011:2016), "-12-15")), 
               labels= c("Dec.2011","Dec.2012","Dec.2013","Dec.2014", "Dec.2015","Dec.2016")) +
  xlab("Time") +
  ylab(expression(paste(Log[10], " Count of Donations By Week"))) +
  ggtitle(expression(paste("Total TNC Donation Counts By Week (", Log[10],")")))

#Time Trends by Median Split for Scaled Donations
Above.Med.Scaled = Merge_TNC$state_abbr[which(Merge_TNC$Count_Per_Week_Mil > median(Merge_TNC$Count_Per_Week_Mil))]

TNC_CLEAN_WEEK %>%
  mutate(MON = as.yearmon(as.character(WEEK_LOW)),
         Above.Median = as.factor(ifelse(state %in% Above.Med.Scaled, 1,0))) %>%
  group_by(WEEK_LOW, Above.Median) %>%
  summarize(COUNT = sum(COUNT),
            POP = sum(POP),
            COUNT_PER_POP = COUNT/POP,
            COUNT_PER_MIL = 1000000*COUNT_PER_POP) %>%
  mutate(Above.Median = ifelse(Above.Median==0,"Below Median", "Above Median")) %>%
  ggplot() +
  geom_line(aes(x = WEEK_LOW,y = COUNT_PER_MIL,group = Above.Median, col = Above.Median, lty = Above.Median)) +
  #geom_vline(xintercept = as.Date(str_c(as.character(2011:2017), "-12-15")),col = "forestgreen",lty = 2) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_colour_discrete("") +
  scale_linetype_manual("", values=c(1,2)) +
  scale_x_date(breaks = as.Date(str_c(as.character(2011:2016), "-12-15")), 
               labels= c("Dec.2011","Dec.2012","Dec.2013","Dec.2014", "Dec.2015","Dec.2016")) +
  xlab("Time") +
  ylab("Weekly Dnations Per Million People") +
  ggtitle("TNC Donations Per Million By Week")


