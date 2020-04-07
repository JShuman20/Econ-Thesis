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


TNC = fread("~/Google Drive/DATA/ECON/RAW/TNC_RAW.csv")
STATES = read_xlsx("~/Google Drive/DATA/ECON/STATES_REGIONS.xlsx") %>%
  rename(CODE = `State Code`)

TNC_CLEAN = TNC %>%
  filter(str_length(state) > 1) %>%
  mutate(state = toupper(state)) %>%
  filter(state %in% c(STATES$CODE)) %>%  #Filtering to 50 States
  filter(donor_type == "Individual") %>% #Remove Non-Individual Donrs
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
         GIFT_DATE = as.Date(str_c(gift_year,gift_mon_int,gift_day,sep = "-"))) %>%
  arrange(member_id,GIFT_DATE) %>%
  mutate(DIFF_ID   = c(1,diff(member_id, lag = 1)),
         DIFF_AMT  = c(1,diff(gift_amount, lag = 1)),
         DIFF_DATE = c(1,diff(GIFT_DATE,lag = 1))) %>%
  mutate(DROP = ifelse(DIFF_ID == 0 & DIFF_AMT ==0 & DIFF_DATE %in% c(28:32),1,0)) %>% # Only Exclude Monthly
  filter(DROP == 0) %>%
  mutate(
    GIFT_DATE = case_when(
      giving_channel == "Mail" ~ GIFT_DATE - 3,
      TRUE ~ GIFT_DATE),
    WEEK_LOW = floor_date(GIFT_DATE, unit = "week")) 
#Removing Strange Individual IDs
Weird_IDS = TNC_CLEAN %>%
  group_by(member_id) %>%
  summarize(Count = n()) %>%
  filter(Count > 500) %>%
  pull(member_id)
TNC_CLEAN = TNC_CLEAN %>%
  filter(!member_id %in% Weird_IDS) 

#Week-Level Merge
TNC_CLEAN_WEEK = TNC_CLEAN %>%
  group_by(WEEK_LOW, state) %>%
  summarize(COUNT = n(),
            AMT = sum(gift_amount))
#Identifying Missing Week-State Pairs and Adding Zero Rows
MISSINGS = TNC_CLEAN_WEEK %>%
  group_by(WEEK_LOW) %>%
  summarize(COUNT = n()) %>%
  filter(COUNT < 50) %>%
  pull(WEEK_LOW)
#Adding Zero Rows
Zeros = map_dfr(.x = 1:length(MISSINGS),
                .f =  ~ data.frame(
                  state = setdiff(STATES$CODE, TNC_CLEAN_WEEK %>%
                                    filter(WEEK_LOW == MISSINGS[.x]) %>%
                                    pull(state)),
                  WEEK_LOW = MISSINGS[.x],
                  COUNT = 0,
                  AMT = 0))

TNC_CLEAN_WEEK = bind_rows(TNC_CLEAN_WEEK, Zeros)
#Remove 2010 and 2018 Weeks
TNC_CLEAN_WEEK = TNC_CLEAN_WEEK %>%
  filter(year(WEEK_LOW) %in% 2011:2017) %>%
  filter(WEEK_LOW != "2017-12-03")
write.csv(TNC_CLEAN_WEEK,"~/Google Drive/DATA/ECON/CLEAN/TNC_CLEAN_Week.csv")    




summary(TNC_CLEAN$gift_amount)
nrow(TNC_CLEAN_WEEK)/50
#Plot Average Weekly Donation Count By State
STATE_GIS = us_states(resolution = "low")

Merge_TNC = TNC_CLEAN_WEEK %>%
  group_by(state) %>%
  summarize(Count_Per_Week  = mean(COUNT)) %>%
  rename(state_abbr = state)
Merge_TNC %>% arrange(Count_Per_Week) %>% View()

STATEPLOT = STATE_GIS %>%
  left_join(Merge_TNC, by = "state_abbr") %>%
  filter(!state_name %in% c("Hawaii", "Alaska", "Puerto Rico")) 

ggplot() +
  geom_sf() +
  geom_sf(data = STATEPLOT, aes(fill = Count_Per_Week)) +
  scale_fill_gradient(low = "orange", high = "blue", name = "Count Per Week") +
  theme_minimal() +
  ggtitle("Average Count of TNC Donations Per Week by State")

#Histograms of Donation Counts For 6 Extreme States
TNC_CLEAN_WEEK %>%
  filter(state %in% c("ND","SD","WY", "FL", "NY","CA")) %>%
  ggplot() + 
  geom_histogram(aes(x = COUNT), fill = "black", bins = 20) +
  theme_bw() +
  facet_grid(~state, scales = "free_x") +
  xlab("Weekly Donations") + ylab("Count") +
  ggtitle("Distribution of Weekly Donation Counts by State")


#Time Trend by Median Split
Above.Med = Merge_TNC$state_abbr[which(Merge_TNC$Count_Per_Week > median(Merge_TNC$Count_Per_Week))] 
#Trends by Week
TNC_CLEAN_WEEK %>%
  mutate(MON = as.yearmon(as.character(WEEK_LOW)),
         Above.Median = as.factor(ifelse(state %in% Above.Med, 1,0))) %>%
  group_by(WEEK_LOW, Above.Median) %>%
  summarize(COUNT = sum(COUNT)) %>%
  ggplot() +
  geom_line(aes(x = WEEK_LOW,y = log10(COUNT),group = Above.Median, col = Above.Median)) +
  #geom_vline(xintercept = as.Date(str_c(as.character(2011:2017), "-12-15")),col = "forestgreen",lty = 2) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_date(breaks = as.Date(str_c(as.character(2011:2016), "-12-15")), 
               labels= c("Dec.2011","Dec.2012","Dec.2013","Dec.2014", "Dec.2015","Dec.2016")) +
  xlab("Time") +
  ylab(expression(paste(Log[10], " Count of Donations By Week"))) +
  ggtitle(expression(paste("Total TNC Donation Counts By Week (", Log[10],")")))

#Donations by Month
TNC_CLEAN %>%
  mutate(MON = as.yearmon(as.character(WEEK_LOW)),
         GENEROUS = as.factor(ifelse(state %in% Above.Med, 1,0))) %>%
  group_by(MON, GENEROUS) %>%
  #filter(MON > 2011.5 & MON < 2017.5) %>%
  summarize(LOG_TOT = log(sum(AMT))) %>%
  ggplot() +
  geom_line(aes(x = MON,y = LOG_TOT,group = GENEROUS, col = GENEROUS)) +
  geom_vline(xintercept = c(0.92 + 2011:2016),col = "forestgreen",lty = 2) +
  theme_minimal() +
  scale_color_discrete(name = "Above Median") +
  scale_x_continuous(breaks = c(c(0.92 + 2011:2016)), labels= c("Dec.2011","Dec.2012","Dec.2013","Dec.2014", "Dec.2015","Dec.2016")) +
  xlab("Time") +
  ylab("Log Total Donations") +
  ggtitle("Log-Total TNC Donations by Month")

