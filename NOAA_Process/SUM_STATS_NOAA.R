library(tidyverse)
library(xtable)
library(zoo)

#Read in Data
DAT_CLEAN = read.csv("~/Google Drive/DATA/ECON/CLEAN/NOAA/02_NOAA_CLEANED.csv") 
DAT_CLEAN = mutate(DAT_CLEAN, BEGIN_DATE_YM = as.yearmon(BEGIN_DATE_YM),
                   YEAR = as.numeric(str_sub(BEGIN_YEARMONTH,1,4))) %>%
  filter(YEAR %in% 1990:2018)

#Number of Observations
nrow(DAT_CLEAN)
#Number of Unique Categories
unique(DAT_CLEAN$EVENT_TYPE)
#Number with Positive Damages
table(DAT_CLEAN$DAMAGE_PROPERTY >0)[2]/nrow(DAT_CLEAN)
#Range of Values
range(DAT_CLEAN$DAMAGE_PROPERTY, na.rm = TRUE)


glimpse(DAT_CLEAN)
#Distribution of Weather Events Across Seasons
DAT_CLEAN %>%
  mutate(SEASON = case_when(
    MONTH %in% c(12,1,2,3) ~ "WINTER",
    MONTH %in% c(4,5,6) ~ "SPRING",
    MONTH %in% c(7,8,9) ~ "SUMMER",
    MONTH %in% c(10,11,12) ~ "FALL")) %>%
  group_by(SEASON, PLACEBO_EVENT) %>% summarize(COUNT = n()/28, AMT = sum(DAMAGE_PROPERTY,na.rm = TRUE)/(28*1000000)) %>%
  pivot_wider(names_from = PLACEBO_EVENT, values_from = c(COUNT,AMT)) %>%
  mutate(COUNT_TOTAL = COUNT_0+COUNT_1, AMT_TOTAL = AMT_0+AMT_1) %>%
  select(1,6,7,2,4,3,5) %>%
  xtable(., caption = "Average Yearly Count and Damage Amount by Season") %>%
  print(., file = "~/Desktop/ECON Thesis/OUTPUT/DATA_Section/Appendix/Storms_by_season.tex")


#Identifying Above Median Damage States Over Time
ABOVE_MED_STATES = DAT_CLEAN %>%
  group_by(STATE) %>%
  summarise(LOG_TOT_PROP = log(1 + sum(DAMAGE_PROPERTY,na.rm = TRUE))) %>%
  mutate(ABOVE_MED = ifelse(LOG_TOT_PROP > median(LOG_TOT_PROP),1,0)) %>%
  filter(ABOVE_MED == 1) %>%
  pull(STATE)

#Plotting Time Trends By Above Median
DAT_CLEAN %>%
  group_by(BEGIN_DATE_YM, STATE)  %>%
  summarize(TOT_PROP = sum(DAMAGE_PROPERTY, na.rm = TRUE)) %>%
  mutate(LOG_TOT = log(TOT_PROP + 1),
         `Above Median` = ifelse(STATE %in% ABOVE_MED_STATES, TRUE,FALSE)) %>%
  group_by(`Above Median`, BEGIN_DATE_YM) %>%
  summarise(TOT_PROP = sum(TOT_PROP),
            LOG_TOT = log(TOT_PROP + 1))  %>%
  ggplot() + 
  geom_line(aes(x = BEGIN_DATE_YM, y = LOG_TOT, group = `Above Median`, col = `Above Median`)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlab("Time") + ylab("Log-Total Property Damage") +
  ggtitle("Log-Total Property Damage By Month") 
   
#Plotting Some Within-State Histograms
DAT_CLEAN %>%
  filter(STATE %in% c("MAINE", "NORTH DAKOTA", "DELAWARE", "UTAH", "FLORIDA")) %>%
  group_by(STATE, BEGIN_DATE_YM) %>%
  summarize(TOT_PROP = sum(DAMAGE_PROPERTY, na.rm = TRUE)) %>%
  mutate(LOG_TOT = log(TOT_PROP + 1)) %>%
  ggplot() + 
  geom_histogram(aes(x = LOG_TOT), bins = 20, fill = "black") +
  facet_grid(~STATE) +
  theme_bw() +
  xlab("Monthly Log-Total Property Damage") +
  ylab("Count") +
  ggtitle("Distribution of Monthly Property Damages For Selected States")



#Appendix Tables
#Types of Storms that Cause Damage
DAT_CLEAN %>%
  #filter(DAMAGE_PROPERTY > 0) %>%
  group_by(EVENT_TYPE) %>%
  summarize(Count = n())%>%
  arrange(desc(Count)) %>%
  mutate(EVENT_TYPE = tolower(EVENT_TYPE)) %>%
  rename(`Storm Class` = EVENT_TYPE) %>%
  filter(row_number() <= 10)  %>%
  xtable(., caption = "Most Frequent Types of Storms", type = "latex") %>%
  print(., file = "~/Desktop/ECON Thesis/OUTPUT/DATA_Section/Appendix/DamageStats.tex", append = TRUE)
    

DAT_CLEAN %>%
  filter(DAMAGE_PROPERTY > 0) %>%
  group_by(EVENT_TYPE) %>%
  summarize(DAMAGE = sum(DAMAGE_PROPERTY,na.rm = TRUE)/1000000) %>%
  arrange(desc(DAMAGE)) %>%
  mutate(EVENT_TYPE = tolower(EVENT_TYPE)) %>%
  rename(`Storm Class` = EVENT_TYPE) %>%
  filter(row_number() <= 10) %>%
  xtable(., caption = "Most Damaging Types of Storms") %>%
  print(., file = "~/Desktop/ECON Thesis/OUTPUT/DATA_Section/Appendix/DamageStats.tex", append = TRUE)
  



           