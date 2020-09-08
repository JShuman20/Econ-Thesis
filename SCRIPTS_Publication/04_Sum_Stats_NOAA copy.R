#--------------------------------------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------------------------------------------------#
#This Script Provides Univariate Summary Statistics for the Cleaned NOAA Storm Data From Script #3.------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------------------------------------------------#


#-------------------------------Loading Required Libraries-------------------------#
library(tidyverse)
library(lubridate)
library(xtable)
library(zoo)

#------------------------Read in Cleaned Data-----------------------------------#
DAT_CLEAN = read.csv("~/Google Drive/DATA/ECON/CLEAN/NOAA/02_NOAA_CLEANED.csv") 
DAT_CLEAN = mutate(DAT_CLEAN, 
                   BEGIN_DATE_YM = as.yearmon(BEGIN_DATE_YM),
                   YEAR = as.numeric(str_sub(BEGIN_YEARMONTH,1,4))) %>%
  filter(YEAR %in% 1990:2018)  #Restricting to Relevant Years


#----------------------------Some Basic Questions----------------------------#
#Number of Observations
nrow(DAT_CLEAN)

#Number of Unique Categories
unique(DAT_CLEAN$EVENT_TYPE)

#Number with Positive Damages
table(DAT_CLEAN$DAMAGE_PROPERTY >0)[2]/nrow(DAT_CLEAN)

#Range of Values
range(DAT_CLEAN$DAM_PROP_2000, na.rm = TRUE)

#Which States Are Hit Hardest
DAT_CLEAN %>%
  group_by(STATE) %>%
  summarize(AMT = sum(DAMAGE_PROPERTY,na.rm = TRUE)) %>%
  arrange(desc(AMT))


#----------------------------Distribution of Weather Events Across Seasons-----------------------------#
DAT_CLEAN %>%
  mutate(SEASON = case_when(
    MONTH %in% c(12,1,2,3) ~ "WINTER",
    MONTH %in% c(4,5,6) ~ "SPRING",
    MONTH %in% c(7,8,9) ~ "SUMMER",
    MONTH %in% c(10,11,12) ~ "FALL")) %>%
  group_by(SEASON, PLACEBO_EVENT) %>% 
  summarize(COUNT = n()/28, AMT = sum(DAM_PROP_2000,na.rm = TRUE)/(28*1000000)) %>%
  pivot_wider(names_from = PLACEBO_EVENT, values_from = c(COUNT,AMT)) %>%
  mutate(COUNT_TOTAL = COUNT_0+COUNT_1, AMT_TOTAL = AMT_0+AMT_1) %>%
  select(1,6,7,2,4,3,5) %>%
  xtable(., caption = "Average Yearly Count and Damage Amount by Season") %>%
  print(., file = "~/Desktop/ECON Thesis/OUTPUT/DATA_Section/Appendix/Storms_by_season.tex")

#-----------------------------Some Information About Cold Weather Events---------------------#
#01. Percentage of States that Have a Cold Weather Event -- 18.5%
table(DAT_CLEAN$PLACEBO_EVENT)/nrow(DAT_CLEAN)
#02. Percentage of Damaging Events -- 6.2%
DAT_CLEAN %>% filter(DAM_PROP_2000 > 0) %>% group_by(PLACEBO_EVENT) %>% summarize(COUNT = n())
#Percent of Overall Damages
DAT_CLEAN %>% filter(DAM_PROP_2000 > 0) %>% group_by(PLACEBO_EVENT) %>% summarize(COUNT = sum(DAM_PROP_2000))
DAT_CLEAN %>%
  group_by(STATE) %>%
  summarise(N_COLD = sum(PLACEBO_EVENT)) %>% arrange(desc(N_COLD))

#--------------------------------Time Trends for Above and Below-Median States------------------------------------#
#Identifying Above Median Damage States Over Time
ABOVE_MED_STATES = DAT_CLEAN %>%
  group_by(STATE) %>%
  summarise(LOG_TOT_PROP = log(1 + sum(DAM_PROP_2000,na.rm = TRUE))) %>%
  mutate(ABOVE_MED = ifelse(LOG_TOT_PROP > median(LOG_TOT_PROP),1,0)) %>%
  filter(ABOVE_MED == 1) %>%
  pull(STATE)

#-----------------------------------Comparing CPI-ADjust to Regular-------------------#
DAT_CLEAN %>%
  group_by(BEGIN_DATE_YM) %>%
  summarize(TOTAL = log(1 + sum(DAMAGE_PROPERTY, na.rm = T)),
            TOT_2000 = log(1 + sum(DAM_PROP_2000,na.rm = T))) %>%
  ggplot() +
  geom_line(aes(x = BEGIN_DATE_YM, y = TOTAL), col = "red") +
  geom_line(aes(x = BEGIN_DATE_YM, y = TOT_2000), col = "blue")

#Checking 2018 Storms
DAT_CLEAN %>% filter(YEAR == "2018") %>%
  summarize(TOTAL = log(1 + sum(DAMAGE_PROPERTY, na.rm = T)),
            TOT_2000 = log(1 + sum(DAM_PROP_2000,na.rm = T))) 


  

#Plotting Time Trends By Above Median
DAT_CLEAN %>%
  group_by(BEGIN_DATE_YM, STATE)  %>%
  summarize(TOT_PROP = sum(DAM_PROP_2000, na.rm = TRUE)) %>%
  mutate(LOG_TOT = log(TOT_PROP + 1),
         `Above Median` = ifelse(STATE %in% ABOVE_MED_STATES, "Above Median", "Below Median")) %>%
  group_by(`Above Median`, BEGIN_DATE_YM) %>%
  summarise(TOT_PROP = sum(TOT_PROP),
            LOG_TOT = log(TOT_PROP + 1))  %>%
  ggplot() + 
  geom_line(aes(x = BEGIN_DATE_YM, y = LOG_TOT, group = `Above Median`, col = `Above Median`, lty = `Above Median`)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_colour_discrete("") +
  scale_linetype_manual("", values=c(1,2)) +
  xlab("Time") + ylab("Log-Total Property Damage") +
  ggtitle("Log-Total Property Damage By Month") 
   
#------------------------------- Within-State Histograms----------------------------------------#
DAT_CLEAN %>%
  filter(STATE %in% c("MAINE", "NORTH DAKOTA", "DELAWARE", "UTAH", "FLORIDA")) %>%
  group_by(STATE, BEGIN_DATE_YM) %>%
  summarize(TOT_PROP = sum(DAM_PROP_2000, na.rm = TRUE)) %>%
  mutate(LOG_TOT = log(TOT_PROP + 1)) %>%
  ggplot() + 
  geom_histogram(aes(x = LOG_TOT), bins = 20, fill = "black") +
  facet_grid(~STATE) +
  theme_bw() +
  xlab("Monthly Log-Total Property Damage") +
  ylab("Count") +
  ggtitle("Distribution of Monthly Property Damages For Selected States")



#-------------------------------------------Appendix Tables----------------------------------------#
#Most Frequent Storm Types
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
    
#Most Damaging Storm Types
DAT_CLEAN %>%
  filter(DAM_PROP_2000 > 0) %>%
  group_by(EVENT_TYPE) %>%
  summarize(DAMAGE = sum(DAM_PROP_2000,na.rm = TRUE)/1000000) %>%
  arrange(desc(DAMAGE)) %>%
  mutate(EVENT_TYPE = tolower(EVENT_TYPE)) %>%
  rename(`Storm Class` = EVENT_TYPE) %>%
  filter(row_number() <= 10) %>%
  xtable(., caption = "Most Damaging Types of Storms") %>%
  print(., file = "~/Desktop/ECON Thesis/OUTPUT/DATA_Section/Appendix/DamageStats.tex", append = TRUE)
  
