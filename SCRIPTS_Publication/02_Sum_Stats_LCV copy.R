#-----------------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------------#
#This Script Computes Univariate Summary Statistics and Figures For Senate and House Voting Data------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------------#

#-------------------------Loading Required Libraries---------------------#
#For proessing
library(tidyverse)
library(ggplot2)
library(dplyr)
library(rlang)

#---------------------Reading in Cleaned Data Generated From Script 1--------------------#
SEN = read.csv("~/Google Drive/DATA/ECON/CLEAN/LCV/SENATE.csv")
HOUSE = read.csv("~/Google Drive/DATA/ECON/CLEAN/LCV/HOUSE.csv")
Combined = map_dfr(.x = c("SEN", "HOUSE"),
                   .f = ~ eval(parse_expr(.x)) %>%
                     dplyr::select(Party, Year, POS_VOTE, ROLL_CALL, Member.of.Congress) %>%
                     mutate(CHAMBER = .x))
glimpse(SEN)
#----------------------------Summary Figures------------------------------------------#
#Number of Votes By Year
Combined %>% group_by(Year, CHAMBER) %>% summarise(COUNT = length(unique(ROLL_CALL))) %>%
  arrange(CHAMBER, COUNT) %>%
  ggplot() + geom_line(aes(x = Year, y = COUNT, col = CHAMBER))
#Number of NAs
Combined %>% mutate(NA_VALS = ifelse(is.na(POS_VOTE),1,0)) %>% group_by(CHAMBER) %>% summarize(NA_PERCENT = mean(NA_VALS))
#NAs by year
Combined %>% mutate(NA_VALS = ifelse(is.na(POS_VOTE),1,0)) %>% group_by(CHAMBER, Year) %>%
  summarize(NA_PERCENT = mean(NA_VALS)) %>% ggplot() + geom_line(aes(x = Year, y = NA_PERCENT, col = CHAMBER))
#NAs by vote
Combined %>%
  mutate(NA_VALS = ifelse(is.na(POS_VOTE),1,0)) %>% group_by(CHAMBER, Year, ROLL_CALL) %>%
  summarize(VOTES = n(),REPS = length(unique(Member.of.Congress)), NA_PERCENT = mean(NA_VALS)) %>%
  mutate(ROLL = Year*1000 + ROLL_CALL) %>%
  filter(NA_PERCENT >0.15)
#NAs by Political Party
Combined %>%
  mutate(NA_VALS = ifelse(is.na(POS_VOTE),1,0),
         REP = ifelse(Party == "R", "R","D")) %>% group_by(REP) %>%
  summarize(NA_PERCENT = mean(NA_VALS))
By_Party

#---------------------Trends in Percentages by Year and Party------------------------@

ByParty_Time = Combined %>% mutate(Party = ifelse(Party == "R", "R", "D/I")) %>%
  group_by(Party, Year, CHAMBER) %>%
  summarize(Positive = 100*mean(POS_VOTE, na.rm = T)) %>%
  ungroup() 
#Plotting
ggplot(ByParty_Time) +
  geom_line(aes(x = Year, y = Positive, 
                col = interaction(Party,CHAMBER),
                lty = interaction(Party,CHAMBER))) +
  ylab("Percent Positive Environmental Votes") +
  scale_color_manual(name = "",
                     values = c("blue", "red", "blue","red"),
                     labels = c("House Democrats", "House Republicans", "Senate Democrats", "Senate Republicans")) +
  scale_linetype_manual(name = "",
                        values = c(1,1,2,2), 
                        labels = c("House Democrats", "House Republicans", "Senate Democrats", "Senate Republicans")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "vertical", legend.text = element_text(size = 11)) 
#Calculating Partisan Divide
ByParty_Time %>% filter(Year == 2015)

#Explanatory Power of Party Only
Combined = Combined %>%
  mutate(REP = ifelse(Party == "R", "R", "D"))

summary(lm(POS_VOTE ~ REP, data = subset(Combined, CHAMBER == "SEN")))$r.squared

#-----------------------Trends in LCV Scores By Party-------------------------#


SEN %>%
  group_by(Party, Year) %>%
  summarise(SCORE = mean(Current.Score, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = Year, y = SCORE, col = Party)) +
  scale_color_manual(values=group.colors) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlab("Year") + ylab("Average LCV Score") +
  ggtitle("LCV Scores By Party Over Time")

#Same Figure but removing independents
SEN %>%
  filter(Party != "I") %>%
  group_by(Party, Year) %>%
  summarise(SCORE = mean(Current.Score, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = Year, y = SCORE, col = Party)) +
  scale_color_manual(values=group.colors) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlab("Year") + ylab("Average LCV Score") +
  ggtitle("LCV Scores By Party Over Time")


#Note: Trend Looks Similar in the House -- Appendix Figure
HOUSE %>%
  group_by(Party, Year) %>%
  summarise(SCORE = mean(Current.Score, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = Year, y = SCORE, col = Party)) +
  scale_color_manual(values=group.colors) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlab("Year") + ylab("Average LCV Score") +
  ggtitle("LCV Scores By Party Over Time")

       
#---------------------------Individual Trends Over Time for Notable, Long-Serving Senators---------------------------------#
People = c("Biden, Joe", "Conrad, Kent", "Hatch, Orrin", "Collins, Susan")
group.colors = c("Biden, Joe" = "blue4", "Conrad, Kent" = "forestgreen", "Hatch, Orrin" = "red4", "Collins, Susan" = "red2")
SEN %>%
  filter(Member.of.Congress %in% People) %>%
  ggplot() +
  geom_line(aes(x = Year, y = Current.Score, group = Member.of.Congress, col = Member.of.Congress, lty = Member.of.Congress)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_colour_manual("", values = c("blue2", "darkorange", "purple", "red")) +
  scale_linetype_manual("", values=c(1,2,5,6)) +
  xlab("Year") + ylab("Average LCV Score") +
  ggtitle("Yearly LCV Scores for Long-Serving Senators")


#Computing Mean and Standard Deviation in LCV Scores for These Four Senators
SEN %>%
  filter(Member.of.Congress %in% People) %>%
  group_by(Year, Member.of.Congress) %>%
  summarize(Mean = mean(Current.Score)) %>%
  group_by(Member.of.Congress) %>%
  summarise(BIG_MEAN = mean(Mean),
            SD = sd(Mean))

#--------------Calculate the Percentage of Reps with Scores in Middle 60 Points----------------#
#For Senate
Mod_Sen = SEN %>%
  filter(Year > 1989) %>%
  group_by(Year, Member.of.Congress) %>%
  summarize(Mean = mean(Current.Score)) %>%
  mutate(Moderate = ifelse(Mean %in% 20:80, 1,0)) 
table(Mod_Sen$Moderate)[2]/nrow(Mod_Sen)
#For House
Mod_House = HOUSE %>%
  filter(Year > 1989) %>%
  group_by(Year, Member.of.Congress) %>%
  summarize(Mean = mean(Current.Score)) %>%
  mutate(Moderate = ifelse(Mean %in% 20:80, 1,0)) 
table(Mod_House$Moderate)[2]/nrow(Mod_House)


#-----------------------------------Number of Votes Per Year--------------------------------#
HOUSE %>%
  distinct(Year, ROLL_CALL) %>%
  group_by(Year) %>%
  summarize(Count = n()) %>%
  ggplot() + 
  geom_line(aes(x = Year, y = Count)) +
  ggtitle("Number of Environmental Votes Per Year - House") +
  theme_bw() + 
  theme(panel.border = element_blank()) 

SEN %>%
  distinct(Year, ROLL_CALL) %>%
  group_by(Year) %>%
  summarize(Count = n()) %>%
  ggplot() + 
  geom_line(aes(x = Year, y = Count)) +
  ggtitle("Number of Environmental Votes Per Year - Senate") +
  theme_bw() + 
  theme(panel.border = element_blank()) 


