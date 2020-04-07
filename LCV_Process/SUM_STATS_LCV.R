library(tidyverse)
library(readxl)
library(foreign)

SEN = read.csv("~/Google Drive/DATA/ECON/CLEAN/LCV/SENATE.csv")
HOUSE = read.csv("~/Google Drive/DATA/ECON/CLEAN/LCV/HOUSE.csv")

#Trends By Party
group.colors = c(D = "blue", R = "red", I = "darkgreen")

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

#Note: Trend Looks Similar in the House
       
#Individual Trends Over Time for Notable, Long-Serving Senators
People = c("Biden, Joe", "Conrad, Kent", "Hatch, Orrin", "Collins, Susan")
group.colors = c("Biden, Joe" = "blue4", "Conrad, Kent" = "lightblue", "Hatch, Orrin" = "red4", "Collins, Susan" = "red2")
SEN %>%
  filter(Member.of.Congress %in% People) %>%
  ggplot() +
  geom_line(aes(x = Year, y = Current.Score, group = Member.of.Congress, col = Member.of.Congress)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlab("Year") + ylab("Average LCV Score") +
  ggtitle("Yearly LCV Scores for Long-Serving Senators")
#
SEN %>%
  filter(Member.of.Congress %in% People) %>%
  group_by(Year, Member.of.Congress) %>%
  summarize(Mean = mean(Current.Score)) %>%
  group_by(Member.of.Congress) %>%
  summarise(BIG_MEAN = mean(Mean),
            SD = sd(Mean))
#Percentage of Voters in Middle 60%
#Let's Restrict to post-1990
Mod_Sen = SEN %>%
  filter(Year > 1989) %>%
  group_by(Year, Member.of.Congress) %>%
  summarize(Mean = mean(Current.Score)) %>%
  mutate(Moderate = ifelse(Mean %in% 20:80, 1,0)) 
table(Mod_Sen$Moderate)[2]/nrow(Mod_Sen)
Mod_House = HOUSE %>%
  filter(Year > 1989) %>%
  group_by(Year, Member.of.Congress) %>%
  summarize(Mean = mean(Current.Score)) %>%
  mutate(Moderate = ifelse(Mean %in% 20:80, 1,0)) 
table(Mod_House$Moderate)[2]/nrow(Mod_House)


#Number of Votes Per Year
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



unique(SEN$Year)


