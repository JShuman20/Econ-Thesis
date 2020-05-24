#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#
#This Script Produced Regression Results For Section 3
#------------------------------------------------------------------------------------------------------------------------------------#

#--------------------------Loading Packages----------------#
library(tidyverse)
library(lubridate)
library(stargazer)
library(car)
library(sandwich)
library(lmetest)
library(plm)
library(expss)


#--------------------------------------------------Read in Senate Data and Construct Variables For Regressions--------------------------------#
SENATE_DAT_ALL = read.csv("~/Google Drive/DATA/ECON/CLEAN/FULL_LAGGED/Sen_All_Lags.csv")%>%
  dplyr::select(Party, ROLL_CALL, DATE, State, Member.of.Congress, Year, POS_VOTE, MON_NUM, 
         Lag_0_30_END, Lag_0_30_END_PLACEBO, Lag_0_30_END_NOT_PLACEBO, TNC_Lag_0_30) %>%
  mutate(Lag_0_30_END = log(Lag_0_30_END + 1),
         Lag_0_30_END_PLACEBO = log(Lag_0_30_END_PLACEBO + 1),
         Lag_0_30_END_NOT_PLACEBO = log(Lag_0_30_END_NOT_PLACEBO + 1),
         Lag_0_30_END_BIN = ifelse(Lag_0_30_END > 0,1,0), #Indicator For Positive
         Lag_0_30_END_PLACEBO_BIN = ifelse(Lag_0_30_END_PLACEBO  > 0,1,0), #Indicator For Positive
         Lag_0_30_END_NOT_PLACEBO_BIN = ifelse(Lag_0_30_END_NOT_PLACEBO>0,1,0), #Indicator For Positive
         BIN_30_Mil = ifelse( (Lag_0_30_END > 0 & Lag_0_30_END < log(1000000)),1,0),
         BIN_30_G_MIL = ifelse(Lag_0_30_END > log(1000000),1,0),
         BIN_30_FiftyMil = ifelse( (Lag_0_30_END > log(1000000) & Lag_0_30_END < log(50000000)), 1,0),
         BIN_30_G_FiftyMil = ifelse(Lag_0_30_END >log(50000000),1,0),
         BIN_30_FHMil = ifelse( (Lag_0_30_END > log(50000000) & Lag_0_30_END < log(400000000)), 1,0),
         BIN_30_Huge = ifelse(Lag_0_30_END > log(400000000), 1,0),
         TNC_Lag_0_30_Per100 = TNC_Lag_0_30/100) %>%  #Binned Damages
  mutate(DATE = as.Date(DATE)) %>%
  mutate(SEASON = case_when(
    MON_NUM %in% c(12,1,2,3) ~ "WINTER",
    MON_NUM %in% c(4,5,6) ~ "SPRING",
    MON_NUM %in% c(7,8,9) ~ "SUMMER",
    MON_NUM %in% c(10,11,12) ~ "FALL")) %>% #Season indicators
  mutate(PANEL_VAR = 100000*Year + ROLL_CALL)  %>% #Time Variable
  mutate(REP = ifelse(Party == "R",1,0)) %>% #Republican indicator
  mutate(State = as.factor(State))


#--------------------------------------------------Read in House Data and Construct Variables For Regressions--------------------------------#
HOUSE_DAT_ALL = read.csv("~/Google Drive/DATA/ECON/CLEAN/FULL_LAGGED/House_All_Lags.csv") %>%
  dplyr::select(Party, ROLL_CALL, DATE, State, Member.of.Congress, Year, POS_VOTE, MON_NUM, 
         Lag_0_30_END, Lag_0_30_END_PLACEBO, Lag_0_30_END_NOT_PLACEBO, TNC_Lag_0_30) %>%
  mutate(Lag_0_30_END = log(Lag_0_30_END + 1),
         Lag_0_30_END_PLACEBO = log(Lag_0_30_END_PLACEBO + 1),
         Lag_0_30_END_NOT_PLACEBO = log(Lag_0_30_END_NOT_PLACEBO + 1),
         Lag_0_30_END_BIN = ifelse(Lag_0_30_END > 0,1,0),
         Lag_0_30_END_PLACEBO_BIN = ifelse(Lag_0_30_END_PLACEBO  > 0,1,0), #Positive Indicator
         Lag_0_30_END_NOT_PLACEBO_BIN = ifelse(Lag_0_30_END_NOT_PLACEBO>0,1,0),#Positive Indicator
         BIN_30_Mil = ifelse( (Lag_0_30_END > 0 & Lag_0_30_END < log(1000000)),1,0),#Positive Indicator
         BIN_30_G_MIL = ifelse(Lag_0_30_END > log(1000000),1,0),
         BIN_30_FiftyMil = ifelse( (Lag_0_30_END > log(1000000) & Lag_0_30_END < log(50000000)), 1,0),
         BIN_30_G_FiftyMil = ifelse(Lag_0_30_END >log(50000000),1,0),
         BIN_30_FHMil = ifelse( (Lag_0_30_END > log(50000000) & Lag_0_30_END < log(400000000)), 1,0),
         BIN_30_Huge = ifelse(Lag_0_30_END > log(400000000), 1,0), #Binned Values
         TNC_Lag_0_30_Per100 = TNC_Lag_0_30/100) %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(SEASON = case_when(
    MON_NUM %in% c(12,1,2,3) ~ "WINTER",
    MON_NUM %in% c(4,5,6) ~ "SPRING",
    MON_NUM %in% c(7,8,9) ~ "SUMMER",
    MON_NUM %in% c(10,11,12) ~ "FALL")) %>% #Season Indicators
  mutate(PANEL_VAR = 100000*Year + ROLL_CALL)  %>% #Time Variable
  mutate(REP = ifelse(Party == "R",1,0)) %>% #Republican indicator
  mutate(Member.of.Congress = as.character.factor(Member.of.Congress)) %>%
  mutate(Member.of.Congress = case_when(
    Member.of.Congress == "Rogers, Mike"  & State == "AL" ~ "Rogers, Mike_AL",
    Member.of.Congress == "Brown, George" & State == "CO" ~ "Brown, George_CO",
    TRUE ~ Member.of.Congress))%>% #This is an Extra Catch -- didnt seem to get caught the first time
  mutate(REP = ifelse(Party == "R",1,0)) %>% 
  mutate(State = as.factor(State))

#------------------------------Reading in The Population Estimates by Year------------------------#
#Note: Senate and House Data Look a little different, so merging require separate processes
Census = read.csv("~/Google Drive/DATA/ECON/STATE_POP.csv") %>%
  rename(Year = YEAR, State = State.Name) %>%
  dplyr::select(State, Year, POP)
#Scaling Lagged Donations By Millions of Residents
HOUSE_DAT_ALL = HOUSE_DAT_ALL %>%
  left_join(Census, by = c("State", "Year")) %>%
  mutate(TNC_Lag_0_30_PER_MIL_POP = 1000000*(TNC_Lag_0_30/POP))
      
Census = read.csv("~/Google Drive/DATA/ECON/STATE_POP.csv") %>%
  rename(Year = YEAR, State = CODE) %>%
  dplyr::select(State, Year, POP)
#Scaling Lagged Donations By Millions of Residents
SENATE_DAT_ALL = SENATE_DAT_ALL %>%
  left_join(Census, by = c("State", "Year")) %>%
  mutate(TNC_Lag_0_30_PER_MIL_POP = (1000000 * TNC_Lag_0_30/POP))
  

#-----------------------------------------Creating table of Means for Section 3 ----------------------------#
#House
H_ToM = HOUSE_DAT_ALL %>%
  dplyr::select(POS_VOTE, TNC_Lag_0_30_PER_MIL_POP, Lag_0_30_END) %>%
  filter(!is.na(POS_VOTE)) %>%
  mutate(Lag_0_30_END = Lag_0_30_END/1000000) %>%
  as.matrix()
#Senate
head(H_ToM)
S_ToM = SENATE_DAT_ALL %>%
  dplyr::select(POS_VOTE, TNC_Lag_0_30_PER_MIL_POP, Lag_0_30_END) %>%
  mutate(Lag_0_30_END = Lag_0_30_END/1000000) %>%
  filter(!is.na(POS_VOTE)) %>%
  as.matrix()

ALL = matrix(nrow = nrow(H_ToM), ncol = 6)
ALL[,1:3] = H_ToM
ALL[1:nrow(S_ToM),4:6] = S_ToM

#Printing Results
ALL = as.data.frame(ALL) %>%
  stargazer(., type = "latex",style = "aer", 
            title = "Summary Stats for Restricted Date Range",
            summary.stat = c("n", "mean","min","max","sd"),
            covariate.labels = c("House Vote", "30-Day Lagged TNC (House)", "30-Day Lagged Damage House","Senate Vote", "30-Day Lagged TNC (Senate)", "30-Day Lagged Damage (Senate)"),
            out = "~/Desktop/ECON Thesis/OUTPUT/DATA_Section/Table_of_Means.tex")



#---------------------------------------------Showing Decrease in Voting Variation Within Rep ---------------------------------#
library(ggplot2)

#Restricted Houe Sample
HOUSE_DAT_ALL %>%
  group_by(Member.of.Congress, Party) %>%
  summarise(Number_votes = n(),
            N_POS = sum(POS_VOTE, na.rm = TRUE)) %>%
  mutate(Percent = N_POS/Number_votes) %>%
  mutate(Binned_Percent = cut(Percent, breaks = seq(0,1,0.1),include.lowest = TRUE)) %>%
  ggplot() +
  geom_bar(aes(fill = Party, x = Binned_Percent, y =stat(count)/sum(stat(count))), position = "stack") +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.5)) + 
  theme_minimal()  +
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_manual(values  = c("Blue", "Red")) +
  xlab("Percent of Positive Votes By Representative") +
  ylab("Percentage of Observations") +
  ggtitle("Distribution of Pro-Environmental Voting Percentage \n By Representative")

#Full House Sample
read.csv("~/Google Drive/DATA/ECON/CLEAN/HOUSE_LAGGED.csv") %>%
  mutate(Party = ifelse(Party == "R","R","D")) %>%
  group_by(Member.of.Congress, Party) %>%
  summarise(Number_votes = n(),
            N_POS = sum(POS_VOTE, na.rm = TRUE)) %>%
  mutate(Percent = N_POS/Number_votes) %>%
  mutate(Binned_Percent = cut(Percent, breaks = seq(0,1,0.1),include.lowest = TRUE)) %>%
  ggplot() +
  geom_bar(aes(fill = Party, x = Binned_Percent, y =stat(count)/sum(stat(count))), position = "stack") +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.45)) + 
  theme_minimal()  +
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_manual(values  = c("Blue", "Red")) +
  xlab("Percent of Positive Votes By Representative") +
  ylab("Percentage of Observations") +
  ggtitle("Distribution of Pro-Environmental Voting Percentage \n By Representative")

#Restricted Senate Sample
SENATE_DAT_ALL %>%
  mutate(Party = ifelse(Party == "R","R","D")) %>%
  group_by(Member.of.Congress, Party) %>%
  summarise(Number_votes = n(),
            N_POS = sum(POS_VOTE, na.rm = TRUE)) %>%
  mutate(Percent = N_POS/Number_votes) %>%
  mutate(Binned_Percent = cut(Percent, breaks = seq(0,1,0.1),include.lowest = TRUE)) %>%
  ggplot() +
  geom_bar(aes(fill = Party, x = Binned_Percent, y =stat(count)/sum(stat(count))), position = "stack") +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.4)) + 
  theme_minimal()  +
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_manual(values  = c("Blue", "Red")) +
  xlab("Percent of Positive Votes By Representative") +
  ylab("Percentage of Observations") +
  ggtitle("Distribution of Pro-Environmental Voting Percentage \n By Representative")

#Full Senate Sample
read.csv("~/Google Drive/DATA/ECON/CLEAN/SENATE_LAGGED.csv") %>%
  mutate(Party = ifelse(Party == "R","R","D")) %>%
  group_by(Member.of.Congress, Party) %>%
  summarise(Number_votes = n(),
            N_POS = sum(POS_VOTE, na.rm = TRUE)) %>%
  mutate(Percent = N_POS/Number_votes) %>%
  mutate(Binned_Percent = cut(Percent, breaks = seq(0,1,0.1),include.lowest = TRUE)) %>%
  ggplot() +
  geom_bar(aes(fill = Party, x = Binned_Percent, y =stat(count)/sum(stat(count))), position = "stack") +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.4)) + 
  theme_minimal()  +
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_manual(values  = c("Blue", "Red")) +
  xlab("Percent of Positive Votes By Representative") +
  ylab("Percentage of Observations") +
  ggtitle("Distribution of Pro-Environmental Voting Percentage \n By Representative")


#----------------------------Defining Clustered, Robust SE Method For linear Model--------------------#
GET_SE_LM = function(MOD){
  MAT = vcovCL(MOD, cluster = attr(MOD, "State"), type = "HC1")
  return(sqrt(diag(MAT)))
}




#---------------------------------------------------Main Regression Table-----------------------------------#
#List of Regressions
Table3ALL = list(
  A = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL),
  B = lm(POS_VOTE ~ TNC_Lag_0_30_PER_MIL_POP*REP + factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL),
  C = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_PER_MIL_POP*REP + factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL),
  D = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL),
  E = lm(POS_VOTE ~ TNC_Lag_0_30_PER_MIL_POP*REP + factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL),
  G = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_PER_MIL_POP*REP + factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL)
)
#Test Coefficient Equality
linearHypothesis(Table3ALL$D, "Lag_0_30_END_BIN + Lag_0_30_END_BIN:REP = 0", white.adjust = "hc1")
#Clustered and Robust SE
Table3ALL_SE = lapply(X = Table3ALL, GET_SE_LM)
#Outputting Results
OUT = stargazer(Table3ALL, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table3ALL_SE,
                title = "Voting on Weather and Opinion by Party -- House",
                dep.var.labels = "Positive Environmental Vote")

cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/COMBINED/Table_3_FINAL.tex", append = TRUE)

#---------------------------------------------Robustness: Polynomial----------------------------------------#
#List of Regressions
Polynomial_Rob = list(
  Square = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_PER_MIL_POP*REP + I(TNC_Lag_0_30_PER_MIL_POP^2)*REP + factor(Year) + factor(MON_NUM) + factor(State), 
              data = HOUSE_DAT_ALL),
  Cube   = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_PER_MIL_POP*REP + I(TNC_Lag_0_30_PER_MIL_POP^2)*REP + I(TNC_Lag_0_30_PER_MIL_POP^3)*REP + factor(Year) + factor(MON_NUM) + factor(State),
              data = HOUSE_DAT_ALL),
  Quart  = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_PER_MIL_POP*REP + I(TNC_Lag_0_30_PER_MIL_POP^2)*REP + I(TNC_Lag_0_30_PER_MIL_POP^3)*REP + I(TNC_Lag_0_30_PER_MIL_POP^4)*REP +
                factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL),
  #First House
  Square1 = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_PER_MIL_POP*REP + I(TNC_Lag_0_30_PER_MIL_POP^2)*REP + factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL),
  Cube1   = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_PER_MIL_POP*REP + I(TNC_Lag_0_30_PER_MIL_POP^2)*REP + I(TNC_Lag_0_30_PER_MIL_POP^3)*REP + factor(Year) + factor(MON_NUM) + factor(State),
               data = SENATE_DAT_ALL),
  Quart1  = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_PER_MIL_POP*REP + I(TNC_Lag_0_30_PER_MIL_POP^2)*REP + I(TNC_Lag_0_30_PER_MIL_POP^3)*REP + I(TNC_Lag_0_30_PER_MIL_POP^4)*REP +
                 factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL)
)
#Standard Errors
Polynomial_Rob_SE = lapply(Polynomial_Rob, GET_SE_LM)
#Outputting Results
OUT = stargazer(Polynomial_Rob, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = c("factor*"), keep.stat = c("n","adj.rsq"), se = Polynomial_Rob_SE,
                title = "Sensitivity to Polynomials in Donations", dep.var.labels = "Positive Environmental Vote")
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/COMBINED/App_Polynomial.tex")      



#-------------------------------------------------Robustness: Logit Specification------------------------------------#
#List of Regressions
Table3ALL_APP = list(
  A = glm(POS_VOTE ~ Lag_0_30_END_BIN*REP + factor(Year) + factor(MON_NUM) + factor(State), 
          data = HOUSE_DAT_ALL, family = "binomial"),
  B = glm(POS_VOTE ~ TNC_Lag_0_30_PER_MIL_POP*REP + factor(Year) + factor(MON_NUM) + factor(State), 
          data = HOUSE_DAT_ALL, family = "binomial"),
  C = glm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_PER_MIL_POP*REP + factor(Year) + factor(MON_NUM) + factor(State), 
          data = HOUSE_DAT_ALL, family = "binomial"),
  D = glm(POS_VOTE ~ Lag_0_30_END_BIN*REP + factor(Year) + factor(MON_NUM) + factor(State), 
          data = SENATE_DAT_ALL, family = "binomial"),
  E = glm(POS_VOTE ~ TNC_Lag_0_30_PER_MIL_POP*REP + factor(Year) + factor(MON_NUM) + factor(State), 
          data = SENATE_DAT_ALL, family = "binomial"),
  G = glm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_PER_MIL_POP*REP + factor(Year) + factor(MON_NUM) + factor(State), 
          data = SENATE_DAT_ALL, family = "binomial")
)
#Test Coefficient Equality
linearHypothesis(Table3ALL$D, "Lag_0_30_END_BIN + Lag_0_30_END_BIN:REP = 0", white.adjust = "hc1")
#Clustered and Robust SE
Table3ALL_APP_SE = lapply(X = Table3ALL_APP, GET_SE_LM)
#Outputting Results
OUT = stargazer(Table3ALL_APP, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table3ALL_APP_SE,
                title = "Voting on Weather and Opinion by Party -- House",
                dep.var.labels = "Positive Environmental Vote")

cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/COMBINED/APP_FINAL_LOGIT.tex", append = TRUE)












