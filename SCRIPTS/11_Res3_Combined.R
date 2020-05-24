library(tidyverse)
library(lubridate)
#need package for standard errors
library(stargazer)
library(plm)
library(expss)
library(survival)


SENATE_DAT_ALL = read.csv("~/Google Drive/DATA/ECON/CLEAN/FULL_LAGGED/Sen_All_Lags.csv")%>%
  select(Party, ROLL_CALL, DATE, State, Member.of.Congress, Year, POS_VOTE, MON_NUM, Lag_0_30_END, Lag_0_30_END_PLACEBO, Lag_0_30_END_NOT_PLACEBO, TNC_Lag_0_30) %>%
  mutate(Lag_0_30_END = log(Lag_0_30_END + 1),
         Lag_0_30_END_PLACEBO = log(Lag_0_30_END_PLACEBO + 1),
         Lag_0_30_END_NOT_PLACEBO = log(Lag_0_30_END_NOT_PLACEBO + 1),
         Lag_0_30_END_BIN = ifelse(Lag_0_30_END > 0,1,0),
         Lag_0_30_END_PLACEBO_BIN = ifelse(Lag_0_30_END_PLACEBO  > 0,1,0),
         Lag_0_30_END_NOT_PLACEBO_BIN = ifelse(Lag_0_30_END_NOT_PLACEBO>0,1,0),
         BIN_30_Mil = ifelse( (Lag_0_30_END > 0 & Lag_0_30_END < log(1000000)),1,0),
         BIN_30_G_MIL = ifelse(Lag_0_30_END > log(1000000),1,0),
         BIN_30_FiftyMil = ifelse( (Lag_0_30_END > log(1000000) & Lag_0_30_END < log(50000000)), 1,0),
         BIN_30_G_FiftyMil = ifelse(Lag_0_30_END >log(50000000),1,0),
         BIN_30_FHMil = ifelse( (Lag_0_30_END > log(50000000) & Lag_0_30_END < log(400000000)), 1,0),
         BIN_30_Huge = ifelse(Lag_0_30_END > log(400000000), 1,0),
         TNC_Lag_0_30_Per100 = TNC_Lag_0_30/100) %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(SEASON = case_when(
    MON_NUM %in% c(12,1,2,3) ~ "WINTER",
    MON_NUM %in% c(4,5,6) ~ "SPRING",
    MON_NUM %in% c(7,8,9) ~ "SUMMER",
    MON_NUM %in% c(10,11,12) ~ "FALL")) %>%
  mutate(PANEL_VAR = 100000*Year + ROLL_CALL)  %>%
  mutate(REP = ifelse(Party == "R",1,0)) %>%
  mutate(State = as.factor(State))


HOUSE_DAT_ALL = read.csv("~/Google Drive/DATA/ECON/CLEAN/FULL_LAGGED/House_All_Lags.csv") %>%
  select(Party, ROLL_CALL, DATE, State, Member.of.Congress, Year, POS_VOTE, MON_NUM, Lag_0_30_END, Lag_0_30_END_PLACEBO, Lag_0_30_END_NOT_PLACEBO, TNC_Lag_0_30) %>%
  mutate(Lag_0_30_END = log(Lag_0_30_END + 1),
         Lag_0_30_END_PLACEBO = log(Lag_0_30_END_PLACEBO + 1),
         Lag_0_30_END_NOT_PLACEBO = log(Lag_0_30_END_NOT_PLACEBO + 1),
         Lag_0_30_END_BIN = ifelse(Lag_0_30_END > 0,1,0),
         Lag_0_30_END_PLACEBO_BIN = ifelse(Lag_0_30_END_PLACEBO  > 0,1,0),
         Lag_0_30_END_NOT_PLACEBO_BIN = ifelse(Lag_0_30_END_NOT_PLACEBO>0,1,0),
         BIN_30_Mil = ifelse( (Lag_0_30_END > 0 & Lag_0_30_END < log(1000000)),1,0),
         BIN_30_G_MIL = ifelse(Lag_0_30_END > log(1000000),1,0),
         BIN_30_FiftyMil = ifelse( (Lag_0_30_END > log(1000000) & Lag_0_30_END < log(50000000)), 1,0),
         BIN_30_G_FiftyMil = ifelse(Lag_0_30_END >log(50000000),1,0),
         BIN_30_FHMil = ifelse( (Lag_0_30_END > log(50000000) & Lag_0_30_END < log(400000000)), 1,0),
         BIN_30_Huge = ifelse(Lag_0_30_END > log(400000000), 1,0),
         TNC_Lag_0_30_Per100 = TNC_Lag_0_30/100) %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(SEASON = case_when(
    MON_NUM %in% c(12,1,2,3) ~ "WINTER",
    MON_NUM %in% c(4,5,6) ~ "SPRING",
    MON_NUM %in% c(7,8,9) ~ "SUMMER",
    MON_NUM %in% c(10,11,12) ~ "FALL")) %>%
  mutate(PANEL_VAR = 100000*Year + ROLL_CALL)  %>%
  mutate(REP = ifelse(Party == "R",1,0)) %>%
  mutate(Member.of.Congress = as.character.factor(Member.of.Congress)) %>%
  mutate(Member.of.Congress = case_when(
    Member.of.Congress == "Rogers, Mike"  & State == "AL" ~ "Rogers, Mike_AL",
    Member.of.Congress == "Brown, George" & State == "CO" ~ "Brown, George_CO",
    TRUE ~ Member.of.Congress))%>% #This is an Extra Catch -- didnt seem to get caught the first time
  mutate(REP = ifelse(Party == "R",1,0)) %>%
  mutate(State = as.factor(State))

#Reading in The Population Estimates by Year
Census = read.csv("~/Google Drive/DATA/ECON/STATE_POP.csv") %>%
  rename(Year = YEAR, State = State.Name) %>%
  select(State, Year, POP)

HOUSE_DAT_ALL = HOUSE_DAT_ALL %>%
  left_join(Census, by = c("State", "Year")) %>%
  mutate(TNC_Lag_0_30_PER_MIL_POP = 1000000*(TNC_Lag_0_30/POP))
      
Census = read.csv("~/Google Drive/DATA/ECON/STATE_POP.csv") %>%
  rename(Year = YEAR, State = CODE) %>%
  select(State, Year, POP)
SENATE_DAT_ALL = SENATE_DAT_ALL %>%
  left_join(Census, by = c("State", "Year")) %>%
  mutate(TNC_Lag_0_30_PER_MIL_POP = (1000000 * TNC_Lag_0_30/POP))
  
H_ToM = HOUSE_DAT_ALL %>%
  dplyr::select(POS_VOTE, TNC_Lag_0_30_PER_MIL_POP, Lag_0_30_END) %>%
  filter(!is.na(POS_VOTE)) %>%
  mutate(Lag_0_30_END = Lag_0_30_END/1000000) %>%
  as.matrix()
head(H_ToM)
S_ToM = SENATE_DAT_ALL %>%
  dplyr::select(POS_VOTE, TNC_Lag_0_30_PER_MIL_POP, Lag_0_30_END) %>%
  mutate(Lag_0_30_END = Lag_0_30_END/1000000) %>%
  filter(!is.na(POS_VOTE)) %>%
  as.matrix()

ALL = matrix(nrow = nrow(H_ToM), ncol = 6)
ALL[,1:3] = H_ToM
ALL[1:nrow(S_ToM),4:6] = S_ToM

ALL = as.data.frame(ALL) %>%
  stargazer(., type = "latex",style = "aer", 
            title = "Summary Stats for Restricted Date Range",
            summary.stat = c("n", "mean","min","max","sd"),
            covariate.labels = c("House Vote", "30-Day Lagged TNC (House)", "30-Day Lagged Damage House","Senate Vote", "30-Day Lagged TNC (Senate)", "30-Day Lagged Damage (Senate)"),
            out = "~/Desktop/ECON Thesis/OUTPUT/DATA_Section/Table_of_Means.tex")







#Showing Decrease in Voting Variation Within RepxYear
#1. For House Only
library(ggplot2)
glimpse(HOUSE_DAT_ALL)
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

HOUSE_DAT %>%
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

#WILL NEED TO RE-READ THESE IN

SENATE_DAT %>%
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


mutate(Party = ifelse(Party == "R","R","D")) %>%#Defining the Robust and Clustered Standard Error Argument for LM
GET_SE_LM = function(MOD){
  MAT = vcovCL(MOD, cluster = attr(MOD, "State"), type = "HC1")
  return(sqrt(diag(MAT)))
}
summary(HOUSE_DAT_ALL$TNC_Lag_0_30_PER_MIL_POP)
#Running Core Specifications Using State Fixed Effects:
Table1House_All = list(
  A = lm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL),
  B = lm(POS_VOTE ~ Lag_0_30_END_BIN + TNC_Lag_0_30_PER_MIL_POP + factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL),
  C = lm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge +  factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL),
  D = lm(POS_VOTE ~ TNC_Lag_0_30_PER_MIL_POP + BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge +  factor(Year) + factor(MON_NUM) + factor(State),
         data = HOUSE_DAT_ALL)
  )
Table1_HouseALL_SE = lapply(X = Table1House_All, GET_SE_LM)
OUT = stargazer(Table1House_All, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                      omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table1_HouseALL_SE,
                      title = "Voting on Weather and Opinion -- House",
                      dep.var.labels = "Positive Environmental Vote",
                      covariate.labels = c("Damage > 0", "Donation Count", "Damage < 1 Mil", "Damage 1-50 Mil", "Damage 50-400 Mil", "Damage > 400 Mil"))

cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/COMBINED/Table_1_FINAL.tex", append = TRUE)


Table1Senate_All = list(
  A = lm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL),
  B = lm(POS_VOTE ~ Lag_0_30_END_BIN + TNC_Lag_0_30_PER_MIL_POP + factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL),
  C = lm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge +  factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL),
  D = lm(POS_VOTE ~ TNC_Lag_0_30_PER_MIL_POP + BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge +  factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL)
)


Table1_SenateALL_SE = lapply(X = Table1Senate_All, GET_SE_LM)
OUT = stargazer(Table1Senate_All, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table1_SenateALL_SE,
                title = "Voting on Weather and Opinion -- Senate",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("Damage > 0", "Donation Count", "Damage < 1 Mil", "Damage 1-50 Mil", "Damage 50-400 Mil", "Damage > 400 Mil"))

cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/COMBINED/Table_1_FINAL.tex", append = TRUE)

linearHypothesis(Table1Senate_All$D, "BIN_30_FHMil = BIN_30_Huge")


#Table 2: Decomposing Effect By Political Party
Table2ALL = list(
  A = lm(POS_VOTE ~ Lag_0_30_END_BIN + TNC_Lag_0_30_PER_MIL_POP + factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL),
  C = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_PER_MIL_POP*REP + factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL),
  D = lm(POS_VOTE ~ Lag_0_30_END_BIN + TNC_Lag_0_30_PER_MIL_POP + factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL),
  G = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_PER_MIL_POP*REP + factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL)
)
Table2ALL_SE = lapply(X = Table2ALL, GET_SE_LM)
OUT = stargazer(Table2ALL, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table2ALL_SE,
                title = "Voting on Weather and Opinion by Party -- House",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("Damage $>$ 0", "Republican", "Donation Count/100", "Damage $>$0 x Rep", "Donation Count/100 x Rep"))
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/COMBINED/Table_2_FINAL.tex", append = TRUE)

#Test Whether House Democrats Respond to Damages
linearHypothesis(Table2ALL$C, "Lag_0_30_END_BIN + Lag_0_30_END_BIN:REP = 0")
linearHypothesis(Table2ALL$C, "TNC_Lag_0_30_Per100 + REP:TNC_Lag_0_30_Per100 = 0")


#Another Way to Cut Table 2
Table3ALL = list(
  A = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL),
  B = lm(POS_VOTE ~ TNC_Lag_0_30_PER_MIL_POP*REP + factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL),
  C = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_PER_MIL_POP*REP + factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL),
  D = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL),
  E = lm(POS_VOTE ~ TNC_Lag_0_30_PER_MIL_POP*REP + factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL),
  G = lm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_PER_MIL_POP*REP + factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL)
)
Table3ALL_SE
Table3ALL_SE = lapply(X = Table3ALL, GET_SE_LM)
OUT = stargazer(Table3ALL, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table3ALL_SE,
                title = "Voting on Weather and Opinion by Party -- House",
                dep.var.labels = "Positive Environmental Vote")

cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/COMBINED/Table_3_FINAL.tex", append = TRUE)
summary(Table3ALL$D)

linearHypothesis(Table3ALL$D, "Lag_0_30_END_BIN + Lag_0_30_END_BIN:REP = 0", white.adjust = "hc1")

#Supplement: Polynomials in Donation counts
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
Polynomial_Rob_SE = lapply(Polynomial_Rob, GET_SE_LM)
OUT = stargazer(Polynomial_Rob, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
          omit = c("factor*"), keep.stat = c("n","adj.rsq"), se = Polynomial_Rob_SE,
          title = "Sensitivity to Polynomials in Donations", dep.var.labels = "Positive Environmental Vote")
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/COMBINED/App_Polynomial.tex")      


#--------------------------------------------------------Appendix Tables------------------------------------------#
Table1_House_ALl_APP = list(
  A = glm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL, family = "binomial"),
  B = glm(POS_VOTE ~ Lag_0_30_END_BIN + TNC_Lag_0_30_Per100 + factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL, family = "binomial"),
  C = glm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge +  factor(Year) + factor(MON_NUM) + factor(State), 
          data = HOUSE_DAT_ALL, family = "binomial"),
  D = glm(POS_VOTE ~ TNC_Lag_0_30_Per100 + BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge +  factor(Year) + factor(MON_NUM) + factor(State),
          data = HOUSE_DAT_ALL, family = "binomial")
)
Table1_House_All_APP_SE = lapply(Table1_House_ALl_APP, GET_SEs)
stargazer(Table1_House_ALl_APP, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
          omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table1_House_All_APP_SE,
          title = "Logistic Regression: Voting on Weather and Opinion -- House",
          dep.var.labels = "Positive Environmental Vote",
          covariate.labels = c("$>$ 0 Damages", "Donation Count/100", "0-1 Million Damages", "1-50 Million Damages",
                               "50-400 Million Damages", "$>$ 400 Million Damages"),
          out = "~/Desktop/ECON Thesis/OUTPUT/COMBINED/App1.tex", append = TRUE)

Table1_Senate_ALl_APP = list(
  A = glm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL, family = "binomial"),
  B = glm(POS_VOTE ~ Lag_0_30_END_BIN + TNC_Lag_0_30_Per100 + factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL, family = "binomial"),
  C = glm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge +  factor(Year) + factor(MON_NUM) + factor(State), 
          data = SENATE_DAT_ALL, family = "binomial"),
  D = glm(POS_VOTE ~ TNC_Lag_0_30_Per100 + BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge +  factor(Year) + factor(MON_NUM) + factor(State),
          data = SENATE_DAT_ALL, family = "binomial")
)
Table1_Senate_ALL_APP_SE = lapply(Table1_Senate_ALl_APP, GET_SEs)
OUT = stargazer(Table1_Senate_ALl_APP, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
          omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table1_Senate_ALL_APP_SE,
          title = "Logistic Regression: Voting on Weather and Opinion -- Senate",
          dep.var.labels = "Positive Environmental Vote",
          covariate.labels = c("$>$ 0 Damages", "Donation Count/100", "0-1 Million Damages", "1-50 Million Damages",
                               "50-400 Million Damages", "$>$ 400 Million Damages"))
cat(paste(OUT, "\n"),file = "~/Desktop/ECON Thesis/OUTPUT/COMBINED/App1.tex", append = TRUE)




#Table 2: Decomposing Effect By Political Party
Table2ALL_APP = list(
  A = glm(POS_VOTE ~ Lag_0_30_END_BIN + TNC_Lag_0_30_Per100 + factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL, family = "binomial"),
  C = glm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_Per100*REP + factor(Year) + factor(MON_NUM) + factor(State), data = HOUSE_DAT_ALL, family = "binomial"),
  D = glm(POS_VOTE ~ Lag_0_30_END_BIN + TNC_Lag_0_30_Per100 + factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL, family = "binomial"),
  G = glm(POS_VOTE ~ Lag_0_30_END_BIN*REP + TNC_Lag_0_30_Per100*REP + factor(Year) + factor(MON_NUM) + factor(State), data = SENATE_DAT_ALL, family = "binomial")
)
Table2ALL_APP_SE = lapply(X = Table2ALL_APP, GET_SEs)
OUT = stargazer(Table2ALL_APP, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table2ALL_APP_SE,
                title = "Logistic Regression: Voting on Weather and Opinion by Party",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("Damage $>$ 0", "Republican", "Donation Count/100", "Damage $>$0 x Rep", "Donation Count/100 x Rep"),
                out = "~/Desktop/ECON Thesis/OUTPUT/COMBINED/App2.tex", append = TRUE)

summary(Table1_House_ALl_APP$A)


















table(House_Dat_all_test$Year)

House_Dat_all_test = House_Dat_all_test %>%
  mutate(Lag_0_30_END_MED = ifelse(Lag_0_30_END > median(Lag_0_30_END), 1,0),
         TNC_Lag_0_30_MED = ifelse(TNC_Lag_0_30 > median(TNC_Lag_0_30),1,0))


Tabl1House = list(
  A = plm(POS_VOTE ~ TNC_Lag_0_30 + factor(Year) + factor(SEASON), 
          data = House_Dat_all_test, model = "within", index = c("Member.of.Congress", "PANEL_VAR")),
  B = plm(POS_VOTE ~ Lag_0_30_END + factor(Year) + factor(SEASON), 
          data = House_Dat_all_test, model = "within", index = c("Member.of.Congress", "PANEL_VAR")),
  C = plm(POS_VOTE ~ TNC_Lag_0_30 + Lag_0_30_END + factor(Year) + factor(SEASON), 
          data = House_Dat_all_test, model = "within", index = c("Member.of.Congress", "PANEL_VAR"))
)

#1 Only TNC
summary(lm(data = House_Dat_all_test, POS_VOTE ~ TNC_Lag_0_30 + factor(State) + factor(Year) + factor(SEASON)))
#2 Only Storms
summary(lm(data = House_Dat_all_test, POS_VOTE ~  I(Lag_0_30_END > 0) + factor(State) + factor(Year) + factor(SEASON)))
summary(lm(data = House_Dat_all_test, POS_VOTE ~  I(Lag_0_30_END > median(Lag_0_30_END)) + factor(State) + factor(Year) + factor(SEASON)))
summary(lm(data = House_Dat_all_test, POS_VOTE ~  I(Lag_0_30_END > 0)*REP + factor(State) + factor(Year) + factor(SEASON)))

summary(lm(data = House_Dat_all_test, POS_VOTE ~ TNC_Lag_0_30 + I(Lag_0_30_END > 0) + factor(State) + factor(Year) + factor(SEASON)))
summary(lm(data = House_Dat_all_test, POS_VOTE ~  I(Lag_0_30_END > 0) + factor(State) + factor(Year) + factor(SEASON)))



summary(plm(POS_VOTE ~ I(Lag_0_30_END_NOT_PLACEBO>0) + factor(SEASON) + factor(Year), 
    data = House_Dat_all_test, model = "within", index = c("Member.of.Congress", "PANEL_VAR")))

stargazer(Tabl1House, omit = "factor*")


head(HOUSE_DAT_ALL)

#Part A: Make Sure Things are Kind of as expected for TNC Donations:
Test_TNC = HOUSE_DAT_ALL %>%
  select(TNC_Lag_0_30, POS_VOTE,Member.of.Congress,PANEL_VAR,Year,SEASON,REP) %>%
  mutate(Quantiled = case_when(
    TNC_Lag_0_30 < quantile(TNC_Lag_0_30, 0.25) ~ "LOW",
    TNC_Lag_0_30 %in% quantile(TNC_Lag_0_30, c(0.25,0.75)) ~ "MED",
    TNC_Lag_0_30 > quantile(TNC_Lag_0_30, 0.75) ~ "HIGH"
  ))

A = plm(POS_VOTE ~ TNC_Lag_0_30 + factor(Year) + factor(SEASON), 
    data = HOUSE_DAT_ALL, model = "within", index = c("Member.of.Congress", "PANEL_VAR"))
B = plm(POS_VOTE ~ TNC_Lag_0_30*REP + factor(Year) + factor(SEASON), 
        data = HOUSE_DAT_ALL, model = "within", index = c("Member.of.Congress", "PANEL_VAR"))


#Test NOAA
Test_NOAA = HOUSE_DAT_ALL %>%
  select(Lag_0_30_END, Lag_0_30_END_NOT_PLACEBO,Lag_0_30_END_PLACEBO, POS_VOTE,Member.of.Congress,PANEL_VAR,Year,SEASON,REP) 


A = plm(POS_VOTE ~ Lag_0_30_END + factor(Year) + factor(SEASON), 
        data = Test_NOAA, model = "within", index = c("Member.of.Congress", "PANEL_VAR"))
A2 = plm(POS_VOTE ~ Lag_0_30_END*REP + factor(Year) + factor(SEASON), 
         data = Test_NOAA, model = "within", index = c("Member.of.Congress", "PANEL_VAR"))
B = plm(POS_VOTE ~ I(Lag_0_30_END>0) + factor(Year) + factor(SEASON), 
        data = Test_NOAA, model = "within", index = c("Member.of.Congress", "PANEL_VAR"))
B2 = plm(POS_VOTE ~ I(Lag_0_30_END>0)*REP + factor(Year) + factor(SEASON), 
         data = Test_NOAA, model = "within", index = c("Member.of.Congress", "PANEL_VAR"))

#Add Year Interaction
B = plm(POS_VOTE ~ I(Lag_0_30_END>0)*factor(Year) + factor(SEASON), 
         data = Test_NOAA, model = "within", index = c("Member.of.Congress", "PANEL_VAR"))
#Add Season Interaction
B = plm(POS_VOTE ~ I(Lag_0_30_END>0)*factor(SEASON)+ factor(Year) , 
        data = Test_NOAA, model = "within", index = c("Member.of.Congress", "PANEL_VAR"))


summary(B)
#Let's Assume thst primary motivation is people's preferences
Tabl1House = list(
  A = plm(POS_VOTE ~ TNC_Lag_0_30 + factor(Year) + factor(SEASON), 
          data = HOUSE_DAT_ALL, model = "within", index = c("Member.of.Congress", "PANEL_VAR")),
  B = 
 
)
TabeHousSE = lapply(Tabl1House, GET_SEs)
OUT = stargazer(Tabl1House, type = "latex", style = "aer", omit = "factor*", keep.stat = c("n","adj.rsq"))


B = plm(POS_VOTE ~ Lag_0_30_END_BIN*REP + factor(Year) + factor(SEASON), 
        data = HOUSE_DAT_ALL, model = "within", index = c("Member.of.Congress", "PANEL_VAR"))
summary(B)

C = plm(POS_VOTE ~ TNC_Lag_0_30*Lag_0_30_END - Lag_0_30_END + factor(Year) + factor(SEASON)

        
      
        
        
        
summary(plm(POS_VOTE ~ TNC_Lag_0_30*Lag_0_30_END - Lag_0_30_END + factor(Year) + factor(SEASON), data = HOUSE_DAT_ALL, model = "within", index = c("Member.of.Congress", "PANEL_VAR")))
summary(plm(POS_VOTE ~ TNC_Lag_0_30 + Lag_0_30_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT_ALL, model = "within", index = c("Member.of.Congress", "PANEL_VAR")))




HOUSE_DAT_ALL %>%
  filter()
  


