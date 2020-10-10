#------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------#
#This Script Takes Senate and House LCV Voting Records Merged With Different Storm Damage Lags---------------------------------#
#I then create some additional indicators and estimate the regressions shown in Results Section 1, along with------------------#
#the corresponding appendix robustness tests-----------------------------------------------------------------------------------#

#--------------------------Final Versions of Regression Output-----------------------#

#----------------------------Sourcing Required Packages-----------------------------#
#For Data Processing
library(data.table)
library(tidyverse)
library(purrr)
#For Visualization
library(gridExtra)
library(thatssorandom)
library(plm)
#For Regression and S.E Computation
library(multiwayvcov)
library(sandwich)
library(stargazer)
library(lmtest)
library(car)
library(fixest)

#---------------------------------------------Read in Merged Senate Data and Create Binned Damage Variables----------------------------------#

#I CAN JUST TURN THIS INTO AN APPLY FUNCTION
SENATE_DAT = fread("~/Google Drive/DATA/ECON/CLEAN/SENATE_LAGGED.csv")
#Construct Necessary Variables for Regressions
SENATE_DAT = SENATE_DAT %>%
  dplyr::mutate_at(which(str_detect(names(SENATE_DAT), "Lag_")), funs(log(.+1))) %>%   #log-transform all "lag" variables
  dplyr::mutate_at(which(str_detect(names(SENATE_DAT), "Lag_")), .funs = list(BIN = ~ifelse(.>0,1,0))) %>% #indicator for lags > 0
  mutate(BIN_30_Mil = ifelse( (Lag_0_30_END > 0 & Lag_0_30_END < log(1000000)),1,0),
         BIN_30_G_MIL = ifelse(Lag_0_30_END > log(1000000),1,0),
         BIN_30_FiftyMil = ifelse( (Lag_0_30_END > log(1000000) & Lag_0_30_END < log(50000000)), 1,0),
         BIN_30_G_FiftyMil = ifelse(Lag_0_30_END >log(50000000),1,0),
         BIN_30_FHMil = ifelse( (Lag_0_30_END > log(50000000) & Lag_0_30_END < log(400000000)), 1,0),
         BIN_30_Huge = ifelse(Lag_0_30_END > log(400000000), 1,0)) %>%   #Creating a severity bins for lagged storms
  mutate(BIN_Thirds = cut(Lag_0_30_END, breaks = c(-Inf,0,quantile(Lag_0_30_END[which(Lag_0_30_END>0)],0.5),Inf),
                          labels = c("Zero", "Low", "High")),
         BIN_FOUR = cut(Lag_0_30_END, breaks = c(-Inf,0,quantile(Lag_0_30_END[which(Lag_0_30_END>0)],c(1/3,2/3)),Inf),
                        labels = c("Zero", "Low","Med", "High")),
         BIN_FIVE = cut(Lag_0_30_END, breaks = c(-Inf,0,quantile(Lag_0_30_END[which(Lag_0_30_END>0)],c(1/4,1/2,3/4)),Inf),
                        labels = c("Zero", "Q1", "Q2","Q3", "Q4")),
         BIN_3_Even = cut(Lag_0_30_END, breaks = quantile(Lag_0_30_END, probs = seq(0,1,1/3)), labels = 1:3, include.lowest = T)) %>%
        # BIN_4_Even = cut(Lag_0_30_END, breaks = quantile(Lag_0_30_END, probs = seq(0,1,1/4)), labels = 1:4, include.lowest = T),
      #   BIN_5_Even = cut(Lag_0_30_END, breaks = quantile(Lag_0_30_END, probs = seq(0,1,1/5)), labels = 1:5, include.lowest = T)) %>%
  mutate(BIN1 = ifelse(BIN_FIVE == "Q1",1,0),
         BIN2 = ifelse(BIN_FIVE  == "Q2", 1,0),
         BIN3 = ifelse(BIN_FIVE == "Q3",1,0),
         BIN4 = ifelse(BIN_FIVE == "Q4", 1,0)) %>%
  mutate(DATE = as.Date(DATE)) %>% #Converting to Date Format
  mutate(SEASON = case_when(
    MON_NUM %in% c(12,1,2,3) ~ "WINTER",
    MON_NUM %in% c(4,5,6) ~ "SPRING",
    MON_NUM %in% c(7,8,9) ~ "SUMMER",
    MON_NUM %in% c(10,11,12) ~ "FALL")) %>% #Season fixed effect
  mutate(PANEL_VAR = 100000*Year + ROLL_CALL)  %>%  #Panel time variables
  mutate(REP = ifelse(Party == "R",1,0)) %>% #Indicator for party Membership
  arrange(Member.of.Congress, PANEL_VAR) %>%
  group_by(Member.of.Congress) %>%
  mutate(LAG_VOTE1 = dplyr::lag(POS_VOTE, n = 1),
         LAG_VOTE2 = dplyr::lag(POS_VOTE, n = 2),
         LAG_VOTE3 = dplyr::lag(POS_VOTE, n = 3),
         PERC_STORM = sum(Lag_0_30_END_BIN)/n()) %>%
  ungroup() 



#---------------------------------------------Read in Merged House Data and Create Variables----------------------------------#
HOUSE_DAT = fread("~/Google Drive/DATA/ECON/CLEAN/HOUSE_LAGGED.csv")
#Construct Necessary Variables for Regressions
HOUSE_DAT = HOUSE_DAT %>%
  dplyr::mutate_at(which(str_detect(names(HOUSE_DAT), "Lag_")),  funs(log(.+1))) %>%  #log-transform all "lag" variables
  dplyr::mutate_at(which(str_detect(names(HOUSE_DAT), "Lag_")), .funs = list(BIN = ~ifelse(.>0,1,0))) %>% #indicator for lags > 0
  mutate(BIN_30_Mil = ifelse( (Lag_0_30_END > 0 & Lag_0_30_END < log(1000000)),1,0),
         BIN_30_G_MIL = ifelse(Lag_0_30_END > log(1000000),1,0),
         BIN_30_FiftyMil = ifelse( (Lag_0_30_END > log(1000000) & Lag_0_30_END < log(50000000)), 1,0),
         BIN_30_G_FiftyMil = ifelse(Lag_0_30_END >log(50000000),1,0),
         BIN_30_FHMil = ifelse( (Lag_0_30_END > log(50000000) & Lag_0_30_END < log(400000000)), 1,0),
         BIN_30_Huge = ifelse(Lag_0_30_END > log(400000000), 1,0)) %>% #Creating a severity bins for lagged storms
  mutate(BIN_Thirds = cut(Lag_0_30_END, breaks = c(-Inf,0,quantile(Lag_0_30_END[which(Lag_0_30_END>0)],0.5),Inf),
                          labels = c("Zero", "Low", "High")),
         BIN_FOUR = cut(Lag_0_30_END, breaks = c(-Inf,0,quantile(Lag_0_30_END[which(Lag_0_30_END>0)],c(1/3,2/3)),Inf),
                        labels = c("Zero", "Low","Med", "High")),
         BIN_FIVE = cut(Lag_0_30_END, breaks = c(-Inf,0,quantile(Lag_0_30_END[which(Lag_0_30_END>0)],c(1/4,1/2,3/4)),Inf),
                        labels = c("Zero", "Q1", "Q2","Q3", "Q4")),
         BIN_3_Even = cut(Lag_0_30_END, breaks = quantile(Lag_0_30_END, probs = seq(0,1,1/3)), labels = 1:3, include.lowest = T),
         BIN_4_Even = cut(Lag_0_30_END, breaks = quantile(Lag_0_30_END, probs = seq(0,1,1/4)), labels = 1:4, include.lowest = T),
         BIN_5_Even = cut(Lag_0_30_END, breaks = quantile(Lag_0_30_END, probs = seq(0,1,1/5)), labels = 1:5, include.lowest = T)) %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(BIN1 = ifelse(BIN_FIVE == "Q1",1,0),
         BIN2 = ifelse(BIN_FIVE  == "Q2", 1,0),
         BIN3 = ifelse(BIN_FIVE == "Q3",1,0),
         BIN4 = ifelse(BIN_FIVE == "Q4", 1,0)) %>%
  mutate(SEASON = case_when(
    MON_NUM %in% c(12,1,2,3) ~ "WINTER",
    MON_NUM %in% c(4,5,6) ~ "SPRING",
    MON_NUM %in% c(7,8,9) ~ "SUMMER",
    MON_NUM %in% c(10,11,12) ~ "FALL")) %>%  #season fixed effect
  mutate(PANEL_VAR = 100000*Year + ROLL_CALL) %>% #Constructing "Time" Variables
  mutate(Member.of.Congress = case_when(
    Member.of.Congress == "Rogers, Mike"  & State == "AL" ~ "Rogers, Mike_AL",
    Member.of.Congress == "Brown, George" & State == "CO" ~ "Brown, George_CO",
    TRUE ~ Member.of.Congress)) %>% #This is an Extra Catch -- didnt seem to get caught the first time
  mutate(REP = ifelse(Party == "R",1,0)) %>% #Republican indicator
  mutate(SEASON = ifelse(Policy == "Air.Right.to.Know" & Year == 2000, "SPRING", SEASON),
         MON = ifelse(Policy == "Air.Right.to.Know" & Year == "2000", "Jun", MON)) %>% 
  #Adding Lags
  arrange(Member.of.Congress, PANEL_VAR) %>%
  group_by(Member.of.Congress) %>%
  mutate(LAG_VOTE1 = dplyr::lag(POS_VOTE, n = 1),
         LAG_VOTE2 = dplyr::lag(POS_VOTE, n = 2),
         LAG_VOTE3 = dplyr::lag(POS_VOTE, n = 3),
         PERC_STORM = sum(Lag_0_30_END_BIN)/n()) %>%
  ungroup() 
  



#------------------------------------------------Visualizing Distribution of Lagged Storm Damages-------------------------#
#Subsets of the Main Datasets
Senate = SENATE_DAT %>%
  select(POS_VOTE,Lag_0_30_END) %>%
  mutate(Lag_0_30_END = exp(Lag_0_30_END)-1) 
House = HOUSE_DAT %>%
  select(POS_VOTE, Lag_0_30_END) %>%
  mutate(Lag_0_30_END = exp(Lag_0_30_END)-1) 

#----------------------------------Censored Histograms of Lagged Storm Damages--------------------------------------------#
#For House Data
H_StormDist = gg_outlier_bin(x  = Test,var_name = "Lag_0_30_END", 
                             cut_off_floor = 1000000, cut_off_ceiling = 500000000, 
                             binwidth = 10000000)  +
  geom_vline(aes(xintercept = mean(Test$Lag_0_30_END)), col = "red") +
  xlab("30-Day Lagged Storm Damages") + ylab("Count") +
  theme_minimal() +
  ggtitle("Distribution of Lagged Damages for House")

map(.x = list("Senate", "House"),
    .f = function(.x){
     PLOT = eval(parse_expr(.x)) %>%
        mutate(BINNED = cut(Lag_0_30_END, breaks = c(-Inf, 0, 1000000, 100000000,Inf),
                            labels = c("Zero", ">=1 Million", "1-100 Million", "<100 Million"),
                            include.lowest = T)) %>%
        ggplot() +
        geom_bar(aes(x = BINNED), col = "black", fill = "black") +
        theme_minimal() +
        ggtitle(str_c("Distribution of Lagged Damages - ", .x)) +
        xlab("Lagged Property Damage") + ylab("Count of Observations")
    ggsave(PLOT, file = str_c("~/Desktop/ECON Thesis/OUTPUT_Publication/", .x, "_Lag.pdf"),
           width = 6, height = 6, units = "in")
    })


         
#For Senate Data
S_StormDist = gg_outlier_bin(x = Test_Sen,var_name = "Lag_0_30_END", 
                             cut_off_floor = 1000000, cut_off_ceiling = 500000000, 
                             binwidth = 10000000) +
  geom_vline(aes(xintercept = mean(Test_Sen$Lag_0_30_END)), col = "red") +
  xlab("30-Day Lagged Storm Damages") + ylab("Count") +
  theme_minimal() +
  ggtitle("Distribution of Lagged Damages for Senate")
#Arranging two Figures
grid.arrange(S_StormDist,H_StormDist,nrow = 2)


#-------------------------------------Define Function to Compute Robust Standard Errors from a panel "plm" model-----------------#
GET_SEs = function(MODEL){
  COV = vcovHC(MODEL, type = "HC1")
  return(sqrt(diag(COV)))
}
#-----------------------------------------------------Section 1. Table 1: Build Time Lags for House and Senate---------------------------------------------#
Table1 = list(
  House1 = feols(POS_VOTE ~ Lag_0_30_END_BIN | Member.of.Congress + Year + MON, data = HOUSE_DAT),
  House2 = feols(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN | Member.of.Congress + Year + MON, data = HOUSE_DAT),
  House3 = feols(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + Lag_60_90_END_BIN | Member.of.Congress + Year + MON, data = HOUSE_DAT),
  Senate1 = feols(POS_VOTE ~ Lag_0_30_END_BIN | Member.of.Congress + Year + MON, data = SENATE_DAT),
  Senate2 = feols(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN | Member.of.Congress + Year + MON, data = SENATE_DAT),
  Senate3 = feols(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + Lag_60_90_END_BIN | Member.of.Congress + Year + MON, data = SENATE_DAT)  
) 


etable(Table1, tex = T, coefstat = "se", keepFactors = F,
       fitstat = ~r2, digits = 3, subtitles = c(rep("House",3), rep("Senate", 3)), title = "Test",
       file = "~/Desktop/ECON Thesis/OUTPUT_Publication/01.Tab1.tex")

#-------------------------------------------------Table 2: Severity and Party Interaction---------------------------------------------#
Table2 = list(
  House1 = feols(POS_VOTE ~ Lag_0_30_END_BIN | Member.of.Congress + Year + MON, data = HOUSE_DAT),
  House2 = feols(POS_VOTE ~ Lag_0_30_END_BIN*REP - REP | Member.of.Congress + Year + MON, data = HOUSE_DAT),
  #House3 = feols(POS_VOTE ~ BIN1 + BIN2 + BIN3 + BIN4 | Member.of.Congress + Year + MON, data = HOUSE_DAT),
  House4 = feols(POS_VOTE ~ BIN1*REP - REP + BIN2*REP - REP + BIN3*REP - REP + BIN4*REP - REP | Member.of.Congress + Year + MON, data = HOUSE_DAT),
  Senate1 = feols(POS_VOTE ~ Lag_0_30_END_BIN | Member.of.Congress + Year + MON, data = SENATE_DAT),
  Senate2 = feols(POS_VOTE ~ Lag_0_30_END_BIN*REP - REP | Member.of.Congress + Year + MON, data = SENATE_DAT),
 # Senate3 = feols(POS_VOTE ~ BIN1 + BIN2 + BIN3 + BIN4 | Member.of.Congress + Year + MON, data = SENATE_DAT),
  Senate4 = feols(POS_VOTE ~ BIN1*REP - REP + BIN2*REP - REP + BIN3*REP - REP + BIN4*REP - REP | Member.of.Congress + Year + MON, data = SENATE_DAT)
)
#-----Hypothesis Tests------#
#Total Effects for Columns 2 and 4
linearHypothesis(Table2$Senate2, "Lag_0_30_END_BIN + Lag_0_30_END_BIN:REP = 0")
linearHypothesis(Table2$House2, "Lag_0_30_END_BIN + Lag_0_30_END_BIN:REP = 0")
#Pairwise Comparisons for House Democrats
linearHypothesis(Table2$House4, "BIN1 - BIN2 = 0")
linearHypothesis(Table2$House4, "BIN1 - BIN3 = 0")
linearHypothesis(Table2$House4, "BIN1 - BIN4 = 0")
linearHypothesis(Table2$House4, "BIN2 - BIN3 = 0")
linearHypothesis(Table2$House4, "BIN2 - BIN4 = 0")
linearHypothesis(Table2$House4, "BIN3 - BIN4 = 0")
#Total Effects for House Republicans
linearHypothesis(Table2$House4, "BIN1 + BIN1:REP = 0")
linearHypothesis(Table2$House4, "BIN2 + REP:BIN2 = 0")
linearHypothesis(Table2$House4, "BIN3 + REP:BIN3 = 0")
linearHypothesis(Table2$House4, "BIN4 + REP:BIN4 = 0")

#Formatting Latex Table
etable(Table2, tex = T, coefstat = "se", keepFactors = F,
       fitstat = ~r2, digits = 3, subtitles = c(rep("House",3), rep("Senate", 3)), title = "Heterogeneity By Party and Storm Severity",
       file = "~/Desktop/ECON Thesis/OUTPUT_Publication/02.Tab2.tex")


#-------------------------------------------Table 3:  Cold and Non-Cold Storms-------------------------------------#
Table3House = list(
  Reg1 = feols(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN |Member.of.Congress+ Year , data = HOUSE_DAT),
  Reg2 = feols(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP - REP |Member.of.Congress+ Year , data = HOUSE_DAT),
  Reg3 = feols(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN |Member.of.Congress+ Year , data = HOUSE_DAT),
  Reg4 = feols(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN*REP - REP |Member.of.Congress+ Year , data = HOUSE_DAT),
  Reg5 = feols(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP - REP + Lag_0_30_END_NOT_PLACEBO_BIN*REP - REP |Member.of.Congress+ Year, data = HOUSE_DAT)
)
#----------------Hypothesis Test-----------------#
#Total Effect of Non-Cold for Reps
linearHypothesis(Table3House$Reg4, "Lag_0_30_END_NOT_PLACEBO_BIN + Lag_0_30_END_NOT_PLACEBO_BIN:REP = 0")
#Total Effect of Cold for Reps
linearHypothesis(Table3House$Reg2, "Lag_0_30_END_PLACEBO_BIN + Lag_0_30_END_PLACEBO_BIN:REP = 0")
#Total Effect of All 4
Table3House$Reg5
linearHypothesis(Table3House$Reg5, "Lag_0_30_END_PLACEBO_BIN + Lag_0_30_END_PLACEBO_BIN:REP +
                                    Lag_0_30_END_NOT_PLACEBO_BIN + REP:Lag_0_30_END_NOT_PLACEBO_BIN  = 0")

#Manually Adding Coefficient -- Get's Dropped From Reg5
Table3House$Reg5

Table3Senate = list(
  Reg1 = feols(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN |Member.of.Congress+ Year , data = SENATE_DAT),
  Reg2 = feols(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP - REP |Member.of.Congress+ Year, data = SENATE_DAT),
  Reg3 = feols(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN |Member.of.Congress+ Year , data = SENATE_DAT),
  Reg4 = feols(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN*REP - REP |Member.of.Congress+ Year , data = SENATE_DAT),
  Reg5 = feols(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP - REP + Lag_0_30_END_NOT_PLACEBO_BIN*REP - REP |Member.of.Congress+ Year, data = SENATE_DAT)
)
Table3Senate$Reg5
#Writing Tables
etable(Table3House, tex = T, coefstat = "se", keepFactors = F,
       fitstat = ~r2, digits = 3, title = "House-Different Storm Types",
       file = "~/Desktop/ECON Thesis/OUTPUT_Publication/03.Tab3.tex")

etable(Table3Senate, tex = T, coefstat = "se", keepFactors = F,
       fitstat = ~r2, digits = 3, title = "House-Different Storm Types",
       file = "~/Desktop/ECON Thesis/OUTPUT_Publication/03.Tab3.tex")




#------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------Supplemental Questions and Appendix Results-----------------------------#
#------------------------------------------------------------------------------------------------------------------------#

#-----------------Interpretation Question: How Often do Cold and Non-Cold Co-Occur?------#
lapply(list(HOUSE_DAT,SENATE_DAT), FUN = function(X){
  VALS = table(X$Lag_0_30_END_PLACEBO_BIN, X$Lag_0_30_END_NOT_PLACEBO_BIN) 
  print(str_c("Percentage of Co-Occuring Cold is  ",as.character(VALS[2,2]/(VALS[2,1] + VALS[2,2]))))
  #Percentage of Cold Storms in the Winter and Spring
  VALS = table(X$SEASON, X$Lag_0_30_END_PLACEBO_BIN)
  print(str_c("Percentage of Cold Storms in Winter or Spring is  ", as.character(sum(VALS[c(2,4),2])/sum(VALS[1:4,2]))))
})

table(HOUSE_DAT$SEASON, HOUSE_DAT$Lag_0_30_END_PLACEBO_BIN)

#Are Cold and non-cold results predicated on month fixed effect
feols(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN*REP - REP |Member.of.Congress+ Year + MON , data = HOUSE_DAT)
feols(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP - REP |Member.of.Congress+ Year + MON , data = HOUSE_DAT)
feols(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN*REP - REP |Member.of.Congress+ Year + MON , data = SENATE_DAT)
feols(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP - REP |Member.of.Congress+ Year + MON , data = SENATE_DAT)
linearHypothesis(Test, "Lag_0_30_END_NOT_PLACEBO_BIN + Lag_0_30_END_NOT_PLACEBO_BIN:REP = 0")

#-----------------------------------------------------------Leave-one-out analysis for Main Results----------------------------------#
UNIQUE_STATES = unique(HOUSE_DAT$State)

library(furrr)
library(rlang)
options("future.fork.enable" = TRUE)
future::plan(multicore(workers = 4)) 
Leave_one_out_house = future_map_dfr(.x = UNIQUE_STATES,
                                     .f = ~ coeftable(feols(POS_VOTE ~ Lag_0_30_END_BIN*REP - REP | Member.of.Congress + Year + SEASON , 
                                                            data = subset(HOUSE_DAT, State != .x))) %>%
                                       rownames_to_column() %>%
                                       mutate(State = .x))

Leave_one_out_senate = future_map_dfr(.x = unique(SENATE_DAT$State),
                                      .f = ~ coeftable(feols(POS_VOTE ~ Lag_0_30_END_BIN*REP - REP | Member.of.Congress + Year + SEASON, 
                                                             data = subset(SENATE_DAT, State != .x))) %>%
                                        rownames_to_column() %>%
                                        mutate(State = .x))
ggplot(Leave_one_out_house) +
  geom_histogram(aes(x = Estimate, group = rowname))

Leave_one_out_house %>%
  filter(str_detect(rowname, "REP", negate = F)) %>%
  filter(Estimate == max(Estimate) | Estimate == min(Estimate))



#----------------Add a Control for What's Happening Everywhere Else--------------------#
VALS = HOUSE_DAT %>% distinct(State, PANEL_VAR)
library(furrr)
options("future.fork.enable" = TRUE)
future::plan(multicore(workers = 4)) 
#Construct Value for House Data
OTHER_VALS = future_map2_dfr(.x = VALS$State, .y = VALS$PANEL_VAR,
                             .f = ~ HOUSE_DAT %>% filter(PANEL_VAR == .y) %>%
                               distinct(State, Lag_0_30_END) %>%
                               mutate(Lag_0_30_END = exp(Lag_0_30_END) - 1) %>%
                               summarize(State_VAL = sum(Lag_0_30_END[which(State == .x)]),
                                         State_OTHER = sum(Lag_0_30_END[which(State != .x)])) %>%
                               mutate(State = .x,
                                      PANEL_VAR = .y)) 
#Merge onto House Data
HOUSE_DAT = HOUSE_DAT %>%
  left_join(OTHER_VALS, by = c("State", "PANEL_VAR")) %>%
  mutate(State_OTHER = log(State_OTHER + 1))
#----------Repeating for Senate Data--------------------#
VALS = SENATE_DAT %>% distinct(State, PANEL_VAR)
OTHER_VALS = future_map2_dfr(.x = VALS$State, .y = VALS$PANEL_VAR,
                             .f = ~ SENATE_DAT %>% filter(PANEL_VAR == .y) %>%
                               distinct(State, Lag_0_30_END) %>%
                               mutate(Lag_0_30_END = exp(Lag_0_30_END) - 1) %>%
                               summarize(State_VAL = sum(Lag_0_30_END[which(State == .x)]),
                                         State_OTHER = sum(Lag_0_30_END[which(State != .x)])) %>%
                               mutate(State = .x,
                                      PANEL_VAR = .y)) 
SENATE_DAT = SENATE_DAT %>%
  left_join(OTHER_VALS, by = c("State", "PANEL_VAR")) %>%
  mutate(State_OTHER = log(State_OTHER + 1))

#-------------------Table of Sensitivity Analysis----------------------------#
library(devtools)
#Using Spec Chart Function from source:
source("https://github.com/ArielOrtizBobea/spec_chart/blob/master/spec_chart_function.R?raw=TRUE")


#Creating a Spec Chart
#-------01. House Data----------#
Test = map_dfr(.x = list(HOUSE_DAT, SENATE_DAT),
    .f =  function(.x){
      Original = feols(POS_VOTE ~ Lag_0_30_END_BIN  | Member.of.Congress + Year + SEASON, data = .x) 
      Restrict = feols(POS_VOTE ~ Lag_0_30_END_BIN | Member.of.Congress + Year + SEASON, 
                 data = subset(.x, PERC_STORM >=0.1 & PERC_STORM <=0.9))
      Other_State = feols(POS_VOTE ~ Lag_0_30_END_BIN + poly(State_OTHER,10) | 
                            Member.of.Congress + Year + MON, data = .x)
      Pre_2000 = feols(POS_VOTE ~ Lag_0_30_END_BIN + poly(State_OTHER,10) | 
                         Member.of.Congress + Year + MON, data = subset(.x, Year < 2000))
      Post_2000 = feols(POS_VOTE ~ Lag_0_30_END_BIN | 
                          Member.of.Congress + Year + MON, data = subset(.x, Year >= 2000))
      #Adding Dem-Specific Effects
      Original_Dem = feols(POS_VOTE ~ Lag_0_30_END_BIN*REP - REP  | Member.of.Congress + Year + SEASON, data = .x) 
      Restrict_Dem = feols(POS_VOTE ~ Lag_0_30_END_BIN*REP - REP | Member.of.Congress + Year + SEASON, 
                       data = subset(.x, PERC_STORM >=0.1 & PERC_STORM <=0.9))
      Other_State_Dem = feols(POS_VOTE ~ Lag_0_30_END_BIN*REP - REP + poly(State_OTHER,10) | 
                            Member.of.Congress + Year + MON, data = .x)
      Pre_2000_Dem = feols(POS_VOTE ~ Lag_0_30_END_BIN*REP - REP + poly(State_OTHER,10) | 
                         Member.of.Congress + Year + MON, data = subset(.x, Year < 2000))
      Post_2000_Dem = feols(POS_VOTE ~ Lag_0_30_END_BIN*REP - REP | 
                          Member.of.Congress + Year + MON, data = subset(.x, Year >= 2000))
      REGS = list(Original, Restrict, Other_State, Pre_2000, Post_2000,
                  Original_Dem, Restrict_Dem, Other_State_Dem, Pre_2000_Dem, Post_2000_Dem)
      map_dfr(REGS, ~coeftable(.x)[1,1:2]) %>%
        mutate(Original = rep(c(T,F,F,F,F),2),
               Restrict_Reps = rep(c(F,T,F,F,F),2),
               Other_State = rep(c(F,F,T,F,F),2),
               Pre_2000 = rep(c(F,F,F,T,F),2),
               Post_2000 = rep(c(F,F,F,F,T),2))
                }) %>%
  mutate(House = c(rep(T,10), rep(F,10)),
         Senate = c(rep(F,10), rep(T,10)),
         Total_Coef = rep(c(rep(T,5),rep(F,5)),2),
         Dem_Coef = rep(c(rep(F,5),rep(T,5)),2))

labels = list("Original", "Restrict_Reps", "Other State (10th Degree)",
              "Pre-2000","Post-2000", "House", "Senate", "Total Effect", "Democrat Effect")

schart(Test,labels, n = c(5,5,5,5), highlight = c(1,6,11,16), 
       heights = c(1,0.4), ylab = "Coefficients + Approx. 95% CI")
labs(y = "Coefficient + Approximate 95% CI")
title("Plot")








#-------------------------------------------------------------------Guess at How Many More Votes Might Have Passed--------------------------------#
#Use a Logit Spec For Prediction Exercise
MODEL = feglm(POS_VOTE ~ Lag_0_30_END_BIN*REP - REP | Member.of.Congress + Year + MON, 
              data = HOUSE_DAT, family = binomial(link = "logit"))
#Excluding Observations Dropped From Logit Regression
Excludes = HOUSE_DAT %>% group_by(Member.of.Congress) %>% 
  summarize(Count = n(), 
            POS = sum(POS_VOTE, na.rm = T),
            NAS = sum(is.na(POS_VOTE))) %>%
  ungroup() %>% filter(POS == 0 | POS == Count | POS + NAS == Count)  %>% pull(Member.of.Congress) 

#Fitted Values
MODEL_FITS = MODEL$fitted.values
#Making
MODEL_PREDS = HOUSE_DAT %>%
  dplyr::select(Member.of.Congress, State, Lag_0_30_END_BIN, POS_VOTE, REP, PANEL_VAR, Year, MON) %>%
  filter(!is.na(POS_VOTE)) %>%
  filter(!Member.of.Congress %in% Excludes) %>%
  #Adding All 1s to Storm Damages
  mutate(Lag_0_30_END_BIN = 1) %>%
  #Adding Predictions and Fitted Values
  mutate(PREDS = predict(MODEL, newdata = .),
         FITTED = MODEL_FITS) 

library(furrr)
set.seed(10)
options("future.fork.enable" = TRUE)
future::plan(multicore(workers = 4)) 
PREDS = future_map_dfr(.x = 1:100, .f = function(.x){
  MODEL_PREDS = MODEL_PREDS %>%
    rowwise() %>%
    mutate(PRED_BIN = rbinom(1,1,PREDS)) %>%
    ungroup() %>%
    #Producing Counts By Vote
    group_by(PANEL_VAR) %>%
    summarise(COUNT = n(), 
              POS = sum(POS_VOTE, na.rm = T),
              PRED = sum(PRED_BIN, na.rm = T))  %>%
    ungroup() %>%
    mutate(PASS_REAL = (POS > 218),
           PASS_PRED = (PRED > 218))
  #Calculating Summary Stats
  data.frame(VOTES = sum(MODEL_PREDS$POS),
             PRED_VOTES = sum(MODEL_PREDS$PRED),
             PASS_REAL = sum(MODEL_PREDS$PASS_REAL),
             PASS_PRED = sum(MODEL_PREDS$PASS_PRED))
})

hist(100*(PREDS$PRED_VOTES - PREDS$VOTES)/PREDS$VOTES[1])
#What Would the Change in Positive Votes Have Been?
#Slight Decrease in Votes
sum(MODEL_PREDS$POS) - sum(MODEL_PREDS$PRED)

#How Many More Votes Pass?
sum(MODEL_PREDS$PASS_REAL) - sum(MODEL_PREDS$PASS_PRED)


MODEL_FITS = Table2$House2$fitted.values
PREDS = rbinom(n = length(MODEL_FITS), 1, prob = MODEL_FITS)
Test = HOUSE_DAT %>%
  dplyr::select(Member.of.Congress, State, Lag_0_30_END_BIN, POS_VOTE, REP, PANEL_VAR) %>%
  filter(!is.na(POS_VOTE)) %>%
  filter(!Member.of.Congress %in% Excludes) %>%
  mutate(FITTED = MODEL_FITS) %>%
  rowwise() %>%
  mutate(PREDICT = rbinom(1,1,FITTED)) %>%
  ungroup() %>%
  group_by(PANEL_VAR) %>%
  summarise(COUNT = n(), 
            POS = sum(POS_VOTE, na.rm = T),
            PRED = sum(PREDICT, na.rm = T))  %>%
  ungroup() %>%
  mutate(REAL_NEG = COUNT - POS,
         PRED_NEG = COUNT - PRED,
         REAL_PASSES = (POS > REAL_NEG),
         PRED_PASSES = (PRED > PRED_NEG))

table(Test$REAL_PASSES)
table(Test$PRED_PASSES)





#---------------------------------------------------------Robustness to Logit Specifications----------------------------------#
#----------------------Table 1----------------------#
Table1_Logit = list(
  House1 = feglm(POS_VOTE ~ Lag_0_30_END_BIN | Member.of.Congress + Year + MON, 
                 data = HOUSE_DAT, family = binomial(link = "logit")),
  House2 = feglm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN | Member.of.Congress + Year + MON, 
                 data = HOUSE_DAT, family = binomial(link = "logit")),
  House3 = feglm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + Lag_60_90_END_BIN | Member.of.Congress + Year + MON,
                 data = HOUSE_DAT, family = binomial(link = "logit")),
  Senate1 = feglm(POS_VOTE ~ Lag_0_30_END_BIN | Member.of.Congress + Year + MON, 
                  data = SENATE_DAT, family = binomial(link = "logit")),
  Senate2 = feglm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN | Member.of.Congress + Year + MON,
                  data = SENATE_DAT, family = binomial(link = "logit")),
  Senate3 = feglm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + Lag_60_90_END_BIN | Member.of.Congress + Year + MON,
                  data = SENATE_DAT, family = binomial(link = "logit"))  
) 
etable(Table1_Logit, tex = T, coefstat = "se", keepFactors = F,
       fitstat = ~r2, digits = 3, title = "Table 1 -- Logit Specs",
       file = "~/Desktop/ECON Thesis/OUTPUT_Publication/A1.T1_Logit.tex")

#-------------------Table 2----------------------#
Table2_Logit = list(
  House1 = feglm(POS_VOTE ~ Lag_0_30_END_BIN | Member.of.Congress + Year + MON, 
                 data = HOUSE_DAT, family = binomial(link = "logit")),
  House2 = feglm(POS_VOTE ~ Lag_0_30_END_BIN*REP - REP | Member.of.Congress + Year + MON, 
                 data = HOUSE_DAT, family = binomial(link = "logit")),
  House4 = feglm(POS_VOTE ~ BIN1*REP - REP + BIN2*REP - REP + BIN3*REP - REP + BIN4*REP - REP | Member.of.Congress + Year + MON, 
                 data = HOUSE_DAT, family = binomial(link = "logit")),
  Senate1 = feglm(POS_VOTE ~ Lag_0_30_END_BIN | Member.of.Congress + Year + MON, 
                  data = SENATE_DAT, family = binomial(link = "logit")),
  Senate2 = feglm(POS_VOTE ~ Lag_0_30_END_BIN*REP - REP | Member.of.Congress + Year + MON, 
                  data = SENATE_DAT, family = binomial(link = "logit")),
  Senate4 = feglm(POS_VOTE ~ BIN1*REP - REP + BIN2*REP - REP + BIN3*REP - REP + BIN4*REP - REP | Member.of.Congress + Year + MON, 
                  data = SENATE_DAT, family = binomial(link = "logit"))
)

etable(Table2_Logit, tex = T, coefstat = "se", keepFactors = F,
       fitstat = ~r2, digits = 3, title = "Table 1 -- Logit Specs",
       file = "~/Desktop/ECON Thesis/OUTPUT_Publication/A2.T2_Logit.tex")



#-----------------01A. Replicating Table 1:----------------------------
#For House
Table1_APP_H = list(
  L1 = feglm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L2 = feglm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L3 = feglm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + Lag_60_90_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L4 = feglm(POS_VOTE ~ Lag_0_15_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L5 = feglm(POS_VOTE ~ Lag_0_15_END_BIN + Lag_15_30_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L6 = feglm(POS_VOTE ~ Lag_0_15_END_BIN + Lag_15_30_END_BIN + Lag_30_45_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit"))
)
esttex(Table1_APP_H, coefstat = "se", yesNoFixed = "No", keepFactors = F, 
       file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/LOGIT/T1.Logit.tex")
#For Senate
Table1_APP_S = list(
  L1 = feglm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L2 = feglm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L3 = feglm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + Lag_60_90_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
        data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L4 = feglm(POS_VOTE ~ Lag_0_15_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L5 = feglm(POS_VOTE ~ Lag_0_15_END_BIN + Lag_15_30_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L6 = feglm(POS_VOTE ~ Lag_0_15_END_BIN + Lag_15_30_END_BIN + Lag_30_45_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
                  data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit"))
)
esttex(Table1_APP_S, coefstat = "se", yesNoFixed = "No", keepFactors = F, 
       file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/LOGIT/T1.Logit.tex")
  

#------------------------01B. Replicating Table 2:--------------------------
Table2_APP_ALL = list(
  L1 = feglm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L2 = feglm(POS_VOTE ~ Lag_0_30_END_BIN*Lag_0_30_END - Lag_0_30_END + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L3 = feglm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L4 = feglm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L5 = feglm(POS_VOTE ~ Lag_0_30_END_BIN*Lag_0_30_END - Lag_0_30_END + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L6 = feglm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit"))
)
esttex(Table2_APP_ALL, coefstate ="se", yesNoFixed = "No", keepFactors = F,
       file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/LOGIT/T2.Logit.tex")


#-------------------------------01C. Replicating Table 3--------------------------------------
#For House
Table3APP_H = list(
  L1 = feglm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
        data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L2 = feglm(POS_VOTE ~ Lag_0_30_END_BIN*REP - REP + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L3 = feglm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L4 = feglm(POS_VOTE ~ BIN_30_Mil*REP + BIN_30_FiftyMil*REP + BIN_30_FHMil*REP + BIN_30_Huge*REP - REP + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit"))
)
esttex(Table3APP_H, coefstate ="se", yesNoFixed = "No", keepFactors = F,
       file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/LOGIT/T3.Logit.tex")
#For Senate
Table3APP_S = list(
  L1 = feglm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L2 = feglm(POS_VOTE ~ Lag_0_30_END_BIN*REP - REP + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L3 = feglm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L4 = feglm(POS_VOTE ~ BIN_30_Mil*REP + BIN_30_FiftyMil*REP + BIN_30_FHMil*REP + BIN_30_Huge*REP - REP + factor(Year) + factor(SEASON) | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit"))
)  
esttex(Table3APP_S, coefstate ="se", yesNoFixed = "No", keepFactors = F,
      file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/LOGIT/T3.Logit.tex")
  

#-----------------------------------------01D. Replicating Table 4-----------------------------------#
#For House
Table4APP_H = list(
  L1 = feglm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN + factor(Year) | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L2 = feglm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP - REP + factor(Year) | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L3 = feglm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN + factor(Year) + factor(SEASON)  | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),


