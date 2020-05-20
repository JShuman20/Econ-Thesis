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

#---------------------------------------------Read in Merged Senate Data and Create Variables----------------------------------#
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
  mutate(DATE = as.Date(DATE)) %>%
  mutate(SEASON = case_when(
    MON_NUM %in% c(12,1,2,3) ~ "WINTER",
    MON_NUM %in% c(4,5,6) ~ "SPRING",
    MON_NUM %in% c(7,8,9) ~ "SUMMER",
    MON_NUM %in% c(10,11,12) ~ "FALL")) %>% #Season fixed effect
  mutate(PANEL_VAR = 100000*Year + ROLL_CALL)  %>%  #Panel time variables
  mutate(REP = ifelse(Party == "R",1,0)) #Indicator for party Membership

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
  mutate(DATE = as.Date(DATE)) %>%
  mutate(SEASON = case_when(
    MON_NUM %in% c(12,1,2,3) ~ "WINTER",
    MON_NUM %in% c(4,5,6) ~ "SPRING",
    MON_NUM %in% c(7,8,9) ~ "SUMMER",
    MON_NUM %in% c(10,11,12) ~ "FALL")) %>%  #season fixed effect
  mutate(PANEL_VAR = 100000*Year + ROLL_CALL) %>%
  mutate(Member.of.Congress = case_when(
    Member.of.Congress == "Rogers, Mike"  & State == "AL" ~ "Rogers, Mike_AL",
    Member.of.Congress == "Brown, George" & State == "CO" ~ "Brown, George_CO",
    TRUE ~ Member.of.Congress)) %>% #This is an Extra Catch -- didnt seem to get caught the first time
  mutate(REP = ifelse(Party == "R",1,0))



#------------------------------------------------Visualizing Distribution of Lagged Storm Damages-------------------------#
#Subsets of the Main Datasets
Test_Sen = SENATE_DAT %>%
  select(POS_VOTE,Lag_0_30_END) %>%
  mutate(Lag_0_30_END = exp(Lag_0_30_END)) 
Test = HOUSE_DAT %>%
  select(POS_VOTE, Lag_0_30_END) %>%
  mutate(Lag_0_30_END = exp(Lag_0_30_END)) 

#Visualizations
H_StormDist = gg_outlier_bin(x  = Test,var_name = "Lag_0_30_END", 
                             cut_off_floor = 1000000, cut_off_ceiling = 500000000, 
                             binwidth = 10000000)  +
  geom_vline(aes(xintercept = mean(Test$Lag_0_30_END)), col = "red") +
  xlab("30-Day Lagged Storm Damages") + ylab("Count") +
  theme_minimal() +
  ggtitle("Distribution of Lagged Damages for House")
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
#------------------------------------------------------------------Section 1. Table 1: Build Time Lags for House and Senate---------------------------------------------------------------#
#List of Regressions for House of Representatives Building Time Lags
Table1_House = list(
  L4 = plm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L5 = plm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L6 = plm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + Lag_60_90_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L1 = plm(POS_VOTE ~ Lag_0_15_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ Lag_0_15_END_BIN + Lag_15_30_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L3 = plm(POS_VOTE ~ Lag_0_15_END_BIN + Lag_15_30_END_BIN + Lag_30_45_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR"))
) 
#Re-calculating robust se's for these six regressions
Table1_House_SE = lapply(X = Table1_House, GET_SEs)
#Latex output for house results
OUT = stargazer(Table1_House, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table1_House_SE,
                title = "Storm Damages Over Different Time Lags on Voting",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("0-15","15-30","30-45","0-30","30-60","60-90"))
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table_1.tex", append = TRUE)

#List of Regressions for Senate Building Time Lags
Table1_Senate = list(
  L4 = plm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L5 = plm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L6 = plm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + Lag_60_90_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L1 = plm(POS_VOTE ~ Lag_0_15_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ Lag_0_15_END_BIN + Lag_15_30_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L3 = plm(POS_VOTE ~ Lag_0_15_END_BIN + Lag_15_30_END_BIN + Lag_30_45_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)
#Calculating Robust se's
Table1_Senate_SE = lapply(X = Table1_Senate, GET_SEs)
#Outputting latex table
OUT = stargazer(Table1_Senate, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table1_Senate_SE,
                title = "Storm Damages Over Different Time Lags on Voting",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("0-30","30-60","60-90","0-15","15-30","30-45"))
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table_1.tex", append = TRUE)


#--------------------------------------Section 1 Table 2: Discrete and Continuous Lags-------------------------------------------#
#List of Regressions For House
Table2House = list(
  L1 = plm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ Lag_0_30_END_BIN*Lag_0_30_END - Lag_0_30_END + factor(Year) + factor(SEASON), data = HOUSE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L4 = plm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge + factor(Year) + factor(SEASON), data = HOUSE_DAT,
           model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)
#Test Equality of Coefficients
linearHypothesis(Table2House$L4, c("BIN_30_FiftyMil - BIN_30_FHMil = 0"), white.adjust = "hc1")
linearHypothesis(Table2House$L4, c("BIN_30_Huge - BIN_30_Mil = 0"), white.adjust = "hc1")
linearHypothesis(Table2House$L4, c("BIN_30_Huge - BIN_30_FHMil = 0"), white.adjust = "hc1")
#Extract robust se's from models
Table2_House_SE = lapply(X = Table2House, GET_SEs)
#Output Latex Table
OUT = stargazer(Table2House, style = "aer", type = "latex", column.sep.width = "5", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table2_House_SE,
                title = "Heterogeneous Effects By Severity of Storm Damages",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("/> 0", "$<$1 Mil.", "1-50 Mil.", "50-400 Mil.","$>$400 Mil","Interaction"))
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table_2.tex", append = TRUE)

#List of Regressions for Senate
Table2SENATE = list(
  L1 = plm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ Lag_0_30_END_BIN*Lag_0_30_END - Lag_0_30_END + factor(Year) + factor(SEASON), data = SENATE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L4 = plm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge + factor(Year) + factor(SEASON), data = SENATE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)
#Test Equality of Coefficients
linearHypothesis(Table2SENATE$L4, c("BIN_30_FiftyMil - BIN_30_FHMil = 0"), white.adjust = "hc1")
linearHypothesis(Table2SENATE$L4, c("BIN_30_Huge - BIN_30_Mil = 0"), white.adjust = "hc1")
linearHypothesis(Table2SENATE$L4, c("BIN_30_Huge - BIN_30_FHMil = 0"), white.adjust = "hc1")
#Robust Se's
Table2_SENATE_SE = lapply(X = Table2SENATE, GET_SEs)
#Outputting Latex Table
OUT = stargazer(Table2SENATE, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table2_SENATE_SE,
                title = "Heterogeneous Effects By Severity of Storm Damages",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("$>0$","$<$1 Mil", "1-50 Mil.", "50-400 Mil", "$>$400 Mil.", "Interaction"))
               
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table_2.tex", append = TRUE)


#--------------------------------------------------------Section 1 Tables 4 and 5: Party Interaction------------------------------#
#List of Regressions for House
Table3House = list(
  L1 = plm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ Lag_0_30_END_BIN*REP + factor(Year) + factor(SEASON), data = HOUSE_DAT,
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L5 = plm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge + factor(Year) + factor(SEASON), 
           data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L6 = plm(POS_VOTE ~ BIN_30_Mil*REP + BIN_30_FiftyMil*REP + BIN_30_FHMil*REP + BIN_30_Huge*REP + factor(Year) + factor(SEASON), 
           data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)
#Test Equality of Coefficients
linearHypothesis(Table3House$L6, "BIN_30_Huge + REP:BIN_30_Huge = 0", white.adjust = "hc1")
#Extracting Robust Se's
Table3HouseSE = lapply(X = Table3House, GET_SEs)
#Output Latex Table
OUT = stargazer(Table3House, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table3HouseSE,
                title = "Heterogeneity: Severity x Party",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("$>0$","$>$1 Mil.","1-50 Mil.", "50-400 Mil.", "$>$400 Mil.", 
                                     "$>0$xREP", "$>$1 Mil x REP", "1-50 Mil x REP", "50-400 Mil x REP", "$>$400 Mil x REP"))
                
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table_3.tex", append = TRUE)

#List of Regressions For Senate              
Table3Senate = list(
  L1 = plm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ Lag_0_30_END_BIN*REP + factor(Year) + factor(SEASON), data = SENATE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L5 = plm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge + factor(Year) + factor(SEASON), 
           data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L6 = plm(POS_VOTE ~ BIN_30_Mil*REP + BIN_30_FiftyMil*REP + BIN_30_FHMil*REP + BIN_30_Huge*REP + factor(Year) + factor(SEASON), 
           data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)
#Test Equality of Coefficients
linearHypothesis(Table3Senate$L2, "Lag_0_30_END_BIN + Lag_0_30_END_BIN:REP = 0", white.adjust = "hc1")
linearHypothesis(Table3House$L6, "BIN_30_Mil = BIN_30_Huge", white.adjust = "hc1")
linearHypothesis(Table3Senate$L2, c("Lag_0_30_END_BIN + Lag_0_30_END_BIN:REP = 0"), white.adjust = "hc1")
linearHypothesis(Table3Senate$L2, "Lag_0_30_END_BIN + Lag_0_30_END_BIN:REP = 0", white.adjust = "hc1")
linearHypothesis(Table3Senate$L6, "BIN_30_Mil + BIN_30_Mil:REP = 0", white.adjust = "hc1")
linearHypothesis(Table3Senate$L6, "BIN_30_FiftyMil + REP:BIN_30_FiftyMil = 0", white.adjust = "hc1")
linearHypothesis(Table3Senate$L6, "BIN_30_Huge + REP:BIN_30_Huge = 0", white.adjust = "hc1")
#Robust se's
Table3SenateSE = lapply(X = Table3Senate, GET_SEs)
#Output Latex Table
OUT = stargazer(Table3Senate, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table3SenateSE,
                title = "Heterogeneity: Severity x Party - Senate",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("$>0$","$>$1 Mil.","1-50 Mil.", "50-400 Mil.", "$>$400 Mil.", 
                                     "$>0$xREP", "$>$1 Mil x REP", "1-50 Mil x REP", "50-400 Mil x REP", "$>$400 Mil x REP"))
                
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table_3.tex", append = TRUE)

#---------------------------------------------------Section 1 Table 5: Differentiate Types of Storms----------------------------------#
#List of Tables For House
Table4House = list(
  L1 = plm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN + factor(Year), data = HOUSE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP + factor(Year), data = HOUSE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L3 = plm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN + factor(Year) + factor(SEASON), 
           data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L4 = plm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN*REP + factor(Year) + factor(SEASON),
           data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L5 = plm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP + Lag_0_30_END_NOT_PLACEBO_BIN*REP + factor(Year) + factor(SEASON), 
           data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)
#Robust se's
Table4HouseSE = lapply(X = Table4House, GET_SEs)
#Outputting latex table
OUT = stargazer(Table4House, style = "aer", type = "latex", column.sep.width = "4", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq", "rsq"), se = Table4HouseSE,
                title = "Cold-Related v. Non-Cold Storms -- House of Representatives",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("Cold $>0$", "Warm $>0$", "Cold $>0$ x Rep", "Warm $>0$ x Rep", "Warm $>0$ x Rep"))
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table4.tex", append = TRUE)
#List of Regressions for Senate
Table4Senate = list(
  L1 = plm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN + factor(Year), data = SENATE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP + factor(Year), data = SENATE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L3 = plm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT,
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L4 = plm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN*REP + factor(Year) + factor(SEASON), data = SENATE_DAT, 
           model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L5 = plm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP + Lag_0_30_END_NOT_PLACEBO_BIN*REP + factor(Year) + factor(SEASON), 
           data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)
#Robust Se's
Table4SenateSE = lapply(X = Table4Senate, GET_SEs)
#Output Latex Table
OUT = stargazer(Table4Senate, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table4SenateSE,
                title = "Cold-Related v. Non-Cold Storms - Senate",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("Cold $>0$", "Warm $>0$", "Cold $>0$ x Rep", "Warm $>0$ x Rep","Warm $>0$ x Rep"))
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table4.tex", append = TRUE)


#----------------------------------------Formatting Table for Inclusion in My Poster----------------------------------#
#List of Regressions
PosterTable = list(
  L1 = plm(POS_VOTE ~ Lag_0_30_END_BIN*REP + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP + factor(Year), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L3 = plm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN*REP + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L4 = plm(POS_VOTE ~ Lag_0_30_END_BIN*REP + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L5 = plm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP + factor(Year), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L6 = plm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN*REP + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)
#Standard Errors
Poster_Table_SE = lapply(PosterTable, GET_SEs)
#Output Latex Table
OUT = stargazer(PosterTable, style = "aer", type= "latex", column.sep.width = "1", no.space = T,
                omit = "factor*", keep.stat = c("n", "adj.rsq"), se = Poster_Table_SE,
                title = "Effect of Storm Type on Environmental Voting By Party",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("$>0$ Damage (All)", "$>0$ Damage (Cold)", "$>0$ Damage (Non-Cold)",
                                     "$>0$ Damage (All) x Rep", "$>0$ Damage (Cold) x Rep", "$>0$ Damage (Non-Cold) x Rep")
                )
cat(paste(OUT,"\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/PosterTable.tex", append = T)


#-----------------------------------------Corresponding Appendix Tables-------------------------------------#
#-------- ----------------01. Sensitivity to Different Definitions of Storm Severity Categories-------------#

#Creating 
HOUSE_DAT = HOUSE_DAT %>%
  mutate(ALT_1 = case_when(
           Lag_0_30_END %in% c(0,NA) ~ "Zero",
           (Lag_0_30_END > 0 & Lag_0_30_END < log(1000000)) ~ "Less_1Mil",
           (Lag_0_30_END >= log(1000000) & Lag_0_30_END < log(25000000)) ~ "25Mil",
           (Lag_0_30_END >= log(25000000) & Lag_0_30_END < log(500000000)) ~ "500Mil",
           Lag_0_30_END > log(500000000) ~ "Huge"),
        ALT2 = case_when(
           Lag_0_30_END %in% c(0,NA) ~ "Zero",
           (Lag_0_30_END > 0 & Lag_0_30_END < log(1000000)) ~ "Less_1Mil",
           (Lag_0_30_END >= log(1000000) & Lag_0_30_END < log(100000000)) ~ "10Mil",
           (Lag_0_30_END >= log(100000000) & Lag_0_30_END < log(1000000000)) ~ "100Mil",
           Lag_0_30_END > log(1000000000) ~ "Huge"))
SENATE_DAT = SENATE_DAT %>%
  mutate(ALT_1 = case_when(
           Lag_0_30_END %in% c(0,NA) ~ "Zero",
          (Lag_0_30_END > 0 & Lag_0_30_END < log(1000000)) ~ "Less_1Mil",
           (Lag_0_30_END >= log(1000000) & Lag_0_30_END < log(25000000)) ~ "25Mil",
          (Lag_0_30_END >= log(25000000) & Lag_0_30_END < log(500000000)) ~ "500Mil",
          Lag_0_30_END > log(500000000) ~ "Huge"),
        ALT2 = case_when(
          Lag_0_30_END %in% c(0,NA) ~ "Zero",
          (Lag_0_30_END > 0 & Lag_0_30_END < log(1000000)) ~ "Less_1Mil",
          (Lag_0_30_END >= log(1000000) & Lag_0_30_END < log(100000000)) ~ "10Mil",
          (Lag_0_30_END >= log(100000000) & Lag_0_30_END < log(1000000000)) ~ "100Mil",
          Lag_0_30_END > log(1000000000) ~ "Huge"))
#Running Regressions For Both Branches
App1 = list(
  L1 = plm(POS_VOTE ~ I(ALT_1 == "Less_1Mil") + I(ALT_1 == "25Mil") + I(ALT_1 == "500Mil") + I(ALT_1 == "Huge") + factor(Year) + factor(SEASON), 
           data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ I(ALT2 == "Less_1Mil") + I(ALT2 == "10Mil") + I(ALT2 == "100Mil") + I(ALT2 == "Huge") + factor(Year) + factor(SEASON), 
           data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L3 = plm(POS_VOTE ~ I(ALT_1 == "Less_1Mil") + I(ALT_1 == "25Mil") + I(ALT_1 == "500Mil") + I(ALT_1 == "Huge") + factor(Year) + factor(SEASON), 
           data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L4 = plm(POS_VOTE ~ I(ALT2 == "Less_1Mil") + I(ALT2 == "10Mil") + I(ALT2 == "100Mil") + I(ALT2 == "Huge") + factor(Year) + factor(SEASON), 
           data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)
#Extracting Standard Errors
App1SE = lapply(X = App1, GET_SEs)
#Output Latex Tables
OUT = stargazer(App1, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = App1SE,
                title = "Sensitivity to Different Storm Damage Bin Definitions",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("Less 1 Mil", "1-25 Mil", "25-500 Mil", "More 500 Mil", "Less 1 Mil", "1-100 Mil", "100 Mil - 1 Bil", "More 1 Bil"),
                out = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/App1_Boundaries.tex")
#----------------------02. Robustness to Logit Specification-----------------------------------------------#
#NOTE: For Senate, I am using "lm" and manually extracting clustered and robust standard errors -- easier for formatting
#NOTE: For Senate, I am using "feglm" command to handle greater number of fixed effects -- clustering and robust seems
#to be default (results look the same when I tested a few cases against results in STATA) -- this function is just difficult to manipulate
#for outputting results

#01A. Replicating Table 1:
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
  

#01B. Replicating Table 2:
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


#01C. Replicating Table 3
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
  

#01D. Replicating Table 4
Table4APP_H = list(
  L1 = feglm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN + factor(Year)  | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L2 = feglm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP - REP + factor(Year) | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L3 = feglm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN + factor(Year) + factor(SEASON)  | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L4 = feglm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN*REP - REP + factor(Year) + factor(SEASON)  | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L5 = feglm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP Lag_0_30_END_NOT_PLACEBO_BIN*REP - REP + factor(Year)  | Member.of.Congress, 
             data = HOUSE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit"))
)
esttex(Table4APP_H, coefstate ="se", yesNoFixed = "No", keepFactors = F,
       file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/LOGIT/T4.Logit.tex")

Table4APP_S = list(
  L1 = feglm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN + factor(Year)  | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L2 = feglm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP - REP + factor(Year) | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L3 = feglm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN + factor(Year) + factor(SEASON)  | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L4 = feglm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN*REP - REP + factor(Year) + factor(SEASON)  | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit")),
  L5 = feglm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP Lag_0_30_END_NOT_PLACEBO_BIN*REP - REP + factor(Year)  | Member.of.Congress, 
             data = SENATE_DAT, panel.id = c("Member.of.Congress", "PANEL_VAR"), family = binomial("logit"))
)
esttex(Table4APP_S, coefstate ="se", yesNoFixed = "No", keepFactors = F,
       file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/LOGIT/T4.Logit.tex")



