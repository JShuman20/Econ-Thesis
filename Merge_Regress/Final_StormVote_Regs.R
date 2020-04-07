#--------------------------Final Versions of Regression Output-----------------------#
library(plm)
library(tidyverse)
library(multiwayvcov)
library(purrr)
library(sandwich)
library(stargazer)
library(lmtest)
library(data.table)
library(car)

#Read in Data
SENATE_DAT = fread("~/Google Drive/DATA/ECON/CLEAN/SENATE_LAGGED.csv")

SENATE_DAT = SENATE_DAT %>%
  mutate_at(vars(contains("Lag_")), ~ log(.+1)) %>%
  mutate_at(vars(contains("Lag_")), .funs = list(BIN = ~ifelse(.>0,1,0))) %>%
  mutate(BIN_30_Mil = ifelse( (Lag_0_30_END > 0 & Lag_0_30_END < log(1000000)),1,0),
         BIN_30_G_MIL = ifelse(Lag_0_30_END > log(1000000),1,0),
         BIN_30_FiftyMil = ifelse( (Lag_0_30_END > log(1000000) & Lag_0_30_END < log(50000000)), 1,0),
         BIN_30_G_FiftyMil = ifelse(Lag_0_30_END >log(50000000),1,0),
         BIN_30_FHMil = ifelse( (Lag_0_30_END > log(50000000) & Lag_0_30_END < log(400000000)), 1,0),
         BIN_30_Huge = ifelse(Lag_0_30_END > log(400000000), 1,0)) %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(SEASON = case_when(
    MON_NUM %in% c(12,1,2,3) ~ "WINTER",
    MON_NUM %in% c(4,5,6) ~ "SPRING",
    MON_NUM %in% c(7,8,9) ~ "SUMMER",
    MON_NUM %in% c(10,11,12) ~ "FALL")) %>%
  mutate(PANEL_VAR = 100000*Year + ROLL_CALL)  %>%
  mutate(REP = ifelse(Party == "R",1,0))


HOUSE_DAT = fread("~/Google Drive/DATA/ECON/CLEAN/HOUSE_LAGGED.csv")
HOUSE_DAT = HOUSE_DAT %>%
  mutate_at(vars(contains("Lag_")), ~ log(.+1)) %>%
  mutate_at(vars(contains("Lag_")), .funs = list(BIN = ~ifelse(.>0,1,0))) %>%
  mutate(BIN_30_Mil = ifelse( (Lag_0_30_END > 0 & Lag_0_30_END < log(1000000)),1,0),
         BIN_30_G_MIL = ifelse(Lag_0_30_END > log(1000000),1,0),
         BIN_30_FiftyMil = ifelse( (Lag_0_30_END > log(1000000) & Lag_0_30_END < log(50000000)), 1,0),
         BIN_30_G_FiftyMil = ifelse(Lag_0_30_END >log(50000000),1,0),
         BIN_30_FHMil = ifelse( (Lag_0_30_END > log(50000000) & Lag_0_30_END < log(400000000)), 1,0),
         BIN_30_Huge = ifelse(Lag_0_30_END > log(400000000), 1,0)) %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(SEASON = case_when(
    MON_NUM %in% c(12,1,2,3) ~ "WINTER",
    MON_NUM %in% c(4,5,6) ~ "SPRING",
    MON_NUM %in% c(7,8,9) ~ "SUMMER",
    MON_NUM %in% c(10,11,12) ~ "FALL")) %>%
  mutate(PANEL_VAR = 100000*Year + ROLL_CALL) %>%
  mutate(Member.of.Congress = case_when(
    Member.of.Congress == "Rogers, Mike"  & State == "AL" ~ "Rogers, Mike_AL",
    Member.of.Congress == "Brown, George" & State == "CO" ~ "Brown, George_CO",
    TRUE ~ Member.of.Congress)) %>% #This is an Extra Catch -- didnt seem to get caught the first time
  mutate(REP = ifelse(Party == "R",1,0))


#Define a Function for Robust
GET_SEs = function(MODEL){
  COV = vcovHC(MODEL, type = "HC1")
  return(sqrt(diag(COV)))
}

#--------------------------------------------Table 1: Build Lags for House and Senate---------------------------#
Table1_House = list(
  L4 = plm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L5 = plm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L6 = plm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + Lag_60_90_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L1 = plm(POS_VOTE ~ Lag_0_15_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ Lag_0_15_END_BIN + Lag_15_30_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L3 = plm(POS_VOTE ~ Lag_0_15_END_BIN + Lag_15_30_END_BIN + Lag_30_45_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
) 
Table1_House_SE = lapply(X = Table1_House, GET_SEs)
OUT = stargazer(Table1_House, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table1_House_SE,
                title = "Storm Damages Over Different Time Lags on Voting",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("0-15","15-30","30-45","0-30","30-60","60-90"))
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table_1.tex", append = TRUE)

 

Table1_Senate = list(
  L4 = plm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L5 = plm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L6 = plm(POS_VOTE ~ Lag_0_30_END_BIN + Lag_30_60_END_BIN + Lag_60_90_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L1 = plm(POS_VOTE ~ Lag_0_15_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ Lag_0_15_END_BIN + Lag_15_30_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L3 = plm(POS_VOTE ~ Lag_0_15_END_BIN + Lag_15_30_END_BIN + Lag_30_45_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)
Table1_Senate_SE = lapply(X = Table1_Senate, GET_SEs)
OUT = stargazer(Table1_Senate, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table1_Senate_SE,
                title = "Storm Damages Over Different Time Lags on Voting",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("0-30","30-60","60-90","0-15","15-30","30-45"))
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table_1.tex", append = TRUE)


#--------------------------------------Table 2: Discrete and Continuous Lags-------------------------------------------#
Table2House = list(
  L1 = plm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L1 = plm(POS_VOTE ~ Lag_0_30_END_BIN*Lag_0_30_END - Lag_0_30_END + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
 # L3 = plm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_G_FiftyMil +  factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L4 = plm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)
#Test Equality of Coefficients
linearHypothesis(Table2House$L4, c("BIN_30_FiftyMil - BIN_30_FHMil = 0"))
linearHypothesis(Table2House$L4, c("BIN_30_Huge - BIN_30_Mil = 0"))
linearHypothesis(Table2House$L4, c("BIN_30_Huge - BIN_30_FHMil = 0"))

Table2_House_SE = lapply(X = Table2House, GET_SEs)
OUT = stargazer(Table2House, style = "aer", type = "latex", column.sep.width = "5", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table2_House_SE,
                title = "Heterogeneous Effects By Severity of Storm Damages",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("/> 0", "$<$1 Mil.", "1-50 Mil.", "50-400 Mil.","$>$400 Mil","Interaction"))
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table_2.tex", append = TRUE)


Table2SENATE = list(
  L1 = plm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L1 = plm(POS_VOTE ~ Lag_0_30_END_BIN*Lag_0_30_END - Lag_0_30_END + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
 # L3 = plm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_G_FiftyMil +  factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L4 = plm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)
#Hypothesis Tests
linearHypothesis(Table2SENATE$L4, c("BIN_30_FiftyMil - BIN_30_FHMil = 0"))
linearHypothesis(Table2SENATE$L4, c("BIN_30_Huge - BIN_30_Mil = 0"))
linearHypothesis(Table2SENATE$L4, c("BIN_30_Huge - BIN_30_FHMil = 0"))

Table2_SENATE_SE = lapply(X = Table2SENATE, GET_SEs)
OUT = stargazer(Table2SENATE, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table2_SENATE_SE,
                title = "Heterogeneous Effects By Severity of Storm Damages",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("$>0$","$<$1 Mil", "1-50 Mil.", "50-400 Mil", "$>$400 Mil.", "Interaction"))
               
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table_2.tex", append = TRUE)


#--------------------------------------------------------Table 3: Party Interaction------------------------------#
Table3House = list(
  L1 = plm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ Lag_0_30_END_BIN*REP + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
 # L3 = plm(POS_VOTE ~ Lag_0_30_END_BIN*Lag_0_30_END - Lag_0_30_END + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
#  L4 = plm(POS_VOTE ~ Lag_0_30_END_BIN*Lag_0_30_END*REP - REP - Lag_0_30_END - Lag_0_30_END*REP - Lag_0_30_END_BIN*REP + Lag_0_30_END_BIN  + factor(Year) + factor(SEASON), 
 #          data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L5 = plm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge + factor(Year) + factor(SEASON), 
           data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L6 = plm(POS_VOTE ~ BIN_30_Mil*REP + BIN_30_FiftyMil*REP + BIN_30_FHMil*REP + BIN_30_Huge*REP + factor(Year) + factor(SEASON), 
           data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)
Table3HouseSE = lapply(X = Table3House, GET_SEs)
OUT = stargazer(Table3House, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table3HouseSE,
                title = "Heterogeneity: Severity x Party",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("$>0$","$>$1 Mil.","1-50 Mil.", "50-400 Mil.", "$>$400 Mil.", 
                                     "$>0$xREP", "$>$1 Mil x REP", "1-50 Mil x REP", "50-400 Mil x REP", "$>$400 Mil x REP"))
                
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table_3.tex", append = TRUE)

linearHypothesis(Table3House$L6, "BIN_30_Huge + REP:BIN_30_Huge = 0")               
                
Table3Senate = list(
  L1 = plm(POS_VOTE ~ Lag_0_30_END_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ Lag_0_30_END_BIN*REP + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
#  L3 = plm(POS_VOTE ~ Lag_0_30_END_BIN*Lag_0_30_END - Lag_0_30_END + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
#  L4 = plm(POS_VOTE ~ Lag_0_30_END_BIN*Lag_0_30_END*REP - REP - Lag_0_30_END - Lag_0_30_END*REP - Lag_0_30_END_BIN*REP + Lag_0_30_END_BIN  + factor(Year) + factor(SEASON), 
#           data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L5 = plm(POS_VOTE ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge + factor(Year) + factor(SEASON), 
           data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L6 = plm(POS_VOTE ~ BIN_30_Mil*REP + BIN_30_FiftyMil*REP + BIN_30_FHMil*REP + BIN_30_Huge*REP + factor(Year) + factor(SEASON), 
           data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)
linearHypothesis(Table3House$L6, "BIN_30_Mil = BIN_30_Huge")
linearHypothesis(Table3Senate$L2, c("Lag_0_30_END_BIN + Lag_0_30_END_BIN:REP = 0"))

Table3SenateSE = lapply(X = Table3Senate, GET_SEs)
OUT = stargazer(Table3Senate, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table3SenateSE,
                title = "Heterogeneity: Severity x Party - Senate",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("$>0$","$>$1 Mil.","1-50 Mil.", "50-400 Mil.", "$>$400 Mil.", 
                                     "$>0$xREP", "$>$1 Mil x REP", "1-50 Mil x REP", "50-400 Mil x REP", "$>$400 Mil x REP"))
                
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table_3.tex", append = TRUE)

linearHypothesis(Table3Senate$L2, "Lag_0_30_END_BIN + Lag_0_30_END_BIN:REP = 0")
linearHypothesis(Table3Senate$L6, "BIN_30_Mil + BIN_30_Mil:REP = 0")
linearHypothesis(Table3Senate$L6, "BIN_30_FiftyMil + REP:BIN_30_FiftyMil = 0")
linearHypothesis(Table3Senate$L6, "BIN_30_Huge + REP:BIN_30_Huge = 0")
#---------------------------------------------------Table 4: Differentiate Types of Storms----------------------------------#
Table4House = list(
  L1 = plm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN + factor(Year), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP + factor(Year), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
 # L3 = plm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*Lag_0_30_END_PLACEBO*REP - REP*Lag_0_30_END_PLACEBO - Lag_0_30_END_PLACEBO_BIN*REP + Lag_0_30_END_PLACEBO_BIN + factor(Year),
#           data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L4 = plm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L5 = plm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN*REP + factor(Year) + factor(SEASON), data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
#  L6 = plm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN*Lag_0_30_END_NOT_PLACEBO*REP - REP*Lag_0_30_END_NOT_PLACEBO - Lag_0_30_END_NOT_PLACEBO_BIN*REP + Lag_0_30_END_NOT_PLACEBO_BIN + factor(Year),
#           data = HOUSE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)

Table4HouseSE = lapply(X = Table4House, GET_SEs)
OUT = stargazer(Table4House, style = "aer", type = "latex", column.sep.width = "4", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq", "rsq"), se = Table4HouseSE,
                title = "Cold-Related v. Non-Cold Storms -- House of Representatives",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("Cold $>0$", "Warm $>0$", "Cold $>0$ x Rep", "Warm $>0$ x Rep"))
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table4.tex", append = TRUE)

Table4Senate = list(
  L1 = plm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN + factor(Year), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L2 = plm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*REP + factor(Year), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
#  L3 = plm(POS_VOTE ~ Lag_0_30_END_PLACEBO_BIN*Lag_0_30_END_PLACEBO*REP - REP*Lag_0_30_END_PLACEBO - Lag_0_30_END_PLACEBO_BIN*REP + Lag_0_30_END_PLACEBO_BIN + factor(Year),
#           data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L4 = plm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR")),
  L5 = plm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN*REP + factor(Year) + factor(SEASON), data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
#  L6 = plm(POS_VOTE ~ Lag_0_30_END_NOT_PLACEBO_BIN*Lag_0_30_END_NOT_PLACEBO*REP - REP*Lag_0_30_END_NOT_PLACEBO - Lag_0_30_END_NOT_PLACEBO_BIN*REP + Lag_0_30_END_NOT_PLACEBO_BIN + factor(Year),
#           data = SENATE_DAT, model = "within", index = c("Member.of.Congress","PANEL_VAR"))
)

Table4SenateSE = lapply(X = Table4Senate, GET_SEs)
OUT = stargazer(Table4Senate, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table4SenateSE,
                title = "Cold-Related v. Non-Cold Storms - Senate",
                dep.var.labels = "Positive Environmental Vote",
                covariate.labels = c("Cold $>0$", "Warm $>0$", "Cold $>0$ x Rep", "Warm $>0$ x Rep"))
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_VOTE/Table4.tex", append = TRUE)



