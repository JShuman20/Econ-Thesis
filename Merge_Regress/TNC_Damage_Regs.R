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


#Log and Binning
TNC_CLEAN_WEEK_FINAL = read.csv("~/Google Drive/DATA/ECON/CLEAN/TNC_Merged_Week.csv") %>%
  mutate(WEEK_LOW = as.Date(WEEK_LOW)) %>%
  mutate_at(vars(contains("Lag")), ~ log(.+1)) %>%
  mutate_at(vars(contains("Lag")), .funs = list(BIN = ~ifelse(.>0,1,0))) %>%
  mutate_at(vars(5:14), .funs = list(MED = ~ifelse(.>median(.),1,0))) %>%
  mutate(AMT_LOG = log(AMT + 1)) %>%
  mutate(PANEL_VAR = as.numeric(WEEK_LOW)) %>%
  mutate(YEAR = year(WEEK_LOW),
         MONTH = month(WEEK_LOW),
         DEC = ifelse(MONTH == 12,1,0)) %>%
  filter(YEAR %in% as.character(2011:2017)) %>%
  mutate(
    BIN_30_Mil = ifelse( (Lag_0_30 > 0 & Lag_0_30 < log(1000000)),1,0),
    BIN_30_G_MIL = ifelse(Lag_0_30 > log(1000000),1,0),
    BIN_30_FiftyMil = ifelse( (Lag_0_30 > log(1000000) & Lag_0_30 < log(50000000)), 1,0),
    BIN_30_G_FiftyMil = ifelse(Lag_0_30 >log(50000000),1,0),
    BIN_30_FHMil = ifelse( (Lag_0_30 > log(50000000) & Lag_0_30 < log(400000000)), 1,0),
    BIN_30_Huge = ifelse(Lag_0_30 > log(400000000), 1,0)) %>%
  ungroup()

#-1. Build Time Trend

Table0 = list(
  A = plm(COUNT ~ Lag_0_15_BIN + factor(YEAR) + factor(MONTH) , data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")) %>%
    coeftest(., vcov = vcovHC(., type="HC1")),
  B = plm(COUNT ~ Lag_0_15_BIN + Lag_15_30_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")) %>%
    coeftest(., vcov = vcovHC(., type="HC1")),
  C = plm(COUNT ~ Lag_0_15_BIN + Lag_15_30_BIN + Lag_30_45_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")) %>%
    coeftest(., vcov = vcovHC(., type="HC1")),
  D = plm(COUNT ~ Lag_0_30_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")) %>%
    coeftest(., vcov = vcovHC(., type="HC1")),
  E = plm(COUNT ~ Lag_0_30_BIN + Lag_30_60_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")) %>%
    coeftest(., vcov = vcovHC(., type="HC1"))
) %>%
  stargazer::stargazer(.,type = "latex", style = "aer", column.sep.width = "1", omit = "factor*", title = "Weeks: Conditional - Count")

Table1_TNC= list(
  D = plm(COUNT ~ Lag_0_30_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")),
  E = plm(COUNT ~ Lag_0_30_BIN + Lag_30_60_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")),
  G = plm(COUNT ~ Lag_0_30_BIN + Lag_30_60_BIN + Lag_60_90_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")),
  A = plm(COUNT ~ Lag_0_15_BIN + factor(YEAR) + factor(MONTH) , data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")),
  B = plm(COUNT ~ Lag_0_15_BIN + Lag_15_30_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")),
  C = plm(COUNT ~ Lag_0_15_BIN + Lag_15_30_BIN + Lag_30_45_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW"))
) 

Table1_TNC_SE = lapply(X = Table1_TNC, GET_SEs)
OUT = stargazer(Table1_TNC, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table1_TNC_SE,
                title = "Lagged Storm Damages on TNC Donations",
                dep.var.labels = "Count of TNC Donations",
                covariate.labels = c("0-30","30-60","60-90","0-15","15-30","30-45"))
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_TNC/Table1.tex", append = TRUE)


Table2_TNC = list(
  A = plm(COUNT ~ Lag_0_30_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")),
  B = plm(COUNT ~ Lag_0_30_BIN*Lag_0_30 - Lag_0_30 + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")),
  C = plm(COUNT ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_G_FiftyMil + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")),
  D = plm(COUNT ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW"))
) 
Table2_TNC_SE = lapply(X = Table2_TNC, GET_SEs)
OUT = stargazer(Table2_TNC, style = "aer", type = "latex", column.sep.width = "3", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table2_TNC_SE,
                title = "Heterogeneity By Storm Severity",
                dep.var.labels = "Count of TNC Donations",
                covariate.labels = c(">0","0-1 Mil","1-50 Mil", ">50 Mil", "50-400 Mil", ">400 Mil", "Interaction"))
cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_TNC/Table2.tex", append = TRUE)


Table3_TNC = list(
  A = plm(COUNT ~ Lag_0_30_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")),
  B = plm(COUNT ~ Lag_Placebo_0_30_BIN + factor(YEAR) + factor(DEC), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")),
  C = plm(COUNT ~ Lag_Not_Placebo_0_30_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")),
  D = plm(COUNT ~ Lag_0_30_BIN*Lag_0_30 - Lag_0_30 + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")),
  E = plm(COUNT ~ Lag_Placebo_0_30_BIN*Lag_Placebo_0_30 - Lag_Placebo_0_30 + factor(YEAR) + factor(DEC), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW")),
  G = plm(COUNT ~ Lag_Not_Placebo_0_30_BIN*Lag_Not_Placebo_0_30 - Lag_Not_Placebo_0_30 + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, model = "within", index = c("state","WEEK_LOW"))
)
Table3_TNC_SE = lapply(X = Table3_TNC, GET_SEs)
OUT = stargazer(Table3_TNC, style = "aer", type = "latex", column.sep.width = "3", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table3_TNC_SE,
                title = "Heterogeneity By Storm Severity",
                covariate.labels = c("ALL_Bin","Cold_Bin","Not Cold Bin","All Int","Cold Int","Not Cold Int"),
                dep.var.labels = "Count of TNC Donations")


cat(paste(OUT, "\n"), file = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_TNC/Table2.tex", append = TRUE)
