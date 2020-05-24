#-----------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------------------------#
#This Script Produces Regression Results For Section 2 of the Paper
#-----------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------Loading Required Libraries-------------------------#
library(tidyverse)
library(purrr)
library(parallel)
library(readxl)
library(data.table)
library(lubridate)
library(zoo)
library(plm)
library(sandwich)
library(lmtest)
library(stargazer)
library(car)


#-----------------------------------Import TNC Panel and Transform Damage Variables---------------------------------------#
TNC_CLEAN_WEEK_FINAL = read.csv("~/Google Drive/DATA/ECON/CLEAN/TNC_Merged_Week.csv") 
TNC_CLEAN_WEEK_FINAL = TNC_CLEAN_WEEK_FINAL %>%
  dplyr::mutate_at(which(str_detect(names(TNC_CLEAN_WEEK_FINAL), "Lag_")),  funs(log(.+1))) %>%
  dplyr::mutate_at(which(str_detect(names(TNC_CLEAN_WEEK_FINAL), "Lag_")), .funs = list(BIN = ~ ifelse(.>0,1,0))) %>%
  mutate(WEEK_LOW = as.Date(WEEK_LOW)) %>%
  mutate(AMT_LOG = log(AMT + 1)) %>% 
  mutate(PANEL_VAR = as.numeric(WEEK_LOW)) %>% #Panel "time" variable
  mutate(YEAR = year(WEEK_LOW),
         MONTH = month(WEEK_LOW),
         DEC = ifelse(MONTH == 12,1,0)) %>% #December indicator and other time variables
  filter(YEAR %in% as.character(2011:2017)) %>%
  mutate(
    BIN_30_Mil = ifelse( (Lag_0_30 > 0 & Lag_0_30 < log(1000000)),1,0),
    BIN_30_G_MIL = ifelse(Lag_0_30 > log(1000000),1,0),
    BIN_30_FiftyMil = ifelse( (Lag_0_30 > log(1000000) & Lag_0_30 < log(50000000)), 1,0),
    BIN_30_G_FiftyMil = ifelse(Lag_0_30 >log(50000000),1,0),
    BIN_30_FHMil = ifelse( (Lag_0_30 > log(50000000) & Lag_0_30 < log(400000000)), 1,0),
    BIN_30_Huge = ifelse(Lag_0_30 > log(400000000), 1,0)) %>%  #Creating Binned Damages
  ungroup()

#----------------------------------------Table of Means for Results Section 2------------------------------------#
TNC_CLEAN_WEEK_FINAL %>%
  select(COUNT_PER_MIL,Lag_0_30) %>%
  mutate(Lag_0_30 = exp(Lag_0_30)/1000000) %>%
  stargazer(., type = "latex",style = "aer", 
           title = "Summary Stats for TNC",
           summary.stat = c("n", "mean","min","max","sd"),
           covariate.labels = c("Donation Count", "30-Day Lagged Damage"),
           out = "~/Desktop/ECON Thesis/OUTPUT/DATA_Section/Table_of_Means_TNC.tex")


#---------------------------------Table 1: Build Time Lags ---------------------------------------#
#List of Regressions
Table1_TNC_Scaled = list(
  D = plm(COUNT_PER_MIL ~ Lag_0_30_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, 
          model = "within", index = c("state","WEEK_LOW")),
  E = plm(COUNT_PER_MIL ~ Lag_0_30_BIN + Lag_30_60_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, 
          model = "within", index = c("state","WEEK_LOW")),
  G = plm(COUNT_PER_MIL ~ Lag_0_30_BIN + Lag_30_60_BIN + Lag_60_90_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, 
          model = "within", index = c("state","WEEK_LOW")),
  A = plm(COUNT_PER_MIL ~ Lag_0_15_BIN + factor(YEAR) + factor(MONTH) , data = TNC_CLEAN_WEEK_FINAL, 
          model = "within", index = c("state","WEEK_LOW")),
  B = plm(COUNT_PER_MIL ~ Lag_0_15_BIN + Lag_15_30_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, 
          model = "within", index = c("state","WEEK_LOW")),
  C = plm(COUNT_PER_MIL ~ Lag_0_15_BIN + Lag_15_30_BIN + Lag_30_45_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, 
          model = "within", index = c("state","WEEK_LOW"))
) 
#Robust SE
Table1_TNC_Scaled_SE = lapply(Table1_TNC_Scaled, GET_SEs)
#Output Table
stargazer(Table1_TNC_Scaled, style = "aer", type = "latex", column.sep.width = "1", no.space = TRUE,
          omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table1_TNC_Scaled_SE,
          title = "Lagged Storm Damages on TNC Donations",
          dep.var.labels = "Count of TNC Donations",
          covariate.labels = c("0-30","30-60","60-90","0-15","15-30","30-45"),
          out = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_TNC/Scaled.tex")

#---------------------------------------Table 2: Different Storm Bins-------------------------------------------------#
#List of Regressions
Table2_TNC_Scaled = list(
  A = plm(COUNT_PER_MIL ~ Lag_0_30_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, 
          model = "within", index = c("state","WEEK_LOW")),
  D = plm(COUNT_PER_MIL ~ BIN_30_Mil + BIN_30_FiftyMil + BIN_30_FHMil + BIN_30_Huge + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, 
          model = "within", index = c("state","WEEK_LOW"))
) 
#Robust Standard Errors
Table2_TNC_Scaled_SE = lapply(X = Table2_TNC_Scaled, GET_SEs)
#Outputting Results
stargazer(Table2_TNC_Scaled, style = "aer", type = "latex", column.sep.width = "3", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table2_TNC_Scaled_SE,
                title = "Heterogeneity By Storm Severity",
                dep.var.labels = "Count of TNC Donations",
                covariate.labels = c(">0","0-1 Mil","1-50 Mil", "50-400 Mil", ">400 Mil"),
                out = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_TNC/Scaled2.tex", append = TRUE)
#Creating Figure for Poster
COEFS = Table2_TNC_Scaled$D$coefficients[1:4]  #Coefficients
COV = GET_SEs(Table2_TNC_Scaled$D)[1:4]        #SE estimates
DF = Table2_TNC_Scaled$D$df.residual
RES = data.frame(
  estimate = COEFS,
  conf.low = COEFS -  qt(0.975, DF) * COV,
  conf.high = COEFS +  qt(0.975, DF) * COV   #t-interval quantiles
) %>%
  rownames_to_column(var = "term")

RES$term  =factor(RES$term, c("BIN_30_Mil", "BIN_30_FiftyMil", "BIN_30_FHMil", "BIN_30_Huge"))

library(dotwhisker)
#Re-arranging Factors
RES = rbind(RES[4,], RES[3,], RES[2,], RES[1,])
#Creating Plot
dwplot(RES) + 
  scale_color_manual(values = "blue") +
  coord_flip() +
  scale_size_manual(values =3) +
  geom_vline(aes(xintercept = 0), col = "red", lty = 2)  +
  scale_y_discrete(labels = c("<1 Mil", "1-50 Mil", "50-400 Mil", ">400 Mil"))+
  ggtitle("95% Confidence Intervals For Coefficient Estimates") +
  xlab("Coefficient -  Donations Per Million People") +
  ylab("Cumulative 30-Day Lagged Storm Property Damage") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold", size =14),
        axis.text = element_text(size = 11),
       legend.position = "none",
       title = element_text(face = "bold", size = 14))

#Testing Equality of Coefficients
linearHypothesis(Table2_TNC_Scaled$D, "BIN_30_Mil = BIN_30_FHMil", white.adjust = "hc1")
linearHypothesis(Table2_TNC_Scaled$D, "BIN_30_FiftyMil = BIN_30_FHMil", white.adjust = "hc1")
linearHypothesis(Table2_TNC_Scaled$D, "BIN_30_FiftyMil = BIN_30_Mil", white.adjust = "hc1")
linearHypothesis(Table2_TNC_Scaled$D, "BIN_30_Huge = BIN_30_FHMil", white.adjust = "hc1")

#-------------------------------------------Table 3: Storm Types----------------------------------------------#
#List of Regressions
Table3_TNC_Scaled = list(
  A = plm(COUNT_PER_MIL ~ Lag_0_30_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, 
          model = "within", index = c("state","WEEK_LOW")),
  B = plm(COUNT_PER_MIL ~ Lag_Placebo_0_30_BIN + factor(YEAR) + factor(DEC), data = TNC_CLEAN_WEEK_FINAL,
          model = "within", index = c("state","WEEK_LOW")),
  C = plm(COUNT_PER_MIL ~ Lag_Not_Placebo_0_30_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL, 
          model = "within", index = c("state","WEEK_LOW")),
  D = plm(COUNT_PER_MIL ~ Lag_Not_Placebo_0_30_BIN + Lag_Placebo_0_30_BIN + factor(YEAR) + factor(MONTH), data = TNC_CLEAN_WEEK_FINAL,
          model = "within", index = c("state","WEEK_LOW"))
)
#Robust SE
Table3_TNC_Scaled_SE = lapply(X = Table3_TNC_Scaled, GET_SEs)
#Outputting Results
stargazer(Table3_TNC_Scaled, style = "aer", type = "latex", column.sep.width = "3", no.space = TRUE,
                omit = "factor*", keep.stat = c("n","adj.rsq"), se = Table3_TNC_Scaled_SE,
                title = "Heterogeneity By Storm Severity",
                covariate.labels = c("$>0 Damage (All)","$>0$ Damage (Cold)","$>0$ Damage (Non-Cold)"),
                dep.var.labels = "Count of TNC Donations",
                out = "~/Desktop/ECON Thesis/OUTPUT/STORM_ON_TNC/Scaled3.tex")


