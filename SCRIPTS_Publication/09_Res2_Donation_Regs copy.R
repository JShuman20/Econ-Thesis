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
library(fixest)




#-----------------------------------------------------------------Day-Level Regressions------------------------------------#
TNC_DAY = read_xlsx("~/Google Drive/DATA/ECON/CLEAN/TNC_CLEAN_DAY_MERGED.xlsx")

#Constructing New Measures
TNC_DAY = TNC_DAY %>%
  mutate(AMT_PER_DON = AMT/COUNT,
         AMT_PER_MIL = AMT/(POP/1000000),
         BIN_FIVE = cut(Lag_0_30, breaks = c(-Inf,0,quantile(Lag_0_30[which(Lag_0_30>0)],c(1/4,1/2,3/4)),Inf),
                        labels = c("Zero", "Q1", "Q2","Q3", "Q4")),
         BIN1 = ifelse(BIN_FIVE == "Q1",1,0),
         BIN2 = ifelse(BIN_FIVE  == "Q2", 1,0),
         BIN3 = ifelse(BIN_FIVE == "Q3",1,0),
         BIN4 = ifelse(BIN_FIVE == "Q4", 1,0),
         DEC = ifelse(MONTH == 12,1,0),
         MONTH = factor(MONTH))

#-----------------------------------------------------------Make Portion of the Summary Stats table---------------------------#
TNC_DAY %>%
  select(COUNT_PER_MIL, Lag_0_30_Bin) %>%
  rename(`Donation Count Per Million State Residents` = COUNT_PER_MIL,
         `30-Day Lagged Storm Damages` =Lag_0_30_Bin) %>%
  as.data.frame() %>%
  stargazer(., type = 'latex', style = 'AER', summary.stat = c("n", "mean","max","sd"))
         


#---------------------------------------------------------------Day Level Results---------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------#
#Table 1: Build Time Lags
Day_Level_T1 = list(
  A = feols(COUNT_PER_MIL ~ Lag_0_1_Bin | state + YEAR + MONTH + DAYOFWEEK, data = TNC_DAY),
  B = feols(COUNT_PER_MIL ~ Lag_0_7_Bin | state + YEAR + MONTH + DAYOFWEEK, data = TNC_DAY),
  C = feols(COUNT_PER_MIL ~ Lag_0_30_Bin | state + YEAR + MONTH + DAYOFWEEK, data = TNC_DAY),
  D = feols(COUNT_PER_MIL ~ Lag_0_30_Bin + Lag_30_60_Bin | state + YEAR + MONTH + DAYOFWEEK, data = TNC_DAY),
  E = feols(COUNT_PER_MIL ~ Lag_0_30_Bin + Lag_30_60_Bin + Lag_60_90_Bin| state + YEAR + MONTH + DAYOFWEEK, data = TNC_DAY))
#Viewing Results
etable(Day_Level_T1)
#Writing t0 Table
etable(Day_Level_T1, tex = T, coefstat = "se", keepFactors = F, fitstat = ~r2,
       digits = 3, file = "~/Desktop/ECON Thesis/OUTPUT_Publication/TNC_DAY_T1.tex")

#Table 2: Compare Different Magnitudes
Day_Level_T2 = list(
  A = feols(COUNT_PER_MIL ~ Lag_0_30_Bin | state + YEAR + MONTH + DAYOFWEEK, data = TNC_DAY),
  B = feols(COUNT_PER_MIL ~ BIN1 + BIN2 + BIN3 + BIN4 | state + YEAR + MONTH + DAYOFWEEK, data = TNC_DAY)
)
#Viewing Results
etable(Day_Level_T2)
#Printing Results
etable(Day_Level_T2, tex = T, coefstat = "se", keepFactors = F, fitstat = ~r2,
       digits = 3, file = "~/Desktop/ECON Thesis/OUTPUT_Publication/TNC_DAY_T2.tex")

#Table 3: Cold and Non-Cold Storms
DAY_Level_T3 = list(
  A = feols(COUNT_PER_MIL ~ Lag_0_30_Bin | state + YEAR  + MONTH + DAYOFWEEK, data = TNC_DAY),
  B = feols(COUNT_PER_MIL ~ Not_Placebo_Lag_0_30_Bin | state + YEAR  + MONTH + DAYOFWEEK, data = TNC_DAY),
  C = feols(COUNT_PER_MIL ~ Placebo_Lag_0_30_Bin | state + YEAR  + MONTH + DAYOFWEEK, data = TNC_DAY),
  D = feols(COUNT_PER_MIL ~ Placebo_Lag_0_30_Bin + Not_Placebo_Lag_0_30_Bin | state + YEAR  + MONTH + DAYOFWEEK, data = TNC_DAY)
)
#Viewing Results
etable(DAY_Level_T3)
#Printing Results
etable(DAY_Level_T3, tex = T, coefstat = "se", keepFactors = F, fitstat = ~r2,
       digits = 3, file = "~/Desktop/ECON Thesis/OUTPUT_Publication/TNC_DAY_T3.tex")
#Accompanying Information: Why is the Winter So Wierd
Cold_Mod    = feols(COUNT_PER_MIL ~ Placebo_Lag_0_30_Bin*MONTH -MONTH | state + YEAR + MONTH + DAYOFWEEK, data = TNC_DAY)
Non_Cold_Mod = feols(COUNT_PER_MIL ~ Not_Placebo_Lag_0_30_Bin*MONTH - MONTH | state + YEAR + MONTH + DAYOFWEEK, data = TNC_DAY)
Overall_Mod = feols(COUNT_PER_MIL ~ Lag_0_30_Bin*MONTH - MONTH | state + YEAR + MONTH + DAYOFWEEK, data = TNC_DAY)
Combined_Mod = feols(COUNT_PER_MIL ~ Placebo_Lag_0_30_Bin*MONTH - MONTH + 
                                     Not_Placebo_Lag_0_30_Bin*MONTH - MONTH | state + YEAR + MONTH + DAYOFWEEK, data = TNC_DAY)
#Cold Labs
map(list(Cold_Mod,Non_Cold_Mod),
           .f = function(.x){
             LABS = coeftable(.x) %>%
               rownames_to_column(var = 'Coef') %>%
               mutate(Coef = ifelse(test =str_detect(Coef,'Not'),
                                    yes = str_replace(Coef,'Placebo_Lag_0_30_Bin','Cold'),
                                    no = str_replace(Coef,'Not_Placebo_Lag_0_30_Bin','Non-Cold')),
                      Coef = str_replace(Coef, ':MONTH','x MON_')) %>%
               pull(Coef)
             PLOT = ggstatsplot::ggcoefstats(.x,stats.labels = F) + 
                               # scale_y_discrete(labels = LABS) +
                                ggtitle('Heterogeneity in Cold Storm Exposure Effect Size By Month') +
                                xlab('Coefficient Estimate') + ylab('Estimate + 95% Confidence Interval') +
                                xlim(c(-3,4.5)) 
             return(PLOT)
           })
Mods

#Plotting Combined Effects
#01. Get TOtal Effect Size


Get_SE_Combined = function(VCOV, Interaction, Main){
  Int = str_replace(Interaction,':','.')
  NAMES = c(Main,Int)
  VCOV_Sub = VCOV %>%
    filter(Coef %in% c(Main,Interaction)) %>%
    dplyr::select(c(NAMES)) %>%
    as.matrix()
  return(sqrt(sum(VCOV_Sub)))
}

MOD_TABLE = coeftable(Combined_Mod) %>% 
  rownames_to_column(var = 'Coef') %>%
  mutate(Jan = ifelse(str_detect(Coef, ':'),0,1),
         MON = str_extract(Coef, "MONTH[[:digit:]]+"),
         MON = ifelse(is.na(MON),1,str_remove(MON,'MONTH')),
         Cold = ifelse(str_detect(Coef, 'Not'),0,1),
         Main_Effect = ifelse(Cold == 1, 'Placebo_Lag_0_30_Bin','Not_Placebo_Lag_0_30_Bin')) %>%
  rowwise() %>%
  mutate(CoefSum = case_when(Jan == 1 ~ Estimate,
                             Jan == 0 & Cold == 1 ~ Estimate + MOD_TABLE$Estimate[which(MOD_TABLE$Coef == 'Placebo_Lag_0_30_Bin')],
                             T ~ Estimate + MOD_TABLE$Estimate[which(MOD_TABLE$Coef == 'Not_Placebo_Lag_0_30_Bin')])) %>%
  rowwise() %>%
  mutate(SEs = ifelse(Jan == 1, `Std. Error`,Get_SE_Combined(Vcov_Combined_Mod,
                                                Interaction = as.character(Coef),
                                                Main   = as.character(Main_Effect))))  %>%
  dplyr::select(Coef,CoefSum,SEs,MON, Cold)

MON_Specific = TNC_DAY %>%
  filter(MONTH != 8) %>%
  group_split(MONTH) %>%
  map_dfr(.x = ., .f = function(.x){
    MON = .x$MONTH[1]
    feols(COUNT_PER_MIL ~ Not_Placebo_Lag_0_30_Bin + Placebo_Lag_0_30_Bin | state + YEAR + DAYOFWEEK, data = .x) %>%
      coeftable() %>%
      rownames_to_column(var = 'Coef') %>%
      mutate(MON = MON)}) %>%
  mutate(Cold = ifelse(str_detect(Coef, 'Not'),'NC','Cold'),
         LOW = Estimate - 1.96 * `Std. Error`,
         HIGH = Estimate + 1.96* `Std. Error`)



PLOT_MODEL = function(DAT, CoefCOL, SECOL){
  DAT = DAT %>%
    rename(c('SE' = SECOL)) %>%
    rename(c('Coefficient' = CoefCOL)) %>%
    mutate(LOW = Coefficient - 1.96*SE,
           HIGH = Coefficient + 1.96*SE,
           MON = as.numeric(MON),
           Cold = as.factor(Cold))
  print(head(DAT))
  P = ggplot(DAT) +
    geom_point(aes(x = Cold, y = Coefficient), col = 'blue') +
    geom_errorbar(aes(x = Cold, ymin = LOW, ymax = HIGH)) +
    geom_hline(yintercept = 0, col = 'red') +
    facet_grid(cols = vars(MON))
  return(P)
    
}


#Final Product
PLOT_MODEL(MOD_TABLE, 'CoefSum','SEs') +
  ggtitle('Cold and Non-Cold by Month') +
  xlab('Cold and Non-Cold Indicators: Sum of Main and Month Interaction Effects') +
  ylab('Coefficient + 95% CI')

PLOT_MODEL(MON_Specific,'Estimate', "Std. Error") +
  ggtitle('Cold and Non-Cold by Month') +
  xlab('Cold and Non-Cold Indicators: Regressions Run By Month') +
  ylab('Coefficient + 95% CI')



#Alternative Visualization
MON_Specific

table(TNC_DAY$MONTH,TNC_DAY$Not_Placebo_Lag_0_30_Bin)
  
TNC_DAY %>%
  filter(MONTH == 7 & Placebo_Lag_0_30_Bin==1)
  
MOD_TABLE = MOD_TABLE %>%
  dplyr::select(Coef,Estimate, CoefSum,SEs)




Get_SE_Combined(Vcov_Combined_Mod, Main = 'Placebo_Lag_0_30_Bin',Interaction = 'Placebo_Lag_0_30_Bin:MONTH4')


append(c('Placebo_Lag_0_30_Bin',"Placebo_Lag_0_30_Bin.MONTH4"), 'Coef')
MOD_TABLE

Vcov_Combined_Mod = vcov(Combined_Mod) %>%
  data.frame() %>%
  rownames_to_column(var = 'Coef')
colnames(Vcov_Combined_Mod)

Vcov_Combined_Mod %>% select('Placebo_Lag_0_30_Bin') %>% filter(Coef )



#Pull Jan Effects
Jan_Effects = MOD_TABLE %>% filter(!str_detect(Coef, ':'))
Interactions = MOD_TABLE %>% filter(!str_detect(Coef, ':'))
Jan_Effects
MOD_ARGS[!str_detect(MOD_ARGS, ':')]

Combined_Mod_VCOV = data.frame(vcov(Combined_Mod)) 
colnames(Combined_Mod_VCOV)

DIAG = diag(vcov(Combined_Mod))
DIAG['Placebo_Lag_0_30_Bin']

LABS = coeftable(Combined_Mod) %>%
  rownames_to_column(var = 'Coef') %>%
  mutate(Coef = str_replace(Coef, 'Not_Placebo_Lag_0_30_Bin', 'Non-Cold'),
         Coef = str_replace(Coef, 'Placebo_Lag_0_30_Bin', 'Cold'),
         Coef = str_replace(Coef, 'MONTH', 'MON')) %>%
  pull(Coef)
  


ggstatsplot::ggcoefstats(Combined_Mod, stats.labels = F, stats.label.color = F) +
  scale_y_discrete(labels = LABS) +
  ggtitle('Month-Specific Effects on TNC Donations\n Including state, year, and day-of-week FE')
  
  


coefplot(Combined_Mod)


Plot_Labs = coeftable(Cold_Mod) %>% 
  rownames_to_column(var = 'Coef') %>%
  mutate(Coef = str_replace(Coef, 'Placebo_Lag_0_30_Bin', 'Cold')) %>%
  mutate(Coef = str_replace(Coef, ':MONTH', ' x MON_')) %>%
  pull(Coef)

ggstatsplot::ggcoefstats(Cold_Mod, stats.labels = F) +
  scale_y_discrete(labels = Plot_Labs) +
  ggtitle('Heterogeneity in Cold Storm Exposure Effect Size By Month') +
  xlab('Coefficient Estimate') + ylab('Estimate + 95% Confidence Interval') +
  xlim(c(-3,4.5)) + coord_flip()
#Overall Labs
Plot_Labs = coeftable(Overall_Mod) %>% 
  rownames_to_column(var = 'Coef') %>%
  mutate(Coef = str_replace(Coef, 'Lag_0_30_Bin', 'Dam')) %>%
  mutate(Coef = str_replace(Coef, ':MONTH', ' x MON_')) %>%
  pull(Coef)
ggstatsplot::ggcoefstats(Overall_Mod, stats.labels = F) +
  scale_y_discrete(labels = Plot_Labs) +
  ggtitle('Heterogeneity in Overall Storm Exposure Effect Size By Month') +
  xlab('Coefficient Estimate') + ylab('Estimate + 95% Confidence Interval') +
  xlim(c(-3,4.5)) + coord_flip()

Plot_Labs = coeftable(Overall_Mod) %>% 
  rownames_to_column(var = 'Coef') %>%
  mutate(Coef = str_replace(Coef, 'Lag_0_30_Bin', 'Non_Cold')) %>%
  mutate(Coef = str_replace(Coef, ':MONTH', '')) %>%
  pull(Coef)
ggstatsplot::ggcoefstats(Non_Cold_Mod, stats.labels = F) +
   scale_y_discrete(labels = Plot_Labs) +
  ggtitle('Heterogeneity in Overall Storm Exposure Effect Size By Month') +
  xlab('Coefficient Estimate') + ylab('Estimate + 95% Confidence Interval') +
  xlim(c(-3,4.5)) + coord_flip()



#Splitting By Month



MON_Specific %>%
  ggplot() +
  geom_point(aes(x = Cold, y = Estimate), col = 'blue') +
  geom_errorbar(aes(x = Cold, ymin = LOW, ymax = HIGH)) +
  geom_hline(yintercept = 0, col = 'red') +
  facet_grid(cols = vars(MON))

MONGROUP
Keys = group_keys(TNC_DAY %>%  group_by(MONTH))
Keys


                  
  })
  map(~feols(COUNT_PER_MIL ~ Not_Placebo_Lag_0_30_Bin | state + YEAR + DAYOFWEEK, data = .)) %>%
  map_dfr(coeftable) %>%
  remove_rownames() %>%
  mutate(MON = as.factor(c(1,2,3,4,5,6,7,9,10,11,12)),
         UPPER = Estimate + 1.96 * `Std. Error`,
         LOWER = Estimate - 1.96 * `Std. Error`) %>%
  ggplot() +
  geom_point(aes(x = MON, y = Estimate), col = 'blue') +
  geom_errorbar(aes(x = MON, ymin = LOWER, ymax = UPPER))

TNC_DAY %>%
  group_split(MONTH) %>%
  map(~feols(COUNT_PER_MIL ~ Not_Placebo_Lag_0_30_Bin | state + YEAR + DAYOFWEEK, data = .)) %>%
  map_dfr(coeftable) %>%
  remove_rownames() %>%
  mutate(MON = 1:12,
         UPPER = Estimate + 1.96 * `Std. Error`,
         LOWER = Estimate - 1.96 * `Std. Error`) %>%
  ggplot() +
  geom_point(aes(x = MON, y = Estimate), col = 'blue') +
  geom_errorbar(aes(x = MON, ymin = LOWER, ymax = UPPER))


TNC_DAY %>%
  group_split(MONTH) %>%
  map(~feols(COUNT_PER_MIL ~ Lag_0_30_Bin | state + YEAR + MONTH + DAYOFWEEK, data = .)) %>%
  map_dfr(coeftable) %>%
  remove_rownames() %>%
  mutate(MON = 1:12,
         UPPER = Estimate + 1.96 * `Std. Error`,
         LOWER = Estimate - 1.96 * `Std. Error`) %>%
  ggplot() +
  geom_point(aes(x = factor(MON), y = Estimate), col = 'blue') +
  geom_errorbar(aes(x = MON, ymin = LOWER, ymax = UPPER))

summary(Overall_Mod)


feols(COUNT_PER_MIL ~ Lag_0_30_Bin | state + YEAR  +MONTH + DAYOFWEEK, data = subset(TNC_DAY,MONTH == 12))


MOD = feols(COUNT_PER_MIL ~ i(Not_Placebo_Lag_0_30_Bin,MONTH,1) + i(Placebo_Lag_0_30_Bin,MONTH,1) | state + MONTH + YEAR +DAYOFWEEK, data = TNC_DAY)
summary(MOD)
coefplot(MOD)

MOD = feols(COUNT_PER_MIL ~ i(BIN4,MONTH,1) | state + MONTH + YEAR + DAYOFWEEK, data = TNC_DAY)
coefplot(MOD)

MODVALS = coeftable(MOD)[1:11,] %>% data.frame()
plot(1:11,MODVALS$Estimate)



ggstatsplot::ggcoefstats(MOD)

#Alternative Representation
TNC_DAY %>% group_by(MONTH) %>% summarize(`Mean Don/Mil` = mean(COUNT_PER_MIL),
                                          `SD of Don/Mil` = sd(COUNT_PER_MIL), 
                                          `SD of Cold Exposure` = mean(Placebo_Lag_0_30_Bin)) %>%
  kableExtra::kable(., format = 'latex', booktabs = T,
                    caption = 'Mean and S.D. of Key Treatment and Outcome by Month')
         
         
COld_Mod#-------------------------------------------------------------Supplemental/Appendix Results-------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------#
BY_STATE = map_dfr(.x = unique(TNC_DAY$state),
                   .f = function(.x){
                     feols(COUNT_PER_MIL ~ Lag_0_30_Bin | MONTH + YEAR + DAYOFWEEK, data = subset(TNC_DAY, state == .x)) %>%
                       coeftable(.) %>%
                       mutate(state = .x)
                   }) %>%
  left_join( (TNC_DAY %>%
                group_by(state) %>% summarize(OVERALL = mean(COUNT_PER_MIL, na.rm = T))),
             by = "state")

#Difference by Vote
President_Vote = read.csv("~/Desktop/Presidents.csv") %>%
  filter(year == 2012 & !writein & candidate != "" & state != "District of Columbia") %>%
  mutate(party = ifelse(party == 'republican','REP','DEM')) %>%
  dplyr::select(state, party, candidatevotes, totalvotes) %>%
  group_by(state, party) %>%
  summarize(Party_Votes = sum(candidatevotes)) %>%
  ungroup() %>%
  pivot_wider(id_cols = 'state', names_from = 'party', values_from = 'Party_Votes') %>%
  mutate(Perc_Dem = 100*DEM/(DEM + REP),
         Dem_Majority = ifelse(DEM>REP,T,F)) %>%
  left_join( (STATES %>% rename(state = `State Name`) %>% dplyr::select(state,CODE)), by = 'state') %>%
  dplyr::select(-state) %>% rename(state = CODE)

BY_STATE = BY_STATE %>%
  left_join(President_Vote, by = 'state')

ggplot(BY_STATE) +
  geom_point(aes(x = Perc_Dem, y = Estimate))

feols(COUNT_PER_MIL ~ Lag_0_30_Bin*party - party | state + YEAR + MONTH + DAYOFWEEK, data = Test)


#LEave-one-out analysis: Doesn't Impact Results
Leave_one_out = map_dfr(.x = unique(TNC_DAY$state),
                        .f = ~ feols(COUNT_PER_MIL ~ Lag_0_30_Bin | state + MONTH + YEAR + DAYOFWEEK, data = subset(TNC_DAY, state != .x)) %>%
                          coeftable() %>%
                          mutate(state = .x))

ggplot()

#Look at these weird results for the cold storms -- let's investigate
Cold_By_Month = feols(COUNT_PER_MIL ~ Lag_0_30_Bin*MONTH - MONTH | state + YEAR  +MONTH +DAYOFWEEK, data = TNC_DAY)
coefplot(Cold_By_Month, main = 'Cold Exposure on Donations With December Fixed Effect')



#-------------------------------------Supplemental Information-----------------------------#
Pred_Mod = feols(COUNT_PER_MIL ~ BIN1 + BIN2 + BIN3 + BIN4 | state + YEAR + MONTH + DAYOFWEEK, data = TNC_DAY)
Predict_New_Dons = TNC_DAY %>%
  select(state,YEAR, MONTH, DAYOFWEEK,COUNT,COUNT_PER_MIL,POP, AMT,
         Lag_0_30,Lag_0_30_Bin, BIN1,BIN2,BIN3,BIN4) %>%
  mutate(across(.cols = c('BIN1','BIN2','BIN3'), .fns = function(x) x = 0)) %>%
  mutate(BIN4 = 1) %>%
  mutate(COUNT_HAT = predict(Pred_Mod,newdata = .) * (POP/1000000)) %>%
  mutate(AMT_PER = AMT/COUNT,
         AMT_HAT = COUNT_HAT * AMT_PER) %>%
  summarize(across(c(COUNT,COUNT_HAT,AMT,AMT_HAT),.fns = function(x) sum(x,na.rm = T)))


#--------Estimate Decay----------#
Estimate_Decay = TNC_DAY %>%
  select(COUNT_PER_MIL,state, YEAR, MONTH, DAYOFWEEK,Lag_0_1, Lag_0_7,Lag_0_15, Lag_15_30, Lag_30_60) %>%
  mutate(Lag_1_7 = Lag_0_7 - Lag_0_1,
         Lag_8_15 = Lag_0_15 - Lag_0_7) %>%
  mutate(across(c(Lag_0_1,Lag_1_7,Lag_8_15,Lag_15_30, Lag_30_60), .fns = function(x) ifelse(x > 0,1,0)))
  
feols(COUNT_PER_MIL ~ Lag_0_1 + Lag_1_7 + Lag_8_15 + Lag_15_30 + Lag_30_60 | state + YEAR + MONTH + DAYOFWEEK, data = Estimate_Decay)

#----Make Visuals
#Average Count by MNonth
TNC_DAY %>% 
  group_by(MONTH) %>%
  summarize(DON = mean(COUNT_PER_MIL))



#---------------------------------------------------------Remove Over-Exposed States---------------------------------------#
Over_Exposed = TNC_DAY %>% group_by(state) %>% summarise(EXP = mean(Lag_0_30_Bin)) %>% filter(EXP > 0.95 | EXP < 0.05) %>% pull(state)
Remove_Overs = feols(COUNT_PER_MIL ~ Lag_0_30_Bin | state + YEAR + MONTH + DAYOFWEEK, data = subset(TNC_DAY, !state %in% Over_Exposed))

feols(COUNT_PER_MIL ~ BIN1 + BIN2+ BIN3 + BIN4 | state + YEAR + MONTH + DAYOFWEEK, data = subset(TNC_DAY, !state %in% Over_Exposed))

#---------------------Change Clustering---------------------#
Test_Clusters = feols(COUNT_PER_MIL ~ Lag_0_30_Bin | state + YEAR + MONTH + DEC + DAYOFWEEK, data = TNC_DAY)
summary(Test_Clusters, se = 'white')

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
    BIN_30_Huge = ifelse(Lag_0_30 > log(400000000), 1,0),
    BIN_FIVE = cut(Lag_0_30, breaks = c(-Inf,0,quantile(Lag_0_30[which(Lag_0_30>0)],c(1/4,1/2,3/4)),Inf),
                 labels = c("Zero", "Q1", "Q2","Q3", "Q4")),
  BIN1 = ifelse(BIN_FIVE == "Q1",1,0),
  BIN2 = ifelse(BIN_FIVE  == "Q2", 1,0),
  BIN3 = ifelse(BIN_FIVE == "Q3",1,0),
  BIN4 = ifelse(BIN_FIVE == "Q4", 1,0),
  BIN_EXTREME = cut(Lag_0_30, breaks = c(-Inf,0,quantile(Lag_0_30[which(Lag_0_30>0)],c(1/4,3/4)),Inf),
                   labels = c("Zero", "LOW", "MID","HIGH"), include.lowest = T),
  BIN1_EXT = ifelse(BIN_EXTREME == "LOW", 1,0),
  BIN2_EXT = ifelse(BIN_EXTREME == "MID", 1,0),
  BIN3_EXT = ifelse(BIN_EXTREME == "HIGH", 1,0)) %>%
  ungroup() %>%
  filter(!state %in% c("GU", "PR", "DC")) %>%
  mutate(state = droplevels(state)) %>%
  group_by(state) %>%
  arrange(WEEK_LOW) %>%
  mutate(DON_LAG = dplyr::lag(COUNT_PER_MIL, n = 1)) %>%
  ungroup()


#Aitocorrelation isn't crazy
TNC_CLEAN_WEEK_FINAL %>%
  group_by(state) %>%
  arrange(WEEK_LOW) %>%
  summarise(ACF = acf(COUNT_PER_MIL, pl = F, lag = 1, 
                      type = "correlation",
                      na.action = na.omit)[[1]][2]) %>%
  arrange(desc(ACF))
  ggplot() +
  geom_histogram(aes(x = ACF))

#----------------------------------------Table of Means for Results Section 2------------------------------------#
TNC_CLEAN_WEEK_FINAL %>%
  select(COUNT_PER_MIL,Lag_0_30) %>%
  mutate(Lag_0_30 = exp(Lag_0_30)/1000000) %>%
  stargazer(., type = "latex",style = "aer", 
           title = "Summary Stats for TNC",
           summary.stat = c("n", "mean","min","max","sd"),
           covariate.labels = c("Donation Count", "30-Day Lagged Damage"),
           out = "~/Desktop/ECON Thesis/OUTPUT/DATA_Section/Table_of_Means_TNC.tex")

table(TNC_CLEAN_WEEK_FINAL$Lag_0_30_BIN>0)

STATES_OK = TNC_CLEAN_WEEK_FINAL %>%
  mutate(G_Z = ifelse(Lag_0_30_BIN > 0 , 1, 0)) %>%
  group_by(state) %>%
  summarize(PERC = sum(G_Z)/n()) %>%
  filter(PERC < 0.95) %>% pull(state)
length(unique(TNC_CLEAN_WEEK_FINAL$state))
length(STATES_OK)

#-----------------------------------------Table 1: Build Time Lags--------------------------------#
Table1_TNC = list(
  A = feols(COUNT_PER_MIL ~ Lag_0_30_BIN | state + YEAR + MONTH, data = TNC_CLEAN_WEEK_FINAL),
  B = feols(COUNT_PER_MIL ~ Lag_0_30_BIN + Lag_30_60_BIN | state + YEAR + MONTH, data = TNC_CLEAN_WEEK_FINAL),
  C = feols(COUNT_PER_MIL ~ Lag_0_30_BIN + Lag_30_60_BIN + Lag_60_90_BIN | state + YEAR + MONTH, data = TNC_CLEAN_WEEK_FINAL)
)

glimpse(TNC_CLEAN_WEEK_FINAL)
feols(COUNT_PER_MIL ~ Dam_Lag_7_BIN | state + YEAR + MONTH, data = TNC_CLEAN_WEEK_FINAL)

etable(Table1_TNC, tex = T, coefstat = "se", keepFactors = F,
       fitstat = ~r2, digits = 3, title = "Build Time Lags",
       file = "~/Desktop/ECON Thesis/OUTPUT_Publication/04.TNC1.tex")


#---------------------------------------Table 2: Different Storm Sizes-----------------------------#
Table2_TNC = list(
  A = feols(COUNT_PER_MIL ~ Lag_0_30_BIN | state + YEAR + MONTH, data = TNC_CLEAN_WEEK_FINAL),
  B = feols(COUNT_PER_MIL ~ BIN1 + BIN2 + BIN3 + BIN4 | state + YEAR + MONTH, data = TNC_CLEAN_WEEK_FINAL)
)

etable(Table2_TNC, tex = T, coefstat = "se", keepFactors = F,
       fitstat = ~r2, digits = 3, title = "TNC -- Different Levels",
       file = "~/Desktop/ECON Thesis/OUTPUT_Publication/05.TNC2.tex")

#Test Difference Between Largest Bin
Table2_TNC$B
linearHypothesis(Table2_TNC$B, "BIN4 - BIN3 = 0")


#--------------------------------------Table 3: Different Storm Types------------------------------#
Table3_TNC = list(
  A = feols(COUNT_PER_MIL ~ Lag_0_30_BIN | state + YEAR , data = TNC_CLEAN_WEEK_FINAL),
  B = feols(COUNT_PER_MIL ~ Lag_Placebo_0_30_BIN | state + YEAR , data = TNC_CLEAN_WEEK_FINAL),
  C = feols(COUNT_PER_MIL ~ Lag_Not_Placebo_0_30_BIN | state + YEAR , data = TNC_CLEAN_WEEK_FINAL),
  D = feols(COUNT_PER_MIL ~ Lag_Not_Placebo_0_30_BIN + Lag_Placebo_0_30_BIN | state + YEAR , data = TNC_CLEAN_WEEK_FINAL)
)
etable(Table3_TNC, tex = T, coefstat = "se", keepFactors = F,
       fitstat = ~r2, digits = 3, title = "TNC -- Different Storm Types",
       file = "~/Desktop/ECON Thesis/OUTPUT_Publication/06.TNC3.tex")



#----------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------Appendix Results and Other Questions---------------------#
#------------------------------------------------------------------------------------------------------------------------------

#------------------------------------------IDEA: Are Donations Autocorrelated?---------------------------#
TNC_CLEAN_WEEK_FINAL %>%
  arrange(state, WEEK_LOW) %>%
  group_by(state) %>%
  mutate(DON_LAG = dplyr::lag(COUNT_PER_MIL, n = 1)) %>%
  summarize(ACF = cor(COUNT_PER_MIL,DON_LAG, use = "complete.obs"))

#Test Autocor of Model Errors
TNC_CLEAN_WEEK_FINAL %>%
  dplyr::select(state, WEEK_LOW) %>%
  mutate(RESID = as.numeric(residuals(Table1_TNC$A))) %>%
  group_by(state) %>%
  arrange(WEEK_LOW) %>%
  summarize(ACF = acf(RESID, lag = 3, pl = F)[[1]][4]) 
  ggplot() +
  geom_histogram(aes(x = ACF), col = "purple", fill = "orange") +
  theme_minimal() +
  ggtitle("Within-State Autocorrelation of Model Errors")

#-----------------------------------------What About if I use Amounts-------------------------------------#
TNC_CLEAN_WEEK_FINAL = TNC_CLEAN_WEEK_FINAL %>%
  mutate(AMT_Per_MIL = (AMT*1000000)/POP,
         AMT_Per_Don = AMT/COUNT_PER_MIL)

feols(AMT_Per_Don ~ Lag_0_30_BIN  | state + YEAR + MONTH, data = TNC_CLEAN_WEEK_FINAL)
#More Dominated By the Extensive Margin




#----------------------------------Which States Matter Most----------------------------------#
feols(COUNT_PER_MIL ~ Lag_0_30_BIN  | state + YEAR + MONTH, data = TNC_CLEAN_WEEK_FINAL)



BY_STATE = map_dfr(.x = unique(TNC_CLEAN_WEEK_FINAL$state),
        .f = function(.x){
          feols(COUNT_PER_MIL ~ Lag_0_30_BIN | MONTH + YEAR, data = subset(TNC_CLEAN_WEEK_FINAL, state == .x)) %>%
          coeftable(.) %>%
          mutate(state = .x)
        }) %>%
  left_join( (TNC_CLEAN_WEEK_FINAL %>%
              group_by(state) %>% summarize(OVERALL = mean(COUNT_PER_MIL, na.rm = T))),
              by = "state")

BY_STATE %>%  mutate(UPPER = ifelse(state %in% c("MA", "NY", "TX", "MS", "PA", 
                                                 "VT", "SC", "OR", "MI", "NM"),
                                    Estimate + 1.9*`Std. Error`,NA),
                     LOWER = ifelse(state %in% c("MA", "NY", "TX", "MS", "PA", 
                                                 "VT", "SC", "OR", "MI", "NM"),
                                    Estimate - 1.9*`Std. Error`, NA),
                     State_Label = ifelse(state %in% c("VT","MA", "OR",
                                                       "MS", "SC", "PA", "NY", "TX",
                                                       "MI", "NM"),
                                          as.character(state),NA)) %>%
                      ggplot() +
  geom_point(aes(x = OVERALL, y = Estimate))  +
#  geom_smooth(aes(x = OVERALL, y = Estimate, se = F)) +
  geom_segment(aes(x = OVERALL, xend = OVERALL, y = LOWER, yend = UPPER), col = "red", alpha = 0.4) +
  geom_text(aes(x = OVERALL+5, y = Estimate, label = State_Label), col = "blue") +
  geom_hline(yintercept = 0, col = "red") +
  theme_minimal()

BY_STATE = BY_STATE %>% filter(state != "VT")
cor.test(BY_STATE$Estimate, BY_STATE$OVERALL)


Test = as.data.frame(coef(summary(feols(COUNT_PER_MIL ~ Lag_0_30_BIN*state  | MONTH + YEAR, data = TNC_CLEAN_WEEK_FINAL)))) %>%
  rownames_to_column(var = 'COEF')
colnames(Test) = c('COEF','EST')

AK_EST = Test[1,2]


Fixed = Test[2:50,] %>% rbind(data.frame(COEF = 'AK',EST = 0)) %>% 
  mutate(COEF = str_remove(COEF, 'state')) %>% rename('FIXED' = 'EST')
Random = Test[51:99,] %>% rbind(data.frame(COEF = 'AK',EST = Test[1,2])) %>% 
  mutate(COEF = str_remove(COEF,"Lag_0_30_BIN:state")) %>% rename('RANDOM' = 'EST')

Test = Fixed %>% left_join(Random, by = 'COEF')
Test = Test %>% #Scale to Normal
  mutate(RANDOM = ifelse(COEF == 'AK',RANDOM, RANDOM +AK_EST)) %>%
  mutate(across(.cols = c(FIXED,RANDOM), .fns = ~ (.x - mean(.x))/sd(.x),.names = 'Scaled_{col}')) %>%
  mutate()
Test
ggplot(Test) +
  geom_text(aes(x = FIXED, y = RANDOM, label = COEF))

TOT_EFFECT = TOT_EFFECT %>%
  left_join(OVERALL_AVG, by = "State") %>%
  mutate(State_Label = ifelse(State %in% c("VT", "MA", "OR", "MS", "SC", "PA", "NY"),State,NA))
#Correlation Between Overall and Effect Size is Strongly Negative 
cor(TOT_EFFECT$Estimate, TOT_EFFECT$OVERALL)

ggplot(TOT_EFFECT) +
  geom_point(aes(x = Estimate, y = OVERALL)) +
  geom_text(aes(x = Estimate, y = (OVERALL-2), label = State_Label), col = "blue") +
  theme_minimal() +
  geom_vline(xintercept = 0, col = "red") +
  xlab("Treatment Effect") + ylab("Average Weekly Donations Per Million") 
#-------------------------------Remove States With All Damages----------------------#
STATES_OK = TNC_CLEAN_WEEK_FINAL %>%
  group_by(state) %>%
  summarize(PERC_STORM = sum(Lag_0_30_BIN)/n()) %>%
  filter(PERC_STORM < 0.9) %>%
  pull(state)


#Compare
Subset_States_Regs = list(
  feols(COUNT_PER_MIL ~ Lag_0_30_BIN  | state + YEAR + MONTH, data = TNC_CLEAN_WEEK_FINAL),
  feols(COUNT_PER_MIL ~ Lag_0_30_BIN  | state + YEAR + MONTH, data = subset(TNC_CLEAN_WEEK_FINAL, state %in% STATES_OK)),
  feols(COUNT_PER_MIL ~  BIN1 + BIN2 + BIN3 + BIN4 | state + YEAR + MONTH, data = subset(TNC_CLEAN_WEEK_FINAL, state %in% STATES_OK))
#  feols(COUNT_PER_MIL ~ Lag_Not_Placebo_0_30_BIN  | state + YEAR + MONTH, data = subset(TNC_CLEAN_WEEK_FINAL, state %in% STATES_OK)),
#  feols(COUNT_PER_MIL ~ Lag_Placebo_0_30_BIN  | state + YEAR + MONTH, data = subset(TNC_CLEAN_WEEK_FINAL, state %in% STATES_OK))
)

etable(Subset_States_Regs, tex = T, coefstat = "se", keepFactors = F, fitstat = ~r2,
       digits = 3, file = "~/Desktop/ECON Thesis/OUTPUT_Publication/TNC_SubsetStates.tex")


glimpse(TNC_CLEAN_WEEK_FINAL)

feols(COUNT_PER_MIL ~ Dam_Lag_7_BIN | state + YEAR + MONTH, data = TNC_CLEAN_WEEK_FINAL)

#------------------------------------------Repeating Results at Day Level-------------------------#









#Amount Regs



#----------------Some Sort of Predictions-----------#
glimpse(TNC_CLEAN_WEEK_FINAL)
MOD = feols(COUNT_PER_MIL ~ Lag_0_30_BIN | state + YEAR + MONTH, data = TNC_CLEAN_WEEK_FINAL)
predict(MOD)

glimpse(TNC_CLEAN_WEEK_FINAL %>%
  mutate(PRED_VALS = predict(MOD)) )

PRED_DON = TNC_CLEAN_WEEK_FINAL %>%
  dplyr::select(state, YEAR, MONTH,COUNT_PER_MIL,AMT,POP, Lag_0_30_BIN,WEEK_LOW) %>%
  mutate(Lag_0_30_BIN = 1) %>%
  mutate(PRED_VALS_Static = predict(MOD)) %>%
  mutate(PRED_ALLSTORM = predict(MOD, newdata = .))
head(PRED_DON)


#PREDICT
PRED_DON %>%
  group_by(YEAR,MONTH) %>%
  summarize(MEAN_NORM = sum(PRED_VALS_Static * (POP/1000000)),
            MEAN_ALLPOS  = sum(PRED_ALLSTORM * (POP/1000000),na.rm = T))
