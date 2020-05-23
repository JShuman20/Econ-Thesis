#---------------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------------#
#The purpose of this script is to import and clean yearly LCV voting files downloaded from the LCV website. This includes scraping roll calls from 
#the House and Senate website -- AN INTERNET CONNECT IS REQUIRED TO RUN THIS SCRIPT. ---------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------------#

#-------------------------Loading Relevant Libraries-------------------#
#For processing data
library(tidyverse)
library(purrr)
library(writexl)
library(xtable)
library(readxl)
#For scraping
library(RCurl)
library(xml2)
library(rvest)

#-----------------------Function to loop over individual LCV files in raw format and do preliminary cleaning------------#
#Function is defined for both the House and Senate
Cleaner = function(YEAR,BRANCH){
  #Define the Import Path
  PATH = str_c("~/Google Drive/DATA/ECON/RAW/LCV_DAT/",BRANCH,"/", YEAR, "-",tolower(BRANCH),"-scorecard-grid-export.csv")
  DAT = read.csv(PATH, skip =6) %>%
    gather(Policy, Vote, -c(1:5)) %>%                     #Convert Grid to Long-Format
    mutate(Year = as.numeric(YEAR)) %>%                               #Add a Year Variable
    rename("Current.Score" = 4) %>%                       #Consistent Variable Naming for row-binding
    mutate(Current.Score = as.factor(Current.Score),      
           Branch = BRANCH,                                      #Add a Flag for House or Senate
           ROLL_CALL = str_extract(Policy, "\\.[0-9]{1,3}\\."),  #Extract Roll Call #
           ROLL_CALL = as.numeric(str_remove_all(ROLL_CALL, "\\D")),         
           Policy = str_remove(Policy, "\\.{2}[0-9]{1,3}\\."))   #Cleaning String Variables
  return(DAT)
}
#Extracting list of years -- argument to the function
YEARS  = list.files("~/Google Drive/DATA/ECON/RAW/LCV_DAT/SENATE/") %>%
  str_sub(start = 1,end = 4)

#Reading, Merging, and Cleaning Raw Senate Data Files
ALL_LCV_SENATE = map_dfr(.x = YEARS,
                         .f = ~ Cleaner(.x, "SENATE")) 
#Reading, Merging, and Cleaning Raw House Data Files
ALL_LCV_HOUSE = map_dfr(.x = YEARS,
                        .f = ~ Cleaner(.x, "HOUSE")) 

#--------------------------------------------Scraping Senate Vote Data---------------------------------#
#List of url paths for senate
urls = c()
for(i in as.character(101:115)){
  for(j in as.character(1:2)){
    urls = c(urls, str_c("https://www.senate.gov/legislative/LIS/roll_call_lists/vote_menu_",
                         i, "_", j,".xml"))
  }
}
urls = c(urls,"https://www.senate.gov/legislative/LIS/roll_call_lists/vote_menu_116_1.xml")

#Function to Scrape Data Tables From Senate.gov
SENATE_EXTRACT = function(URL){
  PAGE = read_xml(URL)
  #First, Extracting Year Variable
  CONG = str_extract(URL, "[[:digit;]]{3}")
  YEAR = xml_find_all(PAGE,"congress_year") %>%
    as.character() %>%
    str_extract("[[:digit:]]{4}")
  #Now Extracting Data From Table
  recs = xml_find_all(PAGE, "//vote")
  vals = trimws(xml_text(recs))
  DAT  = map_dfr(.x = vals,
                 .f  = ~ data.frame(
                   ROLL    = str_sub(.x, 1,5),
                   DAY   = str_sub(.x, 6,7),
                   MON   = str_sub(.x, 9,11),
                   OTHER = str_sub(.x,12,100))) %>%
    mutate(Year = as.numeric(YEAR),
           CONG = CONG)
  return(DAT)
}

#Scraping All Senate Roll Call Records
ALL_SENATE = map_dfr(.x = urls,
                     .f = ~SENATE_EXTRACT(.x)) %>%
  mutate(ROLL_CALL = as.numeric(str_sub(ROLL,3,6)))

#Merging Scraped Senate Roll Call Records With LCV Data
ALL_LCV_SENATE_MERGE = ALL_LCV_SENATE %>%
  left_join(ALL_SENATE, by = c("ROLL_CALL", "Year")) %>%
  filter(!is.na(ROLL)) %>% #Remove irrelevant roll calls (the ones that don't merge)
  filter(str_detect(Policy,str_c("..2x." ,
                                 "Oldham.Confirmation..Fifth.Circuit.Court.of.Appeals",
                                 "McNamee.Confirmation..FERC.",sep = "|"),negate =TRUE)) %>% #remove mistypes and duplicates
  mutate(Current.Score = as.numeric(Current.Score),
         Party = as.factor(Party),
         MON_NUM = case_when(
           MON == "Jan" ~ 1,
           MON == "Feb" ~ 2,
           MON == "Mar" ~ 3,
           MON == "Apr" ~ 4,
           MON == "May" ~ 5,
           MON == "Jun" ~ 6,
           MON == "Jul" ~ 7,
           MON == "Aug" ~ 8,
           MON == "Sep" ~ 9,
           MON == "Oct" ~ 10,
           MON == "Nov" ~ 11,
           MON == "Dec" ~ 12),
         YEAR_MON = str_c(as.character(Year), as.character(MON_NUM), sep = "_"),  #Creating Various Time Variables
         DATE = as.Date(str_c(str_sub(YEAR_MON,1,4),MON_NUM,DAY,sep = "-")),
         POS_VOTE = case_when(Vote == "+" ~ 1,
                              Vote == "-" ~ 0)) #No-Votes are now stored as "NA"

#Writing Intermediate, Cleaned Senate Dataset
write_csv(ALL_LCV_SENATE_MERGE, "~/Google Drive/DATA/ECON/CLEAN/LCV/SENATE.csv")
#---------------------------------------------Scraping Dates of All House Votes-------------------------#
#List of url paths for house
urls_house = c()
for(i in as.character(1990:2019)){
  for(j in as.character(0:9)){
    urls_house = c(urls_house, str_c("http://clerk.house.gov/evs/", i , "/ROLL_",j,"00.asp"))
  }
}

#Function to Scrape Data Tables From Senate.gov
HOUSE_EXTRACT = function(URL){
  if(url.exists(URL)){
    PAGE = read_html(URL)
    CONG = str_extract(as.character(PAGE)[1],">U.S. House of Representatives Roll Call Votes<br>\r\n [[:digit:]]{3}")
    CONG = str_sub(CONG,start = -3)
    RECS = html_nodes(PAGE,"table")[[1]]
    VALS = str_split(RECS, "<tr>")
    VALS = VALS[[1]][-c(1:2)]
    DAT  =  map_dfr(.x = VALS,
                    .f = ~ data.frame(
                      YEAR = str_extract(.,"year=[[:digit:]]{1,4}"),
                      ROLL = str_extract(., "rollnumber=[[:digit:]]{1,3}"),
                      DATE = str_extract(., "size=\"-1\">[[:digit:]]{1,2}-[A-Z]{1}[a-z]{2}")
                    )) %>%
      mutate(CONG = CONG)
    return(DAT)
  }}

#Executing Scrape and Cleaning
ALL_HOUSE = map_dfr(.x = urls_house,
                    .f = ~ HOUSE_EXTRACT(.x)) %>%
  mutate(Year = as.numeric(str_sub(YEAR, 6,10)),
         ROLL_CALL = as.numeric(str_replace(ROLL,"rollnumber=","")),
         DATE = str_sub(DATE, 11),
         DAY  = as.numeric(str_extract(DATE, "[[:digit:]]{1,2}")),
         MON  = str_extract(DATE, "[[A-Z,a-z]]{3}")) %>%
  select(-YEAR)

STATES = read_xlsx("~/Google Drive/DATA/ECON/STATES_REGIONS.xlsx") %>%
  rename(CODE = `State Code`) 

#Merging LCV With Dates
ALL_LCV_HOUSE_MERGE = ALL_LCV_HOUSE %>%
  left_join(ALL_HOUSE, by = c("ROLL_CALL", "Year")) %>%
  filter(!is.na(ROLL)) %>%
  filter(str_detect(Policy,str_c("..2x." ,
                                 "Undermining.the.Land.and.Water.Conservation.Fund",
                                 "Undermining.Bedrock.Environmental.Laws", sep = "|"),negate =TRUE)) %>% #Removing mistypes and duplicates
  mutate(Current.Score = as.numeric(Current.Score),
         Party = as.factor(Party),
         MON_NUM = case_when(
           MON == "Jan" ~ 1,
           MON == "Feb" ~ 2,
           MON == "Mar" ~ 3,
           MON == "Apr" ~ 4,
           MON == "May" ~ 5,
           MON == "Jun" ~ 6,
           MON == "Jul" ~ 7,
           MON == "Aug" ~ 8,
           MON == "Sep" ~ 9,
           MON == "Oct" ~ 10,
           MON == "Nov" ~ 11,
           MON == "Dec" ~ 12),
         YEAR_MON = str_c(as.character(Year), as.character(MON_NUM), sep = "_"),
         DATE = as.Date(str_c(str_sub(YEAR_MON,1,4),MON_NUM,DAY,sep = "-")), #Creating Various Time Variables
         POS_VOTE = case_when(Vote == "+" ~ 1,
                              Vote == "-" ~ 0))%>% #No-Votes Stored as "NA"
  mutate(CODE = str_sub(District,1,2)) %>%
  left_join(STATES, by = "CODE") %>%
  rename(State = `State Name`) %>%
  mutate(Member.of.Congress = case_when(
    Member.of.Congress == "Rogers, Mike"  & CODE == "AL" ~ "Rogers, Mike_AL",
    Member.of.Congress == "Brown, George" & CODE == "CO" ~ "Brown, George_CO",
    TRUE ~ Member.of.Congress)) #Members with the same name

write_csv(ALL_LCV_HOUSE_MERGE, "~/Google Drive/DATA/ECON/CLEAN/LCV/HOUSE.csv")

