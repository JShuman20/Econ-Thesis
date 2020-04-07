library(rvest)
library(tidyverse)
library(purrr)
library(xml2)
library(RCurl)
library(purrr)
library(writexl)

#---------------------------------------------------Scraping Senate Vote Data---------------------------------#
#List of url paths for senate
urls = c()
for(i in as.character(101:115)){
  for(j in as.character(1:2)){
    urls = c(urls, str_c("https://www.senate.gov/legislative/LIS/roll_call_lists/vote_menu_",
                         i, "_", j,".xml"))
  }
}
urls = c(urls, "https://www.senate.gov/legislative/LIS/roll_call_lists/vote_menu_116_1.xml")

#Scraper Function
SENATE_EXTRACT = function(URL){
  PAGE = read_xml(URL)
  #First, Extracting Year Variable
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
    mutate(Year = as.numeric(YEAR))
  return(DAT)
}

#Scraping and Merging
ALL_SENATE = map_dfr(.x = urls,
              .f = ~SENATE_EXTRACT(.x)) %>%
  mutate(ROLL_CALL = as.numeric(str_sub(ROLL,3,6)))

#Merging to LCV Data
ALL_LCV_SENATE_MERGE = ALL_LCV_SENATE %>%
  left_join(ALL_SENATE, by = c("ROLL_CALL", "Year")) %>%
  filter(!is.na(ROLL))
write_xlsx(ALL_LCV_SENATE_MERGE, 
           "~/Desktop/ECON Thesis/Data/CLEAN/LCV/LCV_SENATE_MERGED.xlsx")


#--------------------------------------------Scraping Data For All House Votes------------------------------#
urls_house = c()
for(i in as.character(1990:2019)){
  for(j in as.character(0:9)){
    urls_house = c(urls_house, str_c("http://clerk.house.gov/evs/", i , "/ROLL_",j,"00.asp"))
  }
}

#Function to Extract Data
HOUSE_EXTRACT = function(URL){
  if(url.exists(URL)){
    PAGE = read_html(URL)
    RECS = html_nodes(PAGE,"table")[[1]]
    VALS = str_split(RECS, "<tr>")
    VALS = VALS[[1]][-c(1:2)]
    DAT  =  map_dfr(.x = VALS,
                    .f = ~ data.frame(
                      YEAR = str_extract(.,"year=[[:digit:]]{1,4}"),
                      ROLL = str_extract(., "rollnumber=[[:digit:]]{1,3}"),
                      DATE = str_extract(., "size=\"-1\">[[:digit:]]{1,2}-[A-Z]{1}[a-z]{2}")
                    ))
    return(DAT)
  }}
  
#Executing Scrape and Cleanin g
ALL_HOUSE = map_dfr(.x = urls_house,
                    .f = ~ HOUSE_EXTRACT(.x))
ALL_HOUSE_2 = ALL_HOUSE %>%
  mutate(Year = as.numeric(str_sub(YEAR, 6,10)),
         ROLL_CALL = as.numeric(str_replace(ROLL,"rollnumber=","")),
         DATE = str_sub(DATE, 11),
         DAY  = as.numeric(str_extract(DATE, "[[:digit:]]{1,2}")),
         MON  = str_extract(DATE, "[[A-Z,a-z]]{3}")) %>%
  select(-YEAR)


#Merging LCV With Dates
ALL_LCV_HOUSE_MERGE = ALL_LCV_HOUSE %>%
  left_join(ALL_HOUSE_2, by = c("ROLL_CALL", "Year")) %>%
  filter(!is.na(ROLL))
write_xlsx(ALL_LCV_HOUSE_MERGE, 
           "~/Desktop/ECON Thesis/Data/CLEAN/LCV/LCV_HOUSE_MERGED.xlsx")







