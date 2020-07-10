library(tidyverse)
library(purrr)
library(furrr)
library(parallel)
library(plm)
library(sf)
library(ggmap)
library(sp)
library(plm)
library(readxl)

#

ALL_LCV_SENATE_MERGE = read.csv("~/Google Drive/DATA/ECON/CLEAN/LCV/SENATE.csv")
ALL_LCV_SENATE_MERGE = mutate(ALL_LCV_SENATE_MERGE, DATE = as.Date(DATE))





ALL_LCV_HOUSE_MERGE = read.csv("~/Google Drive/DATA/ECON/CLEAN/LCV/HOUSE.csv")
ALL_LCV_HOUSE_MERGE = mutate(ALL_LCV_HOUSE_MERGE, DATE = as.Date(DATE))
DAT_CLEAN = read.csv("~/Google Drive/DATA/ECON/CLEAN/NOAA/02_NOAA_CLEANED.csv")


Compute_Lag_V2 = function(DSET, DAT, LAG_NEAR = 0, LAG_FAR, PLACE, TIME = "BEGIN"){
  DAT_FAR  = DAT - LAG_FAR
  DAT_NEAR = DAT - LAG_NEAR
  if(TIME == "BEGIN"){
    RES = sum(DSET$DAMAGE_PROPERTY[which(DSET$BEGIN_DATE >= DAT_FAR  & 
                                         DSET$BEGIN_DATE <= DAT_NEAR & 
                                         DSET$STATE_CODE == PLACE)], na.rm = TRUE)
  }
  else if (TIME == "END"){
    RES = sum(DSET$DAMAGE_PROPERTY[which(DSET$END_DATE >= DAT_FAR    & 
                                         DSET$END_DATE <= DAT_NEAR   & 
                                         DSET$STATE_CODE == PLACE)], na.rm = TRUE)
  }
  else if (TIME == "MID"){
    RES = sum(DSET$DAMAGE_PROPERTY[which(DSET$MID_DATE >= DAT_FAR      & 
                                         DSET$MID_DATE <= DAT_NEAR     & 
                                         DSET$STATE_CODE == PLACE)],   na.rm = TRUE)
  }
  return(RES)
}








#-------------------Plan for Granular Merge-------------------#
#01. There are 2 types of geographies: counties and weather zones
#02. 2/3 of events have beginning and ending coordinates -- we will construct a midpoint and assign that as the coordinate
#03. The remaining ones are as follows
NOAA = read.csv("~/Google Drive/DATA/ECON/CLEAN/NOAA/02_NOAA_CLEANED.csv")
NOAA_RELEVANT = NOAA %>% filter(DAMAGE_PROPERTY > 0)
table(is.na(NOAA_RELEVANT$BEGIN_LON), NOAA_RELEVANT$CZ_TYPE) #-we have 2/3 of cases that have coordinates
#04. Now we are looking only at the Type Z Coordinates
NOAA_TypeZ = NOAA_RELEVANT %>% filter(CZ_TYPE == "Z", is.na(BEGIN_LAT))  %>% 
  dplyr::select(STATE, CZ_TYPE, CZ_FIPS, CZ_NAME, STATE_CODE, EVENT_ID, 
                DAMAGE_PROPERTY, BEGIN_DATE, END_DATE, PLACEBO_EVENT)
#Constructing Variables to try merge
NOAA_TypeZ = NOAA_TypeZ %>%
  mutate(STATE_ZONE = case_when(
    str_length(as.character(CZ_FIPS))==1 ~   str_c(STATE_CODE, "00",CZ_FIPS),
    str_length(as.character(CZ_FIPS))==2 ~   str_c(STATE_CODE, "0",CZ_FIPS),
    TRUE ~ str_c(STATE_CODE,CZ_FIPS))) 
nrow(NOAA_TypeZ) #64105 rows

#Need State List
STATES = read_xlsx("~/Google Drive/DATA/ECON/STATES.xlsx") %>% pull(STATE)
######04A: Merging With Shapefiles
Shapes = st_read("~/Desktop/z_03mr20/z_03mr20.shp")
Shapes = Shapes %>% filter(STATE %in% STATES)
Shapes = Shapes %>%
  dplyr::select(STATE_ZONE, LON,LAT, NAME, SHORTNAME, STATE)  %>%
  group_by(STATE_ZONE) %>%
  sample_n(1) #For Duplicates, randomly choosing one of the polygons
#Identifying the Ones That Don't Merge
NOAA_Z_Test = NOAA_TypeZ %>% 
  anti_join(Shapes, by = "STATE_ZONE")   #The Ones that Don't Match
nrow(NOAA_Z_Test)
#Completing the ones that do merge
NOAA_TypeZ = NOAA_TypeZ %>%
  left_join(Shapes, by = "STATE_ZONE") %>%
  filter(!is.na(LON))
nrow(NOAA_TypeZ)
#####Now Figuring out the ones that don't merge -- from the anti-merge
Shapes = Shapes %>%
  mutate(NAME_UPPER = toupper(NAME))
#--------------------Backup Plan -- Doing this by name--------------------------#
#Initialize Vectors For Responses
LON_VEC = numeric()
LAT_VEC = numeric()
#Converting Into Character Vectors
NOAA_Z_Test = NOAA_Z_Test %>%
  mutate(STATE_CODE = as.character(STATE_CODE), CZ_NAME = as.character(CZ_NAME))
#Randomly Select From Matching Options -- Appears to Work Most of the Time
for(i in 1:nrow(NOAA_Z_Test)){
  Restricted = Shapes %>% filter(STATE == NOAA_Z_Test$STATE_CODE[i] & str_detect(NAME_UPPER, NOAA_Z_Test$CZ_NAME[i]))
  if(nrow(Restricted)==0){
    LON_VEC[i]=NA; LAT_VEC[i] = NA
  }
  else{
    Restricted = sample_n(Restricted, 1)
    LON_VEC[i] = Restricted$LON[1]; LAT_VEC[i] = Restricted$LAT[1]
  }
}
head(NOAA_TypeZ)
NOAA_Z_Test = NOAA_Z_Test %>%
  mutate(LON = LON_VEC, LAT = LAT_VEC)
ALL_Zs = bind_rows(NOAA_TypeZ, NOAA_Z_Test) %>%
  select(-c(geometry, STATE, SHORTNAME))

nrow(ALL_Zs)
ALL_Zs_Final = ALL_Zs %>%
  dplyr::select(-c("NAME", "STATE.y")) %>%
  rename(STATE = STATE.x) 

Zs_Failed = filter(ALL_Zs_Final, is.na(LAT)) #---NEED TO HANDLE THIS LATER
ALL_Zs_Final = ALL_Zs_Final %>% 
  filter(!is.na(LAT)) %>%
  dplyr::select(STATE, STATE_CODE, EVENT_ID, DAMAGE_PROPERTY, BEGIN_DATE, END_DATE, PLACEBO_EVENT, LAT, LON)
head(ALL_Zs_Final)

#------------------Now Trying to Work Out Counties----------------#
NOAA_RELEVANT_TypeC = NOAA_RELEVANT %>%
  mutate(END_DATE = as.Date(END_DATE)) %>%
  filter(CZ_TYPE=="C", is.na(BEGIN_LAT)) %>%
  dplyr::select(STATE,STATE_FIPS, CZ_TYPE, CZ_FIPS, CZ_NAME, STATE_CODE, EVENT_ID, 
                DAMAGE_PROPERTY, BEGIN_DATE, END_DATE, PLACEBO_EVENT) %>%
  mutate_at(c("STATE_FIPS", "CZ_FIPS", "CZ_NAME"), as.character) %>%
  mutate(STATE_FIPS = ifelse(str_length(STATE_FIPS)==2, STATE_FIPS, str_c("0", STATE_FIPS)),
         CZ_FIPS = case_when(str_length(CZ_FIPS)==1 ~ str_c("00",CZ_FIPS),
                             str_length(CZ_FIPS)==2 ~ str_c("0", CZ_FIPS),
                             TRUE ~ CZ_FIPS),
         FIPS = str_c(STATE_FIPS,CZ_FIPS)) 
#Pre-2001 Events
NOAA_RELEVANT_Early_TypeC = filter(NOAA_RELEVANT_TypeC, END_DATE <= "2000-12-31") %>%
  #Doing Some Regex Catches For Typos
  mutate(FIPS = case_when(
    str_length(CZ_NAME)==5 & str_detect(CZ_NAME, pattern = "[[:upper:]]{2}\\d{3}") ~ str_c(STATE_CODE, str_sub(CZ_NAME,3,5)),
    TRUE ~ FIPS))
#Merging with pre-2000 shapefiles
EARLY_Years_Shapes = st_read("~/Desktop/US_AtlasHCB_Counties_Gen001/US_HistCounties_Gen001_Shapefile/US_HistCounties_Gen001.shp") %>%
   filter(END_DATE > "1980-01-01") %>%
   dplyr::select(c(NAME, STATE_TERR, FIPS, START_DATE, END_DATE, FULL_NAME, geometry)) %>%
   filter(STATE_TERR != "District of Columbia") %>%
   mutate(FIPS = as.character(FIPS))
######Doing The Merge Piecewise to Increase Speed
library(furrr)
options("future.fork.enable" = TRUE)
future::plan(multicore(workers = 4)) 
Early_Years_TypeC_Merged = future_map_dfr(.x = 1:26, .f = function(.x){
      INDEX = ifelse(.x==1,1,1000*(.x-1)+1)
      IND_HIGH = ifelse(.x==26, nrow(NOAA_RELEVANT_Early_TypeC), INDEX + 999)
      DAT = NOAA_RELEVANT_Early_TypeC[INDEX:IND_HIGH,] %>%
          fuzzyjoin::fuzzy_left_join(EARLY_Years_Shapes,  by = c("FIPS" = "FIPS",
                                                   "END_DATE" = "START_DATE",
                                                   "END_DATE" = "END_DATE"),
                                     match_fun = list(`==`, `>`, `<=`)) 
      return(DAT)
    })

Early_Years_TypeC_Merged = st_sf(Early_Years_TypeC_Merged, sf_column_name = "geometry") %>%
  mutate(NO_MERGE = sf::st_is_empty(geometry))


#-------------------------Now Handling Post-2000 Merges----------------#
Later_Year_Shapefiles = list.files("~/Desktop/Later_Counties/")
First = Later_Year_Shapefiles[1] ; DIR = str_c("~/Desktop/Later_Counties/", First)
FILES = list.files(DIR) ; END_PATH = FILES[grep(".shp$", FILES)]
FILES_LATE =  st_read(str_c(DIR, END_PATH, sep = "/")) %>%
              st_sf(., sf_column_name = "geometry") %>%
              sf::st_zm(.) %>%
              mutate(YEAR = str_extract(END_PATH, "[[:digit:]]{4}")) %>%
              dplyr::select(STATEFP, COUNTYFP, YEAR, geometry)
#Binding
Later_Year_Shapefiles = Later_Year_Shapefiles[-1]
for(i in Later_Year_Shapefiles){
  DIR = str_c("~/Desktop/Later_Counties/", i)
  FILES = list.files(DIR)
  END_PATH = FILES[grep(".shp$", FILES)]
  FILE = st_read(str_c(DIR, END_PATH, sep = "/")) %>%
    st_sf(., sf_column_name = "geometry") %>%
    sf::st_zm(.) 
  YEAR = str_extract(END_PATH, "[[:digit:]]{4}")
  FILE = FILE %>%
    mutate(YEAR = YEAR)
  if(YEAR == "2010"){
    FILE = FILE %>%
      rename(STATEFP = STATE, COUNTYFP = COUNTY)
  }
  FILE = FILE %>%
    dplyr::select(STATEFP, COUNTYFP, YEAR, geometry)
  FILES_LATE = rbind(FILES_LATE,FILE)
}
FILES_LATE = FILES_LATE %>%
  mutate_at(c("STATEFP", "COUNTYFP"), as.character) %>%
  rename(YEAR_MERGE = YEAR) 

#Isolating Relevant Weather Events
LateYear_TypeC_Merged = NOAA_RELEVANT_TypeC %>%
  filter(END_DATE >"2000-12-31") %>%
  mutate(YEAR = lubridate::year(END_DATE),
         #Now Assigning to next available year
         YEAR_MERGE = case_when(
           YEAR < 2010 ~ 2010,
           YEAR %in% 2010:2012 ~ 2013,
           YEAR %in% 2013:2017 ~ YEAR + 1,
           YEAR >= 2018 ~ 2018
         )) %>%
  mutate(YEAR_MERGE = as.character(YEAR_MERGE)) %>%
  rename(STATEFP = STATE_FIPS, COUNTYFP = CZ_FIPS) %>%
  left_join(FILES_LATE, by = c("STATEFP", "COUNTYFP", "YEAR_MERGE")) %>%
  mutate(NO_MERGE = sf::st_is_empty(geometry)) 




#Combining Into Full DataSet
Early_Years_TypeC_Merged_1 = Early_Years_TypeC_Merged %>%
  dplyr::select(-c(FIPS.y, END_DATE.y, FULL_NAME, NAME, STATE_TERR, START_DATE)) %>%
  rename(FIPS = FIPS.x, END_DATE = END_DATE.x,
         COUNTYFP = CZ_FIPS, STATEFP = STATE_FIPS) %>%
  st_sf(., crs = 4269)

head(LateYear_TypeC_Merged)
LateYear_TypeC_Merged_1 = LateYear_TypeC_Merged %>%
  dplyr::select(-c(YEAR, YEAR_MERGE)) %>%
  st_sf(., sf_column_name = "geometry")

ALL_TypeC_Merged = rbind(Early_Years_TypeC_Merged_1,LateYear_TypeC_Merged_1)

head(ALL_TypeC_Merged)






NOAA_TypeC = NOAA_RELEVANT %>% filter(CZ_TYPE == "C", is.na(BEGIN_LAT))  %>% 
  dplyr::select(STATE, CZ_TYPE, CZ_FIPS, CZ_NAME, STATE_CODE, EVENT_ID, DAMAGE_PROPERTY, BEGIN_DATE, END_DATE, PLACEBO_EVENT)
nrow(NOAA_TypeC)
NOAA_TypeC = NOAA_TypeC %>%
  mutate(COUNTYFP = case_when(str_length(as.character(CZ_FIPS))==1 ~   str_c("00",CZ_FIPS),
                              str_length(as.character(CZ_FIPS))==2 ~   str_c("0",CZ_FIPS),
                              TRUE ~ str_c(CZ_FIPS)))
nrow(NOAA_TypeC)
#Now Adding on State FIPS
STATE_DAT = read_xlsx("~/Google Drive/DATA/ECON/STATES.xlsx")
STATE_DATA = read_xlsx("~/Google Drive/DATA/ECON/STATES_REGIONS.xlsx") %>%
  rename(STATE = `State Code`)
STATE_DATA = STATE_DATA %>%
  left_join(STATE_DAT, by = "STATE") %>%
  dplyr::select(STATE, NUMBER) %>%
  rename(STATE_CODE = STATE, STATEFP = NUMBER)
#Now Merging This onto the NOAA A=Data
NOAA_TypeC = NOAA_TypeC %>%
  left_join(STATE_DATA, by = "STATE_CODE")
#Now Extract the Ones That Don't Match
Counties = st_read("~/Desktop/tl_2017_us_county/tl_2017_us_county.shp")
NOAA_C_Test = NOAA_TypeC %>%
  mutate(STATEFP = as.character(STATEFP),
         STATEFP = ifelse(str_length(STATEFP)==1, str_c("0", STATEFP), STATEFP)) %>%
  anti_join(Counties,  by = c("STATEFP", "COUNTYFP"))
nrow(NOAA_C_Test)
table(is.na(NOAA_TypeC$COUNTYFP))
#Now Merging County Shapes onto NOAA Type C

NOAA_TypeC_Good = NOAA_TypeC %>%
  mutate(STATEFP = as.character(STATEFP),
         STATEFP = ifelse(str_length(STATEFP)==1, str_c("0", STATEFP), STATEFP)) %>%
  left_join(Counties, by = c("STATEFP", "COUNTYFP")) %>%
  filter(!is.na(INTPTLAT))
nrow(NOAA_TypeC_Good)
#We Now Have Essentially Everything, which is good!
#Now Handling the Few that Don't Match
#THERE ARE 115 EVENTS THAT DON'T MATCH -- THEY APPEAR TO BE ASSIGNED TO MULTIPLE AREAS/WHOLE STATES, SO I WILL DO THE SAME
head(NOAA_TypeC_Good)

#-------------------------------TRYING TO PASTE EVERYTHING BACK TOGETHER-------------------------------#
#Cleaning Up Type Z Dataset to Facilitate Merge

#Separate the Ones That Do and Don Not Work

#Selecting Centroid From Each Geometry for Successful Type C's
NOAA_TypeC_Good = NOAA_TypeC_Good %>%
  mutate(CENTROID = st_centroid(geometry)) 
COORDS = st_coordinates(NOAA_TypeC_Good$CENTROID) %>% data.frame() %>% rename(LAT = X, LON = Y)
NOAA_TypeC_Good = NOAA_TypeC_Good %>%
  dplyr::select(c(STATE, STATE_CODE, EVENT_ID, DAMAGE_PROPERTY, BEGIN_DATE, END_DATE, PLACEBO_EVENT)) %>%
  bind_cols(COORDS)

#Merging Successful Type C and Type Z


#Now Merging This With The ones That Already Have Coordinates
HAD_COORDS = filter(NOAA_RELEVANT, !is.na(BEGIN_LAT)) %>%
  dplyr::select(STATE, STATE_CODE, EVENT_ID, DAMAGE_PROPERTY, BEGIN_DATE, END_DATE, PLACEBO_EVENT, BEGIN_LAT, BEGIN_LON) %>%
  rename(LAT = BEGIN_LAT, LON = BEGIN_LON) %>%
  mutate(BEGIN_DATE = as.Date(BEGIN_DATE), END_DATE = as.Date(END_DATE))

ALL_Zs_Final = ALL_Zs_Final %>% mutate(END_DATE = as.Date(END_DATE), BEGIN_DATE = as.Date(BEGIN_DATE))
ALL_TypeC_Merged = ALL_TypeC_Merged %>%
  mutate(CENTROID = st_centroid(geometry))
COORDS = st_coordinates(ALL_TypeC_Merged$CENTROID) %>% data.frame() %>% rename(LAT = X, LON = Y)
ALL_TypeC_Merged = ALL_TypeC_Merged %>%
  dplyr::select(c(STATE, STATE_CODE, EVENT_ID, DAMAGE_PROPERTY, BEGIN_DATE, END_DATE, PLACEBO_EVENT)) %>%
  bind_cols(COORDS) 
ALL_TypeC_Merged = ALL_TypeC_Merged %>%
  dplyr::select(-geometry) %>% as.data.frame()
ALL_TypeC_Merged = ALL_TypeC_Merged %>% mutate(BEGIN_DATE = as.Date(BEGIN_DATE))
  

glimpse(ALL_TypeC_Merged)

#Pushing together all the successfuls
ALL_Successful = bind_rows(ALL_TypeC_Merged, ALL_Zs_Final, HAD_COORDS)
ALL_Successful = dplyr::select(ALL_Successful, -geometry) 
ALL_Successful = distinct(ALL_Successful)
glimpse(ALL_Successful)

#WE'VE ACCOUNTED FOR ALL CASES
nrow(ALL_Successful) + nrow(Zs_Failed) - nrow(NOAA_RELEVANT)

as.vector(colnames(ALL_Successful))

#Adding Failed Type Z Back on
Zs_Failed = Zs_Failed %>%
  dplyr::select(STATE, STATE_CODE, EVENT_ID, DAMAGE_PROPERTY, BEGIN_DATE, END_DATE, PLACEBO_EVENT, LAT, LON) %>%
  mutate_at(c("END_DATE", "BEGIN_DATE"), as.Date)

ALL = bind_rows(ALL_Successful, Zs_Failed)
ALL = mutate(ALL, NO_MERGE = is.na(LAT))
#-------------------------------Point-in-Polygon Merge With Congressional District Files---------------#
ALL = ALL %>%
  mutate(BEGIN_DATE = as.Date(BEGIN_DATE), END_DATE = as.Date(END_DATE))
#Create Structure That Stores Date Ranges for Congresses
DATES = read_xlsx("~/Desktop/Congress_Dates.xlsx") %>%
  dplyr::select(Congress, BEGIN_DATE,END_DATE)  %>%
  mutate(BEGIN_DATE = as.Date(BEGIN_DATE), END_DATE = as.Date(END_DATE))

ALL_Successful = ALL %>% filter(!NO_MERGE)
ALL_Unsuccessful = ALL %>% filter(NO_MERGE)

get_congress_map <- function(cong) {
  tmp_file <- tempfile()
  tmp_dir  <- tempdir()
  zp <- sprintf("http://cdmaps.polisci.ucla.edu/shp/districts%03i.zip",cong)
  download.file(zp, tmp_file)
  unzip(zipfile = tmp_file, exdir = tmp_dir)
  fpath <- paste(tmp_dir, sprintf("districtShapes/districts%03i.shp",cong), sep = "/")
  st_read(fpath)
}
#Just for 114 --- only specify the begin date for the storm
MAP = get_congress_map(114) 
MAP = MAP %>%
  dplyr::select(STATENAME, DISTRICT, geometry)
BEGIN = DATES$BEGIN_DATE[which(str_detect(DATES$Congress, "114"))]

#Only For 114
Rel_Storms = filter(ALL_Successful, BEGIN_DATE >= BEGIN)
library(sf)
Test = st_as_sf(Rel_Storms, coords = c("LON", "LAT"), crs = 4269)
RES_114 = st_join(Test, MAP)
#Adding Them on to the 114 Merge
for(i in 99:113){
  print(i)
  MAP = get_congress_map(i) %>% dplyr::select(STATENAME,DISTRICT, geometry)
  MAP = st_transform(MAP, crs = 4269)
  Congress_Num = as.character(i); Congress_Num_End = as.character(i+1)
  BEGIN = DATES$BEGIN_DATE[which(str_detect(DATES$Congress,Congress_Num))]
  END   = DATES$BEGIN_DATE[which(str_detect(DATES$Congress, Congress_Num_End))]
  Rel_Storms = ALL_Successful %>%
       mutate(END_DATE = as.Date(END_DATE), BEGIN_DATE = as.Date(BEGIN_DATE)) %>%
       filter(END_DATE > as.Date(BEGIN) & END_DATE <= as.Date(END))
  library(sf)
  Rel_Storms = st_as_sf(Rel_Storms, coords = c("LON", "LAT"), crs = 4269)
  RES = st_join(Rel_Storms, MAP)
  RES_114 = bind_rows(RES_114,RES)
}

#Separate the successful from unsuccessful
CD_Merged_Unsuccessful = RES_114 %>% filter(is.na(DISTRICT))
#Characteristics of the Ones that dont merge
CD_Merged_Successful %>%
  mutate(YEAR = lubridate::year(END_DATE)) %>%
  group_by(YEAR) %>%
  summarize(COUNT = n()) %>% View()


CD_Merged_Successful = RES_114 %>%
  filter(!is.na(DISTRICT)) %>%
  as.data.frame() %>%
  dplyr::select(-geometry)
head(CD_Merged_Successful)




#-----------------------------------Now Trying to Merge to House of Reps------------------#

ALL_LCV_HOUSE_MERGE = read.csv("~/Google Drive/DATA/ECON/CLEAN/LCV/HOUSE.csv")
ALL_LCV_HOUSE_MERGE = mutate(ALL_LCV_HOUSE_MERGE, DATE = as.Date(DATE))

 
#Defining a merge function
glimpse(CD_Merged_Successful)
CD_Merged_Successful_2 = CD_Merged_Successful %>%
  mutate(District = case_when(
    STATE == "VERMONT" ~ "VT-AL",
    STATE != "VERMONT" & str_length(DISTRICT) == 1 ~ str_c(STATE_CODE, "-0", DISTRICT),
    TRUE ~ str_c(STATE_CODE, "-", DISTRICT)
  ))

head(CD_Merged_Successful_2)
class(ALL_LCV_HOUSE_MERGE)

#Test for 0-30 Date Range
Test = ALL_LCV_HOUSE_MERGE %>%
  sample_n(100) %>%
  mutate(DATE_1 = DATE-0, DATE_2 = DATE-30) %>%
  fuzzyjoin::fuzzy_left_join(CD_Merged_Successful_2,  by = c("District" = "District",
                                                           "DATE_2" = "END_DATE",
                                                           "DATE_1" = "END_DATE"),
                             match_fun = list(`==`, `<=`, `>`)) 

Compute_Lag_District = function(DSET, DAT, LAG_NEAR = 0, LAG_FAR, PLACE, TIME = "BEGIN"){
  DAT_FAR  = DAT - LAG_FAR
  DAT_NEAR = DAT - LAG_NEAR
  if(TIME == "BEGIN"){
    RES = sum(DSET$DAMAGE_PROPERTY[which(DSET$BEGIN_DATE >= DAT_FAR  & 
                                           DSET$BEGIN_DATE <= DAT_NEAR & 
                                           DSET$District == PLACE)], na.rm = TRUE)
  }
  else if (TIME == "END"){
    RES = sum(DSET$DAMAGE_PROPERTY[which(DSET$END_DATE >= DAT_FAR    & 
                                           DSET$END_DATE <= DAT_NEAR   & 
                                           DSET$District == PLACE)], na.rm = TRUE)
  }
  else if (TIME == "MID"){
    RES = sum(DSET$DAMAGE_PROPERTY[which(DSET$MID_DATE >= DAT_FAR      & 
                                           DSET$MID_DATE <= DAT_NEAR     & 
                                           DSET$District == PLACE)],   na.rm = TRUE)
  }
  return(RES)
}

Placebo_Events = CD_Merged_Successful_2 %>%
  filter(PLACEBO_EVENT == 1)
nrow(Placebo_Events)

Test = sample_n(ALL_LCV_HOUSE_MERGE,100)
ALL_LCV_HOUSE_MERGE$Lag_30 =  as.numeric(unlist(mclapply(1:nrow(ALL_LCV_HOUSE_MERGE),
         function(x) Compute_Lag_District(DSET = CD_Merged_Successful_2, DAT = ALL_LCV_HOUSE_MERGE$DATE[x], 
                                          LAG_NEAR = 0, LAG_FAR = 30, PLACE = ALL_LCV_HOUSE_MERGE$District[x], TIME = "END"), mc.cores = 5)))
ALL_LCV_HOUSE_MERGE$Lag_30_Placebo = as.numeric(unlist(mclapply(1:nrow(ALL_LCV_HOUSE_MERGE),
                                                                function(x) Compute_Lag_District(DSET = Placebo_Events, DAT = ALL_LCV_HOUSE_MERGE$DATE[x], 
                                                                                                 LAG_NEAR = 0, LAG_FAR = 30, PLACE = ALL_LCV_HOUSE_MERGE$District[x], TIME = "END"), mc.cores = 5)))
table(ALL_LCV_HOUSE_MERGE$Lag_30_Placebo > 0)

nrow(CD_Merged_Successful)/nrow(NOAA_RELEVANT)

library(fixest)
feols()
head(ALL_LCV_HOUSE_MERGE_Regress)


#Quick Test
ALL_LCV_HOUSE_MERGE_Regress = ALL_LCV_HOUSE_MERGE %>%
  mutate(SEASON = case_when(
    MON_NUM %in% c(12,1,2,3) ~ "WINTER",
    MON_NUM %in% c(4,5,6) ~ "SPRING",
    MON_NUM %in% c(7,8,9) ~ "SUMMER",
    MON_NUM %in% c(10,11) ~ "FALL")) %>% #Season fixed effect
  mutate(PANEL_VAR = 100000*Year + ROLL_CALL) %>%
  mutate(Member.of.Congress = as.character(Member.of.Congress),
        Member.of.Congress = case_when(
              Member.of.Congress == "Rogers, Mike"  & State == "AL" ~ "Rogers, Mike_AL",
               Member.of.Congress == "Brown, George" & State == "CO" ~ "Brown, George_CO",
              TRUE ~ Member.of.Congress)) %>% #This is an Extra Catch -- didnt seem to get caught the fi#Panel time variables
  mutate(REP = ifelse(Party == "R",1,0)) %>%
  mutate(Lag_30_Not_Placebo = Lag_30 - Lag_30_Placebo,
         Lag_30_Not_Placebo_BIN = ifelse(Lag_30_Not_Placebo >0,1,0)) %>%
  mutate(Lag_30_BIN = ifelse(Lag_30 >0,1,0)) %>%
  mutate(Lag_30_Zero = ifelse(Lag_30 %in% 1:1000000,1,0),
         Lag_30_Mid = ifelse(Lag_30 %in% 1000001:50000000,1,0),
         Lag_30_Big = ifelse(Lag_30 >50000000,1,0)) %>%
  mutate(Lag_30_Placebo_BIN = ifelse(Lag_30_Placebo>0,1,0))


glimpse(ALL_LCV_HOUSE_MERGE_Regress)
X = plm(POS_VOTE ~ Lag_30_Placebo_BIN*REP - REP + factor(Year) + factor(SEASON), data = ALL_LCV_HOUSE_MERGE_Regress, 
        model = "within", index = c("Member.of.Congress","PANEL_VAR"))
X1 = plm(POS_VOTE ~ Lag_30_Not_Placebo_BIN*REP - REP + factor(Year) + factor(SEASON), data = ALL_LCV_HOUSE_MERGE_Regress, 
         model = "within", index = c("Member.of.Congress","PANEL_VAR"))


X = plm(POS_VOTE ~Lag_30_Zero*REP + Lag_30_Mid*REP + Lag_30_Big*REP - REP + factor(Year) + factor(SEASON), data = ALL_LCV_HOUSE_MERGE_Regress, 
         model = "within", index = c("Member.of.Congress","PANEL_VAR"))

summary(X1)


plm()
X = feols(POS_VOTE ~  I(Lag_30>0)*REP - REP + factor(SEASON) | Member.of.Congress, 
      data = ALL_LCV_HOUSE_MERGE_Regress, panel.id = c("Member.of.Congress", "PANEL_VAR"))
summary(X)

head(Test)

CD_Merged_Successful_2 %>%
  filter(District == "VA-10", END_DATE >= "2013-05-29", END_DATE < "2013-06-28")

s.numeric(unlist(mclapply(1:nrow(SMALL_VOTE), 
                          function(x) Compute_Lag_V2(DSET = SMALL, DAT = SMALL_VOTE$DATE[x], LAG_NEAR = ..1, LAG_FAR = ..2, 
                                                     PLACE = SMALL_VOTE$State[x], TIME = ..3), mc.cores = 6))))



Test %>%
  group_by(District.x, DATE) %>%
  summarize(SUM = sum(DAMAGE_PROPERTY, na.rm = TRUE))


CD_Merged_Successful_2 %>%
  filter(District == "AR-01", END_DATE >= "2016-04-17", END_DATE < "2016-05-17")


head(CD_Merged_Successful_2)
glimpse(ALL_LCV_HOUSE_MERGE)



RES_114 
colnames(RES_114)
class(RES_114)

CRS(MAP_100)
class(MAP_100)
class(RES_114)
library(tidyverse)
filter(RES_114, is.na(STATENAME))
#Saving Intermediate
RES_114[which(is.na(RES_114$STATENAME)),]
write_sf(RES_114, "~/Desktop/Granular")
#Looking at Errors
anti_join(ALL_Successful, RES_114) #All rows match

table(is.na(RES_114$DISTRICT))
No_Districts = filter(RES_114, is.na(DIS))



DATES$BEGIN_DATE[which(str_detect(DATES$Congress,"99"))]
min(as.Date(ALL_Successful$END_DATE))


ALL_Successful = ALL_Successful %>%
  mutate(BEGIN_DATE = as.Date(BEGIN_DATE), END_DATE = as.Date(END_DATE))
anti_join(ALL_Successful, RES_114)
#-----THERE ARE GAPS BETWEEN BEGINNING AND END DATES



#Overarching Shapefile
#THIS WORKS!!!!1


table(is.na(DAT_CLEAN$END_LAT))
head(DAT_CLEAN)


#Manual Process
#Applying to Senate Data
#15-Day Lag
Distinct_Senate_Votes$Lag15_B = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                              function(x) Compute_Lag_V2(DSET = DAT_CLEAN_NOZEROS,
                                                         Distinct_Senate_Votes$DATE[x], 0, 15, 
                                                         Distinct_Senate_Votes$State[x], TIME = "BEGIN"), mc.cores = 6)))
#30-Day Lag
Distinct_Senate_Votes$Lag30_B = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                              function(x) Compute_Lag(Test$DATE[x], 0, 30, Test$State[x], TIME = "BEGIN"), mc.cores = 8)))
#45-Day Lag
Distinct_Senate_Votes$Lag45_B = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                              function(x) Compute_Lag(Test$DATE[x], 0, 45, Test$State[x], TIME = "BEGIN"), mc.cores = 8)))
#60-Day Lag
Distinct_Senate_Votes$Lag60_B = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                              function(x) Compute_Lag(Test$DATE[x], 0, 60, Test$State[x], TIME = "BEGIN"), mc.cores = 8)))
#90-Day Lag
Distinct_Senate_Votes$Lag90_B = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                              function(x) Compute_Lag(Test$DATE[x], 0, 90, Test$State[x], TIME = "BEGIN"), mc.cores = 8)))
#Now Adding Intermediate Lags
#Lag_15_30
Distinct_Senate_Votes$Lag15_30_B = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                                 function(x) Compute_Lag(Test$DATE[x], 15, 30, Test$State[x], TIME = "BEGIN"), mc.cores = 8)))
#Lag_30_45
Distinct_Senate_Votes$Lag30_45_B = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                                 function(x) Compute_Lag(Test$DATE[x], 30, 45, Test$State[x], TIME = "BEGIN"), mc.cores = 8)))
#Lag_45_60
Distinct_Senate_Votes$Lag45_60_B = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                                 function(x) Compute_Lag(Test$DATE[x], 45, 60, Test$State[x], TIME = "BEGIN"), mc.cores = 8)))
#Lag_30_60
Distinct_Senate_Votes$Lag30_60_B = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                                 function(x) Compute_Lag(Test$DATE[x], 30, 60, Test$State[x], TIME = "BEGIN"), mc.cores = 8)))
#Lag_60_90
Distinct_Senate_Votes$Lag60_90_B = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                                 function(x) Compute_Lag(Test$DATE[x], 60, 90, Test$State[x], TIME = "BEGIN"), mc.cores = 8)))


#Adding All the Same Lags From End Date
Distinct_Senate_Votes$Lag15_E = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                              function(x) Compute_Lag(Test$DATE[x], 0, 15, Test$State[x], TIME = "END"), mc.cores = 8)))
#30-Day Lag
Distinct_Senate_Votes$Lag30_E = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                              function(x) Compute_Lag(Test$DATE[x], 0, 30, Test$State[x], TIME = "END"), mc.cores = 8)))
#45-Day Lag
Distinct_Senate_Votes$Lag45_E = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                              function(x) Compute_Lag(Test$DATE[x], 0, 45, Test$State[x], TIME = "END"), mc.cores = 8)))
#60-Day Lag
Distinct_Senate_Votes$Lag60_E = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                              function(x) Compute_Lag(Test$DATE[x], 0, 60, Test$State[x], TIME = "END"), mc.cores = 8)))
#90-Day Lag
Distinct_Senate_Votes$Lag90_E = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                              function(x) Compute_Lag(Test$DATE[x], 0, 90, Test$State[x], TIME = "END"), mc.cores = 8)))
#Now Adding Intermediate Lags
#Lag_15_30
Distinct_Senate_Votes$Lag15_30_E = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                                 function(x) Compute_Lag(Test$DATE[x], 15, 30, Test$State[x], TIME = "END"), mc.cores = 8)))
#Lag_30_45
Distinct_Senate_Votes$Lag30_45_E = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                                 function(x) Compute_Lag(Test$DATE[x], 30, 45, Test$State[x], TIME = "END"), mc.cores = 8)))
#Lag_45_60
Distinct_Senate_Votes$Lag45_60_E = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                                 function(x) Compute_Lag(Test$DATE[x], 45, 60, Test$State[x], TIME = "END"), mc.cores = 8)))
#Lag_30_60
Distinct_Senate_Votes$Lag30_60_E = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                                 function(x) Compute_Lag(Test$DATE[x], 30, 60, Test$State[x], TIME = "END"), mc.cores = 8)))
#Lag_60_90
Distinct_Senate_Votes$Lag60_90_E = as.numeric(unlist(mclapply(1:nrow(Distinct_Senate_Votes), 
                                 function(x) Compute_Lag(Test$DATE[x], 60, 90, Test$State[x], TIME = "END"), mc.cores = 8)))

#Merge Back Onto Full Senate Dataset
ALL_LCV_SENATE_MERGE = ALL_LCV_SENATE_MERGE %>%
  mutate(DATE = as.Date(DATE)) %>%
  left_join(Distinct_Senate_Votes, by = c("DATE", "Policy", "State"))


glimpse(ALL_LCV_SENATE_MERGE)

ALL_LCV_SENATE_MERGE = ALL_LCV_SENATE_MERGE %>%
  mutate(Lag15_B = log(Lag15_B.y + 1))


summary(plm(POS_VOTE ~ Lag15_B + factor(Year), data = ALL_LCV_SENATE_MERGE, model = "within", index = "Member.of.Congress"))


#---------------------------------------Something to Work on -- Better Implementation of the Lags---------------------------------#
args = list(
  L1 = c(0,15,30,45,60,90),
  L2 = c(0,15,30,45,60,90),
  TIME = c("BEGIN","END")
)
crossedArgs = cross_df(args) %>%
  filter(L2 - L1 >0)
crossedArgs

X = pmap_dfc(list(..1 = crossedArgs$L2, ..2 = crossedArgs$L1, ..3 = crossedArgs$TIME),
     .f = ~ data.frame(as.numeric(unlist(mclapply(1:nrow(SMALL_VOTE), 
                                             function(x) Compute_Lag_V2(DSET = SMALL, DAT = SMALL_VOTE$DATE[x], LAG_NEAR = ..1, LAG_FAR = ..2, 
                                                                       PLACE = SMALL_VOTE$State[x], TIME = ..3), mc.cores = 6)))))
names = pmap_chr(list(..1 = crossedArgs$L2, ..2 = crossedArgs$L1, ..3 = crossedArgs$TIME),
                 ~ str_c("Lag",as.character(..2),as.character(..1),..3,sep = "_"))

X


#
STATES = read_xlsx("~/Google Drive/DATA/ECON/STATES_REGIONS.xlsx")
ALL_LCV_HOUSE_MERGE = read.csv("~/Google Drive/DATA/ECON/CLEAN/LCV/HOUSE.csv")
Distinct_HOUSE_Votes = ALL_LCV_HOUSE_MERGE %>%
  distinct(DATE, Policy, CODE) %>%
  mutate(DATE = as.Date(DATE))

args = list(
  L1 = c(0,15,30,45,60,90),
  L2 = c(0,15,30,45,60,90),
  TIME = c("BEGIN","END")
)
crossedArgs = cross_df(args) %>%
  filter(L2 - L1 >0)

names = pmap_chr(list(..1 = crossedArgs$L1, ..2 = crossedArgs$L2, ..3 = crossedArgs$TIME),
                 ~ str_c("Lag",as.character(..1),as.character(..2),..3,sep = "_"))


glimpse(Distinct_HOUSE_Votes)
glimpse(Distinct_Senate_Votes)
glimpse(DAT_CLEAN_NOZEROS)

unlist(mclapply(1:10, 
      function(x) Compute_Lag_V2(DAT_CLEAN_NOZEROS,Distinct_HOUSE_Votes$DATE[x], 60, 90, Distinct_HOUSE_Votes$CODE[x], TIME = "END"), mc.cores = 3))


