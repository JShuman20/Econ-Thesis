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



library(sf)
CONG_TEST = get_congress_map(114) 

MA = tigris::counties(2010, resolution = "500k", state = "MA")   
MA = st_as_sf(MA)

CONG_TEST

MA
Worcester = MA[which(MA$NAME=="Worcester"),]
Worcester = st_transform(Worcester, crs = 4269)

Worcester


Test = st_join(Worcester,CONG_TEST)
Test



#-------------------Plan for Granular Merge-------------------#
#01. There are 2 types of geographies: counties and weather zones
#02. 2/3 of events have beginning and ending coordinates -- we will construct a midpoint and assign that as the coordinate
#03. The remaining ones are as follows
NOAA = read.csv("~/Google Drive/DATA/ECON/CLEAN/NOAA/02_NOAA_CLEANED.csv")
NOAA_RELEVANT = NOAA %>% filter(DAMAGE_PROPERTY > 0)
#Doing Preliminary Transformations
NOAA_RELEVANT = NOAA_RELEVANT %>%
  mutate(END_DATE = as.Date(END_DATE)) %>%
  mutate(CONG = case_when(
    YEAR %in% 1985:1986 ~ 99,
    YEAR %in% 1987:1988 ~ 100,
    YEAR %in% 1989:1990 ~ 101,
    YEAR %in% 1991:1992 ~ 102,
    YEAR %in% 1993:1994 ~ 103,
    YEAR %in% 1995:1996 ~ 104,
    YEAR %in% 1997:1998 ~ 105,
    YEAR %in% 1999:2000 ~ 106,
    YEAR %in% 2001:2002 ~ 107,
    YEAR %in% 2003:2004 ~ 108,
    YEAR %in% 2005:2006 ~ 109,
    YEAR %in% 2007:2008 ~ 110,
    YEAR %in% 2009:2010 ~ 111,
    YEAR %in% 2011:2012 ~ 112,
    YEAR %in% 2013:2014 ~ 113,
    YEAR %in% 2015:2016 ~ 114,
    YEAR %in% 2017:2018 ~ 115,
    YEAR %in% 2019:2020 ~ 116),
    CONG_MERGE = ifelse(CONG == 116,115,CONG),
    STATE_ZONE = case_when(
      str_length(as.character(CZ_FIPS))==1 ~   str_c(STATE_CODE, "00",CZ_FIPS),
      str_length(as.character(CZ_FIPS))==2 ~   str_c(STATE_CODE, "0",CZ_FIPS),
      TRUE ~ str_c(STATE_CODE,CZ_FIPS))) # There are no 2019-20 Shapefiles Available, so we'll have to deal


#Reading in Weather Zone Shapefiles
STATES = read_xlsx("~/Google Drive/DATA/ECON/STATES.xlsx") %>% pull(STATE)
######04A: Merging With Shapefiles
Shapes = st_read("~/Desktop/z_03mr20/z_03mr20.shp")
Shapes = Shapes %>% filter(STATE %in% STATES)
Shapes = Shapes %>%
  dplyr::select(STATE_ZONE, LON,LAT, NAME, SHORTNAME, STATE)  %>%
  group_by(STATE_ZONE) %>%
  sample_n(1) %>%
  ungroup() %>%
  mutate(STATE_ZONE = as.character(STATE_ZONE),
          NAME_UPPER = toupper(NAME),
         SHORTNAME_UPPER = toupper(SHORTNAME))


#Separating into Type C and Z for different merges
TypeZ = filter(NOAA_RELEVANT, CZ_TYPE == "Z")
TypeC = filter(NOAA_RELEVANT, CZ_TYPE == "C")

head(TypeZ)



#Joining Type Z to State
TypeZ_withShape = TypeZ %>%
  left_join(Shapes, by = "STATE_ZONE") %>%
  filter(!is.na(LAT))

TypeZ_noShape = TypeZ %>% 
  anti_join(Shapes, by = "STATE_ZONE") %>%
  mutate(STATE_CODE = as.character(STATE_CODE), 
         CZ_NAME = as.character(CZ_NAME),
         STATE_ZONE_Merge = NA)

#Filling Rest By Pattern-Matching Names
for(i in 1:nrow(TypeZ_noShape)){
  Restricted = Shapes %>% filter(STATE == TypeZ_noShape$STATE_CODE[i] & 
                                 (str_detect(NAME_UPPER, TypeZ_noShape$CZ_NAME[i]) | 
                                 str_detect(SHORTNAME_UPPER, TypeZ_noShape$CZ_NAME[i])))
  if(nrow(Restricted)>0){
    Restricted = sample_n(Restricted, 1)
  }
  if(!is_empty(Restricted$STATE_ZONE)){
    TypeZ_noShape$STATE_ZONE_Merge[i] = Restricted$STATE_ZONE
  }
}
class(TypeZ_noShape$STATE_ZONE_Merge)
TypeZ_noShape = TypeZ_noShape %>%
  mutate(STATE_ZONE = as.character(STATE_ZONE_Merge)) %>%
  dplyr::select(-STATE_ZONE_Merge) %>%
  left_join(Shapes, by = "STATE_ZONE")

#Merged on everything for which i can find a geometry
TypeZ_withShape = rbind(TypeZ_withShape, TypeZ_noShape[which(!is.na(TypeZ_noShape$LAT)),])
TypeZ_noShape = TypeZ_noShape[which(is.na(TypeZ_noShape$LAT)),]



get_congress_map <- function(cong) {
  tmp_file <- tempfile()
  tmp_dir  <- tempdir()
  zp <- sprintf("http://cdmaps.polisci.ucla.edu/shp/districts%03i.zip",cong)
  download.file(zp, tmp_file)
  unzip(zipfile = tmp_file, exdir = tmp_dir)
  fpath <- paste(tmp_dir, sprintf("districtShapes/districts%03i.shp",cong), sep = "/")
  st_read(fpath)
}



CONG_Test = get_congress_map(114)

CONG_Test = dplyr::select(CONG_Test, c(STATENAME,ID,DISTRICT,STARTCONG,ENDCONG,geometry))

CONG_Test_Maine = filter(CONG_Test, STATENAME == "Maine")
Shapes_Maine = filter(Shapes, STATE == "ME")

Test_Base = st_join(Shapes_Maine, CONG_Test_Maine) 

#Confirmed -- Congress Map Makes Up Whole State
#plot(CONG_Test_Maine$geometry[which(CONG_Test_Maine$DISTRICT == 1)])
#plot(CONG_Test_Maine$geometry[which(CONG_Test_Maine$DISTRICT == 1)], add = T)

#Confirmed -- the Overlaps are doing what I want
Test_Base_Dups = Test_Base %>%
  group_by(STATE_ZONE) %>%
  mutate(COUNT = n()) %>%
  ungroup() %>%
  filter(COUNT == 2) %>%
  distinct()

#plot(CONG_Test_Maine$geometry[which(CONG_Test_Maine$DISTRICT == 1)], col = "grey")
#plot(CONG_Test_Maine$geometry[which(CONG_Test_Maine$DISTRICT == 2)], col = "lightblue", add = T)
#plot(Test_Base_Dups$geometry, border = "red", add = T)


#Prepare Type Z For Merge
TypeZ_withShape = TypeZ_withShape %>%
  dplyr::select(STATE.x, STATE_FIPS, STATE_CODE, STATE_ZONE, YEAR, EVENT_ID, EVENT_TYPE, CZ_NAME, END_DATE,INJURIES_DIRECT, INJURIES_INDIRECT,
                DEATHS_DIRECT,DEATHS_INDIRECT, DAMAGE_PROPERTY, PLACEBO_EVENT, CONG, CONG_MERGE,geometry)



TypeZ_withShape_114 = TypeZ_withShape[which(TypeZ_withShape$CONG_MERGE=="114"),] %>%
  st_as_sf()

TypeZ_CD_Merge = st_join(TypeZ_withShape_114, CONG_Test)


#-----------Now Trying to Handle Type C----------------#
EARLY_Years_Shapes = st_read("~/Desktop/US_AtlasHCB_Counties_Gen001/US_HistCounties_Gen001_Shapefile/US_HistCounties_Gen001.shp") %>%
  filter(END_DATE > "1980-01-01") %>%
  dplyr::select(c(NAME, STATE_TERR, FIPS, START_DATE, END_DATE, FULL_NAME, geometry)) %>%
  filter(STATE_TERR != "District of Columbia") %>%
  mutate(FIPS = as.character(FIPS))


TypeC = TypeC %>%
  dplyr::select(STATE,STATE_FIPS, CZ_TYPE, CZ_FIPS, CZ_NAME, STATE_CODE, EVENT_ID, 
                DAMAGE_PROPERTY, BEGIN_DATE, END_DATE, PLACEBO_EVENT) %>%
  mutate_at(c("STATE_FIPS", "CZ_FIPS", "CZ_NAME"), as.character) %>%
  mutate(STATE_FIPS = ifelse(str_length(STATE_FIPS)==2, STATE_FIPS, str_c("0", STATE_FIPS)),
         CZ_FIPS = case_when(str_length(CZ_FIPS)==1 ~ str_c("00",CZ_FIPS),
                             str_length(CZ_FIPS)==2 ~ str_c("0", CZ_FIPS),
                             TRUE ~ CZ_FIPS),
         FIPS = str_c(STATE_FIPS,CZ_FIPS)) %>%
  #Doing Some Regex Catches For Typos
  mutate(FIPS = case_when(
    str_length(CZ_NAME)==5 & str_detect(CZ_NAME, pattern = "[[:upper:]]{2}\\d{3}") ~ str_c(STATE_CODE, str_sub(CZ_NAME,3,5)),
    TRUE ~ FIPS))

TypeC_Early = filter(TypeC, END_DATE <= "2000-12-31")
TypeC_Late  = filter(TypeC, END_DATE > "2000-12-31")
  
#Merging with pre-2000 shapefiles

######Doing The Merge Piecewise to Increase Speed
library(furrr)
options("future.fork.enable" = TRUE)
future::plan(multicore(workers = 3)) 
TypeC_Early_Merged = future_map_dfr(.x = 1:69, .f = function(.x){
  INDEX = ifelse(.x==1,1,1000*(.x-1)+1)
  IND_HIGH = ifelse(.x==69, nrow(TypeC_Early), INDEX + 999)
  DAT = TypeC_Early[INDEX:IND_HIGH,] %>%
    fuzzyjoin::fuzzy_left_join(EARLY_Years_Shapes,  by = c("FIPS" = "FIPS",
                                                           "END_DATE" = "START_DATE",
                                                           "END_DATE" = "END_DATE"),
                               match_fun = list(`==`, `>`, `<=`)) 
  return(DAT)
}, .progress = T)

TypeC_Early_Merged = TypeC_Early_Merged %>%
  st_as_sf()

#Extracting Ones that Did and Did not work
TypeC_Early_Merged_Successful = TypeC_Early_Merged[which(!is.na(TypeC_Early_Merged$FULL_NAME)),]
TypeC_Early_Merged_NoMatch = TypeC_Early_Merged[which(is.na(TypeC_Early_Merged$FULL_NAME)),]


library(tigris)

Test = lapply(X = list(2000,2010,2011, 2012,2013,2014,2015,2016,2017,2018,2019), FUN = function(X) {
  Counties = counties(X, resolution = "500k", state = c(STATES[-grep("DC",STATES)])) %>%
    st_as_sf()
  Counties = Counties[,c("STATEFP", "COUNTYFP", "NAME", "geometry")]
  Counties$YEAR = as.character(X)
  return(Counties)}) 

All_Counties = rbind(Test[[1]], Test[[2]], Test[[3]],Test[[4]],Test[[5]],Test[[6]],Test[[7]],Test[[8]],
                     Test[[9]],Test[[10]],Test[[11]])

#Create a FIPS Column
All_Counties$FIPS = str_c(All_Counties$STATEFP, All_Counties$COUNTYFP)
All_Counties$YEAR_MERGE = All_Counties$YEAR





TypeC_Late = TypeC_Late %>%
  mutate(YEAR = as.character(lubridate::year(END_DATE)))
TypeC_Late = TypeC_Late %>%
  mutate(YEAR_MERGE = case_when(
           YEAR %in% as.character(2001:2010) ~ "2010",
           TRUE ~ YEAR))


#Now Trying to merge onto counties
TypeC_Late_Merged = TypeC_Late %>%
  left_join(All_Counties, by = c("FIPS", "YEAR_MERGE"))
TypeC_Late_Merged_Successful = TypeC_Late_Merged[which(!is.na(TypeC_Late_Merged$YEAR.y)),]


TypeC_Late_Merged_NoMatch = TypeC_Late_Merged[which(is.na(TypeC_Late_Merged$YEAR.y)),]  

#Now Doing Some Fixes on The No Matches
TypeC_Late_Merged_NoMatch = dplyr::select(as.data.frame(TypeC_Late_Merged_NoMatch), -geometry)
TypeC_Late_Merged_NoMatch = TypeC_Late_Merged_NoMatch %>%
  dplyr::select(-c(YEAR.y,COUNTYFP,STATEFP)) %>%
  mutate(YEAR_MERGE = ifelse(str_detect(CZ_NAME, "JUNEAU BUREAU"), "2010", YEAR_MERGE),
         FIPS = case_when(
           str_detect(CZ_NAME, "HOONAH") ~ "02105",
           str_detect(CZ_NAME, "COPPER") ~ "02261",
           str_detect(CZ_NAME, "WRANGELL") ~ "02275",
           str_detect(CZ_NAME, "BEDFORD") ~ "51019",
           str_detect(CZ_NAME, "HOONAH") ~ "02105",
           str_detect(CZ_NAME, "SHANNON") ~ "46102",
           str_detect(CZ_NAME, "SITKA") ~ "02270",
           str_detect(CZ_NAME, "DENALI") ~ "02068",
           str_detect(CZ_NAME, "JUNEAU BOROUGH") ~ "02110",
           str_detect(CZ_NAME, "YUKON|KOYUKUK") ~ "02290",
           str_detect(CZ_NAME, "SUSITNA|MATANUSKA") ~ "02170",
           str_detect(CZ_NAME, "YAKUTAT") ~ "02282",
           str_detect(CZ_NAME, "BRISTOL BAY") ~ "02060",
           str_detect(CZ_NAME, "KODIAK") ~ "02150",
           str_detect(CZ_NAME, "ANCHORAGE") ~ "02020",
           TRUE ~ FIPS)) %>%
  left_join(All_Counties, by = c("FIPS", "YEAR_MERGE"))




TypeC_Late_Merged_NoMatch = rename(TypeC_Late_Merged_NoMatch, NAME = NAME.x)
TypeC_Late_Merged_NoMatch = TypeC_Late_Merged_NoMatch[,c("STATE", "STATE_FIPS","CZ_TYPE", "CZ_FIPS", "CZ_NAME", "STATE_CODE",
                                                         "EVENT_ID","DAMAGE_PROPERTY", "STATEFP",
                                                         "END_DATE","PLACEBO_EVENT","FIPS","YEAR.x", "geometry")]

head(TypeC_Late_Merged_NoMatch)
TypeC_Late_Merged_Successful = TypeC_Late_Merged_Successful[,c("STATE", "STATE_FIPS","CZ_TYPE", "CZ_FIPS", "CZ_NAME", "STATE_CODE",
                                                               "EVENT_ID","DAMAGE_PROPERTY","STATEFP",
                                                               "END_DATE","PLACEBO_EVENT","FIPS","YEAR.x", "geometry")]

#Combining all successes
TypeC_Late_Merged_Successful = rbind(TypeC_Late_Merged_Successful, 
                                     TypeC_Late_Merged_NoMatch[which(!is.na(TypeC_Late_Merged_NoMatch$STATEFP)),])
head(TypeC_Late_Merged_Successful)
TypeC_Late_Merged_NoMatch = TypeC_Late_Merged_NoMatch[which(is.na(TypeC_Late_Merged_NoMatch$STATEFP)),]


All_TypeCs = list(TypeC_Late_Merged_Successful,TypeC_Late_Merged_NoMatch, 
                  TypeC_Early_Merged_Successful, TypeC_Early_Merged_NoMatch)

write_rds(All_TypeCs, "~/Desktop/ECON Thesis/TypeC_Intermediate.rds", compress = "gz")




#Post_Processed = read_rds("~/Desktop/ECON Thesis/TypeC_Intermediate.rds")


Test = All_TypeCs[[1]] %>%
  st_as_sf(crs = 4269)


Test = Test[,c("STATE", "STATE_FIPS","CZ_TYPE", "CZ_FIPS", "CZ_NAME", "STATE_CODE",
               "EVENT_ID","DAMAGE_PROPERTY",
               "END_DATE","PLACEBO_EVENT","FIPS", "geometry")]


Test_2 = All_TypeCs[[3]] %>%
  st_sf(crs = 4269) %>%
  st_transform(crs = 4269)

Test_2 = rename(Test_2, END_DATE = END_DATE.x, FIPS = FIPS.x)
Test_2 = Test_2[,c("STATE", "STATE_FIPS","CZ_TYPE", "CZ_FIPS", "CZ_NAME", "STATE_CODE",
                   "EVENT_ID","DAMAGE_PROPERTY",
                   "END_DATE","PLACEBO_EVENT","FIPS", "geometry")]

colnames(Test_2) == colnames(Test)

ALL = rbind(Test, Test_2)

class(ALL)
#Combine with Tpye Z
TypeZ_withShape = rename(TypeZ_withShape, STATE = STATE.x)
TypeZ_withShape = TypeZ_withShape[,c("STATE", "STATE_FIPS", "CZ_NAME", "STATE_CODE",
                                     "EVENT_ID","DAMAGE_PROPERTY",
                                     "END_DATE","PLACEBO_EVENT", "geometry")]
TypeZ_withShape = st_as_sf(TypeZ_withShape)
ALL = ALL[,c("STATE", "STATE_FIPS", "CZ_NAME", "STATE_CODE",
             "EVENT_ID","DAMAGE_PROPERTY",
             "END_DATE","PLACEBO_EVENT", "geometry")] 

class(TypeZ_withShape)

ALL = rbind(ALL,TypeZ_withShape) 
#Writing an intermediate file
write_rds(ALL, "~/Desktop/ECON Thesis/All_Storms_Geographies.rds")

#Re-Adding Year and CONG because it got dropped along the way
ALL =ALL %>%
  mutate(END_DATE = as.Date(END_DATE),
         YEAR = lubridate::year(END_DATE)) %>%
  mutate(CONG = case_when(
    YEAR %in% 1985:1986 ~ 99,
    YEAR %in% 1987:1988 ~ 100,
    YEAR %in% 1989:1990 ~ 101,
    YEAR %in% 1991:1992 ~ 102,
    YEAR %in% 1993:1994 ~ 103,
    YEAR %in% 1995:1996 ~ 104,
    YEAR %in% 1997:1998 ~ 105,
    YEAR %in% 1999:2000 ~ 106,
    YEAR %in% 2001:2002 ~ 107,
    YEAR %in% 2003:2004 ~ 108,
    YEAR %in% 2005:2006 ~ 109,
    YEAR %in% 2007:2008 ~ 110,
    YEAR %in% 2009:2010 ~ 111,
    YEAR %in% 2011:2012 ~ 112,
    YEAR %in% 2013:2014 ~ 113,
    YEAR %in% 2015:2016 ~ 114,
    YEAR %in% 2017:2018 ~ 115,
    YEAR %in% 2019:2020 ~ 116),
    CONG_MERGE = ifelse(CONG == 116,115,CONG))
  
  
table(ALL$CONG_MERGE)


get_congress_map <- function(cong) {
  tmp_file <- tempfile()
  tmp_dir  <- tempdir()
  zp <- sprintf("http://cdmaps.polisci.ucla.edu/shp/districts%03i.zip",cong)
  download.file(zp, tmp_file)
  unzip(zipfile = tmp_file, exdir = tmp_dir)
  fpath <- paste(tmp_dir, sprintf("districtShapes/districts%03i.shp",cong), sep = "/")
  st_read(fpath)
}


#Initialize 99
CONG_DAT = get_congress_map(99)
CONG_DAT  = dplyr::select(CONG_DAT, c(STATENAME,ID,DISTRICT,
                                                   STARTCONG,ENDCONG,geometry))
REL_DAT = ALL[which(ALL$CONG_MERGE==99),]
RES = st_join(REL_DAT,CONG_DAT)



#This Handles Merges for 99-114

Merged_CD = lapply(X = c(99:114), FUN = function(X) {
  print(X)
  CONG_DAT = get_congress_map(X) 
  CONG_DAT  = dplyr::select(CONG_DAT, c(STATENAME,ID,DISTRICT,
                               STARTCONG,ENDCONG,geometry)) %>%
    st_transform(crs = 4269)
  REL_DAT = ALL[which(ALL$CONG_MERGE==X),] %>%
    st_transform(crs = 4269)
  print(nrow(REL_DAT))
  RES = st_join(REL_DAT, CONG_DAT)
  print(nrow(RES))
  return(RES)
})

Merged_CD_2 = lapply(X = Merged_CD, FUN = function(X) {X %>% mutate(STATENAME = toupper(STATENAME)) %>%
                        filter(STATE == STATENAME)
})

ALL_MERGED = rbind(Merged_CD_2[[1]], Merged_CD_2[[2]], Merged_CD_2[[3]],Merged_CD_2[[4]],Merged_CD_2[[5]],Merged_CD_2[[6]],
                   Merged_CD_2[[7]],Merged_CD_2[[8]],Merged_CD_2[[9]],Merged_CD_2[[10]],Merged_CD_2[[11]],Merged_CD_2[[12]],
                   Merged_CD_2[[13]],Merged_CD_2[[14]],Merged_CD_2[[15]],Merged_CD_2[[16]])


#Removing the Geometry
ALL_MERGED_NoGeo <- dplyr::select(as.data.frame(ALL_MERGED), -geometry)
write_rds(ALL_MERGED_NoGeo, "~/Desktop/ECON Thesis/ALL_MERGED.rds")

ALL_MERGED_NoGeo = ALL_MERGED_NoGeo %>%
  dplyr::select(-c(STATENAME, ID, STARTCONG,ENDCONG))





#Accessing 115th shapefiles from tigris
CD_115 = tigris::congressional_districts(resolution = "500k")
CD_115 = CD_115 %>%
  st_as_sf() %>%
  st_transform(crs = 4269)
CD_115 = select(CD_115, c(STATEFP, CD115FP, NAMELSAD, geometry))
REL_DAT = ALL[which(ALL$CONG_MERGE==115),] %>%
  st_transform(crs = 4269)
RES = st_join(REL_DAT, CD_115)
RES =  dplyr::select(as.data.frame(RES), -geometry) %>%
  filter(STATE_FIPS == STATEFP)
RES = RES %>%
  dplyr::select(-c(STATEFP, NAMELSAD)) %>%
  rename(DISTRICT = CD115FP)

  
#Bind Everything Together
ALL_MERGED_NoGeo = bind_rows(ALL_MERGED_NoGeo, RES)
ALL_MERGED_NoGeo

write_rds(ALL_MERGED_NoGeo, "~/Desktop/ECON Thesis/ALL_MERGED.rds")
