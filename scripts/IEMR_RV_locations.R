#PCODE: CWS-ATL IEMR River Valley
# Title: "River Valley Ecosystems Project" #from metadata
# Source dataset for this script are in different .xls files
# Author: "Ana Raymundo"
# Date: "February 2nd, 2023"
# ---Notes:
# points projecting in the Atlantic, near Africa. Only St. Augustine River - control (site 2) 
#337484 N	5762033 E	 UTM 21 shows up in Canada  && Treatment Churchill River 566060	5862600	20
#data has two UTM zones 20 &21 no shapefile created




# Load libraries ----------------------------------------------------------
update.packages()
library(pacman)
p_load(dplyr,glue,googledrive,ggplot2, mapview, sf, utils,purrr, readxl,stringr,
       tidyr)
#clean environment
g <- gc(reset = TRUE)
rm(list = ls())

# Initialize variables ----------------------------------------------------
wd <- getwd()
setwd(wd)

organization <- "CWS-ATL"
dataset_code <- "WILDSPACE_HIST"
source_data <- "P5 I1 C1 IEMR River Valley.xlsx"
project <- 'IEMR_RV'

#set working folder
project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}
data_db <- file.path(project_dir, source_data)
if (!file.exists(data_db)) {
  #Download from GoogleDrive
  drive_download(glue('sourceData/{dataset_code}/sourceData/{source_data}'), path = data_db)
}
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

############################
## IMPORT ####
############################
source('./R/multipleSheets.R')
data_all <- multipleSheets(data_db)
names(data_all)

############################
#### LOCATION TABLE ####
############################
raw_location <- data_all [['Site Survey General']]
raw_location <- raw_location %>% select_all(tolower) #changes all column names to lowercase
names(raw_location) <- str_replace_all(names(raw_location),c(' '='_'))# replaces spaces in colnames with _
raw_location<- raw_location[-1,]

pc_location <- raw_location %>%  dplyr::select('site_survey_general_information', '...2','...3', '...23', '...24', '...25') %>% 
  rename('siteid' = 'site_survey_general_information', 'location' = '...2', 'date' = '...3' , 'northing' ='...23', 
         'easting' = '...24', 'UTMzone' = '...25')
#remove two first rows
pc_location <- pc_location[-c(1:3),]

# fix the problem of the date 
pc_location$date <- as.numeric(pc_location$date)
pc_location <- pc_location %>% mutate(date = as.Date(date, origin = '1899-12-30'),
                                      UTMzone = as.numeric(UTMzone)) %>% 
  tidyr::separate('date', c("year", "month", "day"), "-", remove = FALSE) 
#get how many point counts locations and the years of the survey
N_pc <- length(unique(pc_location$location))
yrs <- sort(unique(pc_location$year))

############################
#### VISIT TABLE ####
############################
raw_visit <- data_all [['Site Survey Obs']]
raw_visit <- raw_visit %>% select_all(tolower) #changes all column names to lowercase
names(raw_visit) <- str_replace_all(names(raw_visit),c(' '='_'))# replaces spaces in colnames with _
raw_visit<- raw_visit[-c(1:3),]

pc_visit <- raw_visit %>%  dplyr::select("...2",'...3', '...6', '...7') %>% 
  rename('location' = '...2', 'date' = '...3' , 'species' ='...6', 
         'abundance' = '...7')

# fix the problem of the date 
pc_visit <- pc_visit %>% mutate(date = as.Date(as.numeric(date), origin = '1899-12-30')) %>% 
  tidyr::separate('date', c("year", "month", "day"), "-", remove = FALSE) 
#get how many point counts locations and the years of the survey
N_pc <- length(unique(pc_visit$location))
yrs <- sort(unique(pc_location$year))

survey_summ <- left_join(pc_location, pc_visit, by = c('location', 'date', 'year', 'month', 'day'))

N_pc <- length(unique(survey_summ$location))
yrs <- sort(unique(pc_visit$year))

# get a summary of the number of visits per year
pc_N <- survey_summ %>% group_by(year) %>% 
  summarise(total_pc = n()) %>% 
  mutate(project = dataset_code)

############################
#### MAKE MAP ####
############################
## database CRS (NAD83, UTM ZONES 20 & 21, EPSG: 26920 & 26921)
crs_db20 <- st_crs(26920)
crs_db21 <- st_crs(26921)
# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)
pc_location2 <- pc_location %>%  filter(UTMzone == 20)
pc_location20 <- st_as_sf(pc_location2, coords = c('easting', "northing"), remove = FALSE, crs = crs_db21)
df <- st_transform(pc_location20,crs = crs_WT)
#write shapefile with no missing lat and lon
out_path <- glue ('./SIG/{dataset_code}_GIS/{project}')
ifelse(!dir.exists(out_path), dir.create(out_path), 'Folder already exists')
st_write(df, glue('{out_path}/{project}_UTM20.shp'))
#visualize map
mapview(df, xcol = 'northing', ycol = 'easting', crs = crs_WT, grid = FALSE)
mapview(pc_location20, xcol = 'northing', ycol = 'easting', crs = crs_WT, grid = FALSE)
