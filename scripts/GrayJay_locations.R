#PCODE: GrayJay
# Title: "Nesting behavior of Gray Jays (Perisoreus Canadensis) during military aircraft noise events"
# Source dataset for this script are in different .xls files
# Author: "Ana Raymundo"
# Date: "February 3nd, 2023"
# ---Notes:
# 

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
source_data <- "P14 I1 C1 Gray Jay.xls"
project <- 'GrayJay'

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
raw_location <- data_all [['Sites']]
#make colnames base on row values
names(raw_location) <- raw_location[3,]
names(raw_location) <- str_replace_all(names(raw_location),c(' '='_'))# replaces spaces in colnames with _
raw_location <- raw_location %>% select_all(tolower) #changes all column names to lowercase
raw_location<- raw_location[-c(1:3),] #remove rows with repated colnames

pc_location <- raw_location %>%  dplyr::select('site_id', 'date_established?',
                                               'northing','easting') %>% 
  rename('date' = 'date_established?', 
         'location'= 'site_id') %>% 
# fix the problem of the date 
mutate(date = as.Date(as.numeric(date), origin = '1899-12-30')) %>% 
  tidyr::separate('date', c("year", "month", "day"), "-", remove = FALSE) 
#get how many point counts locations and the years of the survey
N_pc <- length(unique(pc_location$location))
yrs <- sort(unique(pc_location$year))


############################
#### VISIT TABLE ####
############################
raw_visit <- data_all [['Site Survey Obs']]
#make colnames base on row values
names(raw_visit) <- raw_visit[3,]
raw_visit <- raw_visit %>% select_all(tolower) #changes all column names to lowercase
names(raw_visit) <- str_replace_all(names(raw_visit),c(' '='_'))# replaces spaces in colnames with _
raw_visit<- raw_visit[-c(1:3),] 
pc_visit <- raw_visit %>%  dplyr::select('site_id','station_id', 'date_________(yyyy-mm-dd)',
                                         'species', 'count', 'lat', 'long') %>% 
  rename('date' = 'date_________(yyyy-mm-dd)' , 
         'abundance' = 'count', 
         'location' = 'site_id') %>% 
  mutate(date = as.Date(as.numeric(date), origin = '1899-12-30')) %>%
  tidyr::separate('date', c("year", "month", "day"), "-", remove = FALSE) 

#get how many point counts locations and the years of the survey
N_pc <- length(unique(pc_visit$location))
yrs <- sort(unique(pc_visit$year))

survey_summ <- left_join(pc_location, pc_visit, by = c('location', 'date', 
                                                       'year','month', 'day'))

N_pc <- length(unique(survey_summ$location))
yrs <- sort(unique(survey_summ$year))

# get a summary of the number of visits per year
pc_N <- survey_summ %>% group_by(year) %>% 
  summarise(total_pc = n()) %>% 
  mutate(project = dataset_code)

############################
#### MAKE MAP ####
############################
# database CRS (NAD83, UTM ZONE 20, EPSG: 26920)
crs_db <- st_crs(26920)
# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)
pc_location2 <- st_as_sf(pc_location, coords = c('easting', "northing"), remove = FALSE, crs = crs_db)
pc_location2 <- st_set_crs(pc_location2, crs_db)
df <- st_transform(pc_location2,crs = crs_WT)
#write shapefile with no missing lat and lon
out_path <- glue ('./SIG/{dataset_code}_GIS/{project}')
ifelse(!dir.exists(out_path), dir.create(out_path), 'Folder already exists')
st_write(df, glue('{out_path}/{project}.shp'))
#visualize map
mapview(df, xcol = 'northing', ycol = 'easting', crs = crs_WT, grid = FALSE)
mapview(pc_location2, xcol = 'northing', ycol = 'easting', crs = crs_WT, grid = FALSE)
