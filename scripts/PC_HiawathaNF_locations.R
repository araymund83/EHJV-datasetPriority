# PCODE: PC_HiawathaNF
# Title: "Hiawatha Black-Throated, Blue Warbler - Balsam Fir Relationship"
# Source dataset for this script are in a .xls file
# Author: "Ana Raymundo"
# Date: "February 1st, 2023"
# ---Notes:


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

organization <- "PC_HiawathaNF"
dataset_code <- "PC_HiawathaNF"
source_data <- 'Hiawatha National Forest Sampling Units_lklocation.csv'

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
#### LOCATION TABLE ####
############################
raw_location <- read.csv(glue('{project_dir}/{source_data}'))
raw_location <- raw_location %>% select_all(tolower) #changes all column names to lowercase
names(raw_location) <- str_replace_all(names(raw_location),c(' '='_'))# replaces spaces in colnames with _

pc_location <- raw_location %>% filter(types == 'Point Count Point') %>% 
  select(name, short.name, latitude, longitude) %>% rename(location = name)

#get how many point counts locations and the years of the survey
N_pc <- length(unique(pc_location$location))



############################
#### VISIT TABLE ####
############################
raw_visit <- read.csv(glue('{project_dir}/Hiawatha National Forest Point Count Data.csv'))
#remove the first row that is empty 
raw_visit <- raw_visit[-1,]
raw_visit <- raw_visit %>% select_all(tolower) #changes all column names to lowercase
names(raw_visit) <- str_replace_all(names(raw_visit),c(' '='_'))# replaces spaces in colnames with _

pc_visit <- raw_visit %>% select(transect, point, date)
pc_visit <- tidyr::separate(pc_visit, date, c("year", "month", "day"), "-", remove = FALSE)
pc_visit <- pc_visit %>% select(transect, point, year) %>% 
  rename(short.name = 'point')

N_pc <- length(unique(pc_visit$short.name))
yrs <- sort(unique(pc_visit$year))

survey_summ <- left_join(pc_location, pc_visit, by = 'short.name') 

N_pc <- length(unique(survey_summ$short.name))
yrs <- sort(unique(pc_visit$year))

# get a summary of the number of visits per year
pc_N <- survey_summ %>% group_by(year) %>% 
  summarise(total_pc = n()) %>% 
  mutate(project = dataset_code)

############################
#### MAKE MAP ####
############################
# data CRS (datum: NAD1927, projection: UTM zone 16N, EPSG:26716)
# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)

# Transform crs to WT crs
pc_location2 <- st_as_sf(pc_location, coords = c("longitude", "latitude"), crs = 26716)
df <- st_transform(pc_location2, crs = crs_WT)
#write shapefile with no missing lat and lon
out_path <- glue ('./SIG/{dataset_code}_GIS')
ifelse(!dir.exists(out_path), dir.create(out_path), 'Folder already exists')
st_write(df, glue('{out_path}/{dataset_code}.shp'))

mapview(df, xcol = 'longitude', ycol = 'latitude', crs = crs_WT, grid = FALSE)
