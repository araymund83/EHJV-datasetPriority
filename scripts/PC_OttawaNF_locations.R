# PCODE: PC_OttawaNF
# Title: "Ottawa National Forest Point count surveys"
# Source dataset for this script are in a .accdb file
# Author: "Ana Raymundo"
# Date: "February 1st, 2023"
# ---Notes:


# Load libraries ----------------------------------------------------------
update.packages()
library(pacman)
p_load(dplyr,glue,googledrive,ggplot2, mapview, sf, utils,purrr, RODBC,stringr,
       tidyr)
#clean environment
g <- gc(reset = TRUE)
rm(list = ls())

# Initialize variables ----------------------------------------------------
wd <- getwd()
setwd(wd)

organization <- "PC_OttawaNF"
dataset_code <- "PC_OttawaNF"
source_data <- 'OttawaNF.accdb'

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
#######################################################
##                    Connect
#######################################################
#Connect and load tables
con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", data_db))
#--------------------------------------------------------------
#queries<- sqlQuery(project, "BAM-V6")  ##Look up any queries
tbls <- sqlTables(con) ##Look up tables
tbls$TABLE_NAME

############################
#### LOCATION TABLE ####
############################
raw_location <- sqlFetch(con, "Locations")

raw_location <- raw_location %>% select_all(tolower) #changes all column names to lowercase
names(raw_location) <- str_replace_all(names(raw_location),c(' '='_'))# replaces spaces in colnames with _

pc_location <- raw_location  %>%  select(locatio_name, short_name, latitude, longitude) %>% 
  rename('location' = 'locatio_name')

#get how many point counts locations and the years of the survey
N_pc <- length(unique(pc_location$location))

############################
#### VISIT TABLE ####
############################
raw_visit <- sqlFetch(con, "PC_Visit")
raw_visit <- raw_visit %>% select_all(tolower) #changes all column names to lowercase
names(raw_visit) <- str_replace_all(names(raw_visit),c(' '='_'))# replaces spaces in colnames with _

pc_visit <- raw_visit %>% select(location_name, date)
pc_visit <- tidyr::separate(pc_visit, date, c("year", "month", "day"), "-", remove = FALSE) %>% 
  rename('location' = 'location_name')

N_pc <- length(unique(pc_visit$short.name))
yrs <- sort(unique(pc_visit$year))

survey_summ <- left_join(pc_location, pc_visit, by = 'location') 

N_pc <- length(unique(survey_summ$location))
yrs <- sort(unique(pc_visit$year))

# get a summary of the number of visits per year
pc_N <- survey_summ %>% group_by(year) %>% 
  summarise(total_pc = n()) %>% 
  mutate(project = dataset_code)

############################
#### MAKE MAP ####
############################

# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)
pc_location2 <- st_as_sf(pc_location, coords = c("longitude", "latitude"), remove = FALSE, crs = crs_WT)
df <- st_as_sf(pc_location2, coords = c('longitude', 'latitude'), remove = FALSE, crs = crs_WT)
mapview(df, xcol = 'longitude', ycol = 'latitude', crs = crs_WT, grid = FALSE)


