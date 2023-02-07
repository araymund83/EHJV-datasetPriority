# PCODE: MSU/MNHP
# Title: "Michigan Northern Hardwoods Project'
# Source dataset for this script are in .rds files
# Author: "Ana Raymundo"
# Date: "January 25th, 2023"
# ---Notes:
#pc_location has more sites than the unique(pc_visit$site)

# Load libraries ----------------------------------------------------------
update.packages()
library(pacman)
p_load(dplyr,glue,googledrive,ggplot2, mapview, sf, utils,purrr, stringr,
       tidyr)
#clean environment
g <- gc(reset = TRUE)
rm(list = ls())

# Initialize variables ----------------------------------------------------
wd <- getwd()
setwd(wd)

organization <- "MSU"
dataset_code <- "MNHP"
source_data <- 'all_sites_yx.rds'
source_data2 <- 'survey_data.rds'

#set working folder
project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}
data_db <- file.path(project_dir, source_data)
if (!file.exists(data_db)) {
  #Download from GoogleDrive
  drive_download(glue('sourceData/{organization}/{dataset_code}/sourceData/{source_data}'), path = data_db)
}
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

############################
#### LOCATION TABLE ####
############################
raw_location <- readRDS("E:/GitHub/EHJV-datasetPriority/project/MNHP/all_sites_xy.rds")
raw_location <- raw_location %>% select_all(tolower) #changes all column names to lowercase
names(raw_location) <- str_replace_all(names(raw_location), c(' '= '_')) # replaces spaces in colnames with _

pc_location <- raw_location %>% select(site, geometry, x_coord, y_coord, year_surve)

pc_location <- pc_location %>% mutate(latitude = map(pc_location$geometry,2) %>% unlist(),
                                      longitude = map(pc_location$geometry,1) %>% unlist())

# database CRS (WGS83/UTM ZONE 16N)
crs_data <- st_crs(pc_location) #EPSG:32616

# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)

# Transform crs to WT crs
pc_location2 <- st_transform(pc_location, crs = crs_WT)

#get how many point counts locations and the years of the survey
N_pc <- length(unique(pc_location$site))
yrs <- sort(unique(pc_location$year_surve))

############################
#### VISIT TABLE ####
############################
raw_data <- readRDS("E:/GitHub/EHJV-datasetPriority/project/MNHP/detection_data.rds")
raw_data <- raw_data %>% select_all(tolower) #changes all column names to lowercase
names(raw_data) <- str_replace_all(names(raw_data),c(' '='_'))# replaces spaces in colnames with _

pc_visit <- raw_data %>% select(site, date)
pc_visit <- tidyr::separate(pc_visit, date, c("year", "month", "day"), 
                            sep = cumsum(c(4,2,2)), remove = FALSE)


survey_summ <- left_join(pc_visit,pc_location, by= c("site"))

#get how many point counts locations and the years of the survey
N_pc <- length(unique(survey_summ$site))
yrs <- sort(unique(survey_summ$year))

# get a summary of the number of visits per year
pc_N <- pc_visit %>% group_by(year) %>% 
  summarise(total_pc = n()) %>% 
  mutate(project = dataset_code)

############################
#### MAKE MAP ####
############################
new_pc_location <- survey_summ %>% select(site, latitude, longitude)
df <- st_as_sf(new_pc_location,  crs = crs_WT)
df <- st_transform(df,  crs = crs_WT)
mapview(df, crs = crs_WT, grid = FALSE)

