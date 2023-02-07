# PCODE: BAM/RLNWR
# Title: "Rice Lake National Wildlife Refuge"
# Source dataset for this script are in .cvs files
# Author: "Ana Raymundo"
# Date: "January 25th, 2023"
# ---

# Load libraries ----------------------------------------------------------
update.packages()
library(pacman)
p_load(dplyr,glue,googledrive,ggplot2, mapview, sf, utils,purrr, RODBC, stringr,
       tidyr)


# Initialize variables ----------------------------------------------------
wd <- getwd()
setwd(wd)

organization <- "BAM"
dataset_code <- "RLNWR"
source_data <- 'Rice Lake NWR Point Count Data.csv'
source_data2 <- 'Rice Lake NWR Point Count Locations.csv'

#set working folder
project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}
data_db <- file.path(project_dir, source_data)
if (!file.exists(data_db)) {
  #Download from GoogleDrive
  drive_download(glue('sourceData/{organization}/{dataset_code}/Data/{source_data}'), path = data_db)
  drive_download(glue('sourceData/{organization}/{dataset_code}/Data/{source_data2}'), path = data_db)
}
out_dir <- file.path("./out", dataset_code)    # where output dataframe will be exported
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

############################
#### VISIT TABLE ####
############################
pc_location <- read.csv(glue('{project_dir}/Rice Lake NWR Point Count Locations.csv'))
pc_location <- pc_location %>% select_all(tolower) #changes all column names to lowercase
pc_location <- pc_location %>% select(name, short.name, latitude,longitude)


# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)

# Transform crs to WT crs
pc_location2 <- st_as_sf(pc_location, coords = c("longitude", "latitude"), remove = FALSE, crs = crs_WT)

unique(pc_location$name)
unique(pc_location$short.name)

############################
#### VISIT TABLE ####
############################
pc_raw <- read.csv(glue('{project_dir}/Rice Lake NWR Point Count Data.csv'))
pc_visit <- pc_raw %>% select_all(tolower) #changes all column names to lowercase
pc_visit <- tidyr::separate(pc_visit, date, c("month", "day", "year"), "/", remove = FALSE)

pc_visit <- pc_visit %>% select(project, study.area, transect, point, date, year) %>% 
  mutate(point = as.character(point))
##TODO: HOW TO TO CONECT BOTH TABLES WITHOUT A FIELD IN COMMUN 
survey_summ <- inner_join(pc_location, pc_visit, by= c('point', 'short.name'))

##TODO: temporal solution
pc_N <- pc_visit %>% group_by(year) %>% 
  summarise(total_pc = n()) %>% 
  mutate(project = dataset_code)
write.csv(pc_N, file= file.path(out_dir, paste0(dataset_code,"_totalPC.csv")), row.names = FALSE)


############################
#### MAKE MAP ####
############################

df <- st_as_sf(pc_location2, coords = c('longitude', 'latitude'), remove = FALSE, crs = crs_WT)

#write shapefile with no missing lat and lon
out_path <- glue ('./SIG/{dataset_code}_GIS')
ifelse(!dir.exists(out_path), dir.create(out_path), 'Folder already exists')
st_write(pc_location2, glue('{out_path}/{dataset_code}.shp'))
mapview(df, xcol = 'longitude', ycol = 'latitude', crs = crs_WT, grid = FALSE)


