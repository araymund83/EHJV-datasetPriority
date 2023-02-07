# PCODE: Independent-Torrenta/ON-SRHM
# Title: "Species richness habitat modelling in Eastern Ontario"
# Source dataset for this script are in a .ZIP file containing
# Author: "Ana Raymundo"
# Date: "January 31st, 2023"
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

organization <- "Independent-Torrenta"
dataset_code <- "ON-SRHM"
source_data <- 'Remi Torrenta E Ontario Forest Fragments.zip'

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
raw_data <- unzip(glue('{project_dir}/{source_data}'), exdir = project_dir)
raw_data <- read.csv(glue('{project_dir}/Torrenta_CountsVisitsMerged.csv'))

raw_data <- raw_data %>% select_all(tolower) #changes all column names to lowercase
names(raw_data) <- str_replace_all(names(raw_data),c(' '='_'))# replaces spaces in colnames with _

pc_location <- raw_data %>% select(station, year, longitude, latitude) 
#filter the unique station
pc_location2<- pc_location %>% distinct(station, .keep_all = TRUE)

#get how many point counts locations and the years of the survey
N_pc <- length(unique(pc_location$station))
yrs <- sort(unique(pc_location$year))

# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)

# get a summary of the number of visits per year
pc_N <- pc_location %>% group_by(year) %>% 
  summarise(total_pc = n()) %>% 
  mutate(project = dataset_code)

write.csv(pc_N, file= file.path(out_dir, paste0(dataset_code,"_totalPC.csv")), row.names = FALSE)

############################
#### MAKE MAP ####
############################
df <- st_as_sf(pc_location2,  coords = c('longitude', 'latitude'), crs = crs_WT)

#write shapefile with no missing lat and lon
out_path <- glue ('./SIG/{dataset_code}_GIS')
ifelse(!dir.exists(out_path), dir.create(out_path), 'Folder already exists')
st_write(df, glue('{out_path}/{dataset_code}.shp'))
mapview(df, xcol = 'longitude', ycol = 'latitude', crs = crs_WT, grid = FALSE)

