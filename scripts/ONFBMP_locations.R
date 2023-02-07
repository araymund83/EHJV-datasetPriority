# PCODE: Birds Canada/ONFBMP
# Title: "Ontario Forest Bird Monitoring Program 2020-2022"
# Source dataset for this script are in a .csv file
# Author: "Ana Raymundo"
# Date: "January 31st, 2023"
# ---Notes:
## 55 locations were missing latitude and longitude, these points were deleted from 
##sf file.
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

organization <- "Birds Canada"
dataset_code <- "ONFBMP"
source_data <- 'FBMP_BirdsCanadaData.csv'


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
raw_data <- read.csv(glue('{project_dir}/{source_data}'))
raw_data <- raw_data %>% select_all(tolower) #changes all column names to lowercase
names(raw_data) <- str_replace_all(names(raw_data),c(' '='_'))# replaces spaces in colnames with _

pc_location <- raw_data %>% select(siteid,stationid, date, latitude, longitude) %>% 
  mutate(location = glue('{siteid}:{stationid}')) %>% 
  separate(date, c("day", "month", "year"), "/", remove = FALSE)

# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)


#get how many point counts locations and the years of the survey
N_pc <- length(unique(pc_location$location))
yrs <- sort(unique(pc_location$year))


# get a summary of the number of visits per year
pc_N <- pc_location %>% group_by(year) %>% 
  summarise(total_pc = n()) %>% 
  mutate(project = dataset_code)

write.csv(pc_N, file= file.path(out_dir, paste0(dataset_code,"_totalPC.csv")), row.names = FALSE)

############################
#### MAKE MAP ####
############################
pc_location2<- pc_location %>% select(siteid, location, latitude, longitude, year) %>%  drop_na() # 
df <- st_as_sf(pc_location2,  coords = c('longitude', 'latitude'), crs = crs_WT)

#write shapefile with no missing lat and lon
out_path <- glue ('./SIG/{dataset_code}_GIS')
ifelse(!dir.exists(out_path), dir.create(out_path), 'Folder already exists')
st_write(df, glue('{out_path}/{dataset_code}.shp'))
mapview(df, xcol = 'longitude', ycol = 'latitude', crs = crs_WT, grid = FALSE)

#get how many point counts locations and the years of the survey
N_pc <- length(unique(pc_location2$location))
yrs <- sort(unique(pc_location2$year))

pc_N <- pc_location2 %>% group_by(year) %>% 
  summarise(total_pc = n()) %>% 
  mutate(project = dataset_code)
