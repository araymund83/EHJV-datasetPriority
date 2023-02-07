# PCODE: CWS-ATL
# Title: "DCC # IE090214; Commission 2.2.5.8 2011 Common Nighthawk Surveys 5 Wing Goose Bay" # from metadata
# Source dataset for this script are in different .xls files
# Author: "Ana Raymundo"
# Date: "February 2nd, 2023"
# ---Notes:
#fix date problem
#survey sheet does (Site Survey Obs) not have many observations in species but there is
#another column (other species seen with some species names)


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
source_data <- "P1 I1 C1 Common Nighthawk.xls"
project <- 'commonNighthawk'

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
raw_location <- raw_location %>% select_all(tolower) #changes all column names to lowercase
names(raw_location) <- str_replace_all(names(raw_location),c(' '='_'))# replaces spaces in colnames with _
raw_location<- raw_location[-1,]

pc_location <-raw_location %>%  dplyr::select("sites",'...9', '...10', '...11') %>% 
  rename('location' = 'sites', 'date' = '...9' , 'northing' ='...10', 
         'easting' = '...11')
#remove first two rows
pc_location <- pc_location[-c(1:2),]
#check the unique values of date 
dates <- unique(pc_location$date)
# fix the problem of the date 
pc_location<- pc_location %>% mutate(date = as.Date(40707, origin = '1899-12-30')) %>% 
  tidyr::separate('date', c("year", "month", "day"), "-", remove = FALSE) 
#get how many point counts locations and the years of the survey
N_pc <- length(unique(pc_location$location))
yrs <- sort(unique(pc_location$year))


############################
#### MAKE MAP ####
############################
## database CRS (NAD83, UTM ZONE 20, EPSG: 26920)
crs_db <- st_crs(26920)
# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)
pc_location2 <- st_as_sf(pc_location, coords = c('northing', "easting"), remove = FALSE, crs = crs_db)
df <- st_transform(pc_location2,crs = crs_WT)
#write shapefile with no missing lat and lon
out_path <- glue ('./SIG/{dataset_code}_GIS/{project}')
ifelse(!dir.exists(out_path), dir.create(out_path), 'Folder already exists')
st_write(df, glue('{out_path}/{project}.shp'))
#visualize map
mapview(df, xcol = 'northing', ycol = 'easting', crs = crs_WT, grid = FALSE)


