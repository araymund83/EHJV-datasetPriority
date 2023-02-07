# PCODE: SNWR-PC-Crozier
# Title: "Seney National Wildlife Refuge, Upper Peninsula of Michigan"
# Source dataset for this script are in a .xls file
# Author: "Ana Raymundo"
# Date: "February 1st, 2023"
# ---Notes:
# if we consider each data set independently we have 294 unique pointcounts for 1997 
#and 284 for 1998 if we merge both data sets there are only 262. 
# shapefile missing crs and projecting in Venezuela!! ASK MELINA ABOUT THIS!!


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

organization <- "SNWR-PC-Crozier"
dataset_code <- "SNWR-PC-Crozier"
source_data <- 'Bird98.xls'

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
raw_data98 <- read_excel(glue('{project_dir}/{source_data}'))
raw_data97 <- read_excel(glue('{project_dir}/BIRD97.XLS'))
#tidy dataset
raw_data98 <- raw_data98 %>% select_all(tolower) #changes all column names to lowercase
names(raw_data98) <- str_replace_all(names(raw_data98),c(' '='_'))# replaces spaces in colnames with _
pc_location98 <- raw_data98 %>% mutate(year = 1998)

raw_data97 <- raw_data97 %>% select_all(tolower) #changes all column names to lowercase
names(raw_data97) <- str_replace_all(names(raw_data97),c(' '='_'))# replaces spaces in colnames with _
pc_location97 <- raw_data97 %>% mutate(year = 1997)
#filter the unique station
pc_location<- pc_location %>% distinct(site, .keep_all = TRUE)
pc_location97<- raw_data97 %>% distinct(site, .keep_all = TRUE)

#merge both data sets 
pc_location<- merge(pc_location97, pc_location98, by= 'site')


#get how many point counts locations and the years of the survey
N_pc <- length(unique(pc_location$site))
N_pc <- pc_location %>% distinct(site, .keep_all = TRUE)

# get a summary of the number of visits per year
pc_N <- pc_location %>% group_by(site) %>% 
  summarise(total_pc = n_distinct()) %>% 
  mutate(project = dataset_code)


############################
#### MAKE MAP ####
############################

# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)
#read the original shapefile 
snwr_shp <- st_read(glue('./SIG/{dataset_code}_GIS/realpnts.shp'))
df <- st_as_sf(snwr_shp,  coords = c('longitude', 'latitude'), crs = crs_WT) 
df<- st_set_crs(snwr_shp, crs_WT)
df <- st_transform(df,  crs = crs_WT) 
#write shapefile with no missing lat and lon
out_path <- glue ('./SIG/{dataset_code}_GIS')
ifelse(!dir.exists(out_path), dir.create(out_path), 'Folder already exists')
st_write(df, glue('{out_path}/{dataset_code}.shp'))

mapview(df, xcol = 'longitude', ycol = 'latitude', crs = crs_WT, grid = FALSE)
