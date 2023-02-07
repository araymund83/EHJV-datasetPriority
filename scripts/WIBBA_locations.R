# PCODE: BAM/WIBBA
# Title: "Wisconsin Breeding Bird Atlas II"
# Source dataset for this script uses accdb format
# Author: "Ana Raymundo"
# Date: "January 23th, 2023"
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
dataset_code <- "WIBBA"
source_data <- 'WIBBA.accdb'

#set working folder
project_dir <- file.path(wd, "project", dataset_code)
if (!dir.exists(project_dir)) {
  dir.create(project_dir)
}
data_db <- file.path(project_dir, source_data)
if (!file.exists(data_db)) {
  #Download from GoogleDrive
  drive_download(glue('sourceData/{organization}/{dataset_code}/HedwigSharlenesWork/{source_data}'), path = data_db)
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
pc_location <- sqlFetch(con, "Locations")


pc_location <- pc_location %>% select_all(tolower) #changes all column names to lowercase
names(pc_location) <- gsub(' ', '_', names(pc_location)) # replaces spaces in colnames with _

# Data CRS: WGS84
crs_data <- st_crs(4269) ## From PointCounterHandbook2019final.pdf : Map Datum should be NAD83
# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)

# Transform crs to WT crs
pc_location2 <- st_as_sf(pc_location, coords = c("longitude", "latitude"), remove = FALSE, crs = crs_WT)


############################
#### VISIT TABLE ####
############################
pc_visit <- sqlFetch(con, "Visits")#Fix column names that have space

pc_visit <- pc_visit %>% select_all(tolower) #changes all column names to lowercase
names(pc_visit) <- gsub(' ', '_', names(pc_visit)) # replaces spaces in colnames with _

## visitDate
pc_visit$visitDate <- as.character(format(pc_visit$survey_date, format = "%Y-%m-%d"))
pc_visit <- pc_visit %>% select(location_name, pointid, surveydate) 
pc_visit<- tidyr::separate(pc_visit,surveydate, c("year", "month", "day"), "-", remove = FALSE) %>% 
  select(location_name,pointid, year)


survey_summ <- left_join(pc_location, pc_visit, by = c("location_name", 'pointid')) %>% 
  select(location_name, pointid, year)

pc_N <- survey_summ %>% group_by(year) %>% 
  summarise(total_pc = n()) %>% 
  mutate(project = dataset_code)

write.csv(pc_N, file= file.path(out_dir, paste0(dataset_code,"_totalPC.csv")), row.names = FALSE)

############################
#### MAKE MAP ####
############################
df <- st_as_sf(pc_location2, coords = c('longitude', 'latitude'), remove = FALSE, crs = crs_WT)
mapview(df, xcol = 'longitude', ycol = 'latitude', crs = crs_WT, grid = FALSE)

#still testing
##make_map(df = pc_location, targetCRS = crs_WT)
