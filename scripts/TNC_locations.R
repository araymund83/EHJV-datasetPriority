# PCODE: BAM/TNC
# Title: "Two Hearted River Forest System"
# Source dataset for this script uses accdb format
# Author: "Ana Raymundo"
# Date: "January 25th, 2023"
# ---

update.packages()
library(pacman)
p_load(dplyr,glue,googledrive,ggplot2, mapview, sf, utils,purrr, RODBC, stringr,
       tidyr)


## Initialize variables
wd <- getwd()
setwd(wd)

organization <- "BAM"
dataset_code <- "TNC"
source_data <- 'TwoHeartedRiver_BirdCensus_Working.accdb'

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


pc_location <- pc_location %>% select_all(tolower) %>%   #changes all column names to lowercase
  select(`location-name`, latdd, longdd) %>%  
  rename('longitude' = 'longdd',
         'latitude' = 'latdd') %>% tidyr::drop_na()



# WildTrax CRS (EPSG: 4386)
crs_WT <- st_crs(4386)

# Transform crs to WT crs
pc_location2 <- st_as_sf(pc_location, coords = c("longitude", "latitude"), remove = FALSE, crs = crs_WT)

############################
#### VISIT TABLE ####
############################
pc_visit <- sqlFetch(con, "PC_Visit")
pc_visit <- pc_visit %>% select_all(tolower) #changes all column names to lowercase

## visitDate
pc_visit$visitDate <- as.character(format(pc_visit$date, format = "%Y-%m-%d"))
pc_visit <- pc_visit %>% select(`location-name`, year) 


survey_summ <- left_join(pc_location, pc_visit, by = 'location-name') %>% 
  select(`location-name`, year)

N_pc <- length(unique(pc_location$`location-name`))
years <- unique(pc_visit$year)
               
#this produces an output similar to table
pc_N <- survey_summ %>% group_by(`location-name`, year) %>% 
  summarise(count = n()) %>% pivot_wider(id_cols = `location-name`, names_from = 'year',
                                 values_from = 'count')

colSums(pc_N[2:4], na.rm = TRUE)
                      
write.csv(pc_N, file= file.path(out_dir, paste0(dataset_code,"_totalPC.csv")), row.names = FALSE)

############################
#### MAKE MAP ####
############################
df <- st_as_sf(pc_location2, coords = c('longitude', 'latitude'), remove = FALSE, crs = crs_WT)
mapview(df, xcol = 'longitude', ycol = 'latitude', crs = crs_WT, grid = FALSE)
