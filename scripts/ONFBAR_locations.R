# PCODE: Birds Canada/ONFBAR
# Title: "Ontario Forest Bird at Risk 2019-2022'
# Source dataset for this script are in a .xls file
# Author: "Ana Raymundo"
# Date: "January 27th, 2023"
# ---Notes:
#No latitude and longitude information provided

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
dataset_code <- "ONFBAR"
source_data <- 'all_FBAR_PC_data_(2019-present).xlsx'


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
raw_data <- read_excel(glue('{project_dir}/{source_data}'))
raw_data <- raw_data %>% select_all(tolower) #changes all column names to lowercase
names(raw_data) <- str_replace_all(names(raw_data),c(' '='_'))# replaces spaces in colnames with _

pc_location <- raw_data %>% select(site_id,point_id, year) %>% 
  mutate(location = glue('{site_id}:{point_id}'))

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
#No latitude and longitude information provided