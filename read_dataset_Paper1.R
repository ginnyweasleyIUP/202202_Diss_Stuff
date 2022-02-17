#################################################
## 1 Read in Dataset ############################

#choose your own working directory here
wd <- getwd()
setwd(wd)

filepath_data = "/home/ldap-server/ginnyweasley/07_R_Code/iHadCM3LastMill/Data/"

# read in Data 
# please provide all Data in a directory names "Data"

DATA_past1000 <- list()
DATA_past1000$CAVES <- list()
DATA_past1000$CAVES$entity_info <- read.csv(paste0(filepath_data,"SISAL_HadCM3_entity_info.csv"))

DATA_past1000$CAVES$entity_info$elevation <- as.numeric(DATA_past1000$CAVES$entity_info$elevation)
DATA_past1000$CAVES$entity_info$geology <- as.character(DATA_past1000$CAVES$entity_info$geology)
DATA_past1000$CAVES$entity_info$mineralogy <- as.character(DATA_past1000$CAVES$entity_info$mineralogy)
DATA_past1000$CAVES$entity_info$distance_entrance <- as.numeric(levels(DATA_past1000$CAVES$entity_info$distance_entrance))[DATA_past1000$CAVES$entity_info$distance_entrance]
DATA_past1000$CAVES$entity_info$cover_thickness <- as.numeric(levels(DATA_past1000$CAVES$entity_info$cover_thickness))[DATA_past1000$CAVES$entity_info$cover_thickness]

DATA_past1000$CAVES$record_res <- read.csv(paste0(filepath_data,"SISAL_HadCM3_ds.csv"))
DATA_past1000$CAVES$yearly_res <- list()
DATA_past1000$CAVES$yearly_res$a <- read.csv(paste0(filepath_data,"SISAL_HadCM3_xnapa_PMIL_yearly.csv"))
DATA_past1000$CAVES$yearly_res$b <- read.csv(paste0(filepath_data,"SISAL_HadCM3_xnapb_PMIL_yearly.csv"))
DATA_past1000$CAVES$yearly_res$c <- read.csv(paste0(filepath_data,"SISAL_HadCM3_xnapc_PMIL_yearly.csv"))
DATA_past1000$CAVES$season_res <- list()
DATA_past1000$CAVES$season_res$a <- list(
  WINTER = read.csv(paste0(filepath_data,"Seasonal/SISAL_HadCM3_xnapa_PMIL_WINTER.csv")),
  SPRING = read.csv(paste0(filepath_data,"Seasonal/SISAL_HadCM3_xnapa_PMIL_SPRING.csv")),
  SUMMER = read.csv(paste0(filepath_data,"Seasonal/SISAL_HadCM3_xnapa_PMIL_SUMMER.csv")),
  AUTUMN = read.csv(paste0(filepath_data,"Seasonal/SISAL_HadCM3_xnapa_PMIL_AUTUMN.csv"))
)
DATA_past1000$CAVES$season_res$b <- list(
  WINTER = read.csv(paste0(filepath_data,"Seasonal/SISAL_HadCM3_xnapb_PMIL_WINTER.csv")),
  SPRING = read.csv(paste0(filepath_data,"Seasonal/SISAL_HadCM3_xnapb_PMIL_SPRING.csv")),
  SUMMER = read.csv(paste0(filepath_data,"Seasonal/SISAL_HadCM3_xnapb_PMIL_SUMMER.csv")),
  AUTUMN = read.csv(paste0(filepath_data,"Seasonal/SISAL_HadCM3_xnapb_PMIL_AUTUMN.csv"))
)
DATA_past1000$CAVES$season_res$c <- list(
  WINTER = read.csv(paste0(filepath_data,"Seasonal/SISAL_HadCM3_xnapc_PMIL_WINTER.csv")),
  SPRING = read.csv(paste0(filepath_data,"Seasonal/SISAL_HadCM3_xnapc_PMIL_SPRING.csv")),
  SUMMER = read.csv(paste0(filepath_data,"Seasonal/SISAL_HadCM3_xnapc_PMIL_SUMMER.csv")),
  AUTUMN = read.csv(paste0(filepath_data,"Seasonal/SISAL_HadCM3_xnapc_PMIL_AUTUMN.csv"))
)

library(plyr)
library(dplyr)
library(tidyverse)
library(maps)

# 0.1: Masks

# Prepare masks for different analyses 
mask_mean = logical(length = length(DATA_past1000$CAVES$entity_info$entity_id))
mask_var  = logical(length = length(DATA_past1000$CAVES$entity_info$entity_id))
mask_spec = logical(length = length(DATA_past1000$CAVES$entity_info$entity_id))

for(ii in 1:length(DATA_past1000$CAVES$entity_info$entity_id)){
  entity = DATA_past1000$CAVES$entity_info$entity_id[ii]
  data = DATA_past1000$CAVES$record_res %>% filter(entity_id == entity)
  if(length(data$d18O_measurement) >= 10){mask_mean[ii] = T}
  if(length(data$d18O_measurement) >= 20){mask_var[ii] = T}
  if(length(data$d18O_measurement) >= 30){mask_spec[ii] = T}
}

# manually turn off mixed mineralogies from the analysis. You can include the 7 entities, by skipping this step. 
mask_mean[DATA_past1000$CAVES$entity_info$mineralogy == "mixed"] = F
mask_var[DATA_past1000$CAVES$entity_info$mineralogy == "mixed"] = F 
mask_spec[DATA_past1000$CAVES$entity_info$mineralogy == "mixed"] = F

rm(entity, data, ii)

# 0.2: Cluster

# distance based-clustering
dist<-fossil::earth.dist(cbind(DATA_past1000$CAVES$entity_info$latitude[mask_spec],DATA_past1000$CAVES$entity_info$longitude[mask_spec]),dist=TRUE)
hc<-hclust(dist)
DATA_past1000$CAVES$cluster_list <- list(entity_id = as.numeric(DATA_past1000$CAVES$entity_info$entity_id[mask_spec]), cluster_id = as.numeric(cutree(hc,k=8)))

# manually sort cluster = 9 in south east asia. This is e_ID: 226, 238, 319, 335, 367, 399, 436, 523
for(entity in c(226, 238, 319, 335, 367, 399, 436, 523)){
  if(entity %in% DATA_past1000$CAVES$cluster_list$entity_id){
    DATA_past1000$CAVES$cluster_list$cluster_id[which(DATA_past1000$CAVES$cluster_list$entity_id == entity)] = 9
  }
}

rm(dist, hc, entity)

# 0.3 Gridbox

## Gridbox list

# sorting entities into gridboxes
DATA_past1000$CAVES$gridbox_list <- list(entity_id = as.numeric(DATA_past1000$CAVES$entity_info$entity_id[mask_spec]), gridbox_id = numeric(sum(mask_spec)))

for(ii in 1:sum(mask_spec)){
  e.long = DATA_past1000$CAVES$entity_info$longitude[mask_spec][ii]
  e.lat = DATA_past1000$CAVES$entity_info$latitude[mask_spec][ii]
  DATA_past1000$CAVES$gridbox_list$gridbox_id[ii]  = (ceiling((-1*e.lat+90)/180*73)-1)*96 + ceiling((e.long+180)/3.75)
}
rm(e.lat, e.long, ii)

#################################################
# set width and height for output plots
PLOTTING_VARIABLES <- list()
PLOTTING_VARIABLES$WIDTH = 8.3
PLOTTING_VARIABLES$HEIGHT = 5.5

# if not existing - create a output directroy for the plots included in the Paper
if(!dir.exists("Paper_Plots")){
  dir.create("Paper_Plots")
}
