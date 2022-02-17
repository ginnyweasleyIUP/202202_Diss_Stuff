library(plyr)
library(dplyr)
library(tidyverse)
source("../202009_SISAL1k/Functions/STACYmap_PMIL.R")
source("../iHadCM3LastMill/Functions/projection_ptlyr.R")

#SISAL Plot
filepath_data = "/home/ldap-server/ginnyweasley/07_R_Code/iHadCM3LastMill/Data/"
ENTITY_INFO_1 <- read.csv(paste0(filepath_data,"SISAL_HadCM3_entity_info.csv"))
DATA_1 <- read.csv(paste0(filepath_data,"SISAL_HadCM3_ds.csv"))


# Prepare masks for different analyses 
mask_mean = logical(length = length(ENTITY_INFO_1$entity_id))
mask_var  = logical(length = length(ENTITY_INFO_1$entity_id))
mask_spec = logical(length = length(ENTITY_INFO_1$entity_id))

for(ii in 1:length(ENTITY_INFO_1$entity_id)){
  entity = ENTITY_INFO_1$entity_id[ii]
  data = DATA_1 %>% filter(entity_id == entity)
  if(length(data$d18O_measurement) >= 10){mask_mean[ii] = T}
  if(length(data$d18O_measurement) >= 20){mask_var[ii] = T}
  if(length(data$d18O_measurement) >= 30){mask_spec[ii] = T}
}

ENTITY_INFO_2 <- read.csv("Data/SISAL1k_entity_info.csv")
DATA_2 <- read.csv("../202109_SISAL1k/Data/SISAL1k_ds_HadCM3.csv")


mask_MMM = logical(length = length(ENTITY_INFO_1$entity_id))
mask_MMMC = logical(length = length(ENTITY_INFO_1$entity_id))

for(ii in 1:length(ENTITY_INFO_1$entity_id)){
  entity = ENTITY_INFO_1$entity_id[ii]
  if(entity %in% ENTITY_INFO_2$entity_id){
    mask_MMM[ii] = T
    data_rec <- DATA_2 %>% filter(entity_id == entity)
    if(sum(is.na(data_rec$d13C_measurement))<2){
      mask_MMMC[ii] = T
    }
  }
}

ALL_SITES <- read.csv("../../01_DATA/SISAL_v2/site.csv")
sites_spec <- ENTITY_INFO_1$site_id[mask_spec]
sites_var  <- ENTITY_INFO_1$site_id[mask_var]
sites_mean <- ENTITY_INFO_1$site_id[mask_mean]
sites_MMM <- ENTITY_INFO_1$site_id[mask_MMM]
sites_MMMC <- ENTITY_INFO_1$site_id[mask_MMMC]

USED_SITES_MMMC <- ALL_SITES  %>% filter(site_id %in% sites_MMMC) %>% distinct(site_id, longitude, latitude)
USED_SITES_MMMC <- data.frame(lon = USED_SITES_MMMC$longitude, lat = USED_SITES_MMMC$latitude, value = USED_SITES_MMMC$site_id)
USED_SITES_MMM <- ALL_SITES  %>% filter(!site_id %in% sites_MMMC) %>% filter(site_id %in% sites_MMM) %>% distinct(site_id, longitude, latitude)
USED_SITES_MMM <- data.frame(lon = USED_SITES_MMM$longitude, lat = USED_SITES_MMM$latitude, value = USED_SITES_MMM$site_id)

USED_SITES_spec <- ALL_SITES %>% filter(!site_id %in% sites_MMMC) %>% filter(!site_id %in% sites_MMM) %>% 
  filter(site_id %in% sites_spec) %>% distinct(site_id, longitude, latitude)
USED_SITES_spec <- data.frame(lon = USED_SITES_spec$longitude, lat = USED_SITES_spec$latitude, value = USED_SITES_spec$site_id)
USED_SITES_var <- ALL_SITES %>% filter(!site_id %in% sites_MMMC) %>% filter(!site_id %in% sites_MMM) %>% 
  filter(!site_id %in% sites_spec) %>% filter(site_id %in% sites_var) %>% distinct(site_id, longitude, latitude)
USED_SITES_var <- data.frame(lon = USED_SITES_var$longitude, lat = USED_SITES_var$latitude, value = USED_SITES_var$site_id)
USED_SITES_mean <- ALL_SITES %>% filter(!site_id %in% sites_MMMC) %>% filter(!site_id %in% sites_MMM)  %>% 
  filter(!site_id %in% sites_spec) %>% filter(!site_id %in% sites_var) %>% filter(site_id %in% sites_mean) %>% distinct(site_id, longitude, latitude)
USED_SITES_mean <- data.frame(lon = USED_SITES_mean$longitude, lat = USED_SITES_mean$latitude, value = USED_SITES_mean$site_id)
NOT_SITES <- ALL_SITES %>% filter(!site_id %in% sites_mean) %>% distinct(site_id, longitude, latitude)
NOT_SITES <- data.frame(lon = NOT_SITES$longitude, lat = NOT_SITES$latitude, value = NOT_SITES$site_id)


USED_SITES_MMMC_p<- projection_ptlyr(USED_SITES_MMMC, as.character('+proj=robin +datum=WGS84'))
USED_SITES_MMM_p<- projection_ptlyr(USED_SITES_MMM, as.character('+proj=robin +datum=WGS84'))
USED_SITES_spec_p <- projection_ptlyr(USED_SITES_spec, as.character('+proj=robin +datum=WGS84'))
USED_SITES_var_p <- projection_ptlyr(USED_SITES_var, as.character('+proj=robin +datum=WGS84'))
USED_SITES_mean_p <- projection_ptlyr(USED_SITES_mean, as.character('+proj=robin +datum=WGS84'))
NOT_SITES_p <- projection_ptlyr(NOT_SITES, as.character('+proj=robin +datum=WGS84'))


coastline_map <- rgdal::readOGR(dsn ="../iHadCM3LastMill/Functions/naturalearth_10m_physical/ne_10m_coastline.shp", verbose = FALSE)
karst_map <- rgdal::readOGR(dsn = "../iHadCM3LastMill/Functions/naturalearth_10m_physical/karst_wgs.shp", verbose = FALSE)

karst_map2 <- karst_map %>% spTransform(., CRSobj = CRS(as.character('+proj=robin +datum=WGS84'))) %>% fortify(.)
coastline_map2 <- coastline_map %>% spTransform(., CRSobj = CRS(as.character('+proj=robin +datum=WGS84'))) %>% fortify(.)
pt_size = 2.5

plot <- STACYmap(coastline = TRUE, filledbg = TRUE) +
    geom_polygon(data = karst_map2, aes(x=long, y = lat, group = group), fill = '#A57F7C', color = NA) +
    new_scale_color() +
    geom_point(data = NOT_SITES_p, aes(x = long, y = lat, shape = "6", color = '6'),# shape = 20,
               size = (pt_size*0.6), alpha = 0.7, show.legend = c(shape =TRUE)) +
  geom_point(data = USED_SITES_MMM_p, aes(x = long, y = lat, shape = "1", color = '1'),# shape = 17,
             size = (pt_size), alpha = 0.7, show.legend = c(shape =TRUE)) +
  geom_point(data = USED_SITES_MMMC_p, aes(x = long, y = lat, shape = "2", color = '2'),# shape = 17,
             size = (pt_size), alpha = 0.7, show.legend = c(shape =TRUE)) +
    geom_point(data = USED_SITES_spec_p, aes(x = long, y = lat, shape = "3", color = '3'),# shape = 17,
               size = (pt_size), alpha = 0.7, show.legend = c(shape =TRUE)) +
    geom_point(data = USED_SITES_var_p, aes(x = long, y = lat, shape = "4", color = '4'),# shape = 17,
               size = (pt_size), alpha = 0.7, show.legend = c(shape =TRUE)) +
    geom_point(data = USED_SITES_mean_p, aes(x = long, y = lat, shape = "5", color = '5'),# shape = 17,
               size = (pt_size), alpha = 0.7, show.legend = c(shape =TRUE)) +
    scale_color_manual(name = "SISAL v2 sites", labels = c("MMC: d18O and d13C", "MM: d18O res > 36n","S: d18O res > 30n", "V: d18O res > 20n", "M: d18O res > 10n", "other"),
                       values = c('#32936F', '#2274A5','#DD2525', '#F98D11', '#8D0DC4', '#000000')) +
    scale_shape_manual(name = "SISAL v2 sites", labels = c("MMC: d18O and d13C", "MM: d18O res > 36n","S: d18O res > 30n", "V: d18O res > 20n", "M: d18O res > 10n", "other"),
                       values = c(18,18,17, 17, 17, 20)) +
    #geom_polygon(data = coastline_map2, aes(x=long, y=lat, group = group), color = 'black',  size = 0.2, fill = NA, alpha = 0.8) +
    theme(legend.position = c(-0.05, -0.05),
          legend.justification = c(0, 0),
          legend.box = 'vertical',
          legend.box.background = element_blank(),
          legend.background = element_rect(colour = 'black'),
          panel.border = element_blank())

plot

plot %>% ggsave(filename = paste('03_SISAL_database', 'pdf', sep = '.'), plot = ., path = 'Plots', 
                                width = 2*8.3, height = 2*5.5-2, units = 'cm', dpi = 'print', device = "pdf")
                

# ## SISAL Karst Plot
# 
# #################################################
# PLOTTING_VARIABLES <- list()
# PLOTTING_VARIABLES$WIDTH = 8.3
# PLOTTING_VARIABLES$HEIGHT = 5.5
# 
# source("Functions/karst_map_plot.R")
# library(tidyverse)
# 
# # neede to plot all other sites, that are in SISALv2 but not in the analysis
# ALL_SITES <- read.csv("Data/SISALv2/site.csv")
# 
# sites_spec <- DATA_past1000$CAVES$entity_info$site_id[mask_spec]
# sites_var  <- DATA_past1000$CAVES$entity_info$site_id[mask_var]
# sites_mean <- DATA_past1000$CAVES$entity_info$site_id[mask_mean]
# 
# USED_SITES_spec <- ALL_SITES %>% filter(site_id %in% sites_spec) %>% distinct(site_id, longitude, latitude)
# USED_SITES_spec <- data.frame(lon = USED_SITES_spec$longitude, lat = USED_SITES_spec$latitude, value = USED_SITES_spec$site_id)
# USED_SITES_var <- ALL_SITES %>% filter(!site_id %in% sites_spec) %>% filter(site_id %in% sites_var) %>% distinct(site_id, longitude, latitude)
# USED_SITES_var <- data.frame(lon = USED_SITES_var$longitude, lat = USED_SITES_var$latitude, value = USED_SITES_var$site_id)
# USED_SITES_mean <- ALL_SITES  %>% filter(!site_id %in% sites_spec) %>% filter(!site_id %in% sites_var) %>% filter(site_id %in% sites_mean) %>% distinct(site_id, longitude, latitude)
# USED_SITES_mean <- data.frame(lon = USED_SITES_mean$longitude, lat = USED_SITES_mean$latitude, value = USED_SITES_mean$site_id)
# NOT_SITES <- ALL_SITES %>% filter(!site_id %in% sites_mean) %>% distinct(site_id, longitude, latitude)
# NOT_SITES <- data.frame(lon = NOT_SITES$longitude, lat = NOT_SITES$latitude, value = NOT_SITES$site_id)
# 
# #################################################
# ## PLOTTING #####################################
# 
# GLOBAL_STACY_OPTIONS$GLOBAL_FONT_SIZE = 10
# plot <- karst_map_plot(USED_SITES_spec = USED_SITES_spec,
#                        USED_SITES_var = USED_SITES_var,
#                        USED_SITES_mean = USED_SITES_mean,
#                        NOT_SITES = NOT_SITES, pt_size = 2.5) + 
#   theme(legend.position = c(-0.01, 0), legend.justification = c(0, 0), legend.box = 'vertical',
#         axis.text = element_blank(),
#         panel.border = element_blank(),
#         legend.text = element_text(size = 12))
# 
# #plot
# plot %>% ggsave(filename = paste('Fig2_SISAL_database', 'pdf', sep = '.'), plot = ., path = 'Paper_Plots', 
#                 width = 2*PLOTTING_VARIABLES$WIDTH, height = 2*PLOTTING_VARIABLES$HEIGHT-2, units = 'cm', dpi = 'print', device = "pdf")
# 
# remove(plot, NOT_SITES, USED_SITES_mean, USED_SITES_spec, USED_SITES_var, ALL_SITES, sites_mean, sites_spec, sites_var)
# rm(karst_map, coastline_map, bathy_data, map_data, karst_map_plot)

# library(plyr)
# library(dplyr)
# library(rgdal)
# library(latex2exp)
# 
# source("Functions/STACYmap_PMIL.R")
# source("Functions/projection_ptlyr.R")
# 
# 
# coastline_map <- rgdal::readOGR(dsn ="Functions/naturalearth_10m_physical/ne_10m_coastline.shp", verbose = FALSE)
# karst_map <- rgdal::readOGR(dsn = "Functions/naturalearth_10m_physical/karst_wgs.shp", verbose = FALSE)
# 
# karst_map_plot <- function(USED_SITES_spec,
#                            USED_SITES_var,
#                            USED_SITES_mean, 
#                            NOT_SITES, 
#                            projection = as.character('+proj=robin +datum=WGS84'),
#                            pt_size){
#   
#   karst_map2 <- karst_map %>% spTransform(., CRSobj = CRS(projection)) %>% fortify(.)
#   coastline_map2 <- coastline_map %>% spTransform(., CRSobj = CRS(projection)) %>% fortify(.)
#   
#   USED_SITES_spec_p <- projection_ptlyr(USED_SITES_spec, projection)
#   USED_SITES_var_p <- projection_ptlyr(USED_SITES_var, projection)
#   USED_SITES_mean_p <- projection_ptlyr(USED_SITES_mean, projection)
#   NOT_SITES_p <- projection_ptlyr(NOT_SITES, projection)
#   
#   plot <- STACYmap(coastline = TRUE, filledbg = TRUE) +
#     geom_polygon(data = karst_map2, aes(x=long, y = lat, group = group), fill = '#A57F7C', color = NA) +
#     new_scale_color() +
#     geom_point(data = NOT_SITES_p, aes(x = long, y = lat, shape = "4", color = '4'),# shape = 20,
#                size = (pt_size), alpha = 0.7, show.legend = c(shape =TRUE)) +
#     geom_point(data = USED_SITES_spec_p, aes(x = long, y = lat, shape = "1", color = '1'),# shape = 17,
#                size = (pt_size), alpha = 0.7, show.legend = c(shape =TRUE)) +
#     geom_point(data = USED_SITES_var_p, aes(x = long, y = lat, shape = "2", color = '2'),# shape = 17,
#                size = (pt_size), alpha = 0.7, show.legend = c(shape =TRUE)) +
#     geom_point(data = USED_SITES_mean_p, aes(x = long, y = lat, shape = "3", color = '3'),# shape = 17,
#                size = (pt_size), alpha = 0.7, show.legend = c(shape =TRUE)) +
#     scale_color_manual(name = "SISAL v2 sites", labels = c("d18O res > 30n", "d18O res > 20n", "d18O res > 10n", "other"), 
#                        values = c('#DD2525', '#F98D11', '#8D0DC4', '#000000')) +
#     scale_shape_manual(name = "SISAL v2 sites", labels = c("d18O res > 30n", "d18O res > 20n", "d18O res > 10n", "other"), 
#                        values = c(17, 17, 17, 20)) +
#     #geom_polygon(data = coastline_map2, aes(x=long, y=lat, group = group), color = 'black',  size = 0.2, fill = NA, alpha = 0.8) +
#     theme(legend.position = c(0.02, 0.02),
#           legend.justification = c(0, 0),
#           legend.box = 'vertical',
#           legend.box.background = element_blank(),
#           legend.background = element_rect(colour = 'black'),
#           panel.border = element_blank())
#   
#   rm(coastline_map2, karst_map2, NOT_SITES_p)
#   rm(USED_SITES_mean_p, USED_SITES_spec_p, USED_SITES_var_p)
#   
#   return(plot)
# }
# 


