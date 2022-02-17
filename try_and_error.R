#Yannicks Ice core
#IC13THQU01A

#load("/home/ldap-server/kreacher/R/Code/load_iso2k.R")

source("/home/ldap-server/kreacher/R/Code/load_iso2k.R")

elevation_2 = list()
for(ii in 1:651){
  elevation_2 = c(elevation_2, elevation[[ii]])
}

elevation_2 = as.numeric(elevation_2)

which(elevation_2 == 5670)


TS_ice = TS[[261]]

dataset_name = list()
for(ii in 1:651){
  dataset_name = c(dataset_name, TS[[ii]]$dataSetName)
}


max_time = c()
min_time = c()
max_d18O = c()
min_d18O = c()
archiveType = c()
year_or_age = c()
for(ii in 1:651){
  max_time = as.numeric(c(max_time, max(TS[[ii]]$year, na.rm = T)))
  min_time = as.numeric(c(max_time, min(TS[[ii]]$year, na.rm = T)))
  max_d18O = as.numeric(c(min_d18O, max(TS[[ii]]$paleoData_values, na.rm = T)))
  min_d18O = as.numeric(c(min_d18O, min(TS[[ii]]$paleoData_values, na.rm = T)))
  archiveType = as.character(c(archiveType, TS[[ii]]$archiveType))
  if(!is.null(TS[[ii]]$year) & !is.null(TS[[ii]]$age)){year_or_age = as.numeric(c(year_or_age, 1))}
  if(!is.null(TS[[ii]]$year) & is.null(TS[[ii]]$age)){year_or_age = as.numeric(c(year_or_age, 2))}
  if(is.null(TS[[ii]]$year) & !is.null(TS[[ii]]$age)){year_or_age = as.numeric(c(year_or_age, 3))}
  if(is.null(TS[[ii]]$year) & is.null(TS[[ii]]$age)){year_or_age = as.numeric(c(year_or_age, 4))}
}

#Max_time at record 97-102 = -Inf
#Max_time for records 501 503 516 523 527 538 548 549 550 551 552 630 is negative
#The distribution of the rest seems ok

#Double check for min time:
#Min_time at record 97-102 = -Inf
#the same records show negative min time...
# rest seems ok

#For d18O:
#Inf values for 403 406 416 419 433
#There are some very negative values for 61 200 278 522 and 523
#Also there are a lot of values with very high d18O => this is apparently mostly the case for wood and sediments

#Min d18O has the same Infinites
#also the same values are very very negative
#The rest looks reasonable

# ==> There are only a handful of records that need to be checked...





which(max_d18O< -100)
min_time[which(min_time<0)] = NA
#max_time[c(97:102)] = NA
