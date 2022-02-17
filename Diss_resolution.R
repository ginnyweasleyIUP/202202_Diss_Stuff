resolution <- c()
data_points <- c()
for(entity in DATA_past1000$CAVES$entity_info$entity_id[mask_spec]){
  data_rec = DATA_past1000$CAVES$record_res %>% filter(entity_id == entity)
  data_points = c(data_points, length(data_rec$interp_age))
  resolution = c(resolution, mean(diff(data_rec$interp_age), na.rm = T))
}

resolution = as.numeric(resolution)
data_points = as.numeric(data_points)
No.digits = 2

bstrap <- c()
for (i in 1:1000){
  bstrap <- c(bstrap,mean(sample(resolution,length(resolution),replace=T), na.rm = T))
}
print(paste0("Mean resolution: $", round(mean(as.numeric(resolution), na.rm = T),digits = No.digits), "$ unit{yr},",
             " ($", round(quantile(bstrap,0.05), digits = No.digits), "$, $", round(quantile(bstrap,0.95), digits = No.digits), "$)"))