I expect to create a portable toolbox of custom function at hand to perform analyisis  
#This creates a vector pf zeros and length nrow de la tabla) But this is not what I need. Save it somewhere 
repv <- list(unlist(replace_na(vector(mode='list', length =nrow(metrics_wide[[1]])),0)))   

#This replaces all NAs with zeros (not needed right now) 
metrics_w <- map(metrics_w, . %>% replace(is.na(.),0)) 
