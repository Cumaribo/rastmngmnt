# Raster resample GDAL

packs <- c('raster','rgdal', 'R.utils', 'rvest','xml2','tidyverse', 'landscapemetrics', 'sf','dplyr','httr','getPass','gdalUtils',
'gdalUtilities','rgeos','rlang', 'rasterDT', 'furrr')                  
sapply(packs, require, character.only = TRUE)   
tiffes<- dir(dir., pattern='PA.tif')
tiffes1 <- file.path(dir.,tiffes)    
reference.<-raster('/~reference.tif')
 gdal_resample <- function(input, output, r_base, method = 'bilinear'){                                                                                                                                                         
   #Geometry attributes                                                                                                                                                                                                                     
   t1 <- c(xmin(r_base), ymin(r_base),                                                                                                                                                                                                      
           xmax(r_base), ymax(r_base))                                                                                                                                                                                                      
   res <- res(r_base)                                                                                                                                                                                                                       
                     #' #GDAL time!                                                                                                                                                                                                         
   gdalwarp(tiffes1, tiffes2,                                                                                                                                                                                                               
  tr = res, te = t1, r = method)                                                                                                                                                                                      
 resample_raster = raster(tiffes2)                                                                                                                                                                                                          
 return(resample_raster)                                                                                                                                                                                                                    
 }  
 test<- map(1:length(tiffes1), function(x) gdal_resample(tiffes1[x], tiffes2[x], reference., method='bilinear'))
