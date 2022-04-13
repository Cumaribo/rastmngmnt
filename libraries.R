# Here, the lists of libraries I need to upload fpr my scripts nd how to load them in one line 
packs <- c('raster','rgdal','parallel', 'R.utils', 'rvest','xml2','tidyverse', 'landscapemetrics', 'sf',
'dplyr','httr','getPass','gdalUtils','gdalUtilities','rgeos', 'viridis', 'rasterVis','rlang', 'rasterDT', 'furrr')
sapply(packs, require, character.only = TRUE)

Remember 'RStoolbox'
