# Script para alinear:

packs <- c('raster','rgdal','parallel', 'R.utils', 'rvest','xml2', 'furrr' ,'httr','getPass','gdalUtils','gdalUtilities','rgeos', 'viridis', 'rasterVis','rlang', 'rasterDT')
sapply(packs, require, character.only = TRUE)

##### r
# imagen a alienar
target<-'/path/target.tif'
 #este es mi mapa de referencia para alinear. pero en realidad puedes usar cualquier que esté alineaso
 reference. <-"path/mask_ideam90_17.tif"
 
#donde se va a poner: Share 
 dst.<-'path/out/target.tif'
#
# correr función 
 align_rasters(unaligned=target, reference=reference., dstfile=dst., nthreads=6, verbose= TRUE)
 
 

