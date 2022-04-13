# Script para alinear:

packs <- c('raster','rgdal','parallel', 'R.utils', 'rvest','xml2', 'furrr' ,'httr','getPass','gdalUtils','gdalUtilities','rgeos', 'viridis', 'rasterVis','rlang', 'rasterDT')
sapply(packs, require, character.only = TRUE)

##### r
# imagen a alienar
target<-'/storage/share/Angela_Share/DarienMask.tif'
 #este es mi mapa de referencia para alinear. pero en realidad puedes usar cualquier que esté alineaso
 reference. <-"/storage/home/TU/tug76452/Ecosistemas_Colombia/hansen_ideam/mask_ideam90_17.tif"
 #donde se va a poner: Share 
 dst.<-'/storage/share/Shared_summer_21/BioMapcol/DarienMask.tif'
#
# correr función 
 align_rasters(unaligned=target, reference=reference., dstfile=dst., nthreads=6, verbose= TRUE)
 
 

