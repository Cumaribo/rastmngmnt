require('ecochange') #ecochange_1.6.tar.gz
packs <- c('raster','rgdal','parallel', 'R.utils', 'rvest','xml2','tidyverse', 'landscapemetrics', 'sf','dplyr','httr','getPass','gdalUtils','gdalUtilities','rgeos', 'viridis', 'rasterVis','rlang', 'rasterDT')
sapply(packs, require, character.only = TRUE)


dir()

freq(mr[[1]])

#Set rout to folder where the armonized rasters are stored
dir. <- "/storage/home/TU/tug76452/Align/Hansen19_20"

#set list of if files to align
tiffes <- dir(dir.)
tiffes1 <- file.path(dir.,tiffes)
#create folder to store the new rasters 
dir.create("/storage/home/TU/tug76452/Align/Hansen19_20/outs")

#set path to the refernece file
reference. <-"/storage/home/TU/tug76452/Ecosistemas_Colombia/mask_ideam90_17.tif"

tiffes2  <- file.path('/storage/home/TU/tug76452/Align/Hansen19_20/outs',tiffes)

system.time(
malr <- Map(function(x,y)
    align_rasters(
        unaligned=x,
    reference=reference.,
    dstfile=y,
    nThreads=8,
    verbose=TRUE),
    tiffes1,tiffes2)
)

getwd()

setwd('/storage/home/TU/tug76452/Align')

## Mosaicing the whole layers in directory band_2000 into an out.tif layer
##<------------------------------------------------------------------

path.  <- '/storage/home/TU/tug76452/Align/Hansen19_20/outs'# first change folder path

toimp <- dir(path.)[grepl('.tif',dir(path.))]
nwp <- file.path(path., toimp)
dst <- file.path('/storage/home/TU/tug76452/Align/merged_19_20.tif')# later move it to elsewhere 

## set any crs:
cr.  <- "+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"  

#"+proj=longlat +datum=WGS84 +no_defs"
getwd()

## develop the mosaicing process
system.time(
mr <- gdalUtils::mosaic_rasters(
                     gdalfile=nwp,
                     dst_dataset=dst,
                     output_Raster = TRUE,
                     #gdalwarp_params = list(t_srs = cr.),
                     verbose = TRUE)
)
writeRaster(mr[[1]], 'merged_2019a', format='GTiff', overwrite=TRUE)
writeRaster(mr[[2]], 'merged_2020a', format='GTiff', overwrite=TRUE)

freq(mr[[1]])
