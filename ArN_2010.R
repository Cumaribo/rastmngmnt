install.packages(c("devtools", "testthat", "knitr", "RStoolbox", "ggplot2", 'plyr'))

rm(list=ls())


#1. Set the workspace, calibration parameters and load Data.

#Require libraries

library(raster)
library(rgdal)
library(caTools)
library(rgeos)
library(RStoolbox)
library(ggplot2)
library(plyr)

#Path to the folder where the tar.gz files are stored. This will be your main working folder
inpath="~/Documents/Vichada_downloads/2010"
setwd(inpath)

#Define projection. (To improve: if no reprojection necessary, the function needs to be able to recognize it
# and skip the step (if-statement))
projString <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#set p-value for the Chg-no Chg Mask
p_value= 0.0001

#create a list with all the .gz files
files <- list.files(".", ".gz")
######## FIRST STAGE. DECOMPRESS, STACK AND REPROJECT IMAGERY###################################################
###############This creates and names the folders and untars each one of the .tar.gz there ######################

dir()
fdc <- function(files,i){
  fld1 <- substr(files[i], 1, 22)
  path1 <- paste(getwd(), fld1, sep = '/')
  f1 <- untar(files[i], exdir = path1)
  lbnds = list.files(path = path1, pattern ="sr_band")
  return(lbnds)
}
list <- list()
ptm<-proc.time()
for(i in 1:length(files)){
  list[[i]] <- fdc(files,i)
}

#################################################################################################################

paths<- list.dirs()

#stack, reproject and convert to layerstack
ptm_rep<-system.time(
for(i in 1:length(targetlist)){
  remd <- gsub('\\./','', paths)[-1L]
  npaths <- paste(getwd(), remd, sep = '/')
  setwd(npaths[i])
  rasters1 <-list.files(pattern= "sr_band")
  stack1 <- stack(rasters1)
  stack1_sin = projectRaster(stack1, crs = projString)
  print(paste(i, "image(s) reprojected", sep=" "))
  rm(stack1)
  setwd(inpath)
  stack1_sin= round(stack1_sin)
  writeRaster(stack1_sin, paste((remd[i]), "stacked", sep="_"), format="GTiff", overwrite=TRUE, datatype='INT2S')
  rm(stack1_sin)
  print(paste(i, 'image(s) saved to file', sep=" "))
})

  ###################################################################################################################
##########SECOND STAGE, EXTRACT INTERSECTIONS, PSEUDO INVARIANT FEATURES AND CALCULATE REGRESSION PARAMETERS####
######################################+++++++++++++++++++++++++++############################

list_projected = list.files(pattern ="stacked.tif")
list_projected

# Load or define the reference image(s).
#to improve: if there are other images overlapping and with the same date of the reference image
#and (same path+-1 row, it should not be calibrated and instead can be used as a reference image
#too
#PROMPT. Please Select your reference Image.

###### Chooose Ref Image from list.projected###########################################
refname<-list_projected[[1]]

#refname<-"LC0M0PP0RRYYYYMMDD01T1_reprojected.tif"


#1. M   =   LandSat Mission
#2. PP  =   Path
#3. RR  =   Row
#4. YYYY=   Year
#5. MM  =   Month
#6. DD  =   Day
refstack=stack(refname)

targetlist<-list_projected[list_projected !=refname]

targetlist

#Attention: what happens if reference and target image do not intercept?
#This is solved with max x max y  for the extent and comparing them. If the extent don'd meet
# the intersection is not calculated. Will adress this in a new version of the script
#instack1=rasterstack1 ######.


###########################################################################################################
#This for loop executes the remaining code, and the secuential tasks up to the calibration.
#To correct: when 7 bands, the exported plot shows only band 7, as it is opened in a new window. It is necessary
#to correct this


for(i in 1:length(targetlist)){
# Set "i" manually, the loop is not working properly yet, results are overwritten in each iteration.
#  When there is only one image to calibrate it is not necessary
 #refstack=stack(refstack)
   instack=stack(targetlist[i])
   inname= targetlist[i]
   radioname = inname
   # inname = 'LC080070572016020101T1_reprojected.tif'
   # instack=stack(inname)
######################################################################################################

#This function extracts the intersections of two ovelapping images

  getintersection<- function(refimage, targetimage){
    intersection1<- crop(refstack, instack)
    intersection2<- crop(instack, refstack)
    extent(intersection1)=extent(intersection2)
    m <- c(-Inf,0,NA, 0, Inf, 1)
    m <- matrix(m, ncol=3, byrow=TRUE)
    msk1 <- reclassify(intersection1, m)
    msk2 <- reclassify(intersection2, m)
    msk=msk1*msk2
    msk=msk[[1]]*msk[[2]]*msk[[3]]
    striperef<-intersection1*msk
    #print(i, "reference intersection calculated", sep=" ")
    stripetarget<-intersection2*msk
    #print(i, "target intersection calculated", sep=" ")
    writeRaster(striperef, paste(substr(inname, 1, 22), substr(refname,1,22) ,"intersection_ref", sep="_"), format="GTiff", overwrite=TRUE, datatype='INT2S')
    writeRaster(stripetarget, paste(substr(inname, 1, 22), substr(refname,1,22) ,"intersection_tar", sep="_"), format="GTiff", overwrite=TRUE, datatype='INT2S')
    return(list(striperef, stripetarget))
  }
  
intersections<- getintersection(refstack, instack)
print(paste(i, "intersections extracted", sep= " "))

}

#############INSERT iMAGES hERE ########################################################
  refname= "LE070060562010021701T1_LC080050562016011801T1_intersection_ref.tif"
  refstripe= stack(refname)
  inname= "LE070060562010021701T1_LC080050562016011801T1_intersection_tar.tif"
  targetstripe= stack(inname)
  radioname= paste(substr(inname, 1,22), "_stacked.tif", sep= "")

  
  # refstripe= stack(intersections[[1]])
  # targetstripe= stack(intersections[[2]])
  # radioname= paste(substr(inname, 1,22), "_stacked.tif", sep= "")
extent(refstripe)=extent(targetstripe)

  rm(intersections,  instack)


  ################################A#######################################################################
  ##################################Load Intersections############################################################
  #refstripe= stack(select ref intersection)
  #targetstripe= stack(select target intersection)
  #this makes sure that the nbands in ref and in tar is the same

  if (nlayers(refstripe)==nlayers(targetstripe)) {print ("looking good")}
  if (nlayers(refstripe)>nlayers(targetstripe)) {refstripe=subset(refstripe, c(2:7), drop=TRUE)}
  if (nlayers(targetstripe)>nlayers(refstripe)) {targetstripe=subset(targetstripe, c(2:7), drop=TRUE)}

  #######++++++++++++++###################++++++++++++++++++++#################

  ###############FILTER CLOUDS #################################################################################
  
# 
#   refcloud<-cloudMask(refstripe, blue = 1, tir = 6, threshold = 0.9, buffer = NULL, plot = TRUE, verbose=TRUE)
#   ggR(refcloud, 2, geom_raster = TRUE)
#   
#   refcloud2<-cloudMask(refcloud, threshold = 0.2, buffer = NULL, plot = FALSE, verbose = TRUE)
#   ggR(refcloud2, 1, geom_raster = TRUE)
#   #writeRaster(refcloud2, paste(substr(inname, 1, 22), substr(refname,1,22) ,"ref_cloudMask", sep="_"), format="GTiff", overwrite=TRUE, datatype='INT2S')
#   # Change and no change mask
#   tarcloud<-cloudMask(targetstripe, threshold = 0.9, blue = 1, tir = 6, buffer = NULL, plot = TRUE, verbose=TRUE)
#   ggR(tarcloud, 1, geom_raster = FALSE)
#   tarcloud2<-cloudMask(tarcloud, threshold = 0.2, buffer = NULL, plot = FALSE, verbose = TRUE)
#   #writeRaster(tarcloud2, paste(substr(inname, 1, 22), substr(refname,1,22) ,"tar_cloudMask2", sep="_"), format="GTiff", overwrite=TRUE, datatype='INT2S')
  
  
  
###########################Apply filter Mask 


# refstripe<- mask(refstripe, refcloud2[[1]], filename="", inverse=FALSE, maskvalue=1, updatevalue=NA, updateNA=FALSE)
# 
 ggRGB(refstripe, r=4, g=3, b=2)
#   
# 
# targetstripe<- mask(targetstripe, tarcloud2[[1]], filename="", inverse=FALSE, maskvalue=1, updatevalue=NA, updateNA=FALSE)  
#   
#writeRaster(targetstripe, paste(substr(inname, 1, 22), substr(refname,1,22) ,"filtered_tar", sep="_"), format="GTiff", overwrite=TRUE, datatype='INT2S')
  
  ########################################################################################################
  #This function generates the change no change mask by getting the standardized differences between the
  #reference and target stripe.
  #  To improve. If the reference and target image have the same path and row, t he intersection is very  large
  # covering almost the whole scene. Consider a way to make it smaller to lower computing load.
  #pvalue: the probability level to differentiate between change and no change.
  
    ##############################################################################################
   # 1 Get Standardized Differences
  #If necessary, previously saved intersections can be loaded here, if not, just execute the function.


  #ref=refstripe
  #target=stack(targetstripe)
  #rm (refstripe, refstack)

  thresraster=function(ref=refrast, target=tarrast, minvalid=0, maxvalid=100000) {
    # Creates a change and a no change mask between a target image and a reference image
    # based on a probability threshold
    # ref: reference raster
    #target: target raster to normalize
    #pvalue: the probability level to differentiate between change and no change.
    # the number of pixels classified as no change decreases with lower pvalue
    print("calculating standardized differences")
    #drops not valid and negative values
    m <- c(-Inf,minvalid,NA, minvalid, maxvalid, 1, maxvalid,Inf,NA )
    m <- matrix(m, ncol=3, byrow=TRUE)
    msk1 <- reclassify(ref, m)
    msk2 <- reclassify(target, m)
    msk=msk1*msk2
    msk=msk[[1]]*msk[[2]]*msk[[3]]
    rm(msk1, msk2)
    refmskd=mask(ref,msk)
    tarmskd=mask(target, msk)
    rm(ref,target)

    # calculates standardized differences vor the valid pixels
    dif=refmskd-tarmskd
    dif=mask(dif, msk)
    standardizediff=dif
    sampix=Which(msk == 1, cells=TRUE, na.rm=TRUE)
    for (k in 1:nlayers(dif)) {
      #difvec=unlist(as.vector(as.matrix(dif[sampix])))
      difvec=getValues(dif[[k]])
      difvec=difvec[!is.na(difvec)]
      meandif=mean(difvec, na.rm=TRUE)
      sddif=sd(difvec, na.rm=TRUE)
      standardizediff[[k]]= ((meandif-dif[[k]])/sddif)^2
      print(paste(k, "bands processed", sep=" "))
    #the standardized difference is the value that will be contrasted with the chi-square statistic to identify significant differences\
    }
    standardizediff=mask(standardizediff,msk)
    sumstandardizediff=mask(calc(standardizediff, sum, na.rm=TRUE),msk)
    return(sumstandardizediff)
  }
  
  
  ###########################################################################################################
  #Transforms the band of standardized differnces created by threshraster into a binary change-noChange mask
  nochg=function(thresraster=threslayer, pvalue=p_value, degfree=nlayers(ref)-1) {
    print("calculatating no change mask")
    threshold=qchisq(pvalue,degfree)
    m <- c(-Inf,threshold,1,  threshold,999999,NA)
    m <- matrix(m, ncol=3, byrow=TRUE)
    nochgmsk = reclassify(thresraster, m)
    return(nochgmsk)
  }

  ###########################################################################################################
calibrationParameters<-function(ref = refimage, target= targetimage, treshMask){
  #this function generates the calibration parameters comparing the value of corresponding
  #valid pixels in the image pair and running a linear regression. Generates the intercept and the  slope (coefficients a and b)
  print("calculating calibration parameters")
  #Defines groups to aggregate points in the scatterplot to prevent high concentration of pixels
  # with high or low values to bias  the regression.
  
  bracksplit=function(xval=seq(3,102), yval=xval+rnorm(length(xval), mean=0, sd=10), nbrackets=30){
    # reduce the number of data points to the averages of different data ranges
    indata=cbind(xval, yval)
    bracketlength=(max(xval)-min(xval))/nbrackets
    bracketbins=seq(1,nbrackets-1)
    bracketranges=c(min(xval), bracketbins*bracketlength+min(xval), max(xval))
    
    labels=cbind(rep(NA, length(bracketbins)),rep(NA, length(bracketbins)))
    for(i in 1:length(bracketbins)) {
      datasubset=subset(indata, indata[,1]>=bracketranges[i] & indata[,1]<bracketranges[i+1])
      labels[i,1]=mean(datasubset[,1], na.rm=TRUE)
      labels[i,2]=mean(datasubset[,2], na.rm=TRUE)
    }
    return(labels)
  }
  
  ranges =c(-Inf, 0, NA,0,99999999999,1)
  ranges= matrix(ranges)
  mask= reclassify(target,ranges)
  mask<-mask[[1]]
  
  weighted=1
  
  #Masks the NA pixels
  threshMask[is.na(threshMask[])] <- 0
  
  ranges <-c(-Inf,0.0001,NA, 0.0001,Inf,1)
  plotranges<- matrix(ranges, ncol=3, byrow=TRUE)
  maskNA<- reclassify(mask, ranges)
  inmskd=mask(target,maskNA)
  refmskd=mask(ref,maskNA)
  rm(mask)
  # This converts the mask with the pseudo invariant features into a matrix
  allPIF=as.matrix(threshMask)
  allPIF1=allPIF
  allPIF1[which(allPIF==0)]=NaN
  
  
  if (nlayers(inmskd)==nlayers(refmskd)) {nb = (nlayers(inmskd))}
  else {print("different number of bands in  input files, aborting")}
  # if (nlayers(refstripe)==nlayers(targetstripe)) {print ("looking good")}
  # if (nlayers(refstripe)>nlayers(targetstripe)) {refstripe=subset(refstripe, c(2:7), drop=TRUE)}
  # if (nlayers(targetstripe)>nlayers(refstripe)) {targetstripe=subset(targetstripe, c(2:7), drop=TRUE)}
 
  intercepts = rep(0, nb)
  slopes = rep(0, nb)
#Finds parameters for relative radiometric normalization
  
  #############################################################
  insampnormw <- list()
  for (b in 1:nb) {
    inb=as.matrix(inmskd[[b]])
    inb=inb[which(allPIF1==1)]
    inb=as.vector(inb)
    refb=as.matrix(refmskd[[b]])
    refb=refb[which(allPIF1==1)]
    refb=as.vector(refb)

    # if (weighted==1) {
      brackdata=bracksplit(xval=inb, yval=refb, nbrackets=30)
      insampnormw[[b]]=lm(brackdata[,2]~brackdata[,1],
                     na.action=na.omit)
    # } else {
    #   insampnormw=lm(refb~inb,
    #                  na.action=na.omit)}
    # 
     intercepts[b]=as.numeric(insampnormw$coefficients[1])
     slopes[b]=as.numeric(insampnormw$coefficients[2])
     rm(insampnormw)
  }
    #return(insampnormw)
  paramt=data.frame(cbind(intercepts, slopes))
  #save parameters
  write.table(paramt, (paste(substr(radioname, 1, 22),"parameters.csv", sep="")),col.names=T,row.names=T, sep= ",")
  
  out <- list(intercepts=intercepts, slopes=slopes, inmskd=inmskd, refmskd=refmskd)
  ret <- c("intercepts", "slopes",  "inmskd", 'refmskd') 
  out[!names(out) %in% ret] <- NULL
  
  #########################oooooooooooooooooooooooooooo##################################
  return(out)}
  
  ############################0000000000000000000000000######################################
  #RUN FUNCTIONS
  #Standardized Differences

  std_diff_time <- system.time(
  thresband<-thresraster(ref=refstripe, target=targetstripe, minvalid=0, maxvalid=100000)
  
  )
  # No Chgh Mask
  threshMask=nochg(thresraster=thresband, pvalue=0.0001, degfree=nlayers(refstripe)-1)
  
   ######
  Subset pif mask. 
   
  set.seed(5)
  #m <- c(-Inf,0,NA, 0, Inf, 1)
  #m <- matrix(m, ncol=3, byrow=TRUE)
  #pifMap <- reclassify(threshMask, m)
  #count the n umber of cells with a value of 0 (zero)
  cell_count<-freq(threshMask, value = 1, useNA = 'no')
  #randomly sample 80% of the valid pixels, drop na
  pifMap_samp<-sampleRandom(threshMask, size= round(0.8*cell_count), na.rm=TRUE, asRaster=TRUE)
  
  pifMap_samp[is.na(pifMap_samp)] <- 0
  threshMask[is.na(threshMask)]<-0
  #this is the validation data
  pifMap_val= threshMask-pifMap_samp
  
  cell_count2<-freq(pifMap_val, value = 1, useNA = 'no')
  cell_count3<-freq(pifMap_samp, value = 1, useNA = 'no')
  
  #####################################
  #Calibration Parameters
  system.time( 
  param<- calibrationParameters(ref= refstripe, target= targetstripe, threshMask)
  )
###################Print Scatterplots########################################
plotmsk <- function(pifMsk){
allPIF=as.matrix(pifMsk)
allPIF1=allPIF
allPIF1[which(allPIF==0)]=NaN
return(allPIF1)}
allPIF1 <- plotmsk(threshMask)
  
nb <- nlayers(refstripe)
par(mfrow = n2mfrow(nb))
for(nb in 1:nb){    
    inb=as.matrix(param$inmskd[[nb]])
    inb=inb[which(allPIF1==1)]
    inb=as.vector(inb)
    refb=as.matrix(param$refmskd[[nb]])
    refb=refb[which(allPIF1==1)]
    refb=as.vector(refb)
# jpeg(file=paste(substr(inname, 1, 22) ,"scatterplot.jpg", sep="_"))
plot(inb, refb)  
lines(inb, param$intercepts[nb] + param$slopes[nb]*inb)
}
#dev.off()
  
rm(param, img, pifMap, pifMap_samp, ref, refstripe)
  ###############THIRD STAGE. CALIBRATE TARGET IMAGE ##################################

  radiostack= stack(radioname)
 
  #print("calibrating target image", paste(i), sep=" ")

  ################Calibrate Raster #########################################################################
  CalibrateRaster = function(targetimage){

    param=read.csv(paste(substr(radioname, 1, 22),"parameters.csv", sep=""))
    #param=read.csv("LT050060661995072601T1parameters.csv")
    intercepts=param$intercepts
    slopes=param$slopes

    b=1
    normedstack=intercepts[b]+(slopes[b]*radiostack[[b]])
    print(paste(b, "band(s) processed", sep=" "))

    for (b in 2:nlayers(radiostack)) {
      normedstack	=stack(normedstack, intercepts[b]+(slopes[b]*radiostack[[b]]))
      print(paste(b, "band(s) processed", sep=" "))}
      normedstack=round(normedstack, digits=0)
      #print("saving normedstack", paste(i), sep=" ")
      writeRaster(normedstack, paste(substr(radioname, 1,22), "normalized", sep="_"), format="GTiff", overwrite=TRUE, datatype='INT2S')
      return(normedstack)}

  ######################### Run Calibration
  calibrated= CalibrateRaster(radiostack)
rm(calibrated, param, radiostack)


################################Validation trough RSME##############################


radiostack=targetstripe

Calibrateintersection = function(param, targetimage){
  
  param=read.csv(paste(substr(radioname, 1, 22),"parameters.csv", sep=""))
  intercepts=param$intercepts
  slopes=param$slopes
  
  b=1
  normedstack=intercepts[b]+(slopes[b]*radiostack[[b]])
  print(paste(b, "band(s) processed", sep=" "))
  
  for (b in 2:nlayers(radiostack)) {
    normedstack	=stack(normedstack, intercepts[b]+(slopes[b]*radiostack[[b]]))
    print(paste(b, "band(s) processed", sep=" "))}
  valstack=round(normedstack, digits=0)
  print("saving valstack", sep=" ")
  #writeRaster(normedstack, paste(substr(radioname, 1,22), "intersection_norm", sep="_"), format="GTiff", overwrite=TRUE, datatype='INT2S')
  return(valstack)}
#Remove date from memory before a new iteration
valstack= Calibrateintersection(param, radiostack)

refstripe <- stack(refname)

calculate.RMSE <- function(val_data, ref_data, val_msk){
  rast_diff = val_data - ref_data
  sqrdf = (rast_diff)^2
  val_msk = mask(sqrdf, val_msk)
  r <- as.data.frame(cellStats(val_msk,mean))
  RMSE=sqrt(r$`cellStats(val_msk, mean)`)
  return(RMSE)
}

RMSEpredict <- calculate.RMSE(valstack, refstripe, pifMap_samp)

RMSEpredict


 
