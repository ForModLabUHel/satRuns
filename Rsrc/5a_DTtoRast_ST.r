library(devtools)
# Run settings (if modifiedSettings is not set to TRUE in batch job script, default settings from Github will be used)
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/settings.r")
if(file.exists("localSettings.r")) {source("localSettings.r")} # use settings file in local directory if one exists

# Run functions 
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/functions.r")

# Check and create output directories
setwd(generalPath)
mkfldr <- paste0("outRast/","init",startingYear)
if(!dir.exists(file.path(generalPath, mkfldr))) {
  dir.create(file.path(generalPath, mkfldr), recursive = TRUE)
}

load(paste0(procDataPath,"init",startingYear,"/","DA",yearOut,"/XYsegID.rdata"))  
crsX <- crs(raster(baRast))

setkey(XYsegID,segID)
Dda2019 <- Dm2019 <- Ds2019 <- 
  Hda2019 <- Hm2019 <- Hs2019 <- 
  Bda2019 <- Bm2019 <- Bs2019 <- 
  perPm2019 <- perSPm2019 <- perBm2019 <- 
  perPs2019 <- perSPs2019 <- perBs2019 <- 
  perPda2019 <- perSPda2019 <- perBda2019 <- 
  stDA2019 <- 
  data.table()

for(i in 1:20){
  load(paste0("procData/init2016/calST_split/stProbMod",i,".rdata"))
  stProb <- data.table(stProb)
  stX <- stProb[,which.max(c(pST1,pST2,pST3,pST4,pST5)),by=segID]
  setnames(stX,c("segID","ST"))
  setkey(stX,segID)
  stDA2019 <- rbind(stDA2019,merge(XYsegID,stX))
  print(i)
}

  rastX <- rasterFromXYZ(stDA2019[,c("x","y","ST"),with=F])
  crs(rastX) <- crsX
  rastName <- "outRast/init2016/STda2019.tif"
  writeRaster(rastX,filename = rastName,overwrite=T)
  



