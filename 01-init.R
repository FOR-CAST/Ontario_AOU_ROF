################################################################################
## Initialization parameters and settings
################################################################################

.starttime <- Sys.time()

if (file.exists(".Renviron")) readRenviron(".Renviron")

Require(c("magrittr", "PredictiveEcology/quickPlot@development"))

quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer()) ## TODO: temporary for Alex's testing

cacheDir <- config::get("paths")[["cachedir"]]
cloudCacheFolderID <- config::get("cloud")[["cachedir"]]
delayStart <- config::get("delaystart")
deleteSpeciesLayers <- FALSE
eventCaching <- c(".inputObjects", "init")
firstRunSpreadFit <- FALSE
mapParallel <- FALSE
rep <- as.numeric(substr(runName, nchar(runName) - 1, nchar(runName)))
resolution <- as.integer(config::get("resolution"))
scratchDir <- config::get("paths")[["scratchdir"]]
sppEquivCol <- "ON"
studyAreaName <- if (grepl("AOU", runName)) {
  if (grepl("test", runName)) {
    "AOU_test"
  } else {
    "AOU"
  }
} else if (grepl("ROF", runName)) {
  if (grepl("test", runName)) {
    "ROF_test" ## TODO: enable test area in preamble for RoF
  } else {
    "ROF"
  }
} else {
  stop("runName must contain one of 'AOU' or 'ROF'.")
}
successionTimestep <- 10
uplaod2GDrive <- config::get("upload")
useCloudCache <- config::get("cloud")[["usecloud"]]
usePlot <- config::get("plot")
.plotInitialTime <- if (isTRUE(usePlot)) 0 else NA

firstRunMDCplots <- if (rep == 1) TRUE else FALSE
