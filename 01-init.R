################################################################################
## Initialization parameters and settings
################################################################################

.starttime <- Sys.time()

if (file.exists(".Renviron")) readRenviron(".Renviron")

library(config)
library(magrittr)

quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer()) ## TODO: temporary for Alex's testing

cacheDir <- config::get("paths")[["cachedir"]]
cloudCacheFolderID <- config::get("cloud")[["cachedir"]]
deleteSpeciesLayers <- FALSE
eventCaching <- c(".inputObjects", "init")
mapParallel <- FALSE
rep <- config::get("rep")
resolution <- as.integer(config::get("resolution"))
scratchDir <- config::get("paths")[["scratchdir"]]
sppEquivCol <- "ON"
studyAreaName <- if (grepl("AOU", runName)) {
  "AOU"
} else if (grepl("ROF", runName)) {
  "ROF"
} else {
  stop("runName must contain one of 'AOU' or 'ROF'.")
}
successionTimestep <- 10
useCloudCache <- config::get("cloud")[["usecloud"]]
usePlot <- config::get("plot")
.plotInitialTime <- if (isTRUE(usePlot)) 0 else NA
