################################################################################
## Initialization parameters and settings
################################################################################

.starttime <- Sys.time()

if (file.exists(".Renviron")) readRenviron(".Renviron")

library(config)
library(magrittr)

quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer()) ## TODO: temporary for Alex's testing

getMapResFact <- function(runName) {
  res <- strsplit(runName, "_")[[1]] %>%
    grep("res", ., value = TRUE) %>%
    substr(., 4, 6) %>%
    as.integer(.)

  if (identical(res, integer(0))) res <- 250 ## use 250 when not specified, e.g. for old runs

  if (res %in% c(50, 125, 250)) {
    250 / res
  } else {
    warning("res should be one of 250, 125, or 50. using value specified by config.yml.")
    config::get("mapresfact")
  }
}

cacheDir <- config::get("paths")[["cachedir"]]
cloudCacheFolderID <- config::get("cloud")[["cachedir"]]
deleteSpeciesLayers <- FALSE
eventCaching <- c(".inputObjects", "init")
mapParallel <- FALSE
mapResFact <- getMapResFact(runName)
rerunDataPrep <- if (grepl("LandWeb", runName)) FALSE else TRUE ## TODO
rerunSpeciesLayers <- if (grepl("LandWeb", runName)) FALSE else TRUE ## TODO
scratchDir <- config::get("paths")[["scratchdir"]]
sppEquivCol <- "LandWeb" ## TODO
useCloudCache <- config::get("cloud")[["usecloud"]]
